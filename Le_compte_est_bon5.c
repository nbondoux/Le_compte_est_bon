#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <ucontext.h>
#include <setjmp.h>

// generics for generators
#define NB_GENERATOR_STACK_LENGTH 1000

#define NB_PREEMPT(gen) \
  if (!sigsetjmp (((struct NB_BaseGenerator*) gen) -> nextOuterContext,0)) { \
      siglongjmp (((struct NB_BaseGenerator*) gen) -> nextGenContext,1); \
    }

#define NB_YIELD(gen) \
    if (!sigsetjmp (((struct NB_BaseGenerator*) gen) -> nextGenContext,0)) { \
      siglongjmp (((struct NB_BaseGenerator*) gen) -> nextOuterContext,1); \
    }

// a generator
struct NB_BaseGenerator;

// a Holder wich maintains a list of the generators of a same kind (it  links the head of a linked list)
struct NB_BaseGeneratorHolder;

// technical value needed by makecontext; should be protected with a mutex ..
static struct NB_BaseGenerator* NB_Generators_Tmp_Context_Holder;

struct NB_BaseGeneratorHolder {
  // should also hold mutexes for updates
  struct NB_BaseGenerator * poolHeadOfList;
  struct NB_BaseGenerator * freePoolHeadOfList;
};
  

struct NB_BaseGenerator {
  struct NB_BaseGenerator* nextFree;
  struct NB_BaseGenerator* next;
  struct NB_BaseGeneratorHolder* pool;

  // the function that does the generation
  void (*run) (struct NB_BaseGenerator*); 

  // tells that the generator is new, and that no field has been set
  int isNew;

  // it is up to the user of the generator to put "isAtEnd" to 0
  // after generator initialization
  int isAtEnd;

  sigjmp_buf nextGenContext;
  sigjmp_buf nextOuterContext;

  ucontext_t ucp;
  void* stack[NB_GENERATOR_STACK_LENGTH];
};

typedef struct NB_BaseGenerator NB_BaseGenerator_t;
typedef struct NB_BaseGeneratorHolder NB_BaseGeneratorHolder_t;


void NB_BaseGenerator_mainLoop() {

  NB_BaseGenerator_t* generator = (NB_BaseGenerator_t*) NB_Generators_Tmp_Context_Holder;

  while (1) {
    while (generator -> isAtEnd) {
      NB_YIELD (generator);
    }

    generator -> run (generator);

    // should lock a mutex here
    generator -> nextFree = generator -> pool -> freePoolHeadOfList;
    generator -> pool -> freePoolHeadOfList = generator;

    generator -> isAtEnd = 1;
  }
}



NB_BaseGenerator_t* NB_getFreeGenerator (size_t iGeneratorSize,
                                         NB_BaseGeneratorHolder_t* ioGeneratorPool) {
  if (ioGeneratorPool -> freePoolHeadOfList != NULL) {
    NB_BaseGenerator_t* generator = ioGeneratorPool -> freePoolHeadOfList;
    ioGeneratorPool -> freePoolHeadOfList = generator -> nextFree;
    generator -> nextFree = NULL;
    generator -> isAtEnd = 1;
    return generator;
  } else {
    NB_BaseGenerator_t* generator = (NB_BaseGenerator_t*) calloc (1,iGeneratorSize);
    // should lock by mutex here
    generator -> next = ioGeneratorPool -> poolHeadOfList;
    ioGeneratorPool -> poolHeadOfList = generator;
    generator -> isAtEnd = 1;

    // allocate the stack:
    getcontext (&generator -> ucp);
    generator -> ucp.uc_stack.ss_sp = generator -> stack;
    generator -> ucp.uc_stack.ss_size= sizeof (generator -> stack);
    generator -> ucp.uc_link = NULL;
    makecontext (&generator -> ucp,&NB_BaseGenerator_mainLoop,0);

    if (!sigsetjmp(generator -> nextOuterContext,0)) {
      NB_Generators_Tmp_Context_Holder = generator;
      // let's initialize the generator
      setcontext (&generator -> ucp);
    }    
    generator -> pool = ioGeneratorPool;

    generator -> isNew = 1;
    return generator;
  }
}


void NB_freePool (NB_BaseGeneratorHolder_t* ioGeneratorPool, void (*iCleanChildGenerator) (void*) )
{
  // should lock by mutex here ?
  NB_BaseGenerator_t* generator = ioGeneratorPool -> poolHeadOfList;

  while (generator != NULL) {
    NB_BaseGenerator_t* nextGenerator = generator -> next;
    if (iCleanChildGenerator) {
      iCleanChildGenerator (generator);
    }
    free (generator);
    generator = nextGenerator;
  }

  // should lock by mutex here
  ioGeneratorPool -> poolHeadOfList = NULL;
  ioGeneratorPool -> freePoolHeadOfList = NULL;
}

// end of generics for generators

// ////////////////////////////////////////////////////////////////////////////
// simple sub-lists generator
// ////////////////////////////////////////////////////////////////////////////

struct SubCombinationsGenerator {
  NB_BaseGenerator_t super;
  
  // input values;
  void ** l;
  size_t lSize;

  size_t yieldedSubLSize;
  // containes the output sub-list
  // it must be malloced by user of the generator, and will be destroyed at deletion of
  // the generator
  void ** yieldedSubL;
   
};

typedef struct SubCombinationsGenerator SubCombinationsGenerator_t;

void subcombinationsGenerator_clean (SubCombinationsGenerator_t* ioGen) {
  if (ioGen -> yieldedSubL != NULL) {
    free (ioGen -> yieldedSubL);
  }
}

void subcombinationsFixedLSizeGenerator_rec (SubCombinationsGenerator_t* ioGen,
                                             void ** iL,
                                             size_t iSizeL,
                                             void ** ioSubL,
                                             size_t iSubLRemSize) {
  if (iSubLRemSize == 0) {
    NB_YIELD(ioGen);
  } else {
    //all combinations that contain head of list
    *ioSubL = *iL;
    subcombinationsFixedLSizeGenerator_rec (ioGen, iL+1,iSizeL - 1, ioSubL+1, iSubLRemSize - 1);
    //all combinations that do not contain head of list
    if (iSizeL > iSubLRemSize) {
      subcombinationsFixedLSizeGenerator_rec (ioGen, iL+1,iSizeL - 1, ioSubL, iSubLRemSize);
    }
  }
}

void subcombinationsGenerator_run(NB_BaseGenerator_t* ioGen) {

  SubCombinationsGenerator_t* generator = (SubCombinationsGenerator_t*) ioGen;

  size_t i = 0 ;
  size_t lSize = generator -> lSize;
  for (i=0;i <= lSize;++i) {
    generator -> yieldedSubLSize = i;
    subcombinationsFixedLSizeGenerator_rec (generator,
                                            generator -> l,
                                            lSize,
                                            generator -> yieldedSubL,
                                            i);
  }
}




// ////////////////////////////////////////////////////////////////////////////
// Non-ordered couple of sub-lists generator
// ////////////////////////////////////////////////////////////////////////////

struct SubCombinationCouplesGenerator {
  NB_BaseGenerator_t super;
  
  // input values;
  void ** l;
  size_t lSize;

  // containes the output value
  size_t yieldedSubL1Size;
  size_t yieldedSubL2Size;


  // these two lists be malloced by user of the generator, and will be destroyed at deletion of
  // the generator
  void ** yieldedSubL1;
  void ** yieldedSubL2;
   
};

typedef struct SubCombinationCouplesGenerator SubCombinationCouplesGenerator_t;

void subcombinationCouplesGenerator_clean (SubCombinationCouplesGenerator_t* ioGen) {
  if (ioGen -> yieldedSubL1 != NULL) {
    free (ioGen -> yieldedSubL1);
  }

  if (ioGen -> yieldedSubL2 != NULL) {
    free (ioGen -> yieldedSubL2);
  }
}

// returns all subcombinations l1, l2
// subL1RemSize contains the number of elements that remains to be added by the algo to subL1

void subcombinationCouplesFixedLSizeGenerator_rec (SubCombinationCouplesGenerator_t* ioGen,
                                             void ** iL,
                                             size_t iSizeL,
                                             void ** ioSubL1,
                                             size_t iSubL1RemSize,
                                             void ** ioSubL2) {
  if (iSubL1RemSize == 0) {
    size_t i;
    for (i=0;i < iSizeL;++i) {
      ioSubL2[i] = iL[i];
    }
    NB_YIELD(ioGen);
  } else {
    //all combinations where L1 contains head of list
    *ioSubL1 = *iL;
    subcombinationCouplesFixedLSizeGenerator_rec (ioGen, iL+1,iSizeL - 1, ioSubL1+1, iSubL1RemSize - 1, ioSubL2);

    //all combinations where L1 does not contain head of list
    if (iSizeL > iSubL1RemSize) {
      *ioSubL2 = *iL;
      subcombinationCouplesFixedLSizeGenerator_rec (ioGen, iL+1,iSizeL - 1, ioSubL1, iSubL1RemSize, ioSubL2+1);
    }
  }
}


// same as rec1, but for
// for instance, for [a,b,c,d,e,f], and iSubL1RemSize == 3, does not return both [[a,b,c],[d,e,f]] and  [[d,e,f],[a,b,c]]
void subcombinationCouplesFixedLSizeGenerator_rec2 (SubCombinationCouplesGenerator_t* ioGen,
                                             void ** iL,
                                             size_t iSizeL,
                                             void ** ioSubL1,
                                             size_t iSubL1RemSize,
                                             void ** ioSubL2) {
  if (iSubL1RemSize*2 != iSizeL || iSizeL == 0) {
    subcombinationCouplesFixedLSizeGenerator_rec (ioGen, iL,iSizeL, ioSubL1, iSubL1RemSize, ioSubL2);
  } else {
    // ioSubL1 will always hold the sub-lists that contains the first element of L
    *ioSubL1 = *iL;
    subcombinationCouplesFixedLSizeGenerator_rec (ioGen, iL + 1,iSizeL - 1, ioSubL1+1, iSubL1RemSize - 1, ioSubL2);
  }
}


void subcombinationCouplesGenerator_run(NB_BaseGenerator_t* ioGen) {

  SubCombinationCouplesGenerator_t* generator = (SubCombinationCouplesGenerator_t*) ioGen;

  size_t i = 0 ;
  size_t lSize = generator -> lSize;
  for (i=0;i*2 <= lSize;++i) {
    generator -> yieldedSubL1Size = i;
    generator -> yieldedSubL2Size = lSize - i;
    subcombinationCouplesFixedLSizeGenerator_rec2 (generator,
                                                   generator -> l,
                                                   lSize,
                                                   generator -> yieldedSubL1,
                                                   i,
                                                   generator -> yieldedSubL2);
  }
}

NB_BaseGeneratorHolder_t SubCombinationsGeneratorHolder = {0,};
NB_BaseGeneratorHolder_t SubCombinationCouplesGeneratorHolder = {0,};

NB_BaseGeneratorHolder_t LcebFixedSizeGeneratorHolder = {0,};

// ////////////////////////////////////////////////////////////////////////////
// Basic "Le compte est bon" structures
// ////////////////////////////////////////////////////////////////////////////


struct Node {
  unsigned int value;
  union {
    struct { 
      struct Node * ag;
      struct Node * ad;
      enum {Add, Minus, Mult, Divi}  op;
    } Node;
  } u;
  enum {Number, Node} type;
};

typedef struct Node Node_t;

inline Node_t * allocNode() {
  Node_t *a;
  a = (Node_t *) malloc (sizeof (Node_t));
  a->type=Number;
  return a;  
}

// copy inner values of Node
inline void copyInnerNode (const Node_t* iNode,
                           Node_t* oNode) {
  if (iNode->type == Number) {
    oNode->type = Number;
    oNode->value = iNode->value;
  } else {
    // iNode.type == Node
    oNode->type = Node;
    oNode->value = iNode->value;
    oNode->u.Node.op = iNode->u.Node.op;
  }
}

Node_t * duplicateNode(const Node_t * iNode) {
  Node_t* clonedNode = allocNode ();
  copyInnerNode (iNode,clonedNode);
  
  if (iNode->type == Node) {
    clonedNode->u.Node.ag = duplicateNode (iNode->u.Node.ag);
    clonedNode->u.Node.ad = duplicateNode (iNode->u.Node.ad);
  }
  return clonedNode;
}

void freeNode(Node_t * ioNode) {
  if(ioNode->type == Node) {
    freeNode (ioNode->u.Node.ag);
    freeNode (ioNode->u.Node.ad);
  }
  free(ioNode);
}

/*retourne la priorité de l'arbre */
unsigned int opPriority(Node_t * iNode) {
  if(iNode-> type == Node) {
    if(iNode -> u.Node.op == Add || iNode -> u.Node.op == Minus)
      return 1;
    else if(iNode -> u.Node.op == Mult || iNode -> u.Node.op == Divi)
      return 2;
    return 0;
  } else {
    return 100;
  }
}

int opAssociativity(Node_t * iNode) {
  if(iNode-> type == Node) {
    if(iNode -> u.Node.op == Add || iNode -> u.Node.op == Mult)
      return 1;
    else if(iNode -> u.Node.op == Minus || iNode -> u.Node.op == Divi)
      return 0;
    return 0;
  } else {
    return 0;
  }
}


/*alloue (malloc) une chaine de caractères contenant l'expression contenue par l'arbre */
char * stringFromNode (Node_t * iNode) {
  char * c1, *c2, *c;
  unsigned int i1,i2;
  if(iNode -> type == Node) {
    c1=stringFromNode(iNode->u.Node.ag);
    i1=strlen(c1);
    c2=stringFromNode(iNode->u.Node.ad);
    i2=strlen(c2);
    c=(char *) malloc( sizeof(char)*(i1+i2+4+1+1));
    c[0]='\0';

    if(opPriority (iNode->u.Node.ag) < opPriority (iNode)) {
      strcat(c,"(");
      strcat(c,c1);
      strcat(c,")");
    } else
      strcat(c,c1);
      
    if(iNode->u.Node.op == Add)
      strcat(c,"+");
    else if(iNode->u.Node.op == Minus)
      strcat(c,"-");
    else if(iNode->u.Node.op == Mult)
      strcat(c,"*");
    else if(iNode->u.Node.op == Divi)
      strcat(c,"/");
    
    if((!opAssociativity (iNode->u.Node.ad) && opPriority (iNode->u.Node.ad) <= opPriority (iNode))
       || opPriority (iNode->u.Node.ad) < opPriority (iNode)
       ) {
      strcat(c,"(");
      strcat(c,c2);
      strcat(c,")");
    } else
      strcat(c,c2);
    free(c1);
    free(c2);
  } else {
    char n[500];
    sprintf(n,"%u",iNode->value);
    c = (char *) malloc (sizeof(char) * (strlen(n) + 1));
    strcpy(c,n);
  }
  return c;
}

/*malloc une table de Nodes, à partir du tableau d'entiers l, de longueur n */
Node_t ** buildNodeVector(unsigned int * iL, size_t iLSize) {
  unsigned int i;
  Node_t ** a;
  a= (Node_t **) malloc (sizeof(Node_t *)* iLSize);
  
  for(i=0;i<iLSize;i++) {
    a[i]=allocNode();
    a[i]->type=Number;
    a[i]->value= iL[i];
  }
  return a;
}

void cleanNodeVector(Node_t ** ioNodes, size_t iNodeSize) {
  unsigned int i;
  
  for(i=0;i<iNodeSize;i++) {
    freeNode (ioNodes[i]);
  }
  free (ioNodes);
}


inline unsigned int value_Node (Node_t * iNodes) {
  return iNodes->value;
}

typedef struct {
  Node_t *node;
  unsigned int delta;
} BestSolution;

inline void tryBestSolution (Node_t *iCurrentTree, unsigned int iTarget, BestSolution* ioBestSolution) {
  unsigned int currentValue = value_Node (iCurrentTree);
  unsigned int delta = ioBestSolution -> delta;
  if (ioBestSolution -> node == NULL ||
      ((currentValue >= iTarget && currentValue - iTarget < ioBestSolution -> delta) ||
       (currentValue < iTarget && iTarget - currentValue < delta))) {
    
    // new bestSolutionFound 
    if (ioBestSolution -> node != NULL) {
      freeNode(ioBestSolution -> node);
    }
    
    char * c=stringFromNode(iCurrentTree);
    printf("Best solution so far: %d = %s \n",currentValue,c);
    free(c);
    ioBestSolution -> node = duplicateNode(iCurrentTree);

    if (currentValue >= iTarget) {
      ioBestSolution -> delta = currentValue - iTarget;
    } else {
      ioBestSolution -> delta = iTarget - currentValue;
    }    
  }
}



// ////////////////////////////////////////////////////////////////////////////
// Fixed size algo generator
// ////////////////////////////////////////////////////////////////////////////

// For a list l, this algo returns a combinations containing all its numbers combined by operations



int main() {
  
  int nbs [50];
  int i =0;

  for (i=0; i< 50;++i) {
    nbs[i] = i;
  }
  
  void * l[40];
  for (i=0; i< 40;++i) {
    l[i] = (void *) (nbs+i);
  }

  
  NB_BaseGeneratorHolder_t genPool = {0,};

  /*
  SubCombinationsGenerator_t* gen1 =
    (SubCombinationsGenerator_t*) NB_getFreeGenerator (subcombinationsGenerator_gen,
                                                                 sizeof(SubCombinationsGenerator_t),
                                                                 &genPool);
  
  gen1 -> l = l;
  gen1 -> lSize = 27;

  gen1 -> yieldedSubL = (void**) malloc (sizeof(void *) * 27);
  
  gen1 -> super.isAtEnd = 0;

  NB_PREEMPT (gen1);
  while (!(gen1 -> super.isAtEnd)) {
    size_t j;
    for (j = 0;j < gen1 -> yieldedSubLSize;j++) {
      printf ("%d ",* (int *) (gen1 -> yieldedSubL[j]));
    }
    printf ("\n");
    NB_PREEMPT (gen1);
  }


  NB_freePool (&genPool,NULL);
  */

  SubCombinationCouplesGenerator_t* gen1 =
    (SubCombinationCouplesGenerator_t*) NB_getFreeGenerator (sizeof(SubCombinationCouplesGenerator_t),
                                                             &genPool);

  if (gen1 -> super.isNew) {
    printf ("Init Gen1\n");
    gen1 -> super.run = subcombinationCouplesGenerator_run;
    gen1 -> l = l;
    gen1 -> lSize = 6;
    
    gen1 -> yieldedSubL1 = (void**) malloc (sizeof(void *) * 27);
    gen1 -> yieldedSubL2 = (void**) malloc (sizeof(void *) * 27);
    
    gen1 -> super.isNew = 0;
  }
  gen1 -> super.isAtEnd = 0;
  NB_PREEMPT (gen1);
  while (!(gen1 -> super.isAtEnd)) {
    size_t j;
    for (j = 0;j < gen1 -> yieldedSubL1Size;j++) {
      printf ("%d ",* (int *) (gen1 -> yieldedSubL1[j]));
    }
    printf (" | ");
    for (j = 0;j < gen1 -> yieldedSubL2Size;j++) {
      printf ("%d ",* (int *) (gen1 -> yieldedSubL2[j]));
    }

    printf ("\n");
    NB_PREEMPT (gen1);
  }

  if (gen1 -> super.isNew) {
    gen1 -> super.run = subcombinationCouplesGenerator_run;
    gen1 -> l = l;
    gen1 -> lSize = 20;
    
    gen1 -> yieldedSubL1 = (void**) malloc (sizeof(void *) * 27);
    gen1 -> yieldedSubL2 = (void**) malloc (sizeof(void *) * 27);
    
    gen1 -> super.isNew = 0;
  }
  gen1 -> super.isAtEnd = 0;
  NB_PREEMPT (gen1);
  while (!(gen1 -> super.isAtEnd)) {
    size_t j;
    for (j = 0;j < gen1 -> yieldedSubL1Size;j++) {
      printf ("%d ",* (int *) (gen1 -> yieldedSubL1[j]));
    }
    printf (" | ");
    for (j = 0;j < gen1 -> yieldedSubL2Size;j++) {
      printf ("%d ",* (int *) (gen1 -> yieldedSubL2[j]));
    }

    printf ("\n");
    NB_PREEMPT (gen1);
  }



  NB_freePool (&genPool,NULL);



  return 0;
}
