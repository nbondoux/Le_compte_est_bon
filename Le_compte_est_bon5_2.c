#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <ucontext.h>
#include <setjmp.h>


// a simple functor
struct NB_BaseFunctor {
  void (*run) (struct NB_BaseFunctor*, void *);   
};
typedef struct NB_BaseFunctor NB_BaseFunctor_t;


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
  
  size_t generatorSize;

  NB_BaseFunctor_t* elmtConstructor;
  NB_BaseFunctor_t* elmtDestructor;
};
  

struct NB_BaseGenerator {
  struct NB_BaseGenerator* nextFree;
  struct NB_BaseGenerator* next;
  struct NB_BaseGeneratorHolder* pool;

  // the function that does the generation
  void (*run) (struct NB_BaseGenerator*); 

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

inline void NB_stopGenerator (NB_BaseGenerator_t* iGen) {
    // should lock a mutex here
    iGen -> nextFree = iGen -> pool -> freePoolHeadOfList;
    iGen -> pool -> freePoolHeadOfList = iGen;

    iGen -> isAtEnd = 1;
}

void NB_BaseGenerator_mainLoop() {

  NB_BaseGenerator_t* generator = (NB_BaseGenerator_t*) NB_Generators_Tmp_Context_Holder;

  while (1) {
    while (generator -> isAtEnd) {
      NB_YIELD (generator);
    }

    generator -> run (generator);

    NB_stopGenerator (generator);
  }
}


NB_BaseGenerator_t* NB_getFreeGenerator (NB_BaseGeneratorHolder_t* ioGeneratorPool) {
  if (ioGeneratorPool -> freePoolHeadOfList != NULL) {
    NB_BaseGenerator_t* generator = ioGeneratorPool -> freePoolHeadOfList;
    ioGeneratorPool -> freePoolHeadOfList = generator -> nextFree;
    generator -> nextFree = NULL;
    generator -> isAtEnd = 1;
    return generator;
  } else {
    NB_BaseGenerator_t* generator = (NB_BaseGenerator_t*) calloc (1,ioGeneratorPool -> generatorSize);
 
   // should lock by mutex here
    generator -> next = ioGeneratorPool -> poolHeadOfList;
    ioGeneratorPool -> poolHeadOfList = generator;
    generator -> isAtEnd = 1;

    ioGeneratorPool -> elmtConstructor -> run (ioGeneratorPool->elmtConstructor,generator);

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

    return generator;
  }
}


void NB_freePool (NB_BaseGeneratorHolder_t* ioGeneratorPool)
{
  // should lock by mutex here ?
  NB_BaseGenerator_t* generator = ioGeneratorPool -> poolHeadOfList;

  while (generator != NULL) {
    NB_BaseGenerator_t* nextGenerator = generator -> next;

    if (ioGeneratorPool -> elmtDestructor) {
      ioGeneratorPool -> elmtDestructor -> run (ioGeneratorPool->elmtDestructor,generator);
    }
    free (generator);
    generator = nextGenerator;
  }

  // should lock by mutex here
  ioGeneratorPool -> poolHeadOfList = NULL;
  ioGeneratorPool -> freePoolHeadOfList = NULL;
}

// end of generics for generators

// generics for stack-less coroutines

// based on the idea of Simon Tatham: http://www.linuxhowtos.org/C_C++/coroutines.htm

#define crBegin switch(((NB_BaseSLCoroutineCtx_t*) ioCoroCtx) -> line) { case 0:
#define crReturn(x) do { ((NB_BaseSLCoroutineCtx_t*) ioCoroCtx) -> line=__LINE__; return x; \
                         case __LINE__:; } while (0)
#define crFinish }


// the context of a stack-less co-routine
struct NB_BaseSLCoroutineCtx;

// a Holder wich maintains a list of contextes of coroutines of a same kind (it  links the head of a linked list)
struct NB_BaseSLCoroutineCtxHolder;

struct NB_BaseSLCoroutineCtxHolder {
  // should also hold mutexes for updates
  struct NB_BaseSLCoroutineCtx * poolHeadOfList;
  struct NB_BaseSLCoroutineCtx * freePoolHeadOfList;
  
  size_t ctxSize;

  NB_BaseFunctor_t* elmtConstructor;
  NB_BaseFunctor_t* elmtDestructor;
};
  

struct NB_BaseSLCoroutineCtx {
  struct NB_BaseSLCoroutineCtx* nextFree;
  struct NB_BaseSLCoroutineCtx* next;
  struct NB_BaseSLCoroutineCtxHolder* pool;

  unsigned int line; 
};

typedef struct NB_BaseSLCoroutineCtx NB_BaseSLCoroutineCtx_t;
typedef struct NB_BaseSLCoroutineCtxHolder NB_BaseSLCoroutineCtxHolder_t;

inline void NB_freeSLCoroutineCtx (NB_BaseSLCoroutineCtx_t* iCorCtx) {
    // should lock a mutex here
    iCorCtx -> nextFree = iCorCtx -> pool -> freePoolHeadOfList;
    iCorCtx -> pool -> freePoolHeadOfList = iCorCtx;
}


NB_BaseSLCoroutineCtx_t* NB_getSLCoroutine (NB_BaseSLCoroutineCtxHolder_t* ioSLCoroCtxPool) {
  NB_BaseSLCoroutineCtx_t* coro;
  if (ioSLCoroCtxPool -> freePoolHeadOfList != NULL) {
    coro = ioSLCoroCtxPool -> freePoolHeadOfList;
    ioSLCoroCtxPool -> freePoolHeadOfList = coro -> nextFree;
  } else {
    coro = (NB_BaseSLCoroutineCtx_t*) calloc (1,ioSLCoroCtxPool -> ctxSize);
 
    // should lock by mutex here
    coro -> next = ioSLCoroCtxPool -> poolHeadOfList;
    ioSLCoroCtxPool -> poolHeadOfList = coro;

    coro -> pool = ioSLCoroCtxPool;

    if (ioSLCoroCtxPool -> elmtConstructor != NULL) {
      ioSLCoroCtxPool -> elmtConstructor -> run (ioSLCoroCtxPool->elmtConstructor,coro);
    }
  }
  coro -> line = 0;
  return coro;
}


void NB_freeSLCoroutineCtxPool (NB_BaseSLCoroutineCtxHolder_t* ioSLCoroCtxPool)
{
  // should lock by mutex here ?
  NB_BaseSLCoroutineCtx_t* coro = ioSLCoroCtxPool -> poolHeadOfList;

  while (coro != NULL) {
    NB_BaseSLCoroutineCtx_t* nextCoro = coro -> next;

    if (ioSLCoroCtxPool -> elmtDestructor) {
      ioSLCoroCtxPool -> elmtDestructor -> run (ioSLCoroCtxPool->elmtDestructor,coro);
    }
    free (coro);
    coro = nextCoro;
  }

  // should lock by mutex here
  ioSLCoroCtxPool -> poolHeadOfList = NULL;
  ioSLCoroCtxPool -> freePoolHeadOfList = NULL;
}

// end of generics for stack-less coroutines



// Concept of this algorithm:
// The value generated (all extracted combinations of numbers by the 4
// operations) from a list l (size L) are the values made of N numbers
// generated by each of its  sublists of size N + the values made of
// combinations by an operation of the values generated by the couple of lists
// whose combined total number of elements is L
// no duplicate value is therefore ever generated for l or any other sub-lists




// ////////////////////////////////////////////////////////////////////////////
// simple sub-lists generator
// ////////////////////////////////////////////////////////////////////////////

struct SubCombinationsGenerator {
  NB_BaseGenerator_t super;
  
  // input values;
  void ** l;
  size_t lSize;
  size_t maxSize;

  size_t yieldedSubLSize;
  // containes the output sub-list
  // its allocation/deallocation is the responsability of the generator system
  void ** yieldedSubL;
   
};

typedef struct SubCombinationsGenerator SubCombinationsGenerator_t;


void subCombinationsFixedLSizeGenerator_rec (SubCombinationsGenerator_t* ioGen,
                                             void ** iL,
                                             size_t iSizeL,
                                             void ** ioSubL,
                                             size_t iSubLRemSize) {
  if (iSubLRemSize == 0) {
    NB_YIELD(ioGen);
  } else {
    //all combinations that contain head of list
    *ioSubL = *iL;
    subCombinationsFixedLSizeGenerator_rec (ioGen, iL+1,iSizeL - 1, ioSubL+1, iSubLRemSize - 1);
    //all combinations that do not contain head of list
    if (iSizeL > iSubLRemSize) {
      subCombinationsFixedLSizeGenerator_rec (ioGen, iL+1,iSizeL - 1, ioSubL, iSubLRemSize);
    }
  }
}

void subCombinationsGenerator_run(NB_BaseGenerator_t* ioGen) {

  SubCombinationsGenerator_t* generator = (SubCombinationsGenerator_t*) ioGen;

  size_t i = 0 ;
  size_t lSize = generator -> lSize;
  size_t maxSize = generator -> maxSize;

  for (i=0;i <= lSize && i < maxSize;++i) {
    generator -> yieldedSubLSize = i;
    subCombinationsFixedLSizeGenerator_rec (generator,
                                            generator -> l,
                                            lSize,
                                            generator -> yieldedSubL,
                                            i);
  }
}


// initializer:
struct SubCombinationsInitializer {
  NB_BaseFunctor_t super;
  size_t subLMaxStaticSize;
};
typedef struct SubCombinationsInitializer SubCombinationsInitializer_t;

void subCombinationsInitializer (NB_BaseFunctor_t* iSelf, void* ioGen) {
  SubCombinationsInitializer_t* self = (SubCombinationsInitializer_t*) iSelf;
  SubCombinationsGenerator_t* generator = (SubCombinationsGenerator_t*) ioGen;
  
  generator -> super.run = &subCombinationsGenerator_run;
  generator -> yieldedSubL = (void **) malloc (sizeof (void*) * self -> subLMaxStaticSize);
}

//cleaner:
struct SubCombinationsCleaner {
  NB_BaseFunctor_t super;
};
typedef struct SubCombinationsCleaner SubCombinationsCleaner_t;


void subCombinationsCleaner (NB_BaseFunctor_t* iSelf, void* ioGen) {
  SubCombinationsGenerator_t* generator = (SubCombinationsGenerator_t*) ioGen;
  if (generator -> yieldedSubL != NULL) {
    free (generator -> yieldedSubL);
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

// returns all subCombinations l1, l2
// subL1RemSize contains the number of elements that remains to be added by the algo to subL1

void subCombinationCouplesFixedLSizeGenerator_rec (SubCombinationCouplesGenerator_t* ioGen,
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
    subCombinationCouplesFixedLSizeGenerator_rec (ioGen, iL+1,iSizeL - 1, ioSubL1+1, iSubL1RemSize - 1, ioSubL2);

    //all combinations where L1 does not contain head of list
    if (iSizeL > iSubL1RemSize) {
      *ioSubL2 = *iL;
      subCombinationCouplesFixedLSizeGenerator_rec (ioGen, iL+1,iSizeL - 1, ioSubL1, iSubL1RemSize, ioSubL2+1);
    }
  }
}


// same as rec1, but for
// for instance, for [a,b,c,d,e,f], and iSubL1RemSize == 3, does not return both [[a,b,c],[d,e,f]] and  [[d,e,f],[a,b,c]]
void subCombinationCouplesFixedLSizeGenerator_rec2 (SubCombinationCouplesGenerator_t* ioGen,
                                             void ** iL,
                                             size_t iSizeL,
                                             void ** ioSubL1,
                                             size_t iSubL1RemSize,
                                             void ** ioSubL2) {
  if (iSubL1RemSize*2 != iSizeL || iSizeL == 0) {
    subCombinationCouplesFixedLSizeGenerator_rec (ioGen, iL,iSizeL, ioSubL1, iSubL1RemSize, ioSubL2);
  } else {
    // ioSubL1 will always hold the sub-lists that contains the first element of L
    *ioSubL1 = *iL;
    subCombinationCouplesFixedLSizeGenerator_rec (ioGen, iL + 1,iSizeL - 1, ioSubL1+1, iSubL1RemSize - 1, ioSubL2);
  }
}


void subCombinationCouplesGenerator_run(NB_BaseGenerator_t* ioGen) {

  SubCombinationCouplesGenerator_t* generator = (SubCombinationCouplesGenerator_t*) ioGen;

  size_t i = 0 ;
  size_t lSize = generator -> lSize;
  for (i=0;i*2 <= lSize;++i) {
    generator -> yieldedSubL1Size = i;
    generator -> yieldedSubL2Size = lSize - i;
    subCombinationCouplesFixedLSizeGenerator_rec2 (generator,
                                                   generator -> l,
                                                   lSize,
                                                   generator -> yieldedSubL1,
                                                   i,
                                                   generator -> yieldedSubL2);
  }
}

// initializer:
struct SubCombinationCouplesInitializer {
  NB_BaseFunctor_t super;
  size_t subLMaxStaticSize;
};
typedef struct SubCombinationCouplesInitializer SubCombinationCouplesInitializer_t;

void subCombinationCouplesInitializer (NB_BaseFunctor_t* iSelf, void* ioGen) {
  SubCombinationCouplesInitializer_t* self = (SubCombinationCouplesInitializer_t*) iSelf;
  SubCombinationCouplesGenerator_t* generator = (SubCombinationCouplesGenerator_t*) ioGen;
  
  generator -> super.run = &subCombinationCouplesGenerator_run;
  generator -> yieldedSubL1 = (void **) malloc (sizeof (void*) * self -> subLMaxStaticSize);
  generator -> yieldedSubL2 = (void **) malloc (sizeof (void*) * self -> subLMaxStaticSize);
}

//cleaner:
struct SubCombinationCouplesCleaner {
  NB_BaseFunctor_t super;
};
typedef struct SubCombinationCouplesCleaner SubCombinationCouplesCleaner_t;


void subCombinationCouplesCleaner (NB_BaseFunctor_t* iSelf, void* ioGen) {
  SubCombinationCouplesGenerator_t* generator = (SubCombinationCouplesGenerator_t*) ioGen;
  if (generator -> yieldedSubL1 != NULL) {
    free (generator -> yieldedSubL1);
  }
  if (generator -> yieldedSubL2 != NULL) {
    free (generator -> yieldedSubL2);
  }
}


NB_BaseGeneratorHolder_t SubCombinationsGeneratorHolder = {0,};
NB_BaseGeneratorHolder_t SubCombinationCouplesGeneratorHolder = {0,};

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

// return the priority of top operation of the node
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


// allocate with malloc a string that represents the expression of the Node
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
    
    if((!opAssociativity (iNode) && opPriority (iNode->u.Node.ad) <= opPriority (iNode))
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

// allocate (with malloc) a table of Nodes from a table of int
// of size iLSize
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


inline unsigned int valueNode (Node_t * iNodes) {
  return iNodes->value;
}

typedef struct {
  Node_t *node;
  unsigned int delta;
} BestSolution;

inline void tryBestSolution (Node_t *iCurrentTree, unsigned int iTarget, BestSolution* ioBestSolution) {
  unsigned int currentValue = valueNode (iCurrentTree);
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
NB_BaseSLCoroutineCtxHolder_t LcebFixedSizeGeneratorCtxHolder = {0,};

struct LcebFixedSizeGeneratorCtx {
  NB_BaseSLCoroutineCtx_t super;
  
  // input values:
  Node_t** l;
  size_t lSize;

  // internal variables:
  Node_t newNode;
  SubCombinationCouplesGenerator_t* coupleGenerator;

  struct LcebFixedSizeGeneratorCtx* elmt1CoroCtx;
  struct LcebFixedSizeGeneratorCtx* elmt2CoroCtx;

  Node_t* elmt1;
  unsigned int val1;

  Node_t* elmt2;
  unsigned int val2;


  // output variables:
  Node_t* yieldedNode;  
};

typedef struct LcebFixedSizeGeneratorCtx LcebFixedSizeGeneratorCtx_t;

// return false when the coroutine ended (the value in yieldedNode must then
// be ignored)
short lcebFixedSizeGenerator(LcebFixedSizeGeneratorCtx_t* ioCoroCtx) {
  crBegin;
  if (ioCoroCtx -> lSize == 1) {
    ioCoroCtx -> yieldedNode = (ioCoroCtx ->l)[0];
    crReturn(1);
  } else {
    Node_t newNode;
    ioCoroCtx -> yieldedNode = &(ioCoroCtx -> newNode);
    ioCoroCtx -> yieldedNode -> type = Node;

    // Let's yield all combination of elements of l1 X l2
    // it is generate "the values made of combinations by an operation
    // of the values generated by the couple of lists whose combined
    // total number of elements is L" part of the algorithm described
    // in the "Concept of this algorithm" comment

    ioCoroCtx -> coupleGenerator =
      (SubCombinationCouplesGenerator_t*) NB_getFreeGenerator (&SubCombinationCouplesGeneratorHolder);
    
    ioCoroCtx -> coupleGenerator -> l = (void **) (ioCoroCtx -> l);
    ioCoroCtx -> coupleGenerator -> lSize = ioCoroCtx -> lSize;
    ioCoroCtx -> coupleGenerator -> super.isAtEnd = 0;

    NB_PREEMPT (ioCoroCtx -> coupleGenerator);
    while (!ioCoroCtx -> coupleGenerator -> super.isAtEnd) {
      if(ioCoroCtx -> coupleGenerator -> yieldedSubL1Size == 0) {
        NB_PREEMPT (ioCoroCtx -> coupleGenerator);
        continue;
      }
      ioCoroCtx -> elmt1CoroCtx =
        (LcebFixedSizeGeneratorCtx_t*) NB_getSLCoroutine (&LcebFixedSizeGeneratorCtxHolder);
      ioCoroCtx -> elmt1CoroCtx -> l = (Node_t **) (ioCoroCtx -> coupleGenerator -> yieldedSubL1);
      ioCoroCtx -> elmt1CoroCtx -> lSize = ioCoroCtx -> coupleGenerator -> yieldedSubL1Size;
      
      while (lcebFixedSizeGenerator (ioCoroCtx -> elmt1CoroCtx)) {
        ioCoroCtx -> elmt1 = ioCoroCtx -> elmt1CoroCtx -> yieldedNode;
        ioCoroCtx -> val1 = valueNode (ioCoroCtx -> elmt1);
        ioCoroCtx -> newNode.u.Node.ag = ioCoroCtx -> elmt1;

        ioCoroCtx -> elmt2CoroCtx =
          (LcebFixedSizeGeneratorCtx_t*) NB_getSLCoroutine (&LcebFixedSizeGeneratorCtxHolder);
        ioCoroCtx -> elmt2CoroCtx -> l = (Node_t **) (ioCoroCtx ->  coupleGenerator -> yieldedSubL2);
        ioCoroCtx -> elmt2CoroCtx -> lSize = ioCoroCtx -> coupleGenerator -> yieldedSubL2Size;

        while (lcebFixedSizeGenerator (ioCoroCtx -> elmt2CoroCtx)) {
          ioCoroCtx -> elmt2 = ioCoroCtx -> elmt2CoroCtx -> yieldedNode;
          ioCoroCtx -> val2 = valueNode (ioCoroCtx -> elmt2);
          ioCoroCtx -> newNode.u.Node.ad = ioCoroCtx -> elmt2;

            
          if (ioCoroCtx -> val1 > 0 && ioCoroCtx -> val2 > 0) {
            ioCoroCtx -> newNode.value=ioCoroCtx -> val1 + ioCoroCtx -> val2;
            ioCoroCtx -> newNode.u.Node.op = Add;
            crReturn(1);
          }

          if (ioCoroCtx -> val2 > ioCoroCtx -> val1) {
            if (ioCoroCtx -> elmt2 -> type != Node || ioCoroCtx -> elmt2 ->u.Node.op != Add) {
              ioCoroCtx -> newNode.value=ioCoroCtx -> val2 - ioCoroCtx -> val1;
                    
              ioCoroCtx -> newNode.u.Node.ag = ioCoroCtx -> elmt2;
              ioCoroCtx -> newNode.u.Node.ad = ioCoroCtx -> elmt1;
              ioCoroCtx -> newNode.u.Node.op = Minus;
              crReturn(1);
              ioCoroCtx -> newNode.u.Node.ag = ioCoroCtx -> elmt1;
              ioCoroCtx -> newNode.u.Node.ad = ioCoroCtx -> elmt2;
            }
          }
                
          if (ioCoroCtx -> val1 >= ioCoroCtx -> val2) {
            if (ioCoroCtx -> elmt1 -> type != Node || ioCoroCtx -> elmt1 ->u.Node.op != Add) {
              newNode.value=ioCoroCtx -> val1 - ioCoroCtx -> val2;
                    
              ioCoroCtx -> newNode.u.Node.op = Minus;
              crReturn(1);
            }
          }

          if (ioCoroCtx -> val1 > 1 && ioCoroCtx -> val2 > 1) {
            ioCoroCtx -> newNode.value=ioCoroCtx -> val1 * ioCoroCtx -> val2;
            ioCoroCtx -> newNode.u.Node.op = Mult;
            crReturn(1);
          }    

          if (ioCoroCtx -> elmt2 -> type != Node || ioCoroCtx -> elmt2 ->u.Node.op != Mult) {
            if(ioCoroCtx -> val2 > ioCoroCtx -> val1 && ioCoroCtx -> val1 > 1 && (ioCoroCtx -> val2 % ioCoroCtx -> val1) == 0) {
              ioCoroCtx -> newNode.value=ioCoroCtx -> val2 / ioCoroCtx -> val1;
                    
              ioCoroCtx -> newNode.u.Node.ag = ioCoroCtx -> elmt2;
              ioCoroCtx -> newNode.u.Node.ad = ioCoroCtx -> elmt1;
              ioCoroCtx -> newNode.u.Node.op = Divi;
              crReturn(1);
              ioCoroCtx -> newNode.u.Node.ag = ioCoroCtx -> elmt1;
              ioCoroCtx -> newNode.u.Node.ad = ioCoroCtx -> elmt2;
            }
          }

          if (ioCoroCtx -> elmt1 -> type != Node || ioCoroCtx -> elmt1 ->u.Node.op != Mult) {
            if(ioCoroCtx -> val1 >= ioCoroCtx -> val2 && ioCoroCtx -> val2 > 1 && (ioCoroCtx -> val1 % ioCoroCtx -> val2) == 0) {
              ioCoroCtx -> newNode.value=ioCoroCtx -> val1 / ioCoroCtx -> val2;
                    
              ioCoroCtx -> newNode.u.Node.op = Divi;
              crReturn(1);
            }
          }
        }
        NB_freeSLCoroutineCtx((NB_BaseSLCoroutineCtx_t*) (ioCoroCtx -> elmt2CoroCtx));
      }
      NB_freeSLCoroutineCtx((NB_BaseSLCoroutineCtx_t*) (ioCoroCtx -> elmt1CoroCtx));
      NB_PREEMPT (ioCoroCtx -> coupleGenerator);
    }
  }
  crFinish;
  return 0;
}

// initializer:
struct LcebFixedSizeGeneratorInitializer {
  NB_BaseFunctor_t super;
};
typedef struct LcebFixedSizeGeneratorInitializer LcebFixedSizeGeneratorInitializer_t;

void lcebFixedSizeGeneratorInitializer (NB_BaseFunctor_t* iSelf, void* ioGen) {
}

// cleaner:
struct LcebFixedSizeGeneratorCleaner {
  NB_BaseFunctor_t super;
};
typedef struct LcebFixedSizeGeneratorCleaner LcebFixedSizeGeneratorCleaner_t;

void lcebFixedSizeGeneratorCleaner (NB_BaseFunctor_t* iSelf, void* ioGen) {
}


// ////////////////////////////////////////////////////////////////////////////
// all sizes algo generator
// ////////////////////////////////////////////////////////////////////////////
NB_BaseGeneratorHolder_t LcebAllSizesGeneratorHolder = {0,};

struct LcebAllSizesGenerator {
  NB_BaseGenerator_t super;
  
  // input values;
  Node_t** l;
  size_t lSize;

  Node_t* yieldedNode;  
};

 typedef struct LcebAllSizesGenerator LcebAllSizesGenerator_t;

void lcebAllSizesGenerator_run(NB_BaseGenerator_t* ioGen) {
  // this "algo" calls are used for the 
  // "the values made of N numbers generated by each of its sublists
  // of size N" part of the algorithm described on top of the file
      

  LcebAllSizesGenerator_t* generator = (LcebAllSizesGenerator_t*) ioGen;

  
  SubCombinationsGenerator_t* subListGenerator =
      (SubCombinationsGenerator_t*) NB_getFreeGenerator (&SubCombinationsGeneratorHolder);


  subListGenerator -> l = (void**) (generator -> l);
  subListGenerator -> lSize = generator -> lSize;
  subListGenerator -> maxSize = generator -> lSize;
  subListGenerator -> super.isAtEnd = 0;

  NB_PREEMPT (subListGenerator);
  while (!subListGenerator -> super.isAtEnd) {
    LcebFixedSizeGeneratorCtx_t* elmtGenerator =
      (LcebFixedSizeGeneratorCtx_t*) NB_getSLCoroutine (&LcebFixedSizeGeneratorCtxHolder);

      elmtGenerator -> l = (Node_t **)(subListGenerator -> yieldedSubL);
      elmtGenerator -> lSize = subListGenerator -> yieldedSubLSize;

      while (lcebFixedSizeGenerator (elmtGenerator)) {
        generator -> yieldedNode = elmtGenerator -> yieldedNode;
        NB_YIELD (generator);
      }        
    NB_PREEMPT (subListGenerator);
  }

  LcebFixedSizeGeneratorCtx_t* elmtGenerator =
    (LcebFixedSizeGeneratorCtx_t*) NB_getSLCoroutine (&LcebFixedSizeGeneratorCtxHolder);
  
  elmtGenerator -> l = generator -> l;
  elmtGenerator -> lSize = generator -> lSize;

  while (lcebFixedSizeGenerator (elmtGenerator)) {
    generator -> yieldedNode = elmtGenerator -> yieldedNode;
    NB_YIELD (generator);
  }
}
 
// initializer:
struct LcebAllSizesGeneratorInitializer {
  NB_BaseFunctor_t super;
};
typedef struct LcebAllSizesGeneratorInitializer LcebAllSizesGeneratorInitializer_t;

void lcebAllSizesGeneratorInitializer (NB_BaseFunctor_t* iSelf, void* ioGen) {
  SubCombinationCouplesGenerator_t* generator = (SubCombinationCouplesGenerator_t*) ioGen;
  
  generator -> super.run = &lcebAllSizesGenerator_run;
}
 
// cleaner:
struct LcebAllSizesGeneratorCleaner {
  NB_BaseFunctor_t super;
};
typedef struct LcebAllSizesGeneratorCleaner LcebAllSizesGeneratorCleaner_t;

void lcebAllSizesGeneratorCleaner (NB_BaseFunctor_t* iSelf, void* ioGen) {
}


void le_compte_est_bon(unsigned int* iL, size_t iLSize, unsigned int iTarget) {
   
  // initialize pools:
  // SubCombinationsGeneratorHolder
  SubCombinationsInitializer_t aSubCombinationsInitializer;
  aSubCombinationsInitializer.super.run = &subCombinationsInitializer;
  aSubCombinationsInitializer.subLMaxStaticSize = iLSize;

  SubCombinationsCleaner_t aSubCombinationsCleaner;
  aSubCombinationsCleaner.super.run = &subCombinationsCleaner;

  SubCombinationsGeneratorHolder.elmtConstructor = (NB_BaseFunctor_t*) &aSubCombinationsInitializer;
  SubCombinationsGeneratorHolder.elmtDestructor = (NB_BaseFunctor_t*) &aSubCombinationsCleaner;
  SubCombinationsGeneratorHolder.generatorSize = sizeof (SubCombinationsGenerator_t);

  // SubCombinationCouplesGeneratorHolder
  SubCombinationCouplesInitializer_t aSubCombinationCouplesInitializer;
  aSubCombinationCouplesInitializer.super.run = &subCombinationCouplesInitializer;
  aSubCombinationCouplesInitializer.subLMaxStaticSize = iLSize;

  SubCombinationCouplesCleaner_t aSubCombinationCouplesCleaner;
  aSubCombinationCouplesCleaner.super.run = &subCombinationCouplesCleaner;

  SubCombinationCouplesGeneratorHolder.elmtConstructor = (NB_BaseFunctor_t*) &aSubCombinationCouplesInitializer;
  SubCombinationCouplesGeneratorHolder.elmtDestructor = (NB_BaseFunctor_t*) &aSubCombinationCouplesCleaner;
  SubCombinationCouplesGeneratorHolder.generatorSize = sizeof (SubCombinationCouplesGenerator_t);

  // LcebFixedSizeGeneratorHolder
  LcebFixedSizeGeneratorCtxHolder.elmtConstructor = NULL;
  LcebFixedSizeGeneratorCtxHolder.elmtDestructor = NULL;
  LcebFixedSizeGeneratorCtxHolder.ctxSize = sizeof (LcebFixedSizeGeneratorCtx_t);

  
  // LcebAllSizesGeneratorHolder
  LcebAllSizesGeneratorInitializer_t aLcebAllSizesGeneratorInitializer;
  aLcebAllSizesGeneratorInitializer.super.run = &lcebAllSizesGeneratorInitializer;

  LcebAllSizesGeneratorCleaner_t aLcebAllSizesGeneratorCleaner;
  aLcebAllSizesGeneratorCleaner.super.run = &lcebAllSizesGeneratorCleaner;

  LcebAllSizesGeneratorHolder.elmtConstructor = (NB_BaseFunctor_t*) &aLcebAllSizesGeneratorInitializer;
  LcebAllSizesGeneratorHolder.elmtDestructor = (NB_BaseFunctor_t*) &aLcebAllSizesGeneratorCleaner;
  LcebAllSizesGeneratorHolder.generatorSize = sizeof (LcebAllSizesGenerator_t);


  BestSolution bestSolution = {0,};
  Node_t ** l = buildNodeVector(iL, iLSize);

  LcebAllSizesGenerator_t* elmtGenerator =
        (LcebAllSizesGenerator_t*) NB_getFreeGenerator (&LcebAllSizesGeneratorHolder);
  elmtGenerator -> l = l;
  elmtGenerator -> lSize = iLSize;
  elmtGenerator -> super.isAtEnd = 0;

  NB_PREEMPT (elmtGenerator);

  while (!elmtGenerator -> super.isAtEnd) {
    tryBestSolution (elmtGenerator -> yieldedNode,iTarget,&bestSolution);
    if (bestSolution.node != NULL && bestSolution.delta == 0) {
      NB_stopGenerator ((NB_BaseGenerator_t*) elmtGenerator);
      break;
    }
    NB_PREEMPT (elmtGenerator);
  }        

  if (bestSolution.node != NULL) {
    if (bestSolution.delta == 0) {
      char * c;
      c=stringFromNode(bestSolution.node);
      printf("%d = %s\n",iTarget,c);
      free(c);
    } else {
      char * c;
      c=stringFromNode(bestSolution.node);
      printf("No Solution found: nearest solution is: %d = %s\n", valueNode (bestSolution.node),c);
      free(c);
    }
    freeNode (bestSolution.node);
  } else {
    printf("No Solution found\n");
  }

  cleanNodeVector(l, iLSize);

  NB_freePool(&SubCombinationsGeneratorHolder);
  NB_freePool(&SubCombinationCouplesGeneratorHolder);
  NB_freeSLCoroutineCtxPool(&LcebFixedSizeGeneratorCtxHolder);
  NB_freePool(&LcebAllSizesGeneratorHolder);
}

void message_help(char * prog) {
  printf("%s number1 [number 2 [number 3 ...]] target\n",prog);
}
                 
int main( int argc, char ** argv) {
  int i;
  unsigned int * list;
  unsigned int t,target;

  for(i=0;i<argc;i++)
    if(!strcmp(argv[i],"-h")) {
      message_help(argv[0]);
      return 0;
    }
  
  if(argc<3) {
    message_help(argv[0]);
    return 1;
  } else {
    list = (unsigned int*) malloc (sizeof(unsigned int) * (argc - 2));
    for(i=1;i<argc - 1 ;i++) {
      if(sscanf(argv[i],"%u",&t) != 1) {
        message_help(argv[0]);
        return 1;
      } else {
        list[i-1]=t;
      }
    }
    
    if(sscanf(argv[argc-1],"%u",&target) != 1) {
      message_help(argv[0]);
      return 1;
    }
  }
  le_compte_est_bon(list,argc - 2,target);
  free(list);
  return 0;
}

