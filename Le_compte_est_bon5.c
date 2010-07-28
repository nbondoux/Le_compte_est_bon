#include <stdlib.h>
#include <stdio.h>

#include <ucontext.h>
#include <setjmp.h>

// generics for generators
#define NB_GENERATOR_STACK_LENGTH 1000

#define NB_PREEMPT(gen) \
    if (!sigsetjmp (gen -> super.nextOuterContext,0)) { \
      siglongjmp (gen -> super.nextGenContext,0); \
    }

#define NB_YIELD(gen) \
    if (!sigsetjmp (gen -> super.nextGenContext,0)) { \
      siglongjmp (gen -> super.nextOuterContext,0); \
    }

// a generator
struct NB_BaseGenerator;

// a Holder wich maintains a list of the generators of a same kind (it  linksthe head of a linked list)
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


NB_BaseGenerator_t* NB_getFreeGenerator (void (*iContextCreator) (),
                                      size_t iGeneratorSize,
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
    makecontext (&generator -> ucp,iContextCreator,0);

    if (!sigsetjmp(generator -> nextOuterContext,0)) {
      NB_Generators_Tmp_Context_Holder = generator;
      // let's initialize the generator
      setcontext (&generator -> ucp);
    }    
    generator -> pool = ioGeneratorPool;

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


//test

struct TestGenerator {
  NB_BaseGenerator_t super;
  int yieldedValue; 
};
  
typedef struct TestGenerator TestGenerator_t;

void run_gen (TestGenerator_t* iGen, int i) {
  // yield i;
  iGen -> yieldedValue = i;
  
  // go back to outer context
  NB_YIELD (iGen);

  if (i!=10) {
    run_gen (iGen, i+1);
  }
}

void loop_gen () {
  // initialization of the generator

  // this generator is ready (but not free)
  // the caller just has to initialize it and run it

  // go back to outer context
  TestGenerator_t*  generator = (TestGenerator_t*) NB_Generators_Tmp_Context_Holder;

  while (1) {
    while (generator -> super.isAtEnd) {
      NB_YIELD (generator);      
    }

    // run of the generator
    run_gen (generator,0);

    // let's put this generator in free list !
    // should lock by mutex here
    generator -> super.nextFree = generator -> super.pool -> freePoolHeadOfList;
    generator -> super.pool -> freePoolHeadOfList = &generator->super;
      
    // go back to outer context
    generator -> super.isAtEnd = 1;  
  }
}



void use_gen (TestGenerator_t* iGen1, TestGenerator_t* iGen2, int n) {
  if (n == 0)
    return;

  NB_PREEMPT(iGen1);
  if (!iGen1 -> super.isAtEnd) {
    printf ("iGen1 %d %d\n",n,iGen1 -> yieldedValue);
  }

  // let yield another value

  NB_PREEMPT(iGen2);
  if (!iGen2 -> super.isAtEnd) {
    printf ("iGen2 %d %d\n",n,iGen2 -> yieldedValue);
  }

  use_gen (iGen1,iGen2,n-1);
}


int main() {
  NB_BaseGeneratorHolder_t genPool = {0,};


  TestGenerator_t* gen1 = (TestGenerator_t*) NB_getFreeGenerator (loop_gen,
                                                                    sizeof(TestGenerator_t),
                                                                    &genPool);
  gen1->super.isAtEnd = 0;
  TestGenerator_t* gen2 = (TestGenerator_t*) NB_getFreeGenerator (loop_gen,
                                                                    sizeof(TestGenerator_t),
                                                                    &genPool);
  use_gen (gen1,gen2,20);


  TestGenerator_t* gen12 = (TestGenerator_t*) NB_getFreeGenerator (loop_gen,
                                                                    sizeof(TestGenerator_t),
                                                                   &genPool);
  gen12->super.isAtEnd = 0;
  TestGenerator_t* gen22 = (TestGenerator_t*) NB_getFreeGenerator (loop_gen,
                                                                sizeof(TestGenerator_t),
                                                                &genPool);
  gen22->super.isAtEnd = 0;
  use_gen (gen12,gen22,20);

  NB_freePool (&genPool,NULL);

  return 0;
}
