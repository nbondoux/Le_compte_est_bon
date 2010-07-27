#include <stdlib.h>
#include <stdio.h>

#include <ucontext.h>
#include <setjmp.h>



// generics for generators
#define NB_GENERATOR_STACK_LENGTH 1000

// a generator
struct NB_BaseGenerator;

// a Holder wich maintains a list of the generators of a same kind (it  linksthe head of a linked list)
struct NB_BaseGeneratorHolder;

// technical value needed by makecontext; should be protected with a mutex ..
static struct NB_BaseGenerator* NB_Generators_Tmp_Context_Holder;

struct NB_BaseGeneratorHolder {
  struct NB_BaseGenerator * headOfList;
  // should also hold a mutex for update
};
  

struct NB_BaseGenerator {
  struct NB_BaseGenerator* nextFree;
  struct NB_BaseGenerator* next;
  struct NB_BaseGeneratorHolder* freePool;

  int hasFinished;

  sigjmp_buf nextGenContext;
  sigjmp_buf nextOuterContext;

  ucontext_t ucp;
  void* stack[NB_GENERATOR_STACK_LENGTH];
};

typedef struct NB_BaseGenerator NB_BaseGenerator_t;
typedef struct NB_BaseGeneratorHolder NB_BaseGeneratorHolder_t;


NB_BaseGenerator_t* getFreeGenerator (void (*iContextCreator) (),
                                      size_t iGeneratorSize,
                                      NB_BaseGeneratorHolder_t* ioFreeGeneratorPool,
                                      NB_BaseGeneratorHolder_t* ioGeneratorPool) {
  if (ioFreeGeneratorPool -> headOfList != NULL) {
    NB_BaseGenerator_t* generator = ioFreeGeneratorPool -> headOfList;
    ioFreeGeneratorPool -> headOfList = generator -> nextFree;
    generator -> nextFree = NULL;
    generator -> hasFinished = 0;
    return generator;
  } else {
    NB_BaseGenerator_t* generator = (NB_BaseGenerator_t*) calloc (1,iGeneratorSize);
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
    generator -> freePool = ioFreeGeneratorPool;

    // should lock by mutex here
    generator -> next = ioGeneratorPool -> headOfList;
    ioGeneratorPool -> headOfList = generator;

    return generator;
  }
}

void freePool (NB_BaseGeneratorHolder_t* ioFreeGeneratorPool, NB_BaseGeneratorHolder_t* ioGeneratorPool)
{
  // should lock by mutex here ?
  NB_BaseGenerator_t* generator = ioGeneratorPool -> headOfList;

  while (generator != NULL) {
    NB_BaseGenerator_t* nextGenerator = generator -> next;
    free (generator);
    generator = nextGenerator;
  }

  // should lock by mutex here
  ioFreeGeneratorPool ->headOfList = NULL;
  ioGeneratorPool -> headOfList = NULL;
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

  if (i!=10) {
    // go back to outer context
    if (!sigsetjmp (iGen -> super.nextGenContext,0)) {
      siglongjmp (iGen -> super.nextOuterContext,0);
    }
    run_gen (iGen, i+1);
  }
}

void loop_gen () {
  // initialization of the generator

  // this generator is ready (but not free)
  // the caller just has to initialize it and run it

  // go back to outer context
  TestGenerator_t*  generator = (TestGenerator_t*) NB_Generators_Tmp_Context_Holder;

  // go back to outer context
  if (!sigsetjmp (generator -> super.nextGenContext,0)) {
    siglongjmp (generator -> super.nextOuterContext,0);
  }

  while (1) {
    generator -> super.hasFinished = 0;
    // run of the generator
    run_gen (generator,0);

    // let's put this generator in free list !
    // should lock by mutex here
    generator -> super.nextFree = generator -> super.freePool -> headOfList;
    generator -> super.freePool -> headOfList = &generator->super;
      
    // go back to outer context
    generator -> super.hasFinished = 1;

    if (!sigsetjmp (generator -> super.nextGenContext,0)) {
      siglongjmp (generator -> super.nextOuterContext,0);
    }
   
  }
  

}



void use_gen (TestGenerator_t* iGen1, TestGenerator_t* iGen2, int n) {
  if (n == 0)
    return;

  ucontext_t innerContext;
  
  if (!iGen1 -> super.hasFinished) {
    if (!sigsetjmp (iGen1 -> super.nextOuterContext,0)) {
      siglongjmp (iGen1 -> super.nextGenContext,0);
    }
    printf ("iGen1 %d %d\n",n,iGen1 -> yieldedValue);
  }

  // let yield another value

  if (!iGen2 -> super.hasFinished) {
    if (!sigsetjmp (iGen2 -> super.nextOuterContext,0)) {
      siglongjmp (iGen2 -> super.nextGenContext,0);
    }
    printf ("iGen2 %d %d\n",n,iGen2 -> yieldedValue);
  }

  use_gen (iGen1,iGen2,n-1);
}


int main() {
  NB_BaseGeneratorHolder_t genPool = {0,};
  NB_BaseGeneratorHolder_t freeGenPool = {0,};


  TestGenerator_t* gen1 = (TestGenerator_t*) getFreeGenerator (loop_gen,
                                                                    sizeof(TestGenerator_t),
                                                                    &freeGenPool,
                                                                    &genPool);

  TestGenerator_t* gen2 = (TestGenerator_t*) getFreeGenerator (loop_gen,
                                                                    sizeof(TestGenerator_t),
                                                                    &freeGenPool,
                                                                    &genPool);
  use_gen (gen1,gen2,20);


  TestGenerator_t* gen12 = (TestGenerator_t*) getFreeGenerator (loop_gen,
                                                                    sizeof(TestGenerator_t),
                                                                    &freeGenPool,
                                                                    &genPool);

  TestGenerator_t* gen22 = (TestGenerator_t*) getFreeGenerator (loop_gen,
                                                                sizeof(TestGenerator_t),
                                                                &freeGenPool,
                                                                &genPool);

  use_gen (gen12,gen22,20);
  return 0;
}
