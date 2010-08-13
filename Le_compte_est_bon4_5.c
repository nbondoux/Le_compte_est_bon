#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* version recursive rapide (conversion en c); donne une solution avec la profondeur minimum, en faisant des recherches de moins en moins limitées en profondeur
   affiche les solutions les plus proches trouvées

   only accept positive numbers in input
*/

struct struct_arbre {
  unsigned int valeur;
  union {
    struct { 
      struct struct_arbre * ag;
      struct struct_arbre * ad;
      enum {Plus, Moins, Mult, Divi}  op;
    } Noeud;
  } u;
  enum {Nombre, Noeud} type;
};

typedef struct struct_arbre arbre;

inline arbre * create_arbreElem() {
  arbre *a;
  a = (arbre *) malloc (sizeof (arbre));
  a->type=Nombre;
  return a;  
}

// copy inner values of struct_arbre
inline void copy_arbre_inner (const arbre* iArbre,
                              arbre* oArbre) {
  if (iArbre->type == Nombre) {
    oArbre->type = Nombre;
    oArbre->valeur = iArbre->valeur;
  } else {
    // iArbre.type == Noeud
    oArbre->type = Noeud;
    oArbre->valeur = iArbre->valeur;
    oArbre->u.Noeud.op = iArbre->u.Noeud.op;
  }
}

arbre * clone_arbre(const arbre * iArbre) {
  arbre* clonedArbre = create_arbreElem ();
  copy_arbre_inner (iArbre,clonedArbre);
  
  if (iArbre->type == Noeud) {
    clonedArbre->u.Noeud.ag = clone_arbre (iArbre->u.Noeud.ag);
    clonedArbre->u.Noeud.ad = clone_arbre (iArbre->u.Noeud.ad);
  }
  return clonedArbre;
}

void rm_arbre(arbre * a) {
  if(a->type==Noeud) {
    rm_arbre (a->u.Noeud.ad);
    rm_arbre (a->u.Noeud.ag);
  }
  free(a);
}

/*retourne la priorité de l'arbre */
unsigned int op_priority(arbre * a) {
  if(a-> type == Noeud) {
    if(a -> u.Noeud.op == Plus || a -> u.Noeud.op == Moins)
      return 1;
    else if(a -> u.Noeud.op == Mult || a -> u.Noeud.op == Divi)
      return 2;
    return 0;
  } else {
    return 100;
  }
}

/*alloue (malloc) une chaine de caractères contenant l'expression contenue par l'arbre */
char * string_arbre (arbre * a) {
  char * c1, *c2, *c;
  unsigned int i1,i2;
  if(a-> type == Noeud) {
    c1=string_arbre(a->u.Noeud.ag);
    i1=strlen(c1);
    c2=string_arbre(a->u.Noeud.ad);
    i2=strlen(c2);
    c=(char *) malloc( sizeof(char)*(i1+i2+4+1+1));
    c[0]='\0';

    if(op_priority (a->u.Noeud.ag) < op_priority (a)) {
      strcat(c,"(");
      strcat(c,c1);
      strcat(c,")");
    } else
      strcat(c,c1);
      
    if(a->u.Noeud.op == Plus)
      strcat(c,"+");
    else if(a->u.Noeud.op == Moins)
      strcat(c,"-");
    else if(a->u.Noeud.op == Mult)
      strcat(c,"*");
    else if(a->u.Noeud.op == Divi)
      strcat(c,"/");
    
    if(op_priority (a->u.Noeud.ad) <= op_priority (a)) {
      strcat(c,"(");
      strcat(c,c2);
      strcat(c,")");
    } else
      strcat(c,c2);
    free(c1);
    free(c2);
  } else {
    char n[500];
    sprintf(n,"%u",a->valeur);
    c = (char *) malloc (sizeof(char) * (strlen(n) + 1));
    strcpy(c,n);
  }
  return c;
}

/*malloc une table d'arbres, à partir du tableau d'entiers l, de longueur n */
arbre ** construct_arbre(unsigned int * l, unsigned int n) {
  unsigned int i;
  arbre ** a;
  a= (arbre **) malloc (sizeof(arbre *)* n);
  
  for(i=0;i<n;i++) {
    a[i]=create_arbreElem();
    a[i]->type=Nombre;
    a[i]->valeur= l[i];
  }
  return a;
}

inline unsigned int valeur_Noeud (arbre * a) {
  return a->valeur;
}

typedef struct {
  arbre *a;
  unsigned int delta;
} BestSolution;

inline void tryBestSolution (arbre *iCurrentTree, unsigned int iTarget, BestSolution* ioBestSolution) {
  unsigned int currentValue = valeur_Noeud (iCurrentTree);
  unsigned int delta = ioBestSolution -> delta;
  if (ioBestSolution -> a == NULL ||
      ((currentValue >= iTarget && currentValue - iTarget < ioBestSolution -> delta) ||
       (currentValue < iTarget && iTarget - currentValue < delta))) {
    
    // new bestSolutionFound 
    if (ioBestSolution -> a != NULL) {
      rm_arbre(ioBestSolution -> a);
    }
    
    char * c=string_arbre(iCurrentTree);
    printf("Best solution so far: %d = %s \n",currentValue,c);
    free(c);
    ioBestSolution -> a = clone_arbre(iCurrentTree);

    if (currentValue >= iTarget) {
      ioBestSolution -> delta = currentValue - iTarget;
    } else {
      ioBestSolution -> delta = iTarget - currentValue;
    }    
  }
}


void algo (arbre ** l,unsigned int taille_l, unsigned int checkedProf, unsigned int prof, BestSolution * best_res, unsigned int cible, arbre * last_computed_tree, int left_bound_for_new_productions) {

  if (prof == checkedProf) {
    if (last_computed_tree == NULL) {
      for (unsigned int i =0; i< taille_l;++i) {
        tryBestSolution (l[i],cible,best_res);
      }
    } else {
      tryBestSolution (last_computed_tree,cible,best_res);
    }
  }

  prof=prof+1;

  if (prof > checkedProf || (best_res -> a != NULL && best_res -> delta == 0)) {
    return;
  }

  unsigned int a,b;

  arbre nouv_arbre;
  nouv_arbre.type = Noeud;
  
  unsigned int taille_lMinusOne = taille_l - 1;

  for(a = 0;a < taille_lMinusOne;++a) {

    arbre* noeud_a = l[a];
    int val_a = valeur_Noeud(noeud_a); 

    nouv_arbre.type=Noeud;
    
    // to be restored at the end of this for
    l[a] = &nouv_arbre;
    nouv_arbre.u.Noeud.ag = noeud_a;
    
    b = a+1;

    // this optimization seems ugly and could be maybe be better ?
    // the principle is that: if we are in state:
    // a1 ..... am am+1 ... an where am+1 has just been produced,
    // we can expect the cases with all combinations of a1...am have already
    // been tested (by current design)
    if (left_bound_for_new_productions > b) {
      b = left_bound_for_new_productions;
    }
    
    for (;b < taille_l ;++b) {

      arbre* noeud_b = l[b];
      int val_b = valeur_Noeud(noeud_b);

      // to be restored at the end of this for
      l[b] = l[taille_lMinusOne];

      nouv_arbre.u.Noeud.ad = noeud_b;    

      // for optimizations purposes,
      // the following operations are skipped:
      // a + (b+c), (b+c) -a
      // a * (b*c), a*(b/c), (a/c)*b

      if (val_a > 0 && val_b > 0 &&
          (noeud_b -> type != Noeud || noeud_b -> u.Noeud.op != Plus) ) {
        nouv_arbre.valeur=val_a + val_b;
        nouv_arbre.u.Noeud.op = Plus;
        {
          algo(l, taille_lMinusOne, checkedProf, prof, best_res, cible, &nouv_arbre, a);
        }
        if (val_b > val_a) {
          nouv_arbre.valeur=val_b - val_a;
          nouv_arbre.u.Noeud.ag = noeud_b;
          nouv_arbre.u.Noeud.ad = noeud_a;
          
          nouv_arbre.u.Noeud.op = Moins;
          algo(l, taille_lMinusOne, checkedProf, prof, best_res, cible, &nouv_arbre, a);
          nouv_arbre.u.Noeud.ag = noeud_a;
          nouv_arbre.u.Noeud.ad = noeud_b;
        }
      }

      if (val_a >= val_b && val_b > 0) {
         if (noeud_a -> type != Noeud || noeud_a -> u.Noeud.op != Plus) {
           nouv_arbre.valeur=val_a - val_b;
           nouv_arbre.u.Noeud.op = Moins;
           algo(l, taille_lMinusOne, checkedProf, prof, best_res, cible, &nouv_arbre, a);
         }
      }

      if (val_a > 1 && val_b > 1) {
        if ((noeud_b -> type != Noeud || (noeud_b -> u.Noeud.op != Mult && noeud_b -> u.Noeud.op != Divi)) &&
            (noeud_a -> type != Noeud || noeud_a -> u.Noeud.op != Divi)) {
          nouv_arbre.valeur=val_a * val_b;
          nouv_arbre.u.Noeud.op = Mult;
          {
            algo(l, taille_lMinusOne, checkedProf, prof, best_res, cible, &nouv_arbre, a);
          }
        }
      }

      if(val_b > val_a && val_a > 1 && (val_b % val_a) == 0) {
        nouv_arbre.valeur=val_b / val_a;
        nouv_arbre.u.Noeud.ag = noeud_b;
        nouv_arbre.u.Noeud.ad = noeud_a;
        nouv_arbre.u.Noeud.op = Divi;
        {
          algo(l, taille_lMinusOne, checkedProf, prof, best_res, cible, &nouv_arbre, a);
        }
        nouv_arbre.u.Noeud.ag = noeud_a;
        nouv_arbre.u.Noeud.ad = noeud_b;
      }

      if( val_a >= val_b && val_b > 1 && (val_a % val_b) == 0) {
        nouv_arbre.valeur=val_a / val_b;
        nouv_arbre.u.Noeud.op = Divi;
        {
          algo(l, taille_lMinusOne, checkedProf, prof, best_res, cible, &nouv_arbre, a);
        }
      }
      
      l[b] = noeud_b;
    }
    l[a] = noeud_a;
  }
  return;
}

void le_compte_est_bon (unsigned int * liste, unsigned int liste_n, unsigned int cible)  {
  arbre ** liste_a;
  char * c;
  unsigned int i;

  BestSolution res = {0,};

  liste_a=construct_arbre(liste,liste_n);

  for (i=0; i< liste_n;++i) {
    // let's try with growing max prof
    algo(liste_a,liste_n,i,0,&res,cible,NULL,0);
  }

  if (res.a != NULL && res.delta == 0) {
    c=string_arbre(res.a);
    printf("%d = %s\n",cible,c);
    free(c);
  } else if (res.a != NULL) {
    c=string_arbre(res.a);
    printf("No Solution found: nearest solution is: %d = %s\n", valeur_Noeud (res.a),c);
    free(c);
  } else {
    printf("No Solution found\n");
  }
  if(res.a != NULL)
    rm_arbre(res.a);
  for(i=0;i<liste_n;i++)
    rm_arbre(liste_a[i]);
  free(liste_a);
}

void message_help(char * prog) {
  printf("%s number1 [number 2 [number 3 ...]] target\n",prog);
}
                 
int main( int argc, char ** argv) {
  int i;
  unsigned int * liste;
  unsigned int t,cible;

  for(i=0;i<argc;i++)
    if(!strcmp(argv[i],"-h")) {
      message_help(argv[0]);
      return 0;
    }
  
  if(argc<3) {
    message_help(argv[0]);
    return 1;
  } else {
    liste = (unsigned int*) malloc (sizeof(unsigned int) * (argc - 2));
    for(i=1;i<argc - 1 ;i++) {
      if(sscanf(argv[i],"%u",&t) != 1) {
        message_help(argv[0]);
        return 1;
      } else {
        liste[i-1]=t;
      }
    }
    
    if(sscanf(argv[argc-1],"%u",&cible) != 1) {
      message_help(argv[0]);
      return 1;
    }
  }
  le_compte_est_bon(liste,argc - 2,cible);
  free(liste);
  return 0;
}

