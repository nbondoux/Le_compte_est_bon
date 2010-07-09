#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* version recursive rapide (conversion en c); donne une solution avec la profondeur minimum ; affiche les r�sultats interm�diaires; 
   nombre de malloc fait optimise

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

/* solution trouv�e, et nombre d'op�rations dans la solution */

typedef struct {
  int SolNull ; //= 1 si la solution est null
  struct {
    arbre *a;
    unsigned int pr;
  } BestArbre;
} solution;


/*retourne la priorit� de l'arbre */
unsigned int  op_priority(arbre * a) {
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

/* retourne le nombre d'op�rations dans l'arbre ... */
unsigned int profondeur_arbre (arbre * a) {
  if(a-> type == Noeud)
    return profondeur_arbre(a->u.Noeud.ag) + profondeur_arbre(a->u.Noeud.ad) + 1;
  else
    return 0;
}

/*alloue (malloc) une chaine de caract�res contenant l'expression contenue par l'arbre */
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

/*malloc une table d'arbres, � partir du tableau d'entiers l, de longueur n */
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


void algo (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible, arbre * last_computed_tree) {

  unsigned int i=0;
  unsigned int profa;
  char * c;
  
  unsigned int is_sol_found = 0;
  arbre * sol_found = NULL;
  if (last_computed_tree == NULL) {
    while (i< taille_l && valeur_Noeud(l[i]) != cible) {
      i++;
    }
    if (i!= taille_l) {
      is_sol_found = 1;
      sol_found = l[i];
    }
  } else if (valeur_Noeud(last_computed_tree) == cible) {
    is_sol_found = 1;
    sol_found = last_computed_tree;
  }
  
  if(is_sol_found) {
    if (!best_res -> SolNull) {
      /*cette r�f�rence ne sera plus utilis�e */
      rm_arbre(best_res->BestArbre.a);
    }
    
    profa = profondeur_arbre(sol_found);
    c=string_arbre(sol_found);
    printf("%d = %s ; %d\n",cible,c,profa);
    free(c);
    best_res->BestArbre.a=clone_arbre(sol_found);
    best_res->BestArbre.pr=profa;
    best_res->SolNull=0;
    return ;
  } else {
    prof=prof+1;

    if(!best_res->SolNull && prof >= best_res->BestArbre.pr) {
        return;
    }
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

    for (b = a+1;b < taille_l ;++b) {

      arbre* noeud_b = l[b];
      int val_b = valeur_Noeud(noeud_b);

      // to be restored at the end of this for
      l[b] = l[taille_lMinusOne];

      nouv_arbre.u.Noeud.ad = noeud_b;    

      // for optimizations purposes,
      // the following operations are evited:
      // a + (b+c), (b+c) -a
      // a * (b*c), (b*c) /a

      if (noeud_b -> type != Noeud || noeud_b -> u.Noeud.op != Plus) {
        nouv_arbre.valeur=val_a + val_b;
        nouv_arbre.u.Noeud.op = Plus;
        {
          algo(l, taille_lMinusOne, prof, best_res, cible, &nouv_arbre);
        }
        if (val_b > val_a) {
          nouv_arbre.valeur=val_b - val_a;
          nouv_arbre.u.Noeud.ag = noeud_b;
          nouv_arbre.u.Noeud.ad = noeud_a;
          
          nouv_arbre.u.Noeud.op = Moins;
          algo(l, taille_lMinusOne, prof, best_res, cible, &nouv_arbre);
          nouv_arbre.u.Noeud.ag = noeud_a;
          nouv_arbre.u.Noeud.ad = noeud_b;
        }
      }

      if (val_a > val_b) {
         if (noeud_a -> type != Noeud || noeud_a -> u.Noeud.op != Plus) {
           nouv_arbre.valeur=val_a - val_b;
           nouv_arbre.u.Noeud.op = Moins;
           algo(l, taille_lMinusOne, prof, best_res, cible, &nouv_arbre);
         }
      }

      if (noeud_b -> type != Noeud || noeud_b -> u.Noeud.op != Mult) {
        if (val_a > 1 && val_b > 1) {
          nouv_arbre.valeur=val_a * val_b;
          nouv_arbre.u.Noeud.op = Mult;
          {
            algo(l, taille_lMinusOne, prof, best_res, cible, &nouv_arbre);
          }
        }

        if(val_b > val_a && val_a > 1 && (val_b % val_a) == 0) {
          nouv_arbre.valeur=val_b / val_a;
          nouv_arbre.u.Noeud.ag = noeud_b;
          nouv_arbre.u.Noeud.ad = noeud_a;
          nouv_arbre.u.Noeud.op = Divi;
          {
            algo(l, taille_lMinusOne, prof, best_res, cible, &nouv_arbre);
          }
        }
      }

      if (noeud_a -> type != Noeud || noeud_a -> u.Noeud.op != Mult) {
        if( val_a > val_b && val_b > 1 && (val_a % val_b) == 0) {
          nouv_arbre.valeur=val_a / val_b;
          nouv_arbre.u.Noeud.op = Divi;
          {
            algo(l, taille_lMinusOne, prof, best_res, cible, &nouv_arbre);
          }
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

  solution res;
  res.SolNull=1;

  liste_a=construct_arbre(liste,liste_n);
  
  algo(liste_a,liste_n,0,&res,cible,NULL);
  
  if(!res.SolNull) {
    c=string_arbre(res.BestArbre.a);
    printf("%d = %s\n",cible,c);
    free(c);
  }
  else
    printf("No Solution\n");
  
  if(!res.SolNull)
    rm_arbre(res.BestArbre.a);
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

