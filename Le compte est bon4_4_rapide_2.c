#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* version recursive rapide (conversion en c); donne une solution avec la profondeur minimum ; affiche les résultats intermédiaires; 
   nombre de malloc fait optimise

   only accept positive numbers in input
*/

struct struct_arbre {
  union {
    unsigned int Nombre;
    struct { 
      unsigned int valeur;
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
    oArbre->u.Nombre = iArbre->u.Nombre;
  } else {
    // iArbre.type == Noeud
    oArbre->type = Noeud;
    oArbre->u.Noeud.valeur = iArbre->u.Noeud.valeur;
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

typedef struct {
  arbre ** arbre_pool;
  arbre *** arbret_pool;
  unsigned int prof_max;
} malloc_arbre_pool;

void pool_create(malloc_arbre_pool* p, unsigned int n_input) {
  unsigned int i;
  p->prof_max=n_input-1;
  
  p->arbre_pool=(arbre **) malloc(sizeof(arbre*) *(n_input-1));
  p->arbret_pool=(arbre ***) malloc(sizeof(arbre **) *(n_input-1));
  
  for(i=1;i<n_input;i++) {
    p->arbret_pool[i-1]=(arbre **) malloc (sizeof(arbre *)*(n_input-i));
    p->arbre_pool[i-1]=create_arbreElem();
  }
}

void pool_delete(malloc_arbre_pool* p) {
  unsigned int i;
  for(i=1;i<=p->prof_max;i++) {
    free(p->arbre_pool[i-1]);
    free(p->arbret_pool[i-1]);
  }
  free(p->arbret_pool);
  free(p->arbre_pool);
}

inline arbre ** pool_get_arbretable(malloc_arbre_pool* p, unsigned int pr) {
  return p->arbret_pool[pr-1];
}

inline arbre * pool_get_arbre(malloc_arbre_pool* p, unsigned int pr) {
  return p->arbre_pool[pr-1];
}

/* solution trouvée, et nombre d'opérations dans la solution */

typedef struct {
  int SolNull ; //= 1 si la solution est null
  struct {
    arbre *a;
    unsigned int pr;
  } BestArbre;
} solution;


/*retourne la priorité de l'arbre */
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

/* retourne le nombre d'opérations dans l'arbre ... */
unsigned int profondeur_arbre (arbre * a) {
  if(a-> type == Noeud)
    return profondeur_arbre(a->u.Noeud.ag) + profondeur_arbre(a->u.Noeud.ad) + 1;
  else
    return 0;
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
    sprintf(n,"%u",a->u.Nombre);
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
    a[i]->u.Nombre = l[i];
  }
  return a;
}

inline unsigned int valeur_Noeud (arbre * a) {
  if(a->type==Noeud) {
    return a->u.Noeud.valeur;
  } else {
    return a->u.Nombre;
  }
}


void algo (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible, malloc_arbre_pool* p);
void algo2 (arbre ** l,unsigned int taille_l, unsigned int prof, solution*  best_res, int cible, malloc_arbre_pool* p);


void algo (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible, malloc_arbre_pool* p) {
  unsigned int i=0;
  unsigned int profa;
  char * c;
  while (i< taille_l && valeur_Noeud(l[i]) != cible) {
    i++;
  }
  if (i!= taille_l) {
    /*cette référence ne sera plus utilisée */
    if(!best_res->SolNull)
      rm_arbre(best_res->BestArbre.a);
      
    profa = profondeur_arbre(l[i]);
    c=string_arbre(l[i]);
    printf("%d = %s ; %d\n",cible,c,profa);
    free(c);
    best_res->BestArbre.a=clone_arbre(l[i]);
    best_res->BestArbre.pr=profa;
    best_res->SolNull=0;
    return ;
  } else {
    if(!best_res->SolNull) {
      if(prof + 1 < best_res->BestArbre.pr && taille_l != 1)
        algo2(l,taille_l,prof+1,best_res,cible,p);
      return;
    } else {
      if(taille_l != 1)
        algo2(l,taille_l,prof+1,best_res,cible,p);
      return;
    }
  }
}

void algo2 (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible, malloc_arbre_pool* p) {
  short exception = 0;
  unsigned int a,b,i;
  int val_a,val_b;
  arbre ** nouv;

  nouv = pool_get_arbretable(p,prof);
  
  short new_list = 1;
  for(a = 0;a < (taille_l - 1) && ! exception;a++) {
    short new_a = 1;
    for(b = a+1;b < taille_l && ! exception;b++) {
      if (new_list) {
        new_list = 0;
        new_a = 0;
        for(i=0;i < b ;i++)
          nouv[i] = l[i];
        for(i=b+1;i < taille_l  ;i++)
          nouv[i-1] = l[i];
      } else {
        if (new_a) {
          new_a = 0;
          // if we are here, a != 0
          for(i=a-1; i < b ;i++) {
            nouv[i] = l[i];
          }
          for(i=b+1; i < taille_l  ;i++) {
            nouv[i-1] = l[i];
          }
        } else {
          // we change only b
          // b-1 != a
          nouv [b-1] = l[b-1];
        }
      }
      val_a = valeur_Noeud(l[a]);
      val_b = valeur_Noeud(l[b]);
      
      nouv[a] = pool_get_arbre(p,prof);
      nouv[a]->type=Noeud;
      nouv[a]->u.Noeud.valeur=val_a + val_b;
      nouv[a]->u.Noeud.ag = l[a];
      nouv[a]->u.Noeud.ad = l[b];
      nouv[a]->u.Noeud.op = Plus;
      {
        algo(nouv,taille_l - 1, prof, best_res, cible,p);
        
        if (best_res->SolNull == 0 && prof >= best_res->BestArbre.pr) {
          exception=1;
          break;
        }
      }

      nouv[a] = pool_get_arbre(p,prof);
      nouv[a]->type=Noeud;
      if (val_a > val_b) {
        nouv[a]->u.Noeud.valeur=val_a - val_b;
        nouv[a]->u.Noeud.ag = l[a];
        nouv[a]->u.Noeud.ad = l[b];
      } else {
        nouv[a]->u.Noeud.valeur=val_b - val_a;
        nouv[a]->u.Noeud.ag = l[b];
        nouv[a]->u.Noeud.ad = l[a];
      }    
      nouv[a]->u.Noeud.op = Moins;
      {
        algo(nouv,taille_l - 1, prof, best_res, cible,p);
        
        if(best_res->SolNull == 0 && prof >= best_res->BestArbre.pr) {
          exception=1;
          break;
        }
      }        

      if (val_a != 1 && val_b != 1) {
        nouv[a] = pool_get_arbre(p,prof);
        nouv[a]->type=Noeud;
        nouv[a]->u.Noeud.valeur=val_a * val_b;
        nouv[a]->u.Noeud.ag = l[a];
        nouv[a]->u.Noeud.ad = l[b];
        nouv[a]->u.Noeud.op = Mult;
        {
          algo(nouv,taille_l - 1, prof, best_res, cible,p);

          if(best_res->SolNull == 0 && prof >= best_res->BestArbre.pr) {
            exception=1;
            break;
          }            
        }
      }

      if(val_b > 1 && (val_a % val_b) == 0) {
        nouv[a] = pool_get_arbre(p,prof);
        nouv[a]->type=Noeud;
        nouv[a]->u.Noeud.valeur=val_a / val_b;
        nouv[a]->u.Noeud.ag = l[a];
        nouv[a]->u.Noeud.ad = l[b];
        nouv[a]->u.Noeud.op = Divi;
        {
          algo(nouv,taille_l - 1, prof, best_res, cible,p);
          
          if(best_res->SolNull == 0 && prof >= best_res->BestArbre.pr) {
            exception=1;
            break;
          }
        }
      }
      
      nouv[a] = pool_get_arbre(p,prof);
      nouv[a]->type=Noeud;
      nouv[a]->u.Noeud.valeur=val_b - val_a;
      nouv[a]->u.Noeud.ag = l[b];
      nouv[a]->u.Noeud.ad = l[a];
      nouv[a]->u.Noeud.op = Moins;
      {
        algo(nouv,taille_l - 1, prof, best_res, cible,p);
        
        if(best_res->SolNull == 0 && prof >= best_res->BestArbre.pr) {
          exception=1;
          break;         
        }
      }
      
      if(val_a > 1 && (val_b % val_a) == 0) {
        nouv[a] = pool_get_arbre(p,prof);
        nouv[a]->type=Noeud;
        nouv[a]->u.Noeud.valeur=val_b / val_a;
        nouv[a]->u.Noeud.ag = l[b];
        nouv[a]->u.Noeud.ad = l[a];
        nouv[a]->u.Noeud.op = Divi;
        {
          algo(nouv,taille_l - 1, prof, best_res, cible,p);
              
          if(best_res->SolNull == 0 && prof >= best_res->BestArbre.pr) {
            exception=1;
            break;
          }
        }
      }
    }
  }
  return;
}

void le_compte_est_bon (unsigned int * liste, unsigned int liste_n, unsigned int cible)  {
  arbre ** liste_a;
  char * c;
  unsigned int i;

  solution res;
  res.SolNull=1;

  malloc_arbre_pool p;
  
  pool_create(&p,liste_n);
  
  liste_a=construct_arbre(liste,liste_n);
  
  algo(liste_a,liste_n,0,&res,cible,&p);
  
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
  pool_delete(&p);
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

