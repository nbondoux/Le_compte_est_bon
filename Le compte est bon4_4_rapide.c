#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* version recursive rapide (conversion en c); donne une solution avec la profondeur minimum ; affiche les r�sultats interm�diaires; 
   nombre de malloc fait optimise
*/



struct struct_arbre {
  union
  {
    int Nombre;
    struct
    { 
      int valeur;
      struct struct_arbre * ag;
      struct struct_arbre * ad;
      enum {Plus, Moins, Mult, Divi}  op;
    } Noeud;
  } u;
  enum {Nombre, Noeud} type;
  unsigned int nb_refs;
};

typedef struct struct_arbre arbre;


  
inline arbre * create_arbre()
{
  arbre *a;
  a = (arbre *) malloc (sizeof (arbre));
  a->nb_refs=1;
  a->type=Nombre;
  return a;  
}

inline arbre * copy_arbre(arbre * a)
{
  a->nb_refs++;
  return a;
}

void rm_arbre_not_inline(arbre * a);

inline void rm_arbre(arbre * a)
{
  a->nb_refs--;
  if(a->nb_refs==0)
    {
      if(a->type==Noeud)
	{
	  rm_arbre_not_inline (a->u.Noeud.ad);
	  rm_arbre_not_inline (a->u.Noeud.ag);
	}
      free(a);
    }
}

void rm_arbre_not_inline(arbre * a) {
  rm_arbre (a);
}

typedef struct
{
  arbre ** arbre_pool;
  arbre *** arbret_pool;
  unsigned int prof_max;
}   malloc_arbre_pool;

void pool_create(malloc_arbre_pool* p, unsigned int n_input)
{
  unsigned int i;
  p->prof_max=n_input-1;
  
  p->arbre_pool=(arbre **) malloc(sizeof(arbre*) *(n_input-1));
  p->arbret_pool=(arbre ***) malloc(sizeof(arbre **) *(n_input-1));
  
  for(i=1;i<n_input;i++)
    {
      p->arbret_pool[i-1]=(arbre **) malloc (sizeof(arbre *)*(n_input-i));
      p->arbre_pool[i-1]=create_arbre();
      (p->arbre_pool[i-1])->type=Nombre;
    }
}

void pool_delete(malloc_arbre_pool* p)
{
  unsigned int i;
  for(i=1;i<=p->prof_max;i++)
    {
      rm_arbre(p->arbre_pool[i-1]);
      free(p->arbret_pool[i-1]);
    }
  free(p->arbret_pool);
  free(p->arbre_pool);
}

arbre ** pool_get_arbretable(malloc_arbre_pool* p, unsigned int pr)
{
  return p->arbret_pool[pr-1];
}

arbre * pool_get_arbre(malloc_arbre_pool* p, unsigned int pr)
{
  if((p->arbre_pool[pr-1])->nb_refs>1) //si l'arbre est utilis� ailleurs
    {
      rm_arbre(p->arbre_pool[pr-1]);
      p->arbre_pool[pr-1]=create_arbre();
    }
  else
    {
      if((p->arbre_pool[pr-1])->type==Noeud)
	{
	  rm_arbre ((p->arbre_pool[pr-1])->u.Noeud.ad);
	  rm_arbre ((p->arbre_pool[pr-1])->u.Noeud.ag);
          (p->arbre_pool[pr-1])->type=Nombre;
	}
    }
  return p->arbre_pool[pr-1];
}

/* solution trouv�e, et nombre d'op�rations dans la solution */

typedef struct {
  int SolNull ; //= 1 si la solution est null
  unsigned int solId ; //unique id to see if two solutions are distincts
  struct {
    arbre *a;
    unsigned int pr;
  }BestArbre;
} solution;



/*retourne la priorit� de l'arbre */
unsigned int  op_priority(arbre * a)
{
  if(a-> type == Noeud)
    {
      if(a -> u.Noeud.op == Plus || a -> u.Noeud.op == Moins)
	return 1;
      else if(a -> u.Noeud.op == Mult || a -> u.Noeud.op == Divi)
	return 2;
      return 0;
    }
  else
    {
      return 100;
    }
}

/* retourne le nombre d'op�rations dans l'arbre ... */
unsigned int profondeur_arbre (arbre * a)
{
  if(a-> type == Noeud)
    return profondeur_arbre(a->u.Noeud.ag) + profondeur_arbre(a->u.Noeud.ad) + 1;
  else
    return 0;
}

/*alloue (malloc) une chaine de caract�res contenant l'expression contenue par l'arbre */
char * string_arbre (arbre * a)
{
  char * c1, *c2, *c;
  unsigned int i1,i2;
  if(a-> type == Noeud)
    {
      c1=string_arbre(a->u.Noeud.ag);
      i1=strlen(c1);
      c2=string_arbre(a->u.Noeud.ad);
      i2=strlen(c2);
      c=(char *) malloc( sizeof(char)*(i1+i2+4+1+1));
      c[0]='\0';
      if(op_priority (a->u.Noeud.ag) < op_priority (a))
	{
	  strcat(c,"(");
	  strcat(c,c1);
	  strcat(c,")");
	}
      else
	strcat(c,c1);
      
      if(a->u.Noeud.op == Plus)
	strcat(c,"+");
      else if(a->u.Noeud.op == Moins)
	strcat(c,"-");
      else if(a->u.Noeud.op == Mult)
	strcat(c,"*");
      else if(a->u.Noeud.op == Divi)
	strcat(c,"/");
      
      if(op_priority (a->u.Noeud.ad) <= op_priority (a))
	{
	  strcat(c,"(");
	  strcat(c,c2);
	  strcat(c,")");
	}
      else
	strcat(c,c2);
      free(c1);
      free(c2);
    }
  else
    {
      char n[500];
      sprintf(n,"%d",a->u.Nombre);
      c = (char *) malloc (sizeof(char) * (strlen(n) + 1));
      strcpy(c,n);
    }
  return c;
}

/*malloc une table d'arbres, � partir du tableau d'entiers l, de longueur n */
arbre ** construct_arbre(int * l, unsigned int n)
{
  unsigned int i;
  arbre ** a;
  a= (arbre **) malloc (sizeof(arbre *)* n);
  
  for(i=0;i<n;i++)
    {
      a[i]=create_arbre();
      a[i]->type=Nombre;
      a[i]->u.Nombre = l[i];
    }
  return a;
}

inline int valeur_Noeud (arbre * a)
{
  if(a->type==Noeud)
    {
      return a->u.Noeud.valeur;
    }
  else
    {
      return a->u.Nombre;
    }

}


void algo (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible, malloc_arbre_pool* p);
void algo2 (arbre ** l,unsigned int taille_l, unsigned int prof, solution*  best_res, int cible, malloc_arbre_pool* p);


void algo (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible, malloc_arbre_pool* p)
{
  unsigned int i=0;
  unsigned int profa;
  char * c;
  while (i< taille_l && valeur_Noeud(l[i]) != cible)
    {
      i++;
    }
  if (i!= taille_l)
    {
      /*cette r�f�rence ne sera plus utilis�e */
      if(!best_res->SolNull)
	rm_arbre(best_res->BestArbre.a);
      
      profa = profondeur_arbre(l[i]);
      c=string_arbre(l[i]);
      printf("%d = %s ; %d\n",cible,c,profa);
      free(c);
      best_res->BestArbre.a=copy_arbre(l[i]);
      best_res->BestArbre.pr=profa;
      best_res->SolNull=0;
      ++best_res->solId;
      return ;
    }
  else
    {
      if(!best_res->SolNull)
	{
	  if(prof + 1 < best_res->BestArbre.pr && taille_l != 1)
	    algo2(l,taille_l,prof+1,best_res,cible,p);
          return;
	}
      else
	{
	  if(taille_l != 1)
	    algo2(l,taille_l,prof+1,best_res,cible,p);
          return;
	}
    }
}

void algo2 (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible, malloc_arbre_pool* p)
{
  short exception = 0;
  unsigned int a,b,i;
  int val_a,val_b;
  arbre ** nouv;

  nouv = pool_get_arbretable(p,prof);
  
  short new_list = 1;
  for(a = 0;a < (taille_l - 1) && ! exception;a++) {
    short new_a = 1;
    for(b = a+1;b < taille_l && ! exception;b++)
      {
        if (new_list) {
          new_list = 0;
          new_a = 0;
          for(i=0;i < b ;i++)
            nouv[i] = copy_arbre(l[i]);
          for(i=b+1;i < taille_l  ;i++)
            nouv[i-1] = copy_arbre(l[i]);
        } else {
          if (new_a) {
            new_a = 0;
            // if we are here, a != 0
            for(i=a-1; i < b ;i++) {
              rm_arbre (nouv[i]);
              nouv[i] = copy_arbre(l[i]);
            }
            for(i=b+1; i < taille_l  ;i++) {
              rm_arbre (nouv[i-1]);
              nouv[i-1] = copy_arbre(l[i]);
            }
          } else {
            // we change only b
            // b-1 != a
            rm_arbre (nouv[b-1]);
            nouv [b-1] = copy_arbre(l[b-1]);
          }
        }
        val_a = valeur_Noeud(l[a]);
        val_b = valeur_Noeud(l[b]);
	  
        rm_arbre(nouv[a]);
        nouv[a] = copy_arbre(pool_get_arbre(p,prof));
        nouv[a]->type=Noeud;
        nouv[a]->u.Noeud.valeur=val_a + val_b;
        nouv[a]->u.Noeud.ag = copy_arbre(l[a]);
        nouv[a]->u.Noeud.ad = copy_arbre(l[b]);
        nouv[a]->u.Noeud.op = Plus;
        {
          unsigned int oldSolId = best_res->solId;
          algo(nouv,taille_l - 1, prof, best_res, cible,p);

          if (best_res->solId != oldSolId)
            {
              if(prof >= best_res->BestArbre.pr)
                {
                  exception=1;
                  break;
                }
            }
        }
        rm_arbre(nouv[a]);
        nouv[a] = copy_arbre(pool_get_arbre(p,prof));
        nouv[a]->type=Noeud;
        nouv[a]->u.Noeud.valeur=val_a - val_b;
        nouv[a]->u.Noeud.ag = copy_arbre(l[a]);
        nouv[a]->u.Noeud.ad = copy_arbre(l[b]);
        nouv[a]->u.Noeud.op = Moins;
        {
          unsigned int oldSolId = best_res->solId;
          algo(nouv,taille_l - 1, prof, best_res, cible,p);
          
          if(best_res->solId != oldSolId)
            {
              if(prof >= best_res->BestArbre.pr)
                {
                  exception=1;
                  break;
                }
            }
        }

        rm_arbre(nouv[a]);
        nouv[a] = copy_arbre(pool_get_arbre(p,prof));
        nouv[a]->type=Noeud;
        nouv[a]->u.Noeud.valeur=val_a * val_b;
        nouv[a]->u.Noeud.ag = copy_arbre(l[a]);
        nouv[a]->u.Noeud.ad = copy_arbre(l[b]);
        nouv[a]->u.Noeud.op = Mult;
        {
          unsigned int oldSolId = best_res->solId;
          algo(nouv,taille_l - 1, prof, best_res, cible,p);
          if(best_res->solId != oldSolId)
            {
              if(prof >= best_res->BestArbre.pr)
                {
                  exception=1;
                  break;
                }
            }            
        }

        if(val_b != 0 && (val_a % val_b) == 0)
          {
            rm_arbre(nouv[a]);
            nouv[a] = copy_arbre(pool_get_arbre(p,prof));
            nouv[a]->type=Noeud;
            nouv[a]->u.Noeud.valeur=val_a / val_b;
            nouv[a]->u.Noeud.ag = copy_arbre(l[a]);
            nouv[a]->u.Noeud.ad = copy_arbre(l[b]);
            nouv[a]->u.Noeud.op = Divi;
            {
              unsigned int oldSolId = best_res->solId;
              algo(nouv,taille_l - 1, prof, best_res, cible,p);
              
              if(best_res->solId != oldSolId)
                {
                  if(prof >= best_res->BestArbre.pr)
                    {
                      exception=1;
                      break;
                    }
                }
            }
          }
            
        rm_arbre(nouv[a]);
        nouv[a] = copy_arbre(pool_get_arbre(p,prof));
        nouv[a]->type=Noeud;
        nouv[a]->u.Noeud.valeur=val_b - val_a;
        nouv[a]->u.Noeud.ag = copy_arbre(l[b]);
        nouv[a]->u.Noeud.ad = copy_arbre(l[a]);
        nouv[a]->u.Noeud.op = Moins;
        {
          unsigned int oldSolId = best_res->solId;
          algo(nouv,taille_l - 1, prof, best_res, cible,p);
          
          if(best_res->solId != oldSolId)
            {
              if(prof >= best_res->BestArbre.pr)
                {
                  exception=1;
                  break;
                }
            }
        }


        if(val_a != 0 && (val_b % val_a) == 0)
          {
            rm_arbre(nouv[a]);
            nouv[a] = copy_arbre(pool_get_arbre(p,prof));
            nouv[a]->type=Noeud;
            nouv[a]->u.Noeud.valeur=val_b / val_a;
            nouv[a]->u.Noeud.ag = copy_arbre(l[b]);
            nouv[a]->u.Noeud.ad = copy_arbre(l[a]);
            nouv[a]->u.Noeud.op = Divi;
            {
              unsigned int oldSolId = best_res->solId;
              algo(nouv,taille_l - 1, prof, best_res, cible,p);
              
              if(best_res->solId != oldSolId)
                {
                  if(prof >= best_res->BestArbre.pr)
                    {
                      exception=1;
                      break;
                    }
                }
            }
          }
      }
  }
  for(i=0;i<taille_l - 1;i++)
    rm_arbre(nouv[i]);

  // optimisation du pool: on s'assure que le noeud actuel du pool ne reference aucun autre noeud, et ainsi pourra �tre r�utilis� plus tard
  pool_get_arbre(p,prof);

  return;
}

void le_compte_est_bon (int * liste, unsigned int liste_n, int cible)
{
  arbre ** liste_a;
  char * c;
  unsigned int i;

  solution res;
  res.SolNull=1;
  res.solId=0;

  malloc_arbre_pool p;
  
  pool_create(&p,liste_n);
  
  liste_a=construct_arbre(liste,liste_n);
  
  algo(liste_a,liste_n,0,&res,cible,&p);
  
  if(!res.SolNull)
    {
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

void message_help(char * prog)
{
  printf("%s number1 [number 2 [number 3 ...]] target\n",prog);
}
		 
int main( int argc, char ** argv)
{
  int i;
  int * liste;
  int t,cible;
  for(i=0;i<argc;i++)
    if(!strcmp(argv[i],"-h"))
      {
	message_help(argv[0]);
	return 0;
      }
  
  if(argc<3)
    {
      message_help(argv[0]);
      return 1;
    }
  else
    {
      liste = (int*) malloc (sizeof(int) * (argc - 2));
      for(i=1;i<argc - 1 ;i++)
	{
	  if(sscanf(argv[i],"%d",&t) != 1)
	    {
	      message_help(argv[0]);
	      return 1;
	    }
	  else
	    {
	      liste[i-1]=t;
	    }
	}
      if(sscanf(argv[argc-1],"%d",&cible) != 1)
	{
          message_help(argv[0]);
          return 1;
	}
    }
  le_compte_est_bon(liste,argc - 2,cible);
  free(liste);
  return 0;
}

