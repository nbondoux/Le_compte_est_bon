#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* version recursive rapide (conversion en c); donne une solution avec la profondeur minimum ; affiche les résultats intermédiaires*/


typedef enum 
  {Plus, Moins, Mult, Divi} operation;


struct struct_arbre {
  union
  {
  int Nombre;
    struct
    { 
      int valeur;
      struct struct_arbre * ag;
      struct struct_arbre * ad;
      operation op;
    } Noeud;
  } u;
  enum {Nombre, Noeud} type;
  unsigned int nb_refs;
};

typedef struct struct_arbre arbre;

  
arbre * create_arbre()
{
  arbre *a;
  a = (arbre *) malloc (sizeof (arbre));
  a->nb_refs=1;
  return a;  
}

arbre * copy_arbre(arbre * a)
{
  a->nb_refs++;
  return a;
}

void rm_arbre(arbre * a)
{
  a->nb_refs--;
  if(a->nb_refs==0)
    {
      if(a->type==Noeud)
	{
	  rm_arbre (a->u.Noeud.ad);
	  rm_arbre (a->u.Noeud.ag);
	}
      free(a);
    }
}


/* solution trouvée, et nombre d'opérations dans la solution */

typedef struct {
  int SolNull ; //= 1 si la solution est nulle
  struct {
    arbre *a;
    int pr;
  }BestArbre;
} solution;



/*retourne la priorité de l'arbre */
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

/* retourne le nombre d'opérations dans l'arbre ... */
unsigned int profondeur_arbre (arbre * a)
{
  if(a-> type == Noeud)
    return profondeur_arbre(a->u.Noeud.ag) + profondeur_arbre(a->u.Noeud.ad) + 1;
  else
    return 0;
}

/*alloue (malloc) une chaine de caractères contenant l'expression contenue par l'arbre */
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

/*malloc une table d'arbres, à partir du tableau d'entiers l, de longueur n */
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

int valeur_Noeud (arbre * a)
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


solution * algo (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible);
solution * algo2 (arbre ** l,unsigned int taille_l, unsigned int prof, solution*  best_res, int cible);


solution * algo (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible)
{
  unsigned int i=0;
  unsigned int profa;
  char * c;
  solution * new_sol;
  while (i< taille_l && valeur_Noeud(l[i]) != cible)
    {
      i++;
    }
  if (i!= taille_l)
    {
      /*cette référence ne sera plus utilisée */
      if(!best_res->SolNull)
	rm_arbre(best_res->BestArbre.a);
      
      profa = profondeur_arbre(l[i]);
      c=string_arbre(l[i]);
      printf("%d = %s ; %d\n",cible,c,profa);
      free(c);
      new_sol=(solution *) malloc (sizeof(solution));
      new_sol->BestArbre.a=copy_arbre(l[i]);
      new_sol->BestArbre.pr=profa;
      new_sol->SolNull=0;
      free(best_res);
      return new_sol;
    }
  else
    {
      if(!best_res->SolNull)
	{
	  if(prof >= best_res->BestArbre.pr - 1 || taille_l == 1)
	    return best_res;
	  else
	    return algo2(l,taille_l,prof+1,best_res,cible);
	}
      else
	{
	  if(taille_l == 1)
	    return best_res;
	  else
	    return algo2(l,taille_l,prof+1,best_res,cible);
	}
    }
}

solution * algo2 (arbre ** l,unsigned int taille_l, unsigned int prof, solution * best_res, int cible)
{
  short exception = 0;
  unsigned int a,b,i;
  int val_a,val_b;
  arbre ** nouv;
  solution * n_arbre;
  nouv = (arbre **) malloc (sizeof(arbre *) * (taille_l - 1));

  for(a = 0;a < (taille_l - 1) && ! exception;a++)
      for(b = a+1;b < taille_l && ! exception;b++)
	{
	  for(i=0;i < b ;i++)
	    nouv[i] = copy_arbre(l[i]);
	  for(i=b+1;i < taille_l  ;i++)
	    nouv[i-1] = copy_arbre(l[i]);
	  
	  val_a = valeur_Noeud(l[a]);
	  val_b = valeur_Noeud(l[b]);
	  
	  rm_arbre(nouv[a]);
	  nouv[a] = create_arbre();
	  nouv[a]->type=Noeud;
	  nouv[a]->u.Noeud.valeur=val_a + val_b;
	  nouv[a]->u.Noeud.ag = copy_arbre(l[a]);
	  nouv[a]->u.Noeud.ad = copy_arbre(l[b]);
	  nouv[a]->u.Noeud.op = Plus;
	  n_arbre = algo(nouv,taille_l - 1, prof, best_res, cible);
	  if(n_arbre != best_res)
	    {
	      if(prof >= n_arbre->BestArbre.pr)
		{
		  best_res=n_arbre;
		  exception=1;
		  for(i=0;i<taille_l - 1;i++)
		    rm_arbre(nouv[i]);
		  break;
		}
		else
		  best_res=n_arbre;		
	    }


	  
	  rm_arbre(nouv[a]);
	  nouv[a] = create_arbre();
	  nouv[a]->type=Noeud;
	  nouv[a]->u.Noeud.valeur=val_a - val_b;
	  nouv[a]->u.Noeud.ag = copy_arbre(l[a]);
	  nouv[a]->u.Noeud.ad = copy_arbre(l[b]);
	  nouv[a]->u.Noeud.op = Moins;
	  n_arbre = algo(nouv,taille_l - 1, prof, best_res, cible);
	  if(n_arbre != best_res)
	    {
	      if(prof >= n_arbre->BestArbre.pr)
		{
		  best_res=n_arbre;
		  exception=1;
		  for(i=0;i<taille_l - 1;i++)
		    rm_arbre(nouv[i]);
		  break;
		}
		else
		  best_res=n_arbre;		
	    }


	  rm_arbre(nouv[a]);
	  nouv[a] = create_arbre();
	  nouv[a]->type=Noeud;
	  nouv[a]->u.Noeud.valeur=val_a * val_b;
	  nouv[a]->u.Noeud.ag = copy_arbre(l[a]);
	  nouv[a]->u.Noeud.ad = copy_arbre(l[b]);
	  nouv[a]->u.Noeud.op = Mult;
	  n_arbre = algo(nouv,taille_l - 1, prof, best_res, cible);
	  if(n_arbre != best_res)
	    {
	      if(prof >= n_arbre->BestArbre.pr)
		{
		  best_res=n_arbre;
		  exception=1;
		  for(i=0;i<taille_l - 1;i++)
		    rm_arbre(nouv[i]);
		  break;
		}
		else
		  best_res=n_arbre;		
	    }

	  if(val_b != 0 && (val_a % val_b) == 0)
	    {
	      rm_arbre(nouv[a]);
	      nouv[a] = create_arbre();
	      nouv[a]->type=Noeud;
	      nouv[a]->u.Noeud.valeur=val_a / val_b;
	      nouv[a]->u.Noeud.ag = copy_arbre(l[a]);
	      nouv[a]->u.Noeud.ad = copy_arbre(l[b]);
	      nouv[a]->u.Noeud.op = Divi;
	      n_arbre = algo(nouv,taille_l - 1, prof, best_res, cible);
	      if(n_arbre != best_res)
		{
		  if(prof >= n_arbre->BestArbre.pr)
		    {
		      best_res=n_arbre;
		      exception=1;
		      for(i=0;i<taille_l - 1;i++)
			rm_arbre(nouv[i]);
		      break;
		    }
		  else
		    best_res=n_arbre;		
		}
	    }

	  rm_arbre(nouv[a]);
	  nouv[a] = create_arbre();
	  nouv[a]->type=Noeud;
	  nouv[a]->u.Noeud.valeur=val_b - val_a;
	  nouv[a]->u.Noeud.ag = copy_arbre(l[b]);
	  nouv[a]->u.Noeud.ad = copy_arbre(l[a]);
	  nouv[a]->u.Noeud.op = Moins;
	  n_arbre = algo(nouv,taille_l - 1, prof, best_res, cible);
	  if(n_arbre != best_res)
	    {
	      if(prof >= n_arbre->BestArbre.pr)
		{
		  best_res=n_arbre;
		  exception=1;
		  for(i=0;i<taille_l - 1;i++)
		    rm_arbre(nouv[i]);
		  break;
		}
	      else
		best_res=n_arbre;		
	    }


	  if(val_a != 0 && (val_b % val_a) == 0)
	    {
	      rm_arbre(nouv[a]);
	      nouv[a] = create_arbre();
	      nouv[a]->type=Noeud;
	      nouv[a]->u.Noeud.valeur=val_b / val_a;
	      nouv[a]->u.Noeud.ag = copy_arbre(l[b]);
	      nouv[a]->u.Noeud.ad = copy_arbre(l[a]);
	      nouv[a]->u.Noeud.op = Divi;
	      n_arbre = algo(nouv,taille_l - 1, prof, best_res, cible);
	      if(n_arbre != best_res)
		{
		  if(prof >= n_arbre->BestArbre.pr)
		    {
		      best_res=n_arbre;
		      exception=1;
		      for(i=0;i<taille_l - 1;i++)
			rm_arbre(nouv[i]);
		      break;
		    }
		  else
		    best_res=n_arbre;		
		}
	    }
	  for(i=0;i<taille_l - 1;i++)
	    
	    rm_arbre(nouv[i]);
		  
		
	    
	}
  
  free(nouv);
  return best_res;
}

void le_compte_est_bon (int * liste, int liste_n, int cible)
{
  arbre ** liste_a;
  solution * res = (solution *) malloc (sizeof(solution));
  res->SolNull=1;
  char * c;
  unsigned int i;

  
  liste_a=construct_arbre(liste,liste_n);
  
  res=algo(liste_a,liste_n,0,res,cible);
  
  if(!res->SolNull)
    {
      c=string_arbre(res->BestArbre.a);
      printf("%d = %s\n",cible,c);
      free(c);
    }
  else
    printf("No Solution\n");
  
  if(!res->SolNull)
    rm_arbre(res->BestArbre.a);
  for(i=0;i<liste_n;i++)
    rm_arbre(liste_a[i]);
  free(liste_a);
  free(res);
}

void message_help(char * prog)
{
  printf("%s number1 [number 2 [number 3 ...]] target\n",prog);
}
		 
int main( int argc, char ** argv)
{
  unsigned int i;
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

