module Main where 

{- Le compte est bon ; haskell version
   algo in this version returns a list of solutions from the first found to
   the best one, by using the continuation style (without monads)
   main function evaluate each elements of the list and display them   
 -}
import IO
import System
import Char
import List

data Operation = Plus|Moins|Mult|Divi

instance Eq Operation where 
    Plus == Plus = True
    Moins == Moins = True
    Mult == Mult = True
    Divi == Divi = True
    _ == _ = False

instance Show Operation where 
    show Plus = "+"
    show Moins = "-"
    show Mult = "*"
    show Divi = "/"

data Arbre =  Nombre Int | Noeud Int Arbre Arbre Operation  deriving (Eq)


{- solution trouvée, et nombre d'opérations dans la solution -}
type Solution =  Maybe (Arbre,Int)


{-retourne la priorité de l'arbre -}

op_priority (Noeud _ _ _ Plus) = 1
op_priority (Noeud _ _ _ Moins) = 1
op_priority (Noeud _ _ _ Mult) = 2
op_priority (Noeud _ _ _ Divi) = 2
op_priority (Nombre _ ) = 100

profondeur_arbre (Noeud _ ag ad _ ) = profondeur_arbre ag + profondeur_arbre ad + 1
profondeur_arbre  (Nombre _ ) =  0

{- retourne le "poids" des opérateurs de l'arbre; pas utilisé dans cette version de l'algo -}
poids_arbre (Noeud _ ag ad Plus) = 1 + poids_arbre ag + poids_arbre ad
poids_arbre (Noeud _ ag ad Moins) = 2 + poids_arbre ag + poids_arbre ad
poids_arbre (Noeud _ ag ad Mult) = 3 + poids_arbre ag + poids_arbre ad
poids_arbre (Noeud _ ag ad Divi) = 4 + poids_arbre ag + poids_arbre ad
poids_arbre (Nombre _ ) = 0

instance Show Arbre where 
    show arbre =
        case arbre of 
          (Noeud _ a b op) ->
              (
               if (op_priority a < op_priority arbre) then
	           "("++(show a)++")"
               else
                   show a
              )
              ++
              (show op)
              ++
              (	      
               if (op_priority b <= op_priority arbre) then
	           "("++(show b)++")"
	       else
	           show b
	      )    
          (Nombre a)  ->  show a

construct_arbre = construct_arbre_rt [] where
                  construct_arbre_rt base (n:l1) = construct_arbre_rt (base++[Nombre n]) l1
                  construct_arbre_rt base [] = base;;

valeur_Noeud (Noeud e _ _ _) = e
valeur_Noeud (Nombre y) = y 

{- in all these algo* functions, "future" continuation always take as parameter
   the best solution known -}

algo2_pred prof sol p = 
    case sol of 
      (Just (_,n_pr))->
          if prof >= n_pr then
	      \future -> future sol
	  else
	      \future -> p (\new_sol -> future new_sol)
      Nothing -> \future -> p (\new_sol -> future new_sol)

algo l prof best_res cible =
    case find (\x -> valeur_Noeud x == cible) l of
      Just sol ->
          let profa = profondeur_arbre sol in 
	  \future -> (sol,profa):future (Just (sol,profa))
      Nothing ->
	case best_res of
	  Just (_,pm) ->
	      if prof + 1  >= pm  then
		  \future -> future best_res
	      else
		  \future -> algo2 l (prof+1) best_res cible (\sol -> future sol)
	      
	  Nothing -> \future -> algo2 l (prof+1) best_res cible (\sol -> future sol)
 
    
algo2_foreach_op iA iB iBase iTail iPred iBest_res iCible iProf =
    let val_a = valeur_Noeud iA in
    let val_b = valeur_Noeud iB in
    let next_algo next_node iBest_res1 = algo (iBase++[next_node]++iTail) iProf iBest_res1 iCible in
    \future ->
    next_algo (Noeud (val_a+val_b) iA iB Plus) iBest_res (\sol1 -> iPred sol1 (
    next_algo (Noeud (val_a-val_b) iA iB Moins) sol1) (\sol2 -> iPred sol2 (
    next_algo (Noeud (val_a*val_b) iA iB Mult) sol2) (\sol3 -> iPred sol3 (
    (
     if (val_b /= 0  &&  (val_a `mod` val_b) == 0) then
         next_algo (Noeud (div val_a val_b) iA iB Divi) iBest_res
     else
	 \my_future -> my_future sol3
    )) (\sol4 -> iPred sol4 (
    next_algo (Noeud (val_b-val_a) iB iA Moins) sol4) (\sol5 -> iPred sol5 (
     (
      if (val_a /= 0 &&  (val_b `mod` val_a) == 0) then
	  next_algo (Noeud (div val_b val_a) iB iA Divi) sol5
      else
	  \my_future -> my_future sol5
     )) future
    )))))
                                                     

algo2_foreach_b iA iBase iList iPred iBest_res iCible iProf  =
 case iList of
      b:l -> \future -> algo2_foreach_op iA b iBase l iPred iBest_res iCible 
	     iProf (\sol ->
	     iPred sol (algo2_foreach_b iA (iBase++[b]) l iPred sol iCible iProf) future)
      [] -> \future -> future iBest_res

algo2_foreach_a_b iBase iList iPred iBest_res iCible iProf =
  case iList of
      a:l -> \future -> algo2_foreach_b a iBase l iPred iBest_res iCible iProf (\sol ->
	     iPred sol (algo2_foreach_a_b (iBase++[a]) l iPred sol iCible iProf)
             future)
      [] -> \future -> future iBest_res

algo2 l iProf iBest_res iCible  =
    \future -> algo2_foreach_a_b [] l (algo2_pred iProf) iBest_res iCible iProf (\sol -> future (sol))

{- algo_main :: (Num t, Ord t) => [Arbre] -> Int -> [(Arbre, t)] -}
algo_main l cible = algo l 0 Nothing cible (\_->[])

le_compte_est_bon liste cible =
    showSolutions (algo_main (construct_arbre liste) cible) where
        showSolutions ((sol,pr_sol):l) =
            do
              putStr (show cible)
              putStr (" = "++(show sol)++" ; "++(show pr_sol)++"\n")
              showSolutions l
        showSolutions [] =
            do
              putStr "No better solution\n"

message_help iProgName =
    do
      putStr (iProgName++" number1 [number 2 [number 3 ...]] target\n")


main=
    do
      args <- getArgs
      progName <- getProgName
      let longueur = (List.length args)
      if (longueur <2 || (List.elem "-h" args) || (List.elem "--help" args)) then 
          message_help progName
       else 
           let intArgs = List.map (\x  -> read x) args in
           le_compte_est_bon (List.take (longueur -1) intArgs) (List.last intArgs) 
      