module Main where 

{- Le compte est bon ; haskell version
   algo in this version returns a list of solutions from the first found to
   the best one, by using the continuation style (with Cont monads)
   main function evaluate each elements of the list and display them   
 -}
import IO
import Control.Monad.Cont
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
	      return sol
	  else
	      p
      Nothing -> p

{- algo :: (Num a, Ord a) =>
        [Arbre] -> a -> Maybe (Arbre, a) -> Int -> Cont [(Arbre, a)] (Maybe (Arbre, a))
-}

algo l prof best_res cible =
  let length_is_one [_] = True 
      length_is_one _ = False
  in
    case find (\x -> valeur_Noeud x == cible) l of
      Just sol ->
          let profa = profondeur_arbre sol in
	  Cont$ \k -> (sol,profa):(k (Just (sol,profa)))
      Nothing ->
	case best_res of
	  Just (_,pm) ->
	      if prof >= pm - 1 || length_is_one l  then
		  return best_res
	      else
                  algo2 l (prof+1) best_res cible
	      
	  Nothing -> 
	      if length_is_one l  then
	          return best_res
	      else
	          algo2 l (prof+1) best_res cible
 
    
algo2_foreach_op iA iB iBase iTail iPred iBest_res iCible iProf iOp =
    let val_a = valeur_Noeud iA in
    let val_b = valeur_Noeud iB in
    let next_algo my_list = 
            (
             do
               sol <- algo my_list iProf iBest_res iCible
               iPred sol (algo2_foreach_op iA iB iBase iTail iPred sol iCible iProf (iOp+1))
            )
    in
    if (iOp == 0) then
      	next_algo (iBase++[(Noeud (val_a+val_b) iA iB Plus)]++iTail)
    else 
        if (iOp == 1) then 
            next_algo (iBase++[(Noeud (val_a-val_b) iA iB Moins)]++iTail)
      	else
            if (iOp == 2) then
        	next_algo (iBase++[(Noeud (val_a*val_b) iA iB Mult)]++iTail)
            else
                if (iOp == 3) then
      	            if (val_b /= 0  &&  (val_a `mod` val_b) == 0) then
                        next_algo (iBase++[(Noeud (div val_a val_b) iA iB Divi)]++iTail)
	            else
	                algo2_foreach_op iA iB iBase iTail iPred iBest_res iCible iProf (iOp+1)
                else
                    if (iOp == 4) then
                        next_algo(iBase++[(Noeud (val_b-val_a) iB iA Moins)]++iTail)
                    else
                        if (iOp == 5) then
      	                    if (val_a /= 0 &&  (val_b `mod` val_a) == 0) then
	  	                next_algo (iBase++[(Noeud (div val_b val_a) iB iA Divi)]++iTail)
	                    else
	                        return iBest_res
                        else 
                            return iBest_res

algo2_foreach_b iA iBase iList iPred iBest_res iCible iProf =
 case iList of
      b:l -> (do
               sol <- algo2_foreach_op iA b iBase l iPred iBest_res iCible iProf 0 
	       iPred sol (algo2_foreach_b iA (iBase++[b]) l iPred sol iCible iProf)
             )
      [] -> return iBest_res
            

algo2_foreach_a_b iBase iList iPred iBest_res iCible iProf =
  case iList of
    a:l -> (do
             sol <- algo2_foreach_b a iBase l iPred iBest_res iCible iProf
	     iPred sol (algo2_foreach_a_b (iBase++[a]) l iPred sol iCible iProf)
           )
    [] -> return iBest_res

algo2 l iProf iBest_res iCible =
    algo2_foreach_a_b [] l (algo2_pred iProf) iBest_res iCible iProf

{- algo_main :: (Num t, Ord t) => [Arbre] -> Int -> [(Arbre, t)] -}
algo_main l cible = (`runCont` (\_->[])) $ algo l 0 Nothing cible

le_compte_est_bon liste cible =
    showSolutions (algo_main (construct_arbre liste) cible) where
        showSolutions ((sol,pr_sol):l) =
            do
              putStr (show cible)
              putStr (" = "++(show sol)++" ; "++(show pr_sol)++"\n")
              showSolutions l
        showSolutions [] =
            do
              putStr "No more solution\n"

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
      