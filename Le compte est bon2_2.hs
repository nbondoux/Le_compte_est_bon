module Main where 

{- Le compte est bon ; haskell version; return the first solution found;
   use continuation style for exception handling (without monads for
   continuations)
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
                  construct_arbre_rt base (n:l1) = construct_arbre_rt ((Nombre n):base) l1
                  construct_arbre_rt base [] = reverse base

valeur_Noeud (Noeud e _ _ _) = e
valeur_Noeud (Nombre y) = y 


{- utility function: return Just x where x is the first element of the list 
   verifying the predicate, or Nothing if no element is found -}
listFind p (x:l) = if (p x) then Just x else listFind p l 
listFind p [] = Nothing

{- in all these functions, the "future" continuation never takes any arguments;
   so, as there is lazy evaluation, its type is what would be its return type -}

algo l cible future=
  let length_is_one [_] = True 
      length_is_one _ = False
  in
    case listFind (\x -> valeur_Noeud x == cible) l of
      Just sol -> sol:future
      Nothing -> algo2 l cible future
     
algo2_foreach_op iA iB iBase iTail iCible iOp future =
    let val_a = valeur_Noeud iA in
    let val_b = valeur_Noeud iB in
    let algo_next my_list = algo my_list iCible (
           algo2_foreach_op iA iB iBase iTail iCible (iOp+1) future)
    in
    if (iOp == 0) then
      	algo_next (iBase++[(Noeud (val_a+val_b) iA iB Plus)]++iTail)
    else 
        if (iOp == 1) then 
            algo_next (iBase++[(Noeud (val_a-val_b) iA iB Moins)]++iTail)
      	else
            if (iOp == 2) then
        	algo_next (iBase++[(Noeud (val_a*val_b) iA iB Mult)]++iTail)
            else
                if (iOp == 3) then
      	            if (val_b /= 0  &&  (val_a `mod` val_b) == 0) then
	                algo_next (iBase++[(Noeud (div val_a val_b) iA iB Divi)]++iTail)
	            else
	                algo2_foreach_op iA iB iBase iTail iCible (iOp+1) future
                else
                    if (iOp == 4) then
      	                algo_next (iBase++[(Noeud (val_b-val_a) iB iA Moins)]++iTail)
                    else
                        if (iOp == 5) then
      	                    if (val_a /= 0 &&  (val_b `mod` val_a) == 0) then
	  	                algo_next (iBase++[(Noeud (div val_b val_a) iB iA Divi)]++iTail)
	                    else
	                        future
                        else 
                            future

algo2_foreach_b iA iBase iList iCible future =
 case iList of
      b:l -> algo2_foreach_op iA b iBase l iCible 0 (
	     algo2_foreach_b iA (iBase++[b]) l iCible future
                                                       )
      [] -> future

algo2_foreach_a_b iBase iList iCible future=
  case iList of
      a:l -> algo2_foreach_b a iBase l iCible (
	     algo2_foreach_a_b (iBase++[a]) l iCible future
                                              )
      [] -> future

algo2 l iCible future=
  algo2_foreach_a_b [] l iCible future

le_compte_est_bon liste cible =
    showSolutions (algo (construct_arbre liste) cible []) where
        showSolutions (a:l) =
            do
              putStr (show cible)
              putStr (" = "++(show a)++"\n")
                     {-"uncomment this if you want to display all the solutions showSolutions l -}
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
