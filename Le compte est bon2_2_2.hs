module Main where 

{- Le compte est bon ; haskell version; return the first solution found
 -}

import System.IO as IO
import System.Environment
import Data.Char as Char
import Data.List as List

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
                  construct_arbre_rt base (n:l1) = construct_arbre_rt (base++[Nombre n]) l1
                  construct_arbre_rt base [] = base;;

valeur_Noeud (Noeud e _ _ _) = e
valeur_Noeud (Nombre y) = y 


{- utility function: return Just x where x is the first element of the list 
   verifying the predicate, or Nothing if no element is found -}
listFind p (x:l) = if (p x) then Just x else listFind p l 
listFind p [] = Nothing


algo l cible =
  let length_is_one [_] = True 
      length_is_one _ = False
  in
    case listFind (\x -> valeur_Noeud x == cible) l of
      Just sol -> Just sol
      Nothing -> algo2 l cible
     
algo2_foreach_op iA iB iBase iTail iCible iOp =
    let val_a = valeur_Noeud iA in
    let val_b = valeur_Noeud iB in
    let algo_next my_list = case algo my_list iCible of
                              Just sol -> Just sol
                              Nothing -> algo2_foreach_op iA iB iBase iTail iCible (iOp+1)
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
	                algo2_foreach_op iA iB iBase iTail iCible (iOp+1)
                else
                    if (iOp == 4) then
      	                algo_next (iBase++[(Noeud (val_b-val_a) iB iA Moins)]++iTail)
                    else
                        if (iOp == 5) then
      	                    if (val_a /= 0 &&  (val_b `mod` val_a) == 0) then
	  	                algo_next (iBase++[(Noeud (div val_b val_a) iB iA Divi)]++iTail)
	                    else
	                        Nothing
                        else 
                            Nothing

algo2_foreach_b iA iBase iList iCible =
 case iList of
      b:l -> case algo2_foreach_op iA b iBase l iCible 0 of
               Just sol -> Just sol
               Nothing -> algo2_foreach_b iA (iBase++[b]) l iCible
      [] -> Nothing

algo2_foreach_a_b iBase iList iCible =
  case iList of
      a:l -> case algo2_foreach_b a iBase l iCible of
               Just sol -> Just sol
               Nothing -> algo2_foreach_a_b (iBase++[a]) l iCible
      [] -> Nothing

algo2 l iCible =
  algo2_foreach_a_b [] l iCible

le_compte_est_bon liste cible =
 case (algo (construct_arbre liste) cible) of
   Just a -> 
       do
         putStr (show cible)
         putStr (" = "++(show a)++"\n")
   Nothing-> 
       do
         putStr "No Solution\n"

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
      
