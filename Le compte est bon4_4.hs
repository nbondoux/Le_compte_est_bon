module Main where 

{- Le compte est bon ; haskell version -}
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

profondeur_arbre (Noeud _ ag ad _ ) = profondeur_arbre ag + profondeur_arbre ad + 1
profondeur_arbre  (Nombre _ ) =  0

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



algo2_pred prof sol p = 
    case sol of 
      (Just (_,n_pr))->
          if  prof >= n_pr then
	      sol
	  else
	      p
      Nothing -> p 

algo l prof best_res  cible=
  let length_is_one [_] = True 
      length_is_one _ = False
  in
    case find (\x -> valeur_Noeud x == cible) l of
      Just sol ->
          let profa = profondeur_arbre sol in 
	  Just (sol,profa)
      Nothing ->
      	
	case best_res of
	  Just (_,pm) ->
	      if prof >= pm - 1 || length_is_one l  then
		  best_res
	      else
		  algo2 l (prof+1) best_res  cible
	      
	  Nothing -> 
	      if length_is_one l  then
	          best_res
	      else
	          algo2 l (prof+1) best_res  cible
 
    
algo2_foreach_op iA iB iBase iTail iPred iBest_res iCible iProf iOp =
    let val_a = valeur_Noeud iA in
    let val_b = valeur_Noeud iB in
    let next_algo next_noeud iBest_res1 = algo (iBase++[next_noeud]++iTail) iProf iBest_res1 iCible in
    let sol1 = next_algo (Noeud (val_a+val_b) iA iB Plus) iBest_res in iPred sol1 (
    let sol2 = next_algo (Noeud (val_a-val_b) iA iB Moins) sol1 in iPred sol2 (
    let sol3 = next_algo (Noeud (val_a*val_b) iA iB Mult) sol2 in iPred sol3 (
    let sol4 =                                                                              
            if (val_b /= 0  &&  (val_a `mod` val_b) == 0) then
                next_algo (Noeud (div val_a val_b) iA iB Divi) sol3
	    else
	        sol3
            in iPred sol4 (
    let sol5 = next_algo (Noeud (val_b-val_a) iB iA Moins) sol4 in iPred sol5 (
      	    if (val_a /= 0 &&  (val_b `mod` val_a) == 0) then
	  	next_algo (Noeud (div val_b val_a) iB iA Divi) sol5
	    else
	        sol5
    )))))

algo2_foreach_b iA iBase iList iPred iBest_res iCible iProf =
 case iList of
      b:l -> let sol = algo2_foreach_op iA b iBase l iPred iBest_res iCible 
			iProf 0 in
	     iPred sol (algo2_foreach_b iA (iBase++[b]) l iPred sol iCible iProf)
      [] -> iBest_res

algo2_foreach_a_b iBase iList iPred iBest_res iCible iProf=
  case iList of
      a:l -> let sol = algo2_foreach_b a iBase l iPred iBest_res iCible iProf in
	     iPred sol (algo2_foreach_a_b (iBase++[a]) l iPred sol iCible iProf)
      [] -> iBest_res

algo2  l iProf iBest_res iCible =
  algo2_foreach_a_b [] l (algo2_pred iProf) iBest_res iCible iProf

le_compte_est_bon liste cible =
 case (algo (construct_arbre liste) 0 Nothing  cible) of
   Just (a,_) -> 
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
      
