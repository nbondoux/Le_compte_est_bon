module Main where 

{- Le compte est bon ; haskell version
   algo in this version returns a list of solutions from the first found to
   the best one, by using the continuation style (with Cont monads)
   main function evaluate each element of the list and display them
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

{- algo :: (Num a, Ord a) =>
        [Arbre]
        -> a
        -> Maybe (Arbre, a)
        -> Int
        -> (Maybe (Arbre, a) -> Cont [(Arbre, a)] (Maybe (Arbre, a)))
        -> Cont [(Arbre, a)] (Maybe (Arbre, a))
-}

algo l prof best_res cible iException=
     case find (\x -> valeur_Noeud x == cible) l of
       Just sol ->
           let profa = profondeur_arbre sol in
           Cont$ \k -> (sol,profa):(runCont (iException (Just (sol,profa))) k)
       Nothing ->
           let algo2_next = algo2 l (prof+1) best_res cible in
           case best_res of
	     Just (_,pm) ->
                 if prof + 1 < pm then
                     algo2_next
                 else
                     if prof+1 == pm then
                         {- we don't go in next levels; but we continue in current one -}
                         return best_res
                     else
                         {- prof+1 > pm; we quit current level-}
                         iException best_res
             Nothing -> algo2_next
    
algo2_foreach_op iA iB iBase iTail iBest_res iCible iProf iException=
    let val_a = valeur_Noeud iA in
    let val_b = valeur_Noeud iB in
    let next_algo new_node iBest_res1 = algo (iBase++[new_node]++iTail) iProf iBest_res1 iCible iException
    in
      do
        sol1 <-  next_algo (Noeud (val_a+val_b) iA iB Plus) iBest_res
      	sol2 <-  next_algo (Noeud (val_a-val_b) iA iB Moins) sol1
      	sol3 <-  next_algo (Noeud (val_a*val_b) iA iB Mult) sol2
        sol4 <- (
      	         if (val_b /= 0  &&  (val_a `mod` val_b) == 0) then
                     next_algo (Noeud (div val_a val_b) iA iB Divi) sol3
	         else
                     return sol3
                )
        sol5 <- next_algo (Noeud (val_b-val_a) iB iA Moins) sol4
        (
         if (val_a /= 0 &&  (val_b `mod` val_a) == 0) then
	     next_algo (Noeud (div val_b val_a) iB iA Divi) sol5
	 else
	     return sol5)


algo2_foreach_b iA iBase iList iBest_res iCible iProf iException=
 case iList of
      b:l -> (do
               sol <- algo2_foreach_op iA b iBase l iBest_res iCible iProf iException
	       algo2_foreach_b iA (iBase++[b]) l sol iCible iProf iException
             )
      [] -> return iBest_res
            

algo2_foreach_a_b iBase iList iBest_res iCible iProf iException=
  case iList of
    a:l -> (do
             sol <- algo2_foreach_b a iBase l iBest_res iCible iProf iException
	     algo2_foreach_a_b (iBase++[a]) l sol iCible iProf iException
           )
    [] -> return iBest_res

algo2 l iProf iBest_res iCible =
    callCC (\exception -> algo2_foreach_a_b [] l iBest_res iCible iProf exception)

{- algo_launcher :: (Num t, Ord t) => [Arbre] -> Int -> [(Arbre, t)] -}
algo_launcher l cible = (`runCont` (\_->[])) $ callCC (\exception -> algo l 0 Nothing cible exception)

le_compte_est_bon liste cible =
    showSolutions (algo_launcher (construct_arbre liste) cible) where
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
