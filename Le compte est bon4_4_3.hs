module Main where 

{- Le compte est bon ; haskell version
   algo in this version returns a list of solutions from the first found to
   the best one, by using the continuation style (with Cont monads)
   main function evaluate each element of the list and display them
 
   only accept positive numbers in input
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
           let algo2_next = algo2 l (prof+1) best_res cible iException in
           case best_res of
	     Just (_,pm) ->
                 if prof + 1 < pm then
                     algo2_next
                 else {- we don't go in next levels; but we continue in current one -}
                     return best_res
             Nothing -> algo2_next
    
algo2_foreach_op iA iB iBase iTail iBest_res iCible iProf iException=
    let val_a = valeur_Noeud iA in
    let val_b = valeur_Noeud iB in
    let next_algo new_node iBest_res1 = algo (iBase++[new_node]++iTail) iProf iBest_res1 iCible iException
    in
      do
        sol1 <-  next_algo (Noeud (val_a+val_b) iA iB Plus) iBest_res
      	sol2 <-  (
                   if (val_a > val_b) then
                     next_algo (Noeud (val_a-val_b) iA iB Moins) sol1
                   else
                     next_algo (Noeud (val_b-val_a) iB iA Moins) sol1
                 )
      	sol3 <-  (
                  if (val_a /= 1 && val_b /=1) then
                    next_algo (Noeud (val_a*val_b) iA iB Mult) sol2
                  else
                    return sol2
                 )        
        sol4 <- (
      	         if (val_b > 1  &&  (val_a `mod` val_b) == 0) then
                     next_algo (Noeud (div val_a val_b) iA iB Divi) sol3
	         else
                     return sol3
                )
        (
         if (val_a > 1 &&  (val_b `mod` val_a) == 0) then
	     next_algo (Noeud (div val_b val_a) iB iA Divi) sol4
	 else
	     return sol4)


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

algo2 l iProf iBest_res iCible iException =
{- a small explanation:
   exception2 is called when algo2_foreach_a_b did not call is "exception"
   parameter; it will lead to the normal continuation of algo2
   when exception is called, we are leaving current level; we compare the best
   solution found with the level below; if the level below is higher, we escape
   to the exit the the level below (to make short, we rethrow the exception
   if needed)
-}
      callCC (\exception2 ->
              do
                sol1 <- callCC (\exception -> 
                                    do
                                      sol2 <-algo2_foreach_a_b [] l iBest_res iCible iProf exception
                                      exception2 (sol2)
                               )
                ({- we know that exception was called, because exception2 would
                    have made us jump this code -}
                 case sol1 of
	           Just (_,pm) ->
                       if pm + 1 <= iProf then
                           iException sol1
                       else
                           return sol1
                   Nothing -> return sol1))
                

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
           let intArgs = List.map (\x  -> 
                                    let i = read x in 
                                    if i >= 0 then 
                                      i 
                                    else 
                                      error "error: Input numbers cannot be negative"
                                  ) args in
           le_compte_est_bon (List.take (longueur -1) intArgs) (List.last intArgs)
