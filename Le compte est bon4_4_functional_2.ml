(* version recursive rapide; donne une solution avec la profondeur minimum 
; affiche les résultats intermédiaires
Version basée sur des listes et monades de continuation (implémentation interne)
*)

(*definitions continuation monad *)
type ('a,'z) cont = Cont of (('a ->  'z) -> 'z);;
let runCont m k = (match m with Cont v -> v) k;;
let returnCont a = Cont (fun k -> k a);;

let applyCont m k = Cont (fun future -> runCont m (fun a -> runCont (k a) future));;
let callCC f = Cont (fun k -> runCont (f (fun a -> Cont (fun discarded -> k a))) k) ;;
(*end def continuation monad*)

type operation = Plus|Moins|Mult|Divi;;
type arbre = Nombre of int | Noeud of (int * arbre * arbre* operation);;

(* solution trouvée, et nombre d'opérations dans la solution *)
type solution = BestArbre of (arbre * int) | SolNull;;


(*retourne la priorité de l'arbre *)
let op_priority arbre =
match arbre with
  |Noeud (_,_,_,Plus) -> 1
  |Noeud (_,_,_,Moins) -> 1
  |Noeud (_,_,_,Mult) -> 2
  |Noeud (_,_,_,Divi) -> 2
  |Nombre _ -> 100
;;

let rec profondeur_arbre arbre=
match arbre with
  |Noeud (_,ag,ad,_) -> profondeur_arbre ag + profondeur_arbre ad + 1
  | Nombre _ -> 0;;


let rec string_arbre arbre =
match arbre with
  |Noeud (_,a,b,op) -> 
     (if (op_priority a < op_priority arbre) then
       begin 
	 "("^(string_arbre a)^")"
       end
     else
       string_arbre a
     )^
      
       (
	 match op with 
	     Plus -> "+";	    
	   | Moins -> "-";
	   | Mult -> "*";
	   | Divi -> "/";
       )^
	(
	  if (op_priority b <= op_priority arbre) then
	    begin 
	      "("^(string_arbre b)^")";
	    end
	  else
	    string_arbre b;
	)
  |Nombre a -> string_of_int a;;



let rec construct_arbre_rt base l=
  match l with
      n::l1 -> construct_arbre_rt (base@[Nombre n]) l1
    |[] -> base;;

let construct_arbre  = construct_arbre_rt [] ;;

let valeur_Noeud x =
match x with 
 Noeud y -> (match y with (e,_,_,_) -> e )
|Nombre y -> y ;;

let rec algo l prof best_res iException cible=
    if List.exists (fun x -> valeur_Noeud x = cible) l then
      let sol = List.find (fun x -> valeur_Noeud x = cible) l in
      let profa = profondeur_arbre sol in 
      (
	(print_int cible;print_string (" = "^
		 (string_arbre sol)^
		 " ; "^
		 string_of_int(profa));
	 print_endline "";
	 iException (BestArbre (sol,profa))
        )
      )
    else
      (	
        match best_res with
	  |BestArbre(_,pm) ->
	     if prof + 1 < pm then
	       algo2 l (prof+1) best_res iException cible
             else
	       returnCont best_res
	  |SolNull -> 
	     algo2 l (prof+1) best_res iException cible
      )
and

 algo2_foreach_op iA iB iBase iTail iBest_res iException iCible iProf =
  let val_a = valeur_Noeud iA in
  let val_b = valeur_Noeud iB in
  let new_tail = iBase@iTail in
  let next_algo next_node sol = algo (next_node::new_tail) iProf sol iException iCible in
  applyCont (next_algo (Noeud(val_a+val_b,iA,iB,Plus)) iBest_res) (fun sol1 ->
  applyCont (next_algo (Noeud(val_a-val_b,iA,iB,Moins)) sol1) (fun sol2 ->
  applyCont (next_algo (Noeud(val_a*val_b,iA,iB,Mult)) sol2) (fun sol3 ->
  applyCont (
    if (val_b <> 0  &&  (val_a mod val_b) = 0) then
      next_algo (Noeud(val_a/val_b,iA,iB,Divi)) sol3
    else 
      returnCont sol3
  ) (fun sol4 ->
  applyCont (next_algo (Noeud(val_b-val_a,iB,iA,Moins)) sol4) (fun sol5 ->
  if (val_a <> 0 &&  (val_b mod val_a) = 0) then
    next_algo (Noeud(val_b/val_a,iB,iA,Divi)) sol5
  else
    returnCont sol5)
    ))))

and 

 algo2_foreach_b iA iBase iList iBest_res iException iCible iProf =
  match iList with
      b::l -> applyCont (algo2_foreach_op iA b iBase l iBest_res iException iCible 
			iProf) (fun sol->
	      algo2_foreach_b iA (b::iBase) l sol iException iCible iProf)
    |[] -> returnCont iBest_res

and 
 algo2_foreach_a_b iBase iList iBest_res iException iCible iProf=
  match iList with
      a::l -> applyCont (algo2_foreach_b a iBase l iBest_res iException iCible iProf) (fun sol ->
	algo2_foreach_a_b (a::iBase) l sol iException iCible iProf)
    |[] -> returnCont iBest_res

and 
 algo2 l iProf iBest_res iException iCible =
   callCC (fun exception2 ->
             applyCont (callCC (fun exception1 -> 
                   applyCont (algo2_foreach_a_b [] l iBest_res exception1 iCible iProf) (fun sol2 ->
                                      exception2 (sol2))
                               )) (fun sol1 ->
                ((*print_endline ("Exception! "^(string_of_int (iProf))); *)
                 match sol1 with
	           |BestArbre (_,pm) ->
                      if pm + 1 <= iProf then
                        iException sol1
                       else
                         returnCont sol1
                   |SolNull -> returnCont sol1)))
;;

let le_compte_est_bon liste cible =
 match (
runCont (callCC (fun exception1 -> algo (construct_arbre liste) 0 SolNull exception1 cible))(fun x -> x)
)
 with
     BestArbre(a,_) -> print_int cible;print_string (" = "^(string_arbre a));print_endline "";
   |_ -> print_string ("No Solution");print_endline "";;

let message_help()=
  print_string (Sys.argv.(0)^" number1 [number 2 [number 3 ...]] target");print_endline "";;


let main()=
  let longueur = (Array.length Sys.argv) in
  if(longueur <3) then
    message_help()
  else
    begin
      if (
	let i=[|1|] in
	  while i.(0) <longueur &&  (String.compare Sys.argv.(i.(0)) "-h")<>0  do
	    i.(0) <- i.(0)+1;
	  done;
	  i.(0)<>longueur
      )
      then
	message_help()
      else
	try (
	  le_compte_est_bon (List.map int_of_string (Array.to_list (Array.sub Sys.argv 1 (longueur -2))))
	  (int_of_string (Sys.argv.(longueur-1))) 
	)with  Failure("int_of_string") -> message_help();
    end
;;
main ();;
