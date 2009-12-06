(* version recursive rapide; donne une solution avec la profondeur minimum 
; affiche les résultats intermédiaires
Version purement fonctionnelle
*)

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

(* retourne le "poids" des opérateurs de l'arbre; pas utilisé dans cette version de l'algo *)
let rec poids_arbre arbre =
match arbre with
  |Noeud (_,ag,ad,Plus) -> 1 + poids_arbre ag + poids_arbre ad
  |Noeud (_,ag,ad,Moins) -> 2 + poids_arbre ag + poids_arbre ad
  |Noeud (_,ag,ad,Mult) -> 3 + poids_arbre ag + poids_arbre ad
  |Noeud (_,ag,ad,Divi) -> 4 + poids_arbre ag + poids_arbre ad
  |Nombre _ -> 0;;


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



let algo2_pred prof sol p =
  match sol  with
      BestArbre (_,n_pr)->
	if prof < n_pr then
	  p
	else
	  sol
    |_ -> p ;;



let rec algo l prof best_res  cible=
  let length_is_one l = match l with 
      [_] -> true
    | _ -> false in

    if List.exists (fun x -> valeur_Noeud x = cible) l then
      let sol = List.find (fun x -> valeur_Noeud x = cible) l in
      let profa = profondeur_arbre sol in 
      (
	(print_int cible;print_string (" = "^
		 (string_arbre sol)^
		 " ; "^
		 string_of_int(profa));
	 print_endline "";	
	 BestArbre (sol,profa))
      )
  else
      (	
	match best_res with
	    BestArbre(_,pm) ->
	      ( if prof >= pm - 1 ||length_is_one l  then
		  best_res
		else
		  algo2 l (prof+1) best_res  cible
	      )
	  |SolNull -> 
	     if length_is_one l  then
	       best_res
	     else
	       algo2 l (prof+1) best_res  cible
      )
and

 algo2_foreach_op iA iB iBase iTail iPred iBest_res iCible iProf =
  let val_a = valeur_Noeud iA in
  let val_b = valeur_Noeud iB in
  let new_tail = iBase@iTail in
  let next_algo next_node sol = algo (next_node::new_tail) iProf sol iCible in
  let sol1 = next_algo (Noeud(val_a+val_b,iA,iB,Plus)) iBest_res in iPred sol1 (
  let sol2 = next_algo (Noeud(val_a-val_b,iA,iB,Moins)) sol1 in iPred sol2 (
  let sol3 = next_algo (Noeud(val_a*val_b,iA,iB,Mult)) sol2 in iPred sol3 (
  let sol4 = (
    if (val_b <> 0  &&  (val_a mod val_b) = 0) then
      next_algo (Noeud(val_a/val_b,iA,iB,Divi)) sol3
    else 
      sol3
  ) in iPred sol4 (
  let sol5 = next_algo (Noeud(val_b-val_a,iB,iA,Moins)) sol4 in iPred sol5 (
      if (val_a <> 0 &&  (val_b mod val_a) = 0) then
	next_algo (Noeud(val_b/val_a,iB,iA,Divi)) sol5
      else
        sol5
    )))))
and 

 algo2_foreach_b iA iBase iList iPred iBest_res iCible iProf =
  match iList with
      b::l -> let sol = (algo2_foreach_op iA b iBase l iPred iBest_res iCible 
			iProf)
	in iPred sol (algo2_foreach_b iA (b::iBase) l iPred sol iCible iProf)
    |[] -> iBest_res

and 
 algo2_foreach_a_b iBase iList iPred iBest_res iCible iProf=
  match iList with
      a::l -> let sol = (algo2_foreach_b a iBase l iPred iBest_res iCible iProf) in
	iPred sol (algo2_foreach_a_b (a::iBase) l iPred sol iCible iProf)
    |[] -> iBest_res

and 
 algo2  l iProf iBest_res iCible =
  algo2_foreach_a_b [] l (algo2_pred iProf) iBest_res iCible iProf;;


let le_compte_est_bon liste cible =
 match (algo (construct_arbre liste) 0 SolNull  cible)
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
