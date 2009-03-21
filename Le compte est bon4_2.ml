(* version recursive ; donne la meilleure des solutions 
ecrit les intermédiaires*)
(*exemple:
./Le\ compte\ est\ bon4 4 24 82 6  8 287 34 13 178 41  1867
1867 = 8+287+178+34*41
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

(* retourne le "poids" des opérateurs de l'arbre *)
let rec poids_arbre arbre =
match arbre with
  |Noeud (_,ag,ad,Plus) -> 1 + poids_arbre ag + poids_arbre ad
  |Noeud (_,ag,ad,Moins) -> 2 + poids_arbre ag + poids_arbre ad
  |Noeud (_,ag,ad,Mult) -> 3 + poids_arbre ag + poids_arbre ad
  |Noeud (_,ag,ad,Divi) -> 4 + poids_arbre ag + poids_arbre ad
  |Nombre _ -> 0;;

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
	     Plus -> "+"	    
	   | Moins -> "-"
	   | Mult -> "*"
	   | Divi -> "/"
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



let rec construct_arbre l=
  match l with
      n::l1 -> Array.append [|Nombre n|] (construct_arbre l1)
    |[] -> [||];;

let valeur_Noeud x =
match x with 
 Noeud y -> (match y with (e,_,_,_) -> e )

|Nombre y -> y ;;


exception Solution of  (arbre * int) ;;

let rec algo l prof best_res  cible=
  let i =[|0|] in
    while i.(0) <  (Array.length l) && (valeur_Noeud(l.(i.(0))) != cible)
    do
      i.(0) <- i.(0) + 1
    done;
    if i.(0)!= (Array.length l) then
      let profa = profondeur_arbre l.(i.(0)) in 
	(
	  match best_res with
	      BestArbre (a,old_prof) ->  
		if  (profa < old_prof) || (poids_arbre l.(i.(0))) < (poids_arbre a) then
		  (
		    print_int cible;print_string (" = "^(string_arbre l.(i.(0)))^" ; "^string_of_int(profa));print_endline "";
		    BestArbre (l.(i.(0)),profa )
		  )
		else best_res
		       
	    |SolNull -> (
		print_int cible;print_string (" = "^(string_arbre l.(i.(0)))^" ; "^string_of_int(profa));print_endline "";
		BestArbre (l.(i.(0)),profa)
	      )
	)
    else
      (	
	match best_res with
	    BestArbre(_,pm) ->
	      ( if prof >= pm ||Array.length l = 1 then
		  best_res
		else
		  (  
		    algo2 l (prof+1) best_res  cible )
	      )
	  |SolNull -> 
	     if Array.length l = 1 then
	       best_res
	     else
	       algo2 l (prof+1) best_res  cible
      )
and
   
    algo2 l prof best_res  cible =
  try(
  let nouv = Array.make ((Array.length l) - 1) (Nombre 0) in
  let m_arbre = [|best_res|] in
    for a = 0 to (Array.length l)-2 do
      for b = a+1 to (Array.length l)- 1 do
	(
	  let longueur=(Array.length l)-1 in
	    for  i=0 to b-1 do
	      nouv.(i) <-l.(i)
	    done;
	    for  i=b to longueur-1 do
	      nouv.(i) <-l.(i+1)
	    done;
	    
	    (let val_a= valeur_Noeud l.(a) in let  val_b=valeur_Noeud l.(b) in 

	       nouv.(a) <- Noeud (val_a+val_b,l.(a),l.(b),Plus);
	       let n_arbre = algo nouv prof m_arbre.(0) cible in
		 (if m_arbre.(0) == n_arbre then
		    ()
		  else
		    match n_arbre with
			BestArbre (n_a,n_pr) -> 
			  if prof > n_pr then
			    raise (Solution (n_a,n_pr))
			  else
			    m_arbre.(0)<-n_arbre
		      | _ -> ()
		 );
		 
		 nouv.(a) <- Noeud (val_a-val_b,l.(a),l.(b),Moins);
		 let n_arbre = algo nouv prof m_arbre.(0) cible in
		   (if m_arbre.(0) == n_arbre then
		      ()
		    else
		      match n_arbre with
			  BestArbre (n_a,n_pr) -> 
			    if prof > n_pr then
			      raise (Solution (n_a,n_pr))
			    else
			      m_arbre.(0)<-n_arbre
				
			| _ -> ()
		   );
		   
		   nouv.(a) <- Noeud (val_a * val_b,l.(a),l.(b),Mult);
		   let n_arbre = algo nouv prof m_arbre.(0) cible in
		     (if m_arbre.(0) == n_arbre then
			()
		      else
			match n_arbre with
			    BestArbre (n_a,n_pr) -> 
			      if prof > n_pr then
				raise (Solution (n_a,n_pr))
			      else
				m_arbre.(0)<-n_arbre
			  | _ -> ()
		     );
		     
		     (
		       if val_b!=0 &&  (val_a mod val_b) = 0 then 
			 (nouv.(a) <- Noeud (val_a / val_b,l.(a),l.(b),Divi);
			  let n_arbre = algo nouv prof m_arbre.(0) cible in
			    (if m_arbre.(0) == n_arbre then
			       ()
			     else
			       match n_arbre with
				   BestArbre (n_a,n_pr) -> 
				     if prof > n_pr then
				       raise (Solution (n_a,n_pr))
				     else
				       m_arbre.(0)<-n_arbre
				 | _ -> ()
			    )
			 ) 
		       else ();
		       );
		     
		     nouv.(a) <- Noeud (val_b-val_a,l.(b),l.(a),Moins);
		     let n_arbre = algo nouv prof m_arbre.(0) cible in
		       (if m_arbre.(0) == n_arbre then
			  ()
			else
			  match n_arbre with
			      BestArbre (n_a,n_pr) -> 
				if prof > n_pr then
				  raise (Solution (n_a,n_pr))
				else
				  m_arbre.(0)<-n_arbre
			    | _ -> ()
		       );
		       
		       (
			 if val_a!=0 &&  (val_b mod val_a) = 0 then 
			     ( nouv.(a) <- Noeud (val_b / val_a,l.(b),l.(a),Divi); 
			       let n_arbre = algo nouv prof m_arbre.(0) cible in
				 (if m_arbre.(0) == n_arbre then
				    ()
				  else
				    match n_arbre with
					BestArbre (n_a,n_pr) -> 
					  if prof > n_pr then
					    raise (Solution (n_a,n_pr))
					  else
					    m_arbre.(0)<-n_arbre
				      | _ -> ()
				 )
			     )
			 else () 
		       )	       
	    )
	)
      done
    done;
    m_arbre.(0)
) with Solution s -> BestArbre s;;


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
	  while i.(0) <longueur &&  (String.compare Sys.argv.(i.(0)) "-h")!=0  do
	    i.(0) <- i.(0)+1;
	  done;
	  i.(0)!=longueur
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
