(* version rapide ; donne une seule solution (la premiere trouvee)*)
type operation = Plus|Moins|Mult|Divi;;
type arbre = Nombre of int | Noeud of (int * arbre * arbre* operation);;

(*retourne la priorité de l'arbre *)
let op_priority arbre =
match arbre with
  |Noeud (_,_,_,Plus) -> 1
  |Noeud (_,_,_,Moins) -> 1
  |Noeud (_,_,_,Mult) -> 2
  |Noeud (_,_,_,Divi) -> 2
  |Nombre _ -> 100
;;


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

exception Solution of  arbre  ;;

let rec algo l  cible=
  let i =[|0|] in
    while i.(0) <  (Array.length l) && (valeur_Noeud(l.(i.(0))) != cible)
    do
      i.(0) <- i.(0) + 1
    done;
    if i.(0)!= (Array.length l) then
      raise (Solution (l.(i.(0))))

    else
      (	
	if Array.length l = 1 then
	  ()
	else
	  algo2 l   cible
      )
and
    algo2 l  cible =
  for a = 0 to (Array.length l)-2 do
    for b = a+1 to (Array.length l)- 1 do
      (
	let longueur=(Array.length l)-1 in
	let nouv = Array.make (longueur) (Nombre 0) in
	  for  i=0 to b-1 do
	    nouv.(i) <-l.(i)
	  done;
	  for  i=b to longueur-1 do
	    nouv.(i) <-l.(i+1)
	    done;
	  
	  (let val_a= valeur_Noeud l.(a) in let  val_b=valeur_Noeud l.(b) in 
	     nouv.(a) <- Noeud (val_a+val_b,l.(a),l.(b),Plus);
	     algo nouv cible;
	     nouv.(a) <- Noeud (val_a-val_b,l.(a),l.(b),Moins);
	     algo nouv cible;
	     nouv.(a) <- Noeud (val_a * val_b,l.(a),l.(b),Mult);
	     algo nouv cible;
	     (
	       if val_b!=0 &&  (val_a mod val_b) = 0 then 
		 (nouv.(a) <- Noeud (val_a / val_b,l.(a),l.(b),Divi);
		  algo nouv cible;
		 ) 
		 else ();
	     );
	     nouv.(a) <- Noeud (val_b-val_a,l.(b),l.(a),Moins);
	     algo nouv cible;
	     (
	       if val_a!=0 &&  (val_b mod val_a) = 0 then 
		 ( nouv.(a) <- Noeud (val_b / val_a,l.(b),l.(a),Divi); 
		   algo nouv cible;
		 )
	       else (); 
	     );
	    )
      )
    done
  done;;


let le_compte_est_bon liste cible =
 try (algo (construct_arbre liste)  cible; print_string ("No Solution");print_endline "")
 with
     Solution a -> print_int cible;print_string (" = "^(string_arbre a));print_endline "";;

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
