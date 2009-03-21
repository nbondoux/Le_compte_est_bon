(* version parcours horizontal*)
type operation = Plus|Moins|Mult|Divi|Divi2|Moins2;;
type arbre = Nombre of int | Noeud of (int * arbre * arbre* operation);;

let rec construct_arbre l=
        [|(construct_arbre_1 l)|] 
and

construct_arbre_1 l =
match l with
n::l1 -> Array.append [|Nombre n|] (construct_arbre_1 l1)
|[] -> [||];;

let valeur_Noeud x =
match x with 
 Noeud y -> (match y with (e,_,_,_) -> e )

|Nombre y -> y ;;

exception Bad_Divi;;

exception Solution of arbre;;

(*contraintes : 1<= a<b<=longueur de la liste *)

let nouveau_array l a b op cible=
  let longueur=(Array.length l)-1 in
  let nouv = Array.make (longueur) (Nombre 0) in
    for  i=0 to b-1 do
      nouv.(i) <-l.(i)
    done;
    for  i=b to longueur-1 do
      nouv.(i) <-l.(i+1)
    done;


  (let val_a= valeur_Noeud l.(a) in let  val_b=valeur_Noeud l.(b) in match op with
       Plus -> nouv.(a) <- Noeud (val_a+val_b,l.(a),l.(b),Plus)
     |Moins -> nouv.(a) <- Noeud (val_a-val_b,l.(a),l.(b),Moins)
     |Mult  -> nouv.(a) <- Noeud (val_a * val_b,l.(a),l.(b),Mult)
     |Divi -> 
	if val_b!=0 &&  (val_a mod val_b) = 0 then 
	  nouv.(a) <- Noeud (val_a / val_b,l.(a),l.(b),Divi) 
	else 
	  raise Bad_Divi
     |Moins2 -> nouv.(a) <- Noeud (val_b-val_a,l.(b),l.(a),Moins)
     |Divi2 -> 
	if val_a!=0 &&  (val_b mod val_a) = 0 then 
	  nouv.(a) <- Noeud (val_b / val_a,l.(b),l.(a),Divi) 
	else 
	  raise Bad_Divi
  );
  if (valeur_Noeud(nouv.(a))) = cible then
    raise (Solution nouv.(a));
  nouv 
;;

let one_step l cible =
  let nouv = [|[||]|] in
    for i=0 to (Array.length l) -1 do
      if (valeur_Noeud(l.(i))=cible) then
	raise (Solution l.(i))
    done;
    for a = 0 to (Array.length l)-2 do
      for b = a+1 to (Array.length l)- 1 do
	nouv.(0) <- Array.append nouv.(0) [|nouveau_array l a b Plus cible|];
	nouv.(0) <- Array.append nouv.(0) [|nouveau_array l a b Moins cible|];
	nouv.(0) <- Array.append nouv.(0) [|nouveau_array l a b Moins2 cible|];
	nouv.(0) <- Array.append nouv.(0) [|nouveau_array l a b Mult cible|];
        try nouv.(0) <- Array.append nouv.(0) [|nouveau_array l a b Divi cible|] 
	with Bad_Divi -> (nouv.(0) <- nouv.(0));
	    
	try nouv.(0) <- Array.append nouv.(0) [|nouveau_array l a b Divi2 cible|] 
	with Bad_Divi -> (nouv.(0) <- nouv.(0));

      done
    done;
  nouv.(0);;

(*one_step [|Nombre 3; Nombre 4; Nombre 5;Nombre 0|] 20;*)

(* retourne true si tous les arrays de l'array passé en paramètre sont de longueur 1 *)

let array_longueur_1 l =
  let longueur = Array.length l in
  let i = [|0|] in
    while i.(0)<longueur && Array.length l.(i.(0)) == 1 do
      i.(0) <- i.(0) +1;
    done;
    if i.(0)=longueur then
      false
    else
      true
;;




let le_compte_est_bon_1 liste cible =
  let liste1 = [| construct_arbre liste |] in
  let liste2 = [|[||]|] in
    while array_longueur_1 liste1.(0) do
      for i=0 to (Array.length liste1.(0))-1 do
	if Array.length (liste1.(0)).(i) >1 then
	  liste2.(0) <- Array.append liste2.(0) (one_step (liste1.(0)).(i) cible)
	else
	  if (valeur_Noeud (liste1.(0).(i).(0))) = cible then
	    raise (Solution liste1.(0).(i).(0))
      done;
      liste1.(0)<- liste2.(0);
      liste2.(0)<- [||]
    done;

    for i=0 to (Array.length liste1.(0))-1 do
      if (valeur_Noeud liste1.(0).(i).(0)) = cible then
	    raise (Solution liste1.(0).(i).(0))
    done
;;


(*retourne la priorité de l'arbre *)
let op_priority arbre =
match arbre with
  |Noeud (_,_,_,Plus) -> 1
  |Noeud (_,_,_,Moins) -> 1
  |Noeud (_,_,_,Mult) -> 2
  |Noeud (_,_,_,Divi) -> 2
  |Noeud (_,_,_,_) -> 0
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
	     Plus -> "+";	    
	   | Moins -> "-";
	   | Mult -> "*";
	   | Divi -> "/";
	   | _ -> "?"
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

let le_compte_est_bon liste cible =
try le_compte_est_bon_1 liste cible  with 
Solution (arbre) -> print_int cible; print_string (" = "^(string_arbre arbre));print_endline ""
;;

(*le_compte_est_bon [5;4;25;1;6;7] 463 *)

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
	)with  Failure("int_of_string") -> message_help()
    end
;;
main ();;
