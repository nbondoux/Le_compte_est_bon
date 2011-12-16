type 'a lazyList = LazyListEnd | LazyList of 'a * (unit -> ('a lazyList));;

(* simple exemple of generator with CSP
let rec fac x future =
  if x > 1 then
    fac (x-1) (fun a -> LazyList (x*a, fun () -> future (x*a)))
  else
    LazyList (1,fun () -> future 1)
;;
let fac2 x = fac x (fun _ -> LazyListEnd);;
*)


let rec lazyListMap tree f = match tree with
    LazyList (res,next) -> LazyList (f res ,fun () ->  lazyListMap (next ()) f)
  | LazyListEnd -> LazyListEnd
;;



let rec lazyListMerge map1 nmap2 =
    match map1 with
        LazyList (res, next) -> LazyList (res, fun () -> lazyListMerge (next()) nmap2)
      |LazyListEnd -> (nmap2());;

let rec lazyListFoldLeft a lst f=
  match lst with
    LazyList (res,next) -> lazyListFoldLeft (f a res) (next()) f
    |LazyListEnd -> a;;

(* (unit -> 'a lazyList) lazyList -> 'a lazyList = <fun>  *)


let rec lazyListLift lst =
  match lst with
      LazyListEnd -> LazyListEnd
    | LazyList (l,next) -> 
      match l() with 
          LazyList (l2,next2) -> LazyList (l2, fun () -> lazyListLift (LazyList (next2, next)))
        | LazyListEnd -> lazyListLift (next());;

 
(* 
this method is built on this model:
let rec myLift lst = 
  match lst with
      [] -> []
    |l::next -> ( 
      match l with
          l2::next2 -> l2::(myLift (next2::next))   
        |[] -> myLift next
    );;


example for lazyListLift
 let a = LazyList ((fun () -> fac2 4), fun () -> (LazyList ((fun () -> (fac2 5)),fun () -> LazyListEnd)));;
   let LazyList (a,b) = lazyListLift a;;
   let LazyList (a,b) = b();;
 *)


(* return a lazyList holding all possible l1 where
   l1 is made of iL1Size elements of iL
*)

let rec getSubCombinationsFixedLSize iL iL1Size iLSize =
    if iL1Size == 0 then
      LazyList ([],fun () -> LazyListEnd)
    else
      match iL with 
          h::t -> (
            let listsWithH = lazyListMap (getSubCombinationsFixedLSize t (iL1Size - 1) (iLSize -1 )) (fun l -> h::l) in
            if iL1Size < iLSize then
              lazyListMerge listsWithH (fun () -> (getSubCombinationsFixedLSize t (iL1Size) (iLSize - 1)))
            else
              listsWithH
          )
        | [] -> LazyListEnd
;;
  

(*  return a lazyList holding all possible l1 where l1 is made n elements of iL,
    iMinSize <= n < iMaxSize
    l1 are returned by growing order of n
*)

let rec getSubCombinations iL iLSize iMinSize iMaxSize =
  let rec getSubCombinations_rec n =
      if n >= iMaxSize then
        LazyListEnd
      else
        lazyListMerge (getSubCombinationsFixedLSize iL n iLSize) (fun () -> getSubCombinations_rec (n+1))
  in
  getSubCombinations_rec iMinSize;;
        



 (* return a lazyList holding all possible couple (l1,l2) where
    l1 is made of iL1Size elements of iL and l2 contains the complementary elmts
    of iL
 *)

let rec getSubCombinationCouplesFixedL1Size iL iL1Size iLSize =
    if iL1Size == 0 then
      LazyList (([],iL),fun () -> LazyListEnd)
    else
      match iL with 
          h::t -> (
            let hInFirsts = lazyListMap (getSubCombinationCouplesFixedL1Size t (iL1Size - 1) (iLSize -1 )) (fun (l1,l2) -> (h::l1,l2)) in
            if iL1Size < iLSize then
              let hInSeconds = fun () -> lazyListMap (getSubCombinationCouplesFixedL1Size t (iL1Size) (iLSize - 1)) (fun (l1,l2) -> (l1, h::l2)) in
              lazyListMerge hInFirsts hInSeconds 
            else
              hInFirsts
          )
        | [] -> LazyListEnd;;


(* return all non_ordered subcombinations of iL where iL1 has size
   iL1Size; it differs from getSubCombinationCouplesFixedL1Size, as
   if iL1Size*2 == iLSize, we must not return (['a'],['b'])  and (['b'],['a'])
*)
let rec getSubCombinationCouplesFixedL1Size_bis iL iL1Size iLSize =
    if iL1Size * 2 != iLSize then
      getSubCombinationCouplesFixedL1Size iL iL1Size iLSize 
    else
      match iL with 
          h::t -> lazyListMap (getSubCombinationCouplesFixedL1Size t (iL1Size-1) (iLSize-1))
                     (fun (l1,l2) -> (h::l1,l2))
        |[] -> LazyList(([],[]),fun() -> LazyListEnd);;

(* returns all non-ordered couple of sub combinations of l!!! *)
let getAllSubCombinationCouples iL iLSize = 
  let rec f i =
    if i *2 <= iLSize then
      lazyListMerge (getSubCombinationCouplesFixedL1Size_bis iL i iLSize)
        (fun () ->f (i+1))
    else
      LazyListEnd in
  f 0;;



type operation = Add|Minus|Mult|Divi;;
type tree = Number of int | Node of (int * tree * tree* operation);;

type solution = BestTree of tree | SolNull;;

let op_priority_op op =
  match op with
      Add|Minus -> 1
    |Mult|Divi -> 2
;;

let op_priority_node node =
  match node with
      Node (_,_,_,op) -> op_priority_op op
    | Number _ -> 100;;

let value_node node = 
  match node with
      Node (v,_,_,_) -> v
    | Number v -> v;;

let is_operation_associative op =
  match op with
      Add|Mult -> true
    |Minus|Divi -> false;;

let rec tree_to_s t =
    match t with
        Node (_,nl,nr,op) -> (
          let strLeft = tree_to_s nl in
          let strRight = tree_to_s nr in
          let strLeft = (if op_priority_node nl < op_priority_op op then
              "("^strLeft^")"
            else
              strLeft) in
          let strRight = (if ((not (is_operation_associative op) ) &&
                                 op_priority_node nr <= op_priority_op op) ||
              op_priority_node nr < op_priority_op op then
              "("^strRight^")"
            else
              strRight) in
          match op with
              Add -> strLeft^"+"^strRight
            |Minus -> strLeft^"-"^strRight
            |Mult -> strLeft^"*"^strRight
            |Divi -> strLeft^"/"^strRight
        )
      |Number i -> string_of_int i;;

(*
  (* style with lift and map; easier to write, but much slower (because map and lift resolution takes function calls*)
let rec algo_l_size iL iLSize =
  match iL with
      [a] -> LazyList (a,fun () -> LazyListEnd)
    | [] -> LazyListEnd
    | l -> lazyListLift (lazyListMap (getAllSubCombinationCouples l (List.length l)) (fun (l1,l2) ->
      
      let l1_size = List.length l1 in
      let l2_size = List.length l2 in
      
      fun () -> lazyListLift (
        lazyListMap (algo_l_size l1 l1_size) (fun elmt1 ->
          let val1 = value_node elmt1 in
          fun () -> lazyListMap (algo_l_size l2 l2_size) (fun elmt2 ->
            let val2 = value_node elmt2 in
            Node (val1 + val2, elmt1, elmt2, Add)
              
          )
        )
      )
    )
    );;
*)

(* future parameter represents the computation to be appended at the end of the
   computed LazyList;
*)

let rec algo_l_size iL iLSize future =
  match iL with
      [a] -> LazyList (a,fun () -> future())
    | [] -> future()
    | _ -> (
      let rec f_listCouples iListCoupleList =
      match iListCoupleList with
        |LazyList ((l1,l2),nextCouple) -> (
          let l1_size = List.length l1 in
          let l2_size = List.length l2 in
          let rec f_subcombs1 iListCombs1 =
              match iListCombs1 with
                |LazyList (elmt1,nextComb1) -> (
                  let val1 = value_node elmt1 in
                  let rec f_subcombs2 iListCombs2 =

                      match iListCombs2 with
                        |LazyList (elmt2,nextComb2) -> (
                          let val2 = value_node elmt2 in
                          
                          let node_is_op n op=
                            match n with
                                Node (_,_,_,op2) -> op==op2
                              | _ -> false
                          in
                          let nextop4 () =
                            let nextToUpper =
                              (fun () -> f_subcombs2 (nextComb2()))
                            in
                            if val2 > val1 && val1 > 1 && (val2 mod val1) == 0 then
                              LazyList (
                                Node (val2 / val1, elmt2, elmt1, Divi)
                                , nextToUpper)
                            else if  val1 >= val2 && val2 > 1 && (val1 mod val2) == 0 then
                              LazyList (
                                Node (val1 / val2, elmt1, elmt2, Divi)
                                , nextToUpper)
                            else
                              nextToUpper()
                          in
                          let nextop3 () =
                            if val1 > 1 && val2 >1 &&
                              not (node_is_op elmt1 Divi) &&
                              not (node_is_op elmt2 Divi) then
                              LazyList (
                                Node (val1 * val2, elmt1, elmt2, Mult)
                                , fun () -> nextop4())
                            else
                              nextop4()

                          in
                          let nextop2 () =
                            if (val2 > val1) && val1 > 0 then
                              LazyList (Node (val2 - val1, elmt2, elmt1, Minus)
                                          , fun () -> nextop3())
                            else if (val1 >= val2) && val2 > 0 then
                              LazyList (Node (val1 - val2, elmt2, elmt1, Minus)
                                          , fun () -> nextop3())
                            else
                              nextop3()
                          in
                          let nextop1 () =
                            if val1 > 0 && val2 >0 &&
                              not (node_is_op elmt1 Minus) &&
                              not (node_is_op elmt2 Minus) then
                              LazyList (
                                Node (val1 + val2, elmt1, elmt2, Add)
                                , fun () -> nextop2())
                            else
                              nextop2()
                          in
                          nextop1()                         
                        )
                        |LazyListEnd -> f_subcombs1 (nextComb1 ())
                  in
                  f_subcombs2 (algo_l_size l2 l2_size (fun () -> LazyListEnd))
                )
                |LazyListEnd -> f_listCouples (nextCouple())
          in
          f_subcombs1 (algo_l_size l1 l1_size (fun () -> LazyListEnd))
        )
        |LazyListEnd -> future() 
      in f_listCouples (getAllSubCombinationCouples iL (List.length iL))
    )
;;

let algo_all_sizes iL iLSize =
    (*this "algo" calls are used for the 
      "the values made of N numbers generated by each of its sublists
      of size N" part of the algorithm described on top of the file
    
      
      gets all combinations of size n verifying iMinLength <= n < iLSize
    *)
  let subLists = getSubCombinations iL iLSize 1 (iLSize+1) in
  let rec f_sublists iSL =
    match iSL with
        LazyList (sl,nextsls) ->
          algo_l_size sl (List.length sl) (fun () -> f_sublists (nextsls()))
      | LazyListEnd -> LazyListEnd
  in
  f_sublists subLists;;




(*

puts "Best so far: #{@node.value} = #{@node}"
        puts "#{@target} = #{@bestSolution.node}"
      elsif @bestSolution.node
        puts "No Solution; nearest solution is: #{@bestSolution.node.value} = #{@bestSolution.node}"
      else
        puts "No Solution"

*)

let le_compte_est_bon iL iTarget =
  let computations = algo_all_sizes (List.map (fun x -> Number x) iL)
    (List.length iL) in
  let rec rec_algo iComps iBest =
    match iComps with
        LazyList (n,next) -> (
          let val_n = (value_node n) in
          let isNewBest = (
            match iBest with 
              |(Some (iB,delta)) ->
                (iTarget - val_n < delta && val_n - iTarget < delta)
              |None -> true
          )
          in
          let newBest =
            (if isNewBest then                  
                begin
                  print_endline ("Best so far: "^(string_of_int val_n)^" = "^(tree_to_s n));
                  let new_delta = (
                    if val_n >= iTarget then
                      val_n - iTarget
                    else
                      iTarget - val_n
                  ) in
                  Some (n,new_delta)
                end
             else
                iBest
            ) in
          if (val_n == iTarget) then
            Some n
          else
            rec_algo (next()) newBest
        )
      |LazyListEnd -> None
  in
  match (rec_algo computations None) with
      None -> print_endline "No Solution"
    | Some n ->  print_endline ((string_of_int (value_node n))^" = "^(tree_to_s n))
;;

let message_help()=
  print_endline (Sys.argv.(0)^" number1 [number 2 [number 3 ...]] target");;


let main()=
  let length = (Array.length Sys.argv) in
  if(length <3) then
    message_help()
  else
    begin
      if (
	let i=[|1|] in
	  while i.(0) <length &&  (String.compare Sys.argv.(i.(0)) "-h")<>0  do
	    i.(0) <- i.(0)+1;
	  done;
	  i.(0)<>length
      )
      then
	message_help()
      else
	try (
	  le_compte_est_bon (List.map int_of_string (Array.to_list (Array.sub Sys.argv 1 (length -2))))
	  (int_of_string (Sys.argv.(length-1))) 
	)with  Failure("int_of_string") -> message_help();
    end
;;
main ();;
