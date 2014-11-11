open Core.Std
open Pop

module type GEN =
  sig
    val parent : int array array -> float array -> (int array) array
    val flip : int -> int
    val numofbits : int
    val p_mutate : int
    val p_crossover : int
    val mutate : int array -> int array
    val crossover : int array -> int array -> (int array) array
    val reproduce : (int array) array -> (int array) array
  end

module Generation : GEN = 
  struct  
    let parent (int_array : int array array) (floats : float array) : 
	  (int array) array = 
      let total_fitness = Array.fold ~f:(+.) floats ~init:0. in 
      let popsize = Array.length floats in
      let interval = (total_fitness /. (float popsize)) in 
      let rand = Random.float interval in 
      let points_lst = 
	let rec get_points n lst = 
	  if n > 0 
	  then get_points (n-1) ((((float (n-1)) *. interval) +. rand) :: lst) 
	  else lst in
	get_points popsize [] in
      let points = List.to_array points_lst in
      let initial = Array.empty () in 
      let parent_cum : float array = 
	let cumulative = Array.slice floats 0 1 in 
	let rec helper (array1: float array) (n : int) 
		       (cumulative: float array) : float array =
	  if n >= Array.length array1 
	  then cumulative 
	  else helper array1 (n+1) 
		(Array.append cumulative 
		 (List.to_array([array1.(n) +. cumulative.(n-1)]))) in
	helper floats 1 cumulative in 
      let rec comp (pn: int) (fn: int) (parents: int array array) : 
		int array array = 
	if fn >= popsize then parents
	else (if (parent_cum.(pn) >= points.(fn) && pn = 0)
	      then (comp pn (fn + 1) (Array.append parents [|int_array.(pn)|]))
	      else (if parent_cum.(pn) >= points.(fn) && 
			 parent_cum.(pn - 1) <= points.(fn)
		    then (comp pn (fn + 1) 
			       (Array.append parents [|int_array.(pn)|]))
		    else (comp (pn + 1) (fn) (parents)))) in
      comp 0 0 initial
      
     	   
    let flip (x: int) : int =
      if x = 1 then 0 else 1

    let numofbits = Initialpop.coefflength 

    let p_mutate = 100

    let p_crossover = 20

    let mutate (variant : int array) : int array =
      Array.map ~f:(fun x -> if Random.int p_mutate = 0 
			     then flip x else x) variant 

    let crossover (p1: int array) (p2 : int array) : (int array) array =
  (* need to consider corner cases - crossover cannot occur at both ends *)   
      let pointofcrossover = (Random.int (numofbits - 1)) + 1 in
      let p1half1 = Array.slice p1 0 pointofcrossover in
      let p1half2 = Array.slice p1 pointofcrossover 0 in
      let p2half1 = Array.slice p2 0 pointofcrossover in
      let p2half2 = Array.slice p2 pointofcrossover 0 in
      [|(mutate (Array.append p1half1 p2half2));
	(mutate (Array.append p2half1 p1half2))|]
	
    let reproduce (int_array: (int array) array) : (int array) array =
      let popsize = Array.length int_array in
      let numofcoeffs = (Array.length int_array.(0)) / numofbits in				     
      let rec parents (n: int) (lst : int list) : int list = 
	if n >= 0 then parents (n-1) (n::lst) else lst in
      (* tail end recursion  *)
      let parentrand (lst: int list) : int list =
	let rec extract (lst1: int list) (temp : int list ) (n: int) = 
	  match lst1 with 
	  | [] -> failwith "notfound"
	  | x::xs -> if n = 0 then (x, temp @ xs) 
		     else extract (xs) (x::temp) (n-1) in 
	let extract_rand lst2 len =
	  extract lst2 [] (Random.int len) in
	let rec helper lst3 temp len =
	  if len = 0 then temp 
	  else let extracted, rest = extract_rand lst3 len in
	       helper rest (extracted :: temp) (len-1) in
	helper lst [] (List.length lst) in
      let newparents = parentrand (parents (popsize-1) []) in
      let rec crosswithin (n: int) (pt1: int array) (pt2: int array) 
			  (newpt1: int array) (newpt2: int array) : (int array) array = 
	if n >= (numofcoeffs*numofbits) then [|newpt1;newpt2|] 
	else let mat1 = crossover (Array.slice pt1 n (n + numofbits)) 
				  (Array.slice pt2 n (n + numofbits)) in
	     crosswithin (n + numofbits) pt1 pt2 
			 (Array.append newpt1 mat1.(0))
			 (Array.append newpt2 mat1.(1)) in
      let rec newgeneration (a1 : (int array) array) (parents: int list): 
		(int array) array=
	match parents with 
	| p1::p2::tl -> 
	   newgeneration 
	     (Array.append a1 (if Random.int p_crossover = 0 
			       then (let parent1 = int_array.(p1) in
				     let parent2 = int_array.(p2) in
				     crosswithin 0 parent1 parent2 (Array.empty()) (Array.empty()))
			       else [|(mutate int_array.(p1));
				      (mutate int_array.(p2))|])) tl 
	|[] -> a1
	| _ -> failwith "population is not evenly sized" in
      newgeneration (Array.empty ()) (newparents)
		    
  end 


