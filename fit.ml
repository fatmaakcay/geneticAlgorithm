open Core.Std

(* Testing Fit.ml *)
let coeffs (mat : (int array) array) : (float list) list  =
  let popsize = Array.length mat in
  let sizeofcoeffs = 10 in
  let numofcoeffs = (Array.length mat.(0)) / sizeofcoeffs in
  let rec base10 (a2: int array) (m : int) (flt : float) : float = 
    if m < (sizeofcoeffs/2) 
    then (if a2.(m) = 0 
	  then base10 a2 (m + 1) flt
	  else base10 a2 (m + 1) ((((float 2) ** 
				      float(sizeofcoeffs/2 - 1 - m)) *. 
				     float(a2.(m))) +. flt))
    else flt in
  (* variantcoeff finds the coefficients of a single variant *)
  let rec variantcoeff (a1 : int array) (x : int) 
		       (lst1 : float list) : float list =
    if x >= numofcoeffs then lst1 
    else (let intportion = base10 (Array.slice a1 0 (sizeofcoeffs/2)) 0 0. in
	  (*need to figure out how to calculate the size of base10 from base2*)
	  let decportion = ((base10 (Array.slice a1 
			    (sizeofcoeffs/2) sizeofcoeffs) 0 0.) /. 100.) in
	  variantcoeff (Array.slice a1 sizeofcoeffs 0) (x + 1) 
		       ((intportion +. decportion) :: lst1)) in
  (* createcoeffs finds the coefficients of the population *)
  let rec createcoeffs (n : int) 
		       (result : (float list) list) : (float list) list = 
    if n >= popsize then result
    else createcoeffs (n+1) (List.rev(variantcoeff mat.(n) 0 []) :: result) in
  List.rev(createcoeffs 0 [])
	  
(* tests*)
let t1 = [|[|1;0;0;1;0;1;0;0;1;0;1;0;1;1;0;1;0;1;1;0|]|]
let t2 = [|[|1;0;0;1;0;1;0;0;1;0|];[|1;0;1;1;0;1;0;1;1;0|]|]
let t3 = [|[|1;0;0;1;0;1;0;0;1;0;1;0;1;1;0;1;0;1;1;0|];
	   [|1;1;0;0;0;1;1;1;1;1;1;1;1;1;1;1;1;0;0;0|]|]
let _ = assert (coeffs t1 = [[18.18;22.22]])
let _ = assert (coeffs t2 = [[18.18];[22.22]])
let _ = assert (coeffs t3 = [[18.18;22.22];[24.31;31.24]])

let averagefit (lst: float list) : float =
      let finitelst = List.filter ~f:(fun x -> x <> -1.) lst in
      if lst = [] then failwith "empty list"
      else (List.fold_left finitelst ~init:0. 
			   ~f:(+.)) /. float(List.length finitelst) 
(* tests *)
let _ = assert (averagefit [1.] = 1.)
let _ = assert (averagefit [1.;2.;3.;4.;5.] = 3.)
let _ = assert (averagefit [-1.;1.;2.;3.;4.;5.] = 3.) 

let fit (lst: (float list) list) (datamat: (float array) array) : float array =
  let datasize = Array.length datamat in 
  let rec predictedvalue (coefflst : float list) (column : int) (row : int) 
			 (value : float) : float = 
    match coefflst with 
    | [] -> value
    | coeffhd :: coefftl -> 
       predictedvalue coefftl (column + 1) row 
		      (coeffhd *. datamat.(row).(column) +. value) in
  let rec ssecalc (coefflst: float list) (obs: int) (sse: float) : float =
    if obs >= datasize then sse 
    else ssecalc coefflst (obs + 1) 
		 (sse +. (datamat.(obs).(0) -. 
			  predictedvalue coefflst 1 obs 0.) ** 2.) in
  let rec fitcalc (lst: (float list) list) (fitresult: float list)
	  : float list = 
    match lst with
    | [] -> fitresult
    | hd :: tl -> fitcalc tl ((ssecalc hd 0 0.) :: fitresult) in
  let fitlst = fitcalc lst [] in
  let mean = averagefit fitlst in
  let relfit = List.map ~f:(fun x -> x /. mean) fitlst in
  List.to_array(relfit)   
     
let rec parents (n: float) (lst : float list) : float list = 
  if n >= 12. then parents (n -. 1.) (n::lst) else lst 

(* tests *)
let _ = assert (parents 25. [] = [12.;13.;14.;15.;16.;17.;18.;19.;20.;21.;22.;23.;24.;25.])
let _ = assert (parents 12. [] = [12.])

let bestfit (lst: float list) : (float * int) =
  if lst = [] then failwith "empty list"
  else List.fold_left lst ~init:(-2., -1) 
		      ~f:(fun (x,counter) y ->
			  if x = -1. then (x, counter) else
			    if x < y then (y, counter + 1) 
			    else (x, counter))
		      
(* tests *)
let _ = assert (bestfit [1.] = (1.,0))
let _ = assert (bestfit [1.;2.;3.;4.;5.] = (5.,4))
let _ = assert (bestfit [-1.;1.;2.;3.;4.;5.] = (-1.,0)) 
let _ = assert (bestfit [1.;-1.;2.;3.;4.;5.] = (-1.,1))
let _ = assert (bestfit [1.;2.;3.;4.;5.;-1.] = (-1.,5))

let rec gen_array (parents: float list) (a1: (float array) array) 
	: (float array) array =
  match parents with
  | [] -> a1
  | hd :: tl -> gen_array tl (Array.append [|(List.to_array [hd;hd])|] a1)

let matrix = gen_array (parents 25. []) (Array.empty())

(* tests *)
let _ = assert (Array.length matrix = 14)
let _ = assert (Array.length matrix.(0) = 2)

(*
  Code used to calculate within 1% accuracy

let arraycoeffs (coeffs: (float list) list) (loc: int): float array =
  let rec helper (coeffs: (float list) list) (n: int) : float list =
    match coeffs with 
    | [] -> failwith "empty list"
    | hd :: tl -> if n = 0 then hd else helper tl (n - 1) in
  List.fold_right ~init:(Array.empty()) ~f:(fun x y -> 
					    Array.append [|x|] y) 
		  (helper coeffs loc)
 *)		    

(* Testing Gen.ml *)

let flip (x: int) : int =
  if x = 1 then 0 else 1

(* tests *)
let _ = assert (flip 1 = 0)
let _ = assert (flip 0 = 1)

let reproduce (int_array: (int array) array) : (int array) array =
  let popsize = Array.length int_array in
  let numofbits = 10 in
  let p_mutate = 100 in
  let p_crossover = 20 in
  let mutate (variant : int array) : int array =
    Array.map ~f:(fun x -> if Random.int p_mutate = 0 then flip x else x) 
	      variant in
  let crossover (p1: int array) (p2 : int array) : (int array) array =
    (* need to consider corner cases - crossover cannot occur at both ends *)   
    let pointofcrossover = (Random.int (numofbits - 1)) + 1 in
    let p1half1 = Array.slice p1 0 pointofcrossover in
    let p1half2 = Array.slice p1 pointofcrossover 0 in
    let p2half1 = Array.slice p2 0 pointofcrossover in
    let p2half2 = Array.slice p2 pointofcrossover 0 in
    [|(mutate (Array.append p1half1 p2half2));
      (mutate (Array.append p2half1 p1half2))|] in
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
  let rec newgeneration (a1 : (int array) array) (parents: int list)
	  : (int array) array=
    match parents with 
    | p1::p2::tl -> 
       newgeneration (Array.append a1 
				   (if Random.int p_crossover = 0 
				    then crossover int_array.(p1) int_array.(p2) 
				    else [|(mutate int_array.(p1))
					  ;(mutate int_array.(p2))|])) tl 
    |[] -> a1
    | _ -> failwith "population is not evenly sized" in
      newgeneration (Array.empty ()) (newparents)

(* debugging parents *)
let points_lst = 
  let rec get_points n lst = 
    if n > 0 then get_points (n-1) ((((float (n-1)) *. 5.) +. 3.) :: lst) 
    else lst in
  get_points 20 []
let floats = List.to_array( [1.;2.;3.;4.])

let points = List.to_array points_lst
let parent_cum : float array = 
  let cumulative = Array.slice points 0 1 in 
  let rec helper (a2: float array) (n : int) (cumulative: float array) : float array =
	  if n >= Array.length a2 
	  then cumulative 
	  else helper a2 (n+1) (Array.append cumulative 
			       (List.to_array([a2.(n) +. cumulative.(n-1)]))) in
  helper points 1 cumulative  

(* Tests for Pop.ml *)
let popsize = 50
let coefflength = 10
let numofcoeffs = 1
let randpop = fun () -> 
  Array.map ~f:(fun x -> Array.map ~f:(fun _ -> (Random.int 2)) x)
	      (Array.make_matrix popsize (coefflength * numofcoeffs) 0) 
let test = reproduce (randpop ())
let x = Array.fold test ~init:true ~f:(fun accum x -> 
				       accum && (Array.length x = 10))
let _ = assert (x = true)     

let rec lst (n: int) (result: float list) : float list =
  if n < 1 then result else lst (n -1) result@[1.]

(* tests *)
let _ = assert (lst 2 [] = [1.;1.])

(* Tests for Main.ml *)

(* Old printout *)
(*
	  then ((Printf.fprintf oc "%d, " (numofgenerations - times));
		(Printf.fprintf oc "%f, " averagefit);
		(Printf.fprintf oc "Perfect Fit Attained, ");
		(Printf.fprintf oc "%s, " string);
		(Printf.fprintf oc "%f,\n" time);
		(Out_channel.close oc))
	  else ((Printf.fprintf oc "%d, " (numofgenerations - times));
		(Printf.fprintf oc "%f, " averagefit);
		(Printf.fprintf oc "%f, " bestfit);
		(Printf.fprintf oc "%s, " string);
		(Printf.fprintf oc "%f,\n" time);
		(Out_channel.close oc);
 *)
