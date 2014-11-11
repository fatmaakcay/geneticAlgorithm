open Core.Std
open Pop

module type FIT =
  sig
    val coeffs : ((int array) array) -> (float list) list
    val fit : ((float list) list) -> (float array) array -> 
	      ((float -> float) list) -> float array
    val averagefit : float list -> float 
    val bestfit : float list -> (float * int)
    val stringcoeffs : ((float list) list) -> int -> string
  end

module Fitness : FIT =
  struct
    let coeffs (mat : (int array) array) : (float list) list  =
      let popsize = Array.length mat in
      let sizeofcoeffs = Initialpop.coefflength in
      let numofcoeffs = (Array.length mat.(0)) / sizeofcoeffs in
      (* If a2.(m) = 0 then base10 moves to the next bit
         without altering coefficient stored in flt  *)
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
	else (let intportion = 
		base10 (Array.slice a1 0 (sizeofcoeffs/2)) 0 0. in
	      (* need to figure out how to calculate the size of base10 
               * from base2*)
	      let decportion = 
		((base10 (Array.slice a1 (sizeofcoeffs/2) sizeofcoeffs) 0 0.)
		 /. Initialpop.decimalplaces) in
	      variantcoeff (Array.slice a1 sizeofcoeffs 0) (x + 1) 
			   ((intportion +. decportion) :: lst1)) in
      (* createcoeffs finds the coefficients of the population *)
      let rec createcoeffs (n : int) 
			   (result : (float list) list) : (float list) list = 
	if n >= popsize then result
	else createcoeffs (n+1) 
			  (List.rev(variantcoeff mat.(n) 0 []) :: result) in
      List.rev(createcoeffs 0 [])
   
    let averagefit (lst: float list) : float =
      let finitelst = List.filter ~f:(fun x -> x <> -1.) lst in
      if lst = [] then failwith "empty list"
      else (List.fold_left finitelst ~init:0. 
			   ~f:(+.)) /. float(List.length finitelst) 

    (* 
     * variant:  datamat (the data matrix) is in the 
     * form {f(x_1,...,x_n),x_1,...,x_n} 
     *)						 
    let fit (lst: (float list) list) (datamat: (float array) array) 
	    (f : (float -> float) list) : 
	  float array =
      let datasize = Array.length datamat in 
      let arrayf = List.to_array f in
      let rec predictedvalue (coefflst : float list) (column : int) 
			     (row : int) (value : float) : float = 
	match coefflst with 
	| [] -> value
	| coeffhd :: coefftl -> 
	   predictedvalue coefftl (column + 1) row 
			  (coeffhd *. (arrayf.(column-1) datamat.(row).(column)) 
			   +. value) in
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
      let fitlst = List.rev (fitcalc lst []) in
      let mean = averagefit fitlst in 
      let relfit = List.map ~f:(fun x -> if x <> 0. then mean /. x 
					  else -1. ) fitlst in
      (* infinity values return -1. which is identified by Fitness.bestfit
       and handled with a special case in main.ml. Infinity values of -1. 
       are filtered out before calculation of average in Fitness.averagefit*)
      List.to_array(relfit)   
		       
    let bestfit (lst: float list) : (float * int) =
      if lst = [] then failwith "empty list"
      else List.fold_left lst ~init:(-2., -1) 
			  ~f:(fun (x,counter) y ->
			      if x = -1. then (x, counter) else
			      if x < y then (y, counter + 1) 
			      else (x, counter))
    
    let stringcoeffs (coeffs: (float list) list) (loc: int): string =
      let rec helper (coeffs: (float list) list) (n: int) : float list =
	match coeffs with 
	| [] -> failwith "empty list"
	| hd :: tl -> if n = 0 then hd else helper tl (n - 1) in
      List.fold_right ~init:"" ~f:(fun x y -> 
				    Float.to_string x ^ ", " ^ y) 
		      (helper coeffs loc)
		     							      
  end



