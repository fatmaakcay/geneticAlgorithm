open Core.Std 
open Data

module type POP =
sig
    val popsize : int
    val coefflength : int
    val decimalplaces : float
    val randpop : int -> (int array) array
end

module Initialpop : POP =
struct
  let popsize = 50

  let coefflength = 20

  let decimalplaces : float =
    let rec helper (n: int) (result: float) : float = 
      if n < 0 then result
      else helper (n - 1) (2. ** (float n) +. result) in
    10. ** (round(log10(helper(coefflength / 2 - 1) 0.)) +. 1.)

  let randpop (numofcoeffs : int) : (int array) array =  
    Random.self_init ();
    Array.map ~f:(fun x -> Array.map ~f:(fun _ -> (Random.int 2)) x)
	      (Array.make_matrix ~dimx:popsize ~dimy:(coefflength * numofcoeffs) 0)
end 
