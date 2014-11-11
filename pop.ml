open Core.Std 

module type POP =
sig
    val popsize : int
    val bitlength : int
    val randpop : unit -> (int32 array) array
end

module Initialpop : POP =
struct
  let popsize = 50
  let bitlength = 10
  let randpop = fun () -> 
    Array.map ~f:(fun x -> 
		  (Array.map ~f:(fun x -> 
				 (Random.int32 (Int32.succ(Int32.one))))) x)
	      (Array.make_matrix popsize bitlength Int32.zero) 
end 
    
