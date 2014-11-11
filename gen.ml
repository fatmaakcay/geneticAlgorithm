open Core.Std

module type GEN =
  sig
    val reproduce : int32 array -> float array -> (int32 array) array
  end


module Generation : GEN = 
  struct  
    let reproduce int_array floats = 
      let total_fitness = Array.fold_right ~f:(+) floats in 
      let popsize = Array.length floats
      let interval = (total_fitness /. (float_of_int popsize))) in 
      let rand = Random.float 0 interval in 
      let points = let rec get_points n lst = 
		     if n > 0 then get_points (n-1) ((((n-1) *. interval) + rand)::lst) 
		     else lst in
		   get_points popsize []
  end 
