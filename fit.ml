open Core.Std

module type FIT =
  sig
    val coeffs : (('a array) array) -> (float list) list
    val addfitval : (('a array) array) -> float list -> ('a array) array
  end

let test_matrix = [|[|0;1|];[|1;1|]|]
module Fitness : FIT =
  struct
    let coeffs (mat : ((int32 array) array)) : (float list) list  =
      let strtoco (str : array) : float list =
	[34.5;21.2;46.3]
      in
      let rec listgen (row : array) : (float list) list =
       match row with
       | a -> (strtoco row) :: listgen mat.(n - 1)
       | _ -> []
      in
      listgen mat.(Array.length - 1)
  end

