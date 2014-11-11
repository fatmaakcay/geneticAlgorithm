open Core.Std

module type DATA =
  sig
    val matrix : ((float array) array)
  end

module type FUNCTION =
  sig

    (* 
     * Consider f(x) = a * g(x) + b * x + c) 
     * func would be a * g(x) + b * x + c)
     * typefunc would be [(fun x -> g(x));(fun y -> y);(fun _ -> 1.0)] 
     *
     *)

    val func : float -> float
    val typefunc : (float -> float) list

  end

module Linearf : FUNCTION =
  struct
    let func (x : float) : float = 1.2 *. x			     
    let typefunc : (float -> float) list = [(fun x -> x)]					
  end

module Quadraticf : FUNCTION =
  struct
    let func (x : float) : float = 2. *. (x ** 2.)
    let typefunc : (float -> float) list = [(fun x -> x ** 2.)]
  end
    
module LinandQuadf : FUNCTION =
  struct
    let func (x : float) : float = 2. *. (x ** 2.) +. 3. *. x  
    let typefunc : (float -> float) list = [(fun x -> x ** 2.);(fun y -> y)]
  end

module Sinef : FUNCTION =
  struct 
    let func (x : float) : float = sin x
    let typefunc : (float -> float) list = [(fun x -> sin x)]
  end

module Logf : FUNCTION = 
  struct 
    let func (x : float) : float = (5. *. log x)
    let typefunc : (float -> float) list = [(fun x -> log x)]
  end 

module LogandCubicf : FUNCTION =
  struct 
    let func (x : float) : float = (5. *. log x) -. (4.23 *. (x ** 3.))
    let typefunc : (float -> float) list = [(fun x -> log x);(
					      fun y -> -1. *. (y ** 3.))]
  end

module Complexf : FUNCTION =
  struct
    let func (x : float) : float = (1.2 *. (x ** 3.)) +. (6. *. (x ** 2.)) 
				   +. (3.8 *. x) 
    let typefunc : (float -> float) list = [(fun x -> (x ** 3.));
					    (fun y -> y ** 2.);(fun z -> z)]
  end 

module DataFunctor (F : FUNCTION) : DATA =
  struct	       
    let rec xval (n: float) (lst : float list) : float list = 
      if n >= -25. then xval (n -. 1.) (n::lst) else lst 
		  
    let rec gen_array (parents: float list) (a1: (float array) array) 
	    (f : float -> float) : (float array) array =
      match parents with
      | [] -> a1
      | hd :: tl -> 
	 let numofvariables = List.length F.typefunc in
	 let rec lst (n: int) (result: float list) : float list =
	   if n < 1 then result else lst (n - 1) (result@[hd]) in
	 gen_array tl (Array.append [|List.to_array(lst numofvariables [f hd])|] 
				    a1) f
			      
    let matrix = gen_array (xval 25. []) (Array.empty()) F.func
  end 

module StochasticDataFunctor (F : FUNCTION) : DATA  =
  struct	       
    let rec xval (n: float) (lst : float list) : float list = 
      if n >= -25. then xval (n -. 1.) (n::lst) else lst 
 
    let rec gen_array (parents: float list) (a1: (float array) array) 
	    (f : float -> float) : (float array) array =
      match parents with
      | [] -> a1
      | hd :: tl -> 
	 let number = if Random.int 2 = 0 then 1. else -1. in 
	 let numofvariables = List.length F.typefunc in
	 let rec lst (n: int) (result: float list) : float list =
	   if n < 1 then result else lst (n - 1) (result@[hd]) in
	 gen_array tl (Array.append 
			 [|List.to_array(lst numofvariables 
			   [f hd +. (number *. (Random.float 0.2))])|] a1) f
			      
    let matrix = gen_array (xval 25. []) (Array.empty()) F.func
  end 

module Linearm = DataFunctor(Linearf)
module Quadraticm = DataFunctor(Quadraticf)
module LinandQuadm = DataFunctor(LinandQuadf)
module Sinem = DataFunctor(Sinef)
module Logm = DataFunctor(Logf)
module LogandCubicm = DataFunctor(LogandCubicf)
module Complexm = DataFunctor(Complexf)
module RandomTrigm = StochasticDataFunctor(Sinef)
