(* This is the main ML page *)
open Core.Std
open Pop
open Data
open Gen
open Fit

(* 
 *
 * HERE IS A LIST OF PROVIDED FUNCTIONS AND DATA SETS
 * CHOOSE ONE ROW AND INSERT INTO VARIABLES
 * function_name, module_name
 * 
 * Linearf.typefunc, Linearm.matrix
 * Quadraticf.typefunc, Quadraticm.matrix
 * LinandQuadf.typefunc, LinandQuadm.matrix
 * Sinef.typefunc, Sinem.matrix
 * Logf.typefunc, Logm.matrix
 * LogandCubicf.typefunc, LogandCubicm.matrix
 * Complexf.typefunc, Complexm.matrix
 * Sinef.typefunc,  RandomTrigm.matrix
 *
 *)

(* VARIABLES *)
let outputfile = "generation.txt"
let function_name = LinandQuadf.typefunc
let module_name = LinandQuadm.matrix
let numofgenerations = 500

(* this function calls upon the other modules and brings everything together *)
let multiplegeneration (n: int) : unit = 
  let numofcoeffs = List.length function_name in
  let initialpop = Initialpop.randpop numofcoeffs in
  let initialtime = Unix.gettimeofday() in
  (* define data matrix *)
  let rec helper (times : int) (population : (int array) array) : unit = 
    if times < 0 then () 
    else (let coeff_convert = Fitness.coeffs population in
	  let fitnessval = Fitness.fit coeff_convert module_name function_name in
	  let averagefit = Fitness.averagefit (Array.to_list(fitnessval)) in
	  let (bestfit, loc) = Fitness.bestfit (Array.to_list(fitnessval)) in
	  let string = Fitness.stringcoeffs (coeff_convert) (loc) in
	  let time = Unix.gettimeofday() -. initialtime in
	  let oc = open_out_gen [Open_creat;Open_text;Open_append] 
				0o640 outputfile in
	  if bestfit = -1.
	  then ((Printf.fprintf oc "Average: %f\n" averagefit);
		(Printf.fprintf oc "Best: Perfect Fit Attained\n");
		(Printf.fprintf oc "Coefficients: %s\n" string);
		(Printf.fprintf oc "Time: %f\n\n" time);
		(Out_channel.close oc))
	  else ((Printf.fprintf oc "Average: %f\n" averagefit);
		(Printf.fprintf oc "Best: %f\n" bestfit);
		(Printf.fprintf oc "Coefficients: %s\n" string); 
		(Printf.fprintf oc "Time: %f\n\n" time);
		(Out_channel.close oc));
	  let parents = Generation.parent population fitnessval in
	  let newgeneration = Generation.reproduce parents in
	  helper (times - 1) newgeneration) in
  helper n initialpop

(* Run the program *)
let _ = multiplegeneration numofgenerations
