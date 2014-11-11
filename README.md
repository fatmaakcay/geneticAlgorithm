geneticAlgorithm
================
Fatma Akcay, Jay Chakravarty, Howard Zhang

ABOUT: The objective of our project is to develop and evaluate a genetic algorithm that estimates coefficients of  a multiple linear regression. We plan to run this algorithm on data from self generated models in order to derive and compare the resulting regression against those models. Specifically, we plan to generate a set of data points {x1, x2,..., xn, f(x1,x2,...,xn)} and then run the algorithm on this set of data points in order to generate a regression whose accuracy can be evaluated by comparing it to f(x1,x2,...,xn). We will then conduct an analysis on the effect of changing certain variables on the accuracy and efficiency of our genetic algorithm.

INSTRUCTIONS: We wrote and compiled our code in the CS50 appliance. Run make in the directory containing all of our code (Data.ml, Fit.ml, Gen.ml, Main.ml, Pop.ml and Makefile) in order to compile our main function along with all the modules it utilizes. Then run ./Main.native at the command-line in order to generate a text file,  “generation.txt”  which contains the average fitness values, best fitness values, coefficients and total time passed since the execution of the program for each generation. 

To change the population size (popsize) and the number of bits (coefflength), open Pop.ml and change the definitions of popsize and coefflength inside the module Initialpop.

We currently have the default function set to be f(x) =2 x2+3x. In Data.ml, we have some additional example functions already implemented. To choose an existing one, select a module_name and function_name from the list available at the top of Main.ml and redefine module_name and function_name in the VARIABLES section at the top of Main.ml. Note that to create a new function (which will become function_name), define a module of type FUNCTION and pass it into DataFunctor or StochasticDataFunctor (same as DataFunctor but adds some randomness).  Please see the top of Data.ml for more detailed instructions regarding creating a new function). The output of passing the module of type FUNCTION through the functor will then become the module_name in Main.ml. 
