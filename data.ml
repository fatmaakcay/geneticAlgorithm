open Core.Std

module type DATA =
  sig
    type t
    val matrix : ((t array) array)
  end
