(* A signature for variables to be used in named representations of
 * programming languages
 *)

signature VAR = 
sig
  type t
  (* creates a (not necessarily fresh) variable from the given string *)
  val fromString : string -> t

  (* creates a unique variable *)
  val fresh : unit -> t

  (* determines whether two variables are equal *)
  val eq : t -> t -> bool

  (* returns a string representation of a variable *)
  val toString : t -> string
end
