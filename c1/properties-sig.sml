signature C1_PROPERTIES = 
sig
  exception PropertyBroken of string
  
  (* C1 properties
   *
   * (1) every function has exactly one return statement
   * (2) the last statement in a function must be a return statement
   * (3) every program has exactly one main function of the form
   *       nat main () { ... }
   * (4) every program must end with the main function
   *)
  val assertProperties : C1Named.program -> unit
end
