signature EVALUATOR = 
sig
  exception StuckExp of string
  exception StuckCmd of string

  (* evaluates an expression down to a value *)
  val evalExp : MADeBruijn.exp -> MADeBruijn.exp

  (* executes a command with a store down to a final state,
   * modifying the imperative store as appropriate *)
  val evalCmd : MADeBruijn.cmd * MADeBruijn.exp ImpStack.stack ->
                MADeBruijn.cmd
end
