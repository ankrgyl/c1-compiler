signature EXEC = 
sig
  val cleanupStream : ('a * 'b) Stream.stream -> 'a Stream.stream
  val println : string -> unit
  val errorMessage : exn -> string
  val printerror : exn -> unit
  val readFile : string -> Parse.C1P.result
  val go : string -> MADeBruijn.cmd
  val expToInt : MADeBruijn.exp -> int
  val cmdToInt : MADeBruijn.cmd -> int
  
  
   (* useful for testing
   * -parses the contents of the specified file as a C1 program
   * -verifies certain static properties
   * -compiles it to MA 
   * -typechecks it
   * -executes it to a final state of a nat cmd
   * -prints the value of this nat cmd as an int
   * -handles and pretty prints all exceptions
   *)
  val execProgram : string -> unit

  (* useful for debugging
   * -parses the contents of the specified file as a C1 program
   * -verifies certain static properties
   * -compiles it to MA 
   * -typechecks it
   * -executes it to a final state of a nat cmd
   * -returns the value of this nat cmd as an int
   *)
  val evalProgram : string -> int
end
