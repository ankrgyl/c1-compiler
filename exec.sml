structure Exec : EXEC = 
struct
  exception InternalError

 (* The streams from the input library are of type (char * char list)
   * stream. The char list is "for error reporting". We don't care, so
   * we throw it out. *)
  fun cleanupStream s = Stream.map (fn (x, _) => x) s

  fun println s = print (s ^ "\n")

  fun errorMessage e = 
      case e of
          Parse.Parse s => "parse error: " ^ s
        | C1Properties.PropertyBroken s => s
        | Compiler.CompileError s => s
        | Typechecker.TypeExpError s => s
        | Typechecker.TypeCmdError s => s
        | Translator.TranslationError s => s
        | Evaluator.StuckExp s => s
        | Evaluator.StuckCmd s => s
        | Fail s => s
        | _ => exnMessage e

  fun printerror e = println (errorMessage e)

  (* parses the contents of a file as a C1 program *)
  val readFile = Parse.parse o cleanupStream o Input.readFile

  fun go s = 
      let val c1_prog = readFile s
          val () = C1Properties.assertProperties c1_prog
          val ma_prog = Compiler.compile c1_prog
          val ma_prog = Translator.removeNames ma_prog
          val _ = Typechecker.typecheckCmd Context.empty Context.empty ma_prog  
          val ma_prog = Evaluator.evalCmd (ma_prog, ImpStack.new ())
      in ma_prog end
  
  fun expToInt MADeBruijn.EZero = 0
    | expToInt (MADeBruijn.ESucc e) = expToInt e + 1
    | expToInt _ = raise Fail "malformed"

  fun cmdToInt (MADeBruijn.CRet e) = expToInt e
    | cmdToInt _ = raise Fail "malformed"

  fun evalProgram s = cmdToInt (go s)

  fun execProgram s =
      (print ("result: " ^ Int.toString (evalProgram s) ^ "\n"))
      handle e => printerror e

    
end
