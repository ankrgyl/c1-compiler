structure C1Properties : C1_PROPERTIES = 
struct
  exception PropertyBroken of string
 
  structure C1 = C1Named

  val mainVar = Var.fromString "main"
   
  (* ensures that the function has exactly one return statement, 
   * and that the function ends with said return statement *)
  fun assertGoodReturns (func as (t, f, params, body)) =
      let fun assertNoReturns s = 
              case s of
                  C1.SReturn _ => raise PropertyBroken "illegal return"
                | C1.SDecl (t, x, e, s) => assertNoReturns s
                | C1.SSeq (s1, s2) => (assertNoReturns s1 ; assertNoReturns s2)
                | C1.SIf (_, s1, s2) => 
                  (assertNoReturns s1 ; assertNoReturns s2)
                | C1.SWhile (_, s1) => assertNoReturns s1
                | _ => ()
          fun walkStm s = 
              case s of
                  C1.SReturn _ => ()
                | C1.SDecl (t, x, e, s) => walkStm s
                | C1.SSeq (s1, s2) => (assertNoReturns s1 ; walkStm s2)
                | _ => raise PropertyBroken "function must end with a return"
      in walkStm body end


  (* ensures the main function is the last function declared *)
  fun assertGoodMain (p as funcs) = 
      case funcs of
          [] => raise PropertyBroken "must have a main function"
        | [(_, f, _, _)] => 
          if Var.eq f mainVar
            then ()
            else raise PropertyBroken "must end with a main function"
        | ((_, f, _, _) :: fs) => 
          if Var.eq f mainVar
            then raise PropertyBroken ("main function can only be the last function" ^ Var.toString f ^ " " ^ Var.toString mainVar)
            else assertGoodMain fs

  (* asserts various static/lexical properties about C1 programs *)
  fun assertProperties (p as funcs) = 
      let val _ = List.app assertGoodReturns funcs
          val _ = assertGoodMain funcs
      in () end
     

end
