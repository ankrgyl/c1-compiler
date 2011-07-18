structure Evaluator : EVALUATOR = 
struct
  structure L = MADeBruijn
  structure ST = ImpStack
  structure U = MADeBruijnUtil

  exception StuckExp of string
  exception StuckCmd of string

  fun eval e =
      case e of
          L.EVar _ => raise StuckExp "evaluating open term"
        | L.EApp (e1, e2) =>
          let val (L.ELam (_, e1')) = eval e1
              val v2' = eval e2
          in eval (U.expSubstTop v2' e1') end
        | (e as L.EFix (_, e')) => eval (U.expSubstTop e e')
        | L.ELet (e1, e2) =>
          let val v1 = eval e1
          in eval (U.expSubstTop v1 e2) end
        | L.EIfz (e, e0, e1) =>
          (case eval e of
               L.EZero => eval e0
             | L.ESucc e' =>  eval (U.expSubstTop e' e1)
             | _ => raise StuckExp ("not a number " ^ " was a " ^ (U.expToString (eval e)) ))
        | L.ESucc e => L.ESucc (eval e)
        | L.EIf (e, et, ef) =>
          (case eval e of
               L.ETrue => eval et
             | L.EFalse => eval ef
             | _ => raise StuckExp "not a bool")
        (* Values. This are listed individually so we still have the benefit
         * of exhaustiveness checking. *)
        | L.ELam te => L.ELam te
        | L.EZero => e
        | L.ETrue => e
        | L.EFalse => e
        | L.EUnit => e
        | L.EDo _ => e

  fun evalExp e = eval e
      (* This is kind of abusive but it makes things really easy. *)
      handle Bind => raise StuckExp "malformed"
  
  (* You need to implement this *)
  fun evalCmd (L.CRet e, S) = L.CRet (evalExp e)
    | evalCmd (L.CGet i, S) = L.CRet (ST.get S i)
    | evalCmd (L.CSet (i, e), S) =
      let
        val v = evalExp e
        val _ = ST.set S i v
      in
        L.CRet v
      end
    | evalCmd (L.CBind (e, cmd), S) =
    (case (evalExp e) of
          (L.EDo m') =>
          (case evalCmd (m', S) of
               (L.CRet v) => evalCmd (U.cmdExpSubstTop v cmd, S)
              | _ => raise StuckCmd "e's command didn't ret")
        | _ => raise StuckCmd "e wasn't a do")
    | evalCmd (L.CDcl (e, cmd), S) =
    let
      val v = evalExp e
      val _ = ST.push S v
      val result = evalCmd (cmd, S)
      val _ = ST.pop S
    in
      case result of 
           (L.CRet v) => (L.CRet v)
         | _ => raise StuckCmd "decl's command didn't yield a ret"
    end
    
end
