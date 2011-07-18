structure Translator : TRANSLATOR = 
struct
  exception TranslationError of string

  structure C = Context
  structure L1 = MANamed
  structure L2 = MADeBruijn
  structure V = Var

  fun translateTyp t = 
      case t of
          L1.TNat => L2.TNat
        | L1.TBool => L2.TBool
        | L1.TUnit => L2.TUnit
        | L1.TArrow (t1, t2) => L2.TArrow (translateTyp t1, translateTyp t2)
        | L1.TCmd t => L2.TCmd (translateTyp t)


  (* gamma is a context of variables to names
   * sigma is a context of assignables to names *)

  fun removeNamesExp gamma sigma e = 
      case e of
          L1.EVar x =>
          (case C.search (fn (_, x') => V.eq x x') gamma of
              SOME (i, _) => L2.EVar i
            | NONE => 
              raise TranslationError ("undeclared variable " ^ V.toString x))
        | L1.EZero => L2.EZero
        | L1.ESucc e => L2.ESucc (removeNamesExp gamma sigma e)
        | L1.EIfz (e, e0, x, eS) =>
          let val e' = removeNamesExp gamma sigma e
              val e0' = removeNamesExp gamma sigma e0
              val eS' = removeNamesExp (C.bind gamma x) sigma eS
          in L2.EIfz (e', e0', eS') end
        | L1.ETrue => L2.ETrue
        | L1.EFalse => L2.EFalse
        | L1.EIf (e1, e2, e3) =>
          let val e1' = removeNamesExp gamma sigma e1
              val e2' = removeNamesExp gamma sigma e2
              val e3' = removeNamesExp gamma sigma e3
          in L2.EIf (e1', e2', e3') end
        | L1.EUnit => L2.EUnit
        | L1.ELam (x, t, e) => 
          let val t' = translateTyp t
              val e' = removeNamesExp (C.bind gamma x) sigma e
          in L2.ELam (t', e') end
        | L1.EApp (e1, e2) =>
          let val e1' = removeNamesExp gamma sigma e1
              val e2' = removeNamesExp gamma sigma e2
          in L2.EApp (e1', e2') end
        | L1.EFix (x, t, e) => 
          let val t' = translateTyp t
              val e' = removeNamesExp (C.bind gamma x) sigma e
          in L2.EFix (t', e') end
        | L1.ELet (x, e1, e2) => 
          let val e1' = removeNamesExp gamma sigma e1
              val e2' = removeNamesExp (C.bind gamma x) sigma e2
          in L2.ELet (e1', e2') end
        | L1.EDo m => L2.EDo (removeNamesCmd gamma sigma m)
         
  and removeNamesCmd gamma sigma m = 
      case m of
          L1.CDcl (a, e, m) =>
          let val e' = removeNamesExp gamma sigma e
              val m' = removeNamesCmd gamma (C.bind sigma a) m
          in L2.CDcl (e', m') end
        | L1.CGet a =>
          (case C.search (fn (_, a') => V.eq a a') sigma of
              SOME (i, _) => L2.CGet i
            | NONE => 
              raise TranslationError ("undeclared assignable " ^ V.toString a))
        | L1.CSet (a, e) =>
          (case C.search (fn (_, a') => V.eq a a') sigma of
              SOME (i, _) => L2.CSet (i, removeNamesExp gamma sigma e)
            | NONE => 
              raise TranslationError ("undeclared assignable " ^ V.toString a))
        | L1.CBind (x, e, m) =>
          let val e' = removeNamesExp gamma sigma e
              val m' = removeNamesCmd (C.bind gamma x) sigma m
          in L2.CBind (e', m') end
        | L1.CRet e => L2.CRet (removeNamesExp gamma sigma e)

  fun removeNames p = removeNamesCmd C.empty C.empty p 
end
