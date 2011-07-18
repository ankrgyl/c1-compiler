structure MADeBruijnUtil : MA_DEBRUIJN_UTIL = 
struct
  structure L = MADeBruijn
  
  fun typToString typ =
      let fun prec (L.TArrow _) = 1
            | prec (L.TCmd _) = 2
            | prec _ = 3

          fun parenType t = "(" ^ typToString t ^ ")"
          fun binTypeString t (t1, t2) s =
              (* All the binary operators are right associative, so
               * if the precedence is less than or equal on the left
               * we parenthesize and if the precedence is less on the
               * right we parenthesize. *)
              (if prec t1 <= prec t then parenType else typToString) t1
              ^ s ^
              (if prec t2 < prec t then parenType else typToString) t2
      in
          case typ of
              L.TArrow ts => binTypeString typ ts " -> "
            | L.TCmd t => (if prec t < prec typ
                            then parenType else typToString) t ^ " cmd"
            | L.TUnit => "unit"
            | L.TNat => "nat"
            | L.TBool => "bool"
      end

  fun bracketType t = "[" ^ typToString t ^ "]"

  fun expToString exp =
      case exp of 
          L.EVar k => "x[" ^ Int.toString k ^ "]"
        | L.EApp (e1, e2) => parensTerm e1 ^ " " ^ parensTerm e2
        | L.ELam (t, e) => "lam" ^ bracketType t ^ parensTerm e
        | L.EFix (t, e) => "fix" ^ bracketType t ^ parensTerm e
        | L.ELet (e1, e2) => "let(" ^ expToString e1 ^ ", " ^ expToString e2 ^ ")"
        | L.EUnit => "()"
        | L.EZero => "z"
        | L.ESucc e' => "S(" ^ expToString e' ^ ")"
        | L.EIfz (e', e0, e1) => "ifz(" ^ expToString e' ^ ", " ^ expToString e0 ^ ", " ^ expToString e1 ^ ")"
        | L.EIf (e', e0, e1) => "if(" ^ expToString e' ^ ", " ^ expToString e0 ^ ", " ^ expToString e1 ^ ")"
        | L.ETrue => "true"
        | L.EFalse => "false"
        | L.EDo m => "do(" ^ cmdToString m ^ ")"

  and parensTerm exp = "(" ^ expToString exp ^ ")"

  and cmdToString m =
      case m of
          L.CDcl (e, m) => "dcl(" ^ expToString e ^ "; " ^ cmdToString m ^ ")"
        | L.CGet k => "get(a[" ^ Int.toString k ^ "])"
        | L.CSet (k, e) => "set(a[" ^ Int.toString k ^ "] = " ^ expToString e ^ ")"
        | L.CBind (e, m) => "bind(" ^ expToString e ^ "; " ^ cmdToString m ^ ")"
        | L.CRet e => "ret(" ^ expToString e ^ ")"

  fun typEq (t1 : L.typ) t2 = t1 = t2
  fun expEq (e1 : L.exp) e2 = e1 = e2
  fun cmdEq (m1 : L.cmd) m2 = m1 = m2
  
  fun expMap f_var f_ass abs dcl e = let
      fun walk abs dcl e = let
          val walk0 = walk abs dcl
          val walk1 = walk (abs+1) dcl
      in
          case e of
              L.EVar k => f_var (abs, dcl, k)
            | L.EApp (e1, e2) => L.EApp (walk0 e1, walk0 e2)
            | L.ELam (t, e) => L.ELam (t, walk1 e)
            | L.EZero => L.EZero
            | L.ESucc e' => L.ESucc (walk0 e')
            | L.EIfz (e', e0, eS) => L.EIfz (walk0 e', walk0 e0, walk1 eS)
            | L.ETrue => L.ETrue
            | L.EFalse => L.EFalse
            | L.EIf (e1, e2, e3) => 
              L.EIf (walk0 e1, walk0 e2, walk0 e3)
            | L.EFix (t, e) => L.EFix (t, walk1 e)
            | L.ELet (e1, e2) => L.ELet (walk0 e1, walk1 e2)
            | L.EUnit => L.EUnit
            | L.EDo m => L.EDo (cmdMap f_var f_ass abs dcl m)
      end
  in walk abs dcl e end
  and cmdMap f_var f_ass abs dcl m = let
      fun walk abs dcl m = let
          val walk0 = walk abs dcl
          val exp = expMap f_var f_ass abs dcl
      in
          case m of
              L.CDcl (e, m) => L.CDcl (exp e, walk abs (dcl+1) m)
            | L.CGet k => L.CGet (f_ass (abs, dcl, k))
            | L.CSet (k, e) => L.CSet (f_ass (abs, dcl, k), exp e)
            | L.CBind (e, m) => L.CBind (exp e, walk (abs+1) dcl m)
            | L.CRet e => L.CRet (exp e)
      end
  in walk abs dcl m end

  fun tvarId (_, _, k) = L.EVar k
  fun assId (_, _, k) = k

  fun genericShiftAbove mapper v_shift v_cutoff a_shift a_cutoff e_or_m =
      mapper
          (fn (v_cutoff, _, v) => L.EVar (if v < v_cutoff then v else v + v_shift))
          (fn (_, a_cutoff, a) => if a < a_cutoff then a else a + a_shift)
          v_cutoff a_cutoff e_or_m
  fun genericShift mapper v_shift a_shift e_or_m =
      genericShiftAbove mapper v_shift 0 a_shift 0 e_or_m

  val expShift = genericShift expMap
  val cmdShift = genericShift cmdMap

  fun genericSubst mapper e k e'_or_m =
      mapper
          (fn (abs, dcl, k) => if k = abs then expShift abs dcl e else L.EVar k)
          assId
          k 0 e'_or_m

  val expSubst = genericSubst expMap
  val cmdExpSubst = genericSubst cmdMap


  fun expSubstTop e e' = expShift (~1) 0 (expSubst (expShift 1 0 e) 0 e')
  fun cmdExpSubstTop e m = cmdShift (~1) 0 (cmdExpSubst (expShift 1 0 e) 0 m)
end
