structure Typechecker =
struct
  exception TypeExpError of string
  exception TypeCmdError of string
   
  structure L = MADeBruijn
  structure C = Context
  structure U = MADeBruijnUtil

  val isNat = U.typEq L.TNat
  val isBool = U.typEq L.TBool

  (* You need to implement this*)
  fun isMobile L.TNat = true
    | isMobile L.TBool = true
    | isMobile L.TUnit = true
    | isMobile _ = false

  (* You need to implement this*)
  fun typecheckCmd G S (L.CRet e) = typecheckExp G S e
    | typecheckCmd G S (L.CBind (e, m)) =
    let
      val tcmd = typecheckExp G S e
    in
      case tcmd of (L.TCmd t) => typecheckCmd (C.bind G t) S m
                 | _ => raise (TypeCmdError ("Trying to bind x to " ^ (U.expToString e) ^ " which resolves to type (" ^ (U.typToString tcmd)
                 ^ ")" ^ "in body " ^ (U.cmdToString m)))
    end
    | typecheckCmd G S (L.CDcl (e, m)) =
    let
      val t = typecheckExp G S e
      val t' = typecheckCmd G (C.bind S t) m
    in
      case (isMobile t, isMobile t') of (true, true) => t'
                                      | _ => raise TypeCmdError ("Mobility error" ^ (U.expToString e) ^ "/" ^ 
                                                (U.typToString t) ^ "," ^ (U.cmdToString m) ^ "/" ^ (U.typToString t'))
    end
    | typecheckCmd G S (L.CGet a) = (
        case (C.lookup S a) of (SOME t) => t
                           | NONE => raise (TypeCmdError ("Unknown variable " ^ (Int.toString a))))
    | typecheckCmd G S (L.CSet (a, e)) = (
        case (C.lookup S a) of (SOME t) =>
          let
            val t_e = typecheckExp G S e
          in
            case (U.typEq t t_e) of true => t
                            | false => raise (TypeCmdError (
                                  "Trying to assign " ^ (Int.toString a) ^ "of type " ^ (U.typToString t) ^ " to type " ^ (U.typToString t_e)))
            end
                             | _ => raise (TypeCmdError ("Trying to write unknown variable " ^ (Int.toString a))))
  
  and typecheckExp G S e = 
      case e of
          L.EVar k => 
          (case C.lookup G k of
               NONE => raise TypeExpError "unbound exp variable"
             | SOME t =>  t)
        | L.EZero => L.TNat
        | L.ESucc e' => 
          let val t = typecheckExp G S e'
              val () = if isNat t then () else
                       raise TypeExpError "succ e : nat iff e : nat"
          in t end
        | L.EIfz (e', e0, eS) => 
          let val t' = typecheckExp G S e'
              val () = if isNat t' then () else
                       raise TypeExpError "ifz(e, e1, e2) requires e to be of type nat"
              val t0 = typecheckExp G S e0
              val tS = typecheckExp (C.bind G L.TNat) S eS
              val () = if U.typEq t0 tS then () else
                       raise TypeExpError ("ifz(e, e1, e2) requires e1 and e2 to have same type" ^ (U.typToString t0) ^ "/" ^ (U.typToString
                       tS))
          in t0 end
        | L.ETrue => L.TBool
        | L.EFalse => L.TBool
        | L.EIf (e1, e2, e3) =>
          let val t1 = typecheckExp G S e1
              val () = if isBool t1 then () else
                       raise TypeExpError ("if(e1, e2, e3) requires e1 : bool, got " ^ (U.typToString t1))
              val t2 = typecheckExp G S e2
              val t3 = typecheckExp G S e3
              val () = if U.typEq t2 t3 then () else
                       raise TypeExpError ("if(e1, e2, e3) requires e2 and e3 to have same type" ^ 
                       (U.expToString e2) ^ ":" ^ (U.typToString t2) ^ " | " ^ (U.expToString e3) ^ ":" ^ (U.typToString t3))
          in t2 end
        | L.EUnit => L.TUnit
        | L.ELam (t, e) =>
          let val t' = typecheckExp (C.bind G t) S e
          in L.TArrow (t, t') end
        | L.EApp (e1, e1') =>
          (case typecheckExp G S e1 of
               L.TArrow (t1, t2) =>
               if U.typEq t1 (typecheckExp G S e1') then t2
               else raise TypeExpError "function type doesn't match with argument type" 
             | _ => raise TypeExpError "not an arrow type")
        | L.EFix (t, e) => 
          let val t' = typecheckExp (C.bind G t) S e
              val () = if U.typEq t t' then () else
                       raise TypeExpError ("fix expression doesn't typecheck. claim[" ^ (U.typToString t) ^ "] body[" ^ (U.expToString e) ^
                       "," ^ (U.typToString t') ^ "]")
          in t end
        | L.ELet (e1, e2) =>
          let val t1 = typecheckExp G S e1
          in typecheckExp (C.bind G t1) S e2 end
        | L.EDo m => 
          let val t = typecheckCmd G S m
          in L.TCmd t end



end
