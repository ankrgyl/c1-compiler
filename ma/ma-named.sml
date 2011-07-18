structure MANamed = 
struct
  datatype typ = 
           TNat
         | TBool
         | TUnit
         | TArrow of typ * typ
         | TCmd of typ

  datatype exp =
           EVar of Var.t
         | EZero
         | ESucc of exp
         | EIfz of exp * exp * Var.t * exp
         | ETrue
         | EFalse
         | EIf of exp * exp * exp
         | EUnit
         | ELam of Var.t * typ * exp
         | EApp of exp * exp
         | EFix of Var.t * typ * exp
         | ELet of Var.t * exp * exp
         | EDo of cmd

       and cmd =
           (* 1st argument is the bound assignable *)  
           CDcl of Var.t * exp * cmd
           (* get the contents of the given assignable *)
         | CGet of Var.t
           (* set the contents of the given assignable *)
         | CSet of Var.t * exp
           (* 1st argument is the bound variable *)  
         | CBind of Var.t * exp * cmd
         | CRet of exp

  type program = cmd
end
