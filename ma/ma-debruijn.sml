structure MADeBruijn = 
struct
  datatype typ = 
           TNat
         | TBool
         | TUnit
         | TArrow of typ * typ
         | TCmd of typ

  datatype exp =
           (* uses De Bruijn indices for variables *) 
           EVar of int
         | EZero
         | ESucc of exp
           (* binds a variable in the 3rd argument *)
         | EIfz of exp * exp * exp
         | ETrue
         | EFalse
         | EIf of exp * exp * exp
         | EUnit
           (* binds a variable in the 2nd argument *)
         | ELam of typ * exp
         | EApp of exp * exp
           (* binds a variable in the 2nd argument *)
         | EFix of typ * exp
           (* binds a variable in the 2nd argument *)
         | ELet of exp * exp
         | EDo of cmd

       and cmd =  
           (* binds an assignable in the 2nd argument *)
           CDcl of exp * cmd
           (* uses De Bruijn indices for assignables *)
         | CGet of int
         | CSet of int * exp
           (* binds a variable in the 2nd argument *)
         | CBind of exp * cmd
         | CRet of exp

  type program = cmd
end
