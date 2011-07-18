structure C1Named = 
struct
  datatype typ =
           TNat
         | TBool

  datatype exp = 
           EVar of Var.t
         | ETrue
         | EFalse
         | EZero
         | ESucc of exp
         | EPred of exp
         | EIsz of exp
         | EFuncCall of Var.t * exp list

  datatype stm = 
           SNil
         (* declares a variable to be used in a statement *)
         | SDecl of typ * Var.t * exp * stm
         | SAssign of Var.t * exp
         (* mandatory else statement *)
         | SIf of exp * stm * stm
         | SWhile of exp * stm
         (* sequences two statement *)
         | SSeq of stm * stm
         | SReturn of exp
        
  (* return type, function name, parameters, function body *)
  type function = typ * Var.t * (typ * Var.t) list * stm

  type program = function list
end
