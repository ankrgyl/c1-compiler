signature MA_DEBRUIJN_UTIL = 
sig
  val typToString : MADeBruijn.typ -> string
  val expToString : MADeBruijn.exp -> string
  val cmdToString : MADeBruijn.cmd -> string
  (* determines alpha equivalence on types *)
  val typEq : MADeBruijn.typ -> MADeBruijn.typ -> bool
  (* determines alpha equivalence on expressions *)
  val expEq : MADeBruijn.exp -> MADeBruijn.exp -> bool
  (* determines alpha equivalence on commands *)
  val cmdEq : MADeBruijn.cmd -> MADeBruijn.cmd -> bool

  (* Substitution functions. *)
  val expSubstTop : MADeBruijn.exp -> MADeBruijn.exp -> MADeBruijn.exp
  val cmdExpSubstTop : MADeBruijn.exp -> MADeBruijn.cmd -> MADeBruijn.cmd
end
