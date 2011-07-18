signature TYPECHECKER = 
sig
  exception TypeExpError of string
  exception TypeCmdError of string

  (* typecheckExp Gamma Sigma e = tau 
   * iff 
   * Gamma entails e : tau under Sigma *)
  val typecheckExp : MADeBruijn.typ Context.context -> MADeBruijn.typ Context.context -> MADeBruijn.exp -> MADeBruijn.typ 

  (* isMobile tau = true 
   * iff 
   * tau mobile *)
  val isMobile : MADeBruijn.typ -> bool

  (* typecheckCmd Gamma Sigma c = tau 
   * iff 
   * Gamma entails c ~ tau under Sigma *)
  val typecheckCmd : MADeBruijn.typ Context.context -> MADeBruijn.typ Context.context -> MADeBruijn.cmd -> MADeBruijn.typ
end
