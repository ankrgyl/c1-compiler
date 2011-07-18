structure Compiler : COMPILER =
struct
  structure C1 = C1Named
  structure MA = MANamed

  exception CompileError of string

  fun reverse L =
  let
    fun reverser nil B = B
      | reverser (x :: L') B = reverser L' (x :: B)
  in
    reverser L nil
  end

  fun compileExp (C1.EVar v) = MA.CGet v
    | compileExp C1.ETrue = MA.CRet MA.ETrue
    | compileExp C1.EFalse = MA.CRet MA.EFalse


    | compileExp C1.EZero = MA.CRet MA.EZero

    | compileExp (C1.ESucc e) =
    let
      val e_m = compileExp e
      val x = Var.fresh ()
    in
      MA.CBind (x, MA.EDo e_m, MA.CRet (MA.ESucc (MA.EVar x)))
    end

    | compileExp (C1.EPred e) =
    let
      val e_m = compileExp e
      val x = Var.fresh ()
      val y = Var.fresh ()
    in
      MA.CBind (x, MA.EDo e_m, MA.CRet (MA.EIfz (MA.EVar x, MA.EZero, y, MA.EVar y)))
    end

    | compileExp (C1.EIsz e) =
    let 
      val e_m = compileExp e
      val z  = Var.fresh ()
    in
      MA.CBind (z, MA.EDo e_m, MA.CRet (MA.EIfz (MA.EVar z, MA.ETrue, Var.fresh (), MA.EFalse)))
    end

    | compileExp (C1.EFuncCall (f, flist)) =
    let
      val m_list = map (fn arg => MA.EDo (compileExp arg)) flist
      (* add implicit "last" argument as unit *)
      val args = MA.EUnit :: (reverse m_list)
      fun helper nil = MA.EVar f
        | helper (x :: rL) = MA.EApp (helper rL, x)
      val ret = Var.fresh ()
    in
      MA.CBind(ret, helper args, MA.CRet (MA.EVar ret))
    end


  fun compileStm C1.SNil = MA.CRet MA.EUnit

    | compileStm (C1.SDecl (t, x, e, s)) =
    let
      val m_e = compileExp e
      val m_s = compileStm s
      val y = Var.fresh ()
    in
      MA.CBind (y, MA.EDo m_e, MA.CDcl (x, MA.EVar y, m_s))
    end

    | compileStm (C1.SAssign (x, e)) =
    let
      val m_e = compileExp e
      val y = Var.fresh ()
    in
      MA.CBind (y, MA.EDo m_e, MA.CSet (x, MA.EVar y))
    end

    | compileStm (C1.SIf (e, strue, sfalse)) =
    let
      val m_e = compileExp e
      val m_strue = MA.CBind(Var.fresh(), MA.EDo (compileStm strue), MA.CRet MA.EUnit)
      val m_sfalse = MA.CBind(Var.fresh(), MA.EDo (compileStm sfalse), MA.CRet MA.EUnit)

      val y = Var.fresh ()
      val arm = Var.fresh ()

    in
      MA.CBind (y, MA.EDo m_e, 
        MA.CBind(arm, MA.EIf (MA.EVar y, MA.EDo m_strue, MA.EDo m_sfalse),
            MA.CRet MA.EUnit))
    end

    | compileStm (C1.SWhile (e, s)) =
    let
      val cond_e = compileExp e
      val body_s = compileStm s

      val loop = Var.fresh()
      val test = Var.fresh()

      val s_true = MA.CBind (Var.fresh(), 
                             MA.EDo body_s, 
                             MA.CBind (Var.fresh (), MA.EVar loop, MA.CRet MA.EUnit))
      val s_false = MA.CRet MA.EUnit
      val body = MA.CBind (test, MA.EDo cond_e,
                   MA.CBind (Var.fresh (),
                       MA.EIf (MA.EVar test, MA.EDo s_true, MA.EDo s_false),
                       MA.CRet MA.EUnit))
      val loop_rec = MA.EFix(loop, MA.TCmd MA.TUnit, MA.EDo body)
    in
      MA.CBind(Var.fresh(), loop_rec, MA.CRet MA.EUnit)
    end

    | compileStm (C1.SSeq (s1, s2)) =
    let
      val m_s1 = compileStm s1
      val m_s2 = compileStm s2
      val ign = Var.fresh ()
    in
      MA.CBind (ign, MA.EDo m_s1, m_s2)
    end
    
    | compileStm (C1.SReturn e) =
    let
      val m_e = compileExp e
      val y = Var.fresh ()
    in
      MA.CBind (y, MA.EDo m_e, MA.CRet (MA.EVar y))
    end



  fun convertType C1.TNat = MA.TNat
    | convertType C1.TBool = MA.TBool

  fun compileFun (t_ret, f, L, s) =
  let
    val m_s = compileStm s

    fun buildDecls nil = m_s
      | buildDecls ((x', x) :: L) = 
      let
        val temp = Var.fresh()
      in
        MA.CBind (temp, MA.EVar x', MA.CDcl (x, MA.EVar temp, buildDecls L))
      end

    fun generateBody nil vars = MA.ELam (Var.fresh (), MA.TUnit, MA.EDo (buildDecls vars))
      | generateBody ((t, x)::L) vars = 
      let
        val x' = Var.fresh ()
      in
        MA.ELam (x', MA.TCmd (convertType t), generateBody L (vars @ [(x', x)]))
      end

    fun generateType nil = MA.TArrow(MA.TUnit, MA.TCmd (convertType t_ret))
      | generateType ((t, x) :: L) = MA.TArrow(MA.TCmd (convertType t), generateType L)
  in
    MA.EFix (f, generateType L, generateBody L nil)
  end

  (* You need to implement this *)
  fun compileLet nil = raise Fail "compiling a blank thing"
    | compileLet [f] = MA.EApp(compileFun f, MA.EUnit)
    | compileLet ((f as (_, fname, _, _)) :: L) = MA.ELet (fname, compileFun f, compileLet L)

  (*fun compile L = MA.CRet (compileLet L) *)

  fun compile L =
  let
    val ret = Var.fresh()
  in
    MA.CBind (ret, compileLet L, MA.CRet (MA.EVar ret))
  end

end
