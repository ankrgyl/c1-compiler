functor C1LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : C1_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure L = C1Named
exception Parse of string

fun createnat 0 = L.EZero
  | createnat n = L.ESucc (createnat (n-1))

type var = Var.t

fun makeVar s = Var.fromString s


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\008\000\000\000\
\\001\000\002\000\027\000\011\000\026\000\013\000\025\000\014\000\024\000\
\\018\000\013\000\023\000\023\000\000\000\
\\001\000\002\000\030\000\000\000\
\\001\000\002\000\034\000\000\000\
\\001\000\002\000\046\000\005\000\045\000\006\000\044\000\007\000\043\000\
\\008\000\042\000\009\000\041\000\016\000\040\000\017\000\070\000\
\\024\000\039\000\025\000\038\000\000\000\
\\001\000\002\000\046\000\005\000\045\000\006\000\044\000\007\000\043\000\
\\008\000\042\000\009\000\041\000\016\000\040\000\024\000\039\000\
\\025\000\038\000\000\000\
\\001\000\003\000\007\000\004\000\006\000\000\000\
\\001\000\003\000\007\000\004\000\006\000\017\000\016\000\000\000\
\\001\000\016\000\011\000\000\000\
\\001\000\016\000\047\000\000\000\
\\001\000\016\000\048\000\000\000\
\\001\000\016\000\054\000\000\000\
\\001\000\016\000\055\000\000\000\
\\001\000\016\000\057\000\000\000\
\\001\000\017\000\050\000\000\000\
\\001\000\017\000\067\000\000\000\
\\001\000\017\000\071\000\000\000\
\\001\000\017\000\072\000\000\000\
\\001\000\017\000\075\000\000\000\
\\001\000\017\000\076\000\000\000\
\\001\000\017\000\077\000\000\000\
\\001\000\017\000\082\000\000\000\
\\001\000\018\000\013\000\000\000\
\\001\000\019\000\033\000\000\000\
\\001\000\020\000\049\000\000\000\
\\001\000\020\000\052\000\000\000\
\\001\000\023\000\053\000\000\000\
\\001\000\023\000\073\000\000\000\
\\001\000\023\000\074\000\000\000\
\\089\000\000\000\
\\090\000\003\000\007\000\004\000\006\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\022\000\029\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\002\000\027\000\003\000\007\000\004\000\006\000\011\000\026\000\
\\013\000\025\000\014\000\024\000\018\000\013\000\023\000\023\000\000\000\
\\104\000\002\000\027\000\011\000\026\000\013\000\025\000\014\000\024\000\
\\018\000\013\000\023\000\023\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\012\000\085\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\022\000\079\000\000\000\
\\120\000\000\000\
\\121\000\016\000\059\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\"
val actionRowNumbers =
"\031\000\001\000\031\000\030\000\
\\035\000\034\000\009\000\032\000\
\\023\000\008\000\033\000\045\000\
\\038\000\003\000\036\000\046\000\
\\043\000\045\000\024\000\052\000\
\\004\000\048\000\006\000\010\000\
\\011\000\025\000\015\000\007\000\
\\040\000\047\000\042\000\041\000\
\\026\000\057\000\056\000\027\000\
\\012\000\013\000\006\000\014\000\
\\067\000\066\000\065\000\064\000\
\\063\000\006\000\006\000\006\000\
\\037\000\038\000\006\000\053\000\
\\006\000\006\000\016\000\006\000\
\\058\000\005\000\017\000\018\000\
\\028\000\039\000\029\000\019\000\
\\020\000\071\000\021\000\061\000\
\\059\000\002\000\002\000\049\000\
\\044\000\069\000\070\000\068\000\
\\022\000\006\000\051\000\054\000\
\\060\000\061\000\050\000\002\000\
\\062\000\055\000\000\000"
val gotoT =
"\
\\001\000\086\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\002\000\007\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\008\000\000\000\
\\000\000\
\\008\000\010\000\000\000\
\\004\000\013\000\007\000\012\000\000\000\
\\000\000\
\\004\000\020\000\008\000\019\000\009\000\018\000\010\000\017\000\
\\011\000\016\000\012\000\015\000\000\000\
\\006\000\026\000\000\000\
\\000\000\
\\000\000\
\\008\000\019\000\011\000\029\000\012\000\015\000\000\000\
\\000\000\
\\004\000\020\000\008\000\019\000\009\000\030\000\010\000\017\000\
\\011\000\016\000\012\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\035\000\015\000\034\000\016\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\013\000\007\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\054\000\015\000\034\000\016\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\056\000\000\000\
\\014\000\058\000\015\000\034\000\016\000\033\000\000\000\
\\014\000\059\000\015\000\034\000\016\000\033\000\000\000\
\\014\000\060\000\015\000\034\000\016\000\033\000\000\000\
\\000\000\
\\006\000\061\000\000\000\
\\014\000\062\000\015\000\034\000\016\000\033\000\000\000\
\\000\000\
\\014\000\063\000\015\000\034\000\016\000\033\000\000\000\
\\014\000\064\000\015\000\034\000\016\000\033\000\000\000\
\\000\000\
\\014\000\066\000\015\000\034\000\016\000\033\000\000\000\
\\000\000\
\\014\000\067\000\015\000\034\000\016\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\076\000\000\000\
\\000\000\
\\008\000\019\000\012\000\078\000\000\000\
\\008\000\019\000\012\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\081\000\015\000\034\000\016\000\033\000\000\000\
\\000\000\
\\013\000\082\000\000\000\
\\000\000\
\\018\000\084\000\000\000\
\\000\000\
\\008\000\019\000\012\000\085\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 87
val numrules = 41
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUMBER of unit ->  (int) | IDENT of unit ->  (string)
 | func_argsf of unit ->  (L.exp list)
 | func_args of unit ->  (L.exp list) | func_call of unit ->  (L.exp)
 | atomic_exp of unit ->  (L.exp) | exp of unit ->  (L.exp)
 | elsestm of unit ->  (L.stm) | stm of unit ->  (L.stm)
 | stms of unit ->  (L.stm)
 | decl of unit ->  ( ( L.typ * var * L.exp ) )
 | decls_stms of unit ->  (L.stm) | block of unit ->  (L.stm)
 | param of unit ->  (L.typ*var)
 | paramsf of unit ->  ( ( L.typ * var )  list)
 | params of unit ->  ( ( L.typ * var )  list)
 | typ of unit ->  (L.typ) | fdef of unit ->  (L.function)
 | program of unit ->  (L.program) | start of unit ->  (L.program)
end
type svalue = MlyValue.svalue
type result = L.program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "IDENT"
  | (T 2) => "NAT"
  | (T 3) => "BOOL"
  | (T 4) => "TRUE"
  | (T 5) => "FALSE"
  | (T 6) => "ZERO"
  | (T 7) => "NUMBER"
  | (T 8) => "SUCC"
  | (T 9) => "NATCASE"
  | (T 10) => "IF"
  | (T 11) => "ELSE"
  | (T 12) => "WHILE"
  | (T 13) => "RETURN"
  | (T 14) => "OF"
  | (T 15) => "LPAREN"
  | (T 16) => "RPAREN"
  | (T 17) => "LBRACE"
  | (T 18) => "RBRACE"
  | (T 19) => "EQUALS"
  | (T 20) => "GOESTO"
  | (T 21) => "COMMA"
  | (T 22) => "SEMI"
  | (T 23) => "ISZ"
  | (T 24) => "PRED"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ 
(T 2) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.start (fn _
 => let val  (program as program1) = program1 ()
 in (program)
end)
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)

end
|  ( 1, ( rest671)) => let val  result = MlyValue.program (fn _ => ([]
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.program program1, _, program1right)) :: ( _,
 ( MlyValue.fdef fdef1, fdef1left, _)) :: rest671)) => let val  result
 = MlyValue.program (fn _ => let val  (fdef as fdef1) = fdef1 ()
 val  (program as program1) = program1 ()
 in (fdef :: program)
end)
 in ( LrTable.NT 1, ( result, fdef1left, program1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.block block1, _, block1right)) :: ( _, ( 
MlyValue.params params1, _, _)) :: ( _, ( MlyValue.IDENT IDENT1, _, _)
) :: ( _, ( MlyValue.typ typ1, typ1left, _)) :: rest671)) => let val  
result = MlyValue.fdef (fn _ => let val  (typ as typ1) = typ1 ()
 val  (IDENT as IDENT1) = IDENT1 ()
 val  (params as params1) = params1 ()
 val  (block as block1) = block1 ()
 in ((typ, makeVar IDENT, params, block))
end)
 in ( LrTable.NT 2, ( result, typ1left, block1right), rest671)
end
|  ( 4, ( ( _, ( _, NAT1left, NAT1right)) :: rest671)) => let val  
result = MlyValue.typ (fn _ => (L.TNat))
 in ( LrTable.NT 3, ( result, NAT1left, NAT1right), rest671)
end
|  ( 5, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.typ (fn _ => (L.TBool))
 in ( LrTable.NT 3, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _)) ::
 rest671)) => let val  result = MlyValue.params (fn _ => ([]))
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.paramsf 
paramsf1, _, _)) :: ( _, ( MlyValue.param param1, _, _)) :: ( _, ( _, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.params (fn
 _ => let val  (param as param1) = param1 ()
 val  (paramsf as paramsf1) = paramsf1 ()
 in (param :: paramsf)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.paramsf (fn _ => ([]
))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( MlyValue.paramsf paramsf1, _, paramsf1right)) :: ( _,
 ( MlyValue.param param1, _, _)) :: ( _, ( _, COMMA1left, _)) :: 
rest671)) => let val  result = MlyValue.paramsf (fn _ => let val  (
param as param1) = param1 ()
 val  (paramsf as paramsf1) = paramsf1 ()
 in (param :: paramsf)
end)
 in ( LrTable.NT 5, ( result, COMMA1left, paramsf1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.IDENT IDENT1, _, IDENT1right)) :: ( _, ( 
MlyValue.typ typ1, typ1left, _)) :: rest671)) => let val  result = 
MlyValue.param (fn _ => let val  (typ as typ1) = typ1 ()
 val  (IDENT as IDENT1) = IDENT1 ()
 in (typ, makeVar IDENT)
end)
 in ( LrTable.NT 6, ( result, typ1left, IDENT1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.decls_stms 
decls_stms1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.block (fn _ => let val  (decls_stms as 
decls_stms1) = decls_stms1 ()
 in (decls_stms)
end)
 in ( LrTable.NT 7, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.decls_stms decls_stms1, _, decls_stms1right
)) :: ( _, ( MlyValue.decl decl1, decl1left, _)) :: rest671)) => let
 val  result = MlyValue.decls_stms (fn _ => let val  (decl as decl1) =
 decl1 ()
 val  (decls_stms as decls_stms1) = decls_stms1 ()
 in (
case decl of 
                                     (t, x, e) => L.SDecl (t, x, e, decls_stms)
)
end)
 in ( LrTable.NT 8, ( result, decl1left, decls_stms1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.stms stms1, stms1left, stms1right)) :: 
rest671)) => let val  result = MlyValue.decls_stms (fn _ => let val  (
stms as stms1) = stms1 ()
 in (stms)
end)
 in ( LrTable.NT 8, ( result, stms1left, stms1right), rest671)
end
|  ( 14, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.exp exp1, _, _
)) :: _ :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( MlyValue.typ
 typ1, typ1left, _)) :: rest671)) => let val  result = MlyValue.decl
 (fn _ => let val  (typ as typ1) = typ1 ()
 val  (IDENT as IDENT1) = IDENT1 ()
 val  (exp as exp1) = exp1 ()
 in (typ, makeVar IDENT, exp)
end)
 in ( LrTable.NT 9, ( result, typ1left, SEMI1right), rest671)
end
|  ( 15, ( rest671)) => let val  result = MlyValue.stms (fn _ => (
L.SNil))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 16, ( ( _, ( MlyValue.stms stms1, _, stms1right)) :: ( _, ( 
MlyValue.stm stm1, stm1left, _)) :: rest671)) => let val  result = 
MlyValue.stms (fn _ => let val  (stm as stm1) = stm1 ()
 val  (stms as stms1) = stms1 ()
 in (
case stms of 
                                      L.SNil => stm 
                                    | _ => L.SSeq (stm, stms)
)
end)
 in ( LrTable.NT 10, ( result, stm1left, stms1right), rest671)
end
|  ( 17, ( ( _, ( _, SEMI1left, SEMI1right)) :: rest671)) => let val  
result = MlyValue.stm (fn _ => (L.SNil))
 in ( LrTable.NT 11, ( result, SEMI1left, SEMI1right), rest671)
end
|  ( 18, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.exp exp1, _, _
)) :: _ :: ( _, ( MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671))
 => let val  result = MlyValue.stm (fn _ => let val  (IDENT as IDENT1)
 = IDENT1 ()
 val  (exp as exp1) = exp1 ()
 in (L.SAssign (makeVar IDENT, exp))
end)
 in ( LrTable.NT 11, ( result, IDENT1left, SEMI1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.elsestm elsestm1, _, elsestm1right)) :: ( _
, ( MlyValue.stm stm1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _))
 :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.stm (fn _ => let val  (exp as exp1) = exp1 ()
 val  (stm as stm1) = stm1 ()
 val  (elsestm as elsestm1) = elsestm1 ()
 in (L.SIf (exp, stm, elsestm))
end)
 in ( LrTable.NT 11, ( result, IF1left, elsestm1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.stm stm1, _, stm1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, WHILE1left, _)) :: rest671
)) => let val  result = MlyValue.stm (fn _ => let val  (exp as exp1) =
 exp1 ()
 val  (stm as stm1) = stm1 ()
 in (L.SWhile (exp, stm))
end)
 in ( LrTable.NT 11, ( result, WHILE1left, stm1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.block block1, block1left, block1right)) :: 
rest671)) => let val  result = MlyValue.stm (fn _ => let val  (block
 as block1) = block1 ()
 in (block)
end)
 in ( LrTable.NT 11, ( result, block1left, block1right), rest671)
end
|  ( 22, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.exp exp1, _, _
)) :: ( _, ( _, RETURN1left, _)) :: rest671)) => let val  result = 
MlyValue.stm (fn _ => let val  (exp as exp1) = exp1 ()
 in (L.SReturn exp)
end)
 in ( LrTable.NT 11, ( result, RETURN1left, SEMI1right), rest671)
end
|  ( 23, ( rest671)) => let val  result = MlyValue.elsestm (fn _ => (
L.SNil))
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 24, ( ( _, ( MlyValue.stm stm1, _, stm1right)) :: ( _, ( _, 
ELSE1left, _)) :: rest671)) => let val  result = MlyValue.elsestm (fn
 _ => let val  (stm as stm1) = stm1 ()
 in (stm)
end)
 in ( LrTable.NT 12, ( result, ELSE1left, stm1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.atomic_exp atomic_exp1, atomic_exp1left, 
atomic_exp1right)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  (atomic_exp as atomic_exp1) = atomic_exp1 ()
 in (atomic_exp)
end)
 in ( LrTable.NT 13, ( result, atomic_exp1left, atomic_exp1right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.func_call func_call1, func_call1left, 
func_call1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (func_call as func_call1) = func_call1 ()
 in (func_call)
end)
 in ( LrTable.NT 13, ( result, func_call1left, func_call1right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.func_args func_args1, _, func_args1right))
 :: ( _, ( MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671)) => let
 val  result = MlyValue.func_call (fn _ => let val  (IDENT as IDENT1)
 = IDENT1 ()
 val  (func_args as func_args1) = func_args1 ()
 in (L.EFuncCall (makeVar IDENT, func_args))
end)
 in ( LrTable.NT 15, ( result, IDENT1left, func_args1right), rest671)

end
|  ( 28, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.func_args (fn _ => ([]))
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 29, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.func_argsf 
func_argsf1, _, _)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.func_args
 (fn _ => let val  (exp as exp1) = exp1 ()
 val  (func_argsf as func_argsf1) = func_argsf1 ()
 in (exp :: func_argsf)
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 30, ( rest671)) => let val  result = MlyValue.func_argsf (fn _ =>
 ([]))
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 31, ( ( _, ( MlyValue.func_argsf func_argsf1, _, func_argsf1right
)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, COMMA1left, _)) ::
 rest671)) => let val  result = MlyValue.func_argsf (fn _ => let val 
 (exp as exp1) = exp1 ()
 val  (func_argsf as func_argsf1) = func_argsf1 ()
 in (exp :: func_argsf)
end)
 in ( LrTable.NT 17, ( result, COMMA1left, func_argsf1right), rest671)

end
|  ( 32, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.atomic_exp (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 in (L.EVar (makeVar IDENT))
end)
 in ( LrTable.NT 14, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 33, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.atomic_exp (fn _ => (L.ETrue))
 in ( LrTable.NT 14, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 34, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.atomic_exp (fn _ => (L.EFalse))
 in ( LrTable.NT 14, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 35, ( ( _, ( _, ZERO1left, ZERO1right)) :: rest671)) => let val  
result = MlyValue.atomic_exp (fn _ => (L.EZero))
 in ( LrTable.NT 14, ( result, ZERO1left, ZERO1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.NUMBER NUMBER1, NUMBER1left, NUMBER1right))
 :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ => let
 val  (NUMBER as NUMBER1) = NUMBER1 ()
 in (createnat NUMBER)
end)
 in ( LrTable.NT 14, ( result, NUMBER1left, NUMBER1right), rest671)

end
|  ( 37, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( _, SUCC1left, _)) :: rest671)) => let val  result
 = MlyValue.atomic_exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (L.ESucc exp)
end)
 in ( LrTable.NT 14, ( result, SUCC1left, RPAREN1right), rest671)
end
|  ( 38, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( _, PRED1left, _)) :: rest671)) => let val  result
 = MlyValue.atomic_exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (L.EPred exp)
end)
 in ( LrTable.NT 14, ( result, PRED1left, RPAREN1right), rest671)
end
|  ( 39, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( _, ISZ1left, _)) :: rest671)) => let val  result =
 MlyValue.atomic_exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (L.EIsz exp)
end)
 in ( LrTable.NT 14, ( result, ISZ1left, RPAREN1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atomic_exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 14, ( result, LPAREN1left, RPAREN1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : C1_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun NAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMBER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.NUMBER (fn () => i),p1,p2))
fun SUCC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun NATCASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun GOESTO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ISZ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PRED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
end
end
