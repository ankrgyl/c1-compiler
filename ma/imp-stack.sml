structure ImpStack :> IMP_STACK =
struct
  structure A = Array;
  (* You need to implement all of this *)

  (* This isn't correct - you need to figure out the appropriate type *)
  type 'a stack = (('a option) A.array * int) ref;

  fun new () = ref ((A.array (1, NONE)), 0)

  fun resize a =
  let
    val len = 2 * (A.length a)
    val a' = A.array (len, NONE)
    val _ = A.copy{src=a, dst=a', di = 0}
  in
    a'
  end
  

  fun push (stackref as ref (a, n)) e =
    if (A.length a = n) then (stackref := (resize a, n); push stackref e)
    else if (A.length a > n) then (
      case (A.sub (a, n)) of NONE => (A.update (a, n, SOME e); stackref := (a, n+1); ())
                           | (SOME x) => raise Fail "pushing would cause an overwrite :-(")
    else raise Fail "corrupt stack, perceived length greater than actual"

  fun pop (ref (a, 0)) = raise Empty
    | pop (stackref as ref (a, n)) = (
    case (A.sub (a, n-1)) of (SOME x) => (A.update (a, n-1, NONE); stackref := (a, n-1); ())
                           | NONE => raise Fail "stack length is off in pop" )

  fun get (ref (a, n)) i = (
    case (A.sub (a, n - i - 1)) of (SOME x) => x
                                | NONE => raise Subscript)

  fun set (stackref as ref(a, n)) i x =
  let
    val i' = n - i - 1
  in
    if (i' < 0 orelse i' >= n) then raise Subscript
    else
        case (A.sub (a, i')) of (SOME _) => (A.update (a, i', SOME x); ())
                              | NONE => raise Fail "trying to set a NONE"
  end

  fun size (ref (a, n)) = n

end
