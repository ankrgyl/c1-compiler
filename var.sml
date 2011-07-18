structure Var : VAR = 
struct
  type t = string
 
  fun fromString s = "external_" ^ s

  local
    val c = ref 0
  in
    fun fresh () = "internal[" ^ Int.toString (!c) ^ "]" before c := (!c) + 1
  end

  fun eq (v1 : t) v2 = v1 = v2

  fun toString v = v
end
