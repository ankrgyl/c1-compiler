(* Signature for an imperative, mutable stack with random access
 * reads and writes.
 * Indexes for reads and writes are offsets from the top of
 * the stack. *)
signature IMP_STACK =
sig
  type 'a stack

  (* new (): Create a new, empty stack. *)
  val new : unit -> 'a stack

  (* push stack x: push x onto the top of stack.
   * Index 0 now refers to the location where x is. *)
  val push : 'a stack -> 'a -> unit

  (* pop stack: Remove the top element from stack.
   * If stack is empty, raise Empty. *)
  val pop : 'a stack -> unit

  (* get stack i: returns the element at offset i from the top of
   * the stack. If i is out of bounds raise Subscript. *)
  val get : 'a stack -> int -> 'a

  (* set stack i x: update the element at offset i from the top of
   * the stack to be x. If i is out of bounds raise Subscript. *)
  val set : 'a stack -> int -> 'a -> unit

  (* size stack: return the size of stack. *)
  val size : 'a stack -> int
end
