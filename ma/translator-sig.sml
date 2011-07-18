signature TRANSLATOR = 
sig
  exception TranslationError of string

  (* converts a named (closed) MA program to an unnamed one *)
  val removeNames : MANamed.program -> MADeBruijn.program
end
