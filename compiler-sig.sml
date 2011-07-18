signature COMPILER = 
sig
  exception CompileError of string

  (* Compiles a named C1 program to a named Modernized Algol program *)
  val compile : C1Named.program -> MANamed.program
end
