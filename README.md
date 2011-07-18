About
=====

For Foundations of Programming Languages (15-312) at Carnegie Mellon University, we had to write a
compiler and evaluator for a small C-like language (C1).

The compiler transforms C1 into an easy-to-run language, Modernized Algol, which is interpreted with
a simple evaluator. The compiler is written in SML.

Running It
==========

You must have [SML/NJ](http://www.smlnj.org/) installed on your machine. I would recommend installing rlwrap as well, because
without it the SML interpreter is pretty unwieldy. Note that the `rlwrap` below is optional.

From the root of the project:

```
$ rlwrap sml
Standard ML of New Jersey v110.72 [built: Sun Jan 30 15:13:54 2011]
- CM.make "sources.cm";
```

To compile and run a C1 file, use

```
- Exec.evalProgram "filename"
```

For example, from the project root you can do

```
- Exec.evalProgram "tests/simple1.c1";
result: 0
val it = () : unit
```
