# OblivML

## A Language for Oblivious Computation

This repository contains an OCaml implementation of the OblivML language. The language is designed to enforce, statically (i.e. using types), that programs obey Probabilistic Memory Trace Obliviousness (PMTO). Put simply, this means that an attacker who can see (1) the program, (2) the program (instruction) counter, and (3) the memory address accesses of the program cannot infer anything about secrets which reside in memory.

This repository contains a library `oblivml` (contained in `/src`) which supports the core of the language, including...
  * A lexer / parser
  * AST definition
  * Typechecker

### Building

The projects in this repository use [Dune](https://github.com/ocaml/dune). It is being developed
on OCaml version 4.06.1.

To load the `oblivml` library into a REPL, do the following from the repository root:

```
% dune utop src
```

You can then typecheck `res/examples/affine.lo` (for example) by doing:

```
utop # open Oblivml;;
utop # open Base;;
utop # let e = Parser.start Lexer.token (Lexing.from_channel (open_in "res/examples/affine.lo"));;
utop # let t = Static.static (Map.empty (module Var)) (Map.empty (module Var)) e;;
```

At a minimum you will need the following installed via [OPAM](https://opam.ocaml.org/):

* [dune](https://github.com/ocaml/dune)
* [utop](https://github.com/diml/utop)
* [base](https://github.com/janestreet/base)

### Further Reading

For more information about this language, check out [A Language for Probabilistically Oblivious Computation](https://arxiv.org/pdf/1711.09305.pdf).

### License

This software is released under [CRAPL](http://matt.might.net/articles/crapl/).
