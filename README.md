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

```
% opam switch create oblivml 4.06.1
% eval $(opam env)
% opam update
% opam install dune utop core
```

To load the `oblivml` library into a REPL, do the following from the repository root:

```
% dune utop src
```

You can then typecheck `res/examples/affine.lo` (for example) by doing:

```
utop # open Oblivml;;
utop # open Base;;
utop # let Some e = Util.parse_file "res/examples/affine.lo";;
utop # let t = Static.typecheck e;;
```

At a minimum you will need the following installed via [OPAM](https://opam.ocaml.org/):

* [dune](https://github.com/ocaml/dune)
* [utop](https://github.com/diml/utop)
* [core](https://github.com/janestreet/core)

### Examples

* [res/examples/nroram.lo](res/examples/nroram.lo) contains the implementation of non-recursive ORAM, described in Section 5.1
  + [Lines 37 - 56](res/examples/nroram.lo#L37) are the data definitions found in the paper on lines 1024 - 1026.
  + [Lines 88 - 106](res/examples/nroram.lo#L88) is the read and remove operation on buckets (trivial ORAM), found in the paper on lines 1050 - 1060.
  + [Lines 159 - 181](res/examples/nroram.lo#L159) is the non-recursive ORAM read and remove operation on non-recursive ORAM, found in the paper on lines 1062 - 1070.
  + We also support an add operation on non-recursive ORAM ([Lines 224 - 238](res/examples/nroram.lo#L224)), and the required eviction procedure ([Lines 200 - 222](res/examples/nroram.lo#L159)).

### Further Reading

For more information about this language, check out [A Language for Probabilistically Oblivious Computation](https://arxiv.org/pdf/1711.09305.pdf).

### License

This software is released under [CRAPL](http://matt.might.net/articles/crapl/).
