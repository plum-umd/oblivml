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

* [res/examples/nroram.lo](res/examples/nroram.lo) contains the implementation of non-recursive ORAM, described in Section 5.1.
  + [Lines 37 - 56](res/examples/nroram.lo#L37) are the data definitions of non-recursive ORAM, found in the paper on lines 1024 - 1026.
  + [Lines 88 - 106](res/examples/nroram.lo#L88) is the read and remove operation on buckets (trivial ORAM), found in the paper on lines 1053 - 1064.
  + [Lines 159 - 181](res/examples/nroram.lo#L159) is the non-recursive ORAM read and remove operation on non-recursive ORAM, found in the paper on lines 1065 - 1073.
  + We also support an add operation on non-recursive ORAM ([Lines 224 - 238](res/examples/nroram.lo#L224)), and the required eviction procedure ([Lines 200 - 222](res/examples/nroram.lo#L159)).
* [res/examples/oram.lo](res/examples/oram.lo) contains the implementation of recursive ORAM, described in Section 5.2.
  + [Line 58](res/examples/oram.lo#L58) is the data definition of recursive ORAM, found in the paper on line 1147.
  + [Lines 251 - 272](res/examples/oram.lo#L251) is the read and remove operation on recursive ORAM, found in the paper on lines 1155 - 1163.
  + [Lines 274 - 303](res/examples/oram.lo#L274) is the add operation on recursive ORAM, sketched in the paper on lines 1174 - 1180.
* [res/examples/ostack.lo](res/examples/ostack.lo) contains the implementation of oblivious stacks, described in Appendix A. + [Lines 319 - 354](res/examples/ostack.lo#L319) is the `stackop` operation on oblivious stacks, described in Figure 15 (lines 1538 - 1550).

In addition to these large implementations, we also include many of the small examples that appear throughout the paper.
The examples in Figure 3 (lines 197 - 201) appear in [res/examples/affine.lo](res/examples/affine.lo) and [res/examples/uniformity.lo](res/examples/ostack.lo). The pathological example to illustrate the need for unsafe casts which appears in Appendix A.2 can be found in [res/examples/pathological.lo](res/examples/pathological.lo), and the fix with casts can be found in [res/examples/pathological-fix.lo](res/examples/pathological-fix.lo).

### Further Reading

For more information about this language, check out [A Language for Probabilistically Oblivious Computation](https://arxiv.org/pdf/1711.09305.pdf).

### License

This software is released under [CRAPL](http://matt.might.net/articles/crapl/).
