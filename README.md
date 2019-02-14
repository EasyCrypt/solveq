# SolveEQ

## Intro

This library uses symbolic methods to solve probabilistic problems. 
We currently handle, either for rings or rings of characteristic two:
 * computation of an inverse function;
 * when there exist an inverse function, decide uniformity;
 * decide independence from a set of variables, in the sub case where the unbound polynomials are uniform.

## Requirements

opam packages : menhir, batteries, num

## Main Commands

`$make install` installs the ocaml library.
`$make main` compile a small parser only interfaced with inverse and uniformity for rings can be compiled using.
`$make tests` run a small test file based on the parser. 

## Interface 

Sources are in `libs/solveq/`. The interface for rings to use is provided in `types.mli`. Then, high level function for ring elements are provided in each `uniform.ml`, `inverter.ml`, `interference.ml`.

The internal polynomial representation can be found in `monalg.ml`, and `groebnerbasis.ml` is a (naive) implementation of groebner basis computations, the central procedure. Results of Groebner basis computations are greatly influenced by the order on variables, this is why the var type is extended with a priority field which allows to define an ordering between variables.

## Examples

The folder `tests` contains some ml files which are set up for use with the top level and the library, and contain some examples.
