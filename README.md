# SolveEQ

## Intro

This library uses symbolic methods to solve probabilistic problems. 
The following operations are supported

Monom Algebra over a ring (of characteristic zero or two):
 * deducibility (through groebner basis computation) [1]

Rings or rings of characteristic two:
 * computation of an inverse function; [2]
 * when there exist an inverse function, decide uniformity; [2]
 * decide independence from a set of variables, in the sub case where the unbound polynomials are uniform. [2]

Diffie-Hellman Group:
 * deduction, for instances of the form X,g^h1,...,g^hn \- g^s1,...,g^sn, with h1,...,hn,s1,...,sn polynomials over some R[X,Y] [1]

## Requirements

opam packages : menhir, batteries, num

## Main Commands

`$make install` installs the ocaml library.
`$make main` compiles a small parser only interfaced with inverse and uniformity for rings.
`$make tests` run small test file based on the parser. 

## Interface 

Sources are in `libs/solveq/`. The interface for rings to use is provided in `types.mli`. Then, high level function for ring elements are provided in each `uniform.ml`, `inverter.ml`, `interference.ml`.

The internal polynomial representation can be found in `monalg.ml`, and `groebnerbasis.ml` is a (naive) implementation of groebner basis computations, the central procedure. Results of Groebner basis computations are greatly influenced by the order on variables, this is why the var type is extended with a priority field which allows to define an ordering between variables.

## Examples

The folder `tests` contains some ml files which are set up for use with the top level and the library, and contain some examples.

## References

[1] : Symbolic Proofs for Lattice-Based Cryptography. G Barthe, X Fan, J Gancher, B Grégoire, C Jacomme, E Shi
[2] : Symbolic methods in computational cryptography proofs. G Barthe, B Grégoire, C Jacomme, S Kremer, PY Strub
