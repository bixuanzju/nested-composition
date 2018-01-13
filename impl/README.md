# Prototype Implementation

## Build and Run

This project can be built with
[cabal](https://www.haskell.org/cabal/download.html) or
[stack](https://docs.haskellstack.org/en/stable/README/).

* cabal
```
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal exec SEDEL-exe
```

* stack
```
stack build
stack exec SEDEL-exe
```

## REPL

The REPL prompt is `>`:
- type `:q` to quit or input any expression in the source language to check its
result
- type `:load` to load a file

```
> 2
Typing result
: Int

Evaluation result
=> 2.0
```

## Quick Reference

A program consists of list of declarations (separated by `;`), ended with a `main` declaration.
Like Haskell, a line comment starts with `--` and a comment block is wrapped by
`{-` and `-}`. 

* Primitive type: `Int`, `Bool`, `Double, ``String`
* Top type/value: `() : Top`
* Type annotation: `2 : Int`
* Merge: `true ,, 3`
* Intersection type: `Bool & (Int -> Int)`
* If: `if x == 0 then true else false`
* λ term: `(\x -> x+1) : Int -> Int`
* Λ term: `/\ (A * Int) . (\x -> x) : A -> A`
* Disjoint (universal) quantification: `forall (A*Int) . A -> A`
* Term declaration: `id A (x : A) = x`
* Type declaration: `type Person = {name : String, male : Bool}`

## Examples and Case Study

See the [examples/](./examples/) directory, in particular, examples in Section 2
of the paper are inside [examples/overview.sl](./examples/overview.sl). All
examples can be tested:

```
stack test
```
