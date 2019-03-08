# Rholang in Haskell

## Rholang 4a

This is modified `Rholang 4` version without distinction between Names and Processes. For now it still has `*` nad `@` on the syntax level so the next step is to remove this distinction completely as suggested by [Joshy](https://github.com/JoshOrndorff).

## Rholang 4

In this version there is no change in functionality, only implementation is slightly different.
_Final_ representation (or shallow embedding) is completely removed. _Initial_ representation is now defined as GADT with two different constructors for Name and Process. This represent _deep embedding_ similar to `FunC` type from [Combining Deep and Shallow Embedding of Domain-Specific Languages](http://www.cse.chalmers.se/~josefs/publications/deepshallow.pdf).

## Rholang 3

This implementation does not use records. Height is _not_ stored in the collection structure as before. Also the code is simpler, alpha equivalence (syntax erasure) is not mixed with substitution.  
A new thing is the starting point for `new` term that creates new names and also the way to make side-effects.  
Repl new simulates as the term is executed inside `new stdout, stderr in { <repl_term> }`. The real output is the next goal but not before reductions (comms). Looking forward to hear [Isaac's](https://github.com/Isaac-DeFrain/KFramework) implementation and explanation in K Framework.

```sh
stack build

stack run -- -- repl # cabal new-run . repl

# `out` is free, but `stdout` is bound created with `new`
rholang $ for( a <- @42 ) { stdout!(*a) | out!(*a) }
Parsed:

for( x2 <- @{42} ) {
  x0!(*x2) | out!(*x2)
}
```

## Rholang 2

Modified `Rholang 1` implementation with working substitution. Now I'm ready for the third version with Sum type as the initial structure, not Records.

```sh
stack build

stack run -- -- repl

# or
cabal new-run . repl

rholang $ for( a <- @42 ) { a!(*a) | for( @b <- a ) { {*a|b} }  }
Parsed:

for( x1 <- @{42} ) {
  x1!(*x1) | for( @{x0} <- x1 ) {
    *x1 | x0
  }
}
```

## Rholang 1a

`Rholang 1a` is attempt to generalize parser so it does not depends on concrete structure. It compiles successfully with `UndecidableSuperClasses` but infinite cycles with Process and Name types blows up in runtime.

## Rholang 1

Rholang implementation in `Rholang 1` folder uses collection the same as in [Jake's example](https://gist.github.com/Jake-Gillberg/d3b686f17df530395ac296810fcc1463).

Substitution is still not working in every case and there is no distinction between Process and Name variables which may be undesirable.
