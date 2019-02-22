# Rholang in Haskell

## Rholang3

This implementation does not using records. Height is _not_ stored in the collection structure as before. Also the code is simpler, alpha equivalence (syntax erasure) is not mixed with substitution.  
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

## Rholang2

Modified `Rholang1` implementation with working substitution. Now I'm ready for the third version with Sum type as the initial structure, not Records.

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

## Rholang1a

`Rholang1a` is attempt to generalize parser so it does not depends on concrete structure. It compiles successfully with `UndecidableSuperClasses` but infinite cycles with Process and Name types blows up in runtime.

## Rholang1

Rholang implementation in `Rholang1` folder uses collection the same as in [Jake's example](https://gist.github.com/Jake-Gillberg/d3b686f17df530395ac296810fcc1463).

Substitution is still not working in every case and there is no distinction between Process and Name variables which may be undesirable.
