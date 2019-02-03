# Rholang in Haskell

## Rholang1

Rholang implementation in `Rholang1` folder uses collection the same as in [Jake's example](https://gist.github.com/Jake-Gillberg/d3b686f17df530395ac296810fcc1463).

Substitution is still not working in every case and there is no distinction between Process and Name variables which may be undesirable.

## Rholang1a

`Rholang1a` is attempt to generalize parser so it does not depends on concrete structure. It compiles successfully with `UndecidableSuperClasses` but infinite cycles with Process and Name types blows up in runtime.

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

## Arabic numerals repl

```sh
stack run -- -- repl-num

% ( 22 + ( 3 + 4)+5+66)
Parsed exp.:    ((((3+4)+22)+5)+66)
Evaluated exp.: 100
```
