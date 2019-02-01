# Rholang in Haskell

## Run

```
stack build

stack run
```

## Rholang

Rholang implementation in `Rholang1` folder uses collection the same as in [Jake's example](https://gist.github.com/Jake-Gillberg/d3b686f17df530395ac296810fcc1463).

Substitution is still not working in every case and there is no distinction between Process and Name variables which may be undesirable.

`Rholang1a` is attempt to generalize parser so it does not depends on concrete structure. It compiles successfully with `UndecidableSuperClasses` but infinite cycles with Process and Name types blows up in runtime.

```hs
Rholang1.Syntax> run pPar "for( a     <- @42   ) { for( b  <- a  ) { a!(*b)     | a  } }"
--                         for( @{x1} <- @{42} ) { for( x0 <- x1 ) { @{x1}!(x0) | x1 } }
```

## Arabic numerals repl

```
stack run -- -- --repl

% ( 22 + ( 3 + 4)+5+66)
Parsed exp.:    ((((3+4)+22)+5)+66)
Evaluated exp.: 100
```
