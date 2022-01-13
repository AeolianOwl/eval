# Eval - A simple boolean/arithmetic expression evaluator

## Running
```
$ cabal run
```
## Exiting
```
> quit
> exit

Ctrl + C
```
---
## Available Expressions
- Booleans
```
> true
> false
```
- Integers
```
> 42
```
- Reals
```
> -3.14
```
- Arithmetic Expressions
```
> (+ a b)
> (- a b)
> (* a b)
> (/ a b)
> (** a b)
```
- Boolean Expressions
```
> (= a b)
> (/= a b)
> (>= a b)
> (> a b)
> (<= a b)
> (< a b)
> (and a b)
> (or a b)
> (not a)
```
where `a` and `b` are themselves expressions which can be nested.

---
## Misc. Notes
- Input is line-buffered (as per the default), and a cursor/input history are not implemented.
- All numbers are parsed into Doubles.