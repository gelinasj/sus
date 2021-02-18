


```

vdef = var (l)

def ::= Var = t

match ::= Var
        | <v=(l=match...)>

primop ::= + | - | * | / | > | < | == | && | || | !

env ::= empty
      | var = val, env

closure ::= <env, (fn var... t)>

val ::= Int
      | Double
      | Bool
      | closure
      | <v=(l=val...)>

t ::= Val
    | Var
    | primop
    | variant
    | case t of match => t...
    | (t...)
    | let* def... in t
    | let def... in t
    | (fn var... t)

e ::= ...

result := val | e

```

### Etiquette

- Rather comprehensive unit tests for main interfaces
- Lesser comprehensive unit tests for helpers
- integration tests
- quickcheck tests

### Future Todo's

- Eval
- Currying
- Types with parametric polymorphism
- Recursive types
- concrete syntax parser
- repl
- type inference???
- Liquid haskell???
- explore features???
- test with others???
