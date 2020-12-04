


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
