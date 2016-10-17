data Nat : Set where
  zero : Nat
  suc  : Nat -> Nat

_+_ : Nat -> Nat -> Nat
zero  + m  =  m
suc n + m  =  suc (n + m)

data Bool : Set where
  true  : Bool
  false : Bool

if_then_else_ : {A : Set} -> Bool -> A -> A -> A
if true  then x else y  =  x
if false then x else y  =  y
