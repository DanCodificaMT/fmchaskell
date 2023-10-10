module Nat where
import Prelude hiding (sum, exp, pred, min, max, div, quot, rem)

data Nat = O | S Nat
    deriving(Eq, Show)

sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S (sum n m)

mult :: Nat -> Nat -> Nat
mult _ O = O
mult n (S m) = sum (mult n m) n

exp :: Nat -> Nat -> Nat
exp _ O = S O
exp n (S m) = mult n (exp n m)

monus :: Nat -> Nat -> Nat
monus n O = n
monus n (S m) = pred (monus n m)

double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

pred :: Nat -> Nat
pred O = O
pred (S n) = n

fact :: Nat -> Nat
fact O = S O
fact (S n) = mult (S n) (fact n)

fib :: Nat -> Nat
fib (S (S n)) = sum (fib (S n)) (fib n)
fib n = n

min :: Nat -> Nat -> Nat
min _ O = O
min O _ = O
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max n O = n
max O n = n
max (S n) (S m) = S (max m n)

div :: Nat -> Nat -> (Nat, Nat)
div _ O = error "Divisão por zero"
div n m = (n, m)

quot :: Nat -> Nat -> Nat
quot n m = fst (div n m)

rem :: Nat -> Nat -> Nat
rem n m = snd (div n m)