Natural Numbers
===============

The natural numbers are the numbers used for counting and ordering. Historically, they were represented as "dots" for each object and could be compared for equality, excess, or shortage by removing a dot. This was continually refined until Peano published his axioms in 1889 in "The principles of arithmetic presented by a new method." 

> module Numeric.Natural where

These natural numbers are defined by four axioms they are:

* There is a natural number 0.
* Every natural number a has $a$ natural number successor, denoted by $S(a)$
* There is no natural number whose successor is 0
* S in injective (distinct natural numbers have distinct successors: $a\neq b\rightarrow S(a)\neq S(b)$.
* If a property is possed by 0 and also by the successor of every natural number which possess it, then it is possessed by all natural numbers. (Ensures mathematical induction is valid).

The first axiom states the existence of zero.

>  data Natural = Zero

The second axiom includes the successor of every natural number as a natural number.

>    | Succ Natural deriving Show

The Haskell type system implements the remainder of the axioms.


>  add :: Natural -> Natural -> Natural
>  add Zero Zero = Zero
>  add Zero x    = x
>  add x    Zero = x
>  add (Succ x) y = add x $ Succ y

>  fromNatural :: Num a => Natural -> a
>  fromNatural Zero = 0
>  fromNatural (Succ x) = 1 + fromNatural x





<!--
TODO
Eq, Ord, Add
-->
