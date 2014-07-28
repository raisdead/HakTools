> module Numeric.Dimensional where

Physical dimensions have a unit and a quantity. The data type that represents
this will be dimensional because the quantity will have a dimension and hence
be dimensional

> data Dimensional a d = Dimensional a

The phantom type `d` represents the demension of the value `a`


