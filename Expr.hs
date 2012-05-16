module Expr where

data Expr a =
  Leaf !a
  | Apply (Expr a) (Expr a)
  | Lambda String (Expr (Maybe a))
