module Expr where

data Expr a =
  Leaf !a
  | Apply (Expr a) (Expr a)
  | Lambda String (Expr (Maybe a))

instance Functor Expr where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Apply e e') = Apply (fmap f e) (fmap f e')
  fmap f (Lambda x e) = Lambda x $ fmap (fmap f) e
