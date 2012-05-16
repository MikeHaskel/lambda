{-# LANGUAGE FlexibleInstances #-}
module Expr where
import qualified Data.Set as Set
import Data.Set (Set)

data Expr a =
  Leaf !a
  | Apply (Expr a) (Expr a)
  | Lambda String (Expr (Maybe a))

instance Functor Expr where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Apply e e') = Apply (fmap f e) (fmap f e')
  fmap f (Lambda x e) = Lambda x $ fmap (fmap f) e

instance Monad Expr where
  return = Leaf
  Leaf a >>= f = f a
  Apply e e' >>= f = Apply (e >>= f) (e' >>= f)
  Lambda x e >>= f = Lambda x (e >>= f') where
    f' Nothing = Leaf Nothing
    f' (Just a) = fmap Just $ f a

whnf :: Expr a -> Expr a
whnf (Apply f m) = case whnf f of
  Lambda _ e -> whnf $ e >>= maybe (whnf m) return
  f -> Apply f m
whnf e = e

instance Eq a => Eq (Expr a) where
  (==) = \e e' -> go (whnf e) (whnf e') where
    go (Leaf a) (Leaf a') = a == a'
    go (Apply f m) (Apply f' m') = go f f' && m == m'
    go (Lambda _ e) (Lambda _ e') = e == e'

instance Show (Expr String) where
  -- maintain a set of already bound variables to avoid masking
  showsPrec _ = showsAbstr Set.empty where
    showsAbstr ctx (Lambda x e) =
      let x' = freshen ctx x in
      let ctx' = Set.insert x' ctx in
      ("fun "++) .
      (x'++) .
      (" -> "++) .
      showsAbstr ctx' (e >>= maybe (return x') return)
    showsAbstr ctx e = showsAppl ctx e
    
    showsAppl ctx (Apply f m) =
      showsAppl ctx f .
      (' ':) .
      showsAtom ctx m
    showsAppl ctx e = showsAtom ctx e
    
    showsAtom ctx (Leaf x) = (x++)
    showsAtom ctx e = ('(':) . showsAbstr ctx e . (')':)

    freshen ctx = head . filter (flip Set.notMember ctx) . iterate (++"'")
