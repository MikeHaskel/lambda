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

instance Show a => Show (Expr a) where
  -- maintain a set of already bound variables to avoid masking
  -- furthermore, generalize to allow free variables internally
  -- (Left is a free variable, Right is a primitive)
  showsPrec = \d e -> go Set.empty d (fmap Right e) where
    go ctx d (Leaf (Left x)) | d <= 12 = (x++)
    go ctx d (Leaf (Right a)) = showsPrec d a
    go ctx d (Apply f m) | d <= 11 =
      go ctx 11 f . (' ':) . go ctx 12 m
    go ctx d (Lambda x e) | d <= 10 =
      let x' = head $ filter (flip Set.notMember ctx) $ iterate (++"'") x in
      let ctx' = Set.insert x' ctx in
      let e' = fmap (maybe (Left x') id) e in
      ("fun "++) . (x'++) . (" -> "++) . go ctx' 0 e'
    go ctx d e | d <= 12 = ('(':) . go ctx 0 e . (')':)
