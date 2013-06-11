{-
Copyright (c) 2012 Mike Haskel <mike@baltimore-haskels.com>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Expr where
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char


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
    go (Leaf a) (Leaf a') | a == a' = True
    go (Apply f m) (Apply f' m') | go f f' && m == m' = True
    go (Lambda _ e) (Lambda _ e') | e == e' = True
    go _ _ = False

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

instance Read a => Read (Expr a) where
  -- maintain a map of variable names to leaf values, as well as an
  -- embedding function to map from primitives to leaf values
  readsPrec = go Map.empty id where
    go :: Read b =>
          Map String a -> (b -> a) -> Int -> String -> [(Expr a, String)]
    go ctx embd d r =
      [ (Leaf x, r) |
        d <= 12,
        (x, r) <- lexIdent r,
        x <- case Map.lookup x ctx of
          Nothing -> []
          Just x -> [x] ] ++
      [ (Leaf $ embd a, r) |
        (a, r) <- readsPrec d r ] ++
      [ (applyChain (Apply f m) ms, r) |
        d <= 11,
        (f, r) <- go ctx embd 12 r,
        (m, r) <- go ctx embd 12 r,
        (ms, r) <- goAtoms ctx embd r ] ++
      [ (Lambda x e, r) |
        d <= 10,
        ("fun", r) <- lex r,
        (x, r) <- lexIdent r,
        ("->", r) <- lex r,
        let ctx' = Map.insert x Nothing $ fmap Just ctx,
        let embd' = Just . embd,
        (e, r) <- go ctx' embd' 0 r ] ++
      [ (e, r) |
        d <= 12,
        ("(", r) <- lex r,
        (e, r) <- go ctx embd 0 r,
        (")", r) <- lex r ]
    
    lexIdent r =
      [ (x, r) |
        (x, r) <- lex r,
        length x > 0,
        isAlpha $ head x,
        x /= "fun" ]
    
    goAtoms ctx embd r = case go ctx embd 12 r of
      [] -> [([],r)]
      mReader -> [ (m:ms, r) |
                   (m, r) <- mReader,
                   (ms, r) <- goAtoms ctx embd r ]
    
    applyChain f [] = f
    applyChain f (m:ms) = applyChain (Apply f m) ms
