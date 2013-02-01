module Runtime where
import Expr

data Runtime a =
  RLeaf !a
  | RFun (Runtime a -> Runtime a)

runbox :: Runtime a -> a
runbox ~(RLeaf a) = a

rapply :: Runtime a -> Runtime a -> Runtime a
rapply ~(RFun f) = f

run :: Expr (Runtime a) -> Runtime a
run (Leaf a) = a
run (Apply f m) = rapply (run f) (run m)
run (Lambda _ e) = RFun $ \x -> run $ fmap (maybe x id) e

run' :: Expr a -> Runtime a
run' = run . fmap RLeaf
