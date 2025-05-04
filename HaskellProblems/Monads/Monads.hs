import Control.Monad (liftM, ap)

-- SAFE MONAD
data Safe a = Error String | Value a
  deriving (Show)

-- Monad bind function for Safe
bindSafe :: Safe a -> (a -> Safe b) -> Safe b
bindSafe (Error s) _ = Error s                                              -- propagate error
bindSafe (Value x) f = f x                                                  -- apply function if value is present

instance Functor Safe where
  fmap f (Error s) = Error s
  fmap f (Value x) = Value (f x)

instance Applicative Safe where
  pure = Value
  (Error s) <*> _ = Error s
  (Value f) <*> x = fmap f x

instance Monad Safe where
  (>>=) = bindSafe

-- tester safe
testSafe :: IO ()
testSafe = do
  let x = Value 10                                                          -- valid value
      f n = if n > 0 then Value (n * 2) else Error "Non-positive"
  print (x >>= f)                                                           -- Value 20
  print (Error "Oops" >>= f)                                                -- Error "Oops"
  print (Value (-3) >>= f)                                                  -- Error "Non-positive"


-- *****************************************************************************************************

-- FTREE MONAD
data FTree a = Leaf a | Node [FTree a]                                  -- Tree structure where nodes hold lists of subtrees or leaf values
  deriving (Show)

-- Monad bind for tree: recursively applies function to all leaves
bindFTree :: FTree a -> (a -> FTree b) -> FTree b
bindFTree (Leaf x) f = f x
bindFTree (Node xs) f = Node (map (`bindFTree` f) xs)

instance Functor FTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node xs) = Node (map (fmap f) xs)

instance Applicative FTree where
  pure = Leaf
  (<*>) = ap                                                                -- use standard Applicative implementation via Monad

instance Monad FTree where
  (>>=) = bindFTree

-- tester FTree
testFTree :: IO ()
testFTree = do
  let tree = Node [Leaf 1, Leaf 2, Node [Leaf 3]]
      f x = Node [Leaf x, Leaf (x * 2)]                                     -- doubles each leaf
  print (tree >>= f)

-- *****************************************************************************************************

-- PROP MONAD

data Prop a
  = PVar a
  | Const Bool
  | And (Prop a) (Prop a)
  | Or  (Prop a) (Prop a)
  | Not (Prop a)
  | Iff (Prop a) (Prop a)
  deriving (Show)

-- bindProp replaces each PVar using f, rebuilding all other constructors
bindProp :: Prop a -> (a -> Prop b) -> Prop b
bindProp (PVar x)    f = f x
bindProp (Const b)   _ = Const b
bindProp (And p q)   f = And (bindProp p f) (bindProp q f)
bindProp (Or  p q)   f = Or  (bindProp p f) (bindProp q f)
bindProp (Not p)     f = Not (bindProp p f)
bindProp (Iff p q)   f = Iff (bindProp p f) (bindProp q f)

instance Functor Prop where
  fmap = liftM

instance Applicative Prop where
  pure  = PVar
  (<*>) = ap

instance Monad Prop where
  (>>=) = bindProp

-- test Prop
testProp :: IO ()
testProp = do
  let p1 = And (PVar "x") (Or (Const False) (PVar "y"))
      p2 = Iff (PVar "z") (Not (PVar "w"))
      f v = case v of
        "x" -> Const True
        "y" -> And (PVar "y1") (Const True)
        "z" -> Or (PVar "z1") (PVar "z2")
        "w" -> Const False
        _   -> PVar v

  putStrLn "Original p1:"; print p1
  putStrLn "p1 >>= f:" ; print (p1 >>= f)

  putStrLn "\nOriginal p2:"; print p2
  putStrLn "p2 >>= f:" ; print (p2 >>= f)



-- *****************************************************************************************************

-- LAMBDA MONAD
data Lam a = Var a | Abs (Lam a) | App (Lam a) (Lam a)
  deriving (Show)

-- bind applies function to variables, recursing through Abs and App
bindLam :: Lam a -> (a -> Lam b) -> Lam b
bindLam (Var x) f = f x
bindLam (Abs l) f = Abs (bindLam l f)
bindLam (App l1 l2) f = App (bindLam l1 f) (bindLam l2 f)

instance Functor Lam where
  fmap = liftM

instance Applicative Lam where
  pure = Var
  (<*>) = ap

instance Monad Lam where
  (>>=) = bindLam


-- Tester Lambda
testLam :: IO ()
testLam = do
  let lam = App (Var True) (Abs (App (Var False) (Var True)))
      f b = case b of
        True  -> Var "yes"
        False -> Var "no"
  print (lam >>= f)


-- *****************************************************************************************************

-- POLY MONAD
data Poly a = Mono Double [a] | Sum (Poly a) (Poly a)
  deriving (Show)

-- bind replaces each variable with a polynomial, and sums the result
bindPoly :: Poly a -> (a -> Poly b) -> Poly b
bindPoly (Mono c xs) f = foldr (\x acc -> Sum (f x) acc) (Mono c []) xs
bindPoly (Sum p1 p2) f = Sum (bindPoly p1 f) (bindPoly p2 f)

instance Functor Poly where
  fmap = liftM

instance Applicative Poly where
  pure x = Mono 1.0 [x]
  (<*>) = ap

instance Monad Poly where
  (>>=) = bindPoly


-- tester poly
testPoly :: IO ()
testPoly = do
  let p = Sum (Mono 3.0 ["x1"]) (Mono 1.0 ["x2"])
      q "x1" = Mono 2.0 ["y"]
      q "x2" = Sum (Mono 1.0 []) (Mono 3.0 ["z"])
  print (p >>= q)


-- *****************************************************************************************************

-- MAIN FUNCTION :-}
main :: IO ()
main = do
  putStrLn "\n================== Safe =================="
  testSafe

  putStrLn "\n================== FTree =================="
  testFTree

  putStrLn "\n================== Prop =================="
  testProp

  putStrLn "\n================== Lam =================="
  testLam

  putStrLn "\n================== Poly =================="
  testPoly
