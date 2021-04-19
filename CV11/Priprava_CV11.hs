{- Cvicenie 1
data M1 a      = Raise String | Return a  deriving(Show, Read, Eq)
instance Functor M1  where
	fmap  f  (Raise str)     =  Raise str
	fmap  f  (Return x)     =  Return (f x)

1)
fmap id ?=? id
- fmap id (Raise str) = Raise str = id (Raise str)
- fmap id (Return x) = Return (id x) = Return x = id (Return x)

2)
fmap (p.q) ?=? (fmap p) . (fmap q)
fmap ((p.q) (Raise str) = Raise str = ((fmap p) . (fmap q)) (Raise str)

L.S. = fmap ((p.q) (Return x) = Return ((p.q) x) = Return (p (q x))
P.S. (fmap p . fmap q) (Return x) = (fmap p) ((fmap q) (Return x)) = 
      (fmap p) (Return (q x)) =
      (Return (p (q x)) = ... L.S. 
      q.e.d.
-}

data MyMaybe a = MyJust a | MyNothing deriving (Show) -- alias Maybe a
data MyList a     = Null| Cons a (MyList a) deriving (Show)-- alias [a]

instance Functor MyMaybe  where
       fmap f MyNothing    =  MyNothing
       fmap f (MyJust x)   =  MyJust (f x)

myMaybe1 = MyNothing
myMaybe2 = MyJust "hello"
myMaybe3 = MyJust [1,2,3]

       
instance  Functor MyList  where
       fmap f Null = Null
       fmap f (Cons x xs) = Cons (f x) (fmap f xs)     


mylist = (Cons 1 (Cons 2 (Cons 3 Null)))

-------------- Cvicenie 3
       
data LExp a = Var a | APP (LExp a) (LExp a) | ABS a (LExp a) deriving (Show)
instance Functor LExp where
       fmap f (Var x)                   = Var (f x)
       fmap f (APP left right)          = APP (fmap f left) (fmap f right)
       fmap f (ABS x right)             = ABS (f x) (fmap f right)       

omega = ABS "x" (APP (Var "x") (Var "x"))

-- fmap (\x -> x++x) omega


data RoseTree a = Rose a [RoseTree a]       deriving (Show)
instance Functor RoseTree where
       fmap f (Rose a bs)       = Rose (f a) (map (fmap f) bs)

roseTree = Rose "a" [Rose "b" [],Rose "c" [],Rose "d" []]       

--  fmap (\x -> x++x) roseTree


------------------------- Cvičenie 5

1)
pure id <*> v = v

pure id = Return id
(Return id) <*> v = v
fmap id v = v -- pravidlo identity pre Functors
---
2)
L.S.
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
(Return (.)) <*> u <*> v <*> w = 
(fmap (.) u) <*> v <*> w = 
(fmap (.) (Return fu)) <*> v <*> w = 
(Return ((.) fu)) <*> (Return fv) <*> (Return fw) = 
(Return ((.) fu) fv) <*> (Return fw) = 
(Return (fu . fv) <*> (Return fw) = 
(Return ((fu . fv) fw) 
(Return (fu (fv fw))
= 
R.S. 
Return (fu (fv fw))

3)
pure f <*> pure x = (Return f) <*> (Return x) = fmap f (Return x) = Return (f x) = pure (f x)

4) u <*> pure y = (pure $ y) <*> u
(Raise e)
L.S. = (Return f) <*> (Return y) = fmap f (Return y) = (Return (f y))
P.S. = (Return ($ y)) <*> (Return f) = fmap ($ y) (Return f) = Return (($ y) f) = Return (f y)



