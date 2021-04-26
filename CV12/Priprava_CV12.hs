{- VLASTNOSTI MONADY
1) return c  >>=  f               = f c      -- ľavo neutrálny prvok
2) m  >>=  return                 = m        -- pravo neutrálny prvok
3) (m >>= f) >>= g                = m >>= (\x-> (f x >>= g))

ALTERNATIVA
1) return c  >>=  (\x->g) = g[x/c]
2) m  >>=  \x->return x   = m
3) m1 >>= (\x->m2 >>= (\y->m3)) = (m1 >>= (\x->m2)) >>= (\y->m3)
-}
-------------------------------------------------------------------------
{-
Cvicenie1: Identity monad
1) v >=  f = f v
2) m >>= return = (\v -> v) m = m
3) 
L.S. (m >>= f) >>= g = g (f m)
P.S. = m >>= (\x-> f x >>= g) = m >>= (\x-> g (f x)) = (\x-> g (f x)) m = g (f m)
ALTERNATIVA:
L.S. m1 >>= (\x->m2 >>= (\y->m3)) = m1 >>= (\x->m3[y/m2]) = m3[y/m2][x/m1]
P.S. (m1 >>= (\x->m2)) >>= (\y->m3) = m2[x/m1] >>= (\y -> m3) = m3[y/m2[x/m1]]
x sa nachadza len v m2, tak potom ok
-}
-------------------------------------------------------------------------
{-
Cvicenie2: Exception monad

1) Return a >>= f = Return (f a)
2) (Return a) >>= (\v -> Return v) = Return a
   (Raise e) >>= return = (Raise e)
3) L.S. ((Return a) >>= f) >>= Return b = Return (f a) >>= Return b = Return (b (f a))
   P.S. (Return a) >>= (\x-> (f x >>= Return b)) = Return ((\x-> (f x >>= Return b)) a)
        = Return ((f a) >>= Return b) = Return (b (f a))
-}
-------------------------------------------------------------------------
{-
Cvicenie3: State monad
1) SM (\st -> (c,st)) >>= f = SM (\st -> (f c, st))
2) SM (\st -> (M,Mst)) >>= (\v -> SM (\st -> (v,st))) = SM (\st -> (M,Mst))
3) ... du :)

1) return c  >>=  (\x->g) = g[x/c]
    ST(\st -> (c,st)) >== (\x->g) = SM (\st -> (g[x/c], st)
2) m  >>=  \x->return x   = m
    SM (\st -> (M,Mst)) >>= (\x->SM(\st->(x,st)) = SM (\st -> (M,Mst))
3) ... du :)
-}
-------------------------------------------------------------------------
{-
Cvicenie4: sequence pomocou foldr
-}
sequenceUsingFold :: Monad m => [m a] -> m [a]
sequenceUsingFold = foldr (\c cs -> do { x <- c; xs <- cs; return (x:xs) }) (return [])
-------------------------------------------------------------------------

{-
Cvicenie5: List Monad
1) return c  >>=  f               = f c      -- ľavo neutrálny prvok
2) m  >>=  return                 = m        -- pravo neutrálny prvok
3) (m >>= f) >>= g                = m >>= (\x-> (f x >>= g))

1)  [c] >>= f = concat [[f c]] = [f c]
2)  [c1,..,cn] >>= return = concat [[c1],..,[cn]] = [c1,..,cn]
3)  L.S. ([c1,..,cn] >>= (\y->[d1,..,dm)) >>= (\z->[e1,...eq] =
            [ dj[y/ci] ] >>= (\z->[e1,...eq] = 
            [ ek[y/dj[x/ci] ] ]
    P.S. [c1,..,cn] >>= ((\y->[d1,..,dm) >>= (\z->[e1,...eq]) = 
            dtto
-}
-------------------------------------------------------------------------
{-
Cvicenie6: podobne ako Cvicenie2
-}
-------------------------------------------------------------------------
{-
Cvicenie7: 
asoc
p `plus` (q `plus` r) 	= (p `plus` q) `plus` r  
L.S. (Just x) `plus` (Nothing `plus` (Just z)) 	= Just x
P.S. ((Just x) `plus` Nothing) `plus` (Just z) 	= Just x

distrib
(p `plus` q) >>= f 	= (p >>= f) `plus` (q >>= f)
L.S. (Just x `plus` Just y) >>= f = (Just x) >>= Just (f x)
((Just x) >>= f) `plus` (Just y) >>= f = Just (f x) `plus` Just (f y) = Just (f x)
-}
-------------------------------------------------------------------------
{-
Cvičenie 8: Dokážte vlastnoti MonadPlus pre List Monad
vlastnosti zero a `plus`:
zero `plus` p = p       
[] ++ p = p 

p `plus` zero = p
p ++ [] = p

p `plus` (q `plus` r) = (p `plus` q) `plus` r  
asociativita ++

vlastnosti zero `plus` a >>= :
zero >>= f = zero
concat . map f [] = []

p >>= (\x->zero) = zero
concat . map (\x->[]) p = []

(p `plus` q) >>= f = (p >>= f) `plus` (q >>= f)

concat . map f (p ++ q) = 
    concat . map f p 
    ++
    concat . map f q
-}
-------------------------------------------------------------------------
