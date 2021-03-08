module Pondelok_CV05 where

{- cvicenie chce
- predstavit Terms (lambda termy)
- zakladna rekurzia cez lambda term, prip. foldLambda
- show, read (vystup a vstup lambda termu)
- Maybe typ
- do notaciu
- Map alias dictionary
-}

import Terms
--- some useful stuff
import Data.List 
import Data.Char
import Data.Map (Map, insert, lookup, empty)
import Data.Maybe

-- defiujte instance Show LExp
-- instance Show LExp where
  
-- zopar pouzitelnych konstant

izero = (LAMBDA "f" (LAMBDA "x" (ID "x")))
omega = (LAMBDA "x" (APP (ID "x") (ID "x")))
isucc = (LAMBDA "n" 
          (LAMBDA "f" 
            (LAMBDA "x" (APP (ID "f") (APP (APP (ID "n") (ID "f")) (ID "x"))) )))
            
-- kominatory S,K,I            
i = (LAMBDA "x" (ID "x"))            
k = (LAMBDA "x" (LAMBDA "y" (ID "x")))            
s = (LAMBDA "x" (LAMBDA "y" (LAMBDA "z" (APP (APP (ID "x") (ID "z")) (APP (ID "y") (ID "z"))))))
            
-- zopar z nich odvodenych konstant            
ione =    (APP isucc izero)
itwo =    (APP isucc (APP isucc izero))
ifour =   (APP isucc (APP isucc (APP isucc (APP isucc izero))))
ieight =  (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc izero))))))))
--ithree =  (APP (APP iplus itwo) ione)
--inine =   (APP (APP itimes ithree) ithree)
--isixteen = (APP (APP ipower itwo) ifour)

----------------------------- jednoduchy stromove preliezky
-- zoznam vsetkych premennych
vars  :: LExp -> [Var]
vars = undefined


-- vars isucc = ["n","f","x","f","n","f","x"]
-- nub $ vars isucc = ["n","f","x"]

-- obsahuje beta redex, miesto, kde sa da aplikovat beta redukcia
hasRedex :: LExp -> Bool
hasRedex = undefined

-- hasRedex  i == False
-- hasRedex  k == False
-- hasRedex  s == False
-- hasRedex  (APP s k) == True
-- hasRedex  (APP (APP s k) k) == False

------------------------------ abstrakcia nad stromovou rekurziou
-- vseobecny lambda traversal pattern

foldLambda :: (Var -> t -> t) -> (Var -> t) -> (t -> t -> t) -> LExp -> t
foldLambda lambda var apl (LAMBDA str exp)  = lambda str (foldLambda lambda var apl exp)
foldLambda lambda var apl (ID str)          = var str
foldLambda lambda var apl (APP exp1 exp2)   = apl (foldLambda lambda var apl exp1) 
                                                  (foldLambda lambda var apl exp2)
                                                  
vars'  :: LExp -> [Var]                                                  
vars' = undefined

show' :: LExp -> String
show' = undefined

hasRedex' :: LExp -> Bool
hasRedex' = undefined

-- hasRedex'  i == False
-- hasRedex'  k == False
-- hasRedex'  s == False
-- hasRedex'  (APP s k) == False   -whoops 
-- hasRedex'  (APP (APP s k) k) == False  -whoops 
--------------------------------- chcelo by to parser lambda termov, aspon primitivny

fromString  :: Var -> (LExp, Var)
fromString (x:xs) = undefined

{- vieme citat po sebe
fromString $ show izero     (\f->\x->x,"")
fromString $ show omega     (\x->(x x),"")                    
fromString $ show isucc     (\n->\f->\x->(f ((n f) x)),"")
-}


-- pozor, readsPrec ocakava nedeterministicky parser, takze ten nas musime dokalicit na zoznam, alebo ten nas prepisat aby vracal zoznam [(LExp, Var)]

-- definujme instance Read pre LExp
-- instance Read LExp where

-- read "(\n->\f->\x->(f ((n f) x))"

-- (read "\\x->x")::LExp  = \x->x
-- (read "\\f->\\x->x")::LExp = \f->\x->x
-- (read "\\x->(x x)")::LExp = \x->(x x)
-- (read "\\n->\\f->\\x->(f ((n f) x))")::LExp = \n->\f->\x->(f ((n f) x))

----------------------------------------------- funkcia Eq

-- najst vsetky podtermy termu priamociaro
podtermy :: LExp -> [LExp]
podtermy =  undefined


-- podtermy (LAMBDA "x" (APP (ID "x") (ID "x"))) = [\x->(x x),(x x),x]

-- toto vlastne robi deriving class Eq
rovnake :: LExp -> LExp -> Bool
rovnake = undefined

--------------------------------------------
-- type Maybe t = Just t | Nothing
maxim :: Ord t => [t] -> Maybe t
maxim xs = undefined
minim :: Ord t => [t] -> Maybe t
minim xs = undefined

rozdielMaxMin :: (Num t, Ord t) => [t] -> Maybe t
rozdielMaxMin xs = undefined

-- rozdielMaxMin [5,3,1,8,6,4,2] == Just 7
-- rozdielMaxMin [] == Nothing

-- Maybe Monad style
rozdielMaxMin' :: (Num t, Ord t) => [t] -> Maybe t
rozdielMaxMin' xs = undefined

-- rozdielMaxMin' [5,3,1,8,6,4,2] == Just 7
-- rozdielMaxMin' [] == Nothing


------------------------------------------------------------

-- podobne zisti, ci dva termy su strukturalne rovnake, len sa lisia v menach premennych
-- ak su strukturalne rovnake, vrati zoznam Just dvojic premennych na zodpovedajucich miestach, ak su rozne
-- ak nie su strukturalne rovnake, tak vrati Nothing
-- treba sa zamysliet, preco potrebujeme odlisit Just [] od Nothing
-- lebo (ID "x") (ID "x") bude Just [], (ID "x") (ID "y") bude Just [("x","y")]
-- ale (ID "x") (LAMBDA "x" (ID "x")) bude Nothing
podobne :: LExp -> LExp -> Maybe [(Var, Var)]
podobne (LAMBDA v1 e1) (LAMBDA v2 e2) = 
               undefined
podobne (ID v1) (ID v2) = undefined
podobne (APP e11 e21) (APP e12 e22) = 
                undefined
podobne _ _ = Nothing

-- podobne (ID "x") (ID "x") == Just []
-- podobne (ID "x") (ID "y") == Just [("x","y")]
-- podobne (ID "x") (LAMBDA "x" (ID "x")) == Nothing


-- let's try maybe monad by example

podobne' :: LExp -> LExp -> Maybe [(Var, Var)]
podobne' (LAMBDA v1 e1) (LAMBDA v2 e2) = 
                undefined
podobne' (ID v1) (ID v2) = undefined

-- Tu netreba nic menit, krasne to ilustruje eleganciu do notacie. Ale napada mi otazka, malo by zmysel povedat im aj o Applicative pocas kurzu, alebo je to too much? (Mozno si len zle pamatam, ale mari sa mi, ze v slidoch som to nevidel)
-- dovod: Tento zapis je velmi pekny, ale oproti zapisu "(++) <$> podobne' e11 e12 <*> podobne' e12 e22" je na tom horsie vykonovo (ale zanedbatelne, len ceresnicka). Vacsia vyhoda ale podla mna je, ze sa takato operacia da robit aj na niecom, co nemoze byt Monada. Napriklad https://hackage.haskell.org/package/validation
-- Mozno by bolo aj zaujimave im uz teraz z rychlika ukazat ako vyzera desugaring tej do notacie ako tu v podobne'', nech vidia za co mozu byt vdacni :) (sry ak tam mam chybu, pisem to bez kontroly):

-- podobne'' (App e11 e21) (APP e12 e22) = podobne' e11 e12 >>= (\e1 -> podobne e12 e22 >>= (e2 -> Just (e1 ++ e2)))

podobne' (APP e11 e21) (APP e12 e22) = 
                undefined
podobne' _ _ = Nothing

-- podobne' (ID "x") (ID "x") == Just []
-- podobne' (ID "x") (ID "y") == Just [("x","y")]
-- podobne' (ID "x") (LAMBDA "x" (ID "x")) == Nothing


-- input-output tree traverse style

--type Substitution = Map Var LExp  -- casom bude aj toto...

type Substitution = Map Var Var
podobne'' :: LExp -> LExp -> Substitution -> Maybe Substitution
podobne'' (LAMBDA v1 e1) (LAMBDA v2 e2) subst = 
                undefined
podobne'' (ID v1) (ID v2) subst = 
                undefined
podobne'' (APP e11 e21) (APP e12 e22) subst = 
                undefined
podobne'' _ _ _ = Nothing

-- podobne'' (ID "x") (ID "x") empty == Just (fromList [])
-- podobne'' (ID "x") (ID "y") empty == Just (fromList [("x","y")])
-- podobne'' (ID "x") (LAMBDA "x" (ID "x")) empty == Nothing


-- nahradi premennu za premennu - pozor, toto nie je este substitucia
nahrad ::  LExp -> Var -> Var -> LExp
nahrad (ID x) y z = if x == y then ID z else ID x
nahrad (APP e1 e2) y z =  (APP (nahrad e1 y z) (nahrad e2 y z))
nahrad t@(LAMBDA x e) y z =  if x == y then 
                                t 
                             else 
                                (LAMBDA x (nahrad e y z))
