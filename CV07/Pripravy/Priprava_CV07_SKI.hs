module Priprava_CV07 where

import Terms
import TermsSKI
--- some useful stuff
import Data.List 
import Data.Char
import Data.Map (Map, insert, lookup, empty, size)
import Data.Maybe

------------------------------------
    
free :: LExpSki -> [Var]
free (Id x) = [x]
free (App x1 x2) = free x1 ++ free x2
free (Lambda x1 x2) = delete x1 (nub(free x2))
free x = []


-- Riesenie Dominiky M. 2020
--vstupný λ-výraz skonvertuje na zodpovedajúci SKI-výraz
toSki :: LExp -> Ski
toSki = lexpSkiToSki . convertSki . lexpToLExpSki
        where
        lexpToLExpSki :: LExp -> LExpSki
        lexpToLExpSki (LAMBDA x y) = Lambda x (lexpToLExpSki y)
        lexpToLExpSki (ID x) = Id x
        lexpToLExpSki (APP x y) = App (lexpToLExpSki x) (lexpToLExpSki y)
        --
        lexpSkiToSki :: LExpSki -> Ski
        lexpSkiToSki (Ss) = S
        lexpSkiToSki (Kk) = K
        lexpSkiToSki (Ii) = I
        lexpSkiToSki (App x y) = APL (lexpSkiToSki x) (lexpSkiToSki y)
        --
        convertSki ::  LExpSki -> LExpSki
        convertSki (Lambda x a@(Lambda y1 y2)) = convertSki (Lambda x (convertSki a))
        convertSki l@(Lambda x a@(App y1 y2)) | x `notElem` free a = App Kk a
                                              | otherwise = App (App Ss (convertSki (Lambda x y1))) (convertSki (Lambda x y2))
        convertSki (Lambda x (Id y)) | x == y = Ii
                                     | otherwise = App Kk (Id y)
        convertSki (Lambda x Kk) = App Kk Kk
        convertSki (Lambda x Ss) = App Kk Ss
        convertSki (Lambda x Ii) = App Kk Ii

--vstupný SKI-výraz skonvertuje na λ-výraz
fromSki  ::  Ski -> LExp
fromSki  = lexpSkiToLExp . convertLExp . skiToLExpSki
        where
        lexpSkiToLExp :: LExpSki -> LExp
        lexpSkiToLExp (Lambda x y) = LAMBDA x (lexpSkiToLExp y)
        lexpSkiToLExp (Id x) = ID x
        lexpSkiToLExp (App x y) = APP (lexpSkiToLExp x) (lexpSkiToLExp y)
        --
        skiToLExpSki :: Ski -> LExpSki
        skiToLExpSki (S) = Ss
        skiToLExpSki (K) = Kk
        skiToLExpSki (I) = Ii
        skiToLExpSki (APL x y) = App (skiToLExpSki x) (skiToLExpSki y)
        --
        convertLExp :: LExpSki -> LExpSki
        convertLExp (App m n) = (App (convertLExp m) (convertLExp n))
        convertLExp Ii = (Lambda "x" (Id "x"))  
        convertLExp Kk = (Lambda "x" (Lambda "y" (Id "x")))         
        convertLExp Ss = (Lambda "x" (Lambda "y" (Lambda "z" (App (App (Id "x") (Id "z")) (App (Id "y") (Id "z"))))))

--vstupný SKI-výraz zredukuje
nf :: Ski -> Ski
nf l = if a == l then a else nf a where a = oneStep l

oneStep ::  Ski -> Ski
oneStep (APL (APL S K) K) = I
oneStep (APL (APL S K) S) = I
oneStep (APL I x) = x
oneStep (APL (APL K x) y) = x
oneStep (APL (APL S x) (APL y z)) = APL (APL x z) (APL y z)
oneStep (APL (APL (APL S x) y) z) = APL (APL x z) (APL y z)
oneStep (APL x y) = APL (oneStep x) (oneStep y)
oneStep x = x

--príklady na vyskúšanie
--1.uloha
a = LAMBDA "x" (LAMBDA "y" (APP (ID "y") (ID "x")))
b = LAMBDA "x" (LAMBDA "y" (APP (ID "x") (ID "y")))
c = LAMBDA "x" (LAMBDA "y" (LAMBDA "z" (APP (APP (ID "x") (ID "y")) (ID "z"))))

--2.uloha
r = APL(APL S K) (APL S K)
s = APL (APL (APL (APL (APL (APL (APL (APL K K) S) K) S) K) K) S) K

du1 = APL (APL (APL (APL (APL (APL (APL S S) K) K) S) K) S) K
du2 = APL (APL (APL (APL (APL (APL (APL (APL (APL S S) K) K) S) K) S) K) S) K

