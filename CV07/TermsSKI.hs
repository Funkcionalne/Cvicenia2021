module TermsSKI where

data Ski = S | K | I | APL Ski Ski  deriving(Eq) 

instance Show Ski where
    show (S) = "S"
    show (K) = "K"
    show (I) = "I"
    show (APL x y) = "(" ++ show x ++ " " ++ show y ++ ")"

-- medzi forma
data LExpSki = Lambda String LExpSki | Id String | App LExpSki LExpSki | Ss | Kk | Ii
                deriving(Eq)                
    
instance Show LExpSki where
    show (Id x) = x
    show (App x1 x2) = "(" ++ show x1 ++ ") (" ++ show x2 ++ ")"
    show (Lambda x1 x2) = "\\" ++ x1 ++ ".(" ++ show x2 ++ ")"
    show (Ss) = "S"
    show (Kk) = "K"
    show (Ii) = "I"                