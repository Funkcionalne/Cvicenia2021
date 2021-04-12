module PondelokCV09 where

import Prelude hiding ((<*>), (*>), (<*), sequence)
import Data.Char

import Terms -- pre domacu ulohu 1/8
import Parser

-- CV.1
symbol' a = undefined

-- CV.2
digit10 = undefined

-- CV.3
hexa = undefined

-- CV.4 
rep n a = replicate n a
dost = 1000000000
e1 = length $ (rep dost 1) ++ ((rep dost 2) ++ (rep dost 3))  -- right (38.74 secs, 280,000,070,544 bytes)
e2 = length $ ((rep dost 1) ++ (rep dost 2)) ++ (rep dost 3)  -- left (46.26 secs, 336,000,070,344 bytes)

-- lebo zlozitost (++) je umerna dlzke prveho argumentu

-- cv.5
yes = undefined
no = undefined
yesno = undefined

--no' = symbol 'N' <*> symbol 'O' <*> succeed "NO"
-- yes' = symbol 'Y' <*> symbol 'E' <*> symbol 'S' <*> succeed "YES"

no' = symbol 'N' *> symbol 'O' *> succeed "NO"
yes' = symbol 'Y' *> symbol 'E' *> symbol 'S' *> succeed "YES"
yesno' = yes' <|> no'

yesno'' = just yesno

-- cv.6
just' p xs = undefined

-- cv 7.
infixr 6 <:**>
(<:**>) :: Parser s a -> Parser s [a] -> Parser s [a]
p <:**> q = undefined

--cv 8.
sequence' (x:xs) = undefined

--cv 8'.
token' ts = undefined

--cv 9.
mobileNumber  :: Parser Char [Char]
mobileNumber = undefined

psc  :: Parser Char [Char]
psc = undefined

-- cv. 10
year  :: Parser Char [Char]
year = sequence' (replicate 4 digit10)

month  :: Parser Char (Char,Char)
month = symbol '0' <*> digit10
        <|>
        symbol '1' <*> (symbol '0' <|> symbol '1' <|> symbol '2')
        
month'  :: Parser Char [Char]
month' = sequence' [ symbol '0', digit10]
         <|>
         sequence' [symbol '1', (symbol '0' <|> symbol '1' <|> symbol '2') ]        

day  :: Parser Char (Char,Char)
day = symbol '0' <*> digit10
        <|>
        symbol '1' <*> digit10
        <|>
        symbol '2' <*> digit10
        <|>
        symbol '3' <*> (symbol '0' <|> symbol '1')
        
day'  :: Parser Char [Char]
day' = sequence' [ (symbol '0' <|> symbol '1' <|> symbol '2'), digit10]
        <|>
         sequence' [symbol '3', (symbol '0' <|> symbol '1') ]
         
day''  :: Parser Char [Char]
day'' = sequence' [ choice [symbol '0' , symbol '1' , symbol '2' ], digit10]
        <|>
         sequence' [symbol '3', (symbol '0' <|> symbol '1') ]         

-- cv. 11         
natural'  :: Parser Char Int
natural' = undefined

-- cv. 12
-- desatinna cast
fract :: Parser Char Float         
fract = many digit <@ foldr f (0.0)
        where f acc x = (x + fromIntegral acc)/10.0

-- cv. 13 
fixed :: Parser Char Float
fixed = (natural <@ fromIntegral)
        <*>
        (option (symbol '.' *> fract) <?@ (0.0, id))
        <@ uncurry (+)
        
-- (just fixed) "3.14"        


         
         

parserV :: Parser Char [Char]
parserV = ( symbol 'a' *> parserV <* symbol 'a' ) <@ (\x -> ('a':(x ++ "a")))
          <|>
          succeed []
                          
justparserV :: Parser Char [Char]
justparserV = just $ parserV
                                          
--------------------
{-- zle riesenie}               
parserS :: Parser Char [Char]
parserS = ( parserS <*> parserS ) <@ (\(x,y) -> (x++y))
          <|>
          succeed []
---}

parserS :: Parser Char [Char]
parserS = ( symbol 'a' <*> parserS ) <@ (\(x,y) -> (x : y))
          <|>
          succeed []

          
justparserS :: Parser Char [Char]
justparserS = just $ parserS

--------------------
                
parserR :: Parser Char [Char]
parserR = (
            symbol 'a' <*> parserR <*> symbol 'a'
            <|>
            symbol 'b' <*> parserR <*> symbol 'b'
            <|>
            symbol 'c' <*> parserR <*> symbol 'c'
        ) <@ (\(x, (y,z)) -> (x:(y ++ [z])))
          <|>
          succeed []
                          
justparserR :: Parser Char [Char]
justparserR = just $ parserR
                          
----------------------------
                          
parserU :: Parser Char [Char]                          
{-
parserU =  sequence [symbol 'a', parserU, symbol 'b', parserU] 
            <|> 
           sequence [symbol 'b', parserU, symbol 'a', parserU]
           <|> 
           succeed []
-}

parserU =  ( symbol 'a' <*> parserU <*>  symbol 'b' <*>  parserU) <@ (\(a, (bs, (c, ds))) -> (a:bs)++(c:ds))
            <|> 
           ( symbol 'b' <*> parserU <*>  symbol 'a' <*>  parserU) <@ (\(a, (bs, (c, ds))) -> (a:bs)++(c:ds))
           <|> 
           succeed []
           
{-
(just parserU) "aabb"
[("","aabb")]
(0.00 secs, 84,320 bytes)
(just parserU) "aaabb"
[]
(0.00 secs, 77,400 bytes)
(just parserU) "aaabbb"
[("","aaabbb")]
(0.01 secs, 93,520 bytes)
(just parserU) "aababb"
[("","aababb"),("","aababb")]
(0.01 secs, 118,216 bytes)
(just parserU) "ababab"
[("","ababab"),("","ababab"),("","ababab"),("","ababab"),("","ababab")]
(0.01 secs, 184,392 bytes)

-}           
