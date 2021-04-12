module PondelokCV09 where

import Prelude hiding ((<*>), (*>), (<*), sequence)
import Data.Char

import Terms -- pre domacu ulohu 1/8
import Parser

-- CV.1
symbol' a = satisfy (== a)

-- CV.2
digit10 = satisfy isDigit

-- CV.3
hexa = satisfy (\x -> elem x "0123456789ABCDEF")

-- a++b
-- a++(b++c)
--(a++b)++c

-- CV.4 
rep n a = replicate n a
dost = 1000000000
e1 = length $ (rep dost 1) ++ ((rep dost 2) ++ (rep dost 3))  -- right (38.74 secs, 280,000,070,544 bytes)
e2 = length $ ((rep dost 1) ++ (rep dost 2)) ++ (rep dost 3)  -- left (46.26 secs, 336,000,070,344 bytes)

-- lebo zlozitost (++) je umerna dlzke prveho argumentu

-- cv.5
yes = token "YES"
no = token "NO"
yesno = yes <|> no

--no' = symbol 'N' <*> symbol 'O' <*> succeed "NO"
-- yes' = symbol 'Y' <*> symbol 'E' <*> symbol 'S' <*> succeed "YES"

no' = symbol 'N' *> symbol 'O' *> succeed "NO"
yes' = symbol 'Y' *> symbol 'E' *> symbol 'S' *> succeed "YES"
yesno' = yes' <|> no'

yesno'' = just yesno

-- cv.6
just' p xs = [ ([], v) | (ys, v) <- p xs, null ys]

-- cv 7.
infixr 6 <:**>
(<:**>) :: Parser s a -> Parser s [a] -> Parser s [a]
--p <:**> q = p <*> q <@ (\x -> (fst x) : (snd x))
-- p <:**> q = p <*> q <@ (\(x,ys) -> x : ys)
-- p <:**> q = p <*> q <@ uncurry (:)
(p <:**> q ) xs = [ (ys, z:zs) | (xs2, z) <- p xs, (ys, zs) <-q xs2]

--cv 8.
sequence' [] = succeed []
sequence' (p:ps) = p <:*> sequence' ps

--cv 8'.
token' ts = sequence (map symbol ts)

--cv 9.
mobileNumber  :: Parser Char [Char]
mobileNumber = just $ sequence ([symbol '0', symbol '9'] ++ (replicate 8 digit10))

month'' = symbol '0' <*> digit10
        <|>
        symbol '1' <*> (symbol '0' <|> symbol '1' <|> symbol '2')

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
natural' = (greedy1 digit) <@ foldr (\y -> \x -> 10*x+y) 0 . reverse

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
parserV = undefined
                          
justparserV :: Parser Char [Char]
justparserV = just $ parserV
                                          
--------------------
{-- zle riesenie}               
parserS :: Parser Char [Char]
parserS = ( parserS <*> parserS ) <@ (\(x,y) -> (x++y))
          <|>
          succeed []
          
S -> a S | epsilon          
---}

parserS :: Parser Char [Char]
parserS = undefined

          
justparserS :: Parser Char [Char]
justparserS = just $ parserS

--------------------
                
parserR :: Parser Char [Char]
parserR =   (symbol 'a' <*> parserR <*> symbol 'a'
            <|>
            symbol 'b' <*> parserR <*> symbol 'b'
            <|>
            symbol 'c' <*> parserR <*> symbol 'c') <@ (\(x,(y,z)) -> x:(y++[z]))
            <|>
            succeed []
            
-- pocet(a) w = pocet(b) w            
                          
justparserR :: Parser Char [Char]
justparserR = just $ parserR
                          
----------------------------
                          
parserU :: Parser Char [Char]                          

parserU =  ( symbol 'a' <*> parserU <*>  symbol 'b' <*>  parserU) <@ (\(a, (bs, (c, ds))) -> (a:bs)++(c:ds))
            <|> 
           ( symbol 'b' <*> parserU <*>  symbol 'a' <*>  parserU) <@ (\(a, (bs, (c, ds))) -> (a:bs)++(c:ds))
           <|> 
           succeed []


{-
parserU =  sequence [symbol 'a', parserU, symbol 'b', parserU] 
            <|> 
           sequence [symbol 'b', parserU, symbol 'a', parserU]
           <|> 
           succeed []
-}

           
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
