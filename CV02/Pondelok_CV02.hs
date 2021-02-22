module Pondelok_CV02 where
import Test.QuickCheck
--import Text.Show.Functions
import Data.List

-- cabal update
-- cabal install QuickChech

-- pocet 3906
-- slova dlzky 3 nad abecedou
-- slova3 :: [Char] -> [String]
slova3 abeceda = [ [a,b,c] | a<-abeceda, b<-abeceda, c<-abeceda]


-- slova dlzky k nad abecedou
-- slova :: [Char] -> Int -> [[Char]]
-- type String = [Char]
slova :: String -> Int -> [String]
slova abeceda 0 = [ "" ]
slova abeceda n = [ w ++ [ch] | w <- slova abeceda (n-1), ch <- abeceda ]

pocetSlova :: [Char] -> Int -> Int 
pocetSlova abeceda n = (length abeceda) ^ n

qchSlova = quickCheck(\abeceda -> \n -> (length abeceda <= 10 && n >= 0 && n <= 10) ==>
                length (slova abeceda n) == pocetSlova abeceda n)

-- slova dlzky najviac k nad abecedou
slovaNajviac :: [Char] -> Int -> [String]
slovaNajviac abeceda 0 = [ "" ]
slovaNajviac abeceda n = slovaNajviac abeceda (n-1)
                         ++
                         [ ch:w | w <- slovaNajviac abeceda (n-1), ch <- abeceda ]

-- verzia Jozo:
slovaNajviac'' abeceda 0 = [ "" ]
slovaNajviac'' abeceda n = slovaNajviac'' abeceda (n-1)
                         ++
                         [ ch:w | w <- slova abeceda (n-1), ch <- abeceda ]


-- nub

{- length abeceda = q
1 + q + q^2 + ..... q^n  = Sn
    q + q^2 + q^3 + ..... q^(n+1)  = q*Sn
-------------------------------------------    
                      +q^(n+1)-1 = Sn(q-1)

-}
pocetSlovaNajviac [] n =  1
pocetSlovaNajviac [_] n =  n+1
pocetSlovaNajviac abeceda n =  let q = length abeceda in (q^(n+1)-1) `div` (q-1)

qchNajviac = quickCheck(\abeceda -> \n -> (length abeceda <= 6 && n >= 0 && n <= 10) ==>
                length (nub $ slovaNajviac abeceda n) == pocetSlovaNajviac abeceda n)


slovaNajviac' abeceda n =  concat [ slova abeceda k | k <- [0..n]  ]

qchNajviac' = quickCheck(\abeceda -> \n -> (length abeceda <= 6 && n >= 0 && n <= 10) ==>
                length (slovaNajviac' abeceda n) == pocetSlovaNajviac abeceda n)

qchNajviac'' = quickCheck(\abeceda -> \n -> (length abeceda <= 6 && n >= 0 && n <= 10) ==>
                length (slovaNajviac'' abeceda n) == pocetSlovaNajviac abeceda n)


-- slova nad abecedou "ab", ktore neobsahuju aa, teda dve acka za sebou
-- ababbaa
slovaBezAA :: Int -> [String]
slovaBezAA 0 = [ "" ]
slovaBezAA 1 = [ "a", "b" ]
slovaBezAA n = [  'a':'b':w | w <- slovaBezAA (n-2)] 
               ++
               [ 'b':v | v <-slovaBezAA (n-1) ]
               
-- ab................. (n-2)
-- b.................. (n-1)

qchSlovaBezAA = quickCheck(\n -> (n >= 0 && n <= 10) ==>
                           length(slovaBezAA n) == fibonacci n)


-- nemaju substring "aa"
slovaBezAAFilter n = filter (\w -> not(isInfixOf "aa" w)) $ slova "ab" n

qchSlovaBezAA' = quickCheck(\n -> (n >= 0 && n <= 10) ==>
                           length(slovaBezAAFilter n) == fibonacci n)



fibonacci 0 = 1
fibonacci 1 = 2
fibonacci k = fibonacci (k-1) + fibonacci (k-2)

fibonacci' abeceda 0 = 1
fibonacci' abeceda 1 = length abeceda
fibonacci' abeceda k = (length abeceda-1) * (fibonacci' abeceda (k-1) + fibonacci' abeceda (k-2))
    
-- slova neobsahujce "aa"
slovaBezAA' :: String -> Int -> [String]
------------------slovaBezAA' abeceda n | length abeceda < n = [ "" ]
slovaBezAA' _ 0 = [ "" ]
slovaBezAA' abeceda 1 = [ [ch] | ch <- abeceda ]
slovaBezAA' abeceda n = [  'a':ch:w | w <- slovaBezAA' abeceda (n-2), ch <- abeceda, ch /= 'a'] 
                        ++
                        [ ch:v | v <-slovaBezAA' abeceda (n-1), ch <- abeceda, ch /= 'a' ]

qchSlovaBezAA'' = quickCheck(\abeceda -> \n -> 
                            (length abeceda <= 6 && 
                             length abeceda >= n && 
                             length (nub abeceda) == length abeceda &&
                             n >= 0 && n <= 10) ==>
                           length(slovaBezAA' ('a':abeceda) n) == fibonacci' ('a':abeceda) n)

{- ako vybehnut na skody vysky n, ked mozem robit kroky dlzky 1,2
n = 4 => 5
0|1,2,3|4      "bbb"
0|1,2, |4      "bba"
0|1,  3|4      "bab"
0|  2,3|4      "abb"
0|  2, |4      "aba"

n = 5 => 8
0|1,2,3,4|5
0|1,2,3, |5
0|1,2,  4|5
0|1,  3,4|5
0|1,  3, |5
0|  2,3,4|5
0|  2,3, |5
0|  2,  4|5
0|  2,3,4|5

n = 6 => 13....
-}

izoAA2Schody :: String -> [Int]
izoAA2Schody w = [ index | (ch, index) <- zip w [1..], ch == 'b' ] 

izoSchody2AA :: Int ->[Int] -> String
izoSchody2AA n sch = [  if elem index sch then 'b' else 'a' | index <- [1..n] ]


