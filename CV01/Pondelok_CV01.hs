import Data.Bits as DB

import Data.List

import Test.QuickCheck

-- v zalozke Shell urob:
-- cabal update
-- cabal install QuickCheck

minInt = minBound :: Int
maxInt = maxBound :: Int

f1 :: Int -> Integer
f1 n = toInteger n

f2 :: Integer -> Int
f2 n =  fromIntegral n :: Int

fib :: Int -> Int
fib n = if n > 1 then 
           fib (n-1) + fib (n-2)
        else 
           1

fact :: Integer -> Integer
fact n = if n == 1 then 1 else n * fact (n-1)


-- https://www.willamette.edu/~fruehr/haskell/evolution.html

pocetCifier :: Integer -> Integer
pocetCifier n | n < 10 = 1
              | otherwise = 1+pocetCifier (n `div` 10)

pocetNul :: Integer -> Integer
pocetNul n | n == 0 = 1
           | n < 10 = 0
           | otherwise = (if n `mod` 10 == 0 then 1 else 0) +pocetNul (n `div` 10)

pocetKoncovychNul :: Integer -> Integer
pocetKoncovychNul n | n == 0 = 1
                    | n < 10 = 0
                    | otherwise = if n `mod` 10 == 0 then 1 + pocetKoncovychNul (n `div` 10)
                                  else 0

pocetCifier' :: Integer -> Integer
pocetCifier' n = ceiling $ (logBase 10 (fromIntegral n))

-- log (a*b) = log a + log b
-- log (1*2*3*4*5) = sum log i

pocetCifierFact :: Int -> Int
pocetCifierFact n = ceiling $ sum $ map (logBase 10) [1.. fromIntegral n]

-- pocet 5 v p.rozklade n
log5 :: Int -> Int
--log5 n = length $ takeWhile ((==0).(`mod` 5)) $ iterate (`div` 5) n
log5 n = length $ takeWhile (\x -> (x `mod` 5 == 0)) $ iterate (`div` 5) n


-- 1*2*3*4*5*6*7*8*9*
koncoveNulyFact :: Int -> Int
koncoveNulyFact n = sum $ map log5 [1..n]

moznosti :: Int -> [(Int, Int, Int)]
-- moznosti n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x*y*z == n ]
-- moznosti n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x<=y, y<=z,x*y*z == n ]
--moznosti n = [ (x,y,z) | x<-[1..n], y<-[x..n], let z = n `div` (x*y), y<=z, x*y*z == n ]

moznosti n = [ (x,y,z) | x<-delitele n, y<-delitele (n `div` x), x<=y, let z = n `div` (x*y), y<=z, x*y*z == n ]

delitele :: Int -> [Int]
delitele n = [ d | d<-[1..n], n `mod` d == 0]

pocetMoznosti :: Int -> Int
pocetMoznosti n = length (moznosti n)
