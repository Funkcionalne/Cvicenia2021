import Data.List


zips :: [Int] -> [Int] -> [Int]
zips (x:xs) (y:ys) = x : y : zips xs ys
zips xs ys = []

zips3 :: [Int] -> [Int] -> [Int] -> [Int]
zips3 (x:xs) (y:ys) (z:zs) = x : y : z: (zips3 xs ys zs)
zips3 xs ys zs = []

zipsN :: [[Int]] -> [Int]
zipsN xss | all (>0) (map length xss) = (map head xss) ++ (zipsN (map tail xss))
          | otherwise = []

unzips :: [Int] -> ([Int], [Int])
unzips (x1:x2:xs) = (x1:res1 , x2:res2)
                    where (res1, res2) = unzips xs
unzips xs = ([], [])

unzips3 :: [Int] -> ([Int], [Int], [Int])
unzips3 (x1:x2:x3:xs) = (x1:res1 , x2:res2, x3:res3)
                    where (res1, res2, res3) = unzips3 xs
unzips3 xs = ([], [], [])

unzipsN :: [Int] -> Int -> [[Int]]
unzipsN xs n | n <= length xs = zipWith (++) [[x] | x <- (take n xs)] (unzipsN (drop n xs) n)
             | otherwise      = take n $ repeat []




myReverse :: [t] -> [t]
myReverse xs = foldl (\acc x -> x:acc) [] xs 

nub' :: [Int] -> [Int]
nub' xs = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] xs

duplikuj :: [Int] -> [Int]
duplikuj xs = foldr (\x acc -> x:x:acc) [] xs

dropEvery :: [Int] -> Int -> [Int]
dropEvery xs n = foldr (\(i, x) acc -> if i `mod` n == 0 then acc else x:acc) [] $ zip [1..] xs

zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = foldr (\i acc -> (xs!!i, ys!!i):acc) [] [0..len-1]
          where len = minimum [(length xs), (length ys)]