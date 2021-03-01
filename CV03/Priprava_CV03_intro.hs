-- 5x5
tab = ["ABCDE",
       "FGHIJ",
       "KLMNO",
       "PQRST",
       "UVWXY"
    ] 
-- nájdite všetky slová začínajúce A, končiace Y, ak môžeme ísť leb vpravo a dole
-- koľko ich je
slova :: Int -> Int -> [String]
slova i j | i > 4 || j > 4   = []
          | i == 4 && j == 4 = ["Y"]
          | otherwise        = map (tab!!i!!j:) (slova (i+1) j ++ slova i (j+1))

-- length $ slova 0 0
-- comb(8,4)

slova'' :: Int -> Int -> [String]
slova'' i j | i > 4 || j > 4 = []
          | i == 4 && j == 4 = ["Y"]
          | otherwise =       [ tab!!i!!j:w | w <- slova'' (i+1) j ++ slova'' i (j+1)]

-- length $ slova 0 0
-- comb(8,4)

slova' :: Int -> Int -> [String]
slova' 4 4 = ["Y"]
slova' i j = if (i <= 4 && j <= 4) then
                map (tab!!i!!j:) (slova' (i+1) j ++ slova' i (j+1))
            else []    

zlozitost :: Int -> Int -> Int
zlozitost 4 4 = 1
zlozitost i j = if (i <= 4 && j <= 4) then
                    (zlozitost (i+1) j + zlozitost i (j+1))
            else 0

-- slova, ktore neobashuju Q

{-
tab = ["ABCDE",
       "FGHIJ",
       "KLMNO",
       "PQRST",
       "UVWXY"
       
next "ABCDE" [[""], [], [], [], []]

next "FGHIJ" [["A"],["BA"],["CBA"],["DCBA"],["EDCBA"]]
next "KLMPO" [["FA"],["GFA","GBA"],["HGFA","HGBA","HCBA"],["IHGFA","IHGBA","IHCBA","IDCBA"],["JIHGFA","JIHGBA","JIHCBA","JIDCBA","JEDCBA"]]

[["A"],["BA"],["CBA"],["DCBA"],["EDCBA"]]
[["FA"],["GFA","GBA"],["HGFA","HGBA","HCBA"],["IHGFA","IHGBA","IHCBA","IDCBA"],["JIHGFA","JIHGBA","JIHCBA","JIDCBA","JEDCBA"]]

next "KLMNO" [["FA"],["GFA","GBA"],["HGFA","HGBA","HCBA"],["IHGFA","IHGBA","IHCBA","IDCBA"],["JIHGFA","JIHGBA","JIHCBA","JIDCBA","JEDCBA"]]

next "PQRST" [["KFA"],["LKFA","LGFA","LGBA"],["MLKFA","MLGFA","MLGBA","MHGFA","MHGBA","MHCBA"],["NMLKFA","NMLGFA","NMLGBA","NMHGFA","NMHGBA","NMHCBA","NIHGFA","NIHGBA","NIHCBA","NIDCBA"],["ONMLKFA","ONMLGFA","ONMLGBA","ONMHGFA","ONMHGBA","ONMHCBA","ONIHGFA","ONIHGBA","ONIHCBA","ONIDCBA","OJIHGFA","OJIHGBA","OJIHCBA","OJIDCBA","OJEDCBA"]]

[["PKFA"],["QPKFA","QLKFA","QLGFA","QLGBA"],["RQPKFA","RQLKFA","RQLGFA","RQLGBA","RMLKFA","RMLGFA","RMLGBA","RMHGFA","RMHGBA","RMHCBA"],["SRQPKFA","SRQLKFA","SRQLGFA","SRQLGBA","SRMLKFA","SRMLGFA","SRMLGBA","SRMHGFA","SRMHGBA","SRMHCBA","SNMLKFA","SNMLGFA","SNMLGBA","SNMHGFA","SNMHGBA","SNMHCBA","SNIHGFA","SNIHGBA","SNIHCBA","SNIDCBA"],["TSRQPKFA","TSRQLKFA","TSRQLGFA","TSRQLGBA","TSRMLKFA","TSRMLGFA","TSRMLGBA","TSRMHGFA","TSRMHGBA","TSRMHCBA","TSNMLKFA","TSNMLGFA","TSNMLGBA","TSNMHGFA","TSNMHGBA","TSNMHCBA","TSNIHGFA","TSNIHGBA","TSNIHCBA","TSNIDCBA","TONMLKFA","TONMLGFA","TONMLGBA","TONMHGFA","TONMHGBA","TONMHCBA","TONIHGFA","TONIHGBA","TONIHCBA","TONIDCBA","TOJIHGFA","TOJIHGBA","TOJIHCBA","TOJIDCBA","TOJEDCBA"]]

next "ABCDE" [[""], [], [], [], []]
[["A"],["BA"],["CBA"],["DCBA"],["EDCBA"]]
r1 = next "ABCDE" [[""], [], [], [], []]
r2 = next "FGHIJ" r1
r3 = next "KLMNO" r2
r4 = next "PQRST" r3
r5 = next "UVWXY" r4       
-}

next :: String -> [[String]] -> [[String]]
next row xs = tail $ last $
           scanl (\acc -> \xs -> acc ++ [(map (row!!(length acc-1):) (last acc)) 
                                        ++ (map (row!!(length acc-1):) xs)]) 
                                        [[]]
                                        xs

          
vsetko = foldl (\acc -> \row -> next row acc) [[""], [], [], [], []] tab            








comb n k = product[n-k+1..n] `div` product [1..k]
possibs m n = comb ((n-1)+(m-1)) (n-1)

ddmm dd mm = (possibs (dd+1) (mm+1)) * (possibs (100-dd) (100-mm))

ddmm55 dd mm = (possibs (dd+1) (mm+1)) * (possibs (5-dd) (5-mm))

{-
Main> tail $ last $ next "ABCDE" [[""], [], [], [], []]
[["A"],["BA"],["CBA"],["DCBA"],["EDCBA"]]
-}
