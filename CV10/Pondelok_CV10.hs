module Priprava_CV10 where

import Mparser

data LExp = LAMBDA String LExp | 
            ID String | 
            APP LExp LExp |
            CON String | CN Int
            deriving(Show, Read, Eq)

-- L -> (L L) | \id.L | id
lambda   :: Parser LExp
lambda   =  do { id <- identifier; return (ID id) }
            +++
            do { integ <- integer; return (CN integ) }
            +++
            do { open; m <- lambda; space; n<-lambda; close; return (APP m n) }
            +++
            do { char '\\'; id<-identifier; char '.'; m<-lambda; return (LAMBDA id m) }

-- B -> 0B | 1B | eps
binConst :: Parser Int
binConst = undefined
           
-----------------
morse   :: Parser String
morse   =   (char '.' >>= \_ -> char '-' >>= \_ -> return "A")
            `plus`
            (char '-' >>= \_ -> char '.' >>= \_ -> char '.'>>= \_ -> char '.' >>= \_ -> return "B")
            `plus`
            (char '.' >>= \_ -> return "C")
            `plus`
            (char '-' >>= \_ -> return "D")
            
morse1   :: Parser String
morse1   =   do { string ".-"; return "A"}
             `plus`
             do { string "-..."; return "B"}
            `plus`
             do { string "-.-."; return "C"}  
             `plus`
             do { string "-.."; return "D"}


-------
-- S -> aSa | bSb | a | b 

pali :: Parser String
pali = undefined


morseCodes = [(".-","A"), ("-...", "B"), ("-.-.","C"), ("-..","D"), (".","E"), ("..-.","F"),
              ("--.","G"), ("....","H"), ("..","I"), (".---","J"), ("-.-","K"), (".-..","L"), 
              ("--","M"), ("-.","N"), ("---","O"), (".--.","P"), ("--.-","Q"), (".-.","R"),
              ("...","S"), ("-","T"), ("..-","U"), ("...-","V"), (".--","W"), ("-..-","X"),
              ("-.--","Y"), ("--..","Z")
           -- ,("-----","0"), (".----","1"), ("..---","2"), ("...--","3"), ("....-","4"),
           -- (".....","5"), ("-....","6"), ("--...","7"), ("---..","8"), ("----.","9")
             ]
             
morseAlphabetScislami = [ (".-","A"),("-...","B"),("-.-.","C"),("-..","D"),(".","E"),("..-.","F"),("--.","G"),("....","H"),("..","I"),(".---","J"),("-.-","K"),(".-..","L"),("--","M"),("-.","N"),("---","O"),(".--.","P"),("--.-","Q"),(".-.","R"),("...","S"),("-","T"),("..-","U"),("...-","V"),(".--","W"),("-..-","X"),("-.--","Y"),("--..","Z"), (".----","1"),("..---","2"),("...--","3"),("....-","4"),(".....","5"),("-....","6"),("--...","7"),("---..","8"),("----.","9"),("-----","0")]

morseAlphabet = [ (".-","A"),("-...","B"),("-.-.","C"),("-..","D"),(".","E"),("..-.","F"),("--.","G"),("....","H"),("..","I"),(".---","J"),("-.-","K"),(".-..","L"),("--","M"),("-.","N"),("---","O"),(".--.","P"),("--.-","Q"),(".-.","R"),("...","S"),("-","T"),("..-","U"),("...-","V"),(".--","W"),("-..-","X"),("-.--","Y"),("--..","Z")]             

