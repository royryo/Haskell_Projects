module Submission where
import Data.Char

{-

Name(s):  <write names here>
Date:  <submission date>

CSCI 054 - Spring 2023
Week04 problem set

-}

-- sanitize
sanitize :: [Char] -> [Char]
sanitize [] = []
sanitize (x:xs)  
 | (isSpace x) = x:sanitize xs 
 | (isAlpha x) = (toUpper x):sanitize xs 
 | otherwise = sanitize xs  

-- caesar
caesar :: Int -> [Char] -> [Char]
caesar n [] = []
caesar 0 lst = lst
caesar n lst =
 let msg = sanitize lst
 in map (shift n) msg
 where
  shift 0 x = x 
  shift n ' ' = chr((n - 1) `mod` 27 + ord 'A')
  shift n x 
    | (((ord x - ord 'A') + n) `mod` 27) == 26 = ' '
    | otherwise = chr((((ord x - ord 'A') + n) `mod` 27) + ord 'A')

-- keepFirst
keepFirst :: (Eq a) => [a] -> [a]
keepFirst [] = []
keepFirst lst = 
 let 
  keepFirstHelper lst [] = lst
  keepFirstHelper lst (x:xs) 
   | x `elem` lst = keepFirstHelper lst xs 
   | otherwise = keepFirstHelper (lst ++ [x]) xs 
 in 
  keepFirstHelper [] lst 

-- subst
subst :: String -> [(Char, Char)]
subst str = zip (['A'..'Z'] ++ [' ']) (keepFirst str)

-- substEncipher
substEncipher :: String -> String -> String
substEncipher [] [] = []
substEncipher n [] = []
substEncipher [] lst = []
substEncipher key str = 
    let code = subst (sanitize key) 
    in map (replace code) (sanitize str)    
    where   
     replace [] z = z  
     replace ((x,y):xs) z 
      | x == z = y   
      | otherwise = replace xs z 
   
-- substDecipher
substDecipher :: String -> String -> String
substDecipher [] [] = []
substDecipher n [] = []
substDecipher [] lst = []
substDecipher key str = 
    let code = subst (sanitize key) 
    in map (replace code) (sanitize str)    
    where  
     replace [] z = z    
     replace ((x,y):xs) z 
      | y == z = x   
      | otherwise = replace xs z 



