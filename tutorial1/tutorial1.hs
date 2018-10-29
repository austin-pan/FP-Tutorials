-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 1
--
-- Week 1(24-28 Sep.)

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x<-xs, even x]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x<-xs, x>=lo, x<=hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [ x | x<-list, x>0]


-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [ digitToInt x | x<-str++"1", isDigit x]

countDigits :: String -> Int
countDigits str = length [ x | x<-str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits xs = 9^(length xs) >= multDigits xs


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise (x:xs) = toUpper x : [ toLower x | x<-xs]
    --capitalise s = toUpper (s !! 0) : [ toLower (s !! x) | x<-[1..(length s - 1)]]
    --[ if x==0 then toUpper (s !!  x) else toLower (s !! x) | x<-[0..(length s - 1)]]


-- 6. title

lowercase :: String -> String
lowercase s = [ toLower (s !! x) | x<-[0..(length s - 1)]]

-- List-comprehension version
title :: [String] -> [String]
title xs = capitalise (xs !! 0) : [ if length x>=4 then capitalise x else lowercase x | x<-tail xs]


-- 7. signs

sign :: Int -> Char
sign i
    | i>0 && i<10 = '+'
    | i==0 = '0'
    | i>(-10) && i<0 = '-'
    | otherwise = error "na"

    --if i>0 && i<10 then '+'
    --else if i==0 then '0'
    --else if i>(-10) && i<0 then '-'
    --else error "na"

signs :: [Int] -> String
signs xs = [ sign x | x<-xs, x<10, x>(-10)]


-- 8. score

vowels=['a','e','i','o','u']
score :: Char -> Int
score x = sum ([ 1 | k<-['a'..'z'], toLower x == k] ++ [ 1 | k<-vowels, toLower x == k] ++ [ 1 | k<-['A'..'Z'], x == k])

totalScore :: String -> Int
totalScore xs = product [ score x | x<-xs, isLetter x]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs>0

-- Tutorial Activity
-- 10. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = sum [ round (fromIntegral x*90/100) | x<-prices, x*90 < 200*100*100]

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = sum xs >= pennypincher xs

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ x | x<-words, (x !! pos) == letter, length x <= len]


-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [ n | n<-[0..(length str-1)], toLower (str !! n) == toLower goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) >= 0

-- 13. contains

contains :: String -> String -> Bool
contains str substr
    | length str == 0 = False
    | isPrefixOf substr str = True
    | otherwise = contains (drop 1 str) substr
    --length [ k | k<-[0..(length str-length substr)], isPrefixOf substr (drop k str)] > 0

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = if length str1 < length str2 then contains str1 str2 == False else True
