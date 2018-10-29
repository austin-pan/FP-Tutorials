-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 3(01-05 Oct.)
module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
    | x `mod` 2 == 0 = (x `div` 2) : halveEvensRec xs
    | otherwise = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs)
    | x >= lo && x <= hi = x : inRangeRec lo hi xs
    | otherwise = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
    | x > 0 = 1 + countPositivesRec xs
    | otherwise = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositivesRec l == countPositives l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
    | isDigit x = digitToInt x * multDigitsRec xs
    | otherwise = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of length l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs = [ b | (a,b)<-xs ++ [(ch,ch)], ch==a] !! 0

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((a,b):xs)
    | ch==a = b
    | otherwise = lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUpRec c k == lookUp c k


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)
{-
    | isAlpha ch = [ snd x | x <- makeKey k, fst x == ch] !! 0
--    | isAlpha ch = [ alphabet !! ((snd x + k) `mod` (length alphabet)) | x<-zip alphabet [0..], fst x==ch] !! 0
    | otherwise = ch
--}

-- 9.

normalize :: String -> String
normalize [] = []
normalize (x:xs)
    | isAlpha x || isDigit x = (toUpper x) : normalize xs
    | otherwise = normalize xs


encipherStr :: Int -> String -> String
encipherStr k str = [encipher k x | x<-(normalize str)]


-- 10.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [ (b,a) | (a,b)<-xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((a,b):xxs) = (b,a) : reverseKey xxs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey x = reverseKey x == reverseKeyRec x


-- 11.

decipher :: Int -> Char -> Char
decipher k ch
    | isAlpha ch = [ fst x | x <- makeKey k, snd x == ch] !! 0
--    | isAlpha ch = [alphabet !! ((b + (length alphabet) - k) `mod` (length alphabet)) | (a,b) <- (zip alphabet [0..]), a==ch] !! 0
    | otherwise = ch

decipherStr :: Int -> String -> String
decipherStr _ [] = []
decipherStr k (x:xs)
    | isAlpha x && isUpper x || isDigit x || x==' ' = (decipher k x) : decipherStr k xs
    | otherwise = decipherStr k xs

-- Optional Material
-- =================


-- 12.

contains :: String -> String -> Bool
contains [] _ = False
contains (x:xs) subStr
    | isPrefixOf subStr (x:xs) = True
    | otherwise = contains xs subStr


-- 13.

candidates :: String -> [(Int, String)]
candidates str = [ (x,decipherStr x str) | x <- [0..26], y <- ["THE","AND"], contains (decipherStr x str) y ]


-- 14.

splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive str
    | length str >= n = (take n str) : splitEachFive (drop n str)
    | otherwise = [ a | (a,b)<-zip (str++(take n (repeat 'X'))) [1..n] ] : []
        where n=5
-- otherwise = [ a | (a,b)<-zip str [1..5] ] : []

prop_transpose :: String -> Bool
prop_transpose str = transpose (transpose (splitEachFive str)) == splitEachFive str


-- 15.
encrypt :: Int -> String -> String
encrypt k str = [y | x<-transpose (splitEachFive (encipherStr k str)), y<-x]


-- 16.
decrypt :: Int -> String -> String
decrypt k str = [y | x<-transpose (splitEachN (decipherStr k str) (length str)), y<-x]

splitEachN :: String -> Int -> [String]
splitEachN [] l = []
splitEachN str l = (take n str) : splitEachN (drop n str) l
    where n = l `div` 5