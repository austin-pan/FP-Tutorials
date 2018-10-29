-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 4
--
-- Week 5(15-19 Oct.)

module Tutorial4 where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio
import Test.QuickCheck

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles ints = map (* 2) ints

-- b.
penceToPounds :: [Int] -> [Float]
penceToPounds ps = map ((/ 100.0) . fromIntegral) ps

-- c.
uppersComp :: String -> String
uppersComp str = [toUpper c | c<-str]


-- 2. Filter
-- a.
alphas :: String -> String
alphas str = filter isAlpha str

-- b.
above :: Int -> [Int] -> [Int]
above num ints = filter (> num) ints

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals list = filter (\(x,y) -> x/=y) list

-- d.
rmCharComp :: Char -> String -> String
rmCharComp c str = [x | x<-str, x/=c]


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (* 2) $ filter (>3) xs

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse $ filter (even . length) strs

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec []       = []
concatRec [x]      = x
concatRec (xs:xss) = xs ++ concatRec xss

concatFold :: [[a]] -> [a]
concatFold list = foldr (++) [] list

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec [] str     = str
rmCharsRec (x:xs) str = rmCharsRec xs (rmCharComp x str)

rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr rmChar str chars

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform (x:xs) = all (== x) (x:xs)

-- b.
valid :: Matrix -> Bool
valid matrix = (uniform [length row | row<-matrix]) && (length matrix > 0) && (length (matrix !! 0) > 0)


-- 6.
plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2
    | equalMatrices m1 m2 = zipWith (zipWith (+)) m1 m2
    | otherwise           = error "invalid input"
        where
            equalMatrices :: Matrix -> Matrix -> Bool -- checks if matrices can be added
            equalMatrices m1 m2 = (valid m1) && (valid m2) && (length m1 == length m2) && (length (m1 !! 0) == length (m2 !! 0))
    {-
    | equalMatrices m1 m2 = [plusRow (m1 !! n) (m2 !! n) | n <- [0..(length m1-1)]]
    | otherwise = error "invalid input"
        where
            plusRow :: Num a => [a] -> [a] -> [a]
            plusRow [] [] = []
            plusRow (x:xs) (y:ys) = x+y : plusRow xs ys

            equalMatrices :: Matrix -> Matrix -> Bool
            equalMatrices m1 m2 = (valid m1) && (valid m2) && (length m1 == length m2) && (length (m1 !! 0) == length (m2 !! 0)) -}

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2
    | equalRowCol m1 m2 = [[ sum $ zipWith (*) r c | r <- (transpose m2)] | c <- m1]
    | otherwise         = error "invalid input"
        where
            equalRowCol :: Matrix -> Matrix -> Bool
            equalRowCol m1 m2 = (valid m1) && (valid m2) && (length (m1 !! 0) == length m2)

-------------------------------------
-------------------------------------
-- Tutorial Activities
-------------------------------------
-------------------------------------

-- 9.
-- a.
uppers :: String -> String
uppers str = map toUpper str

prop_uppers :: String -> Bool
prop_uppers str = uppers str == uppersComp str

-- b.
rmChar ::  Char -> String -> String
rmChar ch str = filter ((/=) ch) str

prop_rmChar :: Char -> String -> Bool
prop_rmChar ch str = rmChar ch str == rmCharComp ch str

-- c.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' str = map (toUpper) (filter isAlpha str)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- d.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs)
    | x         = andRec xs
    | otherwise = False

andFold :: [Bool] -> Bool
andFold list = foldr (&&) True list

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- 11.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ f a b | (a,b) <- zip xs ys ]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

-------------------------------------
-------------------------------------
-- Optional material
-------------------------------------
-------------------------------------
-- 13.
b,g :: Matrix
b = [[1,2],[3,4]]
g = [[5,6],[7,8]]
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = map $ map f

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f a b = map (uncurry (zipWith (f))) $ zip a b

-- All ways of deleting a single element from a list
removes :: Matrix -> [Matrix]
removes m = [[[x !! c | c <- [0..length x-1], c/=col] | x<-m, fromJust (elemIndex x m) /= 0] | col<-[0..length (m !! 0)-1]]

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = [[[[x !! c | c <- [0..length x-1], c/=col] | x<-m, fromJust (elemIndex x m) /= row] | col<-[0..length (m !! 0)-1] ] | row<-[0..length m-1] ]

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = [[ (-1)^(i+j) | j<-[1..w]] | i<-[1..h]]

{-
--mini :: Matrix -> [[Matrix]]
mini m = [[[x !! c | c <- [0..length x-1], c/=col] | x<-m, fromJust (elemIndex x m) /= 0] | col<-[0..length (m !! 0)-1]]


testD m = sum [((-1)^n) * (topRow !! n) * (d2x2 (minis !! n)) | n<-[0..length minis-1]]
    where
        d2x2 :: Matrix -> Rational
        d2x2 m2 = (m2 !! 0 !! 0) * (m2 !! 1 !! 1) - (m2 !! 0 !! 1) * (m2 !! 1 !! 0)

        minis = mini m
        topRow = m !! 0
--det :: Matrix -> Rational
det m1
    | length m1 == 2 = d2x2 m1
    | length m1 == 1 = m1 !! 0 !! 0
    | otherwise      = sum [((-1)^n) * (topRow !! n) * (det (minis !! n)) | n<-[0..length minis-1]]
        where
            d2x2 :: Matrix -> Rational
            d2x2 m2 = (m2 !! 0 !! 0) * (m2 !! 1 !! 1) - (m2 !! 0 !! 1) * (m2 !! 1 !! 0)

            minis = mini m1
            topRow = m1 !! 0

--d4x4 :: Matrix -> Rational
d4x4 m = sum [((-1)^n) * (topRow !! n) * (d3x3 (minis !! n)) | n<-[0..length minis-1]]
    where
        minis = mini m
        topRow = m !! 0
--d3x3 :: Matrix -> Rational
d3x3 m = sum [((-1)^n) * (topRow !! n) * (d2x2 (minis !! n)) | n<-[0..length minis-1]]
    where
        d2x2 :: Matrix -> Rational
        d2x2 m = (m !! 0 !! 0) * (m !! 1 !! 1) - (m !! 0 !! 1) * (m !! 1 !! 0)

        minis = mini m
        topRow = m !! 0
-}
--testM3 = [[6,1,1],[4,-2,5],[2,8,7]]
--testM4 = [[1,2,3,4],[5,6,7,2],[9,10,11,12],[13,14,13,18]]
--[Matrix] -> [[Matrix]]

determinant :: Matrix -> Rational
determinant [[x]] = x
determinant m1
    | validDet && length m1 == 2 = d2x2 m1
    | otherwise                  = sum [((-1)^n) * (topRow !! n) * (determinant (minis !! n)) | n<-[0..length minis-1]]
        where
            d2x2 :: Matrix -> Rational
            d2x2 m2 = (m2 !! 0 !! 0) * (m2 !! 1 !! 1) - (m2 !! 0 !! 1) * (m2 !! 1 !! 0)

            minis = removes m1 -- matrix of all the matrices that don't coincide with an index of a cell in the top row
            topRow = m1 !! 0

            validDet = valid m1 && (length m1 == length (m1 !! 0))

cofactors :: Matrix -> Matrix
cofactors m = zipMatrix (*) (signMatrix size size) minorDeterminants
    where
        size = length m
        minorDeterminants = mapMatrix determinant (minors m)

scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = mapMatrix (* k)

inverse :: Matrix -> Matrix
inverse m
    | validInv  = scaleMatrix (1 / determinant m) (transpose (cofactors m))
    | otherwise = error "Invalid Matrix"
        where validInv = valid m && (length m == length (m !! 0)) && (determinant m /= 0)
