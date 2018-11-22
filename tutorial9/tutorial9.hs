-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 10(19-23 Nov.)
module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)

type Row a     =  [a]
type Col a     =  [a]
type Matrix a  =  Col (Row a)
type Digit     =  Char

digits :: [Digit]
digits =  ['1'..'9']

blank :: Digit -> Bool
blank d  =  d == ' '

-- 2.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n []   = []
groupBy n list = take n list : groupBy n (drop n list)

-- 3.
intersperse :: a -> [a] -> [a]
intersperse c list = concat $ [[c]] ++ [ [x] ++ [c] | x <- list ]

-- 4. changed [String] to [Char]
showRow :: [Char] -> String
showRow str = concat $ intersperse "|" (group str)

-- 5. changed [[String]] to [[Char]]
showMat :: [[Char]] -> String
showMat strs = unlines $ concat $ intersperse [line] $ group strs
    where line = take (length (head strs)) (repeat '-')

-- 6.
put :: Matrix Digit -> IO ()
put m = putStr $ showMat [ showRow r | r <- m ]

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices m = [ [ conv x | x <- r ] | r <- m ]
    where
        conv :: Char -> String
        conv x = if x == ' ' then "123456789" else [x]

cp :: [[a]] -> [[a]]
cp []        =  [[]]
cp (xs:xss)  =  [ x:ys | x <- xs, ys <- cp xss ]

-- 8.
expand :: Matrix [Digit] -> [Matrix Digit]
expand m = undefined--[ cp r | r <- m ] [ cp]

-- 11, 12, 13.
-- transpose :: [[a]] -> [[a]]
-- transpose [xs]      =  [[x] | x <- xs]
-- transpose (xs:xss)  =  zipWith (:) xs (transpose xss)

ungroup :: [[a]] -> [a]
ungroup =  undefined

rows, cols, boxs :: Matrix a -> Matrix a
rows  =  undefined
cols  =  undefined
boxs  =  undefined

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs  = undefined

-- 15.
valid :: Matrix Digit -> Bool
valid g  = undefined

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple =  filter valid . expand . choices

-- 18.
pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row  = undefined

-- 19.
pruneBy :: (Matrix [Digit] -> Matrix [Digit])
             -> Matrix [Digit] -> Matrix [Digit]
pruneBy f  = undefined

prune :: Matrix [Digit] -> Matrix [Digit]
prune =  undefined

-- 20.
many :: Eq a => (a -> a) -> a -> a
many = undefined

-- 21.
the :: [Digit] -> Digit
the [d]  =  d

extract :: Matrix [Digit] -> Matrix Digit
extract = undefined

-- 22.
solve :: Matrix Digit -> Matrix Digit
solve = undefined

-- 23.
failed :: Matrix [Digit] -> Bool
failed mat  =  undefined

-- 24.
solved :: Matrix [Digit] -> Bool
solved =  undefined

-- 25.
smallest :: Matrix [Digit]
smallest = undefined

-- 27.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = undefined

-- 28.
search :: Matrix Digit -> [Matrix Digit]
search = undefined


-- Example from Bird

book    :: Matrix Digit
book    =  ["  4  57  ",
            "     94  ",
            "36      8",
            "72  6    ",
            "   4 2   ",
            "    8  93",
            "4      56",
            "  53     ",
            "  61  9  "]

-- Examples from websudoku.com

easy    :: Matrix Digit
easy    =  ["    345  ",
            "  89   3 ",
            "3    2789",
            "2 4  6815",
            "    4    ",
            "8765  4 2",
            "7523    6",
            " 1   79  ",
            "  942    "]

medium  :: Matrix Digit
medium  =  ["   4 6 9 ",
            "     3  5",
            "45     86",
            "6 2 74  1",
            "    9    ",
            "9  56 7 8",
            "71     64",
            "3  6     ",
            " 6 9 2   "]

hard    :: Matrix Digit
hard    =  ["9 3  42  ",
            "4 65     ",
            "  28     ",
            "     5  4",
            " 67 4 92 ",
            "1  9     ",
            "     87  ",
            "     94 3",
            "  83  6 1"]

evil    :: Matrix Digit
evil    =  ["  9      ",
            "384   5  ",
            "    4 3  ",
            "   1  27 ",
            "2  3 4  5",
            " 48  6   ",
            "  6 1    ",
            "  7   629",
            "     5   "]
br :: IO ()
br = putStrLn "***"

puts :: [Matrix Digit] -> IO ()
puts  =  sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g  =  put g >>
             puts (search g) >>
             br

main =  puzzle easy >>
        puzzle medium >>
        puzzle hard >>
        puzzle evil
