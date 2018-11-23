-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 10(19-23 Nov.)
module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Test.QuickCheck
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
        conv x = if blank x then "123456789" else [x]

cp :: [[a]] -> [[a]]
cp []        =  [[]]
cp (xs:xss)  =  [ x:ys | x <- xs, ys <- cp xss ]

-- 8.
expand :: Matrix [Digit] -> [Matrix Digit]
expand m = cp [ cp r | r <- m ]

prop_expand :: Matrix [Digit] -> Bool
prop_expand m = length (expand m) == product (map (\x -> product (map length x)) m)

numPosAns :: Matrix Digit -> Int
numPosAns m = length (expand (choices m)) -- wrong
-- 11, 12, 13.
-- transpose :: [[a]] -> [[a]]
-- transpose [xs]      =  [[x] | x <- xs]
-- transpose (xs:xss)  =  zipWith (:) xs (transpose xss)

ungroup :: [[a]] -> [a]
ungroup []     = []
ungroup (x:xs) = x ++ ungroup xs

byRow = replicate 9 "123456789" :: Matrix Digit

rows, cols, boxs :: Matrix a -> Matrix a
rows m = m
cols m = transpose m
boxs m = (map ungroup . ungroup) $ map cols $ (group . map group) m

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = xs == nub xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = and [ distinct r | r <- rows g ] && and [ distinct c | c <- cols g ] && and [ distinct b | b <- boxs g ]

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices

-- 17.
prop_involutions :: Matrix Digit -> Bool
prop_involutions m
    | isSquare m = ((rows . rows) m == m) && ((boxs . boxs) m == m) && ((cols . cols) m == m)
    | otherwise = True
    where
        isSquare :: Matrix Digit -> Bool
        isSquare m = and [ length r > 0 | r <- m ] && length m > 0 && length m == length (head m) && length (nub [ length r | r <- m ]) == 1

-- 18.
pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (\r -> if length r == 1 then r else filter (\x -> notElem x singletons) r) row
    where
        singletons = concat $ filter ((==1) . length) row

-- 19.
pruneBy :: (Matrix [Digit] -> Matrix [Digit])
             -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune m = pruneBy boxs $ pruneBy cols $ pruneBy rows m

-- 20.
many :: Eq a => (a -> a) -> a -> a
many g x = many' g (g x) x
    where
        many' g curr prev
            | curr == prev  = curr
            | otherwise     = many' g (g curr) curr

close :: (Eq a, Ord a) => [(a,a)] -> [(a,a)]
close pairs  =  nub (sort (pairs ++ [ (x,z) | (x,y) <- pairs,
              (y',z) <- pairs,
              y == y' ]))

-- 21.
the :: [Digit] -> Digit
the [d]  =  d

extract :: Matrix [Digit] -> Matrix Digit
extract m = map (map the) m

-- 22. solves easy and medium
solve :: Matrix Digit -> Matrix Digit
solve m = extract $ many prune (choices m)

-- 23.
failed :: Matrix [Digit] -> Bool
failed mat = any ((>0) . length) $ map (filter ((==0) . length)) mat

-- 24.
solved :: Matrix [Digit] -> Bool
solved mat = lengthOne mat && valid (extract mat)
    where lengthOne m = and $ map (all ((==1) . length)) m

-- 25.
smallest :: Matrix [Digit] -> Int
smallest mat = (head . sort) $ map length $ concat [ filter ((>1) . length) r | r <- mat ]

-- 26. already defined
break' :: (a -> Bool) -> [a] -> ([a],[a])
break' f = span (not . f)

-- 27.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat
    | not (lengthOne mat) = [ preMat ++ [preRow ++ [[d']] ++ postRow] ++ postMat | d' <- ds ]
    | otherwise = [[[]]]
        where
            (preMat, row:postMat) =  break (any p) mat
            (preRow, ds:postRow)  =  break p row
            p = ((== sm) . length)
            sm = smallest mat

            lengthOne m = and $ map (all ((==1) . length)) m

-- 28.
search :: Matrix Digit -> [Matrix Digit]
search mat = nub $ search' (many prune (choices mat))
    where
        search' :: Matrix [Digit] -> [Matrix Digit]
        search' mat
            | solved mat = [extract mat]
            | failed mat = [[[]]]
            | otherwise = (concat [ search' (many prune e) | e <- expand1 mat ] \\ [[]] ) \\ [[""]]

{-map extract $ filter solved $ e
    where
        e = many (\m -> concat (map expand1 m)) firstChoices
        firstChoices = expand1 $ many prune (choices mat)
-}


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
