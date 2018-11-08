-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 8(05-09 Nov.)

module Tutorial7 where

import System.Random


-- Importing the keymap module

-- import KeymapList
import KeymapTree

-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = longProd'' xs
    -- longProd' xs 0
    where
        longProd' :: [(Barcode, Item)] -> Int -> Int
        longProd' [] len = len
        longProd' ((_,x):xs) len = longProd' xs (max (length (fst x)) len)

        longProd'' :: [(Barcode, Item)] -> Int
        longProd'' = foldr (\(b,i) -> max (length (fst i))) 0

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (b,i) = b ++ take 3 (repeat '.') ++ fst i ++ take (spill + 3) (repeat ('.')) ++ snd i
    where
        spill = (n - length (fst i))

showCatalogue :: Catalogue -> String
showCatalogue c = concat $ map ((++ "\n") . formatLine (longestProductLen (toList c))) (toList c)

-- Exercise 2
-- a. Maybe; Just ("The Macannihav'nmor Highland Single Malt, Bagpipes of Glory"), Just ("Thompson - \"Haskell: The Craft of Functional Programming\""), Just ("Universal deep-frying pan"), Nothing
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x):xs) = x : catMaybes xs
catMaybes (Nothing:xs)  = catMaybes xs

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bs c = concat [ maybeToList (get b c) | b <- bs ]


-- Exercise 4
{-
    a. 0.82s, 874,749,384 bytes
    b. 0.01s, 0.10s, 0.02s, 0.07s, 0.01s, 0.01s, 0.06s, 0.02s, 0.01s, 0.03s
    c. same time, all the elements
-}
-- For Exercises 6-10 check KeymapTree.hs

-- Exercise 12
{-
    a. 4.13s, 2,018,967,240 bytes
    b. 0.00s, 0.00s, 0.00s, 0.00s, 0.01s, 0.00s, 0.00s, 0.00s, 0.00s, 0.00s
    c. log2 (n)
-}

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
