-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 8(05-09 Nov.)
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + max (depth left) (depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k v left right) = toList left ++ [(k,v)] ++ toList right

-- Exercise 8
{-
    b. sets the leaf to a singleton
    c. sets the current node to the value being inserted if keys are equal; otherwise adds a node to the subtree based on if the key is GT or LT the current node's key
-}
set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | key >= k  = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get k Leaf = Nothing
get key (Node k v left right)
    | key == k  = Just v
    | key <= k  = get key left
    | key >= k  = get key right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = fl'
    where
        -- recursive
        fl :: Ord k => Keymap k a -> [(k,a)] -> Keymap k a
        fl _ [] = Leaf
        fl tree ((k,v):xs) = set k v (fl tree xs)

        -- foldr using uncurry
        fl' :: Ord k => [(k,a)] -> Keymap k a
        fl' xs = foldr (uncurry (set)) Leaf xs

        -- foldr using lambda function
        fl'' :: Ord k => [(k,a)] -> Keymap k a
        fl'' xs = foldr (\(k,v) -> set k v) Leaf xs


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 13

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT key m = fromList $ filter (\x -> (fst x) < key) $ toList m

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT key m = fromList $ filter (\x -> (fst x) > key) $ toList m

-- Exercise 14

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge m1 m2 = merge' m1 $ toList m2
    -- fromList (toList m1 ++ toList m2)
    where
        merge' m1 [] = m1
        merge' m1 ((k,a):xs) = merge' (set k a m1) xs

-- forall merged lists, an element in the merged list exist in either the first or second list that make it
prop_merge :: (Ord k, Eq a) => k -> Keymap k a -> Keymap k a -> Bool
prop_merge key m1 m2 = (get' merged == get' m1) || (get' merged == get' m2)
    where
        merged = merge m1 m2
        get' = get key

-- forall merged lists, the list version of the tree is equal to the combined lists of the first and second list without duplicates
prop_merge' :: (Ord k, Eq a) => Keymap k a -> Keymap k a -> Bool
prop_merge' m1 m2 = toList (merge m1 m2) == (nub (toList m2 ++ toList m1))

-- Exercise 15

del :: Ord k => k -> Keymap k a -> Keymap k a
del k Leaf = Leaf
del key (Node k v left right)
    | key == k  = merge left right
    | key >= k  = Node k v left (del key right)
    | key <= k  = Node k v (del key left) right

-- Exercise 16

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select f Leaf = Leaf
select f (Node k v left right)
    | f v       = Node k v (select f left) (select f right)
    | otherwise = del k (Node k v (select f left) (select f right))
        -- merge (select f left) (select f right)

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
