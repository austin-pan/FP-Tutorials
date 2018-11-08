-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 6(22-26 Oct.)
module Tutorial5 where

import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )


--------------------------------------------------
--------------------------------------------------
---Implementing propositional logic in Haskell----
--------------------------------------------------
--------------------------------------------------

-- The datatype 'Wff'

type Name = String
data Wff = Var Name
          | F
          | T
          | Not Wff
          | Wff :|: Wff
          | Wff :&: Wff
          | Wff :->: Wff
          | Wff :<->: Wff
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Wffs

-- turns a Wff into a string approximating mathematical notation
showWff :: Wff -> String
showWff (Var x)        =  x
showWff (F)            =  "F"
showWff (T)            =  "T"
showWff (Not p)        =  "(~" ++ showWff p ++ ")"
showWff (p :|: q)      =  "(" ++ showWff p ++ "|" ++ showWff q ++ ")"
showWff (p :&: q)      =  "(" ++ showWff p ++ "&" ++ showWff q ++ ")"
showWff (p :->: q)     =  "(" ++ showWff p ++ "->" ++ showWff q ++ ")"
showWff (p :<->: q)    =  "(" ++ showWff p ++ "<->" ++ showWff q ++ ")"

-- evaluates a wff in a given environment
eval :: Env -> Wff -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  eval e (Not p) || eval e q
eval e (p :<->: q)    =  eval e (p :->: q) && eval e (q :->: p)

-- retrieves the names of variables from a wff -
--  NOTE: variable names in the result must be unique
names :: Wff -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a wff is satisfiable
satisfiable :: Wff -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


--------------------------------------------------
--------------------------------------------------
------------------ Exercises ---------------------
--------------------------------------------------
--------------------------------------------------

-- 1.
wff1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
wff2 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))


-- 2.
tautology :: Wff -> Bool
tautology p = and [eval e p | e <- envs (names p)]

prop_taut1 :: Wff -> Bool
prop_taut1 p = tautology p || satisfiable (Not p)

prop_taut2 :: Wff -> Bool
prop_taut2 p = not (satisfiable p) || not (tautology (Not p))


-- 3.
wff3 = (Var "P" :->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))
wff4 = (Var "P" :<->: Var "Q") :&: ((Var "P" :&: Not (Var "Q")) :|: (Not (Var "P") :&: Var "Q"))

-- 4.
equivalent :: Wff -> Wff -> Bool
equivalent p1 p2 = and [eval e p1 == eval e p2 | e <- envs (names (p1 :&: p2))]

-- 5.
subformulas :: Wff -> [Wff]
subformulas (Var x)        =  [Var x]
subformulas (F)            =  [F]
subformulas (T)            =  [T]
subformulas (Not p)        =  nub ([Not p] ++ subformulas p)
subformulas (p :|: q)      =  nub ([p :|: q] ++ subformulas p ++ subformulas q)
subformulas (p :&: q)      =  nub ([p :&: q] ++ subformulas p ++ subformulas q)
subformulas (p :->: q)     =  nub ([p :->: q] ++ subformulas p ++ subformulas q)
subformulas (p :<->: q)    =  nub ([p :<->: q] ++ subformulas p ++ subformulas q)


--------------------------------------------------
--------------------------------------------------
---------------- Tutorial Activities -------------
--------------------------------------------------
--------------------------------------------------
-- Warmup exercises

-- The datatype 'Fruit'
data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 6.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Orange str _)
    | str `elem` bOranges = True
    | otherwise = False
        where
            bOranges :: [String]
            bOranges = ["Tarocco","Moro","Sanguinello"]
isBloodOrange (Apple _ _) = False


-- 7.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments fruits = sum [slicesBloodOrange x | x<-fruits, isBloodOrange x]
    where
        slicesBloodOrange :: Fruit -> Int
        slicesBloodOrange (Orange _ n) = n
        slicesBloodOrange _ = 0

-- 8.
worms :: [Fruit] -> Int
worms fruits = sum [1 | x<-fruits, hasWorms x]
    where
        hasWorms :: Fruit -> Bool
        hasWorms (Apple _ w) = w
        hasWorms _ = False

-- Test your Logic
-- 9.
wff5 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))
wff6 = (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q")


equivalent' :: Wff -> Wff -> Bool
equivalent' p1 p2 = tautology combined
    --(satisfiable combined) && (not (satisfiable (Not combined)))
    where
        combined = p1 :<->: p2 :: Wff

prop_equivalent :: Wff -> Wff -> Bool
prop_equivalent p1 p2 = equivalent p1 p2 == equivalent' p1 p2

--------------------------------------------------
--------------------------------------------------
-- Optional Material
--------------------------------------------------
--------------------------------------------------

-- 9.
-- check for negation normal form
isNNF :: Wff -> Bool
isNNF (Var x)        =  True
isNNF (F)            =  True
isNNF (T)            =  True
isNNF (Not (Var x))  =  True
isNNF (Not p)        =  False
isNNF (p :|: q)      =  isNNF p && isNNF q
isNNF (p :&: q)      =  isNNF p && isNNF q
isNNF (p :->: q)     =  False
isNNF (p :<->: q)    =  False

-- 10.
-- convert to negation normal form
toNNF :: Wff -> Wff
toNNF (Var x)        =  Var x
toNNF (Not (Var x))  =  Not (Var x)
toNNF (F)            =  F
toNNF (T)            =  T
toNNF (Not (Not p))  =  toNNF p
toNNF (Not p)        =  toNNF (distributeNot' p)
    where
        distributeNot' :: Wff -> Wff
        distributeNot' (T)           = F
        distributeNot' (F)           = T
        distributeNot' (Var x)       = Not (Var x)
        distributeNot' (Not (Var x)) = Var x
        distributeNot' (Not p)       = p
        distributeNot' (p :|: q)     = Not p :&: Not q
        distributeNot' (p :&: q)     = Not p :|: Not q
        distributeNot' (p :->: q)    = distributeNot' (Not p :|: q)
        distributeNot' (p :<->: q)   = distributeNot' ((Not p :|: q) :&: (Not q :|: p))
toNNF (p :|: q)      =  toNNF p :|: toNNF q
toNNF (p :&: q)      =  toNNF p :&: toNNF q
toNNF (p :->: q)     =  toNNF (Not p :|: q)
toNNF (p :<->: q)    =  toNNF ((Not p :|: q) :&: (Not q :|: p))

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Wff -> Bool
prop_NNF1 f  =  isNNF (toNNF f)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Wff -> Bool
prop_NNF2 f  =  equivalent f (toNNF f)


-- 11.
-- check whether a formula is in conj. normal form
isCNF :: Wff -> Bool
isCNF (F)                   =  True
isCNF (T)                   =  True
isCNF (p :&: q)             =  isCNF p && isCNF q
isCNF p                     =  isClause p

isClause :: Wff -> Bool
isClause p = isClause' p
    where
        isClause' :: Wff -> Bool
        isClause' (p :|: q)
            | isAtom p && isAtom q        = True
            | isAtom p                    = isClause q
            | isAtom q                    = isClause p
            | isClause' p && isClause' q  = True
            | otherwise                   = False
        isClause' p                       = isAtom p

        isAtom :: Wff -> Bool
        isAtom (T)                  = True
        isAtom (F)                  = True
        isAtom (Var x)              = True
        isAtom (Not (Var x))        = True
        isAtom _                    = False

-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Wff]] -> Wff
listsToCNF p = listsToCNF' p
    where
        listsToCNF' :: [[Wff]] -> Wff
        listsToCNF' [x]  = combineOrs x
        listsToCNF' (p:ps) = combineOrs p :&: listsToCNF' ps

        combineOrs :: [Wff] -> Wff
        combineOrs [T]           = T
        combineOrs [F]           = F
        combineOrs [Var x]       = Var x
        combineOrs [Not (Var x)] = Not (Var x)
        combineOrs (w:ws)        = w :|: combineOrs ws


-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Wff -> [[Wff]]
listsFromCNF (T)           = [[T]]
listsFromCNF (F)           = [[F]]
listsFromCNF (Var x)       = [[Var x]]
listsFromCNF (Not (Var x)) = [[Not (Var x)]]
listsFromCNF (p :&: q)     = listsFromCNF p ++ listsFromCNF q
listsFromCNF (p :|: q)     = [combineOrs (p :|: q)]
    where
        combineOrs :: Wff -> [Wff]
        combineOrs (T)           = [T]
        combineOrs (F)           = [F]
        combineOrs (Var x)       = [Var x]
        combineOrs (Not (Var x)) = [Not (Var x)]
        combineOrs (p :|: q)     = combineOrs p ++ combineOrs q
        combineOrs (p :&: q)     = error "and exists"


-- 15.
-- transform an arbitrary formula into a list of lists
test1 = ((Var "T" :<->: Var "R") :->: (Var "T" :->: Var "Q"))
test2 = ((((Var "A" :|: Var "B") :&: (Var "A" :|: Var "C")) :&: (Var "A" :|: Var "D")) :|: Var "E")
test3 = ((Var "A" :<->: Var "B") :->: Var "C")
test4 = ((((Var "A" :|: Var "B") :|: Var "C") :&: ((Not (Var "D") :|: Var "E") :|: Var "F")) :&: ((Not (Var "F") :|: Not (Var "E")) :|: Var "F"))
test5 = ((T :|: Var "B") :->: (Var "A" :|: T))

toCNFList :: Wff -> [[Wff]]
toCNFList :: Wff -> [[Wff]]
toCNFList p = cnf (toNNF p)
    where
      cnf F              =  [[]]
      cnf T              =  []
      cnf (Var n)        =  [[Var n]]
      cnf (Not (Var n))  =  [[Not (Var n)]]
      cnf (p :&: q)      =  nub (cnf p ++ cnf q)
      cnf (p :|: q)      =  [nub $ x ++ y | x <- cnf p, y <- cnf q]
{-
toCNFList (T)               = [[T]]
toCNFList (F)               = [[F]]
toCNFList p                 = listsFromCNF $ nnfToCNF $ toNNF p
    where
        nnfToCNF :: Wff -> Wff
        nnfToCNF p
            | isCNF p = p
            | otherwise = nnfToCNF $ distributeOr p
                where
                    distributeOr :: Wff -> Wff
                    distributeOr (T)               = T
                    distributeOr (F)               = F
                    distributeOr (Var x)           = Var x
                    distributeOr (Not (Var x))     = Not (Var x)
                    distributeOr (p :|: (q :&: r)) = ((p :|: q) :&: (p :|: r))
                    distributeOr ((q :&: r) :|: p) = ((q :|: p) :&: (r :|: p))
                    distributeOr (p :|: q)         = distributeOr p :|: distributeOr q
                    distributeOr (p :&: q)         = distributeOr p :&: distributeOr q
-}

-- convert to conjunctive normal form
toCNF :: Wff -> Wff
toCNF wff  =  listsToCNF (toCNFList wff)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Wff -> Bool
prop_CNF p  =  equivalent p (toCNF p)


-- counts number of satisfiable options
countTrues :: Wff -> String
countTrues p = show (sum [1 | e <- envs (names p), eval e p]) ++ " / " ++ show (2^(length $ names p))

t1 = (Var "A" :->: Var "B") :|: (Var "C" :->: Var "D")

{-(Var "H" :->: Var "A") :&: (Var "A" :->: (Var "B" :&: Var "C")) :&: ((Var "B" :|: Var "C") :->: Var "D") :&: (Var "A" :->: Var "E") :&: (Var "E" :->: Var "F")
    :&: (Var "F" :->: Var "G") :&: (Var "G" :->: Var "H")-}


-- For QuickCheck --------------------------------------------------------

instance Show Wff where
    show  =  showWff

instance Arbitrary Wff where
    arbitrary  =  sized wff
        where
          wff n | n <= 0     =  atom
                | otherwise  =  oneof [ atom
                                      , liftM Not subform
                                      , liftM2 (:|:) subform subform
                                      , liftM2 (:&:) subform subform
                                      , liftM2 (:->:) subform subform
                                      , liftM2 (:<->:) subform' subform'
                                      ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  wff (n `div` 2)
                   subform' =  wff (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Wff] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showWff p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showWff p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Wff -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Wff -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"
