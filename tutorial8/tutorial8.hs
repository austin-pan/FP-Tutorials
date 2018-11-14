-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 9(12-16 Nov.)
module Tutorial8 where

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations
-- all states, symbols, start state, final states, transition states (state, symbol, next state)
type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int]
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (k,_,_,_,_) = k
alph (_,a,_,_,_)   = a
start (_,_,s,_,_)  = s
final (_,_,_,f,_)  = f
trans (_,_,_,_,t)  = t


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q] --fsm, source state, symbol -> transition states
delta fsm ss s = [ s2 | (s1,sym,s2) <- (trans fsm), s1 == ss, sym == s ]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool -- fsm, inputs -> satisfiable
accepts fsm str = accepts' fsm (start fsm) str
    where
        accepts' :: (Eq q) => FSM q -> q -> String -> Bool -- fsm, current state, inputs left -> satisfiable
        accepts' fsm s []     = s `elem` (final fsm)
        accepts' fsm s (x:xs) = or [ accepts' fsm cs xs | cs <- delta fsm s x ]


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical l = nub $ sort l


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q] -- nfsm, source superstate, symbol -> target superstate
ddelta fsm ss s = canonical $ concat [ delta fsm s' s | s' <- ss ]


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]] -- nfsm, list of superstates -> transition superstates
next fsm ls = canonical $ [start fsm] : [ ddelta fsm l c | l <- ls, c <- alph fsm ]


-- 7. there should be a better way to write this
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]] -- nfsm, list of superstates -> all reachable superstates
reachable fsm ls = canonical $ [start fsm] : r' fsm [[]] ls
    where
        r' :: (Ord q) => FSM q -> [[q]] -> [[q]] -> [[q]]  -- nfsm, previous compilation of list of superstates, list of superstates -> all reachable superstates
        r' f prev ss
            | prev /= ss  = (next' f ss) ++ (r' f ss (next' f ss))
            | otherwise   = prev

        next' :: (Ord q) => FSM q -> [[q]] -> [[q]] -- nfsm, list of superstates -> transition superstates
        next' fsm ls = canonical $ [ ddelta fsm l c | l <- ls, c <- alph fsm ]


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]] -- nfsm, list of superstates -> the superstates within the list that are accepting
dfinal fsm ss = canonical [ s | s <- ss, ssHasFinal fsm s ]
    where
        ssHasFinal fsm s = or [ f `elem` s | f <- final fsm ]


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]] -- nfsm, list of superstates -> list of transitions (state, input symbol, transition state)
dtrans fsm ss = [ (s, a, ddelta fsm s a) | s <- ss, a <- alph fsm ]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q] -- nfsm -> dfsm version of nfsm
deterministic fsm = (superstates, alph fsm, [start fsm], dfinal fsm superstates, dtrans fsm superstates)
    where
        superstates = reachable fsm [[start fsm]]

-- 12.
-- Greek letter epsilon, for making a transition without consuming input
epsilon = '\x03B5'

m3 :: FSM Int
m3 = ([0,1,2,3,4,5],
      ['a','b'],
      0,
      [5],
      [(0,epsilon,1),
       (0,epsilon,5),
       (1,'a',2),
       (2,epsilon,3),
       (3,'b',4),
       (4,epsilon,5),
       (4,epsilon,1)])

dm3 :: FSM [Int]
dm3 =  ([[],[0,1,5],[1,4,5],[2,3]],
        ['a','b'],
        [0,1,5],
        [[0,1,5],[1,4,5]],
        [([],'a',[]),
         ([],'b',[]),
         ([0,1,5],'a',[2,3]),
         ([0,1,5],'b',[]),
         ([1,4,5],'a',[2,3]),
         ([1,4,5],'b',[]),
         ([2,3],'a',[]),
         ([2,3],'b',[1,4,5])])


eClose :: (Ord q) => FSM q -> q -> [q]
eClose = undefined

eeClose :: (Ord q) => FSM q -> [q] -> [q]
eeClose = undefined

eddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
eddelta = undefined

enext :: (Ord q) => FSM q -> [[q]] -> [[q]]
enext = undefined

ereachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
ereachable = undefined

edtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
edtrans = undefined

edeterministic :: (Ord q) => FSM q -> FSM [q]
edeterministic = undefined

-- Optional Material
--13.
charFSM :: Char -> FSM Int
charFSM = undefined

emptyFSM :: FSM Int
emptyFSM = undefined

--14.
intFSM :: (Ord q) => FSM q -> FSM Int
intFSM = undefined

concatFSM :: Ord q => Ord q' => FSM q -> FSM q' -> FSM Int
concatFSM = undefined

--15.
stringFSM :: String -> FSM Int
stringFSM = undefined


-- For QuickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

prop_stringFSM1 n = accepts (stringFSM n') n'
      where n' = safeString n
prop_stringFSM2 n m = (m' == n') || (not $ accepts (stringFSM n') m')
                where m' = safeString m
                      n' = safeString n

--16.
completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM = undefined

unionFSM :: (Ord q) => FSM q -> FSM q -> FSM Int
unionFSM a b = undefined

prop_union n m l =  accepts (unionFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l'|| accepts (stringFSM m') l') &&
                    accepts (unionFSM (stringFSM n') (stringFSM m')) n' && accepts (unionFSM (stringFSM n') (stringFSM m')) m'
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l

--17.
star :: (Ord q) => FSM q -> FSM q
star = undefined


prop_star a n = (star $ stringFSM a') `accepts` (concat [a' | x <- [0..n]]) &&
                (star $ stringFSM a') `accepts` ""
      where a' = safeString a

--18.
complement :: (Ord q) => FSM q -> FSM Int
complement = undefined

prop_complement :: String -> String -> Bool
prop_complement n m = (n' == m')
                      || accepts (complement $ stringFSM n') m'
                      && (not $ accepts (complement $ stringFSM n') n)
                      where n' = safeString n
                            m' = safeString m

intersectFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
intersectFSM a b = undefined

prop_intersect n m l = accepts (intersectFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l' && accepts (stringFSM m') l')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l



prop1 a b = star ((stringFSM a') `unionFSM` (stringFSM b')) `accepts` (a'++b'++a'++a')
 where a' = safeString a
       b' = safeString b

prop2 a b = ((stringFSM a') `intersectFSM` (intFSM ((stringFSM b') `unionFSM` (stringFSM a')))) `accepts` a'
             where a' = safeString a
                   b' = safeString b
