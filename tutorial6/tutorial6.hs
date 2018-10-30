-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 6
--
-- Week 7(29 Oct.-02 Nov.)

-- module Tutorial6 where

import LSystem
import Test.QuickCheck

pathExample = (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#:  Go 30)

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (a :#: Sit)   = split a
split (Sit :#: b)   = split b
split (a :#: b)     = split a ++ split b
split a             = [a]

-- 1b. join
join :: [Command] -> Command
join [c]        = c
join (c:cs)     = c :#: join cs

-- 1c. equivalent
equivalent :: Command -> Command -> Bool
equivalent a b = split a == split b

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c

prop_split :: Command -> Bool
prop_split c = not (Sit `elem` split c) && not (":#:" `elem` map show (split c))


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n c
    | n > 1     = c :#: copy (n-1) c
    | otherwise = c

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d sides = copy sides (Go d :#: Turn angle)
    where
        angle :: Float
        angle = 180 - (fromIntegral ((sides-2) * 180) / fromIntegral sides)



-- Exercise 3
-- 3a. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side n step angle
    | side > 0  &&  n > 1     = Go side :#: Turn angle :#: spiral (side + step) (n-1) step angle
    | otherwise               = Go side :#: Turn angle


-- Exercise 4
-- 4a. optimise
-- Remember that Go does not take negative arguments.

optimise :: Command -> Command
optimise p = join' $ optimise' [c | c <- split p,c /= Sit, c /= Go 0, c /= Turn 0]
        where
            join' :: [Command] -> Command
            join' [c]        = c
            join' [c,Sit]    = c
            join' (Sit:cs)   = join' cs
            join' (c:cs)     = c :#: join' cs

            optimise' :: [Command] -> [Command]
            optimise' []                 = [Sit]
            optimise' [Go 0]             = [Sit]
            optimise' [Turn 0]           = [Sit]
            optimise' [Sit]              = [Sit]
            optimise' [c]                = [c]
            optimise' (Turn 0:cs)        = optimise' (cs)
            optimise' (Go 0:cs)          = optimise' (cs)
            optimise' (Turn a:Turn b:cs) = optimise' (Turn (a+b) : cs)
            optimise' (Go a:Go b:cs)     = optimise' (Go (a+b) : cs)
            optimise' (c:cs)             = c : optimise' (cs)

-- L-Systems

-- 5a. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
        f 0     = GrabPen red :#: Go d
        f x = g (x-1) :#: n :#: f (x-1) :#: n :#: g (x-1)
        g 0     = GrabPen blue :#: Go d
        g x = f (x-1) :#: p :#: g (x-1) :#: p :#: f (x-1)
        n       = Turn 60
        p       = Turn (-60)
        d       = 7

--------------------------------------------------
--------------------------------------------------
---------------- Tutorial Activities -------------
--------------------------------------------------
--------------------------------------------------

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = copy 3 (f x :#: p :#: p) -- f--f--f--
    where
        f 0     = GrabPen red :#: Go d
        f x = f (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: n :#: f (x-1) -- f+f--f+f
        n       = Turn 60
        p       = Turn (-60)
        d       = 10


-- 7. hilbert
hilbert :: Int -> Command
hilbert x = n :#: l x -- l
    where
        l 0     = Sit
        l x = n :#: r (x-1) :#: f (x-1) :#: p :#: l (x-1) :#: f (x-1) :#: l (x-1) :#: p :#: f (x-1) :#: r (x-1) :#: n -- +rf-lfl-fr+
        r 0     = Sit
        r x = p :#: l (x-1) :#: f (x-1) :#: n :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: n :#: f (x-1) :#: l (x-1) :#: p -- -lf+rfr+fl-
        f x     = GrabPen blue :#: Go d
        n       = Turn 90
        p       = Turn (-90)
        d       = 5.0

--------------------------------------------------
--------------------------------------------------
---------------- Optional Material ---------------
--------------------------------------------------
--------------------------------------------------

-- Bonus L-Systems
peanoGosper :: Int -> Command
peanoGosper x = f x -- f
    where
        f 0     = GrabPen red :#: Go d
        f x = f (x-1) :#: n :#: g (x-1) :#: n :#: n :#: g (x-1) :#: p :#: f (x-1) :#: p :#: p :#: f (x-1) :#: f (x-1) :#: p :#: g (x-1) :#: n -- f+g++g-f--ff-g+
        g 0     = GrabPen blue :#: Go d
        g x = p :#: f (x-1) :#: n :#: g (x-1) :#: g (x-1) :#: n :#: n :#: g (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: p :#: g (x-1) -- -f+gg++g+f--f-g
        n       = Turn 60
        p       = Turn (-60)
        d       = 5.0

cross :: Int -> Command
cross x = copy 4 (f x :#: p)-- f-f-f-f-
    where
        f 0     = GrabPen blue :#: Go d
        f x = f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) -- f-f+f+ff-f-f+f
        n       = Turn angle
        p       = Turn ((-1) * angle)
        angle   = 90
        d       = 5.0

branch :: Int -> Command
branch x = g x -- g
    where
        f 0     = GrabPen red :#: Go d
        f x = f (x-1) :#: f (x-1) -- ff
        g 0     = GrabPen blue :#: Go d
        g x = f (x-1) :#: p :#: Branch (Branch (g (x-1)) :#: n :#: g (x-1)) :#: n :#: f (x-1) :#: Branch (n :#: f (x-1) :#: g (x-1)) :#: p :#: g (x-1) -- f-[[g]+g]+f[+fg]-g
        n       = Turn angle
        p       = Turn ((-1) * angle)
        angle   = 22.5
        d       = 5.0

thirtytwo :: Int -> Command
thirtytwo x = copy 3 (f x :#: n) :#: f x -- F+F+F+F
    where
        f 0     = GrabPen blue :#: Go d
        f x = p :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: n -- -F+F-F-F+F+FF-F+F+FF+F-F-FF+FF-FF+F+F-FF-F-F+FF-F-F+F+F-F+
        n       = Turn angle
        p       = Turn ((-1) * angle)
        angle   = 90
        d       = 5.0


main :: IO ()
main = display $ cross 5

{-
    test = let inDirection angle = Branch (Turn angle :#: Go 100) in
          join (map inDirection [20,40..360])
    display pathExample
-}
