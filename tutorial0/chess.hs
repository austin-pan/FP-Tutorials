-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 0
--
-- Week 0(17-21 Sep.)
--
-- Insert your name and matriculation number here:
-- Name:
-- Nr. :
import PicturesSVG
import Test.QuickCheck



-- Exercise 8:

pic1 :: Picture
pic1 = above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside knight (invert knight)) (flipV (beside (invert knight) knight))

--test
pic3 :: Picture
pic3 =  (knight `beside` (invert knight)) `above` ((invert knight) `beside` knight)


-- Exercise 9:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow
    --repeatH 4 (beside blackSquare whiteSquare)

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)

whitePieces :: Picture
whitePieces = beside rook (beside knight (beside bishop (beside queen (beside king (beside bishop (beside knight rook))))))

whiteRow :: Picture
whiteRow = over whitePieces otherEmptyRow

blackRow :: Picture
blackRow = over (invert whitePieces) emptyRow

-- e)

pawns :: Picture
pawns = repeatH 8 pawn

populatedBoard :: Picture
populatedBoard = above (above (above blackRow (over (invert pawns) otherEmptyRow)) (repeatV 2 (above emptyRow otherEmptyRow))) (above (over pawns emptyRow) whiteRow)



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 10:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoAbove (twoBeside x)
