import Debug.Trace 
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.Random
import Data.List ((\\))
import System.Environment
import System.IO.Unsafe
import Data.Either
import OthelloTools
import OthelloMoveCheck

{- | This program is used as a template for the CPSC 449  Othello assignment.

Feel free to modify this file as you see fit.

Copyright: Copyright 2015, Rob Kremer (rkremer@ucalgary.ca), University of Calgary. 
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

---Main-------------------------------------------------------------

main = main' (unsafePerformIO getArgs)
    
{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    putStrLn "\nThe initial board:"
    print initBoard
    
    putStrLn "The initial board rotated 90 degrees:"
    putBoard $ rotateClock $ theBoard initBoard
    

    putStrLn "\nThe initial board with reallyStupidStrategy having played one move (clearly illegal!):"
    let mv = reallyStupidStrategy (initBoard) B
       in case mv of
          Nothing   -> putStrLn "Black passed."
          (Just pt) -> putBoard $ replace2 (theBoard initBoard) pt B

---Strategies-------------------------------------------------------

{- | This is the type for all player functions.  A player strategy function takes a 
     board and returns a point (Int,Int) that it chooses -- this should be a legal move.  
     If the player passes, the funciton should return Nothing.
-}           
type Chooser = GameState -> Cell -> Maybe (Int,Int)

-- | This strategy lives up to it's name: it always chooses to play at cell (0,0).
reallyStupidStrategy  :: Chooser
reallyStupidStrategy b c = Just (0,0)


---Board rotations-------------------------------------------------------------

-- | Rotate a board 90 degrees clockwise.
rotateClock     :: [[a]] -> [[a]]
rotateClock       [ [a0, a1, a2, a3, a4, a5, a6, a7],
                    [b0, b1, b2, b3, b4, b5, b6, b7],
                    [c0, c1, c2, c3, c4, c5, c6, c7],
                    [d0, d1, d2, d3, d4, d5, d6, d7],
                    [e0, e1, e2, e3, e4, e5, e6, e7],
                    [f0, f1, f2, f3, f4, f5, f6, f7],
                    [g0, g1, g2, g3, g4, g5, g6, g7],
                    [h0, h1, h2, h3, h4, h5, h6, h7] ] =
                  [ [h0, g0, f0, e0, d0, c0, b0, a0],
                    [h1, g1, f1, e1, d1, c1, b1, a1],
                    [h2, g2, f2, e2, d2, c2, b2, a2],
                    [h3, g3, f3, e3, d3, c3, b3, a3],
                    [h4, g4, f4, e4, d4, c4, b4, a4],
                    [h5, g5, f5, e5, d5, c5, b5, a5],
                    [h6, g6, f6, e6, d6, c6, b6, a6],
                    [h7, g7, f7, e7, d7, c7, b7, a7] ]

-- | Rotate a board 90 degrees counter clockwise.
rotateCounter     [ [h0, g0, f0, e0, d0, c0, b0, a0],
                    [h1, g1, f1, e1, d1, c1, b1, a1],
                    [h2, g2, f2, e2, d2, c2, b2, a2],
                    [h3, g3, f3, e3, d3, c3, b3, a3],
                    [h4, g4, f4, e4, d4, c4, b4, a4],
                    [h5, g5, f5, e5, d5, c5, b5, a5],
                    [h6, g6, f6, e6, d6, c6, b6, a6],
                    [h7, g7, f7, e7, d7, c7, b7, a7] ] =
                  [ [a0, a1, a2, a3, a4, a5, a6, a7],
                    [b0, b1, b2, b3, b4, b5, b6, b7],
                    [c0, c1, c2, c3, c4, c5, c6, c7],
                    [d0, d1, d2, d3, d4, d5, d6, d7],
                    [e0, e1, e2, e3, e4, e5, e6, e7],
                    [f0, f1, f2, f3, f4, f5, f6, f7],
                    [g0, g1, g2, g3, g4, g5, g6, g7],
                    [h0, h1, h2, h3, h4, h5, h6, h7] ]

-- rough implementation of rotate 45

clock45 :: [[Cell]] -> [[Cell]]
clock45 [ [a0, a1, a2, a3, a4, a5, a6, a7],
   [b0, b1, b2, b3, b4, b5, b6, b7],
   [c0, c1, c2, c3, c4, c5, c6, c7],
   [d0, d1, d2, d3, d4, d5, d6, d7],
   [e0, e1, e2, e3, e4, e5, e6, e7],
   [f0, f1, f2, f3, f4, f5, f6, f7],
   [g0, g1, g2, g3, g4, g5, g6, g7],
   [h0, h1, h2, h3, h4, h5, h6, h7] ] =
-- when implementing code, make sure to not have it based off size of board
      [ [a0],
   [b0, a1],
   [c0, b1, a2],
   [d0, c1, b2, a3],
   [e0, d1, c2, b3, a4],
   [f0, e1, d2, c3, b4, a5],
   [g0, f1, e2, d3, c4, b5, a6],
   [h0, g1, f2, e3, d4, c5, b6, a7],
   [h1, g2, f3, e4, d5, c6, b7],
   [h2, g3, f4, e5, d6, c7],
   [h3, g4, f5, e6, d7],
   [h4, g5, f6, e7],
   [h5, g6, f7],
   [h6, g7],
   [h7] ]

counter45 :: [[Cell]] -> [[Cell]]
counter45     [ [a0, a1, a2, a3, a4, a5, a6, a7],
      [b0, b1, b2, b3, b4, b5, b6, b7],
      [c0, c1, c2, c3, c4, c5, c6, c7],
      [d0, d1, d2, d3, d4, d5, d6, d7],
      [e0, e1, e2, e3, e4, e5, e6, e7],
      [f0, f1, f2, f3, f4, f5, f6, f7],
      [g0, g1, g2, g3, g4, g5, g6, g7],
      [h0, h1, h2, h3, h4, h5, h6, h7] ] =
         [ [a7],
      [a6, b7],
      [a5, b6, c7],
      [a4, b5, c6, d7],
      [a3, b4, c5, d6, e7],
      [a2, b3, c4, d5, e6, f7],
      [a1, b2, c3, d4, e5, f6, g7],
      [a0, b1, c2, d3, e4, f5, g6, h7],
      [b0, c1, d2, e3, f4, g5, h6],
      [c0, d1, e2, f3, g4, h5],
      [d0, e1, f2, g3, h4],
      [e0, f1, g2, h3],
      [f0, g1, h2],
      [g0, h1],
      [h0] ]

reverseClock45 :: [[a]] -> [[a]]
reverseClock45         [ [a0],
         [b0, a1],
         [c0, b1, a2],
         [d0, c1, b2, a3],
         [e0, d1, c2, b3, a4],
         [f0, e1, d2, c3, b4, a5],
         [g0, f1, e2, d3, c4, b5, a6],
         [h0, g1, f2, e3, d4, c5, b6, a7],
         [h1, g2, f3, e4, d5, c6, b7],
         [h2, g3, f4, e5, d6, c7],
         [h3, g4, f5, e6, d7],
         [h4, g5, f6, e7],
         [h5, g6, f7],
         [h6, g7],
         [h7] ] =
            [ [a0, a1, a2, a3, a4, a5, a6, a7],
         [b0, b1, b2, b3, b4, b5, b6, b7],
         [c0, c1, c2, c3, c4, c5, c6, c7],
         [d0, d1, d2, d3, d4, d5, d6, d7],
         [e0, e1, e2, e3, e4, e5, e6, e7],
         [f0, f1, f2, f3, f4, f5, f6, f7],
         [g0, g1, g2, g3, g4, g5, g6, g7],
         [h0, h1, h2, h3, h4, h5, h6, h7] ]

reverseCounter45 :: [[a]] -> [[a]]
reverseCounter45      [[a7],
         [a6, b7],
         [a5, b6, c7],
         [a4, b5, c6, d7],
         [a3, b4, c5, d6, e7],
         [a2, b3, c4, d5, e6, f7],
         [a1, b2, c3, d4, e5, f6, g7],
         [a0, b1, c2, d3, e4, f5, g6, h7],
         [b0, c1, d2, e3, f4, g5, h6],
         [c0, d1, e2, f3, g4, h5],
         [d0, e1, f2, g3, h4],
         [e0, f1, g2, h3],
         [f0, g1, h2],
         [g0, h1],
         [h0] ] =
            [ [a0, a1, a2, a3, a4, a5, a6, a7],
         [b0, b1, b2, b3, b4, b5, b6, b7],
         [c0, c1, c2, c3, c4, c5, c6, c7],
         [d0, d1, d2, d3, d4, d5, d6, d7],
         [e0, e1, e2, e3, e4, e5, e6, e7],
         [f0, f1, f2, f3, f4, f5, f6, f7],
         [g0, g1, g2, g3, g4, g5, g6, g7],
         [h0, h1, h2, h3, h4, h5, h6, h7] ]

-- | These functions return a rotated point the same as the board rotations above.
type PointRotation = (Int,Int) -> (Int,Int)

-- | rotate 90 degrees clockwise.
rotatePt             :: PointRotation
rotatePt             (x,y) = (7-y, x)   
               
---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                    in  (if null zs then (if null ys then [] else init ys) else ys) ++ [elem] ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

-- | Gets a cell in a row of cells
getCell :: [Cell] -> Int -> Cell
getCell xs n = xs !! n

-- | Gets a cell from the board
getCell2 :: [[Cell]] -> (Int,Int) -> Cell
getCell2 xs (x,y) = getCell (xs !! y) x

-- | FLIPPING FUNCTIONS -----

-- | flipThis calls each directional flip to flip cells upon a valid play
flipThis :: [[Cell]] -> Player -> (Int,Int) -> [[Cell]]
flipThis board player (0,0) = (flipDownRightForward (flipDownForward (flipRightForward (replace2 board (0,0) (tile player)) player (1,0)) player (0,1)) player (1,1))
flipThis board player (7,0) = (flipDownLeftForward (flipDownForward (flipLeftForward (replace2 board (7,0) (tile player)) player (6,0)) player (7,1)) player (6,1))
flipThis board player (0,7) = (flipUpRightForward (flipUpForward (flipRightForward (replace2 board (0,7) (tile player)) player (1,7)) player (0,6)) player (1,6))
flipThis board player (7,7) = (flipUpLeftForward (flipUpForward (flipLeftForward (replace2 board (7,7) (tile player)) player (6,7)) player (7,6)) player (6,6))
flipThis board player (x,0) = (flipDownRightForward (flipDownLeftForward (flipDownForward (flipRightForward (flipLeftForward (replace2 board (x,0) (tile player)) player (x-1,0)) player (x+1,0)) player (x,1)) player (x-1,1)) player (x+1,1))
flipThis board player (x,7) = (flipUpRightForward (flipUpLeftForward (flipUpForward (flipRightForward (flipLeftForward (replace2 board (x,0) (tile player)) player (x-1,0)) player (x+1,0)) player (x,6)) player (x-1,6)) player (x+1,6))
flipThis board player (7,y) = (flipDownLeftForward (flipUpLeftForward (flipDownForward (flipUpForward (flipLeftForward (replace2 board (7,y) (tile player)) player (6,y)) player (7,y-1)) player (7, y+1)) player (6,y-1)) player (6,y+1))
flipThis board player (0,y) = (flipDownRightForward (flipUpRightForward (flipDownForward (flipUpForward (flipRightForward (replace2 board (0,y) (tile player)) player (1,y)) player (0,y-1)) player (0, y+1)) player (1,y-1)) player (1,y+1))
flipThis board player (x,y) = (flipDownRightForward (flipUpRightForward (flipDownLeftForward (flipUpLeftForward (flipUpLeftForward (flipDownForward (flipUpForward (flipLeftForward (flipRightForward (replace2 board (x,y) (tile player)) player (x+1,y)) player (x-1, y)) player (x,y-1)) player (x, y+1)) player (x-1,y-1)) player (x-1,y-1)) player (x-1,y+1)) player (x+1,y-1)) player (x+1,y+1))

-- | flip_Forward tests to see that there is a bordering piece on the other side of the played cell (checking rightmost cells from the played cell)
flipRightForward :: Board -> Player -> (Int, Int) -> Board
flipRightForward board player (7,y) = 	if ((getCell2 board (7,y)) == E)
					then board
					else	if ((getCell2 board (7,y)) == tile player)
						then (flipRightBackward board player (6,y))
						else board
flipRightForward board player (0,y) = board
flipRightForward board player (x,y) = 	if ((getCell2 board (x,y)) == (tile (invertPlayer player))) 
					then (flipRightForward board player (x+1,y))
					else	if ((getCell2 board (x,y)) == tile player)
						then (flipRightBackward board player (x-1,y))
						else board

-- | flip_Backward changes the appropriate cells along a line from the discovered bordering piece to the original cell (flipping rightmost cells from the played cell)
flipRightBackward :: Board -> Player -> (Int, Int) -> Board
flipRightBackward board player (x, y) = if (getCell2 board (x,y)  == (tile player))
					then (board)
					else (flipRightBackward (replace2 board (x, y) (tile player)) player (x-1,y))

-- | flip_Forward tests to see that there is a bordering piece on the other side of the played cell (checking leftmost cells from the played cell)
flipLeftForward :: Board -> Player -> (Int, Int) -> Board
flipLeftForward board player (0,y) = if ((getCell2 board (0,y)) == E)
					then board
					else	if ((getCell2 board (0,y)) == tile player)
						then (flipLeftBackward board player (1,y))
						else board
flipLeftForward board player (7,y) = board
flipLeftForward board player (x,y) = if ((getCell2 board (x,y)) == (tile (invertPlayer player))) 
					then (flipLeftForward board player (x-1,y))
					else if ((getCell2 board (x,y)) == tile player)
						then (flipLeftBackward board player (x+1,y))
						else board

-- | flip_Backward changes the appropriate cells along a line from the discovered bordering piece to the original cell (flipping leftmost cells from the played cell)
flipLeftBackward :: Board -> Player -> (Int, Int) -> Board
flipLeftBackward board player (x, y) = if (getCell2 board (x,y)  == (tile player))
					then (board)
					else (flipLeftBackward (replace2 board (x, y) (otherCell (getCell2 board (x,y)))) player (x+1,y))

-- | flip_Forward tests to see that there is a bordering piece on the other side of the played cell (checking cells above the played cell)
flipUpForward :: Board -> Player -> (Int, Int) -> Board
flipUpForward board player (x,7) = if ((getCell2 board (x,7)) == E)
					then board
					else	if ((getCell2 board (x,7)) == tile player)
						then (flipUpBackward board player (x,6))
						else board
flipUpForward board player (x,0) = board
flipUpForward board player (x,y) = if ((getCell2 board (x,y)) == (tile (invertPlayer player)))
					then (flipUpForward board player (x,y-1))
					else if (getCell2 board (x,y)) == (tile player)
						then (flipUpBackward board player (x,y+1))
						else board

-- | flip_Backward changes the appropriate cells along a line from the discovered bordering piece to the original cell (flipping cells above the played cell)
flipUpBackward :: Board -> Player -> (Int, Int) -> Board
flipUpBackward board player (x, y) = if (getCell2 board (x,y)  == (tile player))
					then (board)
					else (flipUpBackward (replace2 board (x, y) (otherCell (getCell2 board (x,y)))) player (x,y+1))

-- | flip_Forward tests to see that there is a bordering piece on the other side of the played cell (checking cells below the played cell)
flipDownForward :: Board -> Player -> (Int, Int) -> Board
flipDownForward board player (x,0) = if ((getCell2 board (x,0)) == E)
					then board
					else	if ((getCell2 board (x,0)) == tile player)
						then (flipDownBackward board player (x,1))
						else board
flipDownForward board player (x,7) = board
flipDownForward board player (x,y) = if ((getCell2 board (x,y)) == (tile (invertPlayer player))) 
					then (flipDownForward board player (x,y+1))
					else if (getCell2 board (x,y)) == (tile player)
						then (flipDownBackward board player (x,y-1))
						else board

-- | flip_Backward changes the appropriate cells along a line from the discovered bordering piece to the original cell (flipping cells below the played cell)
flipDownBackward :: Board -> Player -> (Int, Int) -> Board
flipDownBackward board player (x, y) = if (getCell2 board (x,y)  == (tile player))
					then (board)
					else (flipDownBackward (replace2 board (x, y) (otherCell (getCell2 board (x,y)))) player (x,y-1))

-- | flip_Forward tests to see that there is a bordering piece on the other side of the played cell (checking cells diagonally below and to the right of the played cell)
flipDownRightForward :: Board -> Player -> (Int, Int) -> Board
flipDownRightForward board player (0,y) =  	if (getCell2 board (0,y)) == E
						then board
						else	if (getCell2 board (0,y)) == tile player
							then flipDownRightBackward board player (1,y+1)
							else board
flipDownRightForward board player (x,0) =  	if (getCell2 board (x,0)) == E
						then board
						else	if (getCell2 board (x,0)) == tile player
							then flipDownRightBackward board player (x+1,1)
							else board
flipDownRightForward board player (7,y) = 	if (getCell2 board (7,y)) == E
						then board
						else	if (getCell2 board (7,y)) == tile player
							then flipDownRightBackward board player (6,y-1)
							else board
flipDownRightForward board player (x,7) = 	if (getCell2 board (x,7)) == E
						then board
						else	if (getCell2 board (x,7)) == tile player
							then flipDownRightBackward board player (x-1,6)
							else board
flipDownRightForward board player (x,y) =	if (getCell2 board (x,y)) == (tile (invertPlayer player))
						then flipDownRightForward board player (x+1,y+1)
						else	if (getCell2 board (x,y)) == tile player
							then flipDownRightBackward board player (x-1,y-1)
							else board

-- | flip_Backward changes the appropriate cells along a line from the discovered bordering piece to the original cell (flipping cells diagonally up and to the left of the discovered cell)
flipDownRightBackward :: Board -> Player -> (Int, Int) -> Board
flipDownRightBackward board player (x,y) =	if getCell2 board (x,y) == tile player
						then board
						else flipDownRightBackward (replace2 board (x,y) (tile player)) player (x-1,y-1)

-- | flip_Forward tests to see that there is a bordering piece on the other side of the played cell (checking cells diagonally below and to the left of the played cell)
flipDownLeftForward :: Board -> Player -> (Int, Int) -> Board
flipDownLeftForward board player (7,y) =  	if (getCell2 board (7,y)) == E
						then board
						else	if (getCell2 board (7,y)) == tile player
							then flipDownLeftBackward board player (6,y+1)
							else board
flipDownLeftForward board player (x,0) =  	if (getCell2 board (x,0)) == E
						then board
						else	if (getCell2 board (x,7)) == tile player
							then flipDownLeftBackward board player (x-1,1)
							else board
flipDownLeftForward board player (0,y) = 	if (getCell2 board (0,y)) == E
						then board
						else	if (getCell2 board (0,y)) == tile player
							then flipDownLeftBackward board player (1,y-1)
							else board
flipDownLeftForward board player (x,7) = 	if (getCell2 board (x,7)) == E
						then board
						else	if (getCell2 board (x,7)) == tile player
							then flipDownLeftBackward board player (x+1,6)
							else board
flipDownLeftForward board player (x,y) =	if (getCell2 board (x,y)) == (tile (invertPlayer player))
						then flipDownLeftForward board player (x-1,y+1)
						else	if (getCell2 board (x,y)) == tile player
							then flipDownLeftBackward board player (x+1,y-1)
							else board

-- | flip_Backward changes the appropriate cells along a line from the discovered bordering piece to the original cell (flipping cells diagonally up and to the right of the discovered cell)
flipDownLeftBackward :: Board -> Player -> (Int, Int) -> Board
flipDownLeftBackward board player (x,y) =	if getCell2 board (x,y) == tile player
						then board
						else flipDownLeftBackward (replace2 board (x,y) (tile player)) player (x+1,y-1)

-- | flip_Forward tests to see that there is a bordering piece on the other side of the played cell (checking cells diagonally up and to the left of the played cell)
flipUpLeftForward :: Board -> Player -> (Int, Int) -> Board
flipUpLeftForward board player (7,y) =  	if (getCell2 board (7,y)) == E
						then board
						else	if (getCell2 board (7,y)) == tile player
							then flipUpLeftBackward board player (6,y-1)
							else board
flipUpLeftForward board player (x,7) =  	if (getCell2 board (x,7)) == E
						then board
						else	if (getCell2 board (x,7)) == tile player
							then flipUpLeftBackward board player (x-1,6)
							else board
flipUpLeftForward board player (0,y) = 		if (getCell2 board (0,y)) == E
						then board
						else	if (getCell2 board (0,y)) == tile player
							then flipUpLeftBackward board player (1,y+1)
							else board
flipUpLeftForward board player (x,0) = 		if (getCell2 board (x,0)) == E
						then board
						else	if (getCell2 board (x,0)) == tile player
							then flipUpLeftBackward board player (x+1,1)
							else board
flipUpLeftForward board player (x,y) =	if (getCell2 board (x,y)) == (tile (invertPlayer player))
					then flipUpLeftForward board player (x-1,y-1)
					else	if (getCell2 board (x,y)) == tile player
						then flipUpLeftBackward board player (x+1,y+1)
						else board

-- | flip_Backward changes the appropriate cells along a line from the discovered bordering piece to the original cell (flipping cells diagonally down and to the right of the discovered cell)	
flipUpLeftBackward :: Board -> Player -> (Int, Int) -> Board
flipUpLeftBackward board player (x,y) =	if getCell2 board (x,y) == tile player
					then board
					else flipUpLeftBackward (replace2 board (x,y) (tile player)) player (x+1,y+1)

-- | flip_Forward tests to see that there is a bordering piece on the other side of the played cell (checking cells diagonally up and to the right of the played cell)
flipUpRightForward :: Board -> Player -> (Int, Int) -> Board
flipUpRightForward board player (0,y) =  	if (getCell2 board (0,y)) == E
						then board
						else	if (getCell2 board (0,y)) == tile player
							then flipUpRightBackward board player (1,y-1)
							else board
flipUpRightForward board player (x,7) =  	if (getCell2 board (x,7)) == E
						then board
						else	if (getCell2 board (x,7)) == tile player
							then flipUpRightBackward board player (x+1,6)
							else board
flipUpRightForward board player (7,y) = 	if (getCell2 board (7,y)) == E
						then board
						else	if (getCell2 board (7,y)) == tile player
							then flipUpRightBackward board player (6,y+1)
							else board
flipUpRightForward board player (x,0) = 	if (getCell2 board (x,0)) == E
						then board
						else	if (getCell2 board (x,0)) == tile player
							then flipUpRightBackward board player (x-1,1)
							else board
flipUpRightForward board player (x,y) =	if (getCell2 board (x,y)) == (tile (invertPlayer player))
					then flipUpRightForward board player (x+1,y-1)
					else	if (getCell2 board (x,y)) == tile player
						then flipUpRightBackward board player (x-1,y+1)
						else board

-- | flip_Backward changes the appropriate cells along a line from the discovered bordering piece to the original cell (flipping cells diagonally down and to the left of the discovered cell)	
flipUpRightBackward :: Board -> Player -> (Int, Int) -> Board
flipUpRightBackward board player (x,y) =	if getCell2 board (x,y) == tile player
						then board
						else flipUpRightBackward (replace2 board (x,y) (tile player)) player (x-1,y+1)

-- | flipUpLeftForward tests cells in a diagonal to the left and upwards for a bordering piece
{-flipUpLeftForward :: Board -> Player -> (Int,Int) -> Board
flipUpLeftForward board player (x,y) = if (getCell2 board (x,y) == (tile (invertPlayer player)))
					then (flipUpLeftForward board player (x-1,y-1))
					else 	if ((getCell2 board (x,y)) == tile player)
						then (flipUpLeftBackward board player (x+1,y+1))
						else board


---AIs---------------------------------------------------------------------------

-- | Takes gamestate and player colour and returns Maybe (int, int)
-- This AI prioritizes corners, tthen edges, then spaces not touching the edges, then the remaining
-- if nothing can be found it passes.
-- By Riley Lahd
corners :: Chooser
corners gamestate cell
		| (elem (0,0) validMoves) = Just (0,0)
		| (elem (0,7) validMoves) = Just (0,7)
		| (elem (7,0) validMoves) = Just (7,0)
		| (elem (7,7) validMoves) = Just (7,7)
		| (safeZone validMoves) /= [] = Just ((safeZone validMoves) !! 0)
		| ((length validMoves) > 0) = Just (validMoves !! 0)
		| ((length validMoves) == 0) = Nothing
		where validMoves = (moves (theBoard gamestate) (playerOf cell))
		      --safe = (((fst )> 0) && ((fst ) < 7) && ((snd ) > 0) && ((snd ) < 7))


safeZone :: [(Int, Int)] -> [(Int, Int)]
safeZone [] = []
safeZone (coord:left)
		| ((x > 1) && (x < 6) && (y > 1) && (y < 6)) = [coord] ++ (safeZone left)
		| True = (safeZone left)
		where x = fst coord
		      y = snd coord

