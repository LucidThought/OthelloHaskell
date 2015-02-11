import Debug.Trace 
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
--import System.Random
import Data.List ((\\), nub)
import System.Environment
import System.IO.Unsafe
import Data.Either
import OthelloTools

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



-- | to show the board each time it must look similar to this
{- | playGame :: type1 -> type2 -> type3 -> IO()
playGame type1 type2 type3 = do
		putStrLn (show board) or gamestate
		<insert game logic here>
		playGame switchtypes

		takes tuples of (AI, Player) first one being the one about to play and the second being the one that just played
		
-}

{-
playGame :: (Chooser, Chooser) -> GameState -> Gamestate
playGame (one, two) gamestate
				| ((blackPlayer == Nothing) && (whitePlayer == Nothing)) = gamestate
				| (blackPlayer == Nothing) = playGame (one, two) {(White, whitePlayer), (flipThis (theBoard gamestate) White whitePlayer)}
				| (whitePlayerAfterBlack == Nothing) = playGame (one, two) {(Black, blackPlayer), (flipThis (theBoard gamestate) Black blackPlayer)}
				| Otherwise = playGame (one, two) {(White, whitePlayerAfterBlack), (flipThis (theBoard blackGamestate) White whitePlayerAfterBlack)}
				where blackPlayer = one gamestate B
				      whitePlayer = two gamestate W
				      blackGamestate = (theBoard {(Black, blackPlayer), (flipThis (theBoard gamestate) Black blackPlayer)})
				      whitePlayerAfterBlack = two {(Black, blackPlayer), (flipThis (theBoard gamestate) Black blackPlayer)} W
  
-}
  
{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}


---Strategies-------------------------------------------------------
-- | Takes gamestate and player colour and returns Maybe (int, int)
-- This AI prioritizes corners, tthen edges, then spaces not touching the edges, then the remaining
-- if nothing can be found it passes.
-- By Riley Lahd u r a b
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

-- | Auxilliary Functions -----------------------------------------------
demoBoard = [ [B, E, E, E, E, E, E, E],	
	      [E, W, B, B, E, B, B, E], 
              [E, E, B, W, W, W, B, E],	
              [B, W, B, W, E, W, B, E], 
              [E, E, B, W, W, W, B, E], 
              [E, E, B, B, B, B, B, E], 
              [E, E, E, E, E, E, W, E], 
              [E, E, E, E, E, E, E, B] ]

demoState       :: GameState
demoState       = GameState (Black, Init)
            [ [B, E, E, E, E, E, E, E],	
	      [E, W, B, B, E, B, B, E], 
              [E, E, B, W, W, W, B, E],	
              [B, W, B, W, E, W, B, E], 
              [E, E, B, W, W, W, B, E], 
              [E, E, B, B, B, B, B, E], 
              [E, E, E, E, E, E, W, E], 
              [E, E, E, E, E, E, E, B] ]

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

-- | Drops last element of list
dropLast :: [a] -> [a]
dropLast x = reverse (drop 1 (reverse x))

-- | Get normal coordinates from clockwise 45 degree rotated coordinates
clock45Coord :: (Int, Int) -> (Int, Int)
clock45Coord (x,y)
		| (y <= 7) = (hiX, hiY)
		| (y > 7) = (loX, loY)	
		where hiX = x
		      hiY = y-x
		      loX = y + x - 7
		      loY = 7 - x

-- | Get normal coordinates from counter clockwise 45 degree rotated coordinates
counter45Coord :: (Int, Int) -> (Int, Int)
counter45Coord (x,y)
		| (y <= 7) = (hiX, hiY)
		| (y > 7) = (loX, loY)	
		where hiX = (7-y) + x
		      hiY = x
		      loX = x
		      loY = (y-7) + x

-- | Given a 'Player', return the coorespoinding 'Cell' colour for comparisons
tile :: Player -> Cell
tile Black = B
tile White = W

-- | countTiles takes a board, then passes some initial conditions along with an array of all board cells to the curried function countTiles'
countTiles :: [[Cell]] -> (Int, Int)
countTiles board = countTiles' Black 0 0 (concat board)

-- | countTiles' takes an array of all cells on a board and returns a tuple with the number of black tiles on the board, then the number of white tiles on the board
countTiles' :: Player -> Int -> Int -> [Cell] -> (Int, Int)
countTiles' player p p' [] = (p, p')
countTiles' player p p' (x:xs) = if x == tile player
				then countTiles' player (p+1) p' xs
				else 	if x == tile (invertPlayer player)
					then countTiles' player p (p'+1) xs
					else countTiles' player p p' xs 

---AIs---------------------------------------------------------------------------

-- | Takes gamestate and player colour and returns Maybe (int, int)
-- This AI prioritizes corners, tthen edges, then spaces not touching the edges, then the remaining
-- if nothing can be found it passes.
-- By Riley Lahd u r a b
{-
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
-}

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
						else board-}

-- | Move Checking section ---------------------------------

moves :: Board -> Player -> [(Int, Int)]
-- ^take board and player
-- find all valid moves for this player, return in array as coords
moves board player = nub ((movesDiagonal board player) ++ (movesVertical board player) ++ (movesHorizontal board player))

movesDiagonal :: Board -> Player -> [(Int, Int)]
-- ^Divide diagonal into clockwise right and left and counter right and left
movesDiagonal board player = (movesDiagonalClockwiseLeft board player) ++ (movesDiagonalCounterRight board player) ++ (movesDiagonalCounterLeft board player) ++ (movesDiagonalClockwiseRight board player)

movesVertical  :: Board -> Player -> [(Int, Int)]
-- ^Divide vertival into upwards and downwards
movesVertical board player = (movesVerticalDown board player) ++ (movesVerticalUp board player)

movesHorizontal  :: Board -> Player -> [(Int, Int)]
-- ^Divide horizontal into left and right
movesHorizontal board player = (movesHorizontalLeft board player) ++ (movesHorizontalRight board player)

-- | Compiles a list of valid moves to the left
movesHorizontalLeft  :: [[Cell]] -> Player -> [(Int, Int)]
movesHorizontalLeft board player = (movesHorizontalLeft' (board !! 0) player 0) ++
				(movesHorizontalLeft' (board !! 1) player 1) ++
				(movesHorizontalLeft' (board !! 2) player 2) ++
				(movesHorizontalLeft' (board !! 3) player 3) ++
				(movesHorizontalLeft' (board !! 4) player 4) ++
				(movesHorizontalLeft' (board !! 5) player 5) ++
				(movesHorizontalLeft' (board !! 6) player 6) ++
				(movesHorizontalLeft' (board !! 7) player 7)

-- | Compiles a list of valid moves on a single row looking leftwards
movesHorizontalLeft' :: [Cell] -> Player -> Int -> [(Int, Int)]
movesHorizontalLeft' [] _ _ = []
movesHorizontalLeft' [x] _ _ = []
movesHorizontalLeft' row player rowNum
	| (valid && empty) == True = coord ++ next
	| (valid && empty) == False = next
	where valid = movesInLine (tail (reverse row)) player
	      coord = [((length row)-1, rowNum)]
	      next = movesHorizontalLeft' (dropLast row) player rowNum
	      empty = cell2Char (head (reverse row)) == '_'
movesHorizontalLeft' _ _ _ = [(10,10)]

-- | Compiles a list of valid moves to the right
movesHorizontalRight  :: Board -> Player -> [(Int, Int)]
movesHorizontalRight board player = (movesHorizontalRight' (board !! 0) player 0) ++
				(movesHorizontalRight' (board !! 1) player 1) ++
				(movesHorizontalRight' (board !! 2) player 2) ++
				(movesHorizontalRight' (board !! 3) player 3) ++
				(movesHorizontalRight' (board !! 4) player 4) ++
				(movesHorizontalRight' (board !! 5) player 5) ++
				(movesHorizontalRight' (board !! 6) player 6) ++
				(movesHorizontalRight' (board !! 7) player 7)

-- | Compiles a list of valid moves in a single row looking rightwards
movesHorizontalRight' :: [Cell] -> Player -> Int -> [(Int, Int)]
movesHorizontalRight' [] _ _ = []
movesHorizontalRight' [x] _ _ = []
movesHorizontalRight' row player rowNum
	| (valid && empty) == True = coord ++ next
	| (valid && empty) == False = next
	where valid = movesInLine (tail row) player
	      coord = [(8-(length row), rowNum)]
	      next = movesHorizontalRight' (tail row) player rowNum
	      empty = cell2Char (head row) == '_'
movesHorizontalRight' _ _ _ = []

-- | Compiles a list of valid moves looking up
movesVerticalUp  :: Board -> Player -> [(Int, Int)]
movesVerticalUp board player = (movesVerticalUp' (map (!! 0) board) player 0) ++
				(movesVerticalUp' (map (!! 1) board) player 1) ++
				(movesVerticalUp' (map (!! 2) board) player 2) ++
				(movesVerticalUp' (map (!! 3) board) player 3) ++
				(movesVerticalUp' (map (!! 4) board) player 4) ++
				(movesVerticalUp' (map (!! 5) board) player 5) ++
				(movesVerticalUp' (map (!! 6) board) player 6) ++
				(movesVerticalUp' (map (!! 7) board) player 7)

-- | Compiles a list of valid moves in a single column looking upwards
movesVerticalUp' :: [Cell] -> Player -> Int -> [(Int, Int)]
movesVerticalUp' [] _ _ = []
movesVerticalUp' [x] _ _ = []
movesVerticalUp' column player columnNum
	| (valid && empty) == True = coord ++ next
	| (valid && empty) == False = next
	where valid = movesInLine (tail (reverse column)) player
	      coord = [(columnNum, (length column)-1)]
	      next = movesVerticalUp' (dropLast column) player columnNum
	      empty = cell2Char (head column) == '_'
movesVerticalUp' _ _ _ = []

-- | Compiles a list of valid moves looking down
movesVerticalDown  :: Board -> Player -> [(Int, Int)]
movesVerticalDown board player =(movesVerticalDown' (map (!! 0) board) player 0) ++
				(movesVerticalDown' (map (!! 1) board) player 1) ++
				(movesVerticalDown' (map (!! 2) board) player 2) ++
				(movesVerticalDown' (map (!! 3) board) player 3) ++
				(movesVerticalDown' (map (!! 4) board) player 4) ++
				(movesVerticalDown' (map (!! 5) board) player 5) ++
				(movesVerticalDown' (map (!! 6) board) player 6) ++
				(movesVerticalDown' (map (!! 7) board) player 7)

-- | Compiles a list of valid moves in a single column looking downwards
movesVerticalDown' :: [Cell] -> Player -> Int -> [(Int, Int)]
movesVerticalDown' [] _ _ = []
movesVerticalDown' [x] _ _ = []
movesVerticalDown' column player columnNum
	| (valid && empty) == True = coord ++ next
	| (valid && empty) == False = next
	where valid = movesInLine (tail column) player
	      coord = [(columnNum,8-(length column))]
	      next = movesVerticalDown' (tail column) player columnNum
	      empty = cell2Char (head column) == '_'
movesVerticalDown' _ _ _ = []

-- | Compiles a list of valid moves looking up and right
movesDiagonalClockwiseRight  :: Board -> Player -> [(Int, Int)]
movesDiagonalClockwiseRight board player =(movesDiagonalClockwiseRight' (board45 !! 0) player 0 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 1) player 1 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 2) player 2 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 3) player 3 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 4) player 4 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 5) player 5 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 6) player 6 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 7) player 7 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 8) player 8 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 9) player 9 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 10) player 10 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 11) player 11 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 12) player 12 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 13) player 13 0) ++
				(movesDiagonalClockwiseRight' (board45 !! 14) player 14 0)
				where board45 = clock45 board

-- | Compiles a list of valid moves in a single diagonal looking up-rightwards
movesDiagonalClockwiseRight'  :: [Cell] -> Player -> Int -> Int -> [(Int, Int)]
movesDiagonalClockwiseRight' [] _ _ _ = []
movesDiagonalClockwiseRight' [x] _ _ _ = []
movesDiagonalClockwiseRight' row player rowNum pos
	| (valid && empty) == True = coord ++ next
	| (valid && empty) == False = next
	where valid = movesInLine (tail row) player
	      coord = [ clock45Coord (pos, rowNum) ]
	      next = movesDiagonalClockwiseRight' (tail row) player rowNum (pos+1)
	      empty = cell2Char (head row) == '_'
movesDiagonalClockwiseRight' _ _ _ _ = []

-- | Compiles a list of valid moves looking down and left
movesDiagonalClockwiseLeft  :: Board -> Player -> [(Int, Int)]
movesDiagonalClockwiseLeft board player =(movesDiagonalClockwiseLeft' (board45 !! 0) player 0 0) ++
				(movesDiagonalClockwiseLeft' (board45 !! 1) player 1 1) ++
				(movesDiagonalClockwiseLeft' (board45 !! 2) player 2 2) ++
				(movesDiagonalClockwiseLeft' (board45 !! 3) player 3 3) ++
				(movesDiagonalClockwiseLeft' (board45 !! 4) player 4 4) ++
				(movesDiagonalClockwiseLeft' (board45 !! 5) player 5 5) ++
				(movesDiagonalClockwiseLeft' (board45 !! 6) player 6 6) ++
				(movesDiagonalClockwiseLeft' (board45 !! 7) player 7 7) ++
				(movesDiagonalClockwiseLeft' (board45 !! 8) player 8 6) ++
				(movesDiagonalClockwiseLeft' (board45 !! 9) player 9 5) ++
				(movesDiagonalClockwiseLeft' (board45 !! 10) player 10 4) ++
				(movesDiagonalClockwiseLeft' (board45 !! 11) player 11 3) ++
				(movesDiagonalClockwiseLeft' (board45 !! 12) player 12 2) ++
				(movesDiagonalClockwiseLeft' (board45 !! 13) player 13 1) ++
				(movesDiagonalClockwiseLeft' (board45 !! 14) player 14 0)
				where board45 = clock45 board

-- | Compiles a list of valid moves in a single diagonal looking down-leftwards
movesDiagonalClockwiseLeft'  :: [Cell] -> Player -> Int -> Int -> [(Int, Int)]
movesDiagonalClockwiseLeft' [] _ _ _ = []
movesDiagonalClockwiseLeft' [x] _ _ _ = []
movesDiagonalClockwiseLeft' row player rowNum pos
	| (valid && empty) == True = coord ++ next
	| (valid && empty) == False = next
	where valid = movesInLine (reverse (dropLast row)) player
	      coord = [ clock45Coord (pos, rowNum) ]
	      next = movesDiagonalClockwiseLeft' (dropLast row) player rowNum (pos-1)
	      empty = cell2Char  (row !! pos) == '_'
movesDiagonalClockwiseLeft' _ _ _ _ = []

-- | Compiles a list of valid moves looking up and left
movesDiagonalCounterLeft  :: Board -> Player -> [(Int, Int)]
movesDiagonalCounterLeft board player =(movesDiagonalCounterLeft' (board45 !! 0) player 0 0) ++
				(movesDiagonalCounterLeft' (board45 !! 1) player 1 1) ++
				(movesDiagonalCounterLeft' (board45 !! 2) player 2 2) ++
				(movesDiagonalCounterLeft' (board45 !! 3) player 3 3) ++
				(movesDiagonalCounterLeft' (board45 !! 4) player 4 4) ++
				(movesDiagonalCounterLeft' (board45 !! 5) player 5 5) ++
				(movesDiagonalCounterLeft' (board45 !! 6) player 6 6) ++
				(movesDiagonalCounterLeft' (board45 !! 7) player 7 7) ++
				(movesDiagonalCounterLeft' (board45 !! 8) player 8 6) ++
				(movesDiagonalCounterLeft' (board45 !! 9) player 9 5) ++
				(movesDiagonalCounterLeft' (board45 !! 10) player 10 4) ++
				(movesDiagonalCounterLeft' (board45 !! 11) player 11 3) ++
				(movesDiagonalCounterLeft' (board45 !! 12) player 12 2) ++
				(movesDiagonalCounterLeft' (board45 !! 13) player 13 1) ++
				(movesDiagonalCounterLeft' (board45 !! 14) player 14 0)
				where board45 = counter45 board

-- | Compiles a list of valid moves in a single diagonal looking up-leftwards
movesDiagonalCounterLeft'  :: [Cell] -> Player -> Int -> Int -> [(Int, Int)]
movesDiagonalCounterLeft' [] _ _ _ = []
movesDiagonalCounterLeft' [x] _ _ _ = []
movesDiagonalCounterLeft' row player rowNum pos
	| (valid && empty) == True = coord ++ next
	| (valid && empty) == False = next
	where valid = movesInLine (reverse (dropLast row)) player
	      coord = [ counter45Coord (pos, rowNum) ]
	      next = movesDiagonalCounterLeft' (dropLast row) player rowNum (pos-1)
	      empty = cell2Char  (row !! pos) == '_'
movesDiagonalCounterLeft' _ _ _ _ = []

-- | Compiles a list of valid moves looking down and right
movesDiagonalCounterRight  :: Board -> Player -> [(Int, Int)]
movesDiagonalCounterRight board player =(movesDiagonalCounterRight' (board45 !! 0) player 0 0) ++
				(movesDiagonalCounterRight' (board45 !! 1) player 1 0) ++
				(movesDiagonalCounterRight' (board45 !! 2) player 2 0) ++
				(movesDiagonalCounterRight' (board45 !! 3) player 3 0) ++
				(movesDiagonalCounterRight' (board45 !! 4) player 4 0) ++
				(movesDiagonalCounterRight' (board45 !! 5) player 5 0) ++
				(movesDiagonalCounterRight' (board45 !! 6) player 6 0) ++
				(movesDiagonalCounterRight' (board45 !! 7) player 7 0) ++
				(movesDiagonalCounterRight' (board45 !! 8) player 8 0) ++
				(movesDiagonalCounterRight' (board45 !! 9) player 9 0) ++
				(movesDiagonalCounterRight' (board45 !! 10) player 10 0) ++
				(movesDiagonalCounterRight' (board45 !! 11) player 11 0) ++
				(movesDiagonalCounterRight' (board45 !! 12) player 12 0) ++
				(movesDiagonalCounterRight' (board45 !! 13) player 13 0) ++
				(movesDiagonalCounterRight' (board45 !! 14) player 14 0)
				where board45 = counter45 board

-- | Compiles a list of valid moves in a single diagonal looking down-rightwards
movesDiagonalCounterRight'  :: [Cell] -> Player -> Int -> Int -> [(Int, Int)]
movesDiagonalCounterRight' [] _ _ _ = []
movesDiagonalCounterRight' [x] _ _ _ = []
movesDiagonalCounterRight' row player rowNum pos
	| (valid && empty) == True = coord ++ next
	| (valid && empty) == False = next
	where valid = movesInLine (tail row) player
	      coord = [ counter45Coord (pos, rowNum) ]
	      next = movesDiagonalCounterRight' (tail row) player rowNum (pos+1)
	      empty = cell2Char (head row) == '_'
movesDiagonalCounterRight' _ _ _ _ = []


-- | Given a "path" looking from a space to place a tile, check that this move 
--   would flip at least one enemy tile
movesInLine :: [Cell] -> Player -> Bool
-- ^Assumes that tile will be placed in -1
--i.e. give this the path from placed tile to edge of board, excluding the tile
movesInLine line player
	|hasAlly line player == True = allEnemy (frame line player) player
	|hasAlly line player == False = False

-- | Check if a grouping of cells is a line of enemy tiles
allEnemy :: [Cell] -> Player -> Bool
allEnemy [] _ = False
allEnemy [x] player
	| x == tile (invertPlayer player) = True
allEnemy (x:xs) player
	| x == tile (invertPlayer player) = allEnemy xs player
allEnemy _ _ = False

-- | Checks if a row contains at least one other allied tile to pair with
hasAlly :: [Cell] -> Player -> Bool
hasAlly [] _= False
hasAlly [x] player
	| x == tile player = True
hasAlly (x:xs) player
	| x == tile player = True
	| x /= tile player = hasAlly xs player
hasAlly _ _ = False

-- | Remove any spaces after the closest allied tile
frame :: [Cell] -> Player -> [Cell]
--Assumes that the tile will be placed in spot -1
--finds first ally in row and drops it and anything after
--Leaving the space between ally and proposed placement
frame [] _= []
frame (x:xs) player
	| x == tile player = []
	| x /= tile player = [x] ++ frame xs player























checkInput :: String -> Bool
checkInput a
		| a == "corners" = True
{-
		| a == "greedy" = True
		| a == "random" = True
		| a == "first" = True
		| a == "last" = True
-}
		| otherwise = False


stringToPlayer :: String -> Chooser
stringToPlayer a
		| a == "corners" = corners
{-
		| a == "greedy" = greedy
		| a == "random" = random
		| a == "first" = first
		| a == "last" = last
-}



main = do
{-
   !!!!USELESS REFERENCE CODE

   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName
-}


	args <- getArgs

	--These "getLine" statements need to be moved into the "if" statement below
	{-
	putStrLn "Choose Player 1 strategy"
	player1 <- getLine

	putStrLn "Choose player 2 strategy"
	player2 <- getLine
	-}


	if length (args) == 0
	then
		getInput
	else
		if length (args) == 2
		then
			if checkInput (args !! 0) && checkInput (args !! 1)
			then main' args
			else putStrLn "Invalid input"

		else putStrLn "Invalid input"



getInput :: IO()
getInput = do
	putStrLn "Choose Player 1 strategy"
	player1 <- getLine

	putStrLn "Choose player 2 strategy"
	player2 <- getLine

	if checkInput player1 && checkInput player2
	then main' ([player1] ++ [player2])
	else putStrLn "Invalid input"




main' :: [String] -> IO()
main' args = do

	putStrLn "\nThe initial board:"
	print initBoard

	playGame (stringToPlayer (args !! 0)) (stringToPlayer (args !! 1)) B




playGame :: Chooser -> Chooser -> Cell -> IO()
playGame black white c = do
	putStrLn "The board after one move :"
	let mv = if c == B
		then black (initBoard) B
		else white (initBoard) W
	   in case mv of
		Nothing -> putStrLn "Passed."
		(Just pt) -> putBoard $ replace2 (theBoard initBoard) pt B

