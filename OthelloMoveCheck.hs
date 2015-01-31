{-
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.Random
import Data.List ((\\))
import System.Environment
import System.IO.Unsafe
import Data.Either

-} 
--import Othello

module OthelloMoveCheck where 

import OthelloTools
import Debug.Trace

moves :: Board -> Player -> [(Int, Int)]
--take board and player
--find all valid moves for this player, return in array as coords
moves board player = (movesDiagonalDown board player) ++ (movesDiagonalUp board player) ++ (movesVertical board player) ++ (movesHorizontal board player)

movesDiagonalDown :: Board -> Player -> [(Int, Int)]
movesDiagonalDown board player = (movesDiagonalDownLeft board player) ++ (movesDiagonalDownRight board player)

movesDiagonalUp :: Board -> Player -> [(Int, Int)]
movesDiagonalUp board player = (movesDiagonalUpLeft board player) ++ (movesDiagonalUpRight board player)

movesVertical  :: Board -> Player -> [(Int, Int)]
movesVertical board player = (movesVerticalDown board player) ++ (movesVerticalUp board player)

movesHorizontal  :: Board -> Player -> [(Int, Int)]
movesHorizontal board player = (movesHorizontalLeft board player) ++ (movesHorizontalRight board player)

-- | Compiles a list of valid moves (made valid by pieces of the returned spaces)
movesHorizontalLeft  :: [[Cell]] -> Player -> [(Int, Int)]
movesHorizontalLeft board player = (movesHorizontalLeft' (board !! 0) player 0) ++
				(movesHorizontalLeft' (board !! 1) player 1) ++
				(movesHorizontalLeft' (board !! 2) player 2) ++
				(movesHorizontalLeft' (board !! 3) player 3) ++
				(movesHorizontalLeft' (board !! 4) player 4) ++
				(movesHorizontalLeft' (board !! 5) player 5) ++
				(movesHorizontalLeft' (board !! 6) player 6) ++
				(movesHorizontalLeft' (board !! 7) player 7)

-- | Compiles a list of valid moves on a single row (made valid by pieces to the horizonal left of the returned spaces)
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

-- | Compiles a list of valid moves (made valid by pieces to the horizontal right of the returned spaces)
movesHorizontalRight  :: Board -> Player -> [(Int, Int)]
movesHorizontalRight board player = (movesHorizontalRight' (board !! 0) player 0) ++
				(movesHorizontalRight' (board !! 1) player 1) ++
				(movesHorizontalRight' (board !! 2) player 2) ++
				(movesHorizontalRight' (board !! 3) player 3) ++
				(movesHorizontalRight' (board !! 4) player 4) ++
				(movesHorizontalRight' (board !! 5) player 5) ++
				(movesHorizontalRight' (board !! 6) player 6) ++
				(movesHorizontalRight' (board !! 7) player 7)

-- | Compiles a list of valid moves in a single row (made valid by pieces to the right of the returned spaces)
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

movesVerticalUp  :: Board -> Player -> [(Int, Int)]
movesVerticalUp board player = (movesVerticalUp' (map (!! 0) board) player 0) ++
				(movesVerticalUp' (map (!! 1) board) player 1) ++
				(movesVerticalUp' (map (!! 2) board) player 2) ++
				(movesVerticalUp' (map (!! 3) board) player 3) ++
				(movesVerticalUp' (map (!! 4) board) player 4) ++
				(movesVerticalUp' (map (!! 5) board) player 5) ++
				(movesVerticalUp' (map (!! 6) board) player 6) ++
				(movesVerticalUp' (map (!! 7) board) player 7)

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

movesVerticalDown  :: Board -> Player -> [(Int, Int)]
movesVerticalDown board player =(movesVerticalDown' (map (!! 0) board) player 0) ++
				(movesVerticalDown' (map (!! 1) board) player 1) ++
				(movesVerticalDown' (map (!! 2) board) player 2) ++
				(movesVerticalDown' (map (!! 3) board) player 3) ++
				(movesVerticalDown' (map (!! 4) board) player 4) ++
				(movesVerticalDown' (map (!! 5) board) player 5) ++
				(movesVerticalDown' (map (!! 6) board) player 6) ++
				(movesVerticalDown' (map (!! 7) board) player 7)

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

movesDiagonalUpRight  :: Board -> Player -> [(Int, Int)]
movesDiagonalUpRight board player =(movesDiagonalUpRight' (map (!! 0) board) player 0) ++
				(movesDiagonalUpRight' (map (!! 1) board) player 1) ++
				(movesDiagonalUpRight' (map (!! 2) board) player 2) ++
				(movesDiagonalUpRight' (map (!! 3) board) player 3) ++
				(movesDiagonalUpRight' (map (!! 4) board) player 4) ++
				(movesDiagonalUpRight' (map (!! 5) board) player 5) ++
				(movesDiagonalUpRight' (map (!! 6) board) player 6) ++
				(movesDiagonalUpRight' (map (!! 7) board) player 7)

movesDiagonalUpLeft  :: Board -> Player -> [(Int, Int)]
movesDiagonalUpLeft _ _ = []

movesDiagonalDownRight  :: Board -> Player -> [(Int, Int)]
movesDiagonalDownRight _ _ = []

movesDiagonalDownLeft  :: Board -> Player -> [(Int, Int)]
movesDiagonalDownLeft _ _ = []

movesInLine :: [Cell] -> Player -> Bool
--Assumes that tile will be placed in -1
--i.e. give this the path from placed tile to edge of board, excluding the tile
movesInLine line player
	|hasAlly line player == True = allEnemy (frame line player) player
	|hasAlly line player == False = False

--coordMap :: (Int,Int) -> Cell
--takes coords and returns corresponding cell from board

allEnemy :: [Cell] -> Player -> Bool
--Check if a grouping of cells is a line of enemy tiles
allEnemy [] _ = False
allEnemy [x] player
	| x == tile (invertPlayer player) = True
allEnemy (x:xs) player
	| x == tile (invertPlayer player) = allEnemy xs player
allEnemy _ _ = False

hasAlly :: [Cell] -> Player -> Bool
--Checks if a row contains at least one other allied tile to pair with
hasAlly [] _= False
hasAlly [x] player
	| x == tile player = True
hasAlly (x:xs) player
	| x == tile player = True
	| x /= tile player = hasAlly xs player
hasAlly _ _ = False

frame :: [Cell] -> Player -> [Cell]
--Assumes that the tile will be placed in spot -1
--finds first ally in row and drops it and anything after
--Leaving the space between ally and proposed placement
frame [] _= []
frame (x:xs) player
	| x == tile player = []
	| x /= tile player = [x] ++ frame xs player

dropLast :: [a] -> [a]
--drops last element of list
dropLast x = reverse (drop 1 (reverse x))

-- | Given a 'Cell', return the coorespoinding 'Player'.
tile :: Player -> Cell
tile Black = B
tile White = W

	     --0  1  2  3  4  5  6  7
demoBoard = [ [E, E, E, E, E, E, E, E],	--0
	      [E, E, E, E, E, E, E, E], --1
              [E, E, E, E, E, E, E, E],	--2
              [E, E, E, W, B, E, E, E], --3
              [E, E, E, B, W, E, E, E], --4
              [E, E, E, E, E, E, E, E], --5
              [E, E, E, E, E, E, E, E], --6
              [E, E, E, E, E, E, E, E] ]--7
--Valid moves for B - (5,4) by HorzLeft, (4,5) by VertUp, (2,3) by HorzRight, (3,2) by VertDown
