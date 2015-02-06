--Determines if line should be counted
lineCounts :: (Int, Int) -> Board -> Player -> Directions -> Int
lineCounts (x,y) board player direction
			|hasAlly (getList (x,y) board direction) player == True = score' ((x,y) board player direction)
			|otherwise = 0


--Takes a cell and a direction to create an arry from that cell to the edge of the board in the given direction
getList :: (Int, Int) -> Board -> Directions -> [Cell]
getList ( x,y) board direction
			| direction (x, y) == (-1,-1) = []
			| otherwise = getCell2 (x,y) : getList (direction(x, y))

--Given a cell coordinate and play, assigns a points value if the tile is of the oposite colour
cellValue :: (Int, Int) -> Player -> [[a]] -> Int
cellValue (x,y) player board 

			|getCell2 board (x, y) == tile (invertPlayer (player)) = 1
			|getCell2 board (x, y) /= tile (invertPlayer (player)) = 0


--Gives the potential score for each potential move
score :: (Int, Int) -> Board -> Player -> Int
score (x, y) player = lineCounts (x, y+1) board player Up 
		    + lineCounts (x+1, y+1) board player UpRight 
		    + lineCounts (x-1, y+1) board player UpLeft
		    + lineCounts (x+1, y) board player Right 
		    + lineCounts (x-1, y) board player Left
		    + lineCounts (x, y-1) board player Down
		    + lineCounts (x+1, y-1) board player DownRight
		    + lineCounts (x-1, y-1) board player DownLeft 

--Gets the score for a potential move in a particular direction
score' :: (Int, Int) -> [[a]] -> Player -> Directions -> Int
score' (x, y) board player 
			| ThisSpace (x,y) == (-1,-1) = 0
			| direction (x,y) == (-1,-1) = 0
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (score' (direction (x, y)) board player)


--Picks the potential move with the highest points value
greedyPick :: Chooser
greedyPick [] _ _ = Nothing
greedyPick ( x:[] ) board cell = x
greedyPick ( x:xs) board cell = max (score (x board playerOf (cell)) score (xs board playerOf (cell)))


