getList :: (Int, Int) -> Board -> Directions -> [Cell]
getList ( x,y) board direction
			| direction (x, y) == (-1,-1) = []
			| otherwise = getCell (x,y) : getList (direction(x, y))

cellValue :: (Int, Int) -> Player -> [[a]] -> Int
cellValue (x,y) player board 

			|getCell2 board (x, y) == tile (invertPlayer (player)) = 1
			|getCell2 board (x, y) /= tile (invertPlayer (player)) = 0



score :: (Int, Int) -> Board -> Player -> Int
score (x, y) player = score' (x, y+1) board player Up 
		    + score' (x+1, y+1) board player UpRight 
		    + score' (x-1, y+1) board player UpLeft
		    + score' (x+1, y) board player Right 
		    + score' (x-1, y) board player Left
		    + score' (x, y-1) board player Down
		    + score' (x+1, y-1) board player DownRight
		    + score' (x-1, y-1) board player DownLeft 
		
score' :: (Int, Int) -> [[a]] -> Player -> Directions -> Int
score' (x, y) board player 
			| ThisSpace (x,y) == (-1,-1) = 0
			| direction (x,y) == (-1,-1) = 0
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (score' (direction (x, y)) board player)


greedyPick :: Chooser
greedyPick [] _ _ = Nothing
greedyPick ( x:[] ) board cell = x
greedyPick ( x:xs) board cell = max (score (x board playerOf (cell)) score (xs board playerOf (cell)))


