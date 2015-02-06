score :: (Int, Int) -> Player -> Int
score (x, y) player = scoreUp (x, y+1) board player + scoreUpRight (x+1, y+1) board player 
		+ scoreUpLeft (x-1, y+1) board player	+ scoreRight (x+1, y) board player 
		+ scoreLeft(x-1, y) board player + scoreDown (x, y-1) board player
		+ scoreDownRight (x+1, y-1) board player + scoreDownLeft (x-1, y-1) board player
		
scoreUp :: (Int, Int) -> [[a]] -> Player -> Int
scoreUp (x, y) board player 
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (scoreUp (x, y+1) board player)

scoreUpRight :: (Int, Int) -> [[a]] -> Player -> Int
scoreUpRight (x, y) board player 
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (scoreUpRight (x+1, y+1) board player)

scoreUpLeft :: (Int, Int) -> [[a]] -> Player -> Int
scoreUpLeft (x, y) board player 
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (scoreUpLeft (x-1, y+1) board player)
			
scoreRight :: (Int, Int) -> [[a]] -> Player -> Int
scoreRight (x, y) board player 
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (scoreRight (x+1, y) board player)
			
scoreLeft :: (Int, Int) -> [[a]] -> Player -> Int
scoreLeft (x, y) board player 
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (scoreLeft (x-1, y) board player)
			
scoreDown :: (Int, Int) -> [[a]] -> Player -> Int
scoreDown (x, y) board player 
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (scoreDown (x, y-1) board player)
			
scoreDownRight :: (Int, Int) -> [[a]] -> Player -> Int
scoreDownRight (x, y) board player 
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (scoreDownRight (x+1, y-1) board player)

scoreDownLeft :: (Int, Int) -> [[a]] -> Player -> Int
scoreDownLeft (x, y) board player 
			|(cellValue (x, y) player board) == 0 = 0
			|(cellValue (x, y) player board) == 1 = 1 + (scoreDownRight (x-1, y-1) board player)
			
cellValue :: (Int, Int) -> Player -> [[a]] -> Int
cellValue (x,y) player board 
			|getCell2 board (x, y) == tile (invertPlayer (player)) = 1
			|getCell2 board (x, y) /= tile (invertPlayer (player)) = 0
