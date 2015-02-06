data Directions = Up
				| UpRight
				| UpLeft
				| Right
				| Left
				| Down
				| DownLeft
				| DownRight
				| ThisSpace
				
 Up :: (Int, Int) -> Board -> (Int, Int)
 Up ( _,y) board
			| y+1 > 7 == True = (-1,-1)
			| otherwise = (x, y+1)
			
 UpRight :: (Int, Int) -> Board -> (Int, Int)
 UpRight ( x,y) board
			| y+1 > 7 == True = (-1,-1)
			| x+1 > 7 == True = (-1,-1)
			| otherwise = (x+1, y+1)
			
 UpLeft :: (Int, Int) -> Board -> (Int, Int)
 UpLeft ( x,y) board
			| y+1 > 7 == True = (-1,-1)
			| x-1 < 0 == True = (-1,-1)
			| otherwise = (x-1, y+1)
			
 Right :: (Int, Int) -> Board -> (Int, Int)
 Right ( x,_) board
			| x+1 > 7 == True = (-1,-1)
			| otherwise = (x+1, y)
			
 Left :: (Int, Int) -> Board -> (Int, Int)
 Left ( x,_) board
			| x-1 < 0 == True = (-1,-1)
			| otherwise = (x-1, y)
			
 Down :: (Int, Int) -> Board -> (Int, Int)
 Down ( _,y) board
			| y-1 < 0 == True = (-1,-1)
			| otherwise = (x, y-1)
			
 DownRight :: (Int, Int) -> Board -> (Int, Int)
 DownRight ( x,y) board
			| y-1 < 0 == True = (-1,-1)
			| x+1 > 7 == True = (-1,-1)
			| otherwise = (x+1, y-1)
			
 DownLeft :: (Int, Int) -> Board -> (Int, Int)
 DownLeft ( x,y) board
			| y-1 < 0 == True = (-1,-1)
			| x-1 < 0 == True = (-1,-1)
			| otherwise = (x-1, y-1)

 ThisSpace :: (Int, Int) -> Board -> (Int, Int)
 ThisSpace ( x,y) board
			| y+1 > 7 == True = (-1,-1)
			| x+1 > 7 == True = (-1,-1)
			| y-1 < 0 == True = (-1,-1)
			| x-1 < 0 == True = (-1,-1)
			| otherwise = (x, y+1)






