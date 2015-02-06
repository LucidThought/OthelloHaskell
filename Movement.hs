data Directions = Up
				| UpRight
				| UpLeft
				| Right
				| Left
				| Down
				| DownLeft
				| DownRight 
				
 Up :: (Int, Int) -> Board -> Maybe (Int, Int)
 Up ( _,y) board
			| y+1 > 7 == True = Nothing
			| otherwise = Just (x, y+1)
			
 UpRight :: (Int, Int) -> Board -> Maybe (Int, Int)
 UpRight ( x,y) board
			| y+1 > 7 == True = Nothing
			| x+1 > 7 == True = Nothing
			| otherwise = Just(x+1, y+1)
			
 UpLeft :: (Int, Int) -> Board -> Maybe (Int, Int)
 UpLeft ( x,y) board
			| y+1 > 7 == True = Nothing
			| x-1 < 0 == True = Nothing
			| otherwise = Just(x-1, y+1)
			
 Right :: (Int, Int) -> Board -> Maybe (Int, Int)
 Right ( x,_) board
			| x+1 > 7 == True = Nothing
			| otherwise = Just(x+1, y)
			
 Left :: (Int, Int) -> Board -> Maybe (Int, Int)
 Left ( x,_) board
			| x-1 < 0 == True = Nothing
			| otherwise = Just(x-1, y)
			
 Down :: (Int, Int) -> Board -> Maybe (Int, Int)
 Down ( _,y) board
			| y-1 < 0 == True = Nothing
			| otherwise = Just(x, y-1)
			
 DownRight :: (Int, Int) -> Board -> Maybe (Int, Int)
 DownRight ( x,y) board
			| y-1 < 0 == True = Nothing
			| x+1 > 7 == True = Nothing
			| otherwise = Just(x+1, y-1)
			
 DownLeft :: (Int, Int) -> Board -> Maybe (Int, Int)
 DownLeft ( x,y) board
			| y-1 < 0 == True = Nothing
			| x-1 < 0 == True = Nothing
			| otherwise = Just(x-1, y-1)