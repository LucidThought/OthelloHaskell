isValid :: Board -> Player -> (Int, Int) -> Int -> Int
isValidUp board player (x,y)
			| hasAlly (getListUp (x,y) board) player == True = scoreUp (x,y) board player
			| otherwise = 0
			

getList :: (Int, Int) -> Board -> Directions -> [Cell]
getList ( x,y) board direction
			| direction (x, y) == Nothing = []
			| otherwise = getCell (x,y) : getList (direction(x, y))
