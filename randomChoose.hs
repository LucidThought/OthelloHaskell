--NOT TESTED, random number equation may need to be more random

random :: Chooser
random gamestate cell
		| validMoves == [] = Nothing
		| otherwise = validMoves !! randomNum
		where validMoves = (moves (theBoard gamestate) (playerOf cell))
		      randomNum = mod (67 * ((sum(map fst validMoves)) * sum(map snd validMoves))) length validMoves
