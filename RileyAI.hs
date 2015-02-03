--module RileyAI where 

import Data.List

-- | Takes gamestate and player colour and returns Maybe (int, int)
corners :: Chooser
corners gamestate cell
		| (elem (0,0) validMoves) = (0,0)
		| (elem (0,7) validMoves) = (0,7)
		| (elem (7,0) validMoves) = (7,0)
		| (elem (7,7) validMoves) = (7,7)
		| (safeZone validMoves) /= [] = (safeZone validMoves)
		| ((length validMoves) > 0) = validMoves !! 0
		| ((length validMoves) == 0) = Nothing
		where validMoves = (moves (theBoard gamestate) (playerOf cell))
		      safe = (((fst )> 0) && ((fst ) < 7) && ((snd ) > 0) && ((snd ) < 7))


safeZone :: [(Int, Int)] -> [(Int, Int)]
safeZone [] = []
safeZone (coord:left)
		| ((x > 0) && (x < 7) && (y > 0) && (y < 7)) = [coord] ++ (safeZone left)
		| True = (safeZone left)
		where x = fst coord
		      y = snd coord
