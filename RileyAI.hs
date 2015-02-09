--module RileyAI where 

import Data.List

-- | Takes gamestate and player colour and returns Maybe (int, int)
-- This AI prioritizes corners, tthen edges, then spaces not touching the edges, then the remaining
-- if nothing can be found it passes.
-- By Riley Lahd
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
