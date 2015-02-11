lastMoveChoice :: Chooser
lastMoveChoice board player = last (moves board (playerOf player))
