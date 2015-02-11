firstMoveChoice :: Chooser
firstMoveChoice board player = head (moves board (playerOf player))
