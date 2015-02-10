checkInput :: String -> Bool
checkInput a
		| a == "corners" = True
{-
		| a == "greedy" = True
		| a == "random" = True
		| a == "first" = True
		| a == "last" = True
-}
		| otherwise = False


stringToPlayer :: String -> Chooser
stringToPlayer a
		| a == "corners" = corners
{-
		| a == "greedy" = greedy
		| a == "random" = random
		| a == "first" = first
		| a == "last" = last
-}



main = do
{-
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName
-}


	args <- getArgs




	--These "getLine" statements need to be moved into the "if" statement below
	putStrLn "Choose Player 1 strategy"
	player1 <- getLine

	putStrLn "Choose player 2 strategy"
	player2 <- getLine



	if length (args) == 0
	then
		{-
		putStrLn "Choose Player 1 strategy"
		player1 <- getLine

		putStrLn "Choose player 2 strategy"
		player2 <- getLine
		-}

		if checkInput player1 && checkInput player2
		then main' ([player1] ++ [player2])
		else putStrLn "Invalid input"
	else
		if length (args) == 2
		then
			if checkInput (args !! 0) && checkInput (args !! 1)
			then main' args
			else putStrLn "Invalid input"

		else putStrLn "Invalid input"




main' :: [String] -> IO()
main' args = do

	putStrLn "\nThe initial board:"
	print initBoard

	playGame' (stringToPlayer (args !! 0)) (stringToPlayer (args !! 1)) B




playGame' :: Chooser -> Chooser -> Cell -> IO()
playGame' black white c = do
	putStrLn "The board after one move :"
	let mv = if c == B
		then black (initBoard) B
		else white (initBoard) W
	   in case mv of
		Nothing -> putStrLn "Passed."
		(Just pt) -> putBoard $ replace2 (theBoard initBoard) pt B

