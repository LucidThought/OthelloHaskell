-- | Gets a cell in a row of cells
getCell :: [a] -> Int -> Cell
getCell xs n = xs !! n

-- | Gets a cell from the board
getCell2 :: [[a]] -> (Int,Int) -> Cell
getCell2 xs (x,y) = getCell (xs !! y) x
