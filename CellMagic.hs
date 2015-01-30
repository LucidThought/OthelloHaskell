getCell :: [a] -> Int -> Cell
getCell xs n = xs !! n

getCell2 :: [[a]] -> (Int,Int) -> Cell
getCell2 xs (x,y) = getCell (xs !! y) x
