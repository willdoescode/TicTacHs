module Main where

import Data.Foldable
import Data.Sequence hiding (replicate)
import System.Exit (exitSuccess)

type Board = [[Move]]

type Row = [Move]

data Move = X | O | Blank
  deriving (Eq)

data Play = Play
  { current :: Move,
    board :: Board
  }
  deriving (Show)

instance Show Move where
  show Blank = " "
  show X = "X"
  show O = "O"

showRow :: Row -> String
showRow row =
  show (head row)
    ++ " | "
    ++ show (row !! 1)
    ++ " | "
    ++ show (row !! 2)
    ++ "\n"
    ++ "-   -   -"
    ++ "\n"

showBoard :: Board -> String
showBoard = foldl (\s row -> s ++ showRow row) ""

checkRow :: Row -> Bool
checkRow row
  | all (== X) row = True
  | all (== O) row = True
  | otherwise = False

checkWin :: Board -> Bool
checkWin [[a, b, c], [d, e, f], [g, h, i]] =
  or
    [ checkRow [a, b, c],
      checkRow [d, e, f],
      checkRow [g, h, i],
      checkRow [a, d, g],
      checkRow [b, e, h],
      checkRow [c, f, i],
      checkRow [c, e, g],
      checkRow [a, e, i]
    ]

flatten :: [[a]] -> [a]
flatten arr = [y | x <- arr, y <- x]

checkTie :: Board -> Bool
checkTie board = all (== False) $ flatten $ map (map (== Blank)) board

newBoard :: Board
newBoard = replicate 3 $ replicate 3 Blank

replace :: Int -> a -> [a] -> [a]
replace index el lst = toList $ update index el $ fromList lst

changeElem :: (Int, Int) -> Move -> Board -> Board
changeElem (x, y) move board =
  let row = board !! x
      newRow = replace y move row
   in replace x newRow board

alternatePlayer :: Move -> Move
alternatePlayer X = O
alternatePlayer O = X
alternatePlayer Blank = X

runGame :: Play -> IO ()
runGame Play {current = curr, board = b} = do
  putStrLn "\n"
  putStr $ showBoard b

  putStrLn $ "Current Player: " ++ show curr
  putStrLn "Enter your move row> "
  row <- getLine
  putStrLn "Enter your move row> "
  col <- getLine
  let cell = (read (head . words $ row) - 1 :: Int, read (head . words $ col) - 1 :: Int)
  let newBoard = changeElem cell curr b

  if checkWin newBoard
    then do
      putStrLn $
        show curr
          ++ " Has won!"
      exitSuccess
    else
      if checkTie newBoard
        then do
          putStrLn "There was a tie :("
          exitSuccess
        else runGame Play {current = alternatePlayer curr, board = newBoard}

main :: IO ()
main = runGame Play {current = X, board = newBoard}
