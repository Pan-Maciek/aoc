import Data.List
import System.Environment
import System.IO

-- Board
data Board = Board { score :: Int, rows :: [Int], cols :: [Int], board :: [Int] } deriving (Show, Eq)

createBoard :: [Int] -> Board
createBoard board = Board (sum board) [0,0,0,0,0] [0,0,0,0,0] board

isWin :: Board -> Bool
isWin (Board _ rows cols _) = any (==5) rows || any (==5) cols

markBoard :: Int -> Board -> Board
markBoard n (Board score rows cols board) = 
  case boardLookUp n of
    Nothing -> Board score rows cols board
    Just(row, col) -> Board (score - n) (incAt row rows) (incAt col cols) board
  where
    boardLookUp n = idx2pos <$> findIndex (==n) board 
    idx2pos idx = (idx `div` 5, idx `mod` 5)

-- Utility functiosn
incAt :: Num a => Int -> [a] -> [a]
incAt n xs = as ++ (b+1:bs) where
  (as, (b:bs)) = splitAt n xs

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c xs = case break (==c) xs of 
  (ls, []) -> [ls]
  (ls, x:rs) -> ls : splitOn c rs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n xs' where 
  xs' = drop n xs

-- Input parsing
parseInput :: String -> ([Int], [Board])
parseInput z = (rng, boards) where
  (rng_string, boards_string) = break (== '\n') z
  rng = map read . (splitOn ',') $ rng_string
  boards_numbers = map read . words $ boards_string
  boards = map createBoard . chunksOf 25 $ boards_numbers

-- Solution logic
findWinningBoard :: ([Int], [Board]) -> (Board, Int)
findWinningBoard (x:xs, boards) =
  case find isWin newBoards of
    Just board -> (board, x)
    Nothing -> findWinningBoard (xs, newBoards) 
  where newBoards = map (markBoard x) boards
  
run [file] = do
  text <- readFile file
  let (board, lastNumber) = findWinningBoard (parseInput text)
  print $ score board * lastNumber

run _ = putStrLn "Usage: ./one.out <input>"

main = do
  args <- getArgs
  run args