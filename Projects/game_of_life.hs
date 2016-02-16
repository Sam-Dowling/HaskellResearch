import Data.List
import Control.Monad

type Cell = (Int, Int)
type Grid = [Cell]

neighbours :: Cell -> Grid
neighbours (x, y) = do
  dx <- [-1..1]
  dy <- [-1..1]
  guard (dx /= 0 || dy /= 0)
  return (x + dx, y + dy)

step :: Grid -> Grid
step cells = do
  (newCell, n) <- frequencies $ concatMap neighbours cells
  guard $ (n == 3) || (n == 2 && newCell `elem` cells)
  return newCell

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies xs = do
  x <- group $ sort xs
  return (head x, length x)


formatGrid :: Grid -> String
formatGrid grid = do
  y <- ys
  x <- xs
  [marker x y] ++ eol x
  where
    marker x y
      | (x, y) `elem` grid = '*'
      | otherwise          = ' '
    eol x
      | x == maximum xs = ['\n']
      | otherwise       = []

    xs = gridRange fst
    ys = gridRange snd
    gridRange f = [min grid .. max grid]
      where
        min = minimum . map f
        max = maximum . map f

main = do
  mapM_ printGrid . take 100 $ iterate step row
  where
    beacon = [(0, 0), (1, 0), (0, 1), (3, 3), (2, 3), (3, 2)]
    toad = [(2,2),(3,2),(4,2),(1,3),(2,3),(3,3)]
    glider = [(0,0),(2,0),(1,1),(2,1),(1,2)]
    row = [(x,0) | x <- [0..7]]    
       ++ [(x,0) | x <- [9..13]]
       ++ [(x,0) | x <- [17..19]]
       ++ [(x,0) | x <- [26..32]]  
       ++ [(x,0) | x <- [34..38]]

    printGrid :: Grid -> IO ()
    printGrid grid = do
      putStrLn $ formatGrid grid
      putStrLn ""
