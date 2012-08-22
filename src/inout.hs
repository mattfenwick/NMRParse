import System.IO
import ShiftX

parse1 = do
  h <- openFile "shiftx.txt" ReadMode
  x <- hGetContents h
  return $ shiftx x
