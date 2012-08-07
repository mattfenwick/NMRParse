import System.IO

do {
  h <- openFile "shiftx.txt" ReadMode
  x <- hGetContents h
  putStrLn $ show $ shiftx x
}