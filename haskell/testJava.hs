import Java
import System.IO


myReadFile :: String -> IO String
myReadFile path = 
  openFile path ReadMode >>= hGetContents
  
  
-- test :: IO (Parser Strin
test = myReadFile "ex2.java" >>= (return . scanner)





