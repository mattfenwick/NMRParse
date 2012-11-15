import MParse (getParser)
import Java
import System.IO


myReadFile :: String -> IO String
myReadFile path = 
  openFile path ReadMode >>= hGetContents
  
  
-- test :: IO (Parser Strin
test = myReadFile "java/ex2.java" >>= (return . getParser scanner)





