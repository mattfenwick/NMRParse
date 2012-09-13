module Star3 (

) where

import ParserCombinators
import System.IO
import System.Directory
import Data.List
  
  
-- ------------------
-- the token parsers


data Token =
  DataOpen String 
  | SaveOpen String
  | SaveClose
  | Whitespace String
  | Newline String
  | Comment String
  | Loop
  | Stop
  | Identifier String
  | Value String
  deriving (Show, Eq)


comment :: Parser Char Token
comment = using Comment $ ignoreLeft (literal '#') (many $ pnone "\n\r\f")


dataOpen :: Parser Char Token
dataOpen = using DataOpen $ ignoreLeft (string "data_") (some $ pnone " \t\n\r\f\v")


saveOpen :: Parser Char Token
saveOpen = using SaveOpen $ ignoreLeft (string "save_") (some $ pnone " \t\n\r\f\v")


saveClose :: Parser Char Token
saveClose = preturn SaveClose $ string "save_"


whitespace :: Parser Char Token
whitespace = using Whitespace $ some $ pany $ map literal " \t\v"


newline :: Parser Char Token
newline = using Newline $ some $ pany $ map literal "\n\r\f"


stop :: Parser Char Token
stop = preturn Stop $ string "stop_"


loop :: Parser Char Token
loop = preturn Loop $ string "loop_"


identifier :: Parser Char Token
identifier = using Identifier $ ignoreLeft (literal '_') (some $ pnone " \t\n\r\f\v")


sqstring :: Parser Char Token
sqstring = using Value $ ignoreLeft (literal '\'') $ ignoreRight (many $ pnot '\'') (literal '\'')


dqstring :: Parser Char Token
dqstring = using Value $ ignoreLeft (literal '"') $ ignoreRight (many $ pnot '"') (literal '"')


scstring :: Parser Char Token
scstring = using Value $ ignoreLeft (literal ';') $ ignoreRight (many $ pnot ';') (literal ';')


sbstring :: Parser Char Token
sbstring = using Value $ ignoreLeft (literal '[') $ ignoreRight (many $ pnone "[]") (literal ']')
-- here's one possible way to allow balance [] inside:
--    sbstring = using Value theThing
--      where theThing = ignoreLeft (literal '[') $ ignoreRight content (literal ']')
--        content = using concat $ many $ alt (using (:[]) $ pnone "[]") theThing



uq :: Parser Char Token
uq = using (\(x,y) -> Value (x:y)) $ pseq (pnone "\"#'[]_ \t\v\r\f\n") (many $ pnone " \t\v\r\f\n")


value :: Parser Char Token
value = pany [sqstring, dqstring, scstring, sbstring, uq]


oneToken :: Parser Char Token
oneToken = pany [dataOpen, saveOpen, saveClose, loop, stop, value, whitespace, newline, comment, identifier]


scanner :: Parser Char [Token]
scanner = many oneToken


myReadFile :: String -> IO String
myReadFile path = 
  openFile path ReadMode >>= hGetContents
  
  
-- test :: IO (Parser Strin
test = myReadFile "bmrb2.1.txt" >>= (return . scanner)


-------------
-- the rest of the parsers

data AST
  = PLoop [Token] [Token]
  | PSave [AST]
  | PDatum Token Token
  | PData [AST]
  | PStar AST
  deriving (Show, Eq)


ident :: Parser Token Token
ident (Identifier x:rest) = succeed (Identifier x) rest
ident x = pfail "failed to get an identifier" x


val :: Parser Token Token
val (Value v:rest) = succeed (Value v) rest
val x = pfail "failed to get a value" x


saveme :: Parser Token Token
saveme (SaveOpen s:rest) = succeed (SaveOpen s) rest
saveme x = pfail "failed to get save open" x


datame :: Parser Token Token
datame (DataOpen s:rest) = succeed (DataOpen s) rest
datame x = pfail "failed to get data open" x


pLoop :: Parser Token AST
pLoop = using (uncurry PLoop) $ ignoreLeft (literal Loop) $ ignoreRight stuff (literal Stop)
  where stuff = pseq (many ident) (many val)
  
  
datum :: Parser Token AST
datum = using (uncurry PDatum) $ pseq ident val
  
  
pSave :: Parser Token AST
pSave = using PSave $ ignoreLeft saveme $ ignoreRight contents (literal SaveClose)
  where contents = some $ alt datum pLoop


pData :: Parser Token AST
pData = using PData $ ignoreLeft datame (some pSave)


end :: Parser Token ()
end [] = succeed () []
end xs = pfail "failed to match end of input" xs
  
  
pStar :: Parser Token AST
pStar = using PStar $ ignoreRight pData end


parseMe :: Parser Token AST 
parseMe = pStar . filter notCommentOrWs
  where notCommentOrWs (Comment _) = False
        notCommentOrWs (Newline _) = False
        notCommentOrWs (Whitespace _) = False
        notCommentOrWs _ = True
  
  
-- testParse :: String -> Either String AST
testParse x = scanner x >>= (return . snd) >>= parseMe
  
  
test2 = myReadFile "bmrb2.1.txt" >>= (return . testParse)


test3 = myReadFile "bmrb3.0.txt" >>= (return . testParse)


hmm :: String -> Either a b -> String
hmm x (Right y) = x ++ "  Success!"
hmm x (Left z) = x ++ "  Failure ..."


path = "../../../Desktop/nmr_software/new_bmrb_files/"
bigtest = getDirectoryContents path >>=
  (\x -> let them = filter (isPrefixOf "bmrb") x 
         in mapM (\y -> myReadFile (path ++ y) >>= 
                 (\z -> putStrLn $ hmm (y ++ " " ++ (show $ length z)) (testParse z))) them)

