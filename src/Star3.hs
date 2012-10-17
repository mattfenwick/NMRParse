module Star3 (

) where

import PCs2 -- arserCombinators
import System.IO
import System.Directory
import Data.List
import Control.Monad
  
  
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
-- sqstring = using Value $ ignoreLeft (literal '\'') $ ignoreRight (many $ pnot '\'') (literal '\'')
sqstring = using Value $ ignoreLeft sq $ ignoreRight content sq
  where sq = literal '\''
        content = many $ alt (pnot '\'') non_ending_sq
        non_ending_sq = ignoreRight sq (lookahead $ pnone " \t\n\r\f\v")


dqstring :: Parser Char Token
dqstring = using Value $ ignoreLeft (literal '"') $ ignoreRight (many $ pnot '"') (literal '"')


scstring :: Parser Char Token
scstring = using Value $ ignoreLeft (literal ';') $ ignoreRight chars endsequence
  where chars = many $ ignoreLeft (pnpnot endsequence) getOne
        endsequence = pseq (pany $ map literal "\n\r\f") (literal ';')


uq :: Parser Char Token
uq = using (\(x,y) -> Value (x:y)) $ pseq (pnone "\"#'_ \t\v\r\f\n") (many $ pnone " \t\v\r\f\n")


value :: Parser Char Token
value = pany [sqstring, dqstring, scstring, uq]


oneToken :: Parser Char Token
oneToken = pany [dataOpen, saveOpen, saveClose, loop, stop, value, whitespace, newline, comment, identifier]


scanner :: Parser Char [Token]
scanner = ignoreRight (many oneToken) end


myReadFile :: String -> IO String
myReadFile path = 
  openFile path ReadMode >>= hGetContents
  
  

test = myReadFile "bmrb2.1.txt" >>= \str -> return $ scanner (str, 0)


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
ident (Identifier x:rest, cts) = succeed (Identifier x) (rest, cts)
ident x = pfail ["identifier"] x


val :: Parser Token Token
val (Value v:rest, cts) = succeed (Value v) (rest, cts)
val x = pfail ["value"] x


saveme :: Parser Token Token
saveme (SaveOpen s:rest, cts) = succeed (SaveOpen s) (rest, cts)
saveme x = pfail ["save open"] x


datame :: Parser Token Token
datame (DataOpen s:rest, cts) = succeed (DataOpen s) (rest, cts)
datame x = pfail ["data open"] x


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
  
  
pStar :: Parser Token AST
pStar = using PStar $ ignoreRight pData end


parseMe :: Parser Token AST 
parseMe (tks, ct) = pStar ((filter notCommentOrWs tks), ct)
  where notCommentOrWs (Comment _) = False
        notCommentOrWs (Newline _) = False
        notCommentOrWs (Whitespace _) = False
        notCommentOrWs _ = True
  
  
-- testParse :: String -> Either (Failure Token) (([Token], Integer), AST)
testParse x = doTheParsing $ liftM snd (scanner (x, 0))
  where doTheParsing (Right x) = parseMe (x, 0)
        doTheParsing (Left x) = Left (["the tokenization failed"], ([], 0)) -- so that one file's failure doesn't kill everything
--        doTheParsing (Left (a, (b, c))) = error $ "tokenization failed: " ++ show (a, (take 200 b, c)) -- to get the context
  
  
test2 = liftM testParse $ myReadFile "bmrb2.1.txt"


test3 = liftM testParse $ myReadFile "bmrb3.0.txt"


hmm :: Show a => String -> Either (a, b) c -> String
hmm x (Right y) = x ++ "  Success!"
hmm x (Left z) = x ++ "  Failure ... " ++ (show $ fst z)


path = "../../../Desktop/nmr_software/new_bmrb_files/"
bigtest = getDirectoryContents path >>=
  (\x -> let them = filter (isPrefixOf "bmrb") x 
         in mapM (\y -> myReadFile (path ++ y) >>= 
                 (\z -> putStrLn $ hmm (y ++ " " ++ (show $ length z)) (testParse z))) them)
                 
                 
-- parseFile :: String -> IO (Either String ([Token], AST))
parseFile name = liftM testParse $ myReadFile (path ++ name)
  

