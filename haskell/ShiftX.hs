module ShiftX (

    shiftx
  , ShiftX (..)
  , Line (..)

) where

import System.IO (openFile, hGetContents, IOMode(ReadMode))
import ParserCombinators


type Header = [String]

data Line = Line Integer Char [Float] deriving (Show)

data ShiftX = ShiftX Header [Line] deriving (Show)


--------------
-- more combinators

-- munch first discards any spaces --
--   that is, tabs or ' ' -- and
--   then runs its parser
munch :: Parser Char b -> Parser Char b
munch = ignoreLeft (some space)


-- blow is the same as separatedBy0 except
--   that it tosses the separators' results
blow :: Parser a b -> Parser a c -> Parser a [b]
blow p s = (using fst) $ separatedBy0 p s

-----------


int :: Parser Char String
int = some $ digit

float :: Parser Char Float
float = using (read . concat) $ pall [int, (string "."), int]


capLetter :: Parser Char Char
capLetter = pany $ map literal ['A' .. 'Z']

space :: Parser Char Char
space = pany $ map literal " \t"


newline :: Parser Char Char
newline = pany $ map literal "\n\r\f"

wschar :: Parser Char Char
wschar = alt space newline


ws :: Parser Char String
ws = some space

-- shiftx file format

sxhead :: Parser Char Header
sxhead = munch $ blow (some capLetter) ws
-- pall (ws : (List.intersperse ws $ map string ["NUM", "RES", "HA", "H", "N", "CA", "CB", "C"]))


dashes :: Parser Char ([String], [String])
dashes = munch $ separatedBy0 (some (literal '-')) ws


sxline :: Parser Char Line
sxline = pseq3 Line (munch integer) (munch capLetter) (munch $ blow float ws)


shiftx :: Parser Char ShiftX
shiftx = pseq2 ShiftX (ignoreRight sxhead newline) body
  where body = ignoreLeft (ignoreRight dashes newline) (blow sxline newline)


-- ---

eg1 = do
  h <- openFile "shiftx/shiftx.txt" ReadMode
  x <- hGetContents h
  return $ shiftx x
