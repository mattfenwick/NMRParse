module ShiftX (

    shiftx
  , ShiftX (..)
  , Line (..)

) where

import System.IO (openFile, hGetContents, IOMode(ReadMode))
import MParse
import Classes
import Instances
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)


type Header = [String]

data Line = Line Integer Char [Float] deriving (Show)

data ShiftX = ShiftX Header [Line] deriving (Show)


--------------
-- more combinators

-- munch first discards any spaces --
--   that is, tabs or ' ' -- and
--   then runs its parser
munch :: Parser Char b -> Parser Char b
munch = (*>) (some space)


-- blow is the same as separatedBy0 except
--   that it tosses the separators' results
blow :: Parser a b -> Parser a c -> Parser a [b]
blow p s = fmap fst (sepBy0 p s)

-----------


int :: Parser Char String
int = some $ mconcat $ map literal ['0' .. '9']

float :: Parser Char Float
float = fmap (read . concat) $ commute [int, (string "."), int]


capLetter :: Parser Char Char
capLetter = mconcat $ map literal ['A' .. 'Z']

space :: Parser Char Char
space = satisfy (flip elem " \t")


newline :: Parser Char Char
newline = satisfy (flip elem "\n\r\f")

wschar :: Parser Char Char
wschar = space <|> newline


ws :: Parser Char String
ws = some space

-- shiftx file format

sxhead :: Parser Char Header
sxhead = munch $ blow (some capLetter) ws
-- pall (ws : (List.intersperse ws $ map string ["NUM", "RES", "HA", "H", "N", "CA", "CB", "C"]))


dashes :: Parser Char ([String], [String])
dashes = munch $ sepBy0 (some (literal '-')) ws


sxline :: Parser Char Line
sxline = fmap Line (fmap read $ munch int) <*> (munch capLetter) <*> (munch $ blow float ws)


shiftx :: Parser Char ShiftX
shiftx = fmap ShiftX (sxhead <* newline) <*> body
  where body = (dashes <* newline) *> (blow sxline newline)


-- ---

eg1 = do
  h <- openFile "shiftx/shiftx.txt" ReadMode
  x <- hGetContents h
  return $ getParser shiftx x
