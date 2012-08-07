
import qualified Data.List as List
import Control.Monad.Error


data AST = One String |
           Many [AST] deriving (Eq, Show, Read, Ord)


char :: Char -> String -> Either String (AST, String)
char _ [] = Left "empty string"
char c (x:xs)
  | c == x = Right (One [x], xs)
  | otherwise = Left ("in 'char': wanted " ++ [c] ++ ", got " ++ [x])


pnot :: Char -> String -> Either String (AST, String)
pnot _ [] = Left "empty string"
pnot c (x:xs)
  | c /= x = Right (One [x], xs)
  | otherwise = Left "char *did* match"


zero_or_more :: (String -> Either String (AST, String)) -> String -> Either String (AST, String)
zero_or_more _ "" = Right(Many [], "") -- can probably remove this case
zero_or_more p xs = next (p xs)
  where
    next (Left mess) = Right (Many [], xs)
    next (Right (matched, rest)) = continue $ zero_or_more p rest
      where 
        continue (Left m) = Left m
        continue (Right (Many m2, r)) = Right (Many $ matched : m2, r)


one_or_more :: (String -> Either String (AST, String)) -> String -> Either String (AST, String)
one_or_more p xs = do
  (a1, rest1) <- p xs
  (Many a2, rest2) <- zero_or_more p rest1
  return (Many $ a1 : a2, rest2)


string :: String -> String -> Either String (AST, String)
string search input = whatever $ List.stripPrefix search input
  where
    whatever (Just x) = Right (One search, x)
    whatever Nothing = Left $ "couldn't match string " ++ search ++ ", " ++ input


pand :: (String -> Either String (AST, String)) -> (String -> Either String (AST, String)) -> String -> Either String (AST, String)
pand l r input = do
  (match1, rest1) <- l input
  (match2, rest2) <- r rest1
  return (Many [match1, match2], rest2)


por :: (String -> Either String (AST, String)) -> (String -> Either String (AST, String)) -> String -> Either String (AST, String)
por l r input = hmmmm $ l input
  where
    hmmmm (Left _) = r input
    hmmmm x = x


pany :: [String -> Either String (AST, String)] -> String -> Either String (AST, String)
pany [] = error . (++ "can not build 'any' parser out of empty list") -- could also use `Left` instead ... not sure which is better
pany [x] = x
pany (x:xs) = por x (pany xs)


-- copy implementation of `zero_or_more` to fix nesting problem?
pall :: [String -> Either String (AST, String)] -> String -> Either String (AST, String)
pall [] z = error ("can't have emply list of parsers for 'pall': " ++ z)
pall [x] z = do
  (m1, r1) <- x z
  return (Many [m1], r1)
pall [x,y] z = pand x y z
pall xs input = do
  (m1, r1) <- (head xs) input
  (Many m2, r2) <- pall (tail xs) r1
  return (Many $ m1 : m2, r2) 


-- example:  `lookahead (char 'c') ((== 'd') . head) "cdef"`
-- 
lookahead :: (String -> Either String (AST, String)) -> (String -> Bool) -> String -> Either String (AST, String)
lookahead p l xs = yes $ p xs
  where
    yes (Left m) = Left m
    yes (Right (match, rest)) 
      | l rest = Right (match, rest)
      | otherwise = Left $ "matched (" ++ (show match) ++ ") but failed lookahead on: " ++ rest


{-| what follows are domain-specific parsers -}

number :: String -> Either String (AST, String)
number = one_or_more $ pany $ map char ['0' .. '9']


float :: String -> Either String (AST, String)
float = pall [number, char '.', number]  `por`  number


capLetter :: String -> Either String (AST, String)
capLetter = pany $ map char ['A' .. 'Z']


space = pany $ map char " \t"


newline = pany $ map char "\n\r\f"


wschar = por space newline


ws = one_or_more space

-- shiftx file format

sxhead = pall (ws : (List.intersperse ws $ map string ["NUM", "RES", "HA", "H", "N", "CA", "CB", "C"]))

sxdashes = one_or_more $ pand ws dashes
  where
    dashes = one_or_more $ char '-'

sxline = pand ws $ pall $ List.intersperse ws parsers
  where
    parsers = number : capLetter : (replicate 6 float) 

shiftx :: String -> Either String (AST, String)
shiftx = pall [sxhead, zero_or_more ws, newline, sxdashes, zero_or_more ws, newline, one_or_more (pand sxline newline)]








