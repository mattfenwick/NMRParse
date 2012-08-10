import Data.List -- partition
import Control.Monad.Error -- for Monad instance of Either


-- -----------------------------------------------------------------
-- Tokens

data Token 
  = OpenParen
  | CloseParen
  | OpenSquare
  | CloseSquare
  | Whitespace String
  | Comment String
  | Integer Integer -- wow, that looks weird ... is the first one a constructor and the second one a type ???
  | Decimal (Integer, Integer)
  | String String -- again, weird
  | Symbol String
  deriving (Show)
  
  
mySplit :: (a -> Bool) -> [a] -> ([a], [a])
mySplit _ [] = ([], [])
mySplit f (x:xs)
  | f x = let (l, r) = mySplit f xs
          in (x:l, r)
  | otherwise = ([], x:xs)


nextToken :: String -> Either String (Token, String)
nextToken [] = Left "empty input"
nextToken ('(':rest) = Right (OpenParen, rest)
nextToken (')':rest) = Right (CloseParen, rest)
nextToken ('[':rest) = Right (OpenSquare, rest)
nextToken (']':rest) = Right (CloseSquare, rest)
nextToken str@(x:xs)
  | elem x ['0' .. '9'] = Right (Integer $ read ix, r1)
  | elem x " \t\r\f\n" = Right (Whitespace ws, r2)
  | x == ';' = Right (Comment c, r3)
  | x == '"' = Right (String s, tail r4)
  | elem x (['a' .. 'z'] ++ ['A' .. 'Z']) = Right (Symbol name, r5)
  | otherwise = Left $ "unable to match token from " ++ str
    where
      (ix, r1) = mySplit (flip elem ['0' .. '9']) str
      (ws, r2) = mySplit (flip elem " \t\r\f\n") str
      (c, r3) = mySplit (/= '\n') str
      (s, r4) = mySplit (/= '"') $ tail str
      (name, r5) = mySplit (flip elem (['a' .. 'z'] ++ ['A' .. 'Z'])) str
  
  
scanner :: String -> Either String ([Token], String)
scanner [] = Right ([], [])
scanner str = nextToken str >>= (\(t1, r1) -> 
      scanner r1 >>= (\(t2, r2) -> 
      return (t1 : t2, r2)))
          
      
-- ------------------------------------------------------------------------------
-- parsing combinators

type Failure = [Char]
type Parser a b = [a] -> Either Failure ([a], b)



-- always succeeds, consuming no input
succeed :: b -> Parser a b
succeed val inp = Right (inp, val)


-- always fails
pfail :: String -> Parser a b
pfail m _ = Left m


-- succeeds when predicate is true
satisfy :: (a -> Bool) -> Parser a a
satisfy _ [] = pfail "empty input to 'satisfy'" []
satisfy p (x:xs)
  | p x = succeed x xs
  | otherwise = pfail "'satisfy' predicate false" (x:xs)


-- match a token exactly
literal :: (Eq a) => a -> Parser a a
literal tok = satisfy (== tok)


-- match a parser and do something with its result
using :: (b -> c) -> Parser a b -> Parser a c
using f p inp = p inp >>= (\(rest, r) ->
  return (rest, f r))


-- match both parsers in sequence
pseq :: Parser a b -> Parser a c -> Parser a (b, c)
pseq l r inp = l inp >>= (\(toks1, res1) ->
  r toks1 >>= (\(toks2, res2) ->
  return (toks2, (res1, res2))))


-- match either one of two parsers
alt :: Parser a b -> Parser a b -> Parser a b
alt l r inp = tryLeft (l inp)
  where tryLeft (Left _) = r inp
        tryLeft x = x


-- match 0 or more of the parser
many :: Parser a b -> Parser a [b]
many p = alt (\inp -> p inp >>= (\(rest1, r1) -> 
  many p rest1 >>= (\(rest2, r2) ->
  return (rest2, r1 : r2))))
  (succeed [])
  -- `alt`

{-
many p inp = (do
  (rest1, r1) <- p inp
  (rest2, r2) <- many p rest1
  return (rest2, r1 : r2)) -- (succeed [])
-}    
    