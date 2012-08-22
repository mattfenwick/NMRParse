module MyCombinators (
      succeed
    , pfail
    , satisfy
    , pseq
    , alt
    , using
    , literal
    , many
    , preturn
    , pany
    , pall
    , some
    , pnot

    , Parser
 
    , digit
    , integer
    , alpha
    , wschar
) where

import Control.Monad.Error -- do I need this?

          
      
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
many p = (using cons parser) `alt` (succeed [])
  where cons = uncurry (:)
        parser = pseq p $ many p
        
        
-- match 1 or more of the parser
some :: Parser a b -> Parser a [b]
some p = using cons $ pseq p $ many p
  where cons = uncurry (:)
  
  
-- match any of the parsers
pany :: [Parser a b] -> Parser a b
pany = foldr alt (pfail "'pany' found no matching alternative")


-- throw away result of parser
preturn :: c -> Parser a b -> Parser a c
preturn v p = using (const v) p
  
  
-- matches all of the parsers in sequence
pall :: [Parser a b] -> Parser a [b]
pall = foldr (\p b -> using cons (pseq p b)) (succeed [])
  where cons = uncurry (:)
  
  
-- matches end of input
end :: Parser a ()
end [] = succeed () []
end ip = pfail "not end of input" ip


-- ------------------------------
-- other interesting combinators 

    
separatedByOne :: Parser a b -> Parser a c -> Parser a ([b], [c])
separatedByOne p s = using f $ pseq p (using unzip $ many $ pseq s p)
  where f (fp, (oss, ops)) = (fp:ops, oss)
  

-- always succeeds
separatedBy0 :: Parser a b -> Parser a c -> Parser a ([b], [c])
separatedBy0 p s = (separatedByOne p s) `alt` succeed ([], [])


-- changes error message if parser fails
message :: Failure -> Parser a b -> Parser a b
message m p inp = try $ p inp
  where try (Left _) = Left m
        try x = x

-- ---------------------------------------------------
-- experiments


-- matches 0 or 1 of the parser
--   this appears to be really dumb:
--   the whole returning v thing if
--   it fails -- how are you supposed
--   to distinguish that from a real
--   return value?
optional :: Parser a b -> b -> Parser a b
optional p v = p `alt` (succeed v)


-- succeed but consume no input if parser succeeds
--   this is a very weird combinator that
--   I don't feel comfortable with it
lookahead :: Parser a b -> Parser a ()
lookahead p inp = tryIt $ p inp
  where tryIt (Right _) = succeed () inp
        tryIt (Left _) = pfail "'lookahead' failed" inp


-- doesn't match a parser, consumes no input
pnpnot :: Parser a b -> Parser a ()
pnpnot p inp = tryIt $ p inp
  where tryIt (Left _) = succeed () inp
        tryIt _ = pfail "'pnpnot' planned failure" inp
        
        
-- doesn't match any parser, consumes no input
pnpnone :: [Parser a b] -> Parser a ()
pnpnone = preturn () . pall . map pnpnot



    
    
checkParser :: (b -> Bool) -> Parser a b -> Parser a b
checkParser f p inp = p inp >>= (\(rest, result) ->
  if f result then return (rest, result) else pfail "parser check failed" [])


separatedBy1 :: Parser a b -> Parser a c -> Parser a ([b], [c])
separatedBy1 p s = checkParser ((> 0) . length . fst) $ separatedBy0 p s


-- ---------------------------------------------------
-- parsing fun

digits :: [Parser Char Char]
digits = map literal ['0'..'9']

digit :: Parser Char Char
digit = pany digits

integer :: Parser Char Integer
integer = using read $ some digit

alpha :: Parser Char Char
alpha = pany $ map literal (['a' .. 'z'] ++ ['A' .. 'Z'])

wschar :: Parser Char Char
wschar = pany $ map literal " \t\n\r\f"

-- matches all of the tokens in sequence
string :: (Eq a) => [a] -> Parser a [a]
string = pall . map literal


-- matches if next token is not x
pnot :: (Eq a) => a -> Parser a a
pnot x = satisfy (/= x)


-- matches if next token is not in xs
--   not sure if I like this one
pnone :: (Eq a) => [a] -> Parser a a
pnone xs = satisfy (\x -> not $ elem x xs)