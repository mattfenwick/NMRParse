module PCs4 (

      succeed
    , pfail
    , satisfy
    , pseq
    , alt
    , using
    , usingFail
    , literal
    , many
    , preturn
    , pany
    , pall
    , some
    , pnone
    , pnot
    , end

    , Parser
    , (<|>)
    
    , digit
    , integer
    , alpha
    , wschar
    
    -- a bit more obscure, less general
    , string
    , separatedBy0
    , separatedBy1
    , ignoreLeft
    , ignoreRight
    , not0
    , not1
    , getOne
    , message
    , optional
    , lookahead

) where


-- using own Monad instance because the bundled one
--   forces 'a' to be an Error of some kind
instance Monad (Either a) where
  return = Right
  fail = error "don't use fail with this monad instance for (Either a)"
  (Right r) >>= f = f r
  (Left l) >>= _ = Left l
          
      
-- ------------------------------------------------------------------------------
-- parsing combinators

type Failure a = ([String], ([a], Integer)) -- alternatives, remaining input, # of tokens consumed


-- a: token type; b: result type; Integer: # of tokens consumed
type Parser a b = ([a], Integer) -> Either (Failure a) (([a], Integer), b)



-- always succeeds, consuming no input
succeed :: b -> Parser a b
succeed val inp = Right (inp, val)


-- always fails
pfail :: [String] -> Parser a b
pfail as z = Left (as, z)


-- succeeds when predicate is true
satisfy :: (a -> Bool) -> Parser a a
satisfy _ ([], z) = pfail [] ([], z)
satisfy p ((x:xs), cns)
  | p x = succeed x (xs, cns + 1)
  | otherwise = pfail ["'satisfy' predicate false"] ((x:xs), cns)
  
  
-- this is sort of like 'bind' from the Hutton/Meijer 1996 paper
(<|>) :: Parser a b -> (b -> Parser a c) -> Parser a c
p <|> f = \inp -> p inp >>= 
  \(ctxt, val) -> (f val) ctxt
  
  
-- succeeds, consuming one 'token', as
--   long as input is not empty
getOne :: Parser a a
getOne = satisfy (const True)


-- match a token exactly
literal :: (Eq a, Show a) => a -> Parser a a
literal tok = message (show tok) $ satisfy (== tok)


-- match both parsers in sequence
pseq :: Parser a b -> Parser a c -> Parser a (b, c)
pseq l r = l <|>
  \lval -> r <|>
  \rval -> succeed (lval, rval)


-- match either one of two parsers
alt :: Parser a b -> Parser a b -> Parser a b
alt l r inp = tryLeft (l inp)
  where tryLeft (Left (xs, _)) = addAlts xs r inp
        tryLeft x = x


-- changes error message if parser fails
message :: String -> Parser a b -> Parser a b
message m p inp = tryIt (p inp)
  where tryIt (Left (_, x)) = pfail [m] x
        tryIt y = y


addAlt :: String -> Parser a b -> Parser a b
addAlt a p inp = hmm (p inp)
  where hmm (Left (as, x)) = Left ((a:as), x)
        hmm x = x
        
        
addAlts :: [String] -> Parser a b -> Parser a b
addAlts as p inp = hmm (p inp)
  where hmm (Left (oass, x)) = Left (oass ++ as, x)
        hmm x = x
        

-- match a parser and do something with its result
using :: (b -> c) -> Parser a b -> Parser a c
using f p = p <|> 
  \r -> succeed (f r)
  
  
-- match both parsers in sequence, and return 
--   the value of the second parser
ignoreLeft :: Parser a b -> Parser a c -> Parser a c
ignoreLeft l r = l <|> 
  \_ -> r <|>
  \rval -> succeed rval


-- match both parsers in sequence, and return
--   the value of the first parser
ignoreRight :: Parser a b -> Parser a c -> Parser a b
ignoreRight l r = l <|> 
  \lval -> r <|>
  \_  -> succeed lval


-- match parser 0 or more times
--   couldn't this also be accomplished with a fold?
many :: Parser a b -> Parser a [b]
many p = matcher `alt` (succeed [])
  where matcher = p <|>
          \v  -> (many p) <|>
          \vs -> succeed (v:vs)
        
        
-- match parser 1 or more times
some :: Parser a b -> Parser a [b]
some p = p <|>
  \v -> (many p) <|>
  \vs -> succeed (v:vs)
  
  
-- match any of the parsers
pany :: [Parser a b] -> Parser a b
pany = foldr alt (pfail [])


-- throw away result of parser
preturn :: c -> Parser a b -> Parser a c
preturn v p = using (const v) p
  
  
-- matches all of the parsers in sequence
--   I really wish I understood this code (that I just wrote)
pall = foldr f (succeed [])
  where 
    f p b =  p <|>
      \v  -> b <|>
      \vs -> succeed (v:vs)
  
  
-- matches end of input
end :: Parser a ()
end ([], x) = succeed () ([], x)
end ip = pfail ["end of input"] ip
  
  
-- allow a function applied to the result
--   cause the parser to fail
usingFail :: (b -> Either (Failure a) c) -> Parser a b -> Parser a c
usingFail f p inp = p inp >>= 
  \(rest, r) -> f r >>=
  \c -> return (rest, c)


-- ------------------------------
-- other interesting combinators 

    
separatedByOne :: Parser a b -> Parser a c -> Parser a ([b], [c])
separatedByOne p s = p <|>
  \v1 -> (many $ pseq s p) <|>
  \rs -> let (ss, vs) = unzip rs 
         in succeed (v1:vs, ss)
  

-- always succeeds
separatedBy0 :: Parser a b -> Parser a c -> Parser a ([b], [c])
separatedBy0 p s = (separatedByOne p s) `alt` succeed ([], [])


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


-- succeed but consume no input if 'p' succeeds
--   this is a very weird combinator and
--   I don't feel comfortable with it
lookahead :: Parser a b -> Parser a ()
lookahead p inp = tryIt $ p inp
  where tryIt (Right _) = succeed () inp
        tryIt (Left _) = pfail ["'lookahead' predicate"] inp


-- succeeds if 'p' doesn't succeed
--   consumes no input
not0 :: Parser a b -> Parser a ()
not0 p inp = tryIt $ p inp
  where tryIt (Left _) = succeed () inp
        tryIt _ = pfail ["'pnpnot' expected 0 matches, found 1"] inp


-- succeeds if 'p' doesn't match,
--   and consumes one token
not1 :: Parser a b -> Parser a a
not1 p = ignoreLeft (not0 p) getOne


    
    
checkParser :: (b -> Bool) -> Parser a b -> Parser a b
checkParser f p inp = p inp >>= (\(rest, result) ->
  if f result then return (rest, result) else pfail ["parser check to pass"] inp)


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
string :: (Eq a, Show a) => [a] -> Parser a [a]
string = pall . map literal


-- matches if next token is not x,
--   and consumes one token
pnot :: (Eq a) => a -> Parser a a
pnot x = satisfy (/= x)
-- how about:
--  pnot x = not1 (literal x)
--  pnot = not1 . literal


-- matches if next token is not in xs,
--   consuming one token
--   not sure if I like this one
pnone :: (Eq a) => [a] -> Parser a a
pnone xs = satisfy (\x -> not $ elem x xs)
-- how about:
--  pnone xs = not1 (map literal xs)
--  pnone = not1 . map literal
