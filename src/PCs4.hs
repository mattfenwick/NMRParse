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
    
    -- hmmmm
    , pseq2
    , pseq3
    , string
    , separatedBy0
    , separatedBy1
    , ignoreLeft
    , ignoreRight
    , pnpnot
    , pnpnone
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
ignoreRight l r = using (uncurry const) $ pseq l r


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
pany = foldr alt (pfail [])


-- throw away result of parser
preturn :: c -> Parser a b -> Parser a c
preturn v p = using (const v) p
  
  
-- matches all of the parsers in sequence
pall :: [Parser a b] -> Parser a [b]
pall = foldr (\p b -> using cons (pseq p b)) (succeed [])
  where cons = uncurry (:)
  
  
-- matches end of input
end :: Parser a ()
end ([], x) = succeed () ([], x)
end ip = pfail ["end of input"] ip
  
  

usingFail :: (b -> Either (Failure a) c) -> Parser a b -> Parser a c
usingFail f p inp = p inp >>= 
  \(rest, r) -> f r >>=
  \c -> return (rest, c)
  

pseq2 :: (b -> c -> d) -> Parser a b -> Parser a c -> Parser a d
pseq2 f l r = using (uncurry f) $ pseq l r


pseq3 :: (b -> c -> d -> e) -> Parser a b -> Parser a c -> Parser a d -> Parser a e
pseq3 f l m r = using unpack $ pseq l $ pseq m r
  where unpack (b, (c, d)) = f b c d

-- ------------------------------
-- other interesting combinators 

    
separatedByOne :: Parser a b -> Parser a c -> Parser a ([b], [c])
separatedByOne p s = using f $ pseq p (using unzip $ many $ pseq s p)
  where f (fp, (oss, ops)) = (fp:ops, oss)
  

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


-- succeed but consume no input if parser succeeds
--   this is a very weird combinator and
--   I don't feel comfortable with it
lookahead :: Parser a b -> Parser a ()
lookahead p inp = tryIt $ p inp
  where tryIt (Right _) = succeed () inp
        tryIt (Left _) = pfail ["'lookahead' predicate"] inp


-- doesn't match a parser, consumes no input
pnpnot :: Parser a b -> Parser a ()
pnpnot p inp = tryIt $ p inp
  where tryIt (Left _) = succeed () inp
        tryIt _ = pfail ["'pnpnot' expected 0 matches, found 1"] inp
        
        
-- matches if none of the input parsers match; consumes no input
pnpnone :: [Parser a b] -> Parser a ()
pnpnone = preturn () . pall . map pnpnot



    
    
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


-- matches if next token is not x
pnot :: (Eq a) => a -> Parser a a
pnot x = satisfy (/= x)


-- matches if next token is not in xs
--   not sure if I like this one
pnone :: (Eq a) => [a] -> Parser a a
pnone xs = satisfy (\x -> not $ elem x xs)
