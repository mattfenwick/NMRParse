import Control.Monad.Error -- for Monad instance of Either
import qualified Data.Map as Map -- for eval


          
      
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
  | Decimal Float
  | String String -- again, weird
  | Symbol String
  deriving (Show, Eq)
  
  
myReader :: String -> Integer
myReader [] = 0
myReader xs = read xs


nextToken :: Parser Char Token
nextToken = pany [op, cp, os, cs, ws, flt, int, str, com, sym]
  where
    op = preturn OpenParen $ literal '('
    cp = preturn CloseParen $ literal ')'
    os = preturn OpenSquare $ literal '['
    cs = preturn CloseSquare $ literal ']'
    ws = using Whitespace $ some wschar
    flt = using (Decimal . read . concat) $ alt (pall [some digit, dot, many digit]) (pall [many digit, dot, some digit])
    int = using Integer integer
    str = using (String . concat) $ pall [preturn [] $ literal '"', many $ pnot '"', preturn [] $ literal '"']
    com = using (Comment . concat) $ pall [preturn [] $ literal ';', many $ pnot '\n']
    sym = using Symbol $ some alpha
    dot = using (:[]) $ literal '.'

  
  
scanner :: Parser Char [Token]
scanner = many nextToken


-- ------------------------------------------------------------------
-- AST construction

data ASTNode 
  = Application ASTNode [ASTNode]
  | AList [ASTNode]
  | ASymbol String
  | ANumber Float
  | AChar Char
  deriving (Show)

astring :: Parser Token ASTNode
astring (String s:rest) = succeed (AList $ map AChar s) rest
astring r = pfail "unable to match 'AString'" r

anumber :: Parser Token ASTNode
anumber (Integer i:rest) = succeed (ANumber $ fromIntegral i) rest
anumber (Decimal f:rest) = succeed (ANumber f) rest
anumber r = pfail "unable to match 'ANumber'" r

asymbol :: Parser Token ASTNode
asymbol (Symbol s:rest) = succeed (ASymbol s) rest
asymbol r = pfail "unable to match 'ASymbol'" r

aws :: Parser Token ()
aws (Whitespace w:rest) = succeed () rest
aws r = pfail "unable to match whitespace" r

alist :: Parser Token ASTNode
alist = using (AList . concat) $ pall [os, forms, cs]
  where os = preturn [] $ literal OpenSquare
        forms = many form
        cs = preturn [] $ literal CloseSquare
        
app :: Parser Token ASTNode
app = using (\(_, (f, (rs, _))) -> Application f rs) $ pseq op $ pseq (alt app asymbol) $ pseq (many form) cp
  where op = literal OpenParen
        cp = literal CloseParen
        
form :: Parser Token ASTNode
form = pany [astring, anumber, asymbol, alist, app]

beagle :: Parser Token [ASTNode]
beagle = some form


unwrap :: (Monad m) => m (b, c) -> m c
unwrap x = x >>= (return . snd)

-- :: Monad m => (b, c) -> m c

-- full :: String -> Either String [ASTNode]
full str = scanner str >>= (beagle . filter shit . snd) >>= (return . snd) -- (\x -> Right $ snd x)
  where shit (Whitespace _) = False
        shit _ = True
        

-- --------------------------------------------------------
-- the evaluator

type Environment = Map.Map String LispVal

data LispVal
  = LNumber Float
  | LList [LispVal]
  | LChar Char
  | LBoolean Bool
  | LFunc ([LispVal] -> LispVal)
  | LSpecial (Environment -> [ASTNode] -> LispVal)
  
  
instance Show LispVal where
  show (LNumber f) = show f
  show (LList fs) = show $ map show fs
  show (LChar c) = show c
  show (LFunc f) = "function"
  show (LBoolean b) = show b
  
  
apply :: Environment -> LispVal -> [LispVal] -> Either String (Environment, LispVal)
apply e (LFunc f) args = Right (e, f args)
apply e (LSpecial s) forms = Left "special form evaluation is unimplemented"
apply _ _ _ = Left "hey, you didn't give me a function or special form!"


evalForm :: Either String (Environment, [LispVal]) -> ASTNode -> Either String (Environment, [LispVal])
evalForm base nextForm = base >>= (\(env, evaledForms) -> 
  eval env nextForm >>= (\(nextEnv, evaledForm) -> 
  return (nextEnv, evaledForm : evaledForms)))
  
  
eval :: Environment -> ASTNode -> Either String (Environment, LispVal)
eval e (ANumber f) = Right (e, LNumber f)
eval e (AChar c) = Right (e, LChar c)
eval e (ASymbol s) = (fromMaybe $ Map.lookup s e) >>= (\v -> return (e, v))
  where fromMaybe Nothing = Left ("unbound variable: " ++ s)
        fromMaybe (Just x) = Right x
eval e (AList fs) = foldl evalForm (Right (e, [])) fs >>= 
  (\(newEnv, newForms) -> Right (newEnv, LList $ reverse newForms))
eval e (Application op args) = eval e op >>=
  (\(e1, f) -> foldl evalForm (Right (e1, [])) args >>=
  (\(newEnv, newForms) -> apply newEnv f $ reverse newForms))
  
  
defaultEnv = Map.fromList [("true", LBoolean True), ("false", LBoolean False)]


      
evaluator :: String -> Either String [LispVal]
evaluator str = full str >>= 
  mapM (eval defaultEnv) >>= 
  (return . (Prelude.map snd))
  

