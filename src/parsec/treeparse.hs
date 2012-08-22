import Text.ParserCombinators.Parsec
import Data.Tree


data ASTNode 
  = Application (Tree ASTNode)
  | List
  | Symbol String
  | Number Float
  | Char Char
  deriving (Show)


str :: Parser (Tree ASTNode)
str = 
  char '"' >> 
  many (noneOf ['"']) >>= (\x ->
  char '"' >>
  (return $ Node List (map (\y -> Node (Char y) []) x)))


f1 :: Parser (Tree ASTNode)
f1 = 
  many1 digit >>= (\b ->
  char '.' >>= (\d ->
  many digit >>= (\a ->
  return $ Node (Number $ read $ concat [b, [d], a]) [])))

f2 :: Parser (Tree ASTNode)
f2 =
  many digit >>= (\b ->
  char '.' >>= (\d ->
  many1 digit >>= (\a ->
  return $ Node (Number $ read $ concat [b, [d], a]) [])))

float :: Parser (Tree ASTNode)
float = f1 <|> f2


int :: Parser (Tree ASTNode)
int = many1 digit >>= (\x -> return $ Node (Number $ read x) [])


symbol :: Parser (Tree ASTNode)
symbol = (oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> oneOf "!@#$%^&*-_=+?/!<>") >>= (\f -> 
  many (oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> oneOf ['0'..'9'] <|> oneOf "!@#$%^&*-_=+?/!<>") >>= (\r ->
  return $ Node (Symbol $ f : r) []))


expr :: Parser (Tree ASTNode)
expr =  symbol  <|>  int  <|>  float  <|>  str  <|>  app  <|>  list


whitespace :: Parser String
whitespace = many1 (oneOf " \t\n\r\f")


comment :: Parser String
comment =
  char ';' >>
  many (noneOf "\n")


app :: Parser (Tree ASTNode)
app =
  char '(' >>
  sepBy1 expr whitespace >>= (\(e:es) ->
  char ')' >>
  (return $ Node (Application e) es))


list :: Parser (Tree ASTNode)
list = 
  char '[' >>
  sepBy expr whitespace >>= (\es ->
  char ']' >>
  (return $ Node List es))

{- -}