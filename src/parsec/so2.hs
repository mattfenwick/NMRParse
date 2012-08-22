import Text.ParserCombinators.Parsec
import Data.Tree


data ASTNode 
  = Application (Tree ASTNode)
  | Symbol String
  | Number Float
  deriving (Show)


int :: Parser (Tree ASTNode)
int = many1 digit >>= (\x -> return $ Node (Number $ read x) [])


symbol :: Parser (Tree ASTNode)
symbol = many1 (oneOf ['a' .. 'z']) >>= (\x -> return $ Node (Symbol x) [])


whitespace :: Parser String
whitespace = many1 (oneOf " \t\n\r\f")


app :: Parser (Tree ASTNode)
app =
  char '(' >>
  sepBy1 expr whitespace >>= (\(e:es) ->
  char ')' >>
  (return $ Node (Application e) es))


expr :: Parser (Tree ASTNode)
expr =  symbol  <|>  int  <|>  app


