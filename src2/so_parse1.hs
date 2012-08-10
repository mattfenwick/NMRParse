import Text.ParserCombinators.Parsec


data ASTNode 
  = Application ASTNode [ASTNode]
  | Symbol String
  | Number Float
  deriving (Show)


int :: Parser ASTNode
int = many1 digit >>= (return . Number . read)


symbol :: Parser ASTNode
symbol = many1 (oneOf ['a'..'z']) >>= (return . Symbol)


whitespace :: Parser String
whitespace = many1 (oneOf " \t\n\r\f")


app :: Parser ASTNode
app =
  char '(' >>
  sepBy1 expr whitespace >>= (\(e:es) ->
  char ')' >>
  (return $ Application e es))


expr :: Parser ASTNode
expr =  symbol  <|>  int  <|>  app