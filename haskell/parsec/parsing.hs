import Text.ParserCombinators.Parsec


data ASTNode 
  = Application ASTNode [ASTNode]
  | List [ASTNode]
  | Symbol String
  | Number Float
  | Char Char
  deriving (Show)


str :: Parser ASTNode
str = 
  char '"' >> 
  many (noneOf ['"']) >>= (\x ->
  char '"' >>
  (return $ List $ map Char x))


f1 :: Parser ASTNode
f1 = 
  many1 digit >>= (\b ->
  char '.' >>= (\d ->
  many digit >>= (\a ->
  return $ Number $ read $ concat [b, [d], a])))

f2 :: Parser ASTNode
f2 =
  many digit >>= (\b ->
  char '.' >>= (\d ->
  many1 digit >>= (\a ->
  return $ Number $ read $ concat [b, [d], a])))

float :: Parser ASTNode
float = f1 <|> f2


int :: Parser ASTNode
int = many1 digit >>= (return . Number . read)


symbol :: Parser ASTNode
symbol = (oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> oneOf "!@#$%^&*-_=+?/!<>") >>= (\f -> 
  many (oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> oneOf ['0'..'9'] <|> oneOf "!@#$%^&*-_=+?/!<>") >>= (\r ->
  return $ Symbol (f : r)))


expr :: Parser ASTNode
expr =  symbol  <|>  int  <|>  float  <|>  str  <|>  app  <|>  list


whitespace :: Parser String
whitespace = many1 (oneOf " \t\n\r\f")


comment :: Parser String
comment =
  char ';' >>
  many (noneOf "\n")


app :: Parser ASTNode
app =
  char '(' >>
  sepBy1 expr whitespace >>= (\(e:es) ->
  char ')' >>
  (return $ Application e es))


list :: Parser ASTNode
list = 
  char '[' >>
  sepBy expr whitespace >>= (\es ->
  char ']' >>
  (return $ List es))

