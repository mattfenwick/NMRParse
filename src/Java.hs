module Java (

    scanner

) where

import ParserCombinators


data InputElement
  = WhiteSpace String
  | Comment String
  | Token Token
  deriving (Show)

data Token
  = Identifier String
  deriving (Show)
{-
  | Keyword Keyword
  | Literal -- ???
  | Separator Separator
  | Operator Operator
  
  
-}  
  
  
  
-- ------------------
-- the parsers

inputCharacter :: Parser Char Char
inputCharacter = pnone ['\r', '\f', '\n'] -- whoa! \n wasn't in the spec!


lineTerminator :: Parser Char String
lineTerminator = pany $ map string ["\r\f", "\r", "\f", "\n"]  -- whoa! \n wasn't in the spec!


booleanLiteral :: Parser Char String
booleanLiteral = alt (string "true") (string "false")


nullLiteral :: Parser Char String
nullLiteral = string "null"


keyword :: Parser Char String
keyword = pany $ map string ["abstract", "continue", "for", "new", "switch", 
    "assert",  "default", "if",         "package",   "synchronized", 
    "boolean", "do",      "goto",       "private",   "this", 
    "break",   "double",  "implements", "protected", "throw", 
    "byte",    "else",    "import",     "public",    "throws",
    "case",    "enum",    "instanceof", "return",    "transient",
    "catch",   "extends", "int",        "short",     "try", 
    "char",    "final",   "interface",  "static",    "void",
    "class",   "finally", "long",       "strictfp",  "volatile", 
    "const",   "float",   "native",     "super",     "while"]


javaLetter :: Parser Char Char
javaLetter = pany [alpha, literal '$', literal '_']


javaLetterOrDigit :: Parser Char Char
javaLetterOrDigit = alt javaLetter digit


identifierRest :: Parser Char String
identifierRest = many javaLetterOrDigit
    
    
identifier :: Parser Char String
identifier = ignoreLeft (pnpnone [booleanLiteral, nullLiteral, keyword]) name
  where name = using (uncurry (:)) $ pseq javaLetter identifierRest
  
  
singleCharacter :: Parser Char Char
singleCharacter = pnone "\r\f'\\\n" -- whoa! \n wasn't in the spec!


escapeSequence :: Parser Char String
escapeSequence = pany $ map string ["\\b", "\\t", 
    "\\n", "\\f", "\\r", "\\\"", "\\'", "\\\\"]


characterLiteral :: Parser Char String
characterLiteral = ignoreLeft sq $ ignoreRight (alt (using (:[]) singleCharacter) escapeSequence) sq
  where sq = literal '\''
  
  
stringCharacter :: Parser Char String
stringCharacter = alt (using (:[]) $ pnone "\r\f\"\\\n") escapeSequence -- whoa! \n wasn't in the spec!

  
stringLiteral :: Parser Char [String]
stringLiteral = ignoreLeft dq $ ignoreRight (many stringCharacter) dq
  where dq = literal '"'


separator :: Parser Char Char
separator = pany $ map literal "(){}[];,."


operator :: Parser Char String
operator = pany $ map string ["=", ">", "<", "!", "~", "?", ":",
    "==", "<=", ">=", "!=", "&&", "||", "++", "--",
    "+", "-", "*", "/", "&", "|", "^", "%", "<<", ">>", ">>>",
    "+=", "-=", "*=", "/=", "&=", "|=", "^=", "%=", "<<=", ">>=", ">>>="]
    
    
floatTypeSuffix :: Parser Char Char
floatTypeSuffix = pany $ map literal "dDfF"


sign :: Parser Char Char
sign = alt (literal '+') (literal '-')


signedInteger :: Parser Char String
signedInteger = using (uncurry (:)) $ pseq (optional sign '+') (some digit)


exponentIndicator :: Parser Char Char
exponentIndicator = alt (literal 'e') (literal 'E')


exponentPart :: Parser Char String
exponentPart = using (uncurry (:)) $ pseq exponentIndicator signedInteger


decimalFloatingPointLiteral :: Parser Char String
decimalFloatingPointLiteral = using concat $ pany [f1, f2, f3, f4]
  where f1 = pall [some digit, string ".", many digit, optional exponentPart "", optSuffix]
        f2 = pall [string ".", some digit, optional exponentPart "", optSuffix]
        f3 = pall [some digit, exponentPart, optSuffix]
        f4 = pall [some digit, optional exponentPart "", using (:[]) floatTypeSuffix]
        optSuffix = optional (using (:[]) floatTypeSuffix) ""
        
        
floatingPointLiteral :: Parser Char String
floatingPointLiteral = decimalFloatingPointLiteral


integerTypeSuffix :: Parser Char Char
integerTypeSuffix = alt (literal 'l') (literal 'L')


decimalNumeral :: Parser Char String
decimalNumeral = alt (string "0") (using (uncurry (:)) $ pseq nonZeroDigit $ many digit)
  where nonZeroDigit = pany $ map literal ['1' .. '9']


decimalIntegerLiteral :: Parser Char String
decimalIntegerLiteral = using concat $ pall [decimalNumeral, optional (using (:[]) integerTypeSuffix) ""]


integerLiteral :: Parser Char String
integerLiteral = decimalIntegerLiteral


jliteral :: Parser Char String
jliteral = pany [integerLiteral, floatingPointLiteral, booleanLiteral, characterLiteral, using concat stringLiteral, nullLiteral]
  
  
token :: Parser Char String
token = pany [identifier, keyword, jliteral, using (:[]) separator, operator]


-- I believe this is end of input
sub :: Parser Char ()
sub "" = succeed () ""
sub _ = fail "could not match end of input"


whitespace :: Parser Char String
whitespace = pany [lineTerminator, string " ", string "\t"] -- what about 'form feed'?  wikipedia says it's '\f'


-- hmm, should the line terminator parser go here or not?
comment :: Parser Char String
comment = ignoreLeft (string "//") (many inputCharacter)

  
inputElement :: Parser Char String
inputElement = pany [whitespace, comment, token]


scanner :: Parser Char [String]
scanner = many inputElement



  