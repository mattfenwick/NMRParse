module Java (

    scanner

) where

import ParserCombinators
import JavaTokens
  
  
-- ------------------
-- the token parsers

inputCharacter :: Parser Char Char
inputCharacter = pnone ['\r', '\f', '\n'] -- whoa! \n wasn't in the spec!


lineTerminator :: Parser Char String
lineTerminator = pany $ map string ["\r\f", "\r", "\f", "\n"]  -- whoa! \n wasn't in the spec!


booleanLiteral :: Parser Char Bool
booleanLiteral = alt (preturn True $ string "true") (preturn False $ string "false")


nullLiteral :: Parser Char String
nullLiteral = string "null"


keyword :: Parser Char Keyword
keyword = usingFail nameToKeyword $ pany $ map string keywordNames


javaLetter :: Parser Char Char
javaLetter = pany [alpha, literal '$', literal '_']


javaLetterOrDigit :: Parser Char Char
javaLetterOrDigit = alt javaLetter digit


identifierRest :: Parser Char String
identifierRest = many javaLetterOrDigit
    
    
identifier :: Parser Char String
identifier = usingFail nameCheck name
  where name = using (uncurry (:)) $ pseq javaLetter identifierRest
        nameCheck x 
          | elem x (keywordNames ++ ["true", "false", "null"]) = Left ""
          | otherwise = Right x
  
  
singleCharacter :: Parser Char Char
singleCharacter = pnone "\r\f'\\\n" -- whoa! \n wasn't in the spec!


escapeToChar :: String -> Either String Char
escapeToChar esc = check $ lookup esc escapes
  where escapes = [("\\b", '\b'),
                   ("\\t", '\t'),
                   ("\\n", '\n'),
                   ("\\f", '\f'),
                   ("\\r", '\r'),
                   ("\\\"", '"'),
                   ("\\'", '\''),
                   ("\\\\", '\\')]
        check Nothing = Left ("unable to translate escape sequence " ++ esc)
        check (Just x) = Right x
                   

escapeSequence :: Parser Char Char
escapeSequence = usingFail escapeToChar $ pany $ map string ["\\b", "\\t", 
    "\\n", "\\f", "\\r", "\\\"", "\\'", "\\\\"]


characterLiteral :: Parser Char Char
characterLiteral = ignoreLeft sq $ ignoreRight (alt singleCharacter escapeSequence) sq
  where sq = literal '\''
  
  
stringCharacter :: Parser Char Char
stringCharacter = alt (pnone "\r\f\"\\\n") escapeSequence    -- whoa! \n wasn't in the spec!

  
stringLiteral :: Parser Char String
stringLiteral = ignoreLeft dq $ ignoreRight (many stringCharacter) dq
  where dq = literal '"'


separator :: Parser Char Separator
separator = usingFail stringToSeparator $ pany $ map literal "(){}[];,."


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


decimalFloatingPointLiteral :: Parser Char Double
decimalFloatingPointLiteral = using (read . concat) $ pany [f1, f2, f3, f4]
  where f1 = pall [some digit, string ".", many digit, optional exponentPart "", optSuffix]
        f2 = pall [string ".", some digit, optional exponentPart "", optSuffix]
        f3 = pall [some digit, exponentPart, optSuffix]
        f4 = pall [some digit, optional exponentPart "", using (:[]) floatTypeSuffix]
        optSuffix = optional (using (:[]) floatTypeSuffix) ""
        
        
floatingPointLiteral :: Parser Char Double
floatingPointLiteral = decimalFloatingPointLiteral


integerTypeSuffix :: Parser Char Char
integerTypeSuffix = alt (literal 'l') (literal 'L')


decimalNumeral :: Parser Char String
decimalNumeral = alt (string "0") (using (uncurry (:)) $ pseq nonZeroDigit $ many digit)
  where nonZeroDigit = pany $ map literal ['1' .. '9']


decimalIntegerLiteral :: Parser Char Integer
decimalIntegerLiteral = ignoreRight (using read decimalNumeral) (optional (using (:[]) integerTypeSuffix) "")


integerLiteral :: Parser Char Integer
integerLiteral = decimalIntegerLiteral


jliteral :: Parser Char Literal
jliteral = pany [using LInteger integerLiteral, 
                 using LFloat floatingPointLiteral, 
                 using LBool booleanLiteral, 
                 using LChar characterLiteral, 
                 using LString stringLiteral, 
                 using (const LNull) nullLiteral]
  
  
token :: Parser Char Token
token = pany [using Identifier identifier, 
              using Keyword keyword, 
              using Literal jliteral, 
              using Separator separator, 
              using (const $ Operator No) operator]


-- I believe this is end of input
sub :: Parser Char ()
sub "" = succeed () ""
sub _ = fail "could not match end of input"


whitespace :: Parser Char String
whitespace = pany [lineTerminator, string " ", string "\t"] -- what about 'form feed'?  wikipedia says it's '\f'


-- hmm, should the line terminator parser go here or not?
comment :: Parser Char String
comment = ignoreLeft (string "//") (many inputCharacter)

  
inputElement :: Parser Char InputElement
inputElement = pany [using (Whitespace . concat) $ some whitespace,
                     using Comment comment,
                     using Token token]


scanner :: Parser Char [InputElement]
scanner = many inputElement


  