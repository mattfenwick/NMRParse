module Java (

    scanner

) where

import MParse
import Classes
import Instances
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import JavaTokens
  
  
-- ------------------
-- the token parsers

inputCharacter :: Parser Char Char
inputCharacter = pnone ['\r', '\f', '\n'] -- whoa! \n wasn't in the spec!


lineTerminator :: Parser Char String
lineTerminator = mconcat $ map string ["\r\f", "\r", "\f", "\n"]  -- whoa! \n wasn't in the spec!


booleanLiteral :: Parser Char Bool
booleanLiteral = (string "true" *> pure True) <|> (string "false" *> pure False)


nullLiteral :: Parser Char String
nullLiteral = string "null"


keyword :: Parser Char Keyword
keyword = mconcat $ map p [minBound .. maxBound]
  where
    p k = fmap (const k) $ string $ tail $ show k


letter :: Parser Char Char
letter = satisfy isIn
  where isIn c = elem c ("$_" ++ ['a' .. 'z'] ++ ['A' .. 'Z'])


digit = satisfy (flip elem ['0' .. '9'])


identifierRest :: Parser Char String
identifierRest = many (letter <|> digit)
    
    
identifier :: Parser Char String
identifier = check (not . flip elem ["true", "false", "null"]) name
  where name = fmap (:) letter <*> identifierRest
  
  
singleCharacter :: Parser Char Char
singleCharacter = pnone "\r\f'\\\n" -- whoa! \n wasn't in the spec!


escapes :: [(String, Char)]
escapes = [("\\b",  '\b'),
           ("\\t",  '\t'),
           ("\\n",  '\n'),
           ("\\f",  '\f'),
           ("\\r",  '\r'),
           ("\\\"", '"'),
           ("\\'",  '\''),
           ("\\\\", '\\')]
                   

escapeSequence :: Parser Char Char
escapeSequence = mconcat $ map p escapes
  where
    p (s, c) = fmap (const c) $ string s


characterLiteral :: Parser Char Char
characterLiteral = sq *> (singleCharacter <|> escapeSequence) <* sq
  where sq = literal '\''
  
  
stringCharacter :: Parser Char Char
stringCharacter = pnone "\r\f\"\\\n" <|> escapeSequence    -- whoa! \n wasn't in the spec!

  
stringLiteral :: Parser Char String
stringLiteral = dq *> many stringCharacter <* dq
  where dq = literal '"'


seps :: [(Char, Separator)]
seps = [('(', OpenParen),
        (')', CloseParen),
        ('{', OpenCurly),
        ('}', CloseCurly),
        ('[', OpenSquare),
        (']', CloseSquare),
        (',', Comma),
        (';', Semicolon),
        ('.', Period)]


separator :: Parser Char Separator
separator = mconcat $ map p seps
  where 
    p (c, sep) = fmap (const sep) $ literal c


ops :: [(String, Operator)]
ops = [("=",    Equals),
       (">",    GreaterThan),
       ("<",    LessThan),
       ("!",    ExclamationPoint),
       ("~",    Tilda),
       ("?",    QuestionMark),
       (":",    Colon),
       ("==",   DoubleEquals),
       ("<=",   LessThanOrEquals),
       (">=",   GreaterThanOrEquals),
       ("!=",   NotEquals),
       ("&&",   AndAnd),
       ("||",   OrOr),
       ("++",   PlusPlus),
       ("--",   MinusMinus),
       ("+",    Plus),
       ("-",    Minus),
       ("*",    Times),
       ("/",    DivideBy),
       ("&",    And),
       ("|",    Or),
       ("^",    ToThe),
       ("%",    Percentage),
       ("<<",   DoubleLessThan),
       (">>",   DoubleGreaterThan),
       (">>>",  TripleGreaterThan),
       ("+=",   PlusEquals),
       ("-=",   MinusEquals),
       ("*=",   TimesEquals),
       ("/=",   DivideByEquals),
       ("&=",   AndEquals),
       ("|=",   OrEquals),
       ("^=",   ToTheEquals),
       ("%=",   PercentageEquals),
       ("<<=",  DoubleLessThanEquals),
       (">>=",  DoubleGreaterThanEquals),
       (">>>=", TripleGreaterThanEquals)]


operator :: Parser Char Operator
operator = mconcat $ map p ops
  where p (s, op) = fmap (const op) $ string s
    
    
floatTypeSuffix :: Parser Char Char
floatTypeSuffix = mconcat $ map literal "dDfF"


sign :: Parser Char Char
sign = literal '+' <|> literal '-'


signedInteger :: Parser Char String
signedInteger = fmap f (optional sign) <*> (some digit)
  where
    f Nothing xs    =  xs
    f (Just x) xs   =  x : xs


exponentIndicator :: Parser Char Char
exponentIndicator = literal 'e' <|> literal 'E'


exponentPart :: Parser Char String
exponentPart = fmap (:) exponentIndicator <*> signedInteger


optReturn :: a -> Parser t (Maybe a) -> Parser t a
optReturn x = fmap f
  where
    f Nothing   =  x
    f (Just y)  =  y


-- TOTALLY F-ED UP !!!  PLEASE IMPROVE
-- the read function can *FAIL* !!!!!
decimalFloatingPointLiteral :: Parser Char Double
decimalFloatingPointLiteral = fmap (read . concat) $ mconcat [f1, f2, f3, f4]
  where f1 = commute [some digit,  string ".",    many digit,   optExp,       optSuffix]
        f2 = commute [string ".",  some digit,    optExp,       optSuffix]
        f3 = commute [some digit,  exponentPart,  optSuffix]
        f4 = commute [some digit,  optExp,        fmap (:[]) floatTypeSuffix]
        optSuffix = optReturn "" (optional $ fmap (:[]) floatTypeSuffix)
        optExp = optReturn "" (optional exponentPart)
        
        
floatingPointLiteral :: Parser Char Double
floatingPointLiteral = decimalFloatingPointLiteral


integerTypeSuffix :: Parser Char Char
integerTypeSuffix = literal 'l' <|> literal 'L'


decimalNumeral :: Parser Char String
decimalNumeral = string "0" <|> (fmap (:) nonZeroDigit <*> many digit)
  where nonZeroDigit = mconcat $ map literal ['1' .. '9']


decimalIntegerLiteral :: Parser Char Integer
decimalIntegerLiteral = fmap read decimalNumeral <* optional integerTypeSuffix


integerLiteral :: Parser Char Integer
integerLiteral = decimalIntegerLiteral


jliteral :: Parser Char Literal
jliteral = mconcat [fmap LInteger integerLiteral, 
                    fmap LFloat floatingPointLiteral, 
                    fmap LBool booleanLiteral, 
                    fmap LChar characterLiteral, 
                    fmap LString stringLiteral, 
                    fmap (const LNull) nullLiteral]
  
  
token :: Parser Char Token
token = mconcat [fmap Identifier identifier, 
                 fmap Keyword keyword, 
                 fmap Literal jliteral, 
                 fmap Separator separator, 
                 fmap Operator operator]


whitespace :: Parser Char String
whitespace = mconcat [lineTerminator, string " ", string "\t"] -- what about 'form feed'?  wikipedia says it's '\f'


-- hmm, should the line terminator parser go here or not?
comment :: Parser Char String
comment = string "//" *> many inputCharacter

  
inputElement :: Parser Char InputElement
inputElement = mconcat [fmap (Whitespace . concat) $ some whitespace, -- this is weird ... ???
                        fmap Comment comment,
                        fmap Token token]


scanner :: Parser Char [InputElement]
scanner = many inputElement


{-
  -}