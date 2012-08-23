module Star2 (

) where

import ParserCombinators


blank :: Parser Char Char
blank = pany $ map literal "\t\v "


terminate :: Parser Char String
terminate = many $ pany $ map literal "\n\r\f"


-- c for 'corrected' because the spec is wrong -- had to remove blanks
-- c_ordinarychar :: Parser Char Char
-- c_ordinarychar = pany $ map literal "!%&()*+,-./0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^`abcdefghijklmnopqrstuvwxyz{|}~"
ordinarychar :: Parser Char Char
ordinarychar = pnone "\"#$'_;"


comment :: Parser Char String
comment = ignoreLeft (alt blank terminate) (ignoreLeft (literal '#') 


star = many 