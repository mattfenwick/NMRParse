module Star (

) where

import ParserCombinators

{-
data Token
  = Spaces String
  | Newline String
  | Comment String
  | 
-}

sp :: Parser Char Char
sp = pany $ map literal " \t"


newline :: Parser Char String
newline = pany $ map string ["\n", "\r\f", "\f"] -- ??


comment :: Parser Char String
comment = ignoreLeft (literal '#') (ignoreRight (many $ ignoreLeft (pnpnot newline) getOne) newline)
-- the following doesn't work because it consumes 
--   everything before looking for the newline:
-- comment = ignoreLeft (literal '#') (ignoreRight (many getOne) newline)


wspace :: Parser Char String
wspace = pany [using (:[]) sp, newline, comment]


nonblankchar :: Parser Char Char
nonblankchar = pnpnone [sp, newline] -- or 'bell'


char :: Parser Char Char
char = alt sp nonblankchar
-- isn't this just:
--   char = pnpnot newline   ???


line :: Parser Char String
line = alt () (succeed "")


noQUOTchar :: Parser Char Char
noQUOTchar = pnpnone [literal '"', newline] -- or bell ... this is ill-typed


noAPOSchar :: Parser Char Char
noAPOSchar = pnpnone [literal '\'', newline] -- or bell ... types


leadchar :: Parser Char Char
leadchar = pany $ map literal "\"#$'_[]{};," -- or bell


restrictedchar :: Parser Char Char
restrictedchar = pnone " \t,[]{}" -- or newlines, or bell






