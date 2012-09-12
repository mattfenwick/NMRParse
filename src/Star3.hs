module Star3 (

) where

import ParserCombinators
import System.IO
  
  
-- ------------------
-- the token parsers


data Token =
  DataOpen String 
  | SaveOpen String
  | SaveClose
  | Whitespace String
  | Newline String
  | Comment String
  | Loop
  | Stop
  | Identifier String
  | Value String
  | Global
  deriving (Show, Eq)


comment :: Parser Char Token
comment = using Comment $ ignoreLeft (literal '#') (many $ pnone "\n\r\f")


dataOpen :: Parser Char Token
dataOpen = using DataOpen $ ignoreLeft (string "data_") (some $ pnone " \t\n\r\f\v")


saveOpen :: Parser Char Token
saveOpen = using SaveOpen $ ignoreLeft (string "save_") (some $ pnone " \t\n\r\f\v")


saveClose :: Parser Char Token
saveClose = preturn SaveClose $ string "save_"


whitespace :: Parser Char Token
whitespace = using Whitespace $ some $ pany $ map literal " \t\v"


newline :: Parser Char Token
newline = using Newline $ some $ pany $ map literal "\n\r\f"


stop :: Parser Char Token
stop = preturn Stop $ string "stop_"


global :: Parser Char Token
global = preturn Global $ string "global_"


loop :: Parser Char Token
loop = preturn Loop $ string "loop_"


identifier :: Parser Char Token
identifier = using Identifier $ ignoreLeft (literal '_') (some $ pnone " \t\n\r\f\v")


sqstring :: Parser Char Token
sqstring = using Value $ ignoreLeft (literal '\'') $ ignoreRight (some $ pnot '\'') (literal '\'')


dqstring :: Parser Char Token
dqstring = using Value $ ignoreLeft (literal '"') $ ignoreRight (some $ pnot '"') (literal '"')


scstring :: Parser Char Token
scstring = using Value $ ignoreLeft (literal ';') $ ignoreRight (some $ pnot ';') (literal ';')


sbstring :: Parser Char Token
sbstring = using Value $ ignoreLeft (literal '[') $ ignoreRight (some $ pnone "[]") (literal ']')


uq :: Parser Char Token
uq = using (\(x,y) -> Value (x:y)) $ pseq (pnone "\"#'[]_ \t\v\r\f\n") (many $ pnone "\"#'[] \t\v\r\f\n")


value :: Parser Char Token
value = pany [sqstring, dqstring, scstring, sbstring, uq]


oneToken :: Parser Char Token
oneToken = pany [dataOpen, saveOpen, saveClose, loop, stop, global, value, whitespace, newline, comment, identifier]


scanner :: Parser Char [Token]
scanner = many oneToken


myReadFile :: String -> IO String
myReadFile path = 
  openFile path ReadMode >>= hGetContents
  
  
-- test :: IO (Parser Strin
test = myReadFile "bmrb2.1.txt" >>= (return . scanner)


-------------
-- the rest of the parsers

data AST
  = PLoop [Token] [Token]
  | PSave [AST]
  | PDatum Token Token
  | PGlobal [AST]
  | PData [AST]
  | PStar [AST]
  deriving (Show, Eq)


ident :: Parser Token Token
ident (Identifier x:rest) = succeed (Identifier x) rest
ident x = pfail "failed to get an identifier" x


val :: Parser Token Token
val (Value v:rest) = succeed (Value v) rest
val x = pfail "failed to get a value" x


ws :: Parser Token Token
ws (Whitespace w:rest) = succeed (Whitespace w) rest
ws (Newline n:rest) = succeed (Newline n) rest
ws x = pfail "failed to get ws" x


saveme :: Parser Token Token
saveme (SaveOpen s:rest) = succeed (SaveOpen s) rest
saveme x = pfail "failed to get save open" x


datame :: Parser Token Token
datame (DataOpen s:rest) = succeed (DataOpen s) rest
datame x = pfail "failed to get data open" x


pLoop :: Parser Token AST
pLoop = using (uncurry PLoop) $ ignoreLeft (literal Loop) $ ignoreRight stuff $ pseq (some ws) (literal Stop)
  where stuff = pseq (many $ ignoreLeft (some ws) ident) (many $ ignoreLeft (some ws) val)
  
  
datum :: Parser Token AST
datum = using (uncurry PDatum) $ pseq ident $ ignoreLeft (some ws) val
  
  
pSave :: Parser Token AST
pSave = using PSave $ ignoreLeft saveme $ ignoreRight contents $ pseq (some ws) (literal SaveClose)
  where contents = some $ ignoreLeft (some ws) $ alt datum pLoop
  

{- are these not used in NMR-Star files?
pGlobal :: Parser Token AST  
pGlobal = using PGlobal $ ignoreLeft (literal Global) $ ignoreRight contents (some ws)
  where contents = some $ alt datum pSave
-}


pData :: Parser Token AST
pData = using PData $ ignoreLeft datame $ ignoreRight contents (some ws)
  where contents = some $ ignoreLeft (some ws) $ alt datum pSave
  
  
pStar :: Parser Token AST
pStar = using PStar $ ignoreLeft (many ws) (many pData)


parseMe :: Parser Token AST 
parseMe = pStar . filter notComment
  where notComment x = case x of (Comment _) -> False; _ -> True
  
  
-- testParse :: String -> Either String AST
testParse x = scanner x >>= (return . snd) >>= parseMe
  
  
test2 = myReadFile "bmrb2.1.txt" >>= (return . testParse)


test3 = myReadFile "bmrb3.0.txt" >>= (return . testParse)


q = [Loop,Newline "\n",Whitespace "      ",Identifier "Author_ordinal",Newline "\n",Whitespace "      ",Identifier "Author_family_name",Newline "\n",Whitespace "      ",Identifier "Author_given_name",Newline "\n",Whitespace "      ",Identifier "Author_middle_initials",Newline "\n",Whitespace "      ",Identifier "Author_family_title",Newline "\n\n",Whitespace "      ",Value "1",Whitespace " ",Value "Gao",Whitespace "      ",Value "Yuan",Whitespace "     ",Value ".",Whitespace "    ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "2",Whitespace " ",Value "Boyd",Whitespace "     ",Value "Jonathan",Whitespace " ",Value ".",Whitespace "    ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "3",Whitespace " ",Value "Pielak",Whitespace "   ",Value "Gary",Whitespace "     ",Value "J.",Whitespace "   ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "4",Whitespace " ",Value "Williams",Whitespace " ",Value "Robert",Whitespace "   ",Value "J.P.",Whitespace " ",Value ".",Whitespace " ",Newline "\n\n",Whitespace "   ",Stop]
r = [SaveOpen "entry_information",Newline "\n",Whitespace "   ",Identifier "Saveframe_category",Whitespace "      ",Value "entry_information",Newline "\n\n",Whitespace "   ",Identifier "Entry_title",Whitespace "            ",Newline "\n",Value "\nComparison of Reduced and Oxidized Yeast Iso-1-cytochrome c Using Proton \nParamagnetic Shifts\n",Newline "\n\n",Whitespace "   ",Loop,Newline "\n",Whitespace "      ",Identifier "Author_ordinal",Newline "\n",Whitespace "      ",Identifier "Author_family_name",Newline "\n",Whitespace "      ",Identifier "Author_given_name",Newline "\n",Whitespace "      ",Identifier "Author_middle_initials",Newline "\n",Whitespace "      ",Identifier "Author_family_title",Newline "\n\n",Whitespace "      ",Value "1",Whitespace " ",Value "Gao",Whitespace "      ",Value "Yuan",Whitespace "     ",Value ".",Whitespace "    ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "2",Whitespace " ",Value "Boyd",Whitespace "     ",Value "Jonathan",Whitespace " ",Value ".",Whitespace "    ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "3",Whitespace " ",Value "Pielak",Whitespace "   ",Value "Gary",Whitespace "     ",Value "J.",Whitespace "   ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "4",Whitespace " ",Value "Williams",Whitespace " ",Value "Robert",Whitespace "   ",Value "J.P.",Whitespace " ",Value ".",Whitespace " ",Newline "\n\n",Whitespace "   ",Stop,Newline "\n\n",Whitespace "   ",Identifier "BMRB_accession_number",Whitespace "   ",Value "345",Newline "\n",Whitespace "   ",Identifier "BMRB_flat_file_name",Whitespace "     ",Value "bmr345.str",Newline "\n",Whitespace "   ",Identifier "Entry_type",Whitespace "              ",Value "revision",Newline "\n",Whitespace "   ",Identifier "Submission_date",Whitespace "         ",Value "1995-07-31",Newline "\n",Whitespace "   ",Identifier "Accession_date",Whitespace "          ",Value "1996-04-12",Newline "\n",Whitespace "   ",Identifier "Entry_origination",Whitespace "       ",Value "BMRB",Newline "\n",Whitespace "   ",Identifier "NMR_STAR_version",Whitespace "        ",Value "2.1",Newline "\n",Whitespace "   ",Identifier "Experimental_method",Whitespace "     ",Value "NMR",Newline "\n\n",Whitespace "   ",Loop,Newline "\n",Whitespace "      ",Identifier "Saveframe_category_type",Newline "\n",Whitespace "      ",Identifier "Saveframe_category_type_count",Newline "\n\n",Whitespace "      ",Value "assigned_chemical_shifts",Whitespace " ",Value "1",Whitespace " ",Newline "\n\n",Whitespace "   ",Stop,Newline "\n\n",Whitespace "   ",Loop,Newline "\n",Whitespace "      ",Identifier "Data_type",Newline "\n",Whitespace "      ",Identifier "Data_type_count",Newline "\n\n",Whitespace "      ",Value "1H chemical shifts",Whitespace " ",Value "511",Whitespace " ",Newline "\n\n",Whitespace "   ",Stop,Newline "\n\n",Whitespace "   ",Loop,Newline "\n",Whitespace "      ",Identifier "Revision_date",Newline "\n",Whitespace "      ",Identifier "Revision_keyword",Newline "\n",Whitespace "      ",Identifier "Revision_author",Newline "\n",Whitespace "      ",Identifier "Revision_detail",Newline "\n\n",Whitespace "      ",Value "1999-06-14",Whitespace " ",Value "revision",Whitespace " ",Value "BMRB",Whitespace " ",Value "Converted to BMRB NMR-STAR V 2.1 format",Whitespace "                ",Newline "\n",Whitespace "      ",Value "1996-04-12",Whitespace " ",Value "revision",Whitespace " ",Value "BMRB",Whitespace " ",Value "Error corrected in abrreviations given to non-polymers",Whitespace " ",Newline "\n",Whitespace "      ",Value "1996-03-25",Whitespace " ",Value "reformat",Whitespace " ",Value "BMRB",Whitespace " ",Value "Converted to the BMRB 1996-03-01 STAR flat-file format",Whitespace " ",Newline "\n",Whitespace "      ",Value "1995-07-31",Whitespace " ",Value "original",Whitespace " ",Value "BMRB",Whitespace " ",Value "Last release in original BMRB flat-file format",Whitespace "         ",Newline "\n\n",Whitespace "   ",Stop,Newline "\n\n",SaveClose]
s = [DataOpen "345",Newline "\n\n",Comment "######################",Newline "\n",Comment "  Entry information  #",Newline "\n",Comment "######################",Newline "\n\n",SaveOpen "entry_information",Newline "\n",Whitespace "   ",Identifier "Saveframe_category",Whitespace "      ",Value "entry_information",Newline "\n\n",Whitespace "   ",Identifier "Entry_title",Whitespace "            ",Newline "\n",Value "\nComparison of Reduced and Oxidized Yeast Iso-1-cytochrome c Using Proton \nParamagnetic Shifts\n",Newline "\n\n",Whitespace "   ",Loop,Newline "\n",Whitespace "      ",Identifier "Author_ordinal",Newline "\n",Whitespace "      ",Identifier "Author_family_name",Newline "\n",Whitespace "      ",Identifier "Author_given_name",Newline "\n",Whitespace "      ",Identifier "Author_middle_initials",Newline "\n",Whitespace "      ",Identifier "Author_family_title",Newline "\n\n",Whitespace "      ",Value "1",Whitespace " ",Value "Gao",Whitespace "      ",Value "Yuan",Whitespace "     ",Value ".",Whitespace "    ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "2",Whitespace " ",Value "Boyd",Whitespace "     ",Value "Jonathan",Whitespace " ",Value ".",Whitespace "    ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "3",Whitespace " ",Value "Pielak",Whitespace "   ",Value "Gary",Whitespace "     ",Value "J.",Whitespace "   ",Value ".",Whitespace " ",Newline "\n",Whitespace "      ",Value "4",Whitespace " ",Value "Williams",Whitespace " ",Value "Robert",Whitespace "   ",Value "J.P.",Whitespace " ",Value ".",Whitespace " ",Newline "\n\n",Whitespace "   ",Stop,Newline "\n\n",Whitespace "   ",Identifier "BMRB_accession_number",Whitespace "   ",Value "345",Newline "\n",Whitespace "   ",Identifier "BMRB_flat_file_name",Whitespace "     ",Value "bmr345.str",Newline "\n",Whitespace "   ",Identifier "Entry_type",Whitespace "              ",Value "revision",Newline "\n",Whitespace "   ",Identifier "Submission_date",Whitespace "         ",Value "1995-07-31",Newline "\n",Whitespace "   ",Identifier "Accession_date",Whitespace "          ",Value "1996-04-12",Newline "\n",Whitespace "   ",Identifier "Entry_origination",Whitespace "       ",Value "BMRB",Newline "\n",Whitespace "   ",Identifier "NMR_STAR_version",Whitespace "        ",Value "2.1",Newline "\n",Whitespace "   ",Identifier "Experimental_method",Whitespace "     ",Value "NMR",Newline "\n\n",Whitespace "   ",Loop,Newline "\n",Whitespace "      ",Identifier "Saveframe_category_type",Newline "\n",Whitespace "      ",Identifier "Saveframe_category_type_count",Newline "\n\n",Whitespace "      ",Value "assigned_chemical_shifts",Whitespace " ",Value "1",Whitespace " ",Newline "\n\n",Whitespace "   ",Stop,Newline "\n\n",Whitespace "   ",Loop,Newline "\n",Whitespace "      ",Identifier "Data_type",Newline "\n",Whitespace "      ",Identifier "Data_type_count",Newline "\n\n",Whitespace "      ",Value "1H chemical shifts",Whitespace " ",Value "511",Whitespace " ",Newline "\n\n",Whitespace "   ",Stop,Newline "\n\n",Whitespace "   ",Loop,Newline "\n",Whitespace "      ",Identifier "Revision_date",Newline "\n",Whitespace "      ",Identifier "Revision_keyword",Newline "\n",Whitespace "      ",Identifier "Revision_author",Newline "\n",Whitespace "      ",Identifier "Revision_detail",Newline "\n\n",Whitespace "      ",Value "1999-06-14",Whitespace " ",Value "revision",Whitespace " ",Value "BMRB",Whitespace " ",Value "Converted to BMRB NMR-STAR V 2.1 format",Whitespace "                ",Newline "\n",Whitespace "      ",Value "1996-04-12",Whitespace " ",Value "revision",Whitespace " ",Value "BMRB",Whitespace " ",Value "Error corrected in abrreviations given to non-polymers",Whitespace " ",Newline "\n",Whitespace "      ",Value "1996-03-25",Whitespace " ",Value "reformat",Whitespace " ",Value "BMRB",Whitespace " ",Value "Converted to the BMRB 1996-03-01 STAR flat-file format",Whitespace " ",Newline "\n",Whitespace "      ",Value "1995-07-31",Whitespace " ",Value "original",Whitespace " ",Value "BMRB",Whitespace " ",Value "Last release in original BMRB flat-file format",Whitespace "         ",Newline "\n\n",Whitespace "   ",Stop,Newline "\n\n",SaveClose,Newline "\n\n\n"]

