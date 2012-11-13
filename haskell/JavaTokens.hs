module JavaTokens (
    
    Keyword(..)
  , keywordNames
  , nameToKeyword
    
  , Literal(..)
  
  , Separator(..)
  , stringToSeparator
  
  , Operator(..)
  
  , Token(..)
  
  , InputElement(..)
    
) where


-- whoa prefixes !!! 'double' needs to go before 'do', 
--   'throws' before 'throw', 'finally' before 'final',
--   'interface' before 'int'
data Keyword
  = Kdouble
  | Kthrows
  | Kfinally
  | Kinterface
  | Kabstract
  | Kcontinue
  | Kfor
  | Knew
  | Kswitch
  | Kassert
  | Kdefault
  | Kif
  | Kpackage
  | Ksynchronized
  | Kbreak
  | Kimplements
  | Kprotected
  | Kthrow
  | Kboolean
  | Kdo
  | Kgoto
  | Kprivate
  | Kthis
  | Kbyte
  | Kelse
  | Kimport
  | Kpublic
  | Kcase
  | Kenum
  | Kinstanceof
  | Kreturn
  | Ktransient
  | Kcatch
  | Kextends
  | Kint
  | Kshort
  | Ktry
  | Kclass
  | Klong
  | Kstrictfp
  | Kvolatile
  | Kchar
  | Kfinal
  | Kstatic
  | Kvoid
  | Kconst
  | Kfloat
  | Knative
  | Ksuper
  | Kwhile
  deriving (Show, Eq, Bounded, Enum, Read)


keywordNames :: [String]
keywordNames = map (tail . show) ([minBound .. maxBound] :: [Keyword])


nameToKeyword :: String -> Either String Keyword 
nameToKeyword s = case reads ('K':s) of
                       [(x, "")] -> Right x
                       _ -> Left ("could not get keyword from string " ++ s)


data Literal 
  = LNull
  | LBool Bool
  | LChar Char
  | LString String
  | LInteger Integer
  | LFloat Double
  deriving (Show, Eq)  


data Separator
  = OpenParen
  | CloseParen
  | OpenCurly
  | CloseCurly
  | OpenSquare
  | CloseSquare
  | Comma
  | Semicolon
  | Period
  deriving (Show, Eq)
  
  
stringToSeparator :: Char -> Either String Separator
stringToSeparator s = check $ lookup s seps
  where seps = [('(', OpenParen),
                (')', CloseParen),
                ('{', OpenCurly),
                ('}', CloseCurly),
                ('[', OpenSquare),
                (']', CloseSquare),
                (',', Comma),
                (';', Semicolon),
                ('.', Period)]
        check Nothing = Left ("unable to translate separator " ++ [s])
        check (Just x) = Right x 
  
  
data Operator
  = No
  deriving (Show, Eq)


data Token
  = Identifier String
  | Keyword Keyword
  | Literal Literal
  | Separator Separator
  | Operator Operator
  deriving (Show, Eq)


data InputElement
  = Whitespace String
  | Comment String
  | Token Token
  deriving (Show, Eq)

