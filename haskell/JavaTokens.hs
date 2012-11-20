module JavaTokens (
    
    Keyword(..)
    
  , Literal(..)
  
  , Separator(..)
  
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
  
  
data Operator
  = Equals
  | GreaterThan
  | LessThan
  | ExclamationPoint
  | Tilda
  | QuestionMark
  | Colon
  | DoubleEquals
  | LessThanOrEquals
  | GreaterThanOrEquals
  | NotEquals
  | AndAnd
  | OrOr
  | PlusPlus
  | MinusMinus
  | Plus
  | Minus
  | Times
  | DivideBy
  | And
  | Or
  | ToThe
  | Percentage
  | DoubleLessThan
  | DoubleGreaterThan
  | TripleGreaterThan
  | PlusEquals
  | MinusEquals
  | TimesEquals
  | DivideByEquals
  | AndEquals
  | OrEquals
  | ToTheEquals
  | PercentageEquals
  | DoubleLessThanEquals
  | DoubleGreaterThanEquals
  | TripleGreaterThanEquals
  deriving (Show, Eq)


data Token
  = Identifier String
  | Keyword Keyword
  | Literal Literal
  | Separator Separator
  | Operator Operator
  | AtSign
  | Ellipsis
  deriving (Show, Eq)


data InputElement
  = Whitespace String
  | Comment String
  | Token Token
  deriving (Show, Eq)

