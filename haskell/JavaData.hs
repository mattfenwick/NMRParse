module JavaData (
    
    Identifier(..)
  , QualifiedIdentifier(..)
    
    
  , Type(..)
  , RType(..)
  , TypeArg(..)
  
    
) where

-- Section 1
  
newtype Identifier
    = Identifier String
  deriving (Show, Ord, Eq)


type QualifiedIdentifier = [Identifier]


-- Section 2



-- Section 3

data TypeArg
    = Extends RType   --  ? extends Number
    | Super RType     --  ? super Number
    | Plain RType     --  Integer
    | Wildcard        --  ?
  deriving (Show, Ord, Eq)


data RType
    = RType String [TypeArg]
  deriving (Show, Ord, Eq)


data Type
    = BasicType String
    | RefType RType
  deriving (Show, Ord, Eq)



-- Section 4

data TypeParameter
    = TypeParameter Identifier [RType]
  deriving (Show, Ord, Eq)



-- Section 5