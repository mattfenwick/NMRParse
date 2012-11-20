module JavaAst (

    ASTNode(..)

) where


data ASTNode
    = AIdent    String
    | AQIdent   [ASTNode]          -- qualified identifier
    | AQIdentList [ASTNode]        -- qualified identifier list
    | AAnnoDecl ASTNode ASTNode    -- annotation type declaration
    | AIntfDecl ASTNode [ASTNode] [ASTNode] ASTNode      -- normal interface declaration
    | ABType String
    | ARefType [ASTNode]                             -- parameterized types
    | AArrayInit [ASTNode]                           -- element values
    | APair ASTNode ASTNode                          -- identifier, element value
    | AEnumDecl ASTNode [ASTNode] ASTNode            -- identifier, implements, body
    | AClassDecl ASTNode [ASTNode] (Maybe ASTNode) [ASTNode] ASTNode
    | ACOrIDecl [ASTNode] ASTNode                    -- class or enum declaration
    | AImport Bool [ASTNode] Bool                    -- is static, qualfied identifier, is *
    | APackDecl [ASTNode] ASTNode                    -- annotations, qualified identifier
    | ACompUnit (Maybe ASTNode) [ASTNode] [ASTNode]  -- package declaration, imports, types
    | ATypeArg String ASTNode                        -- extends/super, type
    | AWildcard
    | AParamType ASTNode [ASTNode]                   -- identifier, type args (parameterized type)
    | AType ASTNode Bool                             -- base type, is array
    | ABound [ASTNode]                               -- types
    | ATypeParam ASTNode ASTNode                     -- identifier, bounds
    | AAnno ASTNode [ASTNode]
    | AModifier String                               -- static, public, ... etc.
  deriving (Show, Eq, Ord)