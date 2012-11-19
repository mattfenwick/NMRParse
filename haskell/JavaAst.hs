module JavaAst (

    ASTNode(..)

) where


data ASTNode
    = AIdent    String
    | AQIdent   [ASTNode]      -- qualified identifier
    | AQIdentList [ASTNode]  -- qualified identifier list
    | AExtends  [ASTNode]     -- extends clause
    | AImpls    [ASTNode]       -- implements clause
    | AAnnoDecl ASTNode ASTNode    -- annotation type declaration
    | AIntfDecl ASTNode [ASTNode] [ASTNode] ASTNode      -- normal interface declaration
    | ANWTArgs [ASTNode]                             -- type list
    | ABType String
    | ARefType [ASTNode]                             -- parameterized types
    | AArrayInit [ASTNode]                           -- element values
    | APair ASTNode ASTNode                          -- identifier, element value
    | AEnumDecl ASTNode [ASTNode] ASTNode            -- identifier, implements, body
    | AClassDecl ASTNode [ASTNode] (Maybe ASTNode) [ASTNode] ASTNode
    | ACOrIDecl ASTNode                              -- class or enum declaration
    | AImport Bool [ASTNode] Bool                    -- is static, qualfied identifier, is *
    | APackDecl [ASTNode] ASTNode                    -- annotations, qualified identifier
    | ACompUnit (Maybe ASTNode) [ASTNode] [ASTNode]  -- package declaration, imports, types
    | ATypeArg ASTNode ASTNode                       -- extends/super, type
    | AParamType ASTNode [ASTNode]                   -- identifier, type args (parameterized type)
    | AType ASTNode (Maybe ASTNode)                  -- base type, is array
    | ABound [ASTNode]                               -- types
    | ATypeParam ASTNode ASTNode                     -- identifier, bounds
    | AAnno ASTNode (Maybe ASTNode)
  deriving (Show, Eq, Ord)