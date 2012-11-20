module JavaParser (

) where

import Java
import MParse
import Classes
import Instances
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import JavaTokens
import JavaAst



-- ------------------
-- the 'real', fancy parsers
-- they'll be working on [Token]


sepBy1Fst :: Parser t a -> Parser t b -> Parser t [a]
sepBy1Fst p = fmap fst . sepBy1 p


opt :: a -> Parser t a -> Parser t a
opt x p = p <|> pure x


key :: Keyword -> Parser Token Token
key = literal . Keyword


sep :: Separator -> Parser Token Token
sep = literal . Separator


op :: Operator -> Parser Token Token
op = literal . Operator



-- Section 1: identifiers

identifier :: Parser Token ASTNode
identifier = 
    getOne >>= \t -> case t of
                     (Identifier i) -> pure (AIdent i)
                     _              -> empty


qualifiedIdentifier :: Parser Token ASTNode
qualifiedIdentifier = 
    pure AQIdent <*>
    sepBy1Fst identifier (sep Period)


qualifiedIdentifierList :: Parser Token ASTNode
qualifiedIdentifierList = 
    pure AQIdentList <*>
    sepBy1Fst qualifiedIdentifier (sep Comma)



-- Section 2:  imports, declarations

-- TODO: can this parse a typelist of zero
-- types ... or are there some delimiters
-- to watch out for?
extsClause :: Parser Token [ASTNode]
extsClause = 
    key Kextends  *>
    typeList


impsClause :: Parser Token [ASTNode]
impsClause = 
    key Kimplements  *> 
    typeList


annotationTypeDeclaration :: Parser Token ASTNode
annotationTypeDeclaration = 
    pure AAnnoDecl    <*>
    (literal AtSign    *>
     key Kinterface    *>
     identifier)      <*>
     annotationTypeBody


annotationTypeBody = error "TODO!"


normalInterfaceDeclaration :: Parser Token ASTNode
normalInterfaceDeclaration =
    pure AIntfDecl          <*>
    (key Kinterface          *> 
     identifier)            <*>
     opt [] typeParameters  <*>
     opt [] extsClause      <*>
     interfaceBody


interfaceBody = error "TODO"


enumDeclaration :: Parser Token ASTNode
enumDeclaration = 
    pure AEnumDecl      <*>
    (key Kenum           *>
     identifier)        <*>
     opt [] impsClause  <*>
     enumBody


enumBody = error "TODO"


normalClassDeclaration :: Parser Token ASTNode
normalClassDeclaration = 
    pure AClassDecl         <*>
    (key Kclass              *> 
     identifier)            <*>
     opt [] typeParameters  <*>
     extendsOpt             <*> 
     opt [] impsClause      <*>
     classBody
  where
    extendsOpt = optional (key Kextends *> jtype)


classBody = error "TODO"


interfaceDeclaration :: Parser Token ASTNode
interfaceDeclaration = 
    normalInterfaceDeclaration    <|>
    annotationTypeDeclaration


classDeclaration :: Parser Token ASTNode
classDeclaration = 
    normalClassDeclaration    <|> 
    enumDeclaration


classOrInterfaceDeclaration :: Parser Token ASTNode
classOrInterfaceDeclaration = 
    pure ACOrIDecl         <*> 
    many modifier          <*> 
    (classDeclaration <|> interfaceDeclaration)


-- I think the empty ; rule is just to allow lots of
-- trailing semicolons ... it can probably be stuck
-- on to compilationUnit instead
typeDeclaration :: Parser Token ASTNode
typeDeclaration = 
    classOrInterfaceDeclaration         <|> 
    fmap message (sep Semicolon)
  where
    message = (const $ error "why is this in the Java grammar?")


importDeclaration :: Parser Token ASTNode
importDeclaration = 
    pure AImport                            <*>
    (key Kimport                             *>
     opt False (key Kstatic *> pure True))  <*>
     sepBy1Fst identifier (sep Period)      <*>
     opt False (op Times *> pure True)      <*
     sep Semicolon


packageDecl :: Parser Token ASTNode
packageDecl = 
    pure APackDecl            <*>
    many annotation           <*>
    (key Kpackage              *>
     qualifiedIdentifier      <*
     sep Semicolon)


compilationUnit :: Parser Token ASTNode
compilationUnit = 
    pure ACompUnit           <*>
    optional packageDecl     <*>
    many importDeclaration   <*>
    many typeDeclaration
    



-- Section 3:  types


basicType :: Parser Token ASTNode
basicType = mconcat $ map key2 [Kbyte, Kshort, Kchar, Kint, Klong, Kfloat, Kdouble, Kboolean]
  where
    key2 k = 
        literal (Keyword k)            *> 
        pure (ABType $ tail $ show k)


typeArgument :: Parser Token ASTNode
typeArgument = 
    referenceType           <|>
    fmap ATypeArg bounded
  where
    bounded =
        op QuestionMark >>
        optional (pure (,)        <*>
                  extendsOrSuper  <*>
                  referenceType)
    extendsOrSuper = 
        fmap (tail . show) (key Kextends <|> key Ksuper) -- TODO:  tail . show doesn't work


typeArguments :: Parser Token [ASTNode]
typeArguments =
    op LessThan                          *> 
    sepBy1Fst typeArgument (sep Comma)  <* 
    op GreaterThan


referenceType :: Parser Token ASTNode
referenceType =
    pure ARefType     <*>
    sepBy1Fst paramType (sep Period)
  where
    paramType = 
        pure AParamType       <*>
        identifier            <*> 
        opt [] typeArguments


jtype :: Parser Token ASTNode
jtype =
    pure AType                     <*>
    (basicType <|> referenceType)  <*>
    opt False (array *> pure True)
  where
    array = sep OpenSquare *> sep CloseSquare


-- Section 4: generics

bound :: Parser Token ASTNode
bound = 
    pure ABound     <*>
    sepBy1Fst referenceType (op And)


typeParameter :: Parser Token ASTNode
typeParameter = 
    pure ATypeParam   <*>
    identifier        <*> 
    (key Kextends      *> 
     bound)


typeParameters :: Parser Token [ASTNode]
typeParameters = 
    op LessThan                           *> 
    sepBy1Fst typeParameter (sep Comma)   <* 
    op GreaterThan


nonWildcardTypeArguments :: Parser Token [ASTNode]
nonWildcardTypeArguments = 
    op LessThan      *> 
    typeList        <* 
    op GreaterThan


typeList :: Parser Token [ASTNode]
typeList = sepBy1Fst referenceType (sep Comma)


diamond :: Parser Token ()
diamond = 
    pure ()         <* 
    op LessThan     <* 
    op GreaterThan


typeArgumentsOrDiamond :: Parser Token [ASTNode]
typeArgumentsOrDiamond = 
    (diamond *> pure [])       <|> 
    typeArguments


nonWildcardTypeArgumentsOrDiamond :: Parser Token [ASTNode]
nonWildcardTypeArgumentsOrDiamond = 
    (diamond *> pure [])       <|> 
    nonWildcardTypeArguments




-- Section 5:

elementValues :: Parser Token [ASTNode]
elementValues = sepBy1Fst elementValue (sep Comma)


{- this is very strange;  here are some examples:
		@SuppressWarnings({ })
		ArrayList q1 = new ArrayList();
		@SuppressWarnings({ ,})
		ArrayList q2 = new ArrayList();
		@SuppressWarnings(value = { })
		ArrayList q3 = new ArrayList();
		@SuppressWarnings(value = {, })
		ArrayList q4 = new ArrayList();
		@SuppressWarnings(value = {"unchecked", })
		ArrayList q5 = new ArrayList();
		@SuppressWarnings(value = {"unused", "rawtypes",})
		ArrayList q6 = new ArrayList();
-}
elementValueArrayInitializer :: Parser Token ASTNode
elementValueArrayInitializer = 
    pure AArrayInit          <*>
    (sep OpenCurly            *> 
     opt [] elementValues    <* 
     optional (sep Comma)    <* 
     sep CloseCurly)


elementValue :: Parser Token ASTNode
elementValue = annotation <|> expression1 <|> elementValueArrayInitializer


expression1 = error "TODO"


elementValuePair :: Parser Token ASTNode
elementValuePair = 
    pure APair      <*>
    identifier      <*> 
    (op Equals       *> 
     elementValue)


elementValuePairs :: Parser Token [ASTNode]
elementValuePairs = sepBy1Fst elementValuePair (sep Comma)


annotationElement :: Parser Token [ASTNode]
annotationElement = 
    elementValuePairs         <|> 
    fmap (:[]) elementValue


annotation :: Parser Token ASTNode
annotation = 
    pure AAnno             <*>
    (literal AtSign         *> 
     qualifiedIdentifier)  <*> 
     rest
  where
    rest = opt [] (sep OpenParen               *> 
                   opt [] annotationElement   <* 
                   sep CloseParen)


annotations :: Parser Token [ASTNode]
annotations = some annotation


modifier :: Parser Token ASTNode
modifier = annotation <|> others
  where
    others = mconcat $ map q [Kpublic, Kprotected, Kprivate, Kstatic, Kabstract, Kfinal, Knative, Ksynchronized, Ktransient, Kvolatile, Kstrictfp]
    q k = key k *> pure (AModifier (tail $ show k))


{-

-- Section 6: classes
{-
genericMethodOrConstructorRest :: Parser Token [Token]
genericMethodOrConstructorRest = first <|> second
  where
    first = fmap (\x y z -> x:y:z) (jtype <|> key Kvoid) <*> identifier <*> methodDeclaratorRest
    second = fmap (:) identifer <*> constructorDeclaratorRest


genericMethodOrConstructorDecl :: Parser Token [Token]
genericMethodOrConstructorDecl = fmap (++) typeParameters <*> genericMethodOrConstructorRest


constructorDeclaratorRest :: Parser Token [Token]
constructorDeclaratorRest =


voidMethodDeclaratorRest :: Parser Token [Token]
voidMethodDeclaratorRest


methodDeclaratorRest :: Parser Token [Token]
methodDeclaratorRest


classBody :: Parser Token [Token]
classBody = sep OpenCurly *> classBodyDeclaration <* sep CloseCurly
-}





-- Section 7:


{-
classOrInterfaceType :: ???
classOrInterfaceType = pseq3 f identifier (optional TypeArguments <what value??>) classOrInterfaceTypeRest
  where f = ???


arrayType :: Parser Token Token
arrayType = ignoreRight ptype $ pseq (literal $ Separator OpenSquare) (literal $ Separator CloseSquare)


referenceType :: Parser Token Token
referenceType = alt classOrInterfaceType arrayType


primitiveType :: Parser Token Token
primitiveType = pany $ map (literal . Keyword) [Kbyte, Kshort, Kint, Klong, Kchar, Kfloat, Kdouble, Kboolean]


ptype :: Parser Token Token
ptype = alt primitiveType referenceType




{-


-- constantExpression ?

leftHandSide :: Parser Token [Token]
leftHandSide = pany [expressionName, fieldAccess, arrayAccess]

-}
-}
-}