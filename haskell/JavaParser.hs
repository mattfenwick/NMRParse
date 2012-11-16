module JavaParser (

) where

import Java
import MParse
import Classes
import Instances
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import JavaTokens



-- ------------------
-- the 'real', fancy parsers
-- they'll be working on [Token]



-- Section 1: identifiers

identifier :: Parser Token Token
identifier = satisfy isIdent
  where
    isIdent (Identifier _)   =  True
    isIdent   _              =  False


qualifiedIdentifier :: Parser Token [Token] 
qualifiedIdentifier = fmap fst $ sepBy1 identifier (literal (Separator Period)) 


qualifiedIdentifierList :: Parser Token [Token]
qualifiedIdentifierList = fmap fst $ sepBy1 identifier (literal (Separator Comma))



-- Section 2:  imports, declarations


extsClause :: Parser Token [Token]
extsClause = opt [] $ fmap (:) (key Kextends) <*> typeList


impsClause :: Parser Token [Token]
impsClause = opt [] $ fmap (:) (key Kextends) <*> typeList


annotationTypeDeclaration :: Parser Token [Token]
annotationTypeDeclaration = fmap (:) (literal AtSign *> key Kinterface *> identifier) <*> annotationTypeBody


annotationTypeBody = error "TODO!"


normalInterfaceDeclaration :: Parser Token [Token]
normalInterfaceDeclaration = fmap f (key Kinterface) <*> identifier <*> opt [] typeParameters <*> extsClause <*> interfaceBody
  where
    f _ i ps es b = i : concat [ps,es,b]


interfaceBody = error "TODO"


enumDeclaration :: Parser Token [Token]
enumDeclaration = fmap (\_ x y z -> x : (y ++ z)) (key Kenum) <*> identifier <*> impsClause <*> enumBody


enumBody = error "TODO"


normalClassDeclaration :: Parser Token [Token]
normalClassDeclaration = fmap f (key Kclass) <*> identifier <*> opt [] typeParameters <*> extsClause <*> impsClause <*> classBody
  where
    f _ i ts es is b = i : concat [ts, es, is, b]


classBody = error "TODO"


interfaceDeclaration :: Parser Token [Token]
interfaceDeclaration = normalInterfaceDeclaration <|> annotationTypeDeclaration


classDeclaration :: Parser Token [Token]
classDeclaration = normalClassDeclaration <|> enumDeclaration


classOrInterfaceDeclaration :: Parser Token [Token]
classOrInterfaceDeclaration = fmap (\x y -> concat x ++ y) (many modifier) <*> (classDeclaration <|> interfaceDeclaration)


typeDeclaration :: Parser Token [Token]
typeDeclaration = classOrInterfaceDeclaration <|> fmap (:[]) (sep Semicolon)


importDeclaration :: Parser Token [Token]
importDeclaration = fmap f (key Kimport) <*> stat <*> fmap fst (sepBy1 identifier (sep Period)) <*> star <*> sep Semicolon
  where
    f _ s is st _ = s ++ is ++ st
    stat = opt [] (fmap (:[]) $ key Kstatic)
    star = opt [] (fmap (:[]) $ op Times)



-- Section 3:  types

opt :: a -> Parser t a -> Parser t a
opt x p = p <|> pure x


key :: Keyword -> Parser Token Token
key = literal . Keyword


sep :: Separator -> Parser Token Token
sep = literal . Separator


op :: Operator -> Parser Token Token
op = literal . Operator


basicType :: Parser Token Token
basicType = mconcat $ map key [Kbyte, Kshort, Kchar, Kint, Klong, Kfloat, Kdouble, Kboolean]


typeArgument :: Parser Token [Token]
typeArgument = referenceType <|> (fmap (:) (op LessThan) <*> opt [] rest)
  where
    rest = fmap (:) (key Kextends <|> key Ksuper) <*> referenceType


typeArguments :: Parser Token [Token]
typeArguments = op LessThan *> fmap (concat . fst) (sepBy1 typeArgument (sep Comma)) <* op GreaterThan


referenceType :: Parser Token [Token]
referenceType = fmap (concat . fst) $ sepBy1 main (sep Period)
  where
    main = fmap (:) identifier <*> opt [] typeArguments


jtype :: Parser Token [Token]
jtype = fmap (++) ( fmap (:[]) basicType <|> referenceType) <*> opt [] array
  where
    array = fmap (\x y -> [x,y]) (sep OpenSquare) <*> sep CloseSquare


-- Section 4: generics

bound :: Parser Token [Token]
bound = fmap (concat . fst) $ sepBy1 referenceType (op And)


typeParameter :: Parser Token [Token]
typeParameter = fmap (\x y z -> x:y:z) identifier <*> key Kextends <*> bound


typeParameters :: Parser Token [Token]
typeParameters = op LessThan *> main <* op GreaterThan
  where
    main = fmap (concat . fst) $ sepBy1 typeParameter (sep Comma)


nonWildcardTypeArguments :: Parser Token [Token]
nonWildcardTypeArguments = op LessThan *> typeList <* op GreaterThan


typeList :: Parser Token [Token]
typeList = fmap (concat . fst) $ sepBy1 referenceType (sep Comma)


diamond :: Parser Token [Token]
diamond = pure [] <* op LessThan <* op GreaterThan


typeArgumentsOrDiamond :: Parser Token [Token]
typeArgumentsOrDiamond = diamond <|> typeArguments


nonWildcardTypeArgumentsOrDiamond :: Parser Token [Token]
nonWildcardTypeArgumentsOrDiamond = diamond <|> nonWildcardTypeArguments



-- Section 5:

elementValues :: Parser Token [Token]
elementValues = fmap (concat . fst) $ sepBy1 elementValue (sep Comma)


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
elementValueArrayInitializer :: Parser Token [Token]
elementValueArrayInitializer = sep OpenSquare *> main <* optional (sep Comma) <* sep CloseSquare
  where
    main = fmap (concat . fst) $ sepBy0 elementValue (sep Comma)


elementValue :: Parser Token [Token]
elementValue = annotation <|> expression1 <|> elementValueArrayInitializer


expression1 = error "TODO"

 
elementValuePair :: Parser Token [Token]
elementValuePair = fmap (\x _ y -> x:y) identifier <*> op Equals <*> elementValue


elementValuePairs :: Parser Token [Token]
elementValuePairs = fmap (concat . fst) $ sepBy1 elementValuePair (sep Comma)


annotationElement :: Parser Token [Token]
annotationElement = elementValuePairs <|> elementValue


annotation :: Parser Token [Token]
annotation = fmap (\_ xs ys -> xs ++ ys) (literal AtSign) <*> qualifiedIdentifier <*> rest
  where
    rest = opt [] (sep OpenParen *> opt [] annotationElement <* sep CloseParen)


annotations :: Parser Token [Token]
annotations = fmap concat $ some annotation


modifier :: Parser Token [Token]
modifier = annotation <|> fmap (:[]) others
  where
    others = mconcat $ map key [Kpublic, Kprotected, Kprivate, Kstatic, Kabstract, Kfinal, Knative, Ksynchronized, Ktransient, Kvolatile, Kstrictfp]



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