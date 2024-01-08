module Parser where

import AST
import Text.Parsec ( noneOf, spaces, string, eof, many1, (<|>), try, parseTest, parse )
import Text.ParserCombinators.Parsec ( Parser )

-- parse :: String -> IO ()
-- parse =  parseTest (signature_ <* eof)

parse2signature :: String -> Signature
parse2signature s = case parse (signature_ <* eof) "" s of
     Left err -> error $ "failed"
     Right x -> x

signature_ :: Parser Signature
signature_ = Signature <$> name <*> colon <*> typeexpr

typeexpr :: Parser TypeExpr
typeexpr = try (Function <$> factor <*> to <*> typeexpr)
       <|> try (Constrained <$> factor <*> rightarrow <*> typeexpr)
       <|> (Single <$> factor)

typename :: Parser TypeName
typename = try (TypeWithModifier <$> name <*> typename)
       <|> (TypeName <$> name)
       <|> try (TypeWithTNCons <$> tnfactor <*> typename)
       <|> (TNCons <$> tnfactor)

factor :: Parser Factor
factor = try (SignatureFactor <$> lpar <*> signature_ <*> rpar)
     <|> try (TypeExprFactor <$> lpar <*> typeexpr <*> rpar)
     <|> (TypeNameFactor <$> lpar <*> typename <*> rpar)
     <|> (JustTypeName <$> typename)

tnfactor :: Parser TNFactor
tnfactor = TNFactor <$> lpar <*> typename <*> rpar

name :: Parser Name
name = spaces *> many1 (noneOf " -=():,>") <* spaces

colon :: Parser Colon
colon = Colon <$ (spaces *> string ":" <* spaces)

to :: Parser To
to = To <$ (spaces *> string "->" <* spaces)

rightarrow :: Parser RightArrow
rightarrow = RightArrow <$ (spaces *> string "=>" <* spaces)

lpar :: Parser LPar
lpar = LPar <$ (spaces *> string "(" <* spaces)

rpar :: Parser RPar
rpar = RPar <$ (spaces *> string ")" <* spaces)

s = Signature "func" Colon (Function (JustTypeName (TypeName "a")) To (Function (JustTypeName (TypeName "a")) To (Single (JustTypeName (TypeName "a")))))

t = Signature "func" Colon (Function (TypeExprFactor LPar (Function (JustTypeName (TypeName "Nat")) To (Single (JustTypeName (TypeName "Nat")))) RPar) To (Function (JustTypeName (TypeName "a")) To (Single (JustTypeName (TypeName "a")))))