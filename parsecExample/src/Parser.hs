module Parser where

import Text.ParserCombinators.Parsec
import Control.Monad

newtype Prog = Prog [Prolog]

data Prolog = Module Id Char
            | Types [TypeDef]
            | Def [Relation] 


data Relation = Ratom Atom
              | Rdef Atom String Expr

data Expr = ExprBr Char Expr Char
          | ExprAt Atom
          | And Expr Expr
          | Or Expr Expr

data TypeDef = TypeDef String Type


data Type = TVar Var
          | TAtom Atom
          | TBr Char Type Char
          | Arrow Type Type


data Atom  = Core Id
           | Seq Id SeqAtom


data AtomBr = AtomB Char AtomBr Char
            | AtomA Atom
            | Variable Var


data SeqAtom = H AtomBr
             | T AtomBr SeqAtom
             | V Var
             | VS Var SeqAtom
             | I Id 
             | IS Id SeqAtom 

newtype Id = Id String


newtype Var = Var String

-------------------SHOW-------------------------
instance Show Prog where
  show s = myShowProg s


myShowProg (Prog [])     = ""
myShowProg (Prog [x])    = show x
myShowProg (Prog (x:xs)) = show x ++ show (Prog xs)

instance Show TypeDef where
  show s = myShowTypeDef s

myShowTypeDef (TypeDef s t) = "TypeDef " ++ s ++ show t ++ "\n"

instance Show Type where
    show s = myShowType s

myShowType (TVar v) = show v
myShowType (TAtom a) = show a
myShowType (TBr c a b) =  " (" ++ show a ++ ") "
myShowType (Arrow a b) = show a ++ " -> " ++ show b


instance Show Prolog where
    show s = myShowProlog s

myShowProlog (Def []) = ""
myShowProlog (Def [x]) = show x
myShowProlog (Def (x:xs)) = show x ++ show (Def xs)
myShowProlog (Types []) = ""
myShowProlog (Types [x]) = show x
myShowProlog (Types (x:xs)) = show x ++ show (Types xs)
myShowProlog (Module name dot)= "Module " ++ show name ++ "\n"

instance Show Relation where
    show s = myShowRelation s

myShowRelation (Rdef a c e) = "Def " ++  show a ++ ":-" ++ show e ++ ['\n']
myShowRelation (Ratom a) = "Def " ++ show a ++ ['\n']

instance Show Atom where
    show s = myShowAtom s

myShowAtom (Core a)  = show a 
myShowAtom (Seq b c) = "Atom (" ++ show b ++ show c ++ ")"

instance Show AtomBr where
    show s = myShowAtomBr  s

myShowAtomBr (AtomB obr a cbr) = " (" ++ (show a) ++ ") "
myShowAtomBr (AtomA a) = " (" ++ show a ++ ") "
myShowAtomBr (Variable v) = " (" ++ show v ++ ") "

instance Show SeqAtom where
    show s = myShowSeqAtom s

myShowSeqAtom (H a)   = show a
myShowSeqAtom (T a b) = show a ++ " " ++ show b
myShowSeqAtom (V v) = show v
myShowSeqAtom (VS v t) = show v ++ show t
myShowSeqAtom (I i) = show i
myShowSeqAtom (IS i t) = show i ++ show t

instance Show Id where
    show s = myShowId s

myShowId (Id s) = " (Id " ++ s ++ ") "

instance Show Var where
    show s = myShowVar s

myShowVar (Var s) = " (Var " ++ s ++ ") "

instance Show Expr where
    show s = myShowExpr s

myShowExpr (ExprBr obr e cbr) = [obr] ++ show e ++ [cbr]
myShowExpr (ExprAt atom)      =  show atom 
myShowExpr (And e1 e2)        = "And (" ++ show e1 ++ show e2 ++ ") "
myShowExpr (Or e1 e2)         = "Or (" ++ show e1 ++ show e2 ++ ") "


--------------------PARSE--------------------------------

parseString :: String -> Either ParseError Prog
parseString =
  parse (do r <- exprParser; eof; return r) ""


parseVar :: Parser String
parseVar = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  return (h:t)

parseIdent :: Parser String
parseIdent = do
  h <- (lower <|> char '_') 
  t <- many (alphaNum <|> char '_')
  guard $ (/= "module") $ (h:t)
  guard $ (/= "type") $ (h:t)
  return (h:t)

parseModule :: Parser Prolog
parseModule = do
  _    <- string "module"
  _    <- spaces
  name <- fmap Id parseIdent
  _    <- spaces
  dot  <- char '.'
  return (Module name dot)

parseAtom :: Parser Atom
parseAtom = 
  try (do 
    _ <- spaces
    h <- parseIdent
    _ <- spaces
    t <- parseSeqAtom
    return (Seq (Id h) t)
  ) <|>
  (do
    _ <- spaces
    h <- parseIdent
    _ <- spaces
    return (Core (Id h))
  )


parseAtomBr :: Parser AtomBr
parseAtomBr = 
  try (fmap AtomA parseAtom) <|>
  try (do
    obr <- char '('
    _   <- spaces
    e   <- parseAtomBr
    _   <- spaces
    cbr <- char ')'
    return (AtomB obr e cbr)
  ) <|>
  (do
    _ <- spaces
    v <- parseVar
    _ <- spaces
    return (Variable (Var v))
  )

parseSeqAtom :: Parser SeqAtom
parseSeqAtom =
    try (do 
      _    <- spaces
      _    <- char '('
      _    <- spaces
      val  <- parseAtomBr
      _    <- spaces
      _    <- char ')'
      _    <- spaces
      tail <- parseSeqAtom
      _    <- spaces
      return (T val tail)  
      ) <|>
    try (do 
      _    <- spaces
      _    <- char '('
      _    <- spaces
      val  <- parseAtomBr
      _    <- spaces
      _    <- char ')'
      _    <- spaces
      return (H val)  
      ) <|>
    try (do
      _ <- spaces
      v <- parseVar
      _ <- spaces
      t <- parseSeqAtom
      _ <- spaces
      return (VS (Var v) t)
    ) <|>
    try (do
      _ <- spaces
      v <- parseVar
      _ <- spaces
      return (V (Var v))
    ) <|>
    try (do
      _ <- spaces
      i <- parseIdent
      _ <- spaces
      t <- parseSeqAtom
      _ <- spaces
      return (IS (Id i) t)
    ) <|>
    fmap I (fmap Id parseIdent)


parseFactor :: Parser Expr
parseFactor =
  fmap ExprAt parseAtom <|>
  (do
    _ <- spaces
    _ <- char '('
    _ <- spaces
    e <- parseExpr
    _ <- spaces
    _ <- char ')'
    _ <- spaces
    return (ExprBr '(' e ')')
  )

parseList elem sep = do
  h <- elem
  t <- many (sep >> elem)
  return (h:t)

parseAnd =
  fmap (foldr1 And) $ parseList parseFactor (char ',')

parseExpr = 
  fmap (foldl1 Or) $ parseList parseAnd (char ';')

parseDef =
  try (do
        _ <- spaces
        h <- parseAtom
        _ <- spaces
        c <- string ":-"
        _ <- spaces
        t <- parseExpr
        _ <- char '.'
        return (Rdef h c t)
      ) <|>
  (do
    _ <- spaces
    h <- parseAtom
    _ <- spaces
    _ <- char '.'
    return (Ratom h)
  )

parseDefs = 
  (do
    h <- parseDef
    _ <- spaces
    t <- many parseDef
    return (h:t)
  )

parseType = 
  try (do
        _ <- spaces
        v <- parseVar
        _ <- spaces
        return (TVar (Var v))
      ) <|>
  try (do
        _ <- spaces
        a <- parseAtom
        _ <- spaces
        return (TAtom a)
      ) <|>
  try (do
        _   <- spaces
        obr <- char '('
        _   <- spaces
        v   <- parseTypes
        _   <- spaces
        cbr <- char ')'
        _   <- spaces
        return (TBr obr v cbr)
      ) 

parseList1 elem sep = do
  h <- elem
  t <- many (sep >> elem)
  return (h:t)


parseTypes =
  fmap (foldr1 Arrow) $ parseList parseType (string "->")  

parseTypeDef =
  (do
    _    <- spaces
    _    <- string "type"
    _    <- spaces
    name <- parseIdent
    _    <- spaces
    t    <- parseTypes
    _    <- char '.'
    return (TypeDef name t)
  )  

parseTypeDefs = 
  (do
    h <- parseTypeDef
    _ <- spaces
    t <- many parseTypeDef
    return (h:t)
  )
  
parseProlog1 = 
  try (do
    _ <- spaces
    t <- parseTypeDefs
    _ <- spaces
    return (Types t)
  ) <|>
  parseModule <|>
  (do
    _ <- spaces
    d <- parseDefs
    _ <- spaces
    return (Def d)
  ) 


exprParser = fmap Prog $ many parseProlog1

{-
parseListOr :: Parser Atom
parseListOr = do
  h <- parseAtom
  _ <- char '|'
  t <- parseVar
  return (Seq (Id "cons") h t)

parseListValue :: Parser Atom
parseListValue sep = 
  char ']' <|>
  (do
    _ <- spaces
    _ <- sep
    _ <- spaces
    h <- parseAtom
    _ <- spaces
    t <- parseListValue
    return (Seq (Id "cons") h t)
  )

parseList elem sep = do
  h <- elem
  t <- many (sep >> elem)
  return (h:t)

parseAtomList' :: Parser Atom
parseAtomList' sep = do 
  _ <- char '['
  _ <- spaces
  t <- parseListValue sep
  return (t)
-}
