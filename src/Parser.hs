module Parser (program, parse) where -- how to parse lives here (most of FIXMEs too)

import Syntax
import System.IO
import Control.Monad

import Text.Parsec

import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = words "true false var if else while break continue fun ref return try catch finally reset shift spawn detach join"
           , Token.reservedOpNames = words "+ - * / % == != < > <= >= && || ! ="
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
braces     = Token.braces     lexer -- parses surrounding braces
integer    = Token.integer    lexer -- parses an integer
natural    = Token.natural    lexer -- parses a natural number
semi       = Token.semi       lexer -- parses a semicolon
symbol     = Token.symbol     lexer -- parses one of the ops
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep   = Token.commaSep   lexer -- parses a comma separated list
stringlit = Token.stringLiteral lexer 
top = parse program

program = do
  whiteSpace
  ss <- many statement
  eof
  return $ SBlock $ foldr SSeq SSkip ss

statement =
  empty <|>
  ifStmt <|>
  loneIfStmt <|>
  whileStmt <|>
  block <|>
  returnStmt <|>
  tryStmtWithFinally <|>
  tryStmt <|>
  throwStmt <|>
  breakStmt <|>
  continueStmt <|>

  varDeclStmt <|>
  assignStmt <|>
  exprStmt

empty = semi >> return SSkip
loneIfStmt = do
  reserved "if"
  e <- parens expr
  s1 <- statement
  return $ SIf e s1 SSkip 
ifStmt = try $ do
  SIf e s1 SSkip <- loneIfStmt
  reserved "else"
  s2 <- statement
  return $ SIf e s1 s2
whileStmt = do
  reserved "while"
  e <- parens expr
  s <- statement
  return $ SWhile e s
block = do
  ss <- braces (many statement)
  return $ SBlock $ foldr SSeq SSkip ss
assignStmt = do
  i <- try ( identifier >>= \j -> reservedOp "=" >> return j )
  e <- expr
  semi
  return $ SAssign i e
varDeclStmt = do
  reserved "var"
  i <- identifier
  reservedOp "="
  e <- expr
  semi
  return $ SVarDecl i e
returnStmt = do
  reserved "return"
  e <- expr
  semi
  return $ SReturn e
tryStmt = do
  reserved "try"
  t <- block
  reserved "catch"
  exName <- parens identifier
  c <- block
  return $ STry t c SSkip (EVar exName)
tryStmtWithFinally = try $ do
  STry t c SSkip exName <- tryStmt
  reserved "finally"
  f <- block
  return $ STry t c f exName
throwStmt = do
  reserved "throw"
  e <- expr
  semi
  return $ SThrow e
exprStmt = do
  e <- expr
  semi
  return $ SExpr e

-- break and continue statements
breakStmt = do
  reserved "break"
  semi
  return SBreak
continueStmt = do
  reserved "continue"
  semi
  return SContinue

expr = conjunction `chainl1` binOp "||"
conjunction = relation `chainl1` binOp "&&"
relation = do
  l <- (negation <|> summation)
  (anyBinOp (words "== != < <= > >=") >>= \o -> summation >>= \r -> return $ o l r) <|> return l
  
summation = term `chainl1` anyBinOp (words "+ -")
term = factor `chainl1` anyBinOp (words "* / %")
factor = literal <|> fun <|> atomicOrCall <|> ref 
literal = intLiteral <|> boolLiteral "false" False <|> boolLiteral "true" True <|> stringLiteral

intLiteral = natural >>= \i -> return $ EVal (VInt (fromInteger i))
boolLiteral s v = reserved s >> (return $ EVal (VBool v))
stringLiteral = stringlit >>= \s -> return $ EVal (VString s)

ref = reserved "ref" >> factor >>= \e -> return $ ERef e
deref = reservedOp "*" >> atomic >>= \e -> return $ EDeref e

binOp s = reservedOp s >> (return $ (\a b -> ECall (EVar ("__b" ++ s)) [a, b] []))
negation = reservedOp "-" >> (atomic <|> intLiteral) >>= \e -> return $ ENegation e

anyBinOp ops = foldl1 (<|>) (map binOp ops)

fun = do
  reserved "fun"
  pars <- parens (commaSep identifier)
  body <- block
  return $ EFun pars body

variable = identifier >>= return . EVar
resetExpr = do
  reserved "reset"
  f <- parens fun
  return $ EReset f
shiftExpr = do
  reserved "shift"
  f <- parens fun
  return $ EShift f
spawnExpr = do
  reserved "spawn"
  f <- parens expr
  return $ ESpawn f
detachExpr = do
  reserved "detach"
  tid <- parens expr
  return $ EDetach tid
joinExpr = do
  reserved "join"
  tid <- parens expr
  return $ EJoin tid

atomic = resetExpr <|> shiftExpr <|>
         spawnExpr <|> detachExpr <|> joinExpr <|>
         variable <|> parens expr <|> deref
atomicOrCall = do 
  a <- atomic
  argss <- many (parens (commaSep expr))
  return $ foldl (\a arg -> ECall a arg []) a argss
