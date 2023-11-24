module L1parserStm
    (
      tokenStm
    , tokenStmList
    , tokenWhile
    , tokenDo
    ------------
    , parserStm
    , parserStmList
    , parserAsig
    , parserHalt
    , parserRead
    , parserWrite
    , expArithParser
    )
--
-- Parser para instrucciones de L1 | CAMBIOS
--
where

import HuttonParsing
--
import L1sintaxis
--
import L1parserVar
--
import L1parserExpArith
--
import L1parserExpBool

--
---------------------------------------------------------------------

-- Instrucciones de L1: ---------------
-- Sintaxis de L1 usando notación BNF con * y +.
-- Donde, como en las expresiones regulares:
--      (X)* indica cero o mas elementos de X
--      (X)+ indica uno o mas elementos de X

-- <VarList>    ::= (<Var> ; )+
-- <Var>        ::= <VarId> : <SimpleType>
--
-- <VarId>      ::= <LetraMin> (<LetraOdig>) *
-- <LetraMin>   ::= a | ... | z
-- <Letra>      ::= a | ... | z | A | ... | Z
-- <Dig>        ::= 0 | .. | 9
-- <LetraOdig>  ::= <Letra> | <Digito>
--
-- <SimpleType> ::= Int | [<Entero> : <Entero>]
-- <Entero>     ::= <Digitos> | -<Digitos>
-- <Digitos>    ::= (<Dig>)+


-- <ExpArith>   ::= <ExpBasica> | (<ExpArith> <OpArith> <ExpArith>)
-- <ExpBasica>  ::= <VarId> | <Entero>
-- <OpArith>    ::= + | * | - | /


-- <ExpBool>    ::= <AtomoBool> | <OpBoolUn> (<ExpBool>) | (<ExpBool> <OpBoolBin> <ExpBool>)
-- <AtomoBool>  ::= (<ExpBasica> <OpComp> <ExpBasica>)
--
-- <OpComp>     ::= = | < | > | != | <= | >=
-- <OpBoolUn>   ::= ¬
-- <OpBoolBin>  ::= "|" | & | ->


-- <Stm>        ::=   <AsigStm> | Halt
--                  | Read <Var> | Write <Var>
--                  | <IfThenStm> | <WhileStm>
--                  | <BlockStm>
--
-- <AsigStm>    ::= <Var> := <ExpArith>
-- (IfThenStm>  ::= If <ExpBool> Then <Stm> Else <Stm>
-- <WhileStm>   ::= While <ExpBool> do <Stm>
--
-- <BlockStm>   ::= { <StmList> }
-- <StmList>    ::= (<Stm> ;)*
                -- cero o mas veces "<Stm> ;"

-- <ProgL1>     ::= L1prog <ProgName> VAR <VarList> <Stm>
-- <ProgName>   ::= <LetraMay> (<LetraOdig>)*
-- <LetraMay>   ::= A | ... | Z

haltToken :: Parser String
haltToken = symbol "Halt"
--
parserHalt :: Parser (Stm)
parserHalt =
    do
    _ <- haltToken
    return Shalt
--


-- Parser para la instruccion de asignacion:
-- <AsigStm>    ::= <VarId> := <ExpArith>
--
asignToken :: Parser String
asignToken = symbol ":="
--
parserAsig :: Parser Stm
parserAsig=
    do
    vId <- varIdToken     -- definido en L1parserVar.hs
    _   <- asignToken     -- definido arriba
    ea  <- expArithToken  -- definido en L1parserExpArith
    return (Sasig vId ea)  -- ver data Stm en L1sintaxis.hs
--

-- ---------------------------------------
readToken :: Parser String
readToken = symbol "Read"
--
parserRead :: Parser (Stm)
parserRead =
    do
    _   <- readToken
    vId <- varIdToken
    return (Sread vId)
--
writeToken :: Parser String
writeToken = symbol "Write"
--
parserWrite :: Parser (Stm)
parserWrite =
    do
    _   <- writeToken
    vId <- varIdToken
    return (Swrite vId)
--

-- (IfThenStm>  ::= If <ExpBool> Then <Stm> Else <Stm>
-- ... SIfThen ExpBool Stm Stm
tokenIf :: Parser String
tokenIf = symbol "If"
--
tokenThen :: Parser String
tokenThen = symbol "Then"
--
tokenElse :: Parser String
tokenElse = symbol "Else"
--
parserIfThen :: Parser Stm
parserIfThen =
    do
    _       <- tokenIf
    expBool <- parserExpBool
    _       <- tokenThen
    stm1    <- parserStm
    _       <- tokenElse
    stm2    <- parserStm
    return (SIfThen expBool stm1 stm2)
--


--
-- <WhileStm>   ::= While <ExpBool> Do <Stm>
-- ... SWhile ExpBool Stm
tokenWhile :: Parser String
tokenWhile = symbol "While"
--
tokenDo :: Parser String
tokenDo = symbol "Do"
--
parserWhile :: Parser Stm
parserWhile =
    do
    _       <- tokenWhile
    expBool <- parserExpBool --definido en L1parserExpBool
    _       <- tokenDo
    stm     <- parserStm
    return (SWhile expBool stm)
--

-- <BlockStm>   ::= { <StmList> }
-- <StmList>    ::= (<Stm> ;)* -- Una lista de cero o mas stm's separados por ;
-- ... SblockStm [Stm]
tokenLlaveIzq :: Parser String
tokenLlaveIzq = symbol "{"
--
tokenLlaveDer :: Parser String
tokenLlaveDer = symbol "}"
--
parserStmPuntoYcoma :: Parser Stm
parserStmPuntoYcoma =
    do
    stm     <- parserStm
    _       <- tokenPuntoYcoma
    return stm
--
parserStmList :: Parser [Stm]
-- cero o mas veces "<stm> ;"
parserStmList = many parserStmPuntoYcoma
--
tokenStmList :: Parser [Stm]
tokenStmList = token parserStmList
--
parserBlock :: Parser Stm
parserBlock =
    do
    _       <- tokenLlaveIzq
    stmList <- parserStmList
    _       <- tokenLlaveDer
    return (SblockStm stmList)
--

-- CAMBIOS - for

tokenFor :: Parser String
tokenFor = symbol "For"
--
tokenEq :: Parser String
tokenEq = symbol "="
--
tokenTo :: Parser String
tokenTo = symbol "To"

parserFor :: Parser Stm
parserFor =
     do
    _        <- tokenFor
    vId      <- varIdToken -- Definido en L1parserVar.hs
    _        <- tokenEq
    expArit1 <- expArithParser --definido en L1parserExpArith.hs
    _        <- tokenTo
    expArit2 <- expArithParser --definido en L1parserExpArith.hs
    _        <- tokenDo
    stm      <- parserStm
    return (SFor  vId  expArit1 expArit2 stm)

-- CAMBIOS

--
-- ---------------------------------------
--

-- Instrucciones (statements)
-- <Stm>        ::=   Halt | <AsigStm>
--                  | Read <VarId> | Write <VarId>
--                  | <IfThenStm> | <WhileStm>
--                  | <BlockStm>
--
parserStm :: Parser (Stm)
parserStm
    =   parserHalt
    <|> parserAsig
    <|> parserRead
    <|> parserWrite
    <|> parserIfThen
    <|> parserWhile
    <|> parserBlock
    <|> parserFor
-- --
--
tokenStm :: Parser Stm
tokenStm = token parserStm
--
--


