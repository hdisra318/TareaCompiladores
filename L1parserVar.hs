module L1parserVar
    (
      tokenVarList
    , tokenVar
    , varIdToken
    , tokenPuntoYcoma
    , tokenPuntoPunto
    , dosPuntosToken
    -------------
    , varIdParser
    , varParser
    , enteroParser
    , letraMinParser
    , letraOdigParser
    , charParser
    )
--     varListToken
--     , varToken
--     ---------------
--     , parserVarList
--     , parserVar
--     , parserVx
--     , parserVy
--     , parserVz
--     )
--
-- Parser para variables de L0
--

where
import Data.Char
--
import HuttonParsing
import L1sintaxis


--


---------------------------------------------------------------------
--

-- Sintaxis de L1 usando notación BNF con * y +.
-- Donde, como en las expresiones regulares:
--      (X)* indica cero o mas elementos de X
--      (X)+ indica uno o mas elementos de X

-- <Var>       ::= <VarId> : <SimpleType>

-- <VarId>      ::= <LetraMin> (<LetraOdig>) *
-- <LetraMin>   ::= a | ... | z
-- <Letra>      ::= a | ... | z | A | ... | Z
-- <Dig>        ::= 0 | .. | 9
-- <LetraOdig>  ::= <Letra> | <Digito>

-- <SimpleType> ::= Int | [<Entero>..<Entero>]
-- <Entero>     ::= <Digitos> | - <Digitos>
-- <Digitos>    ::= (<Dig>)+

-- <VarList>    ::= (<Var> ; )+

-- <ExpArith>   ::= <ExpBasica> | (<ExpArith> <OpArith> <ExpArith>)
-- <ExpBasica>  ::= <VarId> | <Entero>
-- <OpArith>    ::= + | * | - | /

-- <AtomoBool>  ::= (<ExpBasica> <OpComp> <ExpBasica>
-- <OpComp>     ::= = | < | > | != | <= | >=

-- <ExpBool>    ::= <AtomoBool> | <OpBoolUn> (<ExpBool>) | (<ExpBool> <OpBoolBin> <ExpBool>)
-- <OpBoolUn>   ::= ¬
-- <OpBoolBin>  ::= "|" | & | ->

-- <Stm>        ::=   Halt | <AsigStm>
--                  | Read <Var> | Write <Var>
--                  | <IfThenStm> | <WhileStm>
--                  | <BlockStm>

-- <AsigStm>    ::= <Var> := <ExpArith>
-- (IfThenStm>  ::= If <ExpBool> Then <Stm> Else <Stm>
-- <WhileStm>   ::= While <ExpBool> do <Stm>

-- <BlockStm>   ::= { <StmList> }
-- <StmList>    ::= NillList | <Stm> ; <StmList>

-- <ProgL1>     ::= L1prog <ProgName> VAR <VarList> <BlockStm>


-- Como ident de Hutton:

charParser :: Char -> Parser Char
charParser x = sat (== x)

letraMinParser :: Parser Char
letraMinParser = sat isLower

letraParser :: Parser Char
letraParser = sat isAlpha

digitParser :: Parser Char
digitParser = sat isDigit

letraOdigParser :: Parser Char
letraOdigParser = letraParser <|> digitParser

varIdParser :: Parser String
-- <VarId>      ::= <LetraMin> (<LetraOdig>) *
varIdParser
    =   do
        x  <- letraMinParser
        xs <- many letraOdigParser
        return (x:xs)

varIdToken :: Parser String
varIdToken = token varIdParser


-- <SimpleType> ::= Int | [<Entero> : <Entero>]
-- <Entero>     ::= <Digitos> | -<Digitos>
-- <Digitos>    ::= (<Dig>)+
--
digitosParser :: Parser Int
digitosParser =
    do
    ds <- some digitParser
    return (read ds)

negDigsParser :: Parser Int
negDigsParser =
    do
    _ <- charParser '-'
    n <- digitosParser
    return (-n)

enteroParser :: Parser Int
enteroParser =
        negDigsParser
    <|> digitosParser



--

-- <SimpleType> ::= Int | [<Entero> : <Entero>]
intParser :: Parser SimpleType
intParser =
    do
    _ <- string "Int"
    return Tint
--
tIntToken :: Parser SimpleType
tIntToken = token intParser
--
lBracketToken :: Parser String
lBracketToken = symbolToken "["
--
rBracketToken :: Parser String
rBracketToken = symbolToken "]"
--
tokenPuntoPunto :: Parser String
tokenPuntoPunto = symbol ".."
--
m2nParser :: Parser SimpleType
m2nParser =
    do
    _ <- lBracketToken
    m <- enteroParser
    _ <- tokenPuntoPunto
    n <- enteroParser
    _ <- rBracketToken
    return(Tm2n m n)
--
m2nToken :: Parser SimpleType
m2nToken = token m2nParser
--
simpleTypeParser :: Parser SimpleType
simpleTypeParser =
        tIntToken
    <|> m2nToken
--
simpleTypeToken :: Parser SimpleType
simpleTypeToken = token simpleTypeParser

-- <Var>       ::= <VarId> : <SimpleType>
--
dosPuntosToken :: Parser String
dosPuntosToken = symbol ":"
--
--
varParser :: Parser Var
varParser =
    do
    v <- varIdToken
    _ <- dosPuntosToken
    t <- simpleTypeToken
    return (Var (v,t))
--
tokenVar :: Parser Var
tokenVar = token varParser

-- <VarList>    ::= (<Var> ; )+
tokenPuntoYcoma :: Parser String
tokenPuntoYcoma = symbol ";"
--
parserVarPuntoyComa :: Parser Var
parserVarPuntoyComa =
    do
    v <- tokenVar
    _ <- tokenPuntoYcoma
    return v
--
parserVarList :: Parser [Var]
parserVarList = many parserVarPuntoyComa
--
tokenVarList :: Parser [Var]
tokenVarList = token parserVarList
--
--

-- Tests: VER L1tests.hs

--

