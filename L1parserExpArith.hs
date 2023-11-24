module L1parserExpArith
    ( -- tokens:
      expArithToken
    , expBasicaToken
    , opArithToken
    , tokenLeftPar
    , tokenRightPar
    -- parsers:
    , expBasicaParser
    , sumParser
    , mulParser
    , opArithParser
    , expArithParser
    )
--
-- Parser para ExpArith de L1
--

where
-- import Data.Char
--
import HuttonParsing
import L1sintaxis
import L1parserVar


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

-- <ProgL1>     ::= L1prog <ProgName> VAR <VarList> PROG <BlockStm>

----------------------------------------------------------------

-- Pueden empezar, en L1parserExpArith.hs, con: parsers para los tokens +, *, -, y /
sumParser :: Parser OpArith
sumParser =
    do
    _ <- charParser '+' -- parser para suma
    return OAsum
--
mulParser :: Parser OpArith
mulParser =
    do
    _ <- charParser '*' -- parser para multiplcación
    return OAmul
--

-- completar ....

-- y luego, usando el operador de parsers para "alternativas", <|>,
--      construir el parser para <OpArith>.
opArithParser :: Parser OpArith
opArithParser =     sumParser
                <|> mulParser
-- completar ....

opArithToken :: Parser OpArith
opArithToken = token opArithParser

-- Después: un parser para <ExpBasica>
-- <ExpBasica>  ::= <VarId> | <Entero>
expBasicaParser :: Parser ExpBasica
expBasicaParser =
    do
        varId <- varIdParser     -- importado de L1parserVar
        return (EBvar varId)
    <|>
    do
        n <- enteroParser    -- importado de L1parserVar
        return (EBint n)
--
expBasicaToken :: Parser ExpBasica
expBasicaToken = token expBasicaParser
--

--
-- Y finalmente el parser para <ExpArith> ....
-- NO olvidar los parsers para los tokens "(" y ")", son importantes para evitar ambigüedad.
-- <ExpArith>   ::= <ExpBasica> | (<ExpArith> <OpArith> <ExpArith>)
--

-- <ExpArith>   ::= <ExpBasica> | ...
eaBasicaParser :: Parser ExpArith
eaBasicaParser =
    do
    eb <- expBasicaParser
    return (EAbasica eb)
--
--
-- <ExpArith>   ::= ... | (<ExpArith> <OpArith> <ExpArith>)
tokenLeftPar :: Parser String
tokenLeftPar = symbolToken "("
--
tokenRightPar :: Parser String
tokenRightPar = symbolToken ")"
--
eaOpAritParser :: Parser ExpArith
eaOpAritParser =
    do
    _   <- tokenLeftPar     -- parentesis "(" necesario para parsing
    ea1 <- expArithParser
    op  <- opArithToken
    ea2 <- expArithParser
    _   <- tokenRightPar    -- parentesis ")" necesario para parsing
    return (EAopArit (ea1,op,ea2))
--

-- <ExpArith>   ::= <ExpBasica> | (<ExpArith> <OpArith> <ExpArith>)
expArithParser :: Parser ExpArith
expArithParser =    eaBasicaParser
                <|> eaOpAritParser
--

--
-- TESTS:
-- ghci> parse expArithToken "(2 + 5)"  -- CON parentesis, parsing OK
-- [(EAop (EAbasica (EBint 2),OAsum,EAbasica (EBint 5)),"")]
-- ghci>
-- ghci> parse expArithToken "2 + 5"  -- FALTAN los parentesis ( ... )
-- [(EAbasica (EBint 2),"+ 5")]
-- ghci>
--
expArithToken :: Parser ExpArith
expArithToken = token expArithParser

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CODIGO DE  L1parserVar: ------------------------------------------------
-- ...

--
--

-- Tests: VER L1tests.hs

--
