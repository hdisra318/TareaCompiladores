module L1parserExpBool
    ( -- tokens:
      tokenExpBool
    , tokenOpComp
    , tokenOpBoolBin
    -- parsers:
    , parserExpBool
    , parserAtomBool
    , parserOpComp
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
import L1parserExpArith


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

-- <AtomoBool>  ::= (<ExpBasica> <OpComp> <ExpBasica>)
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
--

-- <OpComp>     ::= = | < | > | != | <= | >=
--data OpComp = OCequ | OCmen | OCmay | OCdif | OCmenEq | OCmayEq
--
tokenOCequ :: Parser OpComp
tokenOCequ =
    do
    _ <- symbol "="
    return OCequ
--
-- difOFeq c = true iff c is no '='
-- difOFeq = (/=) '='
--
parserOCmen :: Parser OpComp
-- parser de '<', sin confundir con "<="
parserOCmen = P (\inp ->
    case inp of
        '<':xs  -> case xs of
                    '=':_   -> [] -- el parser falla
                    _       -> [(OCmen,xs)] -- OK
        _       -> []) -- el parser falla
--
tokenOCmen :: Parser OpComp
tokenOCmen = token parserOCmen
--
parserOCmay :: Parser OpComp
-- parser de '>', sin confundir con ">="
parserOCmay = P (\inp ->
    case inp of
        '>':xs  -> case xs of
                    '=':_   -> [] -- el parser falla
                    _       -> [(OCmay,xs)] -- OK
        _       -> []) -- el parser falla
--
tokenOCmay :: Parser OpComp
tokenOCmay = token parserOCmay
--
tokenOCdif :: Parser OpComp
tokenOCdif     =
    do
    _ <- symbol "!="
    return OCdif
--
tokenOCmenEq :: Parser OpComp
tokenOCmenEq    =
    do
    _ <- symbol "<="
    return OCmenEq
--
tokenOCmayEq :: Parser OpComp
tokenOCmayEq   =
    do
    _ <- symbol ">="
    return OCmayEq
--
--
parserOpComp :: Parser OpComp
parserOpComp =
        tokenOCequ      -- =
    <|> tokenOCmen      -- <
    <|> tokenOCmay      -- >
    <|> tokenOCdif      -- !=
    <|> tokenOCmenEq    -- <=
    <|> tokenOCmayEq    -- >=
--
tokenOpComp :: Parser OpComp
tokenOpComp = token parserOpComp
--

-- <AtomoBool>  ::= (<ExpBasica> <OpComp> <ExpBasica>)
-- data AtomoBool =  AtomoBool (ExpBasica, OpComp, ExpBasica)
parserAtomBool :: Parser AtomoBool
parserAtomBool =
    do
--     _           <- tokenLeftPar
    ebasica1    <- expBasicaParser
    opComp      <- tokenOpComp
    ebasica2    <- expBasicaParser
--     _           <- tokenRightPar
    return (AtomoBool (ebasica1, opComp, ebasica2))
--


--
-- <OpBoolUn>   ::= ¬
-- data OpBoolUn = OpBnot
parserOpBnot :: Parser OpBoolUn
parserOpBnot =
    do
    _ <- charParser '¬'
    return OpBnot
--
parserOpBoolUn :: Parser OpBoolUn
parserOpBoolUn = parserOpBnot
--
tokenOpBoolUn :: Parser OpBoolUn
tokenOpBoolUn = token parserOpBoolUn
--

-- <OpBoolBin>  ::= "|" | & | ->
-- data OpBoolBin = OpBor | OpBand | OpBimp
--
parserOpBor :: Parser OpBoolBin
parserOpBor =
    do
    _ <- charParser '|'
    return OpBor
--
parserOpBand :: Parser OpBoolBin
parserOpBand =
    do
    _ <- charParser '&'
    return OpBand
--
parserOpBimp :: Parser OpBoolBin
parserOpBimp =
    do
    _ <- stringParser "->"
    return OpBimp
--
parserOpBoolBin :: Parser OpBoolBin
parserOpBoolBin =
        parserOpBor
    <|> parserOpBand
    <|> parserOpBimp
--
tokenOpBoolBin :: Parser OpBoolBin
tokenOpBoolBin = token parserOpBoolBin
--

--
-- <ExpBool>    ::= <AtomoBool> | <OpBoolUn> <ExpBool> | (<ExpBool> <OpBoolBin> <ExpBool>)
-- data ExpBool =
--       EBatom AtomoBool
--     | EBopUn (OpBoolUn,ExpBool)
--     | EBopBin (ExpBool,OpBoolBin,ExpBool)
--
parserEBatom :: Parser ExpBool
parserEBatom =
    do
    atomBool <- parserAtomBool
    return (EBatom atomBool)
--
parserEBopUn :: Parser ExpBool
parserEBopUn =
    do
    opBoolUn    <- tokenOpBoolUn
    expBool     <- parserExpBool
    return (EBopUn (opBoolUn,expBool))
--
parserEBopBin :: Parser ExpBool
parserEBopBin =
    do
    _           <- tokenLeftPar
    expBool1    <- parserExpBool
    opBoolBin   <- tokenOpBoolBin
    expBool2    <- parserExpBool
    _           <- tokenRightPar
    return (EBopBin (expBool1,opBoolBin,expBool2))
--
parserExpBool :: Parser ExpBool
parserExpBool =
        parserEBatom
    <|> parserEBopUn
    <|> parserEBopBin
--
tokenExpBool :: Parser ExpBool
tokenExpBool = token parserExpBool

----------------------------------------------------------------
----------------------------------------------------------------
--

