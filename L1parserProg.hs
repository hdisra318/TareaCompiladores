module L1parserProg
    (
    -- Tokens: -------
      tokenProgL1
    , tokenProgName
    , tokenL1PROG -- palabra reservada "L1PROG"
    -- Parsers: ------
    , parserProgL1      -- Parser para progamas L1
    , parserNullInput   -- Este parser tiene exito sii el input es "".

    )
--
-- Parser para programas de L1
--
where
--
import HuttonParsing
--
import L1sintaxis
--
--------------------
import L1parserVar
--
--import L1parserExp
--
import L1parserStm
--
---------------------------------------------------------------------
--


parserNullInput :: Parser ()
-- () es el tipo "Unit" con un solo elemento que se produce con el constructor ()
-- Es decir, () es un tipo, y () es un constructor.
-- Este parser tiene exito sii el input es "",
-- para verificar que, después de un programa, el resto del input=vacío.
parserNullInput
    = P (\inp -> case inp of
                    []      -> [((), "")]   -- tiene exito si inp es ""
                    (_:_)   -> []           -- falla si inp NO es ""
        )
--
-- No hace falta:
-- nullInputToken :: Parser ()
-- nullInputToken = token parserNullInput



--
-- <ProgL1>     ::= L1PROG <ProgName> VAR <VarList> PROG <Stm>
-- <ProgName>   ::= <LetraMay> (<LetraOdig>)*
-- <LetraMay>   ::= A | ... | Z
--
tokenL1PROG :: Parser String
tokenL1PROG = symbol "L1PROG"
--
parserProgName :: Parser String
parserProgName =
    do
    mayuscula       <- upper
    letraOdigList   <- many alphanum
    return (mayuscula : letraOdigList)
--
tokenProgName :: Parser String
tokenProgName = token parserProgName
--
--
-- <ProgL1> ::= L1PROG <ProgName> VAR <VarList> PROG <Stm>
parserProgL1 :: Parser Prog
parserProgL1 =
    do
    _           <- tokenL1PROG
    progName    <- tokenProgName
    _           <- symbol "VAR"
    varList     <- tokenVarList
    _           <- symbol "PROG"
    stm         <- tokenStm
    _           <- parserNullInput  -- tiene exito sii el input es "". Resto del input=vacío.
    return (Prog (progName, varList, stm))
--
tokenProgL1 :: Parser Prog
tokenProgL1 = token parserProgL1
--
------------------------------------------------------------
-- Tests:
-- *L1parserProg> :l L1parserProg.hs
-- Ok, four modules loaded.
-- *L1parserProg>
-- *L1parserProg> parse parserProgL1 "VAR   \n\t x;\n \t y\n PROG" :: [(Prog Val, String)]
-- [(L1-Program NOMBRE XXX
--     VAR
--         x;
--         y
--     PROG
--         Halt
-- ,"")]
-- it :: [(Prog Val, String)]
-- *L1parserProg>
-- *L1parserProg> parse parserProgL1 "VAR   \n\t x PROG" :: [(Prog Val, String)]
-- [(L1-Program NOMBRE XXX
--     VAR
--         x
--     PROG
--         Halt
-- ,"")]
-- it :: [(Prog Val, String)]
-- *L1parserProg>

--



