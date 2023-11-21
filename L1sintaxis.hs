module L1sintaxis
    (
      Stm(..)
    , Prog(..)
    --, StmList(..)
    , StmList
    -------------
    , VarId
    , SimpleType(..)
    , Var(..)
    , VarList
    , ExpBasica(..)
    , ExpArith(..)
    , OpArith(..)
    -------------
    , AtomoBool(..)
    , OpComp(..)
    , ExpBool(..)
    , OpBoolUn(..)
    , OpBoolBin(..)
    )
--     Prog(..)
--     ,Var(..), VarList(..)
--     ,Val, Val03(..), Val01(..)
--     ,Exp(..)
--     ,Stm(..), StmList(..)
--     ,showStm, showVar, showVal03, showVal
--     ,varListTOhaskell, haskelListTOvarList
--     ,stmListTOhaskell, haskelListTOstmList
--     ,showProg
--     )
--
-- Sintaxis de L1
-- Un lenguanje imperativo "pequeño" y teoricamente Turing-completo..

--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S 
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------
--import Variables (Variable)
--

-- Sintaxis de L1 -------------------------------------------------
--
--
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
-- <SimpleType> ::= Int | [<Entero> .. <Entero>]
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


-- <Stm>        ::=   Halt | <AsigStm>
--                  | Read <VarId> | Write <VarId>
--                  | <IfThenStm> | <WhileStm>
--                  | <BlockStm>
--
-- <AsigStm>    ::= <VarId> := <ExpArith>
-- (IfThenStm>  ::= If <ExpBool> Then <Stm> Else <Stm>
-- <WhileStm>   ::= While <ExpBool> Do <Stm>
--
-- <BlockStm>   ::= { <StmList> }
-- <StmList>    ::= (<Stm> ;)*
                -- cero o mas veces "<Stm> ;"

-- <ProgL1>     ::= L1PROG <ProgName> VAR <VarList> PROG <Stm>
-- <ProgName>   ::= <LetraMay> (<LetraOdig>)*
-- <LetraMay>   ::= A | ... | Z


--------------------------------------------------------
--

-- newtype VarId = Id String
--                 deriving (Eq,Show)
-- --                deriving (Eq)
type VarId = String

-- <SimpleType> ::= Int | [<Entero>..<Entero>]
data SimpleType = Tint | Tm2n Int Int
--                 deriving (Eq,Show)
                deriving (Eq)
--
showSimpleType :: SimpleType -> [Char]
showSimpleType simType= case simType of
                        Tint        -> "Int"
                        Tm2n m n    -> "["++(show m)
                                    ++ ".."
                                    ++ (show n) ++ "]"
--
instance Show (SimpleType) where
    show= showSimpleType

-- <Var>       ::= <VarId> : <SimpleType>
data Var = Var (VarId, SimpleType)
            deriving (Eq,Show)
--

-- Listas de variables
type VarList = [Var] -- Usando listas de Haskell


----------------------------------------------
-- Expresiones Aritmeticas:

-- <ExpArith>   ::= <ExpBasica> | (<ExpArith> <OpArith> <ExpArith>)

-- <ExpBasica>  ::= <VarId> | <Entero>
data ExpBasica  = EBvar VarId
                | EBint Int
--                 deriving (Eq, Show)
                deriving (Eq)

data ExpArith = EAbasica ExpBasica
              | EAopArit (ExpArith,OpArith,ExpArith)
--     deriving (Eq, Show)
    deriving (Eq)
--
data OpArith = OAsum | OAmul | OAdif | OAdiv
--     deriving (Eq, Show)
    deriving (Eq)
--

----------------------------------------------
-- Expresiones Booleanas:

-- <AtomoBool>  ::= (<ExpBasica> <OpComp> <ExpBasica>)
data AtomoBool =  AtomoBool (ExpBasica, OpComp, ExpBasica)
--     deriving (Eq, Show)
    deriving (Eq)
--
-- <OpComp>     ::= = | < | > | != | <= | >=
data OpComp = OCequ | OCmen | OCmay | OCdif | OCmenEq | OCmayEq
--     deriving (Eq, Show)
    deriving (Eq)
--
-- <ExpBool>    ::= <AtomoBool> | <OpBoolUn> <ExpBool> | (<ExpBool> <OpBoolBin> <ExpBool>)
data ExpBool =
      EBatom AtomoBool
    | EBopUn (OpBoolUn,ExpBool)
    | EBopBin (ExpBool,OpBoolBin,ExpBool)
--     deriving (Eq, Show)
    deriving (Eq)
--

-- <OpBoolUn>   ::= ¬
data OpBoolUn = OpBnot
--     deriving (Eq,Show)
    deriving (Eq)
--

-- <OpBoolBin>  ::= "|" | & | ->
data OpBoolBin = OpBor | OpBand | OpBimp
--     deriving (Eq,Show)
    deriving (Eq)
--

----------------------------------------------
-- Instrucciones (statements)
-- <Stm>        ::=   Halt | <AsigStm>
--                  | Read <VarId> | Write <VarId>
--                  | <IfThenStm> | <WhileStm>
--                  | <BlockStm>
--
data Stm
    = Sasig VarId ExpArith
    | Shalt
    | Sread VarId
    | Swrite VarId
    | SIfThen ExpBool Stm Stm
    | SWhile ExpBool Stm
    | SblockStm [Stm]
--     deriving (Eq, Show)
   deriving (Eq)
--

--
--
-- <BlockStm>   ::= { <StmList> }
-- <StmList>    ::= (<Stm> ;)* -- Una lista de cero o mas stm's separados por ;

--
-------------------------------------------------------------------------------
--

--
showExpBasica :: ExpBasica -> String
showExpBasica eb =
    case eb of
        EBvar vId   -> vId
        EBint n     -> show n
--
instance Show (ExpBasica) where
    show = showExpBasica
--

--
showExpArith :: ExpArith -> String
showExpArith ea =
    case ea of
        EAbasica eb             -> show eb
        EAopArit (ea1,opA,ea2)  -> "("  ++ (show ea1)
                                        ++ (show opA)
                                        ++ (show ea2) ++ ")"
--
instance Show (ExpArith) where
    show = showExpArith
--
showOpArith :: OpArith -> [Char]
showOpArith opA =
    case opA of
        OAsum   -> " + "
        OAmul   -> " * "
        OAdif   -> " - "
        OAdiv   -> " / "
--
instance Show (OpArith) where
    show = showOpArith
--

--
showAtomoBool :: AtomoBool -> [Char]
showAtomoBool (AtomoBool (eb1,opC,eb2)) =
    (show eb1) ++ (show opC) ++ (show eb2)
--
instance Show (AtomoBool) where
    show = showAtomoBool
--
showOpComp :: OpComp -> String
showOpComp opComp=
    case opComp of
        OCequ   -> " = "
        OCmen   -> " < "
        OCmay   -> " > "
        OCdif   -> " != "
        OCmenEq -> " <= "
        OCmayEq -> " >= "
--
instance Show (OpComp) where
    show = showOpComp
--

--
showExpBool :: ExpBool -> String
showExpBool eb = case eb of
    EBatom atomB                        -> show atomB
    EBopUn (opBoolUn,ebool)             -> show opBoolUn ++ (show ebool)
    EBopBin (ebool1,opBoolBin,ebool2)   ->
        "(" ++(show ebool1)
        ++ (show opBoolBin)
        ++ (show ebool2) ++ ")"
--
instance Show (ExpBool) where
    show = showExpBool
--
showOpBoolUn :: OpBoolUn -> String
showOpBoolUn opU = case opU of
    OpBnot  -> "¬ "
--
instance Show (OpBoolUn) where
    show = showOpBoolUn
--
showOpBoolBin :: OpBoolBin -> String
showOpBoolBin opBin = case opBin of
    OpBor   -> " | "
    OpBand  -> " & "
    OpBimp  -> " -> "
--
instance Show (OpBoolBin) where
    show = showOpBoolBin
--


showStm :: Stm -> String
showStm stm =
    case stm of
        (Sasig vId e)  -> vId++ ":=" ++ (show e)
        (Sread vId)    -> "Read "  ++ vId
        (Swrite vId)   -> "Write " ++ vId
        (Shalt)      -> "Halt"
        -----------
        (SIfThen eB s1 s2)  -> "If " ++ (show eB)
                                ++ " Then " ++ (show s1)
                                ++ " Else " ++ (show s2)
        (SWhile eB stmW)    -> "While " ++ (show eB) ++ " Do " ++ (show stmW)
        (SblockStm stmL)    -> "{" ++ (show stmL) ++ "}"
--
instance Show (Stm) where
    show = showStm
--
--

-- Programas de L1
-- <ProgL1>     ::= L1PROG <ProgName> VAR <VarList> PROG <Stm>
-- <ProgName>   ::= <LetraMay> (<LetraOdig>)*
-- <LetraMay>   ::= A | ... | Z
--
type StmList = [Stm]
--
data Prog
    = Prog (String, VarList, Stm)
    deriving (Eq,Show)   --(Eq,Show)
--

--
----------------------------------------
--

