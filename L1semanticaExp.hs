module L1semanticaExp
    (
    semExpArith
    , semExpBasica
    , semAtomoBool
    , semExpBool
    , semVar
    )
--
-- Semántica de expresiones de L1
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------
--
import L1sintaxis
--
import L1estados
--
-- Semántica de L1 -------------------------------------------------
--

semVar :: VarId-> EstadoVT -> Int
-- Semántica de una variable v en el estado sigma.
semVar v sigma
    = case maybeTval of
        Just (_,b)  -> b
        Nothing     -> error $ "semVar: variable inválida, v= " ++ (show v)
    where
    lvtb        = listOfEdoVT sigma
    maybeTval   = lookup v lvtb
--

--
semExpBasica :: ExpBasica -> EstadoVT -> Int
semExpBasica eb sigma
    = case eb of
           EBvar v  -> semVar v sigma
           EBint n  -> n
--

--
semExpArith :: ExpArith -> EstadoVT -> Int
-- Semántica de una expresión aritmetica ea en el estado sigma.
-- data ExpArith = EAbasica ExpBasica
--               | EAopArit (ExpArith,OpArith,ExpArith)
semExpArith ea sigma
    = case ea of
           EAbasica eb  ->
                        semExpBasica eb sigma    -- [[eb]]_sigma
           EAopArit (ea1,opA,ea2)
                        ->
                        case opA of
                            OAsum   -> ea1Sem + ea2Sem   -- [[ea1]]_sigma + [[ea2]]_sigma
                            OAmul   -> ea1Sem * ea2Sem
                            OAdif   -> ea1Sem - ea2Sem
                            OAdiv   -> ea1Sem `div` ea2Sem
                            where
                            ea1Sem = semExpArith ea1 sigma
                            ea2Sem = semExpArith ea2 sigma
--

--------------------
--

-- <AtomoBool>  ::= (<ExpBasica> <OpComp> <ExpBasica>)
-- <OpComp>     ::= = | < | > | != | <= | >=
semAtomoBool :: AtomoBool -> EstadoVT -> Bool
semAtomoBool (AtomoBool (eb1,opComp,eb2)) sigma =
    case opComp of
        --OCequ | OCmen | OCmay | OCdif | OCmenEq | OCmayEq
        OCequ   -> eb1Sem == eb2Sem
        OCmen   -> eb1Sem <  eb2Sem
        OCmay   -> eb1Sem >  eb2Sem
        OCdif   -> eb1Sem /= eb2Sem
        OCmenEq -> eb1Sem <= eb2Sem
        OCmayEq -> eb1Sem >= eb2Sem
    where
        eb1Sem = semExpBasica eb1 sigma
        eb2Sem = semExpBasica eb2 sigma
--

-- -- <AtomoBool>  ::= (<ExpBasica> <OpComp> <ExpBasica>)
-- data AtomoBool =  AtomoBool (ExpBasica, OpComp, ExpBasica)
--     deriving (Eq, Show)   --(Eq,Show)
-- --
-- -- <OpComp>     ::= = | < | > | != | <= | >=
-- data OpComp = OCequ | OCmen | OCmay | OCdif | OCmenEq | OCmayEq
--     deriving (Eq, Show)   --(Eq,Show)
-- --
-- -- <ExpBool>    ::= <AtomoBool> | <OpBoolUn> <ExpBool> | (<ExpBool> <OpBoolBin> <ExpBool>)
-- data ExpBool = EBatom AtomoBool
-- --            | <OpBoolUn> <ExpBool>
-- --            | (<ExpBool> <OpBoolBin> <ExpBool>)
--     deriving (Eq, Show)   --(Eq,Show)
-- --

-- <ExpBool>    ::= <AtomoBool> | <OpBoolUn> (<ExpBool>) | (<ExpBool> <OpBoolBin> <ExpBool>)
-- <AtomoBool>  ::= (<ExpBasica> <OpComp> <ExpBasica>)
--
-- <OpComp>     ::= = | < | > | != | <= | >=
-- <OpBoolUn>   ::= ¬
-- <OpBoolBin>  ::= "|" | & | ->   -- Disyuncion, conjuncion, impicacion.
semExpBool :: ExpBool -> EstadoVT -> Bool
semExpBool expBool sigma =
    case expBool of
        EBatom  atomBool        -> semAtomoBool atomBool sigma
        EBopUn  (opU,eBool)     -> semEBopUn (opU,eBool) sigma
        EBopBin (eb1,opbin,eb2) -> semEBopBin (eb1,opbin,eb2) sigma
--
-- <OpBoolUn>   ::= ¬
-- data OpBoolUn = OpBnot
--     deriving (Eq, Show)   --(Eq,Show)
--
semEBopUn :: (OpBoolUn, ExpBool) -> EstadoVT -> Bool
semEBopUn (opU,eBool) sigma =
    case opU of
        OpBnot  -> not (semExpBool eBool sigma)

--
-- -- <OpBoolBin>  ::= "|" | & | ->
-- data OpBoolBin = OpBor | OpBand | OpBimp
semEBopBin :: (ExpBool, OpBoolBin, ExpBool) -> EstadoVT -> Bool
semEBopBin (eb1,opbin,eb2) sigma =
    case opbin of
        OpBor   -> eb1Sem || eb2Sem
        OpBand  -> eb1Sem && eb2Sem
        OpBimp  -> (not eb1Sem) || eb2Sem
    where
        eb1Sem = semExpBool eb1 sigma
        eb2Sem = semExpBool eb2 sigma
--

--
-- data ExpBool = EB AtomoBool
-- --            | <OpBoolUn> (<ExpBool>) | (<ExpBool> <OpBoolBin> <ExpBool>)
--     deriving (Eq, Show)   --(Eq,Show)
--
--
--
-- --
-- -- <OpComp>     ::= = | < | > | != | <= | >=
-- -- <OpBoolUn>   ::= ¬
-- -- <OpBoolBin>  ::= "|" | & | ->

--------------------

--

-- Tests: VER L1tests.hs

--
