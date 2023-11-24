module L1semanticaStm
    (
     semStm
     , semStmList
     , semAsig
     , semWhile
    )
--
-- Semántica de instrucciones de L1
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S 
import Data.List as L
--
-- Modules defined in this project: ---------------------------------
--
import L1sintaxis
--
import L1estados
--
import L1semanticaExp
--
-- Semántica de L1 -------------------------------------------------
--

--
-----------------------------------------------------------
semAsig :: VarId -> ExpArith -> EstadoVT -> [EstadoVT]
-- [[vId := ea]]_sigma = [sigma[vId <- e]]
--                       Lista de un solo estado sigma' que resulta de modificar sigma
--                       poniendo el valor de e en lugar de sigma[vId].
semAsig vId ea sigma = [modifEstadoVT sigma vId eaINsigma]
    where
    eaINsigma = semExpArith ea sigma
--

--
semRead :: VarId -> EstadoVT -> [EstadoVT]
semRead vId sigma = [modifEstadoVT sigma vId b | b <- valListOf v]
        where v = varOFvId sigma vId
        -- Lista de modificaciones de sigma, en la variable v, usando los valores posibles para v.
        -- [ sigma[v <- b] | b in dominio(v) ]
--

semWrite :: VarId -> EstadoVT -> [EstadoVT]
semWrite _ sigma = [sigma]

--     | SIfThen ExpBool Stm Stm
--
semIfThen :: ExpBool -> Stm->Stm -> EstadoVT -> [EstadoVT]
semIfThen expBool stm1 stm2 sigma
    = if (semExpBool expBool sigma)
         then (semStm stm1 sigma)
         else (semStm stm2 sigma)
--

--
semWhile :: ExpBool -> Stm -> EstadoVT -> [EstadoVT]
semWhile expBool stm sigma
    = if ebSem
         then nub $ concat [semWhile expBool stm edo | edo <- sigmaL']
         else [sigma]
    where
        ebSem   = semExpBool expBool sigma
        --semStmSigma = semStm stm sigma
        --xL  = semStmSigma
        sigmaL' = semStm stm sigma
--

-- FOR
semFor :: VarId -> ExpArith -> ExpArith -> Stm -> EstadoVT -> [EstadoVT]
semFor vId e1 e2 stm sigma
    = if ebSem
        then nub $ concat [semFor vId e1 e2 stm edo | edo <- sigmaL']
        else [sigma]
    where
        ebSem = semExpBool (EBatom (AtomoBool (EBvar vId, OCmenEq, obtenerExpBasica e2))) sigma
        -- aux1 = semExpArith e2
        -- aux2 = semVar vId sigma
        -- ebSem = aux1 == aux2
        sigmaL' = semStm stm sigma


obtenerExpBasica :: ExpArith -> ExpBasica
obtenerExpBasica (EAbasica expBasica) = expBasica
obtenerExpBasica (EAopArit _) = error "No es una ExpBasica"
------
-- semAtomoBool (AtomoBool (e2, OCequ, (semVar vId sigma))) sigma

-- semStmList :: StmList -> EstadoVT -> [EstadoVT]
-- -- Semántica de una lista de instrucciones ls en el estado sigma.
-- semStmList (StmList ls) sigma
--     = case ls of
--             s : ls' -> nub $ concat [ semStmList (StmList ls') t | t <- semOFs]
--                         where
--                             semOFs = semStm s sigma
--             []      -> [sigma] -- An empty list of statements is equivalent to Skip.
-- --

semStmList :: StmList -> EstadoVT -> [EstadoVT]
-- Semántica de una lista de instrucciones ls en el estado sigma.
semStmList stmList sigma
    = case stmList of
            s : ls' -> nub $ concat [ semStmList ls' t | t <- semOFs]
                        where semOFs = semStm s sigma
            []      -> [sigma] -- An empty list of statements is equivalent to Skip.
--

--
semBlockStm :: StmList -> EstadoVT -> [EstadoVT]
semBlockStm stmList sigma = semStmList stmList sigma

--
semStm :: Stm  -> EstadoVT -> [EstadoVT]
-- Semántica de una instruccion stm en el estado sigma.
semStm stm sigma
    = case stm of
        Sasig vId ea   -> semAsig vId ea sigma
                        -- [[vId := ea]]_sigma = [sigma[vId <- e]]
        Sread vId    -> semRead vId sigma
        Swrite vId   -> semWrite vId sigma
                        -- La instruccion write no modifica el estado actual, sigma.
        Shalt        -> [edoVTtoOmega sigma]
                        -- (Omega lvb) representa un estado final omega.
        -- Recursivas:
        SIfThen eBool stm1 stm2 -> semIfThen eBool stm1 stm2 sigma
        SWhile eBoolW stmW      -> semWhile eBoolW stmW sigma
        SblockStm stmL          -> semBlockStm stmL sigma
        SFor vId e1 e2 stmF     -> semFor vId e1 e2 stmF sigma
--


-- semL1prog :: Prog -> [EstadoVT]
-- -- Semántica de un programa de L1.
-- semL1prog (Prog (varL, stmL))
--     = concat $ nub [semStmList stmL t | t <- estadosProg]
--         where
--         estadosProg = nub (estadosVTOf varL) -- estados del programa
--

--

-- Tests: VER L1tests.hs

--
---------------------------------------------------------------
--

