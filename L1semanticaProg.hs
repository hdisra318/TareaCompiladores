module L1semanticaProg
    (
      semL1prog
    )
--
-- Semántica de programas de L1
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
-- import L1semanticaExp
--
import L1semanticaStm
--
-- Semántica de L1 -------------------------------------------------
--

--
-- semStmList :: StmList -> EstadoVT -> [EstadoVT]
-- -- Semántica de una lista de instrucciones ls en el estado sigma.
-- semStmList (StmList ls) sigma
--     = case ls of
--             s : ls' -> nub $ concat [ semStmList (StmList ls') t | t <- semOFs]
--                         where
--                             semOFs = semStm s sigma
--             []      -> [sigma] -- An empty list of statements is equivalent to Skip.
-- --

semL1prog :: Prog -> [EstadoVT]
-- Semántica de un programa de L1, p=(progName, varList, stm).
semL1prog (Prog (_, varList, stm))
    = concat $ nub [semStm stm sigma | sigma <- estadosProg]
        where
        estadosProg = nub (estadosVTOf varList) -- estados del programa
--

--

-- Tests: VER L1tests.hs

--
---------------------------------------------------------------
--

