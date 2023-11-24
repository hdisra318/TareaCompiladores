module L1stateGraphsOLD
    ( StateGraph(..)
    , LabTran
    , NumStm
    , grafOfL1prog
    , showStateGraph
    , showStateGraphMN
    ------------------
    )
--
-- StateGraphs
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S
import Data.List as L
--
-- Modules defined in this project: ---------------------------------
import L1sintaxis
--
import L1estados
--
import L1global
--
import L1semanticaStm
--
-- Gráfica de transiciones de estados para L1 -------------------------------------------------
--

-- Instrucciones numeradas con Int
type NumStm = (Int, Stm)

-- Transiciones etiquetadas con instrucciones numeradas.
type LabTran = (EstadoVT, NumStm, EstadoVT)

-- Gráfica de transiciones de estados de un programa
data StateGraph = StateGraph ([EstadoVT], [LabTran])
--
instance Show (StateGraph) where
    show = showStateGraph
--
showStateGraph :: StateGraph -> String
showStateGraph (StateGraph (stateList, tranList ))=
        "States:\n"
    ++ showListSep ind1 sep1 stateList
    ++  "Transitions:\n"
    ++ showListSep ind1 sep1 tranList
    where
    ind1= "    " -- 4 espacios
    sep1= "\n"   -- newline
--
--

showStateGraphMN :: Int->Int-> StateGraph -> String
showStateGraphMN m n (StateGraph (stateList, tranList ))=
        "States (short list):\n"
    ++ showListMN ind1 sep1 m n stateList
    ++  "Transitions:\n"
    ++ showListMN ind1 sep1 m n tranList
    where
    ind1= "    " -- 4 espacios
    sep1= "\n"   -- newline
--

--
semNumStmList :: [(Int, Stm)] -> EstadoVT -> [(EstadoVT, NumStm, EstadoVT)]
-- Semántica de una lista de instrucciones numeradas lns en el estado sigma.
semNumStmList lns sigma
    = case lns of
            []              -> []
            (n,s) : lns'    ->  [(sigma, (n,s), t) | t <- semOFs]
                                ++ concat [semNumStmList lns' t | t <- semOFs]
                            where semOFs = semStm s sigma
            --
--

--
grafOfL1prog :: Prog -> StateGraph
-- Dados un programa de L1, p=(progName, varList, stm),
-- regresa la Gráfica de transicion de estados de p, (progStates, progTrans).
grafOfL1prog (Prog (_, lv, stmL))
    = StateGraph (progStates, progTrans)
        where
        progStates  = nub (estadosVTOf lv) -- estados del programa
        progTrans   = concat [semNumStmList lns sigma | sigma <- progStates]
        lns         = (zip [1..] stmL) -- instrucciones numeradas
--

-----------------------------------------
--




--
-----------------------------------------------------------
--
-- An execution path ...

-- Tests: VER L1tests.hs

--
---------------------------------------------------------------
--
