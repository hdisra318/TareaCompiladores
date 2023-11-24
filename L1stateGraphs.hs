module L1stateGraphs
    ( StateGraph(..)
    , grafOfL1prog
    , grafOfL1progFrom
    , transOFlabStmList
    , transOFlabStm
    , showStateGraph
    , showStateGraphMN
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
import L1semanticaExp
--
import L1semanticaStm
--
import L1semanticaLabStm
--
-- Gráfica de transiciones de estados para L1 -----
--

-- Transiciones etiquetadas con instrucciones etiquetadas.
type LabTran2 = (EstadoVT,((Int,Int),Stm),EstadoVT)

--
-- Gráfica de transiciones de estados de un programa
data StateGraph = StateGraph ([EstadoVT], [LabTran2])
    deriving (Eq)
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
--
showStateGraphMN :: Int -> Int -> StateGraph -> String
showStateGraphMN m n (StateGraph (stateList, tranList ))=
        "States (short list):\n"
    ++ showListMN ind1 sep1 m n stateList
    ++  "Transitions (short list):\n"
    ++ showListMN ind1 sep1 m n tranList
    where
    ind1= "    " -- 4 espacios
    sep1= "\n"   -- newline
--

--
-- transOFlabStm :: LabStm->EstadoVT -> [(EstadoVT,((Int,Int),Stm),EstadoVT)]
-- -- Dados: una instruccion etiquetada, labStm,
-- --        y estado p,
-- -- regresa:
-- --      las transiciones, a partir de p, de labStm.
-- transOFlabStm labStm p = [(p,(lab,stm),q) | q <- semOFstm]
--     where
--     (lab,stm)   = partsOFlabStm labStm
--     semOFstm    = semStm stm p
--

--transOFWhile :: LabStm -> EstadoVT -> [(EstadoVT,((Int,Int),Stm),EstadoVT)]
transOFWhile :: (Int,Int)->ExpBool->LabStm->EstadoVT
                      -> [(EstadoVT, ((Int,Int),Stm), EstadoVT)]
-- Dados: una instruccion etiquetada, labStm,
--        y estado p,
-- regresa:
--      las transiciones, a partir de p, de labStm.
transOFWhile lab eb labS p =
    if ebSem
        then    nub $ transOFlabStm_labS_p
            ++  concat [transOFlabStm labWhile q | (_,_,q) <- transOFlabStm_labS_p]
        else [(p,(lab,stmWhile),p)]
    where
    labWhile = Lwhile lab eb labS
    ebSem   = semExpBool eb p
    transOFlabStm_labS_p = transOFlabStm labS p
    --whileTrans =  [(p,lab,q) | q <- transOFlabStm labStm p]
    --
    (_,stmWhile)   = partsOFlabStm labWhile
    --inLab = (mLab,mLab+1)
    --exitL = (mLab,nLab)
    --lab   = (mLab,nLab)
--

--
transOfLifThen :: (Int,Int) -> ExpBool -> LabStm -> LabStm -> EstadoVT
    -> [(EstadoVT, ((Int, Int), Stm), EstadoVT)]
transOfLifThen _ eb lS1 lS2 p =
    if ebSem
        then (transOFlabStm lS1 p)
        else (transOFlabStm lS2 p)
    where
    ebSem   = semExpBool eb p
    --ifTrans =  [(p,lab,q) | q <- transOFlabStm labStm p]


--
transOFlabStm :: LabStm -> EstadoVT -> [(EstadoVT,((Int,Int),Stm),EstadoVT)]
-- Dados: una instruccion etiquetada, labStm,
--        y estado p,
-- regresa:
--      las transiciones, a partir de p, de labStm.
transOFlabStm labStm p = case labStm of
    Lasig  _ _ _    -> [(p,(lab,stm),q) | q <- semStm stm p]
    Lread  _ _      -> [(p,(lab,stm),q) | q <- semStm stm p]
    Lwrite _ _      -> [(p,(lab,stm),q) | q <- semStm stm p]
    Lhalt  _        -> [(p,(lab,stm),q) | q <- semStm stm p]
    ----------------
    LifThen l eb lS1 lS2    -> transOfLifThen l eb lS1 lS2 p
    Lwhile l eb labS        -> transOFWhile l eb labS p
    LblockStm _ labStmList  -> transOFlabStmList labStmList p
    where
    (lab,stm)   = partsOFlabStm labStm
--

--
transOFlabStmList :: [LabStm]->EstadoVT -> [(EstadoVT,((Int,Int),Stm),EstadoVT)]
-- Dados: una lista de instrucciones etiquetadas, labStmList
--        y estado p,
-- regresa:
--      las transiciones, a partir de p, de labStmList.
transOFlabStmList labStmList p
    = case labStmList of
        []              -> []
        labStm : labStL'  ->     transOF_labS_p
                            ++ concat [transOFlabStmList labStL' q | q <- semOFstm]
                        where
                        transOF_labS_p = transOFlabStm labStm p
                        (_,stm)   = partsOFlabStm labStm
                        semOFstm    = semStm stm p
--

--
grafOfL1progFrom :: Prog -> [EstadoVT] -> StateGraph
-- Dados:
--      un programa de L1, p=(progName, varList, stm),
--      y una lista de estados iniciales, initStates,
-- regresa:
--      la Gráfica de transicion de estados de p, (progStates, progTrans),
--      a partir de los estados iniciales, initStates.
grafOfL1progFrom (Prog (_, _, stm)) initStates
    = StateGraph (initStates, progTrans)
        where
        progTrans = concat [transOFlabStm labStm sigma | sigma <- initStates]
        labStm  = stmTOlabStm 1 stm -- Instrucciones etiquetada
--

grafOfL1prog :: Prog -> StateGraph
-- Dados un programa de L1, p=(progName, varList, stm),
-- regresa la Gráfica de transicion de estados de p, (progStates, progTrans).
grafOfL1prog (Prog (_, lv, stm))
    = StateGraph (progStates, progTrans)
        where
        progStates  = nub (estadosVTOf lv) -- estados del programa
        progTrans   = concat [transOFlabStm labStm sigma | sigma <- progStates]
        labStm  = stmTOlabStm 1 stm -- Instrucciones etiquetada
--

------------------------------------------------------------
------------------------------------------------------------
--OTRO INTENTO:


--
-----------------------------------------------------------
--
-- An execution path ...

-- Tests: VER L1tests.hs

--
---------------------------------------------------------------
--
