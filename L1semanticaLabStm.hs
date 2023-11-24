module L1semanticaLabStm
    ( LabStm(..)
    , LabProg(..)
    , stmListTOlabStmList
    , stmTOlabStm
    , progTOlabelProg
    , partsOFlabStm
    , endLabOF
    )
--
-- StateGraphs
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S
--import Data.List as L
--
-- Modules defined in this project: ---------------------------------
import L1sintaxis
--
import L1estados
--
import L1global
--
--import L1semanticaExp
--
--import L1semanticaStm
--
-- Gráfica de transiciones de estados para L1 -----
--

--
-- Instrucciones etiquetadas con (Int,Int)
data LabStm =
      Lasig  (Int,Int) VarId ExpArith
    | Lread  (Int,Int) VarId
    | Lwrite (Int,Int) VarId
    | Lhalt  (Int,Int)
    -------
    | LifThen     (Int,Int) ExpBool LabStm LabStm
    | Lwhile      (Int,Int) ExpBool LabStm
    | LblockStm   (Int,Int) [LabStm]
    deriving (Eq,Show)
--

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
partsOFlabStm :: LabStm -> ((Int, Int), Stm)
partsOFlabStm labStm =
    case labStm of
        Lasig  labels vId ea    -> (labels, Sasig  vId ea)
        Lread  labels vId       -> (labels, Sread  vId)
        Lwrite labels vId       -> (labels, Swrite vId)
        Lhalt  labels           -> (labels, Shalt)
        -------
        LifThen labels eb ls1 ls2   -> (labels, SIfThen eb s1 s2)
                where
                    (_,s1) = partsOFlabStm ls1
                    (_,s2) = partsOFlabStm ls2
        Lwhile  labels eb lStm     -> (labels, SWhile eb stm)
                where
                    (_,stm) = partsOFlabStm lStm
        LblockStm labels labStmList -> (labels, SblockStm stmList)
                where
                    stmList = [stm | (_,stm) <- partsOFlabStmList]
                    partsOFlabStmList = [partsOFlabStm labS | labS <- labStmList]
--

--
endLabOF :: LabStm -> Int
endLabOF labStm =
    case labStm of
        Lasig (_,endLab)  _ _   -> endLab
        Lread (_,endLab)  _     -> endLab
        Lwrite (_,endLab) _     -> endLab
        Lhalt (_,endLab)        -> endLab
        -------
        LifThen (_,endLab) _ _ _    -> endLab
        Lwhile  (_,endLab) _ _      -> endLab
        LblockStm (_,endLab) _      -> endLab
--

--
ifThenTOlabStm :: Int -> (ExpBool, Stm, Stm) -> LabStm
ifThenTOlabStm n (eb,s1,s2)
    = LifThen (n,s2End) eb s1Lab s2Lab
    where
    s1Lab   = stmTOlabStm (n+1) s1
    s1End   = endLabOF s1Lab
    --
    s2Lab   = stmTOlabStm (s1End+1) s2
    s2End   = endLabOF s2Lab
--

whileTOlabStm :: Int -> (ExpBool, Stm) -> LabStm
whileTOlabStm m (eb,s)
    = Lwhile (m,labEnd) eb sLab
    where
    sLab    = stmTOlabStm (m+1) s
    labEnd  = endLabOF sLab
--     (_,sx)  = partsOFlabStm sLab
--     sLab'   = ((n+1,n),sx) -- next label is while
--

--

stmListTOlabStmList :: Int -> [Stm] -> [LabStm]
stmListTOlabStmList m stmList
    = case stmList of
        []      -> [] --[LblockStm []]
        s:ls    -> (sLab: lsLab)
                where
                sLab    = stmTOlabStm m s
                sLabEnd = endLabOF sLab
                lsLab   = stmListTOlabStmList sLabEnd ls
--

--

--
stmTOlabStm :: Int -> Stm -> LabStm
stmTOlabStm n stm =
    case stm of
        Sasig vId ea    -> Lasig  (n,n+1) vId ea
        Sread vId       -> Lread  (n,n+1) vId
        Swrite vId      -> Lwrite (n,n+1) vId
        Shalt           -> Lhalt  (n,n+1)
        -------
        SIfThen eb s1 s2    -> ifThenTOlabStm n (eb,s1,s2)
        SWhile eb s         -> whileTOlabStm n (eb,s)
        SblockStm stmL      -> blockStmTOlabStm n stmL
--
--
-- stmTOlabStm :: Int->Int -> Stm -> LabStm
-- stmTOlabStm m n stm =
--     case stm of
--         Sasig vId ea    -> Lasig  (m,n) vId ea
--         Sread vId       -> Lread  (m,n) vId
--         Swrite vId      -> Lwrite (m,n) vId
--         Shalt           -> Lhalt  (m,n)
--         -------
--         SIfThen eb s1 s2    -> ifThenTOlabStm m (eb,s1,s2)
--         SWhile eb s         -> whileTOlabStm m n (eb,s)
--         SblockStm stmL      -> blockStmTOlabStm m stmL
--

--blockStmTOlabStm :: Int -> StmList -> LabStm
blockStmTOlabStm :: Int -> [Stm] -> LabStm
blockStmTOlabStm n stmList = LblockStm blockLab stmListLab
    where
    stmListLab  = stmListTOlabStmList n stmList
    blockLab    = (n, endLabOF (last stmListLab))
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

--

--

data LabProg
    = LabProg (String, VarList, LabStm)
    deriving (Eq,Show)   --(Eq,Show)
--

--
progTOlabelProg :: Prog -> LabProg
progTOlabelProg (Prog (name, varL, stm)) =
    (LabProg (name, varL, labStm))
    where
    labStm  = stmTOlabStm 1 stm -- instruccion etiquetada

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
