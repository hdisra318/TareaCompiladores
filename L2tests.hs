module L1tests
--
-- Tests para L1
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------
--
import L1global
--
import L1sintaxis
--
import L1estados
--
-- Semantica: --------
import L1semanticaExp
--
import L1semanticaStm
--
import L1semanticaProg
--
-- Grafica de estados: ----
import L1semanticaLabStm
--
import L1stateGraphs
--
-- -- Parsers: ---------
import HuttonParsing
--
import L1parserVar
--
import L1parserExpArith
--
import L1parserExpBool
--
import L1parserStm
-- --
import L1parserProg
-- --
import L1textIO
--

-- Semantica L2
import L2semanticaStm

import L2textIO


------------------------------
----- Test para L2 (For) -----
------------------------------

-- Variable x
x = "x"

-- Desde x = 0
xInicio = EAbasica (EBint 0)

-- Hasta x = 1
xFin = EAbasica (EBint 1)

-- Instruccion z = z + 1
asignacion = Sasig "z"(EAopArit(EAbasica (EBvar "z"), OAsum, EAbasica (EBint 1)))

-- Estado x <- 0, z <- 0
sigmax0z0 :: EstadoVT
sigmax0z0   = EstadoVT
            [ ("x",((Tm2n 0 7),0))
            , ("z",((Tm2n 0 3),0))
            ]

-- Ejemplo de ejecucion For x := 0 To 1 Do z := z + 1
testFor = semFor x xInicio xFin asignacion sigmax0z0

-------------------------
----- Test para .L2 -----
-------------------------

archivo = "progSuma.L2"

testArchivo = readAndShowL2prog archivo