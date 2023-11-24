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

-- Tests para L1 -------------------------------------------------------------
--
--
showListSep_L1 :: IO ()
showListSep_L1= putStr $ -- para ejecutar caracteres de escape.
                showListSep "    " ";\n" ([1,2,3,4]::[Int])
             -- showListSep ind     sep   lx
--


--
-- Variables --------------------------

xId :: String
xId = "x"

yId :: String
yId = "y"

t01 :: SimpleType
t01 = Tm2n 0 1

t07 :: SimpleType
t07 = Tm2n 0 7

x :: Var
x   = Var ("x", t07)
--
y :: Var
y   = Var ("y", t01)

-- Expresiones: ----------------------
eaCero :: ExpArith
eaCero  = (EAbasica (EBint 0))

ebCero :: ExpBasica
ebCero  = (EBint 0)

eaVy :: ExpArith
eaVy    = (EAbasica (EBvar "y"))

ebVy :: ExpBasica
ebVy    = (EBvar "y")

--
-- EstadosVT: -------------------------------
edosVTxy :: [EstadoVT]
edosVTxy = estadosVTOf [x,y] -- estadosOf [x,y]
--
-- Un estado:
sigmaVTx0y1 :: EstadoVT
sigmaVTx0y1 = edosVTxy !! 1     -- sigmaVTx0y1 = [(x,0),(y,1)]
--
x1y1VT :: EstadoVT
x1y1VT= modifEstadoVT sigmaVTx0y1 "x" 1
--

-- Asignaciones: ----------------------------
asigX0 :: Stm
asigX0  = Sasig xId eaCero  -- x:=0
--
asigXY :: Stm
asigXY  = Sasig xId eaVy    -- x:=y

-- Semantica: -------------------------------
ySemantica :: Int
ySemantica= semVar "y" x1y1VT

xSemantica :: Int
xSemantica= semVar "x"  x1y1VT

semAsigX0 :: [EstadoVT]
semAsigX0 = semStm asigX0 sigmaVTx0y1
--
semAsigXY :: [EstadoVT]
semAsigXY = semStm asigXY sigmaVTx0y1

-------------------
semStmTest :: IO ()
semStmTest  =
    do {
        putStrLn "-- Test semStm, Semántica de Instrucciones de L1:\n";

        putStrLn "show semAsigXY";
        putStrLn (show semAsigXY);

        putStrLn "show semAsigX0";
        putStrLn (show semAsigX0);

        putStrLn "show xSemantica";
        putStrLn (show xSemantica);

        putStrLn "show ySemantica";
        putStrLn (show ySemantica);

        putStrLn "show ebVy";
        putStrLn (show ebVy);

        putStrLn "show yId";
        putStrLn (show yId);

        putStrLn "show ebCero";
        putStrLn (show ebCero);

        }
----------------------------
--

-- Listas de variables: -------------


lv0 :: VarList
lv0 = [Var ("z",t01)]       -- lv0=[]
--
lv1 :: VarList
lv1 = Var ("y",t01): lv0    -- lv1=[y,z]
--
lv2 :: VarList
lv2 = Var ("x",t01): lv1   -- lv2=[x,y,z]
--
lv3 :: VarList
lv3 = [Var ("x",t01)]      -- lv3=[x]
--

-- Instrucciones: -------------------
--

while1 :: Stm
while1 = SWhile (EBatom (AtomoBool (EBvar "x", OCmenEq, EBint 5)))
                (Sasig "x" (EAopArit (EAbasica (EBvar "x"),OAsum,EAbasica (EBint 1))) )
--

-- Semantica: -----------------------
while1Sem :: [EstadoVT]
while1Sem = semStm while1 sigmaVTx0y1
--

-------------------------------------
-- Tests grafica de estados: --------

-- Test grafica de estados del programa p1 en L1:

varL1 :: [Var]
varL1 = []  -- XXX

stmL1 :: StmList
stmL1 = []  -- XXX algo NO vacio

p1 :: Prog
p1 = Prog ("P1", varL1, SblockStm stmL1) -- p1 es un programa vacío.

p1Graph :: StateGraph
-- Grafica de estados de p1
p1Graph = grafOfL1prog p1

p1Sem :: [EstadoVT]
p1Sem = semL1prog p1

--
-------------------------
-- Test grafica de estados del programa p2 en L1

varL2 :: [Var]
varL2 = [Var ("x",Tm2n 0 7)]

stmL2 :: StmList
stmL2 = [
        -- x:=1;
          Sasig "x" (EAbasica (EBint 1))
        -- Write x;
        , Swrite "x"
        -- while x<=5 x:= x+1;
        , SWhile -- while
            -- x <= 5
            --                          x     <=     5
            (EBatom (AtomoBool (EBvar "x", OCmenEq, EBint 5)))
            -- x:= x+1
            --      x :=                           x     +    1
            (Sasig "x" (EAopArit (EAbasica (EBvar "x"),OAsum,EAbasica (EBint 1))) )
        -- Write x;
        , Swrite "x"
        -- Halt;
        , Shalt
        ]
--

p2 :: Prog
p2 = Prog ("P2", varL2, SblockStm stmL2)

p2Graph :: StateGraph
-- Grafica de estados de p2
p2Graph = grafOfL1prog p2

sigma0 :: EstadoVT
-- Un estado para varL2 = [Var ("x",Tm2n 0 7)]
sigma0 = EstadoVT [("x",((Tm2n 0 7),0))]

p2GraphFrom :: StateGraph
-- Grafica de estados de p2 a partir de la lista de estados [sigma0].
p2GraphFrom = grafOfL1progFrom p2 [sigma0]

p2Sem :: [EstadoVT]
p2Sem = semL1prog p2
--
------

p3 :: Prog
p3 = Prog ("P3", [Var ("x",Tm2n 0 3)], SblockStm stmL3)

stmL3 :: StmList
stmL3 = [
        -- while x<=2 x:= x+1;
        SWhile -- while
            --                         x     <=     2
            (EBatom (AtomoBool (EBvar "x", OCmenEq, EBint 2)))
            --      x :=                           x     +     1
            (Sasig "x" (EAopArit (EAbasica (EBvar "x"),OAsum,EAbasica (EBint 1))) )
        , Shalt
        ]
--

p3Graph :: StateGraph
-- Grafica de estados de p3
p3Graph = grafOfL1prog p3

p3GraphFrom :: StateGraph
-- Grafica de estados de p3 a partir de la lista de estados.
p3GraphFrom = grafOfL1progFrom p3 [EstadoVT [("x",((Tm2n 0 3),0))]]

--------
--
xleq2 :: ExpBool
xleq2 = (EBatom (AtomoBool (EBvar "x", OCmenEq, EBint 2)))

xGETxsum1 :: Stm
xGETxsum1 = (Sasig "x" (EAopArit (EAbasica (EBvar "x"),OAsum,EAbasica (EBint 1))) )

while_xleq2_xsum1 :: Stm
while_xleq2_xsum1= (SWhile xleq2 xGETxsum1)

labWhile_xleq2_xsum1 :: LabStm
labWhile_xleq2_xsum1 = stmTOlabStm 1 while_xleq2_xsum1

p4 :: Prog
p4 = (Prog ("P4", [Var ("x",Tm2n 0 3)], while_xleq2_xsum1))
-- p4 deberia estar definido en un archivo de texto para hacer parsing.
-- Por ejemplo, para L0, el archivo de texto se puede leer con
-- la funcion readL0prog del modulo L0textIO.hs
--
-- Parte de sintaxis de L1:
-- <ProgL1>     ::= L1PROG <ProgName> VAR <VarList> PROG <Stm>
-- <WhileStm>   ::= While <ExpBool> Do <Stm>
-- <BlockStm>   ::= { <StmList> }
-- <StmList>    ::= (<Stm> ;)*
--
-- El archivo de texto para p4 seria algo parecido a (VER sintaxis de L1):
-- L1PROG P4
-- VAR
--     x: [0..3];
-- PROG
--     While x <= 2 Do
--         x:=(x + 1)
--

initStates0 :: [EstadoVT]
initStates0 = [EstadoVT [("x",((Tm2n 0 3),0))]]

-- p4GraphFrom :: StateGraph
-- p4GraphFrom = grafOfL1progFrom
--                 (Prog ("P4", [Var ("x",Tm2n 0 3)], [while_xleq2_xsum1]))
--                 [EstadoVT [("x",((Tm2n 0 3),0))]]
p4GraphFrom :: StateGraph
-- Grafica de estados del programa p4 a partir de initStates0.
p4GraphFrom = grafOfL1progFrom p4 initStates0

--
-------------------------------------------------------------------------------
--
estadosTest :: IO ()
estadosTest  =
    let
    --
    in do {
        putStrLn "-- Test estados:\n";

        putStrLn "-- show (lv0,lv1,lv2,lv3)";
        putStrLn (show (lv0,lv1,lv2,lv3));

        }
--
--------------------------------------------------------------------
semanticaTest :: IO ()
semanticaTest  =
    do {
        putStrLn "-- Semantica:\n";

        putStrLn "-- show while1";
        putStrLn (show while1);

        putStrLn "-- show sigmaVTx0y1:";
        putStrLn (show sigmaVTx0y1);
        putStrLn "";

        putStrLn "-- show while1Sem = semStm while1 sigmaVTx0y1:";
        putStrLn (show while1Sem);
        putStrLn "";
        }
--


----------------------------------------------------------------------------------------
-- -- Test parsing L1 -----------
--

parseVarX1_int :: [(Var, String)]
parseVarX1_int = parse varParser "x1:Int bbbbb"

parseVarX2_07 :: [(Var, String)]
parseVarX2_07 = parse varParser "x2 : [0:7] bbbbb"


parseEAxSum1 :: [(ExpArith, String)]
parseEAxSum1 =  parse expArithToken "(x + 1)"

parseEA2mulX :: [(ExpArith, String)]
parseEA2mulX =  parse expArithToken "(2 * x)"


parseEB2 :: [(ExpBasica, String)]
parseEB2 = parse expBasicaToken "2"

parseEBx :: [(ExpBasica, String)]
parseEBx = parse expBasicaToken "x"
------------------------------

parseEBxMenEq2andXmayEq0 :: [(ExpBool, String)]
parseEBxMenEq2andXmayEq0 = parse parserExpBool "(x <= 2 & x >= 0) Do \n x:=(x + 1)"
------------------------------

parseWhileXle2Xge0 :: [(Stm, String)]
parseWhileXle2Xge0       = parse parserStm "While (x <= 2 & x >= 0) Do \n x:=(x + 1)"

------------------------------
p4Str :: String
p4Str= "L1PROG P4 \n VAR \n x: [0..3]; \n PROG \n While x <= 2 Do \n x:=(x + 1)"

p4Prog :: [(Prog, String)]
p4Prog = parse tokenProgL1 p4Str

------------------------------
readP4 :: IO ()
readP4 = readL1prog "./progP4.L1"

readAndShowP4 :: IO ()
readAndShowP4 = readAndShowL1prog "./progP4.L1"


------------------------------
p5Str :: String
p5Str = "L1PROG P5 \n VAR \n x: [0..3]; \n PROG \n While (x <= 2 & x >= 0) Do \n x:=(x + 1)"
--
p5Prog :: [(Prog, String)]
p5Prog = parse tokenProgL1 p5Str

--
-- ====================================================================================


-- PRUEBAS FOR

for1 = SFor xId (EAbasica (EBint 1)) (EAbasica(EBint 2)) (Sasig "x" (EAopArit (EAbasica (EBvar "x"),OAsum,EAbasica (EBint 1))))
for1Sem = semStm for1 sigmaVTx0y1