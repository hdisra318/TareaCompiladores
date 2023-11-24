module L2textIO
    (
      readL2prog
    , readAndShowL2prog
    -------------------
    , parseProgStms
    , checkParsingProgStms
    , progStms
    , progStmsB
    , progStmsStr
    )
--
where

import System.IO
-- import Control.Monad
--
import HuttonParsing
--
import L1sintaxis
--
import L2parserProg
-- import L1parserExp
--
---------------------------------------
--
readL2prog :: FilePath -> IO ()
-- Acciones Haskell para:
--      leer un archivo de texto que contiene un programa L2
--      aplicar parsing
--      regresar el resultado del parsing
readL2prog fileName
    = do
        -- abrir (declarar) el archivo:
        handle      <- openFile fileName ReadMode
        -- leer el contenido del archivo:
        textProgL2  <- hGetContents handle
        -- aplicar parsing
        let progL2 = (parse tokenProgL2 textProgL2)
        -- regresa proL2:
        print progL2
        hClose handle -- cerrar el archivo.
--

readAndShowL2prog :: FilePath -> IO ()
-- Acciones Haskell para:
--      leer un archivo de texto que contiene un programa L2
--      aplicar parsing
--      mostrar el resultado del parsing
readAndShowL2prog fileName
    = do
        -- abrir (declarar) el archivo:
        handle      <- openFile fileName ReadMode
        -- leer el contenido del archivo:
        textProgL2  <- hGetContents handle
        -- aplicar parsing al contenido del archivo:
        let progL2  = parse tokenProgL2 textProgL2
        -- desplegar en la pantalla el resultado del parsing:
        putStrLn (show progL2)
        hClose handle -- cerrar el archivo.
--

-------------------------------------------------------
-- data ExpArith = EAbasica ExpBasica
--               | EAopArit (ExpArith,OpArith,ExpArith)
eb1 :: ExpBasica
eb1= (EBvar "x1")
--
eb2 :: ExpBasica
eb2= (EBint 1)
--
ea1 :: ExpArith
ea1 = EAopArit (EAbasica eb1, OAsum, EAbasica eb2)

atomBool1 :: AtomoBool
atomBool1 = AtomoBool ((EBvar "x1"), OCmenEq, (EBint 2))
--
eBool1 :: ExpBool
eBool1 = EBatom atomBool1
--
eBool2 :: ExpBool
eBool2 = EBopUn (OpBnot,eBool1)
--
eBool3 :: ExpBool
eBool3 = EBopBin (eBool1,OpBor,eBool2)
--
eBool4 :: ExpBool
eBool4 = EBopBin (eBool2,OpBand,eBool3)
--
eBool5 :: ExpBool
eBool5 = EBopBin (eBool4,OpBimp,eBool2)

blockS1 :: Stm
blockS1 = SblockStm
    [
    Sasig "x1" (EAbasica (EBint 0)),
    Sasig "x1" (EAbasica (EBint 1)),
    Sasig "x1" (EAbasica (EBint 2)),
    Sasig "x1" (EAbasica (EBint 3))
    ]
--
blockS2 :: Stm
blockS2 = SblockStm
    [
    Sasig "x1" (EAbasica (EBint 3)),
    Sasig "x1" (EAbasica (EBint 2))
    ]
--
blockStms :: Stm
blockStms = SblockStm
    [
    Sasig "x1" ea1,
    Sread "x1",
    Swrite "x1",
    SIfThen eBool1 blockS1 blockS2,
    SWhile eBool5 (Sasig "x1" (EAbasica (EBint 3))),
    SWhile eBool4 blockS2,
    Shalt
    ]
--
progStms :: Prog
progStms = Prog ("Pstms"
                ,[Var ("x1",Tm2n 0 3)]
                ,blockStms)
--
progStmsStr :: String
progStmsStr = "L2PROG Pstms" ++
    "\n VAR" ++
    "\n\t    x1: [0..3];" ++
    "\n PROG" ++
    "\n\t    {x1:=(x1 + 1);" ++
    "\n\t    Read x1;" ++
    "\n\t    Write x1;" ++
    "\n\t    If x1 <= 2" ++
    "\n\t\t        Then" ++
    "\n\t\t\t            {x1:=0;" ++
    "\n\t\t\t             x1:=1;" ++
    "\n\t\t\t             x1:=2;" ++
    "\n\t\t\t             x1:=3;}" ++
    "\n\t\t        Else" ++
    "\n\t\t\t            {x1:=3;" ++
    "\n\t\t\t            x1:=2;};" ++
    "\n\t    While ((¬ x1 <= 2 & (x1 <= 2 | ¬ x1 <= 2)) -> ¬ x1 <= 2) Do" ++
    "\n\t\t        x1:=3;" ++
    "\n\t    While (¬ x1 <= 2 & (x1 <= 2 | ¬ x1 <= 2)) Do" ++
    "\n\t\t        {" ++
    "\n\t\t        x1:=3;" ++
    "\n\t\t        x1:=2;};" ++
    "\n\t    Halt;" ++
    "\n\t    }"
--
parseProgStms :: [(Prog, String)]
parseProgStms = parse tokenProgL2 progStmsStr

checkParsingProgStms :: Bool
checkParsingProgStms =
    progStms == fst (head (parse tokenProgL2 progStmsStr))
------------------------------------------
--

--atomBool1B = AtomoBool ((EBvar "x1"), OCmenEq, (EBint 2))
--
eBool4B :: ExpBool
eBool4B = EBopBin (EBatom $ AtomoBool ((EBint 0), OCmenEq, (EBvar "x1"))
                    ,OpBand
                  ,EBatom $ AtomoBool ((EBvar "x1"), OCmen, (EBint 3)) )
--
eBool5B :: ExpBool
eBool5B = EBopBin (EBatom $ AtomoBool ((EBint 0), OCmen, (EBvar "x1"))
                    ,OpBand
                  ,EBatom $ AtomoBool ((EBvar "x1"), OCmenEq, (EBint 2)) )


--
blockStmsB :: Stm
blockStmsB = SblockStm
    [
    Sasig "x1" ea1,
    Sread "x1",
    Swrite "x1",
    SIfThen eBool1 blockS1 blockS2,
    SWhile eBool4B (Sasig "x1" (EAbasica (EBint 3))),
    SWhile eBool5B blockS2,
    Shalt
    ]
--
progStmsB :: Prog
progStmsB = Prog ("PstmsB"
                ,[Var ("x1",Tm2n 0 3)]
                ,blockStmsB)
--


-- Test:

--
---------------------------------------
