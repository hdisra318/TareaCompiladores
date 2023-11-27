# TareaCompiladores
Tarea 1 de Compiladores

## Integrantes del Equipo:
- Natalia Abigail Pérez Romero
- Marco Antonio Rivera Silva
- Diego Martinez Calzada
- Israel Hernandez Dorantes
- Kevin Jair Torres Valencia

## Instrucciones

- Iniciar `ghci`.
- Cargar el archivo `L2tests.hs` con el comando `:l L2tests.hs`.
- Ejecutar la función `testfor` sin argumentos para ver un ejemplo del for.
- Ejecutar la función `testArchivo` sin argumentos para leer el archivo `progSuma.L2`.

## Ejercicios:
1. Definir un lenguaje, L2, que resulta de agregar a L1 una instrucción "For".
En L2, la sintaxis cambia a:
 <Stm>        ::=   Halt | <AsigStm>
                  | Read <VarId> | Write <VarId>
                  | <IfThenStm> | <WhileStm>
                  | <BlockStm>
                  | <ForStm>
<ForStm>   ::= For <VarId> = <ExpArith> To <ExpArith> Do <Stm>
<ProgL2>     ::= L2PROG <ProgName> VAR <VarList> PROG <Stm>

Modificamos en L1sintaxis.hs la definición de la sintaxis del lenguaje:

```haskell
data Stm
    = Sasig VarId ExpArith
    | Shalt
    | Sread VarId
    | Swrite VarId
    | SIfThen ExpBool Stm Stm
    | SWhile ExpBool Stm
    | SblockStm [Stm]
    | SFor VarId  ExpArith  ExpArith  Stm
   deriving (Eq)
```
Y tambien modificamos la función showStm de L1sintaxis.hs para mostrar la sentencia For de manera agradable

```haskell
showStm :: Stm -> String
showStm stm =
    case stm of
        ...
        (SFor vId e1 e2 stmF) -> "For " ++ vId ++ "=" ++ (show e1) ++ " To " ++ (show e2) ++ " Do " ++ (show stmF)
```


2.Hacer los cambios necesarios para que una función readL2prog (definida en un módulo L1textIO.hs) compile y funcione haciendo parsing a programas L2.
Esta función es similar a readL1prog definida en L1textIO.hs.

Modificamos el archivo L1.textIO para incluir la función readL2prog 
```haskell
--Cambio L2
readL2prog :: FilePath -> IO ()
readL2prog fileName
    = do
        -- abrir (declarar) el archivo:
        handle      <- openFile fileName ReadMode
        -- leer el contenido del archivo:
        textProgL2  <- hGetContents handle
        -- aplicar parsing
        let progL2 = (parse tokenProgL2 textProgL2)
        -- regresa proL1:
        print progL2
        hClose handle -- cerrar el archivo. 
```

No olvidar incluir en el modulo L1textIO
```haskell
module L1textIO
    (
      readL1prog
    , readL2prog
    ,...
    )
```

Luego en L1parserProg.hs es necesario añadir la siguiente función:

```haskell
-- Definicion del token L2PROG
tokenL2PROG :: Parser String
tokenL2PROG = symbol "L2PROG"
-- <ProgL2> ::= L2PROG <ProgName> VAR <VarList> PROG <Stm>
parserProgL2 :: Parser Prog
parserProgL2 =
    do
    _           <- tokenL2PROG
    progName    <- tokenProgName
    _           <- symbol "VAR"
    varList     <- tokenVarList
    _           <- symbol "PROG"
    stm         <- tokenStm
    _           <- parserNullInput  -- tiene exito sii el input es "". Resto del input=vacío.
    return (Prog (progName, varList, stm))
--
tokenProgL2 :: Parser Prog
tokenProgL2 = token parserProgL2
```


3. Escribir, en un archivo de texto progSuma.L2, un programa L2 que calcule la suma de los números entre 1 y el valor de la variable x, mediante una instrucción For.

Para realizar este archvo usamos el siguiente codigo

```haskell
L2PROG PSuma
VAR
    x: [0..3];
    y: [0..3];
    z: [0..3];
PROG
    {x1:=3;
    z1:=0;
    For y1 = 0 To x1 Do
        z1:=(z1 + y1);
    Halt;
    }
```

Lo que básicamente hace es crear 3 variables de L2: x, y y z y utilizar a y1 como nuestra variable que va a cambiar en el for, a x1 como nuestro limite para el for, y a z1 para alamacenar la suma de los numeros.

4. Aplicar el parser de programas L2 a progSuma.L2

En L2tests.hs podemos definir la llamada a un parse sobre sobre un programa L2 que usa For:
```haskell
-- Ejemplo de ejecucion For x := 0 To 1 Do z := z + 1
testFor = semFor x xInicio xFin asignacion sigmax0z0

-------------------------
----- Test para .L2 -----
-------------------------

archivo = "progSuma.L2"

testArchivo = readAndShowL2prog archivo
```

5. Definir la semántica de la instrucción For, e implementarla en Haskell, en un módulo L2semanticaStm.hs similar al módulo L1semanticaStm.
Sugerencia: ¿Cómo se escribe un For usando un While?

En L2semanticaStm.hs agregamos lo siguiente para la implementación de la sentencia For:
```haskell
----------------------------------------
-- Implementacion de For usando While --
----------------------------------------

-- Usamos semanticas ya implementadas para transformar los parametros dados
-- para un for en la semantica de un block y la de un while, asignando el
-- valor ea1 para el vId y empezar la iteracion hasta ea2, sumando en uno a vId
-- y ejecutando stm
semFor :: VarId -> ExpArith -> ExpArith -> Stm -> EstadoVT -> [EstadoVT]
semFor vId ea1 ea2 stmF sigma -- Semantica de "For vid=ea1 To ea2 Do stmF"
    = semStm forTOwhile sigma
    where
    forTOwhile =
        SblockStm                       -- {
            [Sasig vId ea1,             -- v := ea1;
            SWhile vLEQea2              -- While v <= ea2 Do
                (SblockStm              --      {
                    [stmF,              --      stm;
                    Sasig vId vSumUno   --      v := v+1;
                ])                      --      }
                ]                       -- }
    vLEQea2 = EBatom (AtomoBool (EBvar vId, OCmenEq, EBint ea2Sem)) -- v <= ea2
    ea2Sem  = semExpArith ea2 sigma -- semantica de ea2 EN sigma XXX
    vIdEA   = EAbasica (EBvar vId)  -- vId como una ExpArith VER ExpArith
    unoEA   = EAbasica (EBint 1)    -- 1 como una ExpArith VER ExpArith
    vSumUno = EAopArit (vIdEA,OAsum,unoEA) -- v+1
```
Que verifica si la sematica de ebSem (expresion booleana) tenga sentido en el estado sigma de manera similar a la semantica de un While.
