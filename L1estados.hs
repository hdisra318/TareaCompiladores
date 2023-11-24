module L1estados
    (
      EstadoVT(..)
    , modifEstadoVT
    , estadosVTOf
    , boundsOFvarId
    , listOfEdoVT
    , varOFvId
    , edoVTtoOmega
    , valListOf
    , modifLista
    )
--
-- Estados de L1
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

--
-----------------------------------------------------------------
--

-- Estados de L1 -------------------------------------------------
--

-- Estados de variables con tipo SimpleType.
data TypeVal = TypeVal (SimpleType,Int)
    deriving (Eq)
--
showTypeVal :: TypeVal -> String
showTypeVal (TypeVal (_,val)) = show val
--
instance Show TypeVal where
    show = showTypeVal
--
type EstadoVTlist = [(VarId,(SimpleType,Int))]
-- type EstadoVTlist = [(VarId,TypeVal)]
data EstadoVT = EstadoVT EstadoVTlist
              | OmegaVT  EstadoVTlist
              -- deriving (Eq,Show)
              deriving (Eq)
--
--
-- showEstadoVT :: EstadoVT -> String
-- showEstadoVT sigma =
--     case sigma of
--         EstadoVT lvtb   -> show lvtb
--         OmegaVT lvtb    -> "omega"++(show lvtb)
--
showEstadoVT :: EstadoVT -> String
showEstadoVT sigma =
    case sigma of
        EstadoVT lvtb   -> "[" ++ showLvtb lvtb ++ "]"
        OmegaVT lvtb    -> "omega"++("[" ++ showLvtb lvtb ++ "]")
    where
    showLvtb l =
        case l of
             []     -> ""
             (v,(_,b)):l'   -> show (v,b) ++ showLvtb l'
--
instance Show (EstadoVT) where
    show = showEstadoVT
--

--
varOFvId :: EstadoVT -> VarId -> Var
-- Variable de un vId en un estado.
varOFvId sigma vId = case maybeTval of
                          Just (t,_)    -> Var (vId,t)
                          Nothing       -> error $ "varOFvId: variable inválida, v= " ++ vId
                    where
                    lvtb        = listOfEdoVT sigma
                    maybeTval   = lookup vId lvtb
--

--
boundsOFvarId :: EstadoVT -> VarId -> (Int,Int)
-- Límites de un VarId en un estado abstracto.
boundsOFvarId sigma vId
    = case sigma of
        (EstadoVT lvtb) -> case maybeTypeVal of
                            Just (vType,_)  -> case vType of
                                        Tint       -> (minInt,maxInt) -- Valores entre minInt y maxInt
                                        Tm2n m n   -> (m,n)
                            Nothing     -> error $ "boundsOFvarId: variable invalida, v = " ++ vId
                            where
                            maybeTypeVal = lookup vId lvtb
        (OmegaVT lvtb)  ->  case maybeTypeVal of
                            Just (vType,_)  -> case vType of
                                        Tint       -> (minInt,maxInt) -- Valores entre minInt y maxInt
                                        Tm2n m n   -> (m,n)
                            Nothing     -> error $ "boundsOFvarId: variable invalida, v = " ++ vId
                            where
                            maybeTypeVal = lookup vId lvtb
--

--
valListOf :: Var -> [Int]
-- Lista de valores de una variable
valListOf (Var (_, sType)) =
    case sType of
         Tint       -> [minInt..maxInt] -- Valores entre minInt y maxInt
         Tm2n m n   -> [m..n]
--

--
estadosVTOf :: VarList -> [EstadoVT]
-- Lista de estados de variables con tipo para la lista de variables lv
-- Slv := {((v1,t1,b1), (v2,t2,b2), . . . , (v_|lv|,t_|lv|,b_|lv|) )
--          | v_i=lv(i), ti=typeOf(vi) and bi ∈ Dom(v_i)}
estadosVTOf lv =
    case lv of
        v@(Var (vId,vT)) : lv'   -> [EstadoVT ((vId,(vT,b)) : lvtb) | b <- valListOf v, EstadoVT lvtb <- s_lv']
                                    where
                                    s_lv' = estadosVTOf lv'
        []                      -> [ EstadoVT[] ]
--

--
modifLista :: [(VarId, b)] -> VarId -> b -> [(VarId, b)]
modifLista l v b = case l of
                       (x,d) : l'   ->  if x == v
                                               then (v,b) : l'
                                               else (x,d) : (modifLista l' v b)
                       []               -> []
--

--
modifEstadoList :: VarId -> Maybe (SimpleType,Int) -> EstadoVTlist -> Int -> EstadoVTlist
modifEstadoList vId maybeTypeVal lvtb b =
    case maybeTypeVal of
        Just (Tm2n m n,_)
            -> if (m<=b && b<=n)
                then (modifLista lvtb vId (Tm2n m n,b))
                else error $ "modifEstadoList: b not in [m..n], (m,n, b)= " ++ show(m,n,b)
        Just (Tint,_)
            -> if (minInt<=b && b<=maxInt)
                then (modifLista lvtb vId (Tint,b))
                else error $ "modifEstadoList: b not in [minInt..maxInt], (minInt,maxInt, b)= "
                     ++ show(maxInt,maxInt,b)
        Nothing
            -> error $ "modifEstadoList: variable vId not in sigma, vId)= " ++ vId
--
modifEstadoVT :: EstadoVT -> VarId -> Int -> EstadoVT
modifEstadoVT sigma vId b =
    case sigma of
        EstadoVT lvtb   -> EstadoVT (modifEstadoList vId maybeTypeVal lvtb b)
                        where
                        maybeTypeVal = lookup vId lvtb
        OmegaVT lvtb    -> OmegaVT lvtb

--
--
listOfEdoVT :: EstadoVT -> [(VarId, (SimpleType, Int))]
-- Lista de pares (v,t,b) del estado sigma.
listOfEdoVT sigma = case sigma of
           (EstadoVT lvtb) -> lvtb
           (OmegaVT lvtb)  -> lvtb
--
--
edoVTtoOmega :: EstadoVT -> EstadoVT
-- Transforma un estado a un estado final, Omega.
edoVTtoOmega sigma = case sigma of
           (EstadoVT lvb) -> (OmegaVT lvb)
           (OmegaVT lvb)  -> (OmegaVT lvb)
--

--

---------------------------------------------------------------
--
-- Tests: VER L1tests.hs
--

