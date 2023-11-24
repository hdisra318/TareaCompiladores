module L1global
    ( minInt
    , maxInt
    , showListSep
    , showListMN
    , showShortList
    )
--
-- Global functions
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------

--


--
-- Global functions: -------------------------------------------------
--

-- OBSERVACION. Int NO es $\mathbf{Z}$.
-- En Haskell, el tipo Int NO es $\mathbf{Z}$ (los números enteros),
-- en realidad: el tipo Int consta de los enteros que van de minInt a maxInt.
-- Es decir, $Int = \{ x \in \mathbf{Z} \mid minInt \leq x \leq maxInt \}$.
--
minInt :: Int
-- minInt = minBound :: Int -- -9223372036854775808
minInt = -128 :: Int -- Tipo Int REDUCIDO XXX
--
maxInt :: Int
-- maxInt = maxBound :: Int --  9223372036854775807
maxInt =  128 :: Int -- Tipo Int REDUCIDO XXX
--

--
showElemsSep :: (Show b,Eq b) => String -> String -> [b] -> String
showElemsSep ind sep lx =
    case lx of
         []     -> ""
         x:lx'  -> ind ++ show x ++ sep
                       ++ showElemsSep ind sep lx'
--
showListSep  :: (Show b,Eq b) => String -> String -> [b] -> String
showListSep ind sep lx =
    if lx==[]
       then ind ++ "[]"
       else showElemsSep ind sep lx
--

--
showShortList  :: (Show b,Eq b) => String->String-> [b] -> String
showShortList ind sep lx =
    if lxSize <= 4
       then showListSep ind sep lx
       else showListSep ind sep lxShortStr
    where
    lxSize      = length lx
    lxShort     = (take 3 lx) ++ [last lx]
    lxShortStr  = [   (show (lxShort!!0))
                    , (show (lxShort!!1))
                    , (show (lxShort!!2))
                    , "..."
                    , (show (lxShort!!3))
                    ]
--

showListMN  :: (Show b,Eq b) => String->String-> Int->Int-> [b] -> String
showListMN ind sep m n lx =
    if m+n >= lxSize
       then showListSep ind sep lx
       else showListSep ind sep lxMNstr
    where
    lxSize      = length lx
    lxFirstm    = take m lx
    lxLastn     = drop (lxSize - n) lx
    lxMNstr     =  [show x | x  <- lxFirstm]
                ++ ["..."]
                ++ [show x | x  <- lxLastn]

--     lxShort     = (take 3 lx) ++ [last lx]
--     lxShortStr  = [   (show (lxShort!!0))
--                     , (show (lxShort!!1))
--                     , (show (lxShort!!2))
--                     , "..."
--                     , (show (lxShort!!3))
--                     ]
--

--

-- Tests: VER L1tests.hs

--
