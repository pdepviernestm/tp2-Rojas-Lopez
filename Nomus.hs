module Library where
import pdepreludat


-- Parte 1

data Nomu = UnNomu {
    alas :: String,
    brazos :: Number,
    ojos :: Number,
    colorDePiel :: String,
    vida :: Number,
    fuerza :: Number
} deriving (Show, Eq)


puedeVer :: Nomu -> Bool
puedeVer nomu = ojos nomu > 0


categoriaNomu :: Nomu -> String
categoriaNomu nomu
    | fuerza nomu <= 1000 = "pichi"
    | fuerza nomu < 3000 = "comun"
    | fuerza nomu <= 10000 = "fuerte"
    | otherwise = "High-end"

