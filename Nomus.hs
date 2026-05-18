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

-- Parte 2

entrenarNomu :: Nomu -> Nomu
entrenarNomu nomu =
    nomu { fuerza = fuerza nomu * 2000 }


puedeIrALaGuerra :: [Nomu] -> Bool
puedeIrALaGuerra ejercito =
    all (\nomu -> fuerza nomu > 2500) (map entrenarNomu ejercito)


esFuerte :: Nomu -> Bool
esFuerte nomu =
    fuerza nomu > 3000 && fuerza nomu <= 10000