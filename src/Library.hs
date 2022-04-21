module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


data Hechicero = Hechicero {antiguedad :: Int, clan :: String, grado :: Int} deriving(Show, Eq)

type Equipos = [Hechicero]


equipo1 = [nobara, satoru, yuji]
equipo2 = [nobara, satoru, yuji, maki]
nobara = Hechicero {antiguedad = 1, clan = "Kugisaki", grado = 3}
satoru = Hechicero {antiguedad = 15, clan = "Gojo", grado = 0}
maki = Hechicero {antiguedad = 3, clan = "Zenin", grado = 4}
yuji = Hechicero {antiguedad = 0, clan = "Itadori", grado = 1}

estaPreparado :: Equipos -> Bool
estaPreparado unequipo = length unequipo > 3
tieneExperiencia :: Hechicero -> Bool
tieneExperiencia unhechicero = ((>1).antiguedad) unhechicero
bajarGrado :: Hechicero -> Int
bajarGrado unhechicero = max 0 (grado unhechicero-1)
subirRango :: Hechicero -> Hechicero
subirRango unhechicero = unhechicero {antiguedad = antiguedad unhechicero, clan = clan unhechicero, grado = bajarGrado unhechicero }
esPrestigioso :: Hechicero -> Bool
esPrestigioso unhechicero = clan unhechicero == "Zenin" || clan unhechicero == "Gojo" || clan unhechicero == "Kamo"
