module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


data Hechicero = Hechicero {antiguedad :: Number, clan :: String, grado :: Number} deriving(Show, Eq)

type Equipos = [Hechicero]


equipo1 = [nobara, satoru, yuji]
equipo2 = [nobara, satoru, yuji, maki]
nobara = Hechicero {antiguedad = 1, clan = "Kugisaki", grado = 3}
satoru = Hechicero {antiguedad = 15, clan = "Gojo", grado = 0}
maki = Hechicero {antiguedad = 3, clan = "Zenin", grado = 4}
yuji = Hechicero {antiguedad = 0, clan = "Itadori", grado = 1}

estaPreparado :: Equipos -> Bool
estaPreparado unequipo = tamanioEquipo unequipo > 3

tamanioEquipo :: Equipos -> Number
tamanioEquipo = length

tieneExperiencia :: Hechicero -> Bool
tieneExperiencia unhechicero = ((>1).antiguedad) unhechicero

--bajarGrado :: Hechicero -> Number
--bajarGrado unhechicero = max 0 (grado unhechicero-1)
subirRango :: Hechicero -> Hechicero
subirRango unHechicero 
        | gradoEspecial unHechicero = unHechicero
        | otherwise = unHechicero { grado = (grado unHechicero) - 1}
--{antiguedad = antiguedad unhechicero, clan = clan unhechicero, grado = bajarGrado unhechicero }

gradoEspecial :: Hechicero -> Bool
gradoEspecial unHechicero = ((== 0).grado) unHechicero

esPrestigioso :: Hechicero -> Bool
esPrestigioso unhechicero = elem (clan unhechicero) ["Zenin","Gojo","Kamo"]

esInvencible :: Equipos -> Bool
esInvencible equipo = any gradoEspecial equipo

esFavorito :: Equipos -> Bool
esFavorito equipo = all esPrestigioso equipo

sonExpertos :: Equipos -> Equipos
sonExpertos equipo = filter tieneExperiencia equipo

haceFrenteACualquierMaldicion :: Equipos -> Bool
haceFrenteACualquierMaldicion equipo = esInvencible equipo || estaPreparado equipo

powerUp :: Equipos -> Equipos
powerUp = map subirRango



cantidadHechicerosEspeciales :: Equipos -> Number
cantidadHechicerosEspeciales = length.filter gradoEspecial


promedioGrados :: Equipos -> Number
promedioGrados equipo = div (sum.map grado $ equipo) (tamanioEquipo equipo) 
