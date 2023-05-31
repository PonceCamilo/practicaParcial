module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Nombre = String
type Sabor = String
type Sabores = [Sabor]
type Peso = Number
type Temperatura = Number

data Postre = UnPostre {
    nombre :: Nombre,
    sabores :: Sabores,
    peso :: Peso,
    temperatura :: Temperatura
} deriving (Eq, Show)

type Calor = Number
type Congela = Bool
type Elimina = Bool

data Hechizo = UnHechizo {
    llamado :: Nombre,
    modificaPeso :: Peso,
    modificaCalor :: Calor,
    congela :: Congela,
    eliminaSabores :: Elimina,
    modificarSabores :: Sabores
} deriving (Eq, Show)

perderPeso :: Postre -> Number -> Number
perderPeso postre numero = (peso postre) - (peso postre) * numero

modificarTemperatura :: Postre -> Number -> Bool -> Number
modificarTemperatura postre numero congela
    |congela = 0
    |otherwise = (temperatura postre) + numero

modificarSaboresPostre :: Postre -> Hechizo -> Sabores
modificarSaboresPostre postre hechizo
    |eliminaSabores hechizo = []
    |otherwise = (sabores postre) ++ (modificarSabores hechizo)

incendio :: Hechizo
incendio = UnHechizo "incendio" 0.05 1 False False []

immobulus :: Hechizo
immobulus = UnHechizo "immobulus" 0 0 True False []

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = UnHechizo "wingardiumLeviosa" 0.1 0 False False ["concentrado"] 

diffindo :: Number -> Hechizo 
diffindo numero = UnHechizo "diffindo" (numero / 100) 0 False False [] 

riddikulus :: Sabor -> Hechizo
riddikulus sabor = UnHechizo "riddikulus" 0 0 False False [reverse sabor] 

avadakedavra :: Hechizo
avadakedavra = UnHechizo "avadakedavra" 0 0 True True [] 

hechizoAplicado :: Hechizo -> Postre -> Postre
hechizoAplicado hechizo postre = postre {sabores = modificarSaboresPostre postre hechizo, peso = perderPeso postre (modificaPeso hechizo), temperatura = modificarTemperatura postre (modificaCalor hechizo) (congela hechizo)}

bizcocho :: Postre
bizcocho = UnPostre "Bizcocho" ["fruta", "crema"] 100 25

tarta :: Postre
tarta = UnPostre "tarta" ["melaza"] 50 0

mesa = [bizcocho, tarta]

type Postres = [Postre]

estaListo :: Postre -> Bool
estaListo postre = (sabores postre) /= [] && (peso postre) > 0 && (temperatura postre) > 0

estanListos :: Postres -> Hechizo -> Bool
estanListos postres hechizo = all estaListo (map (hechizoAplicado hechizo) postres)

postresListos :: Postres -> Hechizo -> Postres
postresListos postres hechizo = filter estaListo (map (hechizoAplicado hechizo) postres)

pesosPostresListos :: Postres -> Hechizo -> [Number]
pesosPostresListos postres hechizo = map (peso) (postresListos postres hechizo)

pesoPromedio :: Postres -> Hechizo -> Number
pesoPromedio postres hechizo = (sum (pesosPostresListos postres hechizo)) / (length (postresListos postres hechizo))

type Cantidad = Number

data Mago = UnMago {
    hechizos :: [Hechizo],
    horrorcrux :: Cantidad
} deriving (Eq, Show)

harry :: Mago
harry = UnMago [incendio, riddikulus "mila"] 0

tieneHechizo :: Mago -> Hechizo -> Bool
tieneHechizo mago hechizo = elem hechizo (hechizos mago)

sumaHorrocrux :: Hechizo -> Postre-> Number
sumaHorrocrux hechizo postre
    |(hechizoAplicado hechizo postre) == (hechizoAplicado avadakedavra postre) = 1
    |otherwise = 0

defensaCocinasOscuras :: Mago -> Hechizo -> Postre -> Mago
defensaCocinasOscuras mago hechizo postre
    |not (tieneHechizo mago hechizo) = mago {hechizos = (hechizos mago) ++ [hechizo], horrorcrux = (horrorcrux mago) + (sumaHorrocrux hechizo postre)}
    |otherwise = mago {horrorcrux = (horrorcrux mago) + (sumaHorrocrux hechizo postre)}

