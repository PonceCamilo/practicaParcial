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


type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio postre = postre {temperatura = temperatura postre + 1, peso = peso postre - peso postre * 0.05}

immobulus :: Hechizo
immobulus postre = postre {temperatura = 0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre {sabores = sabores postre ++ ["concentrado"], peso = peso postre - peso postre * 0.1}

diffindo :: Number -> Hechizo 
diffindo numero postre = postre {peso = peso postre - peso postre * (numero / 100)}

riddikulus :: Sabor -> Hechizo
riddikulus sabor postre = postre {sabores = sabores postre ++ [reverse sabor]}

avadakedavra :: Hechizo
avadakedavra postre = immobulus (postre {sabores = []})

bizcocho :: Postre
bizcocho = UnPostre "Bizcocho" ["fruta", "crema"] 100 25

tarta :: Postre
tarta = UnPostre "tarta" ["melaza"] 50 0

mesa = [bizcocho, tarta]

type Postres = [Postre]

estaListo :: Postre -> Bool
estaListo postre = (sabores postre) /= [] && (peso postre) > 0 && (temperatura postre) > 0

estanListos :: Hechizo -> Postres -> Bool
estanListos hechizo postres = all estaListo (map hechizo postres)

postresListos :: Hechizo -> Postres -> Postres
postresListos hechizo postres  = filter estaListo (map hechizo postres)

pesosPostresListos :: Hechizo -> Postres -> [Number]
pesosPostresListos hechizo postres = map (peso) (postresListos hechizo postres)

pesoPromedio :: Hechizo -> Postres -> Number
pesoPromedio hechizo postres = (sum (pesosPostresListos hechizo postres)) / (length (postresListos hechizo postres))

type Cantidad = Number

data Mago = UnMago {
    hechizos :: [Hechizo],
    horrorcrux :: Cantidad
} deriving (Eq, Show)

harry :: Mago
harry = UnMago [incendio, riddikulus "mila"] 0

tieneHechizo :: Mago -> Hechizo -> Bool
tieneHechizo mago hechizo  

sumaHorrocrux :: Hechizo -> Postre-> Number
sumaHorrocrux hechizo postre
    |hechizo postre == avadakedavra postre = 1
    |otherwise = 0

defensaCocinasOscuras :: Mago -> Hechizo -> Postre -> Mago
defensaCocinasOscuras mago hechizo postre
    |not (tieneHechizo mago hechizo) = mago {hechizos = (hechizos mago) ++ [hechizo], horrorcrux = (horrorcrux mago) + (sumaHorrocrux hechizo postre)}
    |otherwise = mago {horrorcrux = (horrorcrux mago) + (sumaHorrocrux hechizo postre)}
