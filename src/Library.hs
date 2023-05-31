module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


type Cansancio = Number
type Stress = Number
type Idioma = String

data Turista = UnTurista {
    cansancio :: Cansancio,
    stress :: Stress,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving Show

type Excursion = Turista -> Turista

irALaPlaya:: Excursion
irALaPlaya turista | (viajaSolo turista)==True = turista { cansancio = ((cansancio turista)-5) }
                   | otherwise = turista { stress = ((stress turista)-1) }

apreciarElemento:: String -> Excursion
apreciarElemento elemento turista  = turista {stress = ((stress turista)- (length elemento))}

caminar:: Number-> Excursion
caminar minutos turista = turista {stress = (stress turista) - (minutos/4), cansancio = (cansancio turista)+(minutos/4)}

salirAHablar :: Idioma -> Excursion
salirAHablar idiomaNuevo turista  
    | yaHablaIdioma idiomaNuevo turista = turista {viajaSolo = True}
    | otherwise = turista {idiomas = idiomaNuevo : idiomas turista, viajaSolo = True}

yaHablaIdioma :: Idioma -> Turista -> Bool
yaHablaIdioma idiomaAAprender turista = elem idiomaAAprender (idiomas turista)



data Marea =
    Tranquila
    |Moderada
    |Fuerte


paseoEnBarco :: Marea -> Excursion
paseoEnBarco Tranquila turista =  (caminar 10.apreciarElemento "mar".salirAHablar "aleman") turista
paseoEnBarco Moderada  turista = turista
paseoEnBarco Fuerte turista = turista {stress = (stress turista) + 6, cansancio = (cansancio turista) + 10}


idearExcursion :: Excursion -> Turista -> Turista
idearExcursion excursion turista = excursion (turista {stress = stress turista * 0.90})

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun criterio turista excursion = deltaSegun criterio (idearExcursion excursion turista) turista

esEducativa :: Turista -> Excursion -> Bool
esEducativa turista = (>0) . deltaExcursionSegun (length . idiomas) turista

-- esDesestresante  :: Turista -> Excursion -> Bool
-- esDesestresante  turista = (<= -3) . deltaExcursionSegun stress turista

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista 


excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarElemento "cascada", caminar 40, irALaPlaya, salirAHablar "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina mareaIsla = [paseoEnBarco mareaIsla, excursionIslaVecina mareaIsla, paseoEnBarco mareaIsla]

excursionIslaVecina :: Marea -> Excursion
excursionIslaVecina Fuerte = apreciarElemento "lago"
excursionIslaVecina _ = irALaPlaya



ana :: Turista
ana = UnTurista{
    cansancio = 0,
    stress = 21,
    viajaSolo = False,
    idiomas = ["espaniol"]
}

beto :: Turista
beto = UnTurista{
    cansancio = 15,
    stress = 15,
    viajaSolo = True,
    idiomas = ["aleman"]
}

cathi :: Turista
cathi = UnTurista{
    cansancio = 15,
    stress = 15,
    viajaSolo = True,
    idiomas = ["aleman", "catalan"]
}

