{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Lib () where
import Text.Show.Functions()

-- ************************************ -.PUNTO 1.- ***************************************

data Tablero
  = Tablero
  { celdas :: [[Bolita]],
    cabezal :: Coordenadas,
    tamañoTablero :: Tamaño
  } deriving (Show)

type Coordenadas = (X,Y)
type Tamaño = (X,Y)
data Bolita = Azul | Rojo | Negro | Verde deriving (Show, Eq) 
data Direccion = Norte | Sur | Este | Oeste deriving (Show) 
type X = Int
type Y = Int

-- ************************************ -.PUNTO 2.- ***************************************

inicializarTablero :: Int -> Int -> Tablero
inicializarTablero filas columnas =
  Tablero {
  celdas = replicate (filas*columnas) [],
  cabezal = (1,1),
  tamañoTablero = (filas,columnas)
  }

-- ************************************ -.PUNTO 3.A.- ***************************************

moverCabezal :: Direccion -> Tablero -> Tablero
moverCabezal  direccion tablero
  | cabezalCayo tablero direccion = error "El cabezal se cayó del tablero"
  | otherwise = moverCabezal' tablero direccion

cabezalCayo :: Tablero -> Direccion -> Bool
cabezalCayo (Tablero _ (_, y) (_, b)) Norte  = y + 1 > b
cabezalCayo (Tablero _ (_, y) (_, _)) Sur   = y - 1 < 1
cabezalCayo (Tablero _ (x, _) (_, _)) Oeste  = x - 1 < 1 
cabezalCayo (Tablero _ (x, _) (a, _)) Este   = x + 1 > a 

moverCabezal' :: Tablero -> Direccion -> Tablero
moverCabezal' (Tablero lst (x, y) (a, b)) Norte = Tablero lst (x, y + 1) (a, b)
moverCabezal' (Tablero lst (x, y) (a, b)) Sur   = Tablero lst (x, y - 1) (a, b)
moverCabezal' (Tablero lst (x, y) (a, b)) Oeste = Tablero lst (x - 1, y) (a, b)
moverCabezal' (Tablero lst (x, y) (a, b)) Este  = Tablero lst (x + 1, y) (a, b)

-- ************************************ -.PUNTO 3.B.- ***************************************

ponerBolita ::  Bolita -> Tablero -> Tablero
ponerBolita  unaBolita (Tablero celdas cabezal tamaño) =
  Tablero (agregaEnLista unaBolita (posicion cabezal tamaño) celdas) cabezal tamaño

posicion :: Coordenadas -> Tamaño -> Int
posicion (x, y) (m, _) = (y - 1) * m + (x - 1)  
                                                
agregaEnLista :: Bolita -> Int -> [[Bolita]] -> [[Bolita]] 
agregaEnLista unaBolita indiceCelda celdas =
  take indiceCelda celdas ++ [(celdas !! indiceCelda) ++ [unaBolita]] ++ drop (indiceCelda + 1) celdas

-- ************************************ -.PUNTO 3.C.- ***************************************

sacarBolita :: Tablero -> Bolita -> Tablero
sacarBolita (Tablero celdas cabezal tamaño) unaBolita
  | hayBolitaDeColor unaBolita (Tablero celdas cabezal tamaño) = Tablero (quitarDeLista unaBolita  (posicion cabezal tamaño) celdas) cabezal tamaño
  | otherwise = error ("No hay bolitas del color " ++  show unaBolita ++  " para sacar de la celda actual")

quitarDeLista :: Bolita -> Int -> [[Bolita]] -> [[Bolita]] 
quitarDeLista unaBolita indiceCelda celdas =
    let (antes, despues) = splitAt indiceCelda celdas 
    in antes ++ [delete unaBolita (head despues)] ++ tail despues


delete :: Eq a => a -> [a] -> [a] 
delete _ [] = []
delete x (y:ys)
  | x == y = ys
  | otherwise = y : delete x ys

-- ************************************ -.PUNTO 4.A.- ***************************************

type Sentencia = Tablero -> Tablero

repetir :: Int -> [Sentencia] -> Tablero -> Tablero
repetir 0 _ unTablero = unTablero
repetir n sentencias unTablero = ejecutarSentencias (repetir (n-1) sentencias unTablero) sentencias

ejecutarSentencias :: Tablero -> [Sentencia] -> Tablero
ejecutarSentencias = foldl (flip ($)) --Hermosa función

-- ************************************ -.PUNTO 4.B.- ***************************************

type Condicion = Tablero -> Bool

alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Tablero -> Tablero
alternativa condicion sentenciasTrue sentenciasFalse unTablero
  | condicion unTablero = ejecutarSentencias unTablero sentenciasTrue
  | otherwise = ejecutarSentencias unTablero sentenciasFalse

si :: Condicion -> [Sentencia] -> Tablero -> Tablero
si unaCondicion sentencias unTablero
  | unaCondicion unTablero = ejecutarSentencias unTablero sentencias
  | otherwise = unTablero

siNo :: Condicion -> [Sentencia] -> Tablero -> Tablero
siNo unaCondicion sentencias unTablero
  | not (unaCondicion unTablero) = ejecutarSentencias unTablero sentencias
  | otherwise = unTablero

-- ************************************ -.PUNTO 4.C.- ***************************************

mientras :: Condicion -> [Sentencia] -> Tablero -> Tablero
mientras unaCondicion sentencias unTablero
  | unaCondicion unTablero = mientras unaCondicion sentencias (ejecutarSentencias unTablero sentencias)
  | otherwise = unTablero

-- ************************************ -.PUNTO 4.D.- ***************************************

irAlBorde :: Direccion -> Tablero -> Tablero
irAlBorde unaDireccion unTablero 
  | puedeMoverse unaDireccion unTablero  = irAlBorde unaDireccion (moverCabezal unaDireccion unTablero)
  | otherwise = unTablero

-- ************************************ -.PUNTO 5.A.- ***************************************

puedeMoverse :: Direccion -> Tablero -> Bool
puedeMoverse unaDireccion unTablero = not (cabezalCayo unTablero unaDireccion)

-- ************************************ -.PUNTO 5.B.- ***************************************

hayBolitaDeColor :: Bolita -> Tablero -> Bool
hayBolitaDeColor unaBolita (Tablero celdas cabezal tamaño) = unaBolita `elem` (celdas !! (posicion cabezal tamaño))

-- ************************************ -.PUNTO 5.C.- ***************************************

cantidadDeBolitas :: Bolita -> Tablero -> Int
cantidadDeBolitas unaBolita (Tablero celdas cabezal tamaño) = length . filter (== unaBolita) $ (celdas !! (posicion cabezal tamaño))

-- ************************************ -.PUNTO 6.- ***************************************
-- Igual a funcion ejecutarSentencias (PUNTO 4.A) pero definida mediante lambda

programa :: Tablero -> [Sentencia] -> Tablero
programa = foldl (\unTablero sentencias -> sentencias unTablero)

-- ************************************ -.PUNTO 6.- ***************************************

instrucciones :: [Sentencia]
instrucciones = [
    moverCabezal Norte,
    ponerBolita Negro,
    ponerBolita Negro,
    ponerBolita Azul,
    moverCabezal Norte,
    (repetir 15 [ponerBolita Rojo, ponerBolita Azul]), 
    (si (hayBolitaDeColor Verde) [moverCabezal Este, ponerBolita Negro]),
    (siNo (hayBolitaDeColor Verde) [moverCabezal Sur, moverCabezal Este, ponerBolita Azul]),
    moverCabezal Este,
    mientras (\tablero -> cantidadDeBolitas Verde tablero <= 9) [ponerBolita Verde],
    ponerBolita Azul
  ]

