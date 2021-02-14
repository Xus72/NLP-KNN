module Knn
( obtieneDistancia
, obtieneClaseMayoritaria
, obtieneListasClases
, obtieneClasesMayoritarias
, obtienePrecision
) where

import Types
import DocVector as Dv
import Data.List (sortBy)
import Data.Function (on)
import Data.Array

-- Calcula la similitud entre los datos del xtrain y del xtest
obtieneDistancia :: (Ord a, Floating a) => Matriz a -> Matriz a -> [[(Int,a)]]
obtieneDistancia xtrain xtest = [simDoc xtrain (colMat i xtest) | i <- [1..snd(snd(bounds xtest))]]

-- Función que calcula la similitud de un documento del xtest al xtrain
-- y los ordena en función de lo similares que sean esos documentos
simDoc :: (Ord a, Floating a) => Matriz a -> Vector a -> [(Int,a)]
simDoc train doc = ordenaPorSegundoElemento d
        where sim = [similitud (colMat i train) doc | i <- [1..columnas]]
              d = zip [1..columnas] sim
              columnas = snd(snd(bounds train))

-- Obtiene las clases de los k vecinos más cercanos de los datos del xtest al xtrain
obtieneListasClases :: (Ord a, Floating a) => Matriz a -> Matriz a -> [Integer] -> Int -> [[Integer]]
obtieneListasClases xtrain xtest ytrain k = [take k $ obtieneClases xtrain (colMat i xtest) ytrain | i <- [1..snd(snd(bounds xtest))]]

-- Obtiene las clase de los datos del xtrain respecto a la similitud de un documento del xtest
obtieneClases :: (Ord a, Floating a) => Matriz a -> Vector a -> [Integer] -> [Integer]
obtieneClases pesos doc ytrain = [ytrain!!(j-1) | j <- pr]
      where pr = [fst p | p <- sim]
            sim = simDoc pesos doc

-- Obtiene la clase mayoritaria con respecto a los resultados obtenidos de la función
-- obtieneListasClases
obtieneClaseMayoritaria :: [Integer] -> Integer 
obtieneClaseMayoritaria ls = if uncurry (>) tupla then 0 else 1
      where tupla = foldl (\(j,k) x -> if x == 1 then (j,k+1) else (j+1,k)) (0,0) ls


obtieneClasesMayoritarias :: [[Integer]] -> [Integer]
obtieneClasesMayoritarias lss = map obtieneClaseMayoritaria lss

-- Ordena los elementos de mayor a menor en función del segundo elemento de la tupla
ordenaPorSegundoElemento :: Ord b => [(a, b)] -> [(a, b)]
ordenaPorSegundoElemento = sortBy (flip compare `on` snd)

-- Obtiene los resultados de la clasificación
obtienePrecision :: [Integer] -> [Integer] -> Float
obtienePrecision resultados ytest = (fromIntegral a) / (fromIntegral b)
      where b = length ytest
            zipped = zip resultados ytest
            a = foldl (\acc x -> if uncurry (==) x then acc + 1 else acc) 0 zipped

-- Función que calcula la similitud entre dos documentos.
-- Para ello se ha empleado la similitud de coseno
similitud :: (Floating a, Eq a) => Vector a -> Vector a -> a
similitud v1 v2
    | b == 0 = 0.0
    | otherwise = a/b
    where a = sum [i*j | (i,j) <- zip (elems v1) (elems v2)]
          b = sqrt(sum [i^2 | i <- elems v1]) * sqrt(sum [i^2 | i <- elems v2])

colMat :: Num a => Int -> Matriz a -> Vector a
colMat m p = listaVector [p!(i, m) | i <- [1..numFilas p]]

numFilas :: Array (a, b) e -> a
numFilas m = fst(snd(bounds m))