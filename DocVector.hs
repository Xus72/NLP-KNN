module DocVector
( frequencyWordDoc
, numDocPalabra
, freqDocInversa
, pesoEnDocumento
, pesosEnDocumento
, matrizLista
, listaVector
, listaMatriz
) where

import Types
import Data.Array

--Función que calcula el frecuencia en la que aparece cada término en un documento
frequencyWordDoc :: Documento -> String -> Int
frequencyWordDoc doc pal = foldl (\acc x -> if pal == x then acc + 1 else acc) 0 doc

-- Función que calcula el número de palabras que tiene un documento
numDocPalabra :: Corpus a -> String -> Int
numDocPalabra corp pal = foldl f 0 corp
    where f acc x
            | frequencyWordDoc x pal /= 0 = acc + 1
            | otherwise = acc

-- Función que calcula la frecuencia documental inversa
freqDocInversa :: Corpus a -> String -> Float
freqDocInversa corp pal = logBase 10 $ n / freqDocumental
    where n = fromIntegral(length corp)
          freqDocumental = fromIntegral(numDocPalabra corp pal)

-- Función que calcula el peso que tiene una palabra en un documento.
-- El peso se calcula multiplicandole a cada palabra la frecuencia inversa documental
-- y el número de veces que aparece en el documento
pesoEnDocumento :: Corpus a -> String -> [Float]
pesoEnDocumento corp pal = map (* freqi) ls --listArray (1, n) ls'
    where ls = [fromIntegral(frequencyWordDoc x pal) | x <- corp]
          --n = length ls'
          freqi = freqDocInversa corp pal

-- Convierte en un matriz el resultado de la función pesoEnDocumento
pesosEnDocumento :: Corpus a -> Matriz Float
pesosEnDocumento corp = listaMatriz ls
    where ls = [pesoEnDocumento corp x | x <- eliminarRepetidos (concat corp)]

listaMatriz :: Num a => [[a]] -> Matriz a
listaMatriz xss = listArray ((1,1),(m,n)) (concat xss)
    where m = length xss
          n = length (head xss)

separa :: Int -> [a] -> [[a]]
separa _ [] = []
separa n xs = take n xs : separa n (drop n xs)

numColumnas:: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

matrizLista :: Num a => Matriz a -> [[a]]
matrizLista p = separa (numColumnas p) (elems p)

-- Elimina las palabras repetidas que aparecen en los documentos
eliminarRepetidos :: Documento -> Documento 
eliminarRepetidos = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

listaVector :: Num a => [a] -> Vector a
listaVector xs = array (1, n) (zip [1..n] xs)
    where
        n = length xs
