module DocVector
( frequencyWordDoc
, numDocPalabra
, freqDocInversa
, pesoEnDocumento
, pesosEnDocumento
, matrizLista
, listaVector
, obtieneVecinos
, prodEscalar
) where

import Types
import Data.Array
import Data.List (sortBy)
import Data.Function (on)

frequencyWordDoc :: Documento -> String -> Int
frequencyWordDoc doc pal = foldl (\acc x -> if pal == x then acc + 1 else acc) 0 doc

numDocPalabra :: Corpus a -> String -> Int
numDocPalabra corp pal = foldl f 0 corp
    where f acc x
            | frequencyWordDoc x pal /= 0 = acc + 1
            | otherwise = acc

freqDocInversa :: Corpus a -> String -> Float
freqDocInversa corp pal = logBase 10 $ n / freqDocumental
    where n = fromIntegral(length corp)
          freqDocumental = fromIntegral(numDocPalabra corp pal)

pesoEnDocumento :: Corpus a -> String -> [Float]
pesoEnDocumento corp pal = map (* freqi) ls --listArray (1, n) ls'
    where ls = [fromIntegral(frequencyWordDoc x pal) | x <- corp]
          --n = length ls'
          freqi = freqDocInversa corp pal

pesosEnDocumento :: Corpus a -> Matriz Float
pesosEnDocumento corp = listaMatriz ls
    where ls = [pesoEnDocumento corp x | x <- (removeDuplicates2 (concat corp))]

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

--(\acc x -> if (frequencyWordDoc x pal) /= 0 then acc + 1 else acc)

representacionVectorial :: Corpus a -> Matriz a
representacionVectorial corp = undefined

removeDuplicates2 = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

--prodEscalar :: Foldable a => Vector a -> Vector a -> a
prodEscalar :: Floating a => Vector a -> Vector a -> a
prodEscalar v1 v2 = a/b 
    where a = sum [i*j | (i,j) <- zip (elems v1) (elems v2)]
          b = sqrt(sum [i^2 | i <- elems v1]) * sqrt(sum [i^2 | i <- elems v2])

obtieneVecinos matriz doc n
    | n > length (matrizLista matriz) = obtieneVecinos matriz doc (length (matrizLista matriz))
    | otherwise = take n ordenada
        where ls = zip (matrizLista matriz) [1..]
              lss = map (\(i,j) -> (i, prodEscalar (listaVector i) (listaVector doc))) ls
              ordenada = mySort lss


mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` snd)

listaVector :: Num a => [a] -> Vector a
listaVector xs = array (1, n) (zip [1..n] xs)
    where
        n = length xs


-- [2.091515,1.0457575,0.52287877,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

-- [0.52287877,0.0,0.52287877,0.0,0.0,0.0,0.0,0.0,0.0,0.52287877]