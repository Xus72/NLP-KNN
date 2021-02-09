module Knn
( obtieneVecinos
, obtieneClases
, obtieneClaseMayoritaria
) where

import Types
import DocVector as Dv
import Data.List (sortBy)
import Data.Function (on)

obtieneVecinos corp doc = ordenaPorSegundoElemento tuplas
        where matriz = Dv.pesosEnDocumento corp
              ls = matrizLista matriz
              lss = map (\i -> prodEscalar (listaVector i) (listaVector doc)) ls
              tuplas = zip corp lss

obtieneClases tuplas zip n = take n [i | (x,y) <- tuplas, (j,i) <- zip, x == j]

obtieneClaseMayoritaria :: [Int] -> Int
obtieneClaseMayoritaria ls = if fst tupla > snd tupla then 0 else 1 
      where tupla = foldl (\(j,k) x -> if x == 1 then (j,k+1) else (j+1,k)) (0,0) ls

ordenaPorSegundoElemento :: Ord b => [(a, b)] -> [(a, b)]
ordenaPorSegundoElemento = sortBy (compare `on` snd)