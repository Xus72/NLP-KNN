module Knn
( obtieneVecinos
, obtieneClases
) where

import Types
import DocVector as Dv
import Data.List (sortBy)
import Data.Function (on)

obtieneVecinos corp doc = mySort tuplas
        where matriz = Dv.pesosEnDocumento corp
              ls = matrizLista matriz
              lss = map (\i -> prodEscalar (listaVector i) (listaVector doc)) ls
              tuplas = zip corp lss

obtieneClases tuplas zip n = take n [i | (x,y) <- tuplas, (j,i) <- zip, x == j]

mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (compare `on` snd)