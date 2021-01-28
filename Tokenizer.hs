------------------------------------------------------------------
--
-- Este módulo eliminará los caracteres innecesarios de los textos
-- como espacios en blanco, signos de puntuación, los tags de las webs
-- y pasará todos las cadenas de carácteres a minúscula.
--
------------------------------------------------------------------

module Tokenizer
( stripTags
, limpiarDatos
, pasarAMinusculas
, toList
) where

import Data.Array
import Data.Char (toLower)

-- Tipo que representa un documento
type Documento = [String]

-- Tipo que representar un corpus (un conjunto de documentos)
type Corpus a = [Documento]

type Vector a = Array Int a

--Funcion para eliminar los tags web
stripTags :: String -> String
stripTags []         = []
stripTags ('<' : x:xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs


--Eliminamos los signos de puntuación y los carácteres especiales
limpiarDatos :: String -> String
limpiarDatos xs = [ x | x <- noTag, not (x `elem` "(),.?!:;\"\'")]
    where noTag = stripTags xs

--Pasamos el texto a minúsculas
pasarAMinusculas :: String -> String
pasarAMinusculas xs = map toLower xs

toList :: String -> Documento
toList xs = words minus
    where ld = limpiarDatos xs
          minus = pasarAMinusculas ld