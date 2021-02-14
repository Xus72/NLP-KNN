------------------------------------------------------------------
--
-- Este módulo eliminará los caracteres innecesarios de los textos
-- como espacios en blanco, signos de puntuación, los tags de las webs
-- y pasará todos las cadenas de carácteres a minúscula.
--
------------------------------------------------------------------

module Tokenizer
( eliminaTags
, limpiarDatos
, pasarAMinusculas
, toList
) where

import Data.Array
import Data.Char (toLower, isDigit)
import Types ( Documento )

--Funcion para eliminar los tags web
eliminaTags :: String -> String
eliminaTags []         = []
eliminaTags ('<' : x:xs) = eliminaTags $ drop 1 $ dropWhile (/= '>') xs
eliminaTags (x : xs)   = x : eliminaTags xs


-- Función que elimina los signos de puntuación, los caracteres especiales y los tags
limpiarDatos :: String -> String
limpiarDatos xs = [ x | x <- noDig, not (x `elem` "-$%(),.?!:;/\"\'")]
    where noDig = eliminaDigitos noTag
          noTag = eliminaTags xs

--Funcion que elimina los dígitos
eliminaDigitos :: String -> String 
eliminaDigitos xs = [x | x <- xs, not (x `elem` "0123456789")]

--Pasamos el texto a minúsculas
pasarAMinusculas :: String -> String
pasarAMinusculas xs = map toLower xs


-- Función que elimina las stopWords y devuelve el resultado de limpiar los datos 
toList :: String -> Documento 
toList xs = filter (\x -> not (x `elem` mostUsedWords)) ls
    where ls = words minus
          ld = limpiarDatos xs
          minus = pasarAMinusculas ld

mostUsedWords :: [String]
mostUsedWords = ["i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you"
     , "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she"
     , "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs"
     , "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am"
     ,"is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having"
     , "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because"
     , "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between"
     , "into", "through", "during", "before", "after", "above", "below", "to", "from", "up"
     , "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once"
     , "here", "there", "when", "where", "why", "how", "all", "any", "both"
     , "each"
     , "few"
     , "more"
     , "most"
     , "other"
     , "some"
     , "such"
     , "no"
     , "nor"
     , "not"
     , "only"
     , "own"
     , "same"
     , "so"
     , "than"
     , "too"
     , "very"
     , "s"
     , "t"
     , "can"
     , "will"
     , "just"
     , "don"
     , "should"
     , "now"
     ]