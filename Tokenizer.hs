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
import Types

--Funcion para eliminar los tags web
stripTags :: String -> String
stripTags []         = []
stripTags ('<' : x:xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs


--Eliminamos los signos de puntuación y los carácteres especiales
limpiarDatos :: String -> String
limpiarDatos xs = [ x | x <- noTag, not (x `elem` "-$%(),.?!:;\"\'")]
    where noTag = stripTags xs

--Pasamos el texto a minúsculas
pasarAMinusculas :: String -> String
pasarAMinusculas xs = map toLower xs



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