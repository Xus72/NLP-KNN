module Types
( Documento
, Corpus
, Vector
, Matriz
) where

import Data.Array

-- Tipo que representa un documento
type Documento = [String]

-- Tipo que representar un corpus (un conjunto de documentos)
type Corpus a = [Documento]

type Vector a = Array Int a

type Matriz a = Array (Int,Int) a