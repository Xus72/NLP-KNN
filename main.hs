--Importación de librerias
import System.IO
import System.Environment (getArgs)
import System.Directory
import Control.Exception
import Data.Char (toLower)
import System.IO.Error
import Data.List
import Tokenizer as Tk

main0 :: IO ()
main0 = do
    args <- getArgs
    if length args > 0 then do
        let carpeta = head args
        existeCarpeta <- doesDirectoryExist carpeta
        if existeCarpeta then do
            ficheros <- listDirectory carpeta
            let prefijo = map ((carpeta ++ "/") ++) ficheros
            contents <- traverse readFile prefijo
            let train = [Tk.toList (contents!!i) | i <- [0..length contents - 1]]
            let corpus = [[contents!!i] | i <- [0..length contents - 1]]
            print corpus
        else
            putStrLn ("El directorio " ++ carpeta ++ " no existe")
    else do
        putStrLn "Debe introducir un directorio existente"

main = main0

