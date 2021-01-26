--Importación de librerias
import System.IO
import System.Environment (getArgs)
import System.Directory
import Control.Exception
import Data.Char (toLower)
import System.IO.Error

main0 = do
    args <- getArgs
    if length args > 0 then do
        let carpeta = head args
        existeCarpeta <- doesDirectoryExist carpeta
        if existeCarpeta then do
            ficheros <- getDirectoryContents carpeta
            print ficheros
        else
            putStrLn ("El directorio " ++ carpeta ++ " no existe")
    else do
        putStrLn "Debe introducir un directorio existente"

main = main0