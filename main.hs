--Importación de librerias
import System.IO (readFile, print)
import System.Environment (getArgs)
import System.Directory ( doesDirectoryExist, listDirectory )
import Tokenizer as Tk ( toList )
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath ((</>), splitDirectories)
import Control.Monad (filterM)
import Data.Char ( isDigit )
import DocVector as Dv ( pesosEnDocumento )
import Knn
    ( obtieneClasesMayoritarias,
      obtieneListasClases,
      obtienePrecision )
import Data.Array
import Data.List ( nub )
import System.Random
import Types

main0 :: IO ()
main0 = do
    args <- getArgs
    if length args > 1 then do
        let carpetas = args
        -- Comprobamos si la carpeta existe
        existeCarpeta <- mapM doesDirectoryExist carpetas
        -- Le pedimos al usuario que introduzca un valor de k
        -- Si todas las carpetas están en el directorio a leer los ficheros
        if all (==True) existeCarpeta then do
            -- Le pedimos al usuario que introduzca un valor de k
            k <- leerK
            -- Obtenemos las carpetas que hay dentro de la carpeta principales, que serán nuestras clases
            clasesC <- mapM obtieneDirectorios carpetas
            -- Eliminamos las clases repetidas
            let clases = claseUnica clasesC
            -- Obtenemos el path de los ficheros que están en las carpetas
            ficheros <- mapM getFilesRecursive carpetas
            -- Leemos los ficheros
            contenidoTrain <- obtenerDatos (head ficheros)
            contenidoTest <- obtenerDatos (last ficheros)
            -- Creamos los conjuntos de datos
            let x_train = [Tk.toList c | c <- contenidoTrain]
            let x_test = [Tk.toList c | c <- contenidoTest]
            -- Creamos los conjuntos que contienen las clases de los datos
            let y_train = creaY clases (head ficheros)
            let y_test= creaY clases (last ficheros)
            -- Calculamos la representación vectorial de cada documento
            let pesostrain = pesosEnDocumento x_train
            let pesostest = pesosEnDocumento x_test
            -- Obtenemos las clases de los documentos k más cercanos
            let clases = obtieneListasClases pesostrain pesostest y_train k
            -- Obtenemos la clase predominante para en cada 
            let sol = obtieneClasesMayoritarias clases
            -- Calculamos la precisión de la clasificación
            let precision = obtienePrecision sol y_test
            print ("Rendimiento sobre el conjunto de test: " ++ show precision)
        else
            -- Si no existe la carpeta devolvemos un error
            putStrLn "Los directorios no existen"
    else do
        -- Si no introducimos ningún path, devolvemos un error
        putStrLn "Número de parámetros incorrecto"

main :: IO ()
main = main0

--Funcion para obtener los directorios de los datos
obtieneDirectorios :: FilePath -> IO [FilePath]
obtieneDirectorios filePath = listDirectory filePath >>= filterM (doesDirectoryExist . (filePath </>))

--Función que elimina las clases repetidas
claseUnica :: [[FilePath]] -> [FilePath]
claseUnica xss = concat (nub xss)

-- Función que lee las reviews
obtenerDatos :: [FilePath] -> IO [String]
obtenerDatos path = traverse readFile path

--Función que trozea el path
trozeaPath :: [FilePath] -> [[FilePath]]
trozeaPath path = [splitDirectories i | i <- path]

-- Función que obtiene la lista con las clases de las reviews
creaY :: [String] -> [FilePath] -> [Integer]
creaY clases path = [if (perteneceA!!i) == head clases then 0 else 1 | i <- [0.. length perteneceA - 1]]
    where trPath = trozeaPath path
          perteneceA = [(trPath!!i)!!j | j <- [0..length (head trPath) - 1],
           i <- [0..length trPath - 1],
           (trPath!!i)!!j == head clases || (trPath!!i)!!j == last clases]

leerK :: IO Int
leerK = do
    putStrLn "Introduce un valor para k: "
    a <- getLine
    if not (null a) then
        if allDigits a then
            return (read a)
        else do
            print "Introduce un valor correcto"
            leerK
    else do
        let k = "5"
        return (read k)

allDigits :: [Char] -> Bool
allDigits str
      | str == "" = True
      | otherwise = isDigit (head str) && allDigits (drop 1 str)

leeDigito :: String -> IO Int
leeDigito c = do
    putStrLn c
    x <- getChar
    if isDigit x then
        return (read [x])
    else do
       let k = 5
       return k
