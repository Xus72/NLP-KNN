--Importación de librerias
import System.IO (readFile, print)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory)
import Tokenizer as Tk ( toList )
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath ((</>), splitDirectories)
import Control.Monad (filterM)
import DocVector as Dv
import Knn
import Data.Array


main0 :: IO ()
main0 = do
    args <- getArgs
    if length args > 0 then do
        let carpeta = head args
        -- Comprobamos si la carpeta existe
        existeCarpeta <- doesDirectoryExist carpeta
        -- Si existe leemos su contenido
        if existeCarpeta then do
            -- Obtenemos las carpetas que hay dentro de la carpeta principales, que serán nuestras clases
            clases <- getDirectories carpeta
            -- Cargamos los ficheros que hay dentro de la carpeta
            ficheros <- getFilesRecursive carpeta
            -- Trozeamos el path de los ficheros
            let pathTrozeado = [splitDirectories (ficheros!!i) | i <- [0..length ficheros - 1]]
            -- Obtenemos a que carpeta pertenece cada fichero
            let perteneceA = [(pathTrozeado!!i)!!j | j <- [0..length (head pathTrozeado) - 1], i <- [0..length pathTrozeado - 1], (pathTrozeado!!i)!!j == head clases || (pathTrozeado!!i)!!j == (clases!!1)]
            -- Creamos el conjunto de entrenamiento con las clases
            let y_train = [if (perteneceA!!i) == head clases then 0 else 1 | i <- [0.. length perteneceA - 1]]
            -- Leemos los ficheros
            contents <- traverse readFile ficheros
            -- Creamos el conjunto de entrenamiento
            let x_train = [Tk.toList (contents!!i) | i <- [0..length contents - 1]]
            --print x_train
             --let ndp = numDocPalabra x_train "see"
            --print ndp
            let pesos = pesosEnDocumento x_train
            --print pesos
            --let v1 = [0.52287877,0.0,0.52287877,0.0,0.0,0.0,0.0,0.0,0.0,0.52287877]
            --let v1 = array (1,10) [(1,2.091515),(2,1.0457575),(3,0.52287877),(4,0.0),(5,0.0),(6,0.0),(7,0.0),(8,0.0),(9,0.0),(10,0.0)]
            --let v2 = [2.091515,1.0457575,0.52287877,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
            let clases = obtieneListasClases pesos pesos y_train
            let sol = obtieneClasesMayoritarias clases
            --let zipped = zip dist y_train
            --let clases = vecinos zipped
            print sol
            let precision = obtienePrecision sol y_train
            print precision
            --let s = map snd product
            --print $ Dv.matrizLista pesos
        else
            -- Si no existe la carpeta devolvemo un error
            putStrLn ("El directorio " ++ carpeta ++ " no existe")
    else do
        -- Si no introducimos ningún path, devolvemos un error
        putStrLn "Debe introducir un directorio existente"

main = main0

--Funcion para obtener los directorios de los datos
getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = listDirectory filePath >>= filterM (doesDirectoryExist . (filePath </>))

