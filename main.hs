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
            let perteneceA = [(pathTrozeado!!i)!!j | j <- [0..length (pathTrozeado!!0) - 1], i <- [0..length pathTrozeado - 1], (pathTrozeado!!i)!!j == (clases!!0) || (pathTrozeado!!i)!!j == (clases!!1)]
            -- Creamos el conjunto de entrenamiento con las clases
            let y_train = [if (perteneceA!!i) == (clases!!0) then 0 else 1 | i <- [0.. length perteneceA - 1]]
            -- Leemos los ficheros
            contents <- traverse readFile ficheros
            -- Creamos el conjunto de entrenamiento
            let x_train = [Tk.toList (contents!!i) | i <- [0..length contents - 1]]
            --print x_train
             --let ndp = numDocPalabra x_train "see"
            --print ndp
            --let pesos = matrizLista $ pesosEnDocumento x_train
            --print pesos
            let v1 = [0.52287877,0.0,0.52287877,0.0,0.0,0.0,0.0,0.0,0.0,0.52287877]
            let v2 = [2.091515,1.0457575,0.52287877,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
            let vecinos = obtieneVecinos x_train v1
            let zipped = zip x_train y_train
            let clases = obtieneClases vecinos zipped 5
            print clases
            --let s = map snd productos
            --print s
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

