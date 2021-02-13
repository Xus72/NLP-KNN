--Importación de librerias
import System.IO (readFile, print)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory)
import Tokenizer as Tk ( toList )
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath ((</>), splitDirectories)
import Control.Monad (filterM)
import Data.Char
import DocVector as Dv
import Knn
import Data.Array
import Data.List
import System.Random
import Types

main0 :: IO ()
main0 = do
    args <- getArgs
    if length args > 1 then do
        let carpetas = args
        let carpeta = head args
        k <- leeDigito "Introduce un valor de k"
        -- Comprobamos si la carpeta existe
        existeCarpeta <- mapM doesDirectoryExist carpetas
        -- Si existe leemos su contenido
        if head existeCarpeta then do
            -- Obtenemos las carpetas que hay dentro de la carpeta principales, que serán nuestras clases
            clasesC <- mapM getDirectories carpetas
            -- Eliminamos las clases repetidas
            let clases = claseUnica clasesC
            -- Cargamos los ficheros que hay dentro de la carpeta
            ficheros2 <- mapM getFilesRecursive carpetas
            let ficheros = concat ficheros2
            --print ficheros
            -- Trozeamos el path de los ficheros
            let pathTrozeado = [splitDirectories i | i <- ficheros]
            -- Obtenemos a que carpeta pertenece cada fichero
            let perteneceA = [(pathTrozeado!!i)!!j | j <- [0..length (head pathTrozeado) - 1], i <- [0..length pathTrozeado - 1], (pathTrozeado!!i)!!j == head clases || (pathTrozeado!!i)!!j == (clases!!1)]
            -- Creamos el conjunto de entrenamiento con las clases
            let y = [if (perteneceA!!i) == head clases then 0 else 1 | i <- [0.. length perteneceA - 1]]
            -- Leemos los ficheros
            contenido <- traverse readFile ficheros
            -- Creamos el conjunto de entrenamiento
            let x = [Tk.toList c | c <- contenido]
            let mezclaConj = mezclaConjuntos x y 20
            let xtrain = fstElem mezclaConj
            let xtest = sndElem mezclaConj
            let pesostrain = pesosEnDocumento xtrain
            let pesostest = pesosEnDocumento xtest
            let ytrain = trdElem mezclaConj
            let ytest = fthElem mezclaConj
            let clases = obtieneListasClases pesostrain pesostest ytrain k
            let sol = obtieneClasesMayoritarias clases
            --print sol
            --print y_train
            let precision = obtienePrecision sol ytest
            print ("Rendimiento sobre el conjunto de test: " ++ show precision)
        else
            -- Si no existe la carpeta devolvemo un error
            putStrLn ("El directorio " ++ carpeta ++ " no existe")
    else do
        -- Si no introducimos ningún path, devolvemos un error
        putStrLn "Debe introducir un directorio existente"

main :: IO ()
main = main0

--Funcion para obtener los directorios de los datos
getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = listDirectory filePath >>= filterM (doesDirectoryExist . (filePath </>))

--Función que elimina las clases repetidas
claseUnica :: [[FilePath]] -> [FilePath]
claseUnica xss = concat (nub xss)

--Funcion que mezcla el conjunto de entrenamiento 
mezclaConjuntos :: [Documento] -> [Integer] -> Int -> ([Documento],[Documento],[Integer],[Integer])
mezclaConjuntos x y porc = (xtrain,xtest,ytrain,ytest)
    where xtrain = [x | i <- listIndXtrain, (x,y) <- zip x [1..lenXtrain], i == y]
          xtest = [x | i <- listIndXtest, (x,y) <- zip x [lenXtrain..lenX], i == y]
          ytrain = [x | i <- listIndXtrain, (x,y) <- zip y [1..lenXtrain], i == y]
          ytest = [x | i <- listIndXtest, (x,y) <- zip y [lenXtrain..lenX], i == y]
          lenX = length x
          porce = div (lenX * porc) 100
          lenXtrain = lenX - porce
          lenXtest = lenX - lenXtrain
          listIndXtrain = take lenXtrain $ randomRs (1, lenXtrain-1) (mkStdGen 8)
          listIndXtest = take lenXtest $ randomRs (lenXtrain, lenX) (mkStdGen 8)

leeDigito :: String -> IO Int
leeDigito c = do
    putStrLn c
    x <- getChar 
    if isDigit x then
        return (read [x])
    else do
       let k = 0
       return k

fstElem :: (a,b,c,d) -> a
fstElem (a,_,_,_) = a
sndElem :: (a,b,c,d) -> b
sndElem (_,b,_,_) = b
trdElem :: (a,b,c,d) -> c
trdElem (_,_,c,_) = c
fthElem :: (a,b,c,d) -> d
fthElem (_,_,_,d) = d
