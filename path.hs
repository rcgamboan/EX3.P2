-- Pregunta 2, Parte b, Examen 3 CI3641.
-- Elaborado por Roberto Gamboa, 16-10394

-- Implementacion que recibe la ruta de una carpeta y cuenta todos los archivos
-- en sus subdirectorios

import Control.Monad ( forM )
import System.Directory
    ( listDirectory,
      doesDirectoryExist,
      doesFileExist,
      getCurrentDirectory )
import System.FilePath ((</>))
import Data.List ( intercalate )
import Control.Concurrent ( forkIO )


-- Funcion que recibe la ruta del directorio desde el que empezara a buscar los archivos
-- Se obtienen todos los archivos y directorios contenidos en la ruta suministrada
-- Por cada archivo obtenido se verifica si es un directorio o un archivo regular
-- Si se trata de un directorio, se crea un hilo con la funcio forkIO
-- y se llama recursivamente a la funcion con el directorio encontrado como raiz
-- retorna una lista con todos los archivos encontrados en el arbol de directorios
-- que tiene como raiz la ruta suministrada
listarArchivos :: FilePath -> IO [FilePath]
listarArchivos ruta = do
    
    archivos <- listDirectory ruta
    paths <- forM archivos $ \archivo -> do
        let path = ruta </> archivo
        esDirectorio <- doesDirectoryExist path
        if esDirectorio
        then do
            forkIO $ do return ()
            listarArchivos path
        else return [path]
    return (concat paths)

main = do
    path <- getCurrentDirectory
    archivos <- listarArchivos path
    putStrLn $ "Hay " ++ show (length archivos) ++ " archivos " ++ "en el directorio actual y sus subdirectorios"
    putStrLn "Archivos:"
    putStrLn $ intercalate "\n" archivos