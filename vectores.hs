import Control.Concurrent
    ( forkIO, newEmptyMVar, putMVar, takeMVar, readMVar )

-- Funcion que recibe dos vectores a ser multiplicados
-- el indice del los elementos que se estan multiplicando actualmente
-- y una lista donde se almacenaran las multiplicaciones de los elementos
-- para ser sumadas al final de la ejecucion del programa
multiplicarVectores:: [Int] -> [Int] -> Int -> [Int] -> IO Int
multiplicarVectores v1 v2 (-1) lista = return (sum lista)
multiplicarVectores v1 v2 indice lista = do
    m <- newEmptyMVar
    forkIO $ putMVar m $ (v1!!indice)*(v2!!indice)
    valor <- readMVar m
    let nuevaLista = lista ++ [valor]
    --putStrLn $ "lista : " ++ show nuevaLista
    multiplicarVectores v1 v2 (indice-1) nuevaLista


main = do
    let v1 = [1,2,3]
    let v2 = [5,6,7]
    let len = length v1
    resultado <- multiplicarVectores v1 v2 (len-1) []
    putStrLn $ show resultado
    