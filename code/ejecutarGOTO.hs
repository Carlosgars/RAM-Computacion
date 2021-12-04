module Main where

import SimuladorGOTO (gotosimulator)
import CodDecodGOTO (codGOTO)

main:: IO ()
main = do putStrLn "Introduce el Programa GOTO. Debe ser una lista con las instrucciones."
     programa <- getLine
     putStrLn "Introduce el input del programa. Debe ser una lista."
     input <- getLine
     programaCodificado <- cod programa
     output <- gotoSimulador input programaCodificado
     putStrLn $ "El programa devuelve: " ++ (show output)


