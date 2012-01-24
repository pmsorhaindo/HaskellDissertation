import System.Environment (getArgs)

interactWith function inputFile outputFile = do
                input <- readFile inputFile
                writeFile outputFile (function input)

main = mainWith myFunction
        where mainWith function = do
                args <- getArgs
                case args of
                   [input,output] -> interactWith function input output
                   _ -> putStrLn "Error: exactly two arguments needed."
              --replace the value assigned to myFunction with the functions name.
              myFunction = id


