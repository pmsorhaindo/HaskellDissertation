import Data.Char

main = do
        putStrLn "Hello , what is your name?"
        name <- getLine
        putStrLn ("Hey " ++ name ++ ", you rock!")
        
        putStrLn "What's your first name?"
        firstName <- getLine
        putStrLn "What's your last name?"
        lastName <- getLine
        let bigFirstName = map toUpper firstName
            bigLastName = map toUpper lastName
        putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
