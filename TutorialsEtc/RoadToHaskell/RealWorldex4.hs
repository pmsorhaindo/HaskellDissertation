splitLines :: String -> [String]

splitLines [] = []
splitLines cs =
        let (pre, suf) = break isLineTerminator cs -- break: a function provided by prelude to which splits lines based on a boolean function applied to each element in a list.
        in pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest) -> splitLines rest
                ('\n':rest) -> splitLines rest
                _ -> []

isLineTerminator c = c == '\r' || c == '\n' -- definition of isLineTerminator used in the above function

fixLines :: String -> String
fixLines input = unlines (splitLines input)

