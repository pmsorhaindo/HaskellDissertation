--Random Nums

import System.Random

main = do
        gen <- newStdGen
        let ns = randoms gen :: [Float]
        print $ take 10 ns

