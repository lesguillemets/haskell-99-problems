import qualified System.Random as Random

rndSelect xs n = do
    gen <- Random.getStdGen
    return $ randSelector gen xs n

randSelector :: Random.StdGen -> [a] -> Int -> [a]
randSelector gen xs n = map (\(x,_)->xs!!x) $ take n choices
    where
        choices = tail $ iterate (\(n,gen) -> Random.randomR (0,length xs-1) gen) (0,gen)


main = do
    rndSelect "abcdefgh" 3 >>= print
