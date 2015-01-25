import qualified System.Random as Random


rndPerm xs = do
    gen <- Random.getStdGen
    return $ randSelector gen xs (length xs)

randSelector :: Random.StdGen -> [a] -> Int -> [a]
randSelector gen xs n = map (\(x,_)->xs!!x) $ take n choices
    where
        choices = tail $ iterate (\(_,ge) -> Random.randomR (0,length xs-1) ge) (0,gen)

main = do
    rndPerm "abcdefg" >>= print
