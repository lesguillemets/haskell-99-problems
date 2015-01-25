import qualified System.Random as Random

diffSelect n m = do
    gen <- Random.getStdGen
    return $ randChoise gen n m

randChoise :: Random.StdGen -> Int -> Int -> [Int]
randChoise gen n m = differ $ map (fst . fst) $ take n choices
    where
        choices = tail $ iterate (\((_,gn),i) -> ((Random.randomR (0,i) gn),i-1)) ((0,gen),m)

differ :: [Int] -> [Int]
differ ns = differ' ns []
differ' :: [Int] -> [Int] -> [Int]
differ' [] ns = ns
differ' (x:xs) ns = differ' xs ((x+add):ns)
    where
        add = length . filter (<=x) $ ns

main = do
    diffSelect 6 49 >>= print
