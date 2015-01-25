makeGroup :: [Int] -> [a] -> [[a]]
makeGroup nums members = makeGroup' nums members [[]]
makeGroup' :: [Int] -> [a] -> [[a]] -> [[a]]
makeGroup' [] _ grps = grps
makeGroup' _ [] _ = error "no"
makeGroup' (n:ns) mems grps
    = concat [makeGroup' ns restant (newgroup:grps) | (newgroup,restant) <- draw n mems]

draw :: Int -> [a] -> [([a],[a])]
draw 0 mems = [([],mems)]
draw n [] = []
draw n (x:xs) = withHim ++ withoutHim
    where
        withHim = [(x:ps,qs) | (ps,qs) <- draw (n-1) xs]
        withoutHim = [(ps,x:qs) | (ps,qs) <- draw n xs]

main = do
    print $ take 100 $ makeGroup [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
