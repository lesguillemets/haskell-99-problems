dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupliAcc :: [a] -> [a] -> [a]
dupliAcc [] acc = acc
dupliAcc (x:xs) acc = dupliAcc xs (x:x:acc)
dupli' = (flip dupliAcc $ []) . reverse

dupli'' = concatMap (replicate 2)

main = do
    print $ dupli [1,2,3]
    print $ dupli' [1,2,3]
    print $ dupli'' [1,2,3]
