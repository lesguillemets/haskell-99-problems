-- zero-indexed
main = do
    print $ elementAt [0..] 4
    print $ elementAt' [0..] 4
    print $ elementAt'' [0..] 4

elementAt :: [a] -> Int -> a
elementAt l n = l!!n

elementAt' l n = head $ drop n l

elementAt'' l 0 = head l
elementAt'' (_:xs) n =  elementAt'' xs (n-1)
elementAt'' _ _ = error "Index out of range"
