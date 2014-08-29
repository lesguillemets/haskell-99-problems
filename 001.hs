main = do
    print $ myLast "there"
    print $ myLast' "there"
    print $ myLast'' "there"

myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myLast' = head . reverse

myLast'' l = head $ drop (length l-1) l

myLast''' l = l!!(length l-1)
