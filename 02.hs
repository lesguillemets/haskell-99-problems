main = do
    print $ myButLast "there"
    print $ myButLast' "there"
    print $ myButLast'' "there"
    print $ myButLast''' "there"

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (_:[]) = error "too short"
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

myButLast' = head . tail . reverse

myButLast'' l = head $ drop (length l-2) l

myButLast'''  = last . init
