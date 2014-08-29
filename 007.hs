data NestedList a = Elem a | List [NestedList a]

main = do
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

flatten :: (NestedList a) -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (++) [] (map flatten xs)
