maybeAddLong : Maybe Int -> Maybe Int -> Maybe Int
maybeAddLong x y = case x of
                         Nothing => Nothing
                         Just x1 => case y of
                                        Nothing => Nothing
                                        Just x2 => Just(x1+x2)

maybeAddCombinator : Maybe Int -> Maybe Int -> Maybe Int
maybeAddCombinator x y = x >>= \x1 =>
                                y >>= \y1 => Just(x1 + y1)
                                
maybeAddDo : Maybe Int -> Maybe Int -> Maybe Int
maybeAddDo x y = do x1 <- x
                    y1 <- y
                    Just (x1 + y1)                                