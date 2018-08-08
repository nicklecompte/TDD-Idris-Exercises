labelWith: Stream labelType -> List a -> List (labelType, a)
labelWith xs [] = []
labelWith (label :: labels) (x :: xs) = (label,x) :: labelWith labels xs

label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)