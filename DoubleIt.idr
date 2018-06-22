doubleIt : Int -> Int
doubleIt x = x + x

doubleGeneric : Num ty => ty -> ty
doubleGeneric x = x + x

doubleFunctionCall : (a -> a) -> a -> a
doubleFunctionCall f a = f (f a)
