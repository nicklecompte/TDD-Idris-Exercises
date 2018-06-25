StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "ninety-four"
getStringOrInt True = 94

||| Converts a string or an int to a canonical string representation
valToString : (isInt : Bool) -> (StringOrInt isInt) -> String
valToString False x = trim x
valToString True x = cast x

||| Converts a string or an into to a canonical string representation, without a helper function
valToString2 : (isInt : Bool) -> (case isInt of
                                       True => Int
                                       False => String) -> String
valToString2 False x = trim x
valToString2 True x = cast x
