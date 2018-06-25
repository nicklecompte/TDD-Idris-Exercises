||| Format providers for printf. Ex: "This %s is on page number %d" <==> (Lit "This " (Str (Lit " is on page number " (Number End))))
data Format = Number Format -- %i followed by the rest of the format specifier
            | Str Format -- %s followed by rest of format specifier
            | Chr Format -- %c
            | Dbl Format -- %d
            | Lit String Format -- a literal string, followed by rest of provider
            | End -- empty format provider

||| Given a Format, get the type of the argument that must correspond to those arguments. E.g (Str (Lit "=" (Number End)) has type String -> Int -> String
PrintfType : Format -> Type
PrintfType (Number fmt) = (i: Int) -> PrintfType fmt
PrintfType (Str fmt) = (str:String) -> PrintfType fmt
PrintfType (Chr fmt) = (chr:Char) -> PrintfType fmt
PrintfType (Dbl fmt) = (dbl:Double) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String
            
||| Helper function to build a string from a format, providing lambdas for Str and Number inputs
printfFmt : (fmt : Format) -> (acc: String) -> PrintfType fmt
printfFmt (Number x) acc = (\i => printfFmt x (acc ++ (show i)))
printfFmt (Str x) acc = (\str => printfFmt x (acc ++ str))
printfFmt (Dbl x) acc = (\dbl => printfFmt x (acc ++ (show dbl)))
printfFmt (Chr x) acc = (\ch => printfFmt x (acc ++ (show ch)))
printfFmt (Lit x y) acc = printfFmt y (acc ++ x)
printfFmt End acc = acc

||| Reader that takes in a List Char and outputs a Format - goes through char-by-char and splits accordingly
toFormat : List Char -> Format
toFormat [] = End
toFormat ('%' :: 'i' :: xs) = Number (toFormat xs)
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
toFormat ('%' :: 'c' :: xs) = Chr (toFormat xs)
toFormat ('%' :: 'd' :: xs) = Dbl (toFormat xs)
toFormat ('%' :: xs) = Lit "%" (toFormat xs)
toFormat (x :: xs) = case toFormat xs of
                        Lit strng fmt => Lit (strCons x strng) fmt
                        fmt => Lit (strCons x "") fmt

||| Printf :)
printf : (fmt : String) -> PrintfType (toFormat (unpack fmt)) -- in the type definition we get the Format and type the argument of printf accordingly
printf fmt = printfFmt _ "" -- you can use an underscore for this. 
                            -- Idris knows the type must be PrintfType (toFormat (unpack fmt))
                            -- and that therefore the ONLY possible argument to printfFmt is (toFormat (unpack fmt))
