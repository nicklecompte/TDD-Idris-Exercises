module Diff

diff_length : String -> String -> Nat
diff_length word1 word2
  = let len1 = length word1
        len2 = length word2 in
    case (lt len2 len1) of
        True => len1 - len2 
        _ => len2 - len1