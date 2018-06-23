data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1

||| For inserting into a binary search tree. BST has left subtree with all elements < node, right subtree > node
insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left y right) = case (compare x y) of
                                    -- if x < y then we insert it in left subtree
                                    LT => Node (insert x left) y right
                                    EQ => orig
                                    GT => Node left y  (insert x right)

||| Takes a list of ordered values and puts them in a tree in binary search order
listToBST : Ord a => List a -> Tree a
listToBST [] = Empty
listToBST (x :: xs) = insert x (listToBST xs)

||| Takes a BST and flattens it in search order
bstToList : Ord a => Tree a -> List a
bstToList Empty = []
bstToList (Node left x right) = (bstToList left) ++ [x] ++ (bstToList right)

data Expression = Single Int
                                 | Addition Expression Expression
                                 | Subtraction Expression Expression
                                 | Multiplication Expression Expression

evaluate : Expression -> Int
evaluate (Single x) = x
evaluate (Addition x y) = (evaluate x) + (evaluate y)
evaluate (Subtraction x y) = (evaluate x) - (evaluate y)
evaluate (Multiplication x y) = (evaluate x) * (evaluate y)


testval: Int
testval = evaluate (Multiplication (Single 10) (Addition (Single 6) (Single 3)))
