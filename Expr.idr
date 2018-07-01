data Expr num = Val num
              | Add (Expr num) (Expr num) 
              | Sum (Expr num)  (Expr num) 
              | Mul (Expr num) (Expr num) 
              | Div (Expr num) (Expr num) 
              | Abs (Expr num) 

eval : (Neg num, Integral num, Abs num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Sum x y) = (eval x) - (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Div x y) = div (eval x) (eval y)
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where              
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger -- function composition, fromInteger x = Val (fromInteger x)

Neg ty => Neg (Expr ty) where  
  negate x = 0 - x --  0 is defined from fromInteger
  (-) = Sum

showHelper : Show num => num -> num -> (opCharacter: Char) -> String
showHelper x y opCharacter =  "(" ++ show x ++ ") " ++ (pack [opCharacter]) ++ " (" ++ show y ++ ")"

Show num => Show (Expr num) where
  show (Val x) = show x
  show (Add x y) = showHelper x y '+'
  show (Sum x y) = showHelper x y '-'
  show (Mul x y) = showHelper x y '*'
  show (Div x y) = showHelper x y '/'
  show (Abs x) = "|" ++ show x ++ "|"
  showPrec d x = show x

(Neg num, Integral num, Abs num, Eq num) => Eq (Expr num) where
  (==) x y = (eval x) == (eval y)


(Neg num, Integral num, Abs num) => Cast (Expr num) num where
  cast orig = eval orig
  
Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sum x y) = Sum (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs (map func x)
