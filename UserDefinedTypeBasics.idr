-- Enumerated type: define the valid possibilites directly.
-- Example from Prelude: data Bool = True | False.
-- F# calls em discriminated unions.
data Direction =
    North
  | South
  | East
  | West

total turnClockwise : Direction -> Direction
turnClockwise North = East -- hitting Ctrl-=Alt-C autofills in the types
turnClockwise South = West -- since it auto-checks Direction exhaustively the total directive holds
turnClockwise East = South
turnClockwise West = North

-- Union types are extensions of enumerated types.
-- The constructors of the type can themselves carry data.
-- E.g., you might try an enumated type of shapes:
-- data EnumeratedShape = Triangle | Rectangle | Circle
-- but you will want to couple a Triangle with say its base and height, or a circle with its radius.
-- The constructor for Bool takes no argument, you just take one of the two valid answers.
-- But you could also define Circle Double - let's use ||| to document this

||| Represents a few simple shapes
data Shape = ||| an isoceles triangle of x-dim base and y-dim height
             IsocelesTriangle Double Double
           | ||| A rectangle of x-dim length and y-dim width
             Rectangle Double Double
           | ||| A circle of radius
             Circle Double

area : Shape -> Double
area (IsocelesTriangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x


{-
Syntactical variation (more verbose but more flexible)
data Shape : Type where
     Triangle : Double -> Double -> Shape
     Rectangle : Double -> Double -> Shape
     Circle : Double -> Shape
-}

-- Recursive types
-- Ex: data Nat = Z | S Nat
-- so 0 = Z, 1 = S Z, 2 = S (S Z) and so on.
-- Prelude also includes helpers to-from int but this is what it is under the hood.
||| A type to build simple pictures from the simple Shape type
data Picture = ||| A Primitive picture is just a Shape (rectangle, circle, isoceles triangle)
               Primitive Shape
             | ||| A Picture can also be built by Combine two other pictures
               Combine Picture Picture
             | ||| A Picture created from a Picture rotated some degrees
               Rotate Double Picture
             | ||| A Picture created by an affine x-y translation of another picture
               Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (IsocelesTriangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
                (Combine (Translate 35 5 circle)
                (Translate 15 25 rectangle))

-- Idris autocompletion in Atom makes writing a "compute area of picture function easy"
pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic1 pic2) = pictureArea pic1 + pictureArea pic2
pictureArea (Rotate angle pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

-- Generic types
-- if you do :t List in REPL you get List : Type -> Type
-- that's because List isn't a type, it's a function that takes a type and returns a type
-- List Int is a type.
-- Defined in Prelude as data List elem = Nil | (::) elem (List elem)

-- maxMaybe returns the max of two inputs if they are Just a, or Nothing if they're both Nothing
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe _ Nothing = Nothing
maxMaybe Nothing _ = Nothing
maxMaybe (Just x) (Just y) = Just (max x y)

||| Returns Just the area of the biggest IsocelesTriangle in a picture if there is one, otherewise Nothing
biggestTriangle : Picture -> Maybe Double

biggestTriangle (Primitive x) = case x of
                                     IsocelesTriangle _ _ => Just(area x)
                                     _ => Nothing

biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate angle pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (IsocelesTriangle 2 3))
                   (Primitive (IsocelesTriangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 2 3))
                   (Primitive (Circle 4))
