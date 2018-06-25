import Data.Vect

Position : Type
Position = (Double,Double) -- type synonym for a coordinate pair

Polygon : Nat -> Type
Polygon n = Vect n Position