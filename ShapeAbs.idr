module ShapeAbs

export
data Shape = Triangle Double Double |
             Rectangle Double Double |
             Circle Double

export
triangle : Double -> Double -> Shape
triangle = Triangle            

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle

public export
data ShapeView : Shape -> Type where
    ShapeViewTriangle : ShapeView (triangle base height)
    ShapeViewRectangle : ShapeView (rectangle length height)
    ShapeViewCircle : ShapeView (circle radius)                        

export
shapeView : (shape: Shape) -> (ShapeView shape)
shapeView (Triangle x y) = ShapeViewTriangle
shapeView (Rectangle x y) = ShapeViewRectangle
shapeView (Circle x) = ShapeViewCircle
--shapeView (Rectangle _ _) impossible
--shapeView (Circle _) impossible