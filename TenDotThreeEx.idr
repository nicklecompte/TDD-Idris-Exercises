import DataStore
import ShapeAbs

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues x with (storeView x)
  getValues x | SNil = []
  getValues (addToStorePublic (_,value) store) | (SAdd rec) = value :: (getValues store | rec)

testStore1 : DataStore (SString .+. SString .+. SInt)
testStore1 = addToStorePublic ("Mercury","Mariner 10",1974)
                (addToStorePublic ("Venus","Venera",1961) 
                    (addToStorePublic ("Uranus", "Voyager 2", 1986)
                        (addToStorePublic ("Pluto","New Horizon", 2005)
                            empty)))

area : Shape -> Double
area shape with (shapeView shape)
  area (triangle base height) | ShapeViewTriangle = 0.5 * base * height
  area (rectangle length height) | ShapeViewRectangle = length * height
  area (circle radius) | ShapeViewCircle = pi * radius * radius
