data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
-- If you use Atom-Idris autocomplete to fill in the function definition,
-- it doesn't even give an option for Bicycle -
-- because Vehicle is a dependent type, Vehicle Petrol as the input type
-- means that "refuel Bicycle" is an illegal input
-- you can make this explicit with "refuel Bicycle impossible"
-- but Idris knows it's impossible. If it IS possible but you assert it isn't,
-- that's also illegal and Idris will get sad.
