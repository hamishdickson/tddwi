
import Data.Vect

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : Vehicle Pedal
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol


wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motorcycle fuel) = 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible -- this is cool, Bicycle wouldn't type check here, but you can add it for clarity

takeVect : (n : Nat) -> Vect (n + m) a -> Vect n a
takeVect Z xs = []
takeVect (S k) (x :: xs) = x :: takeVect k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just idx) => Just (index idx xs + index idx ys)
