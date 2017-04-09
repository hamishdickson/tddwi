import Data.Vect

Postition : Type
Postition = (Double, Double)

tri1 : Vect 3 Postition
tri1 = [(0.0, 0.0), (0.0, 3.0), (0.0, 4.0)]

Polygon : Nat -> Type
Polygon k = Vect k Postition

tri : Polygon 3
tri = [(0.0, 0.0), (0.0, 3.0), (0.0, 4.0)]
