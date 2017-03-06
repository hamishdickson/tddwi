-- extension of Enum type

||| Represents a Shape
data Shape = ||| Triangle with a base and height length
             Triangle Double Double
           | ||| Rectangle with width and height
             Rectangle Double Double
           | ||| Circle with radius
             Circle Double

-- awesome fact - `pi` is defined in Prelude
area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle width height) = width * height
area (Circle radius) = pi * radius * radius
