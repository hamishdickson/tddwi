data MyNat = Z | S MyNat


||| Represents a Shape - taken from Unions.idr
data Shape = ||| Triangle with a base and height length
             Triangle Double Double
           | ||| Rectangle with width and height
             Rectangle Double Double
           | ||| Circle with radius
             Circle Double

data Picture = Primitive Shape
            | Combine Picture Picture
            | Rotate Double Picture
            | Translate Double Double Picture

testPicture : Picture
testPicture = Combine ?pic1 (Combine ?pic2 ?pic3)
