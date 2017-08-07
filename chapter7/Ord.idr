record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

help : Album
help = MkAlbum "The beatles" "help" 1965

heros : Album
heros = MkAlbum "David Bowie" "Heros" 1977

collection : List Album
collection = [help, heros]

Eq Album where
  (==) (MkAlbum artist title year) (MkAlbum artist' title' year')
    = artist == artist' && title == title' && year == year'

Ord Album where
  compare (MkAlbum artist title year) (MkAlbum artist' title' year')
    = case compare artist artist' of
      EQ => case compare year year' of
        EQ => compare title title'
        diff_year => diff_year
      diff_artist => diff_artist

-- exercises

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

-- Eq for Shape
Eq Shape where
    (==) (Triangle x y) (Triangle x' y') = x == x' && y == y'
    (==) (Rectangle x y) (Rectangle x' y') = x == x' && y == y'
    (==) (Circle r) (Circle r') = r == r'
    (==) _ _ = False

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle b h) = b * h
area (Circle r) = pi * r * r


Ord Shape where
    compare x y = compare (area x) (area y)


testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]

Show Album where
    show (MkAlbum artist title year) = title ++ " by " ++ artist ++ " (released " ++ show year ++ ")"
