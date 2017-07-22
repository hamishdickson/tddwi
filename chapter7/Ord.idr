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
                Eq => case compare year year' of
                           Eq => compare title title'
                           diff_year => diff_year
                diff_artist => diff_artist
