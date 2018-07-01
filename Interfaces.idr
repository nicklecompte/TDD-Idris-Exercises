occurrences : Eq ty => (item: ty) -> (values : List ty) -> Nat
occurrences item [] = Z
occurrences item (x :: xs) = case (x == item) of
                                False => (occurrences item xs)
                                True => S (occurrences item xs)

data Matter = Solid | Liquid | Gas

Eq Matter where
    (==) Solid Solid = True
    (==) Liquid Liquid = True
    (==) Gas Gas = True
    (==) _ _ = False

    (/=) a b = not (a == b)

-- extend Tree from Ch. 4 to implement Eq

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

-- this says "if equality is defined on the element, it extends to the Tree"               
Eq elem => Eq (Tree elem) where               
  (==) Empty Empty = True
  (==) (Node leftX x rightX) (Node leftY y rightY)
    = (leftX == leftY) && (x == y) && (rightX == rightY)
  (==) _ _ = False
  (/=) x y = not (x == y)

interface MyEq ty where
    (==) : ty -> ty -> Bool
    (/=) : ty -> ty -> Bool

    (==) x y = not (x /= y)
    (/=) x y = not (x == y)

interface MyEq ty => MyOrd ty where
    compare : ty -> ty -> Ordering
    (<) : ty -> ty -> Bool
    (>) : ty -> ty -> Bool
    (<=) : ty -> ty -> Bool
    (>=) : ty -> ty -> Bool
    max : ty -> ty -> ty
    min : ty -> ty -> ty

    -- default implementations
    (<) x y = case (compare x y) of
                LT => True
                _ => False
    (>) x y = case (compare x y) of
                GT => True
                _ => False
    (<=) x y = case (compare x y) of
                GT => False
                _ => True
    (>=) x y = case (compare x y) of
                LT => False
                _ => True
    max x y = case (compare x y) of
                LT => y
                _ => x
    min x y = case (compare x y) of
                LT => x
                _ => y                                
    
    
record Album where
    constructor MkAlbum
    artist : String
    title : String
    year : Integer

Eq Album where
  (==) x y = (artist x == artist y) && (title x == title y) && (year x == year y)

Ord Album where  
  compare x y = case (compare (artist x) (artist y)) of
                    EQ => case (compare (title x) (title y)) of
                            EQ => compare (year x) (year y)
                            a => a
                    b => b
        
    
help : Album
help = MkAlbum "The Beatles" "Help!" 1965

blackSaint : Album
blackSaint = MkAlbum "Charles Mingus" "The Black Saint and the Sinner Lady" 1960

mingusAhUm : Album
mingusAhUm = MkAlbum "Charles Mingus" "Mingus Ah Um" 1957

ninetyThreeTilInfinity : Album
ninetyThreeTilInfinity = MkAlbum "Souls of Mischief" "'93 'Til Infinity" 1993

liveAtTheRegal : Album
liveAtTheRegal = MkAlbum "B.B. King"  "Live At the Regal" 1965

liveAtTheRegal2 : Album
liveAtTheRegal2 = MkAlbum "B.B. King"  "Live At the Regal" 1966


collection : List Album
collection = [liveAtTheRegal2, mingusAhUm,help, blackSaint, ninetyThreeTilInfinity,liveAtTheRegal]

Show Album where
    show (MkAlbum artist title year)
        = title ++ " by " ++ artist ++ " (released in " ++ show year ++ ")"