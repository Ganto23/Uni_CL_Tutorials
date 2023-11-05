-- Exercise 1
-- B doesnt have a bold outline
-- D id a different colour
-- C is a different shape
-- E is smaller
-- A is the only one without a distinct feature


-- Exercise 2

data Thing = A | B | C | D | E
    deriving (Eq,Show)

things :: [Thing]
things = [A,B,C,D,E]

-- Features : bold outline , orange or blue , square or circle , big or small

data Colour = Orange | Blue
    deriving (Eq,Show)

data Outline = Thick | Thin
    deriving (Eq,Show)

data Shape = Square | Circle
    deriving (Eq,Show)

data Size = Big | Small
    deriving (Eq,Show)

colour :: Thing -> Colour
colour A = Orange
colour B = Orange
colour C = Orange
colour D = Blue
colour E = Orange


outline :: Thing -> Outline
outline A = Thick
outline B = Thin
outline C = Thick
outline D = Thick
outline E = Thick


shape :: Thing -> Shape
shape A = Square
shape B = Square
shape C = Circle
shape D = Square
shape E = Square


size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small



--Exercise 3

type Predicate u = u -> Bool

isOrange :: Predicate Thing
isOrange x = x `elem` [A,B,C,E]

isBlue :: Predicate Thing
isBlue x = not (isOrange x)

isThick :: Predicate Thing
isThick x = x `elem` [A,C,D,E]

isThin :: Predicate Thing
isThin x = not (isThick x)

isSquare :: Predicate Thing
isSquare x = not (isCircle x)

isCircle :: Predicate Thing
isCircle x = x == C

isBig :: Predicate Thing
isBig x  = not (isSmall x)

isSmall :: Predicate Thing
isSmall x = x == E

isGreat :: Predicate Thing -> String
isGreat x = "isGreat"


-- Exercise 4
-- 1. [isThin x | x <- things , isBlue x && isSquare x]
-- 2. [not (isBig x) | x <- things , isOrange x && isCircle x]


-- Exercise 5
-- not (or[is Blue x | x <- things , is Square x])
-- and[not (isBlue x) | x <- things , isSquare x]



-- Exercise 6

thingsOtherThan :: Thing -> [Thing]
thingsOtherThan x 
    | isBlue x = [A,B,C,E]
    | isThin x = [A,C,D,E]
    | isCircle x = [A,B,D,E]
    | isSmall x = [A,B,C,D]
    | otherwise = [B,C,D,E]


properties :: [Predicate Thing]
properties = [isBlue,isOrange,isThick,isThin,isSquare,isCircle,isBig,isSmall]

properties2 :: [String]
properties2 = ["isBlue","isOrange","isThick","isThin","isSquare","isCircle","isBig","isSmall"]


propertiesOf :: Thing -> [String]
propertiesOf x = [ q | p <- properties , q <- properties2 , p x ]


isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing p x = undefined





