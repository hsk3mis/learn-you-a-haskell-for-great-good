module DataTypes
( Point(..)           -- .. exports all value constructors for a given type
, Shape(..)           -- Shape() would export type Shape without any value constructors
                      -- so client can create those type ONLY using provided functions (eg. baseCircle, baseRect)
                      -- eg. Map module do this and you need Map.fromList functions and other
                      -- this gives more abstract data types, but users can't pattern match against those value constructors
, surface
, nudge
, baseCircle
, baseRect
) where

import qualified Data.Map as Map

{-----------------------------------------------------------------------}
{- Data Types (Algebraic data types) -}
{- data <data_type_name> = <value_constructor> <types_of_values_contained_in_this_value_constructor> | <another_value_constructor> -}
{- value_constructor = is just a function that return a value of the data_type -}
data Point = Point Float Float deriving (Show)
data Shape = Empty | Circle Point Float | Rectangle Point Point deriving (Show)

{- INFO ABOUT A TYPE -}
-- :info Point

someCircle = Circle (Point 1.0 2.0) 3.0

surface :: Shape -> Float           --Circle is not a type => Shape is a type
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle p1@(Point x1 y1) p2@(Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

{- Shapes in (0.0, 0.0) point -}
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)


{-----------------------------------------------------------------------}
{- Data types with Record syntax = additionally create functions that lookup fields in the data type -}
data Car = Car {company :: String,              -- company :: Car -> String (generated function)
                model :: String,
                year :: Int } deriving (Eq, Show, Read)

someCar = Car {company="Ford", model="Mustang", year=1967}
someCarModel = model someCar

{- Pattern matching in lambdas -}
car2ModelYearTuple = (\(Car{model = model, year = y}) -> (y, model))

{-----------------------------------------------------------------------}
{- Type Parameters = used when the type that's contained isn't really that important (list of anything is still a list) -}
{- Type constructors can take types as parameters to produce new types -}
{- data <type_constructor> type = definition -}
data Maybe' a = Nothing' | Just' a
-- data (Ord k) => Map' k v = ... -- don't do this => this makes you write more Typeclass constraints in functions that don't need them !!
                                  -- add type constrains to functions that require those constraints
                                  -- disabled by default in Haskell 98 (Haskell creators think it was a bad idea)
                                  -- constraints only refers to the constructors = functions still need those constraints
-- data C a  =>  T a = Cons a

{- Maybe is not a type per se - it's a type constructor (to become a real type, it has to have all type parameters filled up -}
m1 = Nothing :: Maybe a -- it's inferred polymorphic type of Nothing
n1 = Nothing :: Maybe a
testNothingsAreEqual = (m == n) == True

m2 = Nothing :: Maybe Int
n2 = Nothing :: Maybe a
test2NothingsAreEqual = (m1 == n1) == True -- n1 has type "Maybe a" so it adopts to "Maybe Int"

{- But Nothing of different types can't be compared -}
m1 = Nothing :: Maybe Int
n1 = Nothing :: Maybe String
{- m == n   don't compile-}

{- Derived instances = eg. deriving Eq instance for a type -}
{- First checks if value constructors match, then checks if all data contained inside matches by testing each field with == -}
ford = read "Car {company =\"Ford\", model=\"Mustang\", year=1967" :: Car

data Bool' = False' | True' deriving (Ord)
testDerivedOrdBasedOnOrder = (True `compare` False) == GT

testDerivedOrdBasedOnOrder2 = (Nothing < Just -1) == True
testDerivedOrdBasedOnOrder3 = (Just 0 < Just 1) == True
--testDontCompile: Just (*3) < Just (*2)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

testMinBound = (minBound :: Day) == Monday
testMaxBound = (maxBound :: Day) == Sunday
testSuccessor = (succ Monday) == Tuesday
testEnum = [Thursday .. Sunday] == [Thursday,Friday,Saturday,Sunday]


{- TYPE SYNONYMS -}
{- Type synonyms (and types generally) can only be used in the type portion of Haskell => when defining new types or after :: -}
{- :: => is in type declarations or in type annotations -}
type String' = [Char]
type AssocList k v = [(k,v)] -- parametrized

type Company = String
type Model = String
type Year = Int
data Car = Car {company :: Company, model :: Model, year :: Year }

tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

{- Type constructor build from partially apply another type constructor -}
type IntMap = Map.Map Int


{- EITHER => Left usually used for Errors | Right for proper answer -}
{- Nothing could be used, but it only tells us that something failed but we don't know why -}
data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

{- Search for a locker => if taken or doesn't exist then we get the explanation, otherwise we get the code for the locker -}
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing            -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (Taken, code) -> Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
    Just (Free, code)  -> Right code

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

testFree = lockerLookup 101 lockers
testTaken = lockerLookup 100 lockers
testDontExist = lockerLookup 102 lockers


{- Infixity of functions -}
{-
  infixl 7 *
  infixl 6 +

  Both * and + are left-associative: 1 + 2 + 3 == (1 + 2) + 3
  But * binds tighter than +: 1 + 2 * 3 == 1 + (2 * 3)
-}
infixr 6 ***
a *** b = a * b

infixl 6 +++
(+++) a b = a + b

infixr 5 :-:    -- make function (constructor) infix (5 = how tightly the operator binds) (right or left associative)
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

{- Lists concatenation -}
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


{- Recursive data types -}

{- BINARY SEARCH TREE -}
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x tree@(Node a left right)
  | x == a  = tree
  | x < a   = Node a (treeInsert x left) right
  | x > a   = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

nums = [8,6,4,1,7,3,5]
numsTreeRightFolded = foldr treeInsert EmptyTree nums

{- Foldl applies arguments in a different way!! -}
treeInsert' :: (Ord a) => Tree a -> a -> Tree a
treeInsert' EmptyTree x = singleton x
treeInsert' tree@(Node a left right) x
  | x == a  = tree
  | x < a   = Node a (treeInsert x left) right
  | x > a   = Node a left (treeInsert x right)

numsTreeLeftFolded = foldl treeInsert' EmptyTree nums


{- Typeclasses = defines some behavior -}
{- Type is an instance of a typeclass - we mean that we can use functions that the typeclass defines with that type -}

class Eq' a where
    (==) :: a -> a -> Bool    -- specify type declarations of some functions
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)     -- implementation is not required!!
    x /= y = not (x == y)


data TrafficLight = Red | Yellow | Green  -- don't derive any class instance automatically

{- Makes the Type instance of the Typeclass = Class instance for TrafficLight implemented manually -}
instance Eq TrafficLight where
    Red == Red = True             -- implement (==) with pattern matching - minimal complete definition for the typeclass
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False                -- other combinations are not equal

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

{- Typeclass subclassing -}
{- It says that you need to make a type an instance of Eq, before you can make it an instance of Ord' -}
class (Eq a) => Ord' a where
    compare :: a -> a -> Ordering

{- Typeclasses with type constructors => Maybe is not a concrete type, but (Maybe a) is a concrete type!! -}
{- We say this: we want all types of the form Maybe m to be part of the Eq typeclass, but only those types where the m (so what's contained inside the Maybe) is also a part of Eq. -}
instance (Eq m) => Eq (Maybe m) where   -- class constraint on m, to allow Eq on Maybe
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-- WARNING: you CAN create Just from types without Eq, but then you can't use == on Just instances

{- DEFINITION OF TYPECLASS && LIST ALL INSTANCES OF A TYPECLASS -}
:info Num
{- Also works with Types, Type constructors, Functions -}

{- Example Yes-No typeclass => JavaScript like true/false semantics -}
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing  = False
    yesno (Just _) = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _         = True

{- This doesn't compile -}
--instance YesNo (Tree a) where
--    yesno EmptyTree = False
--    yesno _         = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True


yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult


{- FUNCTOR Typeclass = for things that can be mapped over (higher-order concept) -}
{- Math: functor is a mapping between categories that hold special rules -}
{- Types that can act like a boxes can be Functors -}
{-  -}
{- Functors MUST obey some laws, so that they may have some properties that we can depend on!!
   Functor Laws (example):
    - fmap over a type should return this type in the same order (eg. order of elements in the list)
    - id - applying identity should return same result
    - fmap must keep then properties of the type (eg. binary tree properties)
-}
class Functor f where -- f is a type constructor that takes 1 parameter
    fmap :: (a -> b) -> f a -> f b -- takes a functor applied with one type "a" and returns functor applied with other type "b"

{- How it works with lists? -}
-- type of map function
map :: (a -> b) -> [a] -> [b]

{- List is a Functor -}
-- takes [] (type constructor), not [a] (concrete type)
instance Functor [] where
    fmap == map

-- map and fmap are the same for lists
map (*2) [1..3] == fmap (*2) [1..3]

{- Maybe is a Functor -}
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x)  = Just (f x)

{- Tree can be a Functor -}
instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

{- Either partially applied is a Functor -}
{- (b -> c) -> (Either a) b -> (Either a) c -}
instance Functor (Either a) where
    fmap _ (Left x)  = Left x      -- Left value constructor just don't map it's value
    fmap f (Right x) = Right (f x) -- mapping only in the case of the Right value constructor

{- Map is a Functor -}
{- (v -> v') -> (Map k) v -> (Map k) v' -}
instance Functor (Map.Map k) where
    fmap f m = Map.map f m


{- How Types are applied to Type constructors? -}
{-
  Values = eg. 3, "abc", takeWhile (functions are also values)
  Types = little labels carried by values (so we can reason about the values)
  Concrete type = a type that doesn't take any type parameters (values can only have types that are concrete types)
  Kinds = little labels carried by Types = the type of the type (types and kinds are parallel things)
  :k Int => shows kinds of the type (* - means a concrete type)
      Int        :: *
      Maybe      :: * -> *
      Maybe Int  :: *
      Either     :: * -> * -> *
      Either Int :: * -> *

  Kinds kind of formalizes type parameters (just like type declarations formalizes function parameters).
  Function and Type Constructors are completely different things, but very similar.
-}

{-
  fmap :: (a -> b) -> f a -> f b
      => because a is used as the type of a value in a function, so it has to be a concrete type
      => so f needs to be of kind * -> * (produces concrete type from a concrete type)
-}


{- Some Type-Foo example: -}
class Tofu t where
    tofu :: j a -> t a j
{-
  What's this type?
    - "j a" needs to have a kind of * (because it's used as a function parameter)
    - so "j" needs to have a kind of * -> * (assuming "a" has kind of *)
    - t has to produce a concrete value too, and it takes 2 types as parameters
    - so t has to be a kind of * -> (* -> *) -> *
-}

data Frank a b = Frank { frankField :: b a } deriving (Show)
{-
  Fields in data types are to hold values, so they have to be a kind of *. And we assume * for a.
-}

:t Frank {frankField = Just "HAHA"}                   => Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
:t Frank {frankField = Node 'a' EmptyTree EmptyTree}  => Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
:t Frank {frankField = "YES"}                         => Frank {frankField = "YES"} :: Frank Char []

instance Tofu Frank where
    tofu x = Frank x
{-
  tofu takes "j a" as an argument (example Maybe Int) and returns a "t a j",
  so if we replace "t" with Frank, the result type would be Frank Int Maybe
-}
tofu (Just 'a') :: Frank Char Maybe                   => Frank {frankField = Just 'a'}
tofu ["HELLO"] :: Frank [Char] []                     => Frank {frankField = ["HELLO"]}


:k Car      :: * -> * -> *
:k Functor  :: (* -> *)    -> Constraint
:k Tofu     :: (* -> (* -> *) -> *) -> Constraint


data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry t k) where
    fmap f (Barry {yabba = y, dabba = d}) = Barry {yabba = f y, dabba = d}

b = Barry {yabba = 123 :: Integer, dabba = Just 'a'}
fmap (+5) b == Barry {yabba = 128 :: Integer, dabba = Just 'a'}














