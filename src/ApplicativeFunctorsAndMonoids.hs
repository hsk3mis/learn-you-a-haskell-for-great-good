import Data.Char
import Data.List

{- FUNCTOR
  Functor - defined commonly using a box analogy that you can map over - it takes a function "a -> b" and a box with a and it returns a box with b,
            but more correct is the analogy of computational context!!
            The function being mapped over a computation results is the same computation but the result of that computation is modified with the function!!

            We can also look at functors as things that output values in a context.
              For instance, Just 3 outputs the value 3 in the context that it might or not output any values at all.
              [1,2,3] outputs three values—1, 2, and 3, the context is that there may be multiple values or no values.
              The function (+3) will output a value, depending on which parameter it is given.
            If you think of functors as things that output values, you can think of mapping over functors as attaching a transformation to the output of the functor that changes the value.

            Maybe, Either a = The context might be that the computation can have a value or it might have failed
            List = there might be more values or not
            IO = like boxes that have little feet, that go out and fetch some value from outside world for us
            (->) r = (r ->) = function type "r -> a" can be rewritten as "(->) r a" - just a type constructor that takes two parameters ":k (->)"

  Lifting a function - If we can think of fmap not as a function that takes one function and a functor and returns a functor, but as a function that takes a function and returns a new function that's just like the old one, only it takes a functor as a parameter and returns a functor as the result.
            fmap - is a lifting operator because it transforms a function between simple types (a -> b) into a function between other types of those types (f a -> f b) (eg. Maybe a -> Maybe b, eg. Pair a a -> Pair b b)
              :t fmap (*2)            :: (Num a, Functor f) => f a -> f a
              :t fmap (replicate 3)   :: (Functor f) => f a -> f [a]          //will take a functor over any type and return a functor over a list of elements of that type.
                                                                              //fmap (replicate 3) [1,2,3]        == [[1,1,1],[2,2,2],[3,3,3]]
                                                                              //fmap (replicate 3) (Right "blah") == Right ["blah","blah","blah"]
                                                                              //fmap (replicate 3) (Left "foo")   == Left "foo"
  Functor over numbers - think about it as a functor that has numbers in it

  Functor Laws:
    1. If we map the "id" function over a functor, the functor that we get back should be the same as the original functor.
        fmap id = id
    2. Composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one
        fmap (f . g) = fmap f . fmap g              <=>           for any functor F: fmap (f . g) F = fmap f (fmap g F)

    If something do mapping and additionally it does something else, then it might not obey the functor laws (eg. Just with counter increased when mapped)
    If something obey the laws of Functor, then we know it will do only mapping, and nothing more!!
    This leads to code that is more abstract and extensible, because we can use laws to reason about behaviors that any functor should have and make functions that operate reliably on any functor.
-}



{- Functor definition for type constructor f -}
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

{- Example implementation of Functor for IO monad -}
instance Functor' IO where
    fmap' f action = do
        result <- action    -- take the value of the IO
        return (f result)   -- create new IO with that values mapped over

{- Using standard approach vs fmap'ing over IO monad -}
reverseInput1 :: IO ()
reverseInput1 = do
        line <- getLine
        let line' = reverse line
        putStrLn $ "You said " ++ line' ++ " backwards!"

reverseInput2 :: IO ()
reverseInput2 = do
        line <- fmap reverse getLine    -- line will reflect the result that is already reversed
        putStrLn $ "You said " ++ line ++ " backwards!"

addMarkToLine :: IO String
addMarkToLine = fmap (++"!") getLine

{- "abcdef" ==>> "F-E-D-C-B-A" -}
intersparsedReverseUppercasedLine :: IO String
intersparsedReverseUppercasedLine = fmap (intersperse '-' . reverse . map toUpper) getLine

{- Function type is also a Functor with fixed arguments type -}
{- The box analogy still holds if you thing of the function (+100) as a box that contains its eventual result. -}
{- ?? Is it right to think of a function as a box for all of the mappings between argument set to value set ?? -}
instance Functor' ((->) r) where
--  fmap' f g = (\x -> f (g x))   -- fmap' :: (a -> b) -> (r -> a) -> (r -> b)
--  fmap' f g = f . g
    fmap' = (.)

{----------------------------------------------------------------------------------------------------------}

{- APPLICATIVE FUNCTOR - upgraded functors
  Laws:
    1. pure f <*> x = fmap f x
    2. pure id <*> v = v
    3. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    4. pure f <*> pure x = pure (f x)
    5. u <*> pure y = pure ($ y) <*> u                      -- ($ y) is infix operator section (partial application of second parameter)

  They allow us to COMBINE different computations (I/O computations, non-deterministic computations, computations that might have failed) by using applicative style!!
  Just by using <$> and <*> we can uniformly operate on any number of applicative functors and take advantage of the semantics of each one (different for Maybe, different for List, different for ZipList)
-}

let justMultiplyBy3 = fmap (*) (Just 3)         ==> Just (* 3)  :: Maybe (Int -> Int) -- functor with function inside - how to use it??
let just18 = fmap (\f -> f  6) justMultiplyBy3  ==> Just 18     :: Maybe Int          -- we can map function that takes the function inside and apply it, getting the result

-- What if we have Just (3 *) and Just 5 and we want take out function from one functor and map it over another functor??
-- Normal functors only support mapping NORMAL function over existing functors => we could pattern match agains Just and then apply, but it's not very general and abstract

class (Functor f) => Applicative' f where         -- if we want to make a type constructor part of the Applicative typeclass, it has to be in Functor first
    pure' :: a -> f a                             -- It takes a value of any type and return an applicative functor with that value inside it (in the box analogy).
                                                  -- We take a value and we wrap it in an applicative functor that has that value as the result inside it.
                                                  -- It takes a value and puts it in some sort of default (or pure) context — a minimal context that still yields that value.
    (<*>') :: f (a -> b) -> f a -> f b            -- upgraded fmap - takes a functor that has a function in it and another functor and then maps the function in the first functor over the second functor
                                                  -- it extract function from the first functor => actually extract mean run and extract => actually mean sequence

instance Functor' Maybe where
    fmap' _ Nothing = Nothing
    fmap' f (Just x)  = Just (f x)

instance Applicative' Maybe where                 -- f that plays the role of the applicative functor should take one concrete type as a parameter,
    pure' = Just
    Nothing <*>' _ = Nothing
    (Just f) <*>' something = fmap f something

let just12 = Just (+3) <*> Just 9
let just12inApplicativeContext = pure (+3) <*> Just 9   -- use pure if you're dealing with Maybe in the Applicative context, otherwise use Just
let nothing1 = Just (++"hahah") <*> Nothing
let nothing2 = Nothing <*> Just "woot"                  -- we try to extract a function from a Nothing and then map it over something, which of course results in a Nothing.

{-
 Applicative functors, allow you to operate on several functors with a single function.
 Applicative functors and the applicative style of doing "pure f <*> x <*> y <*>" allow us to apply functions to parameters in Functor contexts!!
 The function can take as many parameters as we want, because it's always partially applied step by step between occurences of <*>.

 Applicative Laws:
  1. pure f <*> x     ==    fmap f x                   you can write "pure f <*> x <*> y" as "fmap f x <*> y" or "f <$> x <*> y"
                                                       <$> = is just fmap as an infix operator

-}
let just8 = pure (+) <*> Just 3 <*> Just 5
let nothing3 = pure (+) <*> Just 3 <*> Nothing
let nothing4 = pure (+) <*> Nothing <*> Just 5

let aPure = pure 1
let aMaybe = a :: Maybe Int         -- Just 1
let aEither = a :: Either Int Int   -- Right 1
let aList = a :: [Int]              -- [1]

(<$>') :: (Functor f) => (a -> b) -> f a -> f b       -- f is a type variable with a class constraint (any type constructor that replaces f should be in the Functor typeclass)
f <$>' x = fmap f x                                   -- f in the function body is a function we map over x (not connected to the f type variable)

let justJohnDo = (++) <$> Just "John" <*> Just "Do"   -- Just "JohnDo"
let nothing5   = (++) <$> Just "John" <*> Nothing     -- Nothing
let johnDo     = (++) "John" "Do"                     -- "JohnDo"



{---------------------------------------------------------------}
instance Functor' [] where
    fmap' == map

instance Applicative' [] where
    pure' x = [x]                                 -- [] empty list would be the minimal context, but it would not yields the value given
    fs <*>' xs = [f x | f <- fs, x <- xs]         -- :: [a -> b] -> [a] -> [b]  (every possible combination of a list of functions with a list values). Left-associative.

let listApplicative   = pure "Hey" :: [String]              -- ["Hey"]
let maybeApplicative  = pure "Hey" :: Maybe String          -- Just "Hey"

let everyCombinationOfFunctionsAndValues = [(*0),(+100),(^2)] <*> [1,2,3]     -- [0,0,0, 101,102,103, 1,4,9]
let appliedBetween2Lists = [(+),(*)] <*> [1,2] <*> [3,4]                      -- [(1+),(2+),(1*),(2*)] <*> [3,4]  =>  [4,5,5,6, 3,4,6,8]
let stringFun = (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]                 -- ["ha?","ha!","ha.", "heh?","heh!","heh.", "hmm?","hmm!","hmm."]
-- Normal function (++) is used between two applicative functors of strings just by inserting the applicative operators!!
let alternativeToListComprehensions = (*) <$> [2,5,10] <*> [8,10,11]          -- <=>  [ x*y | x <- [2,5,10], y <- [8,10,11]]


{- LISTS = viewed as NON-DETERMINISTIC COMPUTATIONS
  A value like 100 or "what" can be viewed as a deterministic computation that has only one result,
  whereas a list like [1,2,3] can be viewed as a computation that can't decide on which result it wants to have, so it presents us with all of the possible results.
  (+) <$> [1,2,3] <*> [4,5,6], you can think of it as adding together two non-deterministic computations with +, only to produce another non-deterministic computation that's even less sure about its result.
  That's why the result is the non-terministinc computation with all the possible ways to combine previous non-deterministic computations
-}


{---------------------------------------------------------------}
instance Functor' IO where
    fmap' f action = do
        result <- action
        return (f result)

instance Applicative' IO where
    pure' = return                        -- yields some value as a result, and nothing more (no printing or reading)
    a <*>' b = do                         -- :: IO (a -> b) -> IO a -> IO b
        f <- a
        x <- b                            -- those actions are sequenced/glued together into one
        return (f x)

read2LinesAndConcatenate :: IO String
let read2LinesAndConcatenate = (++) <$> getLine <*> getLine
-- Common patter of binding IO action to name, then applying some function, then returning the result

{---------------------------------------------------------------}
instance Functor' ((->) r) where
--  fmap' f g = (\x -> f (g x))
    fmap' = (.)

instance Applicative' ((->) r) where      -- rarely used in the applicative style ;)
    pure' x = (\_ -> x)                   -- :: a -> (r -> a) -- function that takes a value and creates (minimal default context) a function that ignores its parameter and always returns that value
    f <*>' g = \x -> f x (g x)            -- (f x) return what's inside applicative f (= a function) and then apply the first parameter as the result of (g x)
                                          -- You can think of functions as boxes that contain their eventual results,
                                          -- so doing k <$> f <*> g creates a function that will call k with the eventual results from f and g.

let always3 = (pure 3) "blah"             -- <=> pure 3 "blah"
let functionAlwaysReturn8_1 = pure (+) <*> pure 3 <*> pure 5 $ "blah"
let functionAlwaysReturn8_2 = (+) <$> pure 3 <*> pure 5 $ 123
let result508 = (+) <$> (+3) <*> (*100) $ 5
                                          {-
                                            ((+) <$> (+3) <*> (*100) ) 5
                                            ((+) <$> (+3)) 5 ((*100) 5)
                                            (pure (+) <*> (+3)) 5 ((*100) 5)
                                            ((pure (+)) 5 ((+3) 5) ((*100) 5)
                                            ((+)            8)      500
                                            (+8)                   500
                                            508
                                          -}
let arrayFrom5 = (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5   -- [8.0,10.0,2.5]

{---------------------------------------------------------------}
{- There are MORE ways for lists to be Applicatives (but one type can't have two instances of same typeclass)
   ZipList -> Lists, but with an Applicative functor based on zipping. It contains one field - a list
   :module +Control.Applicative
 -}
data ZipList' a = ZipList' { list :: [a] } deriving (Show)

instance Applicative' ZipList' where
    pure' x = ZipList' (repeat x)                                             -- has to produce value on any position
                                                                              -- with definition "ZipList' x" it would break the law: "pure f <*> xs should equal fmap f xs"
                                                                              -- eg. of breaking the law: pure (*2) <*> ZipList [1,5,10] would result in ZipList [2]
    ZipList' fs <*>' ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)       -- result is as long as the shortest list
                                                                              -- zipWith, zipWith3, zipWith4, .., zipWith7 (zips X lists together)
                                                                              -- !!! By using ZipLists in an Applicative style we don't need specialized zipWithX functions !!! We can zip any number of lists

let pairOf4s = ZipList [(+3),(*2)] <*> ZipList [1,2]                      -- [4, 4]
let zipped1 = (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]           -- [101,102,103]
let zipped2 = (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  -- [('d','c','r'),('o','a','a'),('g','t','t')]

{---------------------------------------------------------------}
{- liftA2 - helper function that hides the applicative style => it just applies a function between two applicatives
          - it shows that we can apply functions between multiple functors => with normal functors you can only map function over one functor
          - looking as a function type as "(a -> b -> c) -> (f a -> f b -> f c)" we can see that it takes normal binary function, and promotes it to a function that operates on two functors

  :module +Control.Applicative
  liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
  liftA2' f a b = f <$> a <*> b
 -}

let just5 = liftA2 (+) (Just 1) (Just 4)

let concatElementToList1 = liftA2 (:) (Just 3) (Just [4])               -- Just [3,4]
let concatElementToList2 = (:) <$> Just 3 <*> Just [4]                  -- Just [3,4]

-- a function that takes a list of applicatives and returns an applicative that has a list as its result value
sequenceA :: (Applicative f) => [f a] -> f [a]
--sequenceA [] = pure []
--sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
sequenceA = foldr (liftA2 (:)) (pure [])

let just12 = sequenceA [Just 1, Just 2]
      {-
        (:) <$> Just 1 <*> sequenceA [Just 2]
        (:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])
        (:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])
        (:) <$> Just 1 <*> Just [2]
        Just [1,2]
      -}
let just321 = sequenceA [Just 3, Just 2, Just 1]              -- Just [3,2,1]
let nothing10 = sequenceA [Just 3, Nothing, Just 1]           -- Nothing
let list710 = sequenceA [(+2), (*2)] $ 5                      -- [7, 10]
      {-
        (+) <$> (+3) <*> (*2) ==> will create a function that takes a parameter, feeds it to both (+3) and (*2) and then calls + with those two results
        sequenceA [(+3),(*2)] ==> similar, makes a function that takes a parameter and feeds it to all of the functions in the list
      -}
let functionToList = sequenceA [(+3),(+2),(+1)] 3             -- (a -> [a]) => takes a list of function and returns a function that returns a list
let list654 = functionToList 3                                -- [6,5,4] =>3 is applied to that function
let pairs = sequenceA [[1,2,3],[4,5,6]]                       -- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
let listEmpty = sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]        -- []

-- Eg. list of Maybe becomes Nothing if any element is Nothing
-- Eg. list of function becomes a function that returns a list

-- Eg. when we have a list of functions and we want to feed the same input to all of them and then view the list of results

-- Eg. usage: check if a value satisfies ALL predicates in the list:
-- without applicatives: and $ map (\f -> f 7) [(>4),(<10),odd]
and $ sequenceA [(>4),(<10),odd] SOME_VALUE

-- For IO sequenceA is the same as sequence function
sequence [getLine, getLine, getLine]                          -- ["input1", "input2", "input3"]


sequenceA [[1,2],[3,4]]           -- [[1,3],[1,4],[2,3],[2,4]]
    {-
      sequenceA [[1,2],[3,4]]
      (:) <$> [1,2] <*> sequenceA [[3,4]]
      (:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])
      (:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])
                         (:) <$> [3,4] <*> [[]]
                         [3:[], 4:[]]
                         [[3],[4]]
      (:) <$> [1,2] <*> [[3],[4]]
      [1:[3], 1:[4], 2:[3], 2:[4]]
      [[1,3],[1,4],[2,3],[2,4]
    -}

-- Interpretation of sequence with lists
(+) <$> [1,2] <*> [4,5,6]     -- results in a non-deterministic computation x + y where x takes on every value from [1,2] and y takes on every value from [4,5,6]



{---------------------------------------------------------------}
{- newtype - new type out of existing type (but with ONLY a single field
           - for wrapping, doing something and then unwrapping => eg. ZipList
           - compiler optimizes this and you don't pay the price of wrapping/unwrapping and keep the type internally the same!!
           - Haskell knows that you're just using it to wrap an existing type into a new type (hence the name), because you want it to be the same internally but have a different type
             Haskell can internally represent the values of the new type in the same way as the original values
           - newtype don't get any functions or typeclasses from source type
 -}
newtype ZipList a = ZipList { getZipList :: [a] }

getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]     -- [2,200,15]

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

-- Functor for ((,) a):
instance Functor ((,) a) where
    fmap f (a,b) = (a, f b)

-- How to make some types instances of typeclasses we want? eg. (a, b) where we want fmap to work on a type ???? or Either where we want fmap to work on Left ????
newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)   -- fmap :: (a -> b) -> Pair c a -> Pair c b

getPair $ fmap (*100) (Pair (2,3))                      -- (200,3)
getPair $ fmap reverse (Pair ("london calling", 3))     -- ("gnillac nodnol",3)

-- Laziness !! "newtype" is more lazy than "data"
undefined                                   -- Exception
head [3,4,5,undefined,2,undefined]          -- 3 (no Exception)

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"              -- don't use argument => just return "hello"

data CoolBool = CoolBool { getCoolBool :: Bool }
helloMe undefined                           -- Exception, because "data" can have multiple value constructors
                                            -- So in order to see if the value given to our function conforms to the (CoolBool _) pattern, Haskell has to evaluate the value just enough to see which value constructor was used when we made the value

newtype CoolBool = CoolBool { getCoolBool :: Bool }
helloMe undefined                           -- "hello" (no Exception)
                                            -- Haskell doesn't have to evaluate the value passed to the function to make sure that it conforms to the (CoolBool _) pattern because newtype types can only have one possible value constructor and one field!

{- type vs newtype vs data
  type    = making type synonyms (want your type signatures to look cleaner and be more descriptive)
  newtype = for taking existing types and wrapping them in new type (easier to make them instances of certain type classes)
            can only have one constructor and one field
  data    = for making your own data types

  Pattern matching on data is like taking something out of a box
  Pattern matching on newtype is more about making a direct conversion from one type to another.

  Typeclasses = says which behaviors type supports (what it can act like)
-}


---------------------------------------------------------------}
{- Półgrupa = zbiór z określonym na nim działaniem dwuargumentowym wenętrznym (wynik jest też z tego zbioru), łącznym (a + b) + c = a + (b + c)
   Monoid   = półgrupa której działanie ma element neutralny
   Grupa    = monoid w którym każdy element ma element odwrotny

   Struktura matematyczna = Pojęcie fundamentalne, definiowane różnie w zależności od teorii i kontekstu.
                            Najczęściej mówimy o Strukturze na danym zbiorze X, który nazywamy nośnikiem lub podkładem tej struktury.

   associativity = łączność
 -}

class Monoid m where                    -- "m" = only concrete types can be made instances of Monoid
    mempty :: m                         -- polymorphic constant = represents identity value for a particular monoid
    mappend :: m -> m -> m              -- takes two monoid values and returns a third (unfortunate name, because it implies that we're appending two things in some way) (works for "++" but doesn't work for "*")
    mconcat :: [m] -> m                 -- takes a list of monoid values and reduces them to a single value by doing mappend between the list's elements
    mconcat = foldr mappend mempty      -- the reason mconcat is there at all is because for some instances, there might be a more efficient way to implement mconcat, but for most instances the default implementation is just fine

{- Monoid Laws:
  1. mempty `mappend` x = x                                       -- mempty has to act as identity with respect to mappend
  2. x `mappend` mempty = x
  3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)    -- associativity (the order in which we use mappend to reduce several monoid values into one doesn't matter)
-}

-- Lists are Monoids
instance Monoid [a] where               -- [a] because Monoid requires concrete type for an instance
    mempty = []
    mappend = (++)

-- Numbers can act like monoid in multiple ways
newtype Product a =  Product { getProduct :: a } deriving (Eq, Ord, Read, Show, Bounded)
instance Num a => Monoid (Product a) where              -- Product a is an instance of Monoid for all a that are also an instance of Num
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

let value27 = getProduct $ Product 3 `mappend` Product 9

newtype Sum a =  Sum { getSum :: a } deriving (Eq, Ord, Read, Show, Bounded)
instance Num a => Monoid (Sum a) where              -- Product a is an instance of Monoid for all a that are also an instance of Num
    mempty = Sum 1
    Sum x `mappend` Sum y = Sum (x + y)

-- Booleans can act like monoid in multiple ways
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)

-- Ordering can act like monoid
-- The Ordering monoid is very cool because it allows us to easily compare things by many different criteria
-- and put those criteria in an order themselves, ranging from the most important to the least.
instance Monoid Ordering where                -- this definition resembles the way we alphabetically compare words
    mempty = EQ                               -- "x `mappend` y" doesn't equal "y `mappend` x"
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT

-- Eg. compare strings by length, and if they are equal compare them normally
lengthCompareWithoutMonoid :: String -> String -> Ordering
lengthCompareWithoutMonoid x y = let a = length x `compare` length y
                                     b = x `compare` y
                                 in  if a == EQ then b else a

lengthCompareWithMonoid :: String -> String -> Ordering
lengthCompareWithMonoid x y = (length x `compare` length y) `mappend` (x `compare` y)

let lt = lengthCompare "zen" "ants"
let gt = lengthCompare "zen" "ant"

lengthCompareWithVowels :: String -> String -> Ordering
lengthCompareWithVowels x y = (length x `compare` length y) `mappend`
                              (vowels x `compare` vowels y) `mappend`
                              (x `compare` y)
                            where vowels = length . filter (`elem` "aeiou")

-- Maybe can act like monoid in multiple ways

-- treat Maybe as Monoid only if its type parameter a is a monoid as well
-- in use when you're dealing with monoids as results of computations that may have failed
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

let justAndy = Nothing `mappend` Just "andy"
let justLt = Just LT `mappend` Nothing
let justSum7 = Just (Sum 3) `mappend` Just (Sum 4)

-- Discard the second value and keep the first one (the only place where we needed the type parameter to act like a monoid was with two justs - this type discards this problem by ignoring second just
-- First is useful when we have a bunch of Maybe values and we just want to know if any of them is a Just
newtype First a = First { getFirst :: Maybe a } deriving (Eq, Ord, Read, Show)
instance Monoid (First a) where
    mempty = First Nothing            -- Nothing wrapped with First newtype constructor
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

let justA = getFirst $ First (Just 'A') `mappend` First (Just 'B')
let just9 = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]

-- Last a - works like First a, only the last non-Nothing value is kept when mappending
let just10 = getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]


{- Monoids are useful to FOLD data structures = Helps define folds over various data structures
   Foldable => implement "foldr" directly or more easily implement helper "foldMap"
   foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-}

let a6 = foldr (*) 1 [1,2,3]
let just11 = foldl (+) 2 (Just 9) -- acts like a list with one element if it's a Just value and as an empty list if it's Nothing

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Foldable Tree where
    foldMap f Empty = mempty                              -- if our tree is empty, the monoid value it becomes is mempty.
    foldMap f (Node x l r) = foldMap f l `mappend`        -- function f was provided to us
                             f x         `mappend`
                             foldMap f r

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Leaf x) = f x z
    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

-- foldMap isn't only useful for making new instances of Foldable; it comes in handy for reducing our structure to a single monoid value.
-- For instance, if we want to know if any number in our tree is equal to 3, we can do this:
let aTrue = getAny $ foldMap (\x -> Any $ x == 3) testTree

let aFalse = getAny $ F.foldMap (\x -> Any $ x > 15) testTree

-- turn our tree into a list by doing a foldMap that will automatically use mappend to concatenate monoids provided by the function
let treeAsList = foldMap (\x -> [x]) testTree



testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

test1 = foldl (+) 0 testTree
test2 = foldl (\a x -> a ++ (show x)) [] testTree
test3 = foldMap (\x -> [x]) testTree
test4 = foldl (\a x -> a ++ [x]) [] testTree


newtype InfixTree a = InfixTree { getTreeInfix :: Tree a } deriving (Eq, Read, Show)
instance Foldable InfixTree where
    foldMap f (InfixTree Empty) = mempty
    foldMap f (InfixTree (Node x l r)) = foldMap f (InfixTree l) `mappend` f x `mappend` foldMap f (InfixTree r)

newtype PrefixTree a = PrefixTree { getTreePrefix :: Tree a } deriving (Eq, Read, Show)
instance Foldable PrefixTree where
    foldMap f (PrefixTree Empty) = mempty
    foldMap f (PrefixTree (Node x l r)) = f x `mappend` foldMap f (PrefixTree l) `mappend` foldMap f (PrefixTree r)

newtype PostfixTree a = PostfixTree { getTreePostfix :: Tree a } deriving (Eq, Read, Show)
instance Foldable PostfixTree where
    foldMap f (PostfixTree Empty) = mempty
    foldMap f (PostfixTree (Node x l r)) = foldMap f (PostfixTree l) `mappend` foldMap f (PostfixTree r) `mappend` f x
