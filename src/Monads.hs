{- MONADS
  Functor = for values that can be mapped over
    When we have a function of type "a -> b" and some data type "f a", how do we map that function over the data type to end up with "f b"
    fmap :: (Functor f) => (a -> b) -> f a -> f b

  Applicative = view values of certain data types as values with context and use normal functions on those values while preserving the meaning of this context
    Allow us to take a normal function and make it operate on values with contexts.
    A value inside some Context = a "fancy" value
    What if that function "a -> b" is already wrapped inside a functor value?
    (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
    pure :: wraps value inside the context

    Maybe a = computation that might have failed
    [a]     = non-deterministic computation
    IO a    = value that have side-effect

  Monads = upgraded Applicatives
    If you have a value with a context, "m a", how do you apply to it a function that takes a normal a and returns a value with a context "a -> m b"?
    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b        //bind function



-}

class (Applicative m) => Monad'' m where
    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

instance Monad'' Maybe where
    (>>=) (Just a) f = f a
    (>>=) Nothing _ = Nothing

class Monad' m where                        -- Every Monad IS an Applicative (but there were no Applicatives when Haskell and monads were fist defined)
    return :: a -> m a                      -- same as "pure" (but with a different name)
                                            -- It takes a value and puts it in a minimal default context that still holds that value.
                                            -- It doesn't end function execution or anything, it just takes a normal value and puts it in a context.
    (>>=) :: m a -> (a -> m b) -> m b       -- bind = it's like function application

    (>>) :: m a -> m b -> m b               -- default implementation is usually ok.
    x >> y = x >>= \_ -> y                  -- if forgets what was before and returns a new monad

    fail :: String -> m a                   -- used by Haskell to enable failure in special syntactic constructs for monads
    fail msg = error msg


instance Monad Maybe' where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f  = f x
    fail _ = Nothing

let justWhat = return "WHAT" :: Maybe String    -- Just "WHAT"
let just90 = Just 9 >>= \x -> return (x*10)     -- Just 90
                                                -- Like extracting a value from Maybe without pattern-matching!! And still preserve the context (if Nothing, then it's Nothing)


{- Walk a line -}
type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n,right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right + n)

let birdIncoming = landLeft 2 (0,0)         -- (2,0)
let birdFlyingAway = landRight (-1) (1,2)   -- (1,1)

let threeOne = landLeft 2 (landRight 1 (landLeft 1 (0,0))) -- (3,1)

-- We want the pole to go first and then the number of birds
x -: f = f x        -- 100 -: (*3)  ;;  (0,0) -: landLeft 2

let threeOne' = (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2

-- Add the context of failure to the landing functions
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- Now we need somthing that takes "Maybe Pole" and a function "Pole -> Maybe Pole" and return a new "Maybe Pole" => it's (>>=)
let just21 = landRight 1 (0,0) >>= landLeft 2       -- Just (2,1)
let nothing = Nothing >>= landLeft 2                -- Nothing

let just24 = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2      -- Just (2,4)

-- UWAGA: Kolejność od lewej do prawej jest WAŻNA !!!! Nie jest tak jak w Monoidzie, że operacja jest łączna !!!!!!
let nothing = return (0,0) >>= landLeft 3 >>= landLeft 1 >>= landLeft (-1)
-- Jakbyśmy najpierw wykonali te operacje po prawej, to byśmy uzyskali Just (3,0) a poprawnym wynikiem jest Nothing !!!!!
-- Each step relies on the previous one's result

-- always causes the slip on the bananna
banana :: Pole -> Maybe Pole
banana _ = Nothing

let nothing = return (0,0) >>= landLeft 1 >>= banana >>= landRight 1        -- Nothing
let nothing = return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1        -- Nothing

-- >> ignores the previous value preserving the context!!!! If there was something, then replace it with the new value ignoring previous!! If there was Nothing then preserve the context!! It's still Nothing!!
let nothing = Nothing >> Just 3
let just4 = Just3 >> Just 4

{-
  We took some functions that we had and saw that they would work better if the values that they returned supported failure.
  By turning those values into Maybe values and replacing normal function application with >>=, we got a mechanism for handling failure pretty much for free,
  because >>= is supposed to preserve the context of the value to which it applies functions.
  In this case, the context was that our values were values with failure and so when we applied functions to such values, the possibility of failure was always taken into account.
-}

{- DO NOTATION = special syntax for gluing together monadic values in sequence -}
-- Jak zachować pośrednie wyniki, poza samym kontekstem sukcesu/porażki
let just3! = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))   -- Just "3!"
-- Similar to let x = 3; y = "!" in show x ++ y but with the failure context

let just3! = foo = do
    x <- Just 3         --if there is Nothing here, then the whole result is Nothing
    y <- Just "!"       --if there is Nothing here, then the whole result is Nothing
    Just (show x ++ y)

{- WHEN TO USE DO NOTATION AND WHEN TO USE >>= ?
  In this case it's probably more readable to use the >>= notation, because every step depends just on the step directly before it !!
-}
routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second


{- FAILURES AND PATTERN MATCHING IN DO NOTATION -}
















