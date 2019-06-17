import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Data.List
import Data.Function (on)
import Data.Char

import Control.Monad.Writer

main = defaultMain unitTestsMonads

unitTestsMonads = testGroup "Unit tests for Monads"
  [
    testCase "writer monad" $ assertEqual [] (15, ["Got number: 3","Got number: 5","Got number: 10","Gonna multiply first two","Got number: 12"]) $ runWriter multWithLog
  ]

{- Monads in general:
    - Monads can be used to take values with contexts and apply them to functions and by using ">>=" or "do" notation allows us to focus on the values themselves while the context gets handled for us.
    - Maybe monad = it adds a context of possible failure to values
    - List monad = introduces non-determinism
    - other monads = makes our program clearer by letting us treat all sort of values as monadic ones
-}




{- Writer monad - for values that have another value attached that acts as a sort of log value
  	- Writer allows us to do computations while making sure that all the log values are combined into one log value that then gets attached to the result.
  	- The writer monad lets us emit a lazy stream of values from within a monadic context.
  	- Example use-cases:
  		-- logs
  		-- summing of price of products added

  	- Value with attached Monoid acts like a Monadic value !!
 -}

--newtype Writer w a = Writer { runWriter :: (a, w) }
--instance (Monoid w) => Monad (Writer w) where
--    return x = Writer (x, mempty)
--    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- The Writer instance doesn't feature an implementation for fail, so if a pattern match fails in do notation, error is called.
-- tell - function that is part of MonadWriter type = creates a Writer with dummy value '()' with given monoid

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do                                                        --
  a <- logNumber 3                                                      -- logNumber 3 >>=
  b <- logNumber 5                                                      --  (\x -> logNumber 5 >>=
  c <- logNumber 10   -- adds to log even if we don't use c value       --    (\y -> logNumber 10 >>=
  tell ["Gonna multiply first two"]                                     --
  logNumber 12
  return (a * b)                                                        --      (\z -> return (x * y))))


