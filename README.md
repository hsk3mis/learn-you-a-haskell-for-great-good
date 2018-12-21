# learn-you-a-haskell-for-greater-good

* Important commands
```bash
stack ghci (+ :load Main)
stack setup
stack build
stack exec learn-you-a-haskell-for-greater-good
```

* Run test
```bash
stack test :chapter1
stack test :chapter3
```

* GHCI Links
    * Debugging: https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/ghci-debugger.html

* Haskell Links
    * Learn You a Haskell for Great Good: http://learnyouahaskell.com
    * Kurs inny: http://www.cse.chalmers.se/edu/course/TDA555/schedule.html
    * Zadanka:
        http://www.cse.chalmers.se/edu/course/TDA555/ex-week2.html
        https://wiki.haskell.org/99_questions/1_to_10
    * Category Theory for Programmers:
        HTML: https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/
        PDF+codes: https://github.com/hmemcpy/milewski-ctfp-pdf/
        YouTube: https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_
    * Type constructors:
        https://wiki.haskell.org/Constructor
        https://wiki.haskell.org/Syntactic_sugar/Cons
        https://www.haskell.org/tutorial/goodies.html
        https://stackoverflow.com/questions/5597157/standard-definition-of-list

    * Hitchhikers guide to Haskell: https://wiki.haskell.org/Hitchhikers_guide_to_Haskell
    * like-Haskell on JVM: https://eta-lang.org/
    * Zadanka z cyklem w ciągu:
        * Periodic sequence vs Eventually Periodic sequence => https://en.wikipedia.org/wiki/Periodic_sequence
        * Pisano period => https://en.wikipedia.org/wiki/Pisano_period
        * Cycle detection in general => https://en.wikipedia.org/wiki/Cycle_detection
        * Obliczyć dla Pisano period i sprawdzić z tabelkami na Wiki
        * Sprawdzić dla dowolnego ciągu periodycznego jest dość łatwe, co z ciągiem eventually periodic?
        * Sprawdzić dla dowolnego ciągu czy ma okres?
    
    * Classes:
        * Hierarchy = https://www.haskell.org/onlinereport/basic.html
        * Num = http://zvon.org/other/haskell/Outputprelude/Num_c.html
        * Fractional = http://zvon.org/other/haskell/Outputprelude/Fractional_c.html
        * Enum = http://zvon.org/other/haskell/Outputprelude/Enum_c.html

* Common Conventions
    * xxxx' (apostrophe at the end of the function name) = denotes the strict (not lazy) version of the function or a slightly modified version 

* Data structures
    * List = homogenous (of the single type)

* Types
    * Integer = default, arbitrary precision integers
    * Int = machine word-sized integers (bounded)
    * Haskell DON'T do any automatic types conversions (no different execution for different ranges of the integer value!!)

    * Type variables = polymorphic functions (like generics)
    * Typeclass = sort of interface that defines some behavior.
        - a Type can be a part of Typeclass (a member of some class) - it means that it supports and implements the behavior the typeclass describes 
        - eg. (==) :: (Eq a) => a -> a -> Bool
            Everything before "=>" is a class constraint!
        - Eq: ==, /=
        - Ord (have an ordering): >, <, >=, <= (But they don't have to be enumerable, just comparable)
            123 `compare` 456 returns Ordering type with values (GT, LT, EQ)
            Require Eq.
        - Show: can be presented as String
            show - function that converts Show class to String 
        - Read: can be readable from String
            read - function that converts String into proper type
        - Enum: sequentially ordered types (can be enumerated). Are also Ord'ered and has additional properties (succ, pred = can be enumerated)
            Can be used in ranges!
            Have defined successor and predecessor (functions: succ, pred).
        - Bounded: have an upper and a lower bound
            minBound, maxBound - functions of type (Bounded a) => a
            Polymorphic constants:
                minBound :: Int => -2147483648
                maxBound :: Char => '\1114111'
                maxBound :: Bool => True
        - Num: numeric typeclass (can act like numbers)
            All numbers are just polymorphic constants (they can act like any type that's a member of the Num)
                type of 20 is (Num t) => t
                20 :: Double, 20 :: Int
            Require Show and Eq.
        - Integral: whole numbers
            fromIntegral: converts Integral to more general Num
        - Floating: floating point numbers
        

    * Type annotation (:: Type) = to explicitly give type to the expression
        - read "5" :: Integer

* Pattern Matching (deconstruction of elements) and Guards (to test if some properties are true/false = if/else on steroids with support for pattern matching):
    * matching on multiple elements require enclosing parentheses
        eg. head (x:_) = x

    * matching the pattern and the whole
        eg. capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

    * Guards
        - all guards must evaluate to Boolean
        - if all guards evaluate to False, then evaluation falls through to the next pattern

* Where bindings vs Let bindings:
    Where can be used in Guards, Let is more local.
    Where = just syntactic constructs; readable because function body is closer to the function name
    Let = expressions themselves


* Notes
    * Combining simple (correct) functions into more complex functions
    * $ operator => to avoid parentheses (anything appearing after it will take precedence over anything that comes bofore
        eg. putStrLn (show (1 + 2))   <=>   putStrLn $ show $ 1 + 2
    * . operator => to chain functions
        eg. (putStrLn . show) (1 + 2)


* Laziness
    * Evaluates expressions ONLY when needed and then remember those evaluations
        ** Map Example
            list = [1,2,3,4,5,6,7,8]
            list2 = map mapper list
            mapper x = x + 1    //:breakpoint
            
            - "head list2" only calls mapper 1once
            - another call to "head list2" doesn't call mapper
        ** List equality Example
            forall s p = [x | x <- [-bound..bound], s x, not (p x)] == []
            
            - checking if 2 lists are equal is also lazy! If it checks that first list has 1 item it stops generating the rest of the first list!
            - but this is NOT Lazy: forall s p = length [x | x <- [-bound..bound], s x, not (p x)] == 0
    
* Recursion + Higher order functions
    * Necessary to define what something is (not how it's created), so there's no while/for loops

    * replicate :: Int -> a -> [a]                                  //Replicates x multiple times
    * zipWith'  :: (a -> b -> c) -> [a] -> [b] -> [c]               //Zip with function transformation
    * flip      :: (a -> b -> c) -> b -> a -> c                     //flips the arguments of the function given
        flip f = \x y -> f y x
    * map       :: (a -> b) -> [a] -> [b]
    * filter    :: (a -> Bool) -> [a] -> [a]                        //NOT work on Infinite Lists (use takeWhile)
    * takeWhile :: (a -> Bool) -> [a] -> [a]                        //Takes elements as long as the predicate holds
    * foldl     :: Foldable t => (b -> a -> b) -> b -> t a -> b)    //Reduces list to a single value, left to right
        sum xs = foldl (\acc x -> acc + x) 0 xs                     //NOT Lazy !!
        sum = foldl (+) 0
        reverse' xs = foldl (flip (:)) []
    * foldr                                                         //od prawej, przydatne szczególnie jak chcemy zbudować listę wynikową !!
        map' f xs = foldr (\x acc -> f x :: acc) [] xs              //bo używamy : zamiast ++
                                                                    //works on infinite lists => you can start from some place of the infinite list and go to the beginning!! With foldl you'll never finish
    * foldl1, foldr1                                                //starting value = first element (don't work on empty lists!)
    
    * RIGHT FOLD => foldr f acc [3,4,5,6] = f 3 (f 4 (f 5 (f 6 acc)))
    * LEFT FOLD  => foldl g acc [3,4,5,6] = g (g (g (g z 3) 4) 5) 6

    * scanl, scanr              //like folds, but report all intermediate accumulator results as list
        scanl (+) 0 [3,4,5,6]   => [0,3,8,10,11]
        scanr (+) 0 [3,4,5,6]   => [11,8,3,1,0]

* Lambdas:
    * Simple lambda: \x -> x + 3
    * Partial application (better readability): (+3)
    * Multiple arguments: \x y -> x + y
    * With pattern matching: \(x, y) -> x + y

* Function application: " " and "$"
    * Function application with " " has very high precedence and it's left-associative: f x y = ((f x) y)
    * Function application with "$" has the lowest precedence and it's right-associative: f x y = (f (x y))
    * ($) :: (a -> b) -> a -> b
    
    * Works the same as $:
        (@@)       :: (a -> b) -> a -> b
        f @@ x     =  f x
        infixr 0 @@
    * Or this:
        i       :: (a -> b) -> a -> b
        f `i` x     =  f x
        infixr 0 `i`

    * Function application "$" is just another function:
        map ($ 3) [(4+), (10*), (^2)]   => [7, 30, 9]               //($ 3) = apply 3 for each function in the list
        
    * Example:
        f :: Float -> String
        f x = show x
        
        g :: String -> Int
        g x = length x
        
        (g . f) 1 => daje tą samą wartość co => g $ f 1 => ale inne są tworzy po drodze!!
        gg = g . f
        gg' = (g $ f) //to się już nie kompiluje

* Function composition with "." (cleaner to write than lambdas)
    * (.) :: (b -> c) -> (a -> b) -> a -> c
    * f . g = \x -> f (g x)  
    * Right-associtive: (f . g. h) x = f (g (h x))
        replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))   =   replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]    
        
        sum (takeWhile (<10000) (filter odd (map (^2) [1..])))   =   sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
        
    * Better readability:
        oddSquareSum =   
            let oddSquares = filter odd $ map (^2) [1..]  
                belowLimit = takeWhile (<10000) oddSquares  
            in  sum belowLimit 

    * Point-free style (defining functions in the point free style)
        - functions that never mention the data upon which they operate. You use first class functions, currying, and composition all together.
        - Point-free composition improves the clarity and readability of the code.
            More than that, it favors the practice of decomposing everything into smaller pieces that can be then composed together in a very expressive manner.
            Point-free style goes hand in hand with the practice of giving intention-revealing names. Taking the time to write good function names makes point-free composition much easier to read.
        
        - It helps the writer (and reader) think about composing functions (high level), rather than shuffling data (low level).
            Explicit points often obscure the underlying algorithm.
        - A 'points-free' definition of a function is one which does not explicitly mention the points (values) of the space on which the function acts. (Topology)
        
        sum' xs = foldl (+) 0 xs
        sum' = foldl (+) 0              //point free style
        
    * Free variable = variables that are neither local variables nor parameters of the function
    * Bound variable = was free, but has been bound to a specific value (or set of values)

* Modules
    * import Data.List (nub, sort)      //import only some of the functions
    * import Data.List hiding (nub)     //don't import some functions
    * import qualified Data.Map as M    //import qualified functions (if you have name clashes)
        use M.filter instead of Data.Map.filter
    
    * foldl'    = strict version of foldl; NOT lazy version of foldl
        Due to the lazy nature of the folds, the accumulator value isn't actually updated as the folding happens.
        What actually happens is that the accumulator kind of makes a promise that it will compute its value when asked to actually produce the result (also called a thunk).
        That happens for every intermediate accumulator and all those thunks overflow your stack.
        
        The strict folds aren't lazy buggers and actually compute the intermediate values as they go along instead of filling up your stack with thunks.
        So if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions.

* ????????????
    * Maybe = Just something | Nothing




* Symbols in Haskell: (https://stackoverflow.com/questions/10548170/what-characters-are-permitted-for-haskell-operators)

    * Operator: (operators are infix by default)
        - an operator symbol eg. + or $$
        - or an ordinary identifier enclosed in grave accents (backquotes), eg. `op`












---
# Category Theory for programmers

* Category theory
    * the source of useful programming ideas (explored eg. by Haskell)
    * deals with the kind of structure that makes programs composable (composition is at the root of category theory definition)
        1. subrouties made structured programming composable
            Side effects don't scale!
        2. object-oriented programming is all about composing objects
            Concurrency: But data hiding combined with sharing and mutation is the recipe for data races!
            Concurrecy: Combining mutex with data it protects is nice, but unfortunately locks don't compose and lock hiding makes deadlocks more likely and harde to debug! 
        3. functional programming is about composing functions and algebraic data structures,
            Concurrency: But also about composable concurrency (impossible with outer paradigms)


















