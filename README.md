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

* Links
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


















