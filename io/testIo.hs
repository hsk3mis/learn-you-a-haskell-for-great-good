--import Data.Char
--import Control.Monad
--import System.IO
--import System.Directory
--import System.Environment
--import Data.List

--SEPARATING PURE AND IMPURE PARTS OF THE CODE => only deal with impure data inside impure environment

--IO action is only performed, when given the name of 'main' or inside another bigger IO action
--main = putStrLn "Hello, world!"

--main = do --the last action is the result of 'do'
--  putStrLn "Hello, What's your name?"
--  name <- getLine                     -- perform I/O action 'getLine' and then bind its result value to 'name'
--                                      -- get data from inside the I/O action
--                                      -- it can be performed only inside another I/O action
--                                      -- <- can't be the last action in the do statement
--  let upperName = map toUpper name    -- <- binds result of I/O action to name, and let binds pure expressions to names
--                                      -- "let a = x in b" is and expression, but "let a = x" is 'probably' IO action
--      lowerName = map toLower name
--  _ <- putStrLn "Getting result of action that return () is kind of stupid"
--  putStrLn ("Hey " ++ upperName ++ " " ++ lowerName ++ ", you rock!")

--main = do
--  line <- getLine
--  return 123                          -- creates I/O action that doesn't really do anything (we just encapsulate the value and don't use it any more)
--
--  if null line
--    then return ()                    -- return makes I/O action from a pure value (return :: Monad m => a -> m a)
--                                      -- return is the opposite to <-
--    else (do                          -- () are optional here
--      putStrLn $ reverseWords line
--      main)                           -- main is an I/O action, so you can call it recursively as usual
--
--reverseWords :: String -> String
--reverseWords = unwords . map reverse . words

-- putting "ala ma kota" will wait until you hit Enter, but only print "ala" on the screen, because first ' ' stops execution
--main = do
--    c <- getChar
--    if c /= ' '
--        then do
--            putChar c
--            main
--        else return ()

--when (Control.Monad.when :: Applicative f => Bool -> f () -> f ()) - looks like control flow statement, but it's just a normal function that encapsulates "if condition then I/O action else return ()" pattern
--main = do
--     c <- getChar
--     when (c /= ' ') $ do --if condition is False, then 'when' returns "return ()" action, if True then it returns given IO action
--        putChar c
--        main

--sequence (sequence :: [IO a] -> IO [a]) - takes a list of I/O actions and returns an I/O action that will perform those actions one after another and return the list of the results
--main = do
--    rs <- sequence [getLine, getLine, getLine]    --same as doing: a <- getLine; b <- getLine; print [a, b]
--    print rs
--common pattern - mapping an I/O action over a list and then sequencing it (creates a single I/O action that performs a list of I/O actions)
--sequence (map print [1,2,3,4,5])
--mapM print [1,2,3]  => sequence (map print [1,2,3])
--mapM_ print [1,2,3] => if we don't care about the result of the I/O actions

--forever - repeats I/O action forever

--forM - same as mapM but the arguments are switched; means make an I/O action for every element in the list
--main = do
--    colors <- forM [1,2,3,4] (\a -> do  --colors are extracted, so they are a normal list of values
--        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
--        color <- getLine    --can be shortened as just "getLine" without extracting value
--        return color)
--    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
--    mapM putStrLn colors    --same as: forM colors putStrLn

--FILES AND STREAMS

--getContents (:: IO String) => reads everything from stdin until it encounters end-of-file character (Ctrl-D). Lazy!
--content <- getContents => reads only part of the stdin into content string, lazily, because it's a list

--Lazy reading stdin and capslock it (stream like!!) - Doesn't read whole stdin! it prints out capslocked version as it reads it!
--main = do
--    contents <- getContents         --strings are lists (Lazy!) and getContents is Lazy!
--                                    --this binding is not represented as a real thing in memory, but more like a promise to produce a string eventually
--    putStr (map toUpper contents)
--When putStr happens, it asks for a capslocked line. Then contents gets the line from the terminal. Then it's mapped to toUpper and is given to putStr. Then putStr requests another line!

--it works ;)
--putStr (map toUpper (repeat 'a'))

-- print only lines shorter than 10 characters
--main = do
--    contents <- getContents
--    putStr (shortLinesOnly contents)
--
--shortLinesOnly :: String -> String
--shortLinesOnly input =
--    let allLines = lines input
--        shortLines = filter (\line -> length line < 10) allLines
--        result = unlines shortLines
--    in  result


--interact => common idiom of reading from stdin, transforming, and outputting to stdout
--main = interact $ unlines . filter ((<10) . length) . lines

--respondPalindromes - respond with "palindrome" or "not palindrome" for every line of input
--respondPalindromes = unlines . map (\line -> if isPalindrome line then "palindrome" else "not palindrome") . lines
--  where isPalindrome xs = xs == reverse xs
--main = interact respondPalindromes

--Lazy reading/writing of files/network sockets is done in chunks (controlled by OS or you can control it yourself with hSetBuffering
--by default flushing is done by every line/chunk depending on the buffering mode. You can flush it manually with hFlush

--Add a line to todo.txt file
--main = do
--    todoItem <- getLine
--    appendFile "todo.txt" (todoItem ++ "\n")

--Remove numbered line from todo.txt file
--main = do
--    handle <- openFile "todo.txt" ReadMode
--    (tempName, tempHandle) <- openTempFile "." "temp"
--    contents <- hGetContents handle
--    let todoTasks = lines contents
--        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
--    putStrLn "These are your TO-DO items:"
--    putStr $ unlines numberedTasks
--    putStrLn "Which one do you want to delete?"
--    numberString <- getLine
--    let number = read numberString
--        newTodoItems = delete (todoTasks !! number) todoTasks
--    hPutStr tempHandle $ unlines newTodoItems
--    hClose handle
--    hClose tempHandle
--    removeFile "todo.txt"
--    renameFile tempName "todo.txt"

-- Exit program with error
--errorExit :: IO ()

{-------------- Command line arguments -- todo program (eg. todo add todo.txt "Find the magic sword of power") --------------}
--dispatch :: [(String, [String] -> IO ())]           --dispatch association that maps commands to functions that take arguments and returns IO action
--dispatch =  [ ("add", add)
--            , ("view", view)
--            , ("remove", remove)
--            , ("bump", bump)
--            ]
--
--main = do
--    (command:args) <- getArgs
--    let (Just action) = lookup command dispatch     --lookup :: a -> [(a, b)] -> Maybe b
--    action args
--
--add :: [String] -> IO ()
--add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")   --args: fileName todoItem
--
--view :: [String] -> IO ()
--view [fileName] = do                                                --args: fileName
--    contents <- readFile fileName
--    let todoTasks = lines contents
--        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
--    putStr $ unlines numberedTasks
--
--remove :: [String] -> IO ()
--remove [fileName, numberString] = do
--    removeAndReturn [fileName, numberString]
--    return ()
--    -- inny zapis w jednej linii:
--    = do removeAndReturn [fileName, numberString]; return ()

--
--removeAndReturn :: [String] -> IO String
--removeAndReturn [fileName, numberString] = do                                --args: fileName numberToDelete
--    handle <- openFile fileName ReadMode
--    (tempName, tempHandle) <- openTempFile "." "temp"
--    contents <- hGetContents handle
--    let number = read numberString
--        todoTasks = lines contents
--        taskToBeRemoved = todoTasks !! number
--        newTodoItems = delete taskToBeRemoved todoTasks
--    hPutStr tempHandle $ unlines newTodoItems
--    hClose handle
--    hClose tempHandle
--    removeFile fileName
--    renameFile tempName fileName
--    return taskToBeRemoved
--
----moves given todoItem number and bumps it to the top of the list
--bump :: [String] -> IO ()
--bump [fileName, numberString] = do
--    removedTask <- removeAndReturn [fileName, numberString]
--    addAtBeginning [fileName, removedTask]
--
--addAtBeginning :: [String] -> IO ()
--addAtBeginning [fileName, todoItem] = do
--    handle <- openFile fileName ReadMode
--    (tempName, tempHandle) <- openTempFile "." "temp"
--    contents <- hGetContents handle
--    hPutStr tempHandle $ (todoItem ++ "\n" ++ contents)
--    hClose handle
--    hClose tempHandle
--    removeFile fileName
--    renameFile tempName fileName


{-------------- Randomness --------------}
-- RandomGen typeclass for types that can act as sources of randomness
-- Random    typeclass for things that can take on random values (eg. Boolean, Number)
--           every type that what to allow you make random values of itself need to provide instance of Random (random and randomR functions)
-- StdGen    type exported by System.Random that is an instance of RandomGen typeclass
--           created manually with mkStdGen :: Int -> StdGen or get from system with getStdGen :: IO StdGen

--import System.Random
--random :: (RandomGen g, Random a) => g -> (a, g)
--random (mkStdGen 100) :: (Int, StdGen)    -- we need to tell Haskell what type of random we want
                                            -- random function can return value of any type that's part of the Random typeclass
                                            -- calling random function actually calls random function of instance of proper type that we what to randomize
--main = do
--    let (randomInt, _) = random (mkStdGen 100) :: (Int, StdGen)
--    putStrLn randomInt
--    let (randomBool, _) = random (mkStdGen 100) :: (Bool, StdGen)
--    putStrLn randomBool

-- tossing a coin 3 times
--threeCoins :: StdGen -> (Bool, Bool, Bool)
--threeCoins gen =
--    let (firstCoin, newGen) = random gen
--        (secondCoin, newGen') = random newGen
--        (thirdCoin, newGen'') = random newGen'
--    in  (firstCoin, secondCoin, thirdCoin)
--
--threeCoins (mkStdGen 21) -- no need to specify type we want to generate, because it's already specified in the function type

-- randoms function that takes a random generator and returns an infinite sequence of values
--take 5 $ randoms (mkStdGen 22) :: [Int]

--randoms' :: (RandomGen g, Random a) => g -> [a]
--randoms' g = let (value, newGen) = random g in value:randoms' newGen --lazily creates a list of random values

-- how to return a random generator back
--finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
--finiteRandoms 0 gen = ([], gen)
--finiteRandoms n gen = let (value, newGen) = random gen
--                          (restOfList, finalGen) = finiteRandoms (n-1) newGen
--                      in (value: restOfList, finalGen)

-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
--randomR (1,6) (mkStdGen 23)
-- randomRs - creates a stream of random values within a range
--take 10 $ randomRs ('a', 'z') (mkStdGen 24) :: [Char]

-- getStdGen :: IO RandomGen => random generator from the System
-- calling getStdGen twice will return the same random generator twice !!!!
-- you need to always propagate new random generator generated from the previous one
-- or you can create an infinite stream of random values and use it / split it etc.
--main = do
--    gen <- getStdGen
--    putStrLn $ take 20 (randomRs ('a','z') gen) -- "pybphhzzhuepknbykxhe"
--    gen2 <- getStdGen
--    putStr $ take 20 (randomRs ('a','z') gen2)  -- "pybphhzzhuepknbykxhe" the same string!!

-- newStdGen - splits current global random generator into 2 generator and updates the global random generator
-- then calling getStdGen again returns this new updated global random generator
--    get3 <- newStdGen -- new random generator
--    newStdGen         -- or just generate new random generator and don't get it yet
                        -- ciągłe zmienianie stanu globalnego (synchronizacja) oraz ciągłe eksloatowanie zrodła entropii to nie jest najlepszy pomysł
                        -- dlatego warto używac zwracanego generatora!!
-- jak biznesowo obsługiwać dużą potrzebę losowych rzeczy? => np. przekazać nieskończony strumień Intów

-- Example: guessing for a random number
--main = do
--    gen <- getStdGen
--    askForNumber gen
--
--askForNumber :: StdGen -> IO ()
--askForNumber gen = do
--    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
--    putStr "Which number in the range from 1 to 10 am I thinking of? "
--    numberString <- getLine
--    when (not $ null numberString) $ do
--        let number = read numberString                                    -- read convert string to some other type
--        if randNumber == number
--            then putStrLn "You are correct!"
--            else putStrLn $ "Sorry, it was " ++ show randNumber
--        askForNumber newGen

{-------------- ByteStrings --------------}
-- Bytestrings - similar to lists but each element is a byte (8 bits) (Word8) and laziness behaves differently
-- processing big files as a lazy list of characters is not very efficient (list are veeery lazy and always process a single element and a promise ('thunk' in technical term) for the rest
-- Strict (Data.ByteString) - no laziness !!
-- Lazy (Data.ByteString.Lazy) - lazy, but not as lazy as lists !! Lazy by chunks of 64KB
--import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString as S

-- Word8 is also in Num typeclass, so eg. 5 is polymorphic and can also act like a Word8. If used with bigger number (eg. 336) will wrap around (eg. 80)
-- pack :: [Word8] -> ByteString -- takes lazy list and make it less lazy ;)
--B.pack [99,97,110]
--B.pack [98..120]
-- unpack :: ByteString -> [Word8] - takes a bytestring and turns it into a list of bytes

-- fromChunks - takes a list of strict bytestrings and converts it to a lazy bytestring
-- toChunks - takes a lazy bytestring and converts it to a list of strict ones
-- good if you have a lot of small strict bytestrings and you want to process them efficiently without joining them into one big strict bytestring in memory first

-- Concatenation = bytestring version of :
-- cons - adds byte at the beginning, lazy so always creating a new chunk
-- cons' - strict version, adds to existing chunk if it can
-- empty - creates empty bytestring
--foldr B.cons' B.empty [50..60]

-- copying files: runhaskell bytestringcopy.hs something.txt ../../something.txt
--import System.Environment
--import qualified Data.ByteString.Lazy as B
--
--main = do
--    (fileName1:fileName2:_) <- getArgs
--    copyFile fileName1 fileName2
--
--copyFile :: FilePath -> FilePath -> IO ()
--copyFile source dest = do
--    contents <- B.readFile source
--    B.writeFile dest contents

-- HINT: Usually write programs by using normal strings and then convert them to use bytestrings if the performance is not satisfactory.


{-------------- Exceptions --------------}
-- Use algebraic data types like Maybe or Either for functions that may not return result etc.
-- Exceptions - makes more sense in IO context, but pure code also can throw exceptions (eg. head [], 4 `div` 0)
-- But exceptions can only be caught in IO part of the application!! But can be thrown by the pure code
-- Because pure code is lazy and doesn't have a well-defined order of evaluation, but IO code does!!
-- HINT: try not to mix pure code and exceptions => better use types like Either or Maybe to represent results that may have failed

-- catch :: IO a -> (IOError -> IO a) -> IO a -- takes IO action (eg. open a file), exception handler, the result if either the result of the first action or the action produced by the handler
-- type IOError can't be patter-matched, because it depends on the implementation of the language
-- userError / isUserError predicate - can create user defined errors with description

--import System.Environment
--import System.IO
--import System.IO.Error
--import Control.Exception
--
--main = toTry `catch` handler
--
--toTry :: IO ()
--toTry = do (fileName:_) <- getArgs
--           contents <- readFile fileName
--           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--
--handler :: IOError -> IO ()
--handler e
--    | isDoesNotExistError e = putStrLn "The file doesn't exist!"    -- predication over IOError
--    | isDoesNotExistError e =
--            case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
--                                     Nothing   -> putStrLn "Whoops! File does not exist at unknown location!"
--    | otherwise = ioError e                                         -- rethrow other exceptions (ioError :: IOException -> IO a -- creates IO action that will throw exception)

-- different parts of the code can be handled by different handlers
--main = do myFunctionToTry `catch` handler1
--          thenTryThisMyOtherFunction `catch` handler2
--          thenLaunchRockets


--ioIf :: IO Bool -> IO a -> IO a -> IO a
--ioIf p f g = do
--  b <- p
--  if b then f else g
--
--main = do
--  let t = return True
--  ioIf t (putStrLn "true") (putStrLn "false")



import System.Random
--
--f :: Char -> Char
--f x = x
--
--main = do
--  let (val, gen) = random (mkStdGen 100)
--  return $ f val


main = do
  gen <- getStdGen
  putStrLn


