import System.Random (randomRIO)
import Data.Char (isAlpha, toUpper)
import Data.List (intersperse)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering) )

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          f <- readFile "wordlist"
          startplaying $ lines f

startplaying :: [String] -> IO ()
startplaying words = do index <- randomRIO (0,length words)
                        let beg = beginstate $ words !! index
                        playgame beg
                        putStrLn "Play another game? [y/n]"
                        ans   <- getChar
                        case ans of
                           'n' -> return ()
                           _   -> startplaying words

playgame :: State -> IO ()
playgame state
   | condition state == AlreadyGuessed = newguess
   | condition state == InvalidLetter  = newguess
   | condition state == Win            = do putStrLn $ formatState state
                                            putStrLn "Congratulations! You got it!"
                                            return ()
   | condition state == Loss           = do putStrLn $ formatState state
                                            putStrLn $ "You're dead. The word was " ++ word state
                                            return ()
   | condition state == Okay           = newguess
   where newguess = do putStrLn $ formatState state
                       putStrLn "\nPick a letter"
                       l <- getChar
                       putStr "\n"
                       playgame $ turn state l

formatState :: State -> String
formatState state = "\n\n\n\n\n" ++ 
                    unlines [ hangman !! tries state
                            , showprogress
                            , "letters used: " ++ intersperse ' ' (guessed state)
                            ]
   where showprogress = zipWith (\x y -> if y then x else '_') (word state) (progress state)

beginstate :: String -> State
beginstate word = State { condition = Okay
                        , word      = word
                        , progress  = replicate (length word) False
                        , tries     = 0
                        , guessed   = []
                        }


turn :: State -> Char -> State
turn state letter
   | not  $ isAlpha letter                       = samestate InvalidLetter
   | elem (toUpper letter) $ guessed state = samestate AlreadyGuessed
   | won  $ newstate Win                         = newstate  Win
   | lost $ newstate Loss                        = newstate  Loss
   | otherwise                                   = newstate  Okay
   where samestate :: Condition -> State
         samestate cond =
             State { condition = cond
                   , word      = word state
                   , progress  = progress state
                   , tries     = tries state
                   , guessed   = guessed state
                   }
         newstate :: Condition -> State
         newstate cond =
             State { condition = cond
                   , word      = word state
                   , progress  = zipWith
                        (||)
                        (progress state)
                        guess
                   , tries     = if or guess
                                    then tries state
                                    else tries state + 1
                   , guessed   = toUpper letter : guessed state
                   }
         guess :: [Bool]
         guess = map ((==) $ toUpper letter) $ (map toUpper $ word state)
         won :: State -> Bool
         won state = and $ progress state
         lost :: State -> Bool
         lost state = tries state >= (length hangman - 1)

data State = State { condition      :: Condition
                   , word           :: String
                   , progress       :: [Bool]
                   , tries          :: Int
                   , guessed        :: [Char]
                   } deriving (Eq, Show)

data Condition = Okay
               | InvalidLetter
               | AlreadyGuessed
               | Loss
               | Win
               deriving (Eq, Show)

hangman = [ unlines  [ ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     ]
          , unlines  [ ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , " ___"
                     ]
          , unlines  [ ""
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|        |"
                     , "|        |"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|       /|"
                     , "|        |"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|       /|\\"
                     , "|        |"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|       /|\\"
                     , "|        |"
                     , "|       /"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|       /|\\"
                     , "|        |"
                     , "|       / \\"
                     , "|"
                     , "|___"
                     ]
          ]
