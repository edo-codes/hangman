import System.Random (randomRIO)
import Data.Char (isAlpha, toUpper)
import Data.List (intersperse)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering) )

lang = EN

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          f <- readFile $ show lang
          startplaying $ lines f

startplaying :: [String] -> IO ()
startplaying words = do index <- randomRIO (0,length words)
                        let beg = beginstate $ words !! index
                        playgame beg
                        putStrLn $ strings lang "Another"
                        ans   <- getChar
                        case ans of
                           'n' -> return ()
                           _   -> startplaying words

playgame :: State -> IO ()
playgame state
   | condition state == AlreadyGuessed = newguess
   | condition state == InvalidLetter  = newguess
   | condition state == Win            = do putStrLn $ formatState state
                                            putStrLn $ strings lang "Won"
                                            return ()
   | condition state == Loss           = do putStrLn $ formatState state
                                            putStrLn $ (strings lang "Lost") ++ word state
                                            return ()
   | condition state == Okay           = newguess
   where newguess = do putStrLn $ formatState state
                       putStrLn $ "\n" ++ (strings lang "Pick")
                       l <- getChar
                       putStr "\n"
                       playgame $ turn state l

formatState :: State -> String
formatState state = "\ESC[2J" ++ 
                    unlines [ hangman !! tries state
                            , showprogress
                            , (strings lang "Used") ++ intersperse ' ' (guessed state)
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

strings :: Language -> String -> String
strings NL s = case s of
                  "Another" -> "Wil je nog een keer spelen? [y/n]"
                  "Won"     -> "Gefeliciteerd! Je hebt het woord geraden!"
                  "Lost"    -> "Je bent dood. Het woord was "
                  "Pick"    -> "Kies een letter"
                  "Used"    -> "Gebruikte letters: "
strings EN s = case s of
                  "Another" -> "Play another game? [y/n]"
                  "Won"     -> "Congratulations! You got it!"
                  "Lost"    -> "You're dead. The word was "
                  "Pick"    -> "Pick a letter"
                  "Used"    -> "Used letters: "

data Language = NL
              | EN
              deriving (Show)
