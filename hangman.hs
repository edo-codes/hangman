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
startplaying words = do index <- randomRIO (0,length words - 1)
                        playgame (words !! index) []
                        putStrLn $ strings lang Another
                        ans   <- getChar
                        case ans of
                           'n' -> return ()
                           _   -> startplaying words

playgame :: String -> [Char] -> IO ()
playgame word guessed
  | complete                                       = printState word guessed Won ""
  | guessedwrong word guessed >= length hangman -1 = printState word guessed Lost word
  | otherwise                                      = do printState word guessed Pick ""
                                                        l <- fmap toUpper getChar
                                                        let guessed' | not (isAlpha l)  = guessed
                                                                     | l `elem` guessed = guessed
                                                                     | otherwise        = l : guessed
                                                        playgame word guessed'
  where complete :: Bool
        complete = all (`elem` guessed) (map toUpper word)

guessedwrong :: String -> [Char] -> Int
guessedwrong word guessed = length $ filter (`notElem` map toUpper word) guessed

printState :: String -> [Char] -> Message -> String -> IO ()
printState word guessed message string = putStrLn $ "\ESC[2J" ++
                                  unlines [ hangman !! (guessedwrong word guessed)
                                          , map (\x -> if (elem (toUpper x) guessed) then x else '_') word
                                          , (strings lang Used) ++ intersperse ' ' guessed
                                          , strings lang message ++ string
                                          ]

strings :: Language -> Message -> String
strings NL m = case m of
                  Another -> "Wil je nog een keer spelen? [Y/n]"
                  Won     -> "Gefeliciteerd! Je hebt het woord geraden!"
                  Lost    -> "Je bent dood. Het woord was "
                  Pick    -> "Kies een letter"
                  Used    -> "Gebruikte letters: "
strings EN m = case m of
                  Another -> "Play another game? [Y/n]"
                  Won     -> "Congratulations! You got it!"
                  Lost    -> "You're dead. The word was "
                  Pick    -> "Pick a letter"
                  Used    -> "Used letters: "

data Message = Another | Won | Lost | Pick | Used

data Language = NL
              | EN
              deriving (Show)

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