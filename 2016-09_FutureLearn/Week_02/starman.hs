-- Replace letters by dashes if not correctly guessed
makeSecretExcept :: String -> String -> String
makeSecretExcept secret guesses =
  [if elem letter guesses then letter else '-' | letter <- secret]

-- Check if the user has won
hasWon :: String -> String -> Bool
hasWon secret guesses = all (flip elem guesses) secret

mainLoop :: String -> Int -> Int -> String -> IO ()
mainLoop secret maxLives currLives guesses = do
  putStrLn ((show currLives) ++ " of " ++ (show maxLives))
  if currLives == 0
  then putStrLn "Game over"
  else do
    putStrLn (makeSecretExcept secret guesses)
    putStr "Your guess: "
    line <- getLine
    let newGuesses = head line : guesses
    let currLives' = if elem (head line) secret
                     then currLives
                     else currLives - 1
    if hasWon secret newGuesses
    then do { putStrLn secret; putStrLn "You won!" }
    else mainLoop secret maxLives currLives' newGuesses


-- Solution --
check :: String -> String -> Char -> (Bool, String)
check word display c =
  (elem c word, [if c==x then x else y | (x, y) <- zip word display])

makeSecret :: String -> String
makeSecret word = ['-' | _ <- word]

turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
     then putStrLn "You lose"
     else if word==display
          then putStrLn "You win!"
          else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n - 1
     turn word display' n'

starman :: String -> Int -> IO ()
starman word n = turn word (makeSecret word) n
