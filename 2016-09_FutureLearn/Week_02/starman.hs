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
