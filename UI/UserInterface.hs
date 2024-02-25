{-
For our UI, we want users to be able to enter a sentence and get back a sentence thats marked with phrasal categories.
We need to ask the user to enter a sentence first.
-}

module UserInterface where 
{- To run it, try:
 ghci
 :load SyntacticTaggerUI
 main
-}
import System.IO

-- references the code at https://www.cs.ubc.ca/~poole/cs312/2024/haskell/TwentyQs.hs
-- this will probably take our model and produce our string
categorize :: String -> IO String
categorize model = 
    do
        putStrLn "Please enter your sentence. :)"
        sentence <- getLine
        -- line <- preprocess sentence
        -- ans <- predict model sentence
        putStrLn "ans"
        putStrLn "Is this correct?"
        correct <- getLine
        if correct `elem` ["n","no","non"]
            then do 
                -- update model
                categorize model
            else return "ans"
        
        askagain model
        return sentence

askagain :: String -> IO String
askagain model = 
    do
        putStrLn "Do you want to ask again?"
        ans <- getLine
        if ans `elem` ["y","yes","ye","oui"]
            then do
                categorize model
            else return ""

main = categorize "main"