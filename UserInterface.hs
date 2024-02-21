{-
For our UI, we want users to be able to enter a sentence and get back a sentence thats marked with phrasal categories.
We need to ask the user to enter a sentence first.
-}

module SyntacticTaggerUI where 
{- To run it, try:
 ghci
 :load TwentyQs
 go
-}
import System.IO

-- this will probably take our model and produce our string
categorize :: String -> IO String
categorize model = 
    do
        putStrLn "Please enter your sentence. :)"
        sentence <- getLine
        return sentence