{-
For our UI, we want users to be able to enter a sentence and get back a sentence thats marked with phrasal categories.
We need to ask the user to enter a sentence first.
-}

module UserInterface where 
{- To run it, try:
 ghci
 :load SyntacticTaggerUI
 go
-}
import System.IO
import Train
import Prediction
import ReadWriteModel (readModel)

-- this will probably take our model and produce our string
categorize :: HMMModel -> IO [(String, String)]
categorize model = 
    do
        putStrLn "Please enter your sentence. :)"
        sentence <- getLine
        if (sentence == "")
            then do
                putStr "no input, please enter something"
                categorize model
            else do
                let line = process_user_data sentence
                    ans = predict model line
                return ans

        --         askagain model
        -- return ans


askagain :: HMMModel -> IO [(String, String)]
askagain model = 
    do
        putStrLn "Do you want to ask again?"
        ans <- getLine
        if ans `elem` ["y","yes","ye","oui"]
            then do
                categorize model
            else return [("", "")]

main = do 
    -- model <- makeMatrixes "new_test.txt"
    -- model <- readModel "new_train_model.txt"
    model <- readModel "./small_model.txt"
    categorize model