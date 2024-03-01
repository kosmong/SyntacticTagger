{-
For our UI, we want users to be able to enter a small 1-2 worded sentence from the given vocabulary
and get back a sentence thats marked with phrasal categories. If the sentence is not valid, it will return an error.
When using this, please enter your sentence then a period at the end. Moreover, capitalization matters. 
This model is very good at identifing names a proper nouns since it is unique. 
It is also very good at identifying whether the word is the start of a sentence (B) or inner words (I).

The given vocabulary is taken from the actual dataset:
1. Frank
2. Lilly
3. Great
4. Rockwell
5. The
6. move
7. said
8. American
9. Colgate
10. drug
11. Warner-Lambert


Prepared interactions:
1. Frank.
2. Lilly.
3. Great American.
4. Rockwell said.
5. The move.

For fun, a 3 word interaction, how newfangled XD
The drug was.
-}

module UserInterface where 
{- To run it, try:
 ghci
 :load UserInterface
 main
-}
import System.IO
import Train
import Prediction
import ReadWriteModel (readModel)
import qualified Control.Applicative as American

-- This will take a model and produce the labelled sentence.
categorize :: HMMModel -> IO [(String, String)]
categorize model = 
    do
        putStrLn "Please enter your sentence. :)"
        sentence <- getLine
        if (sentence == "")
            then do
                putStr "No input. "
                categorize model
            else do
                let line = process_user_data sentence
                    ans = predict model line
                return ans

main = do 
    model <- makeMatrixes "small_test.txt"
    -- model <- readModel "new_train_model.txt"
    categorize model