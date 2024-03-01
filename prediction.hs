module Prediction where

import Data.Char
import Train
import Data.List
import Data.Ord

-- split the user input sentence string into list of strings
split2lst :: String -> [String]
split2lst str = splitsep (\c -> c == ' ') str

-- helper function for split2lst
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep f [] = [[]]
splitsep f (h:t) = helper_split2lst f (h:t) []

-- helper function for splitsep
helper_split2lst :: (a -> Bool) -> [a] -> [a] -> [[a]]
helper_split2lst f [] acc = [acc]
helper_split2lst f (h:t) acc = if f h then acc : helper_split2lst f t [] else helper_split2lst f t (acc ++ [h])

-- separates the words and punctuation marks
split_punctuation_marks :: [String] -> [String]
split_punctuation_marks [] = []
split_punctuation_marks (h:t) 
    | isLetter (last h) = h : split_punctuation_marks t
    | otherwise = (take (length h - 1) h) : [last h] : split_punctuation_marks t

-- process the user input data into list of String
process_user_data :: String -> [String]
process_user_data str = split_punctuation_marks (split2lst str)

-- argmax function that returns the arguments that maximize the function
argmax :: (Ord a) => (b -> a) -> [b] -> b
argmax f l
    | null l = error "Invalid sentence, cannot predict."
    | otherwise = maximumBy (comparing f) l

-- enumerate all the senarios
enumerateScenarios :: [a] -> Int -> [[a]]
enumerateScenarios _ 0 = [[]]  -- Base case: an empty list
enumerateScenarios xs k = [x : rest | x <- xs, rest <- enumerateScenarios xs (k - 1)]

generate_transitions :: [String] -> HMMModel -> [[String]]
generate_transitions [] _ = [["<S>", "<E>"]]
generate_transitions lststr (Model (Matrix (c:cs) catlst2 lstoflst) emission_matrix) = 
    let
        transitions = enumerateScenarios cs (length lststr)
    in 
        addSE transitions

addSE :: [[String]] -> [[String]]
addSE [] = []
addSE (l:ls) = ("<S>" : l ++ ["<E>"]) : addSE ls

transition_filter ::  HMMModel -> [String] -> [[String]] -> [[String]]
transition_filter model strs lstoflst = filter (\x -> prob model strs x /= 0) lstoflst


prob :: HMMModel -> [String] -> [String] -> Double
prob None _ _ = 0
prob (Model transition_matrix emission_matrix) sentence categories = 
    let
        transition_probability = trans_prob transition_matrix categories
        emission_probability = emis_prob emission_matrix sentence (removeFirstLast categories)
    in 
        -- if emission_probability == 0
        --     then transition_probability
        --     else transition_probability * emission_probability
        transition_probability * emission_probability


trans_prob :: HMMMatrix -> [String] -> Double
trans_prob _ [] = 0
trans_prob _ ("<E>" : []) = 1
trans_prob matrix (str1 : str2 : t) = getData matrix str1 str2 * trans_prob matrix (str2:t)

emis_prob :: HMMMatrix -> [String] -> [String] -> Double
emis_prob _ [] _ = 1
emis_prob matrix (w:ws) (c:cs) = (getData matrix w c) * (emis_prob matrix ws cs)

removeFirstLast :: [a] -> [a]
removeFirstLast [] = []
removeFirstLast (x:xs) = init xs

best_solution :: HMMModel -> [String] -> [[String]] -> [String]
best_solution model sentence lstoflst = argmax (prob model sentence) (transition_filter model sentence lstoflst)

final_output :: [String] -> [String] -> [(String, String)]
final_output sentence category = ("", "<S>") : zip sentence (removeFirstLast category) ++ [("", "<E>")]

predict :: HMMModel -> [String] -> [(String, String)]
predict model sentence = final_output sentence (best_solution model sentence (generate_transitions sentence model))