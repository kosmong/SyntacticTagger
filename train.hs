module Train where

data HMMMatrix = Empty
        | Matrix [String] [String] [[Double]]
    deriving Show

data HMMModel = None
        | Model HMMMatrix HMMMatrix
    deriving Show

{- 
Get data from the matrix
-}
getData :: HMMMatrix -> String -> String -> Double
getData Empty _ _ = 0.0
getData (Matrix lst1 lst2 lstoflst) str1 str2 =
    if (str1 `elem` lst1 && str2 `elem` lst2) then
        ((lstoflst !! (find lst1 str1 0)) !! find lst2 str2 0)
    else
        0.0

{- 
The function that replaces the element in the list given index
-}
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

{- 
The function that replace the element in the 2d list given index
-}
changeElem :: Int -> Int -> a -> [[a]] -> [[a]]
changeElem _ _ _ [] = []  -- Empty list case
changeElem 0 col x (l:ls) = replaceAtIndex col x l : ls  -- Reduce the column
changeElem row col x (l:ls) = l : changeElem (row - 1) col x ls  -- Reduce the row

{-
Change the element in the HMMMatrix corresponding to the given strings to the target value
-}
changeElem_str :: String -> String -> Double -> HMMMatrix -> HMMMatrix
changeElem_str str1 str2 x (Matrix lst1 lst2 lstoflst) =
    Matrix lst1 lst2 (changeElem (find lst1 str1 0) (find lst2 str2 0) x lstoflst)


{- 
Find the element, return index if found, return -1 if not found
-}
find :: (Eq t) => [t] -> t -> Int -> Int
find [] _ _ = -1
find (h : t) s n
    | h == s = n
    | otherwise = find t s (n+1)

{-
Initialize the matrix to all 0s
-}
initMatrix :: [String] -> [String] -> HMMMatrix
initMatrix lst1 lst2 = Matrix lst1 lst2 [[0.0 | _ <- lst2] | _ <- lst1]

{- 
Split the elements into pairs of strings
-}
split :: String -> (String, String)
split [] = ("","")
split str = helperfun str ""

{-
Helper for split function, build string until space
-}
helperfun :: String -> String -> (String, String)
helperfun "" _ = ("","")
helperfun (h:t) acc
    | h == ' ' = (acc, t)
    | otherwise = helperfun t (acc ++ [h])

{-
Given a data set in a text file, make the emission and transmission matrixes for it
Return a model storing both the matrixes
-}
makeMatrixes :: FilePath -> IO HMMModel
makeMatrixes path = do
    contents <- readFile path
    let
        sentences = lines contents
        -- list of pairs: first is a word, second is the pos
        pair_lst = map split sentences

        -- words and pos lst (repetitive elements)
        words = map fst pair_lst
        pos = map snd pair_lst

        -- words and pos lst
        word_lst = filter (/= "") (newElem words [])
        pos_lst = filter (/= "") (newElem pos [])
        front_pos = "<S>": pos_lst
        back_pos = pos_lst ++ ["<E>"]
        pos_SE = ("<S>":addSEPos pos) ++ ["<E>"]

        -- get all possible combination for transition and emission
        wrd_cat_combos = [(x,y) | x<-word_lst, y<-pos_lst]
        cat_cat_combos = [(x,y) | x<-front_pos, y<-back_pos]

        -- matrix initialization
        empty_transition_matrix = initMatrix front_pos back_pos
        empty_emission_matrix = initMatrix word_lst pos_lst

        -- fill in the matrixes
        emission_matrix = fillEmissionMatrix empty_emission_matrix pair_lst wrd_cat_combos
        transition_matrix = fillTransitionMatrix empty_transition_matrix pos_SE cat_cat_combos
    return (Model transition_matrix emission_matrix)


{-
Filter out redundant elements
-}
newElem :: Eq a => [a] -> [a] -> [a]
newElem [] acc = acc
newElem (h:t) acc
    | h `elem` acc = newElem t acc
    | otherwise = newElem t (h : acc)

{-
Add <E> to the end of a sentence and <S> to the beginning of a sentence
-}
addSEPos :: [String] -> [String]
addSEPos [] = []
addSEPos (h:t)
    | h == "" = "<E>":("<S>" : addSEPos t)
    | otherwise = h:addSEPos t

{-
Fill the emission matrix
Calculate probability of all Word Category combos
-}
fillEmissionMatrix :: HMMMatrix -> [(String,String)] -> [(String,String)] -> HMMMatrix
fillEmissionMatrix Empty _ _ = Empty
fillEmissionMatrix e_matrix _ [] = e_matrix
fillEmissionMatrix (Matrix lst1 lst2 lstoflst) pairs (p1:rest) =
    let
        curr_emission_matrix = calculateEmission (Matrix lst1 lst2 lstoflst) (fst p1) (snd p1) pairs
    in
        fillEmissionMatrix curr_emission_matrix pairs rest

{-
Calculate the conditional probability of a Word Category combo
Change the probability in the matrix corresponding to the Word and Category combo
-}
calculateEmission :: HMMMatrix -> String -> String -> [(String,String)] -> HMMMatrix
calculateEmission Empty _ _ _ = Empty
calculateEmission (Matrix lst1 lst2 lstoflst) wrd cat pairs =
        let
            pairsWithCategory = filter (\x -> snd x == cat) pairs
            catWrdCoocurrence = filter (\x -> fst x == wrd) pairsWithCategory
            catWrdCoocurrenceCount = fromIntegral (length catWrdCoocurrence)
            catCount = fromIntegral (length pairsWithCategory)
            prob = catWrdCoocurrenceCount / catCount
        in
            changeElem_str wrd cat prob (Matrix lst1 lst2 lstoflst)

{-
Fill the transition matrix
Calculate probability of all consecutive Category combos
-}
fillTransitionMatrix :: HMMMatrix -> [String] -> [(String,String)] -> HMMMatrix
fillTransitionMatrix Empty _ _ = Empty
fillTransitionMatrix t_matrix _ [] = t_matrix
fillTransitionMatrix (Matrix lst1 lst2 lstoflst) pos_order (p1:rest) =
    let
        pairs = getNeighbourPosPairs pos_order
        pos_pairs = pairs ++ [(snd (last pairs), "<E>")]
        curr_transition_matrix = calculateTransmission (Matrix lst1 lst2 lstoflst) (fst p1) (snd p1) pos_pairs
    in
        fillTransitionMatrix curr_transition_matrix pos_order rest

{-
Calculate the conditional probability of a consecutive Category combo
Change the probability in the matrix corresponding to the combo
-}
calculateTransmission :: HMMMatrix -> String -> String -> [(String, String)] -> HMMMatrix
calculateTransmission Empty _ _ _ = Empty
calculateTransmission (Matrix lst1 lst2 lstoflst) pos1 pos2 pos_pairs =
    let
        pairsWithPos1First = filter (\x -> fst x == pos1) pos_pairs
        pairsWithPos1Pos2 = filter (\x -> snd x == pos2) pairsWithPos1First
        pos1Pos2Count = fromIntegral (length pairsWithPos1Pos2)
        pos1Count = fromIntegral (length pairsWithPos1First)
        prob = pos1Pos2Count / pos1Count
    in
        changeElem_str pos1 pos2 prob (Matrix lst1 lst2 lstoflst)

{-
Get all pairs of consecutive string in a list of strings
-}
getNeighbourPosPairs :: [String] -> [(String, String)]
getNeighbourPosPairs [] = []
getNeighbourPosPairs (c1:c2:t)
    | c2 == [] || t == [] = []
    | c1 == "<E>" && c2 == "<S>" = getNeighbourPosPairs t
    | otherwise = (c1,c2) : getNeighbourPosPairs (c2:t)


-- for small test
lst1= ["yes","no","jak", "pos"]
lst2=["adj","adv","n"]
ma = initMatrix lst1 lst2
pairs = [("yes","adj"),("no","adv"),("jak","n"), ("pos", "n")]
all_combo = [(x,y) | x<-lst1, y<-lst2]
-- ghci> fillEmissionMatrix ma pairs all_combo