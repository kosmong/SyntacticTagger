data HMMMatrix = Empty
        | Matrix [String] [String] [[Double]]
    deriving Show

data HMMModel = None
        | Model HMMMatrix HMMMatrix
    deriving Show

-- get data from the matrix
getData :: HMMMatrix -> String -> String -> Maybe Double
getData Empty _ _ = Nothing
getData (Matrix lst1 lst2 lstoflst) str1 str2 =
    if (str1 `elem` lst1 && str2 `elem` lst2) then
        Just ((lstoflst !! (find lst1 str1 0)) !! find lst2 str2 0)
    else
        Nothing

-- the function that replace the element in the list given index
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- the function that replace the element in the 2d list given index
changeElem :: Int -> Int -> a -> [[a]] -> [[a]]
changeElem _ _ _ [] = []  -- Empty list case
changeElem 0 col x (l:ls) = replaceAtIndex col x l : ls  -- Reduce the column
changeElem row col x (l:ls) = l : changeElem (row - 1) col x ls  -- Reduce the row

changeElem_str :: String -> String -> Double -> HMMMatrix -> HMMMatrix
changeElem_str str1 str2 x (Matrix lst1 lst2 lstoflst) =
    Matrix lst1 lst2 (changeElem (find lst1 str1 0) (find lst2 str2 0) x lstoflst)


-- find the element return index if found, return -1 if not found
find :: (Eq t) => [t] -> t -> Int -> Int
find [] _ _ = -1
find (h : t) s n
    | h == s = n
    | otherwise = find t s (n+1)

-- initialize the matrix
initMatrix :: [String] -> [String] -> HMMMatrix
initMatrix lst1 lst2 = Matrix lst1 lst2 [[0.0 | _ <- lst2] | _ <- lst1]

-- split the elements into pairs of strings
split :: String -> (String, String)
split [] = ("","")
split str = helperfun str ""

helperfun :: String -> String -> (String, String)
helperfun "" _ = ("","")
helperfun (h:t) acc
    | h == ' ' = (acc, t)
    | otherwise = helperfun t (acc ++ [h])

-- Loads words from a text file into a list.
readtxt :: FilePath -> IO String
readtxt path = do
    contents <- readFile path
    let
        -- list of pairs: first is a word, second is the pos
        pair_lst = (map split (lines contents))

        -- words and pos lst (repetitive elements)
        words = map fst pair_lst
        pos = map snd pair_lst

        -- words and pos lst
        word_lst = newElem words []
        pos_lst = newElem pos []

        -- matrix initialization
        transition_matrix = initMatrix ("<S>": pos_lst) (pos_lst ++ ["<E>"])
        emission_matrix = initMatrix word_lst pos_lst

    return contents

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

        -- get word and pos counts as well
        wrd_c = newElemCount words [] []
        pos_c = newElemCount pos [] []

        word_counts = uncurry zip wrd_c
        pos_counts = uncurry zip pos_c

        -- words and pos lst
        word_lst = fst wrd_c
        pos_lst = fst pos_c

        -- get all possible combination of wrd and cat
        all_combos = [(x,y) | x<-word_lst, y<-pos_lst]

        -- matrix initialization
        transition_matrix = initMatrix ("<S>": pos_lst) (pos_lst ++ ["<E>"])
        empty_emission_matrix = initMatrix word_lst pos_lst

        emission_matrix = fillEmissionMatrix empty_emission_matrix pair_lst all_combos

    return (Model transition_matrix emission_matrix)

-- filter out redundant elements and count number of ocurrences
newElemCount :: Eq a => [a] -> [a] -> [Double] -> ([a],[Double])
newElemCount [] acc count = (acc,count)
newElemCount (h:t) acc count
    | h `elem` acc = newElemCount t acc (addIndex count (find acc h 0) 0)
    | otherwise = newElemCount t (h : acc) (1:count)

-- add to counts at specified index
addIndex :: [Double] -> Int -> Int ->  [Double]
addIndex (h:t) index acc
    | index == acc = h+1:t
    | otherwise = h: addIndex t index (acc+1)

-- filter out redundant elements
newElem :: Eq a => [a] -> [a] -> [a]
newElem [] acc = acc
newElem (h:t) acc
    | h `elem` acc = newElem t acc
    | otherwise = newElem t (h : acc)


fillEmissionMatrix :: HMMMatrix -> [(String,String)]-> [(String,String)] -> HMMMatrix
fillEmissionMatrix Empty _ _ = Empty
fillEmissionMatrix e_matrix _ [] = e_matrix
fillEmissionMatrix (Matrix lst1 lst2 lstoflst) pairs (p1:rest) = 
    let
        curr_emission_matrix = calculateEmission (Matrix lst1 lst2 lstoflst) (fst p1) (snd p1) pairs
    in
        fillEmissionMatrix curr_emission_matrix pairs rest

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


-- for small test
lst1= ["yes","no","jak", "pos"]
lst2=["adj","adv","n"]
ma = initMatrix lst1 lst2
pairs = [("yes","adj"),("no","adv"),("jak","n"), ("pos", "n")]
all_combo = [(x,y) | x<-lst1, y<-lst2]
-- ghci> fillEmissionMatrix ma pairs all_combo