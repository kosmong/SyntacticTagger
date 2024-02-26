
data HMMMatrix = Empty
        | Matrix [String] [String] [[Double]]
    deriving Show

-- get data from the matrix
getData :: HMMMatrix -> String -> String -> Maybe Double
getData Empty _ _ = Nothing
getData (Matrix lst1 lst2 lstoflst) str1 str2 = 
    if (str1 `elem` lst1 && str2 `elem` lst2) then
        Just ((lstoflst !! (find lst1 str1 0)) !! find lst2 str2 0)
    else
        Nothing 

-- find the element return index if found, return -1 if not found
find :: (Num t1, Eq t2) => [t2] -> t2 -> t1 -> t1
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

makeMatrixes :: FilePath -> IO (HMMMatrix, HMMMatrix)
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
        word_counts = newElemCount words [] []
        pos_counts = newElemCount pos [] []

        -- words and pos lst
        word_lst = fst word_counts
        pos_lst = fst pos_counts

        -- matrix initialization
        transition_matrix = initMatrix ("<S>": pos_lst) (pos_lst ++ ["<E>"])
        emission_matrix = initMatrix word_lst pos_lst
    
    return (transition_matrix, emission_matrix)

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


fillEmission :: HMMMatrix -> [(String,Double)] -> [(String,String)] -> IO HMMMatrix
fillEmission (Matrix wrds pos probs) pos_counts pairs = do
    -- for each category and word:
    -- get pairs with certain category from list of pairs
    -- count co-occurences of category and word
    -- get total occurences of category
    -- use the 2 counts to calculate emission probability of the co-ocurrence of a category and word
    --still need to map
    let 
        cat = snd (head pairs)
        wrd = fst (head pairs)
        catPairs = getCatLst pairs cat
        coOccur = filter (\x -> fst x == wrd) catPairs
        coCount = fromIntegral (length coOccur)
        catCount = snd (head pos_counts)
        prob = catCount / coCount
        modProbs = modifyMatrix probs 0 0 prob

    return (Matrix wrds pos modProbs)

-- change the value of M[i][j] in matrix
modifyMatrix :: [[Double]] -> Int -> Int -> Double -> [[Double]]
modifyMatrix [[]] _ _ _ = [[]]
modifyMatrix probs i j p =
    let splitRow = splitAt i probs
        rowsBefore = fst splitRow
        rowsAfter = drop 1 (snd splitRow)
        row = head (snd splitRow)
        splitCol = splitAt j row
        colsBefore = fst splitCol
        colsAfter = drop 1 (snd splitCol)
        changedRow = [colsBefore ++ [p] ++ colsAfter]
    in
        rowsBefore ++ changedRow ++ rowsAfter


getCatLst :: Eq a1 => [(a2, a1)] -> a1 -> [(a2, a1)]
getCatLst pairs cat = filter (\x -> snd x == cat) pairs