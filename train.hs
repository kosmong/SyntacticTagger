data Matrix = Empty
        | Matrix [String] [String] [[Double]]
    deriving Show

-- get data from the matrix
getData :: Matrix -> String -> String -> Maybe Double
getData Empty _ _ = Nothing
getData (Matrix lst1 lst2 lstoflst) str1 str2 = 
    if (str1 `elem` lst1 && str2 `elem` lst2) then
        Just ((lstoflst !! (find lst1 str1 0)) !! find lst2 str2 0)
    else
        Nothing

-- the function that rep;ace the element in the list given index
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- the function that replace the element in the 2d list given index
changeElem :: Int -> Int -> a -> [[a]] -> [[a]]
changeElem _ _ _ [] = []  -- Empty list case
changeElem 0 col x (l:ls) = replaceAtIndex col x l : ls  -- Reduce the column
changeElem row col x (l:ls) = l : changeElem (row - 1) col x ls  -- Reduce the row

changeElem_str :: String -> String -> Double -> Matrix -> Matrix
changeElem_str str1 str2 x (Matrix lst1 lst2 lstoflst) = 
    Matrix lst1 lst2 (changeElem (find lst1 str1 0) (find lst2 str2 0) x lstoflst)
    

-- find the element return index if found, return -1 if not found
find :: (Eq t) => [t] -> t -> Int -> Int
find [] _ _ = -1
find (h : t) s n 
    | h == s = n
    | otherwise = find t s (n+1)

-- initialize the matrix
init_Matrix :: [String] -> [String] -> Matrix
init_Matrix lst1 lst2 = Matrix lst1 lst2 [[0.0 | _ <- lst2] | _ <- lst1]

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
        word_lst = new_elem words []
        pos_lst = new_elem pos []

        -- matrix initialization
        transition_matrix = init_Matrix ("<S>": pos_lst) (pos_lst ++ ["<E>"])
        emission_matrix = init_Matrix word_lst pos_lst
    
    return contents

make_matrixes :: FilePath -> IO (Int, Int)
make_matrixes path = do
    contents <- readFile path
    let 
        sentences = lines contents
        -- list of pairs: first is a word, second is the pos
        pair_lst = (map split sentences)

        -- words and pos lst (repetitive elements)
        words = map fst pair_lst
        pos = map snd pair_lst

        -- words and pos lst
        word_lst = new_elem words []
        pos_lst = new_elem pos []

        -- matrix initialization
        transition_matrix = init_Matrix ("<S>": pos_lst) (pos_lst ++ ["<E>"])
        emission_matrix = init_Matrix word_lst pos_lst
    
    return (length words, length word_lst)

-- filter out redundant elements
new_elem :: Eq a => [a] -> [a] -> [a]
new_elem [] acc = acc
new_elem (h:t) acc 
    | h `elem` acc = new_elem t acc
    | otherwise = new_elem t (h : acc)