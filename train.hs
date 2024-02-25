data Matrix = Empty
    | Matrix [String] [String] [[Double]]

-- get data from the matrix
getData :: Matrix -> String -> String -> Maybe Double
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

init_Matrix lst1 lst2 = Matrix lst1 lst2 [[0.0 | _ <- lst2] | _ <- lst1]

split :: String -> (String, String)
split [] = ("","")
split str = helperfun str ""

helperfun :: String -> String -> (String, String)
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
        transition_matrix = init_Matrix pos_lst pos_lst
        emission_matrix = init_Matrix word_lst pos_lst
    
    return contents

new_elem [] acc = []
new_elem (h:t) acc 
    | h `elem` acc = new_elem t acc
    | otherwise = new_elem t (h : acc)