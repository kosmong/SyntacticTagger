module ReadWriteModel where
import Train
writeModel :: IO HMMModel -> FilePath -> IO ()
-- writeModel None _ = return "There is no model"
writeModel modelIO path = do
    model <- modelIO
    let
        modelstr = show model
    writeFile path modelstr

readModel :: FilePath -> IO HMMModel
readModel path = do
    model <- readFile path
    let 
        (t_string, e_string) = findMatrixInString model
        transition_matrix = stringToMatrix t_string
        emission_matrix = stringToMatrix e_string

    return (Model transition_matrix emission_matrix)

stringToMatrix :: String -> HMMMatrix
stringToMatrix str =
    let
        split1 = splitAt (findString str "] " 0 + 2) str
        l1 = filter (/= ",") (stringToStringArray (init (fst split1)) "\"")
        lst1 = init (tail l1)

        split2 = splitAt (findString (snd split1) "] " 0 + 2) (snd split1)
        l2 = filter (/= ",") (stringToStringArray (init (fst split2)) "\"")
        lst2 = init (tail l2)

        lstl = map (drop 2) (stringToStringArray (init (init (snd split2))) "],")
        lsofls = map (`stringToStringArray` ",") lstl
        lstoflst = map (map (\x -> read x :: Double)) lsofls

    in
        Matrix lst1 lst2 lstoflst

findMatrixInString :: String -> (String,String)
findMatrixInString str =
    let
        t_matrix_start = (findString str " (Matrix " 0) + (length " (Matrix ")
        starts = snd (splitAt t_matrix_start str)
        e_matrix_start = findString starts " (Matrix " 0
        matrixes = splitAt e_matrix_start starts
        t_matrix_str = init (fst matrixes)
        e_matrix_str = init (drop (length " (Matrix ") (snd matrixes))
    in
    (t_matrix_str, e_matrix_str)

-- return index right before target string
findString :: String -> String -> Int -> Int
findString "" target _ = error "word not found"
findString str target i
    | take (length target) str == target = i
    | otherwise = findString (tail str) target i+1

stringToStringArray :: String -> String -> [String]
stringToStringArray "" _ = []
stringToStringArray str splitter =
    let
        w = findSeparated str splitter
        new_str = drop (length w+1) str
    in
        w : stringToStringArray new_str splitter

findSeparated :: String -> String -> String
findSeparated "" _ = ""
findSeparated str splitter
    | take (length splitter) str == splitter = ""
    | otherwise = head str : findSeparated (tail str) splitter

getPosOrder :: FilePath -> IO [String]
getPosOrder path = do
    contents <- readFile path
    let
        sentences = lines contents
        -- list of pairs: first is a word, second is the pos
        pair_lst = map split sentences

        -- words and pos lst (repetitive elements)
        words = map fst pair_lst
        pos = map snd pair_lst

        start_end_pos = "<S>" : putStartsEnds pos
    return start_end_pos

putStartsEnds :: [String] -> [String]
putStartsEnds [] = ["<E>"]
putStartsEnds (h:tail)
    | h == "" = "<E>":("<S>":putStartsEnds tail)
    | otherwise = h:putStartsEnds tail