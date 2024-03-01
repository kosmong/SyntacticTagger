module ReadWriteModel where
import Train

{-
Write the given HMMModel into a text file of the given path
-}
writeModel :: IO HMMModel -> FilePath -> IO ()
writeModel modelIO path = do
    model <- modelIO
    let
        modelstr = show model
    writeFile path modelstr

{-
Read the HMMModel from text file of the given path
-}
readModel :: FilePath -> IO HMMModel
readModel path = do
    model <- readFile path
    let 
        (t_string, e_string) = findMatrixInString model
        transition_matrix = stringToMatrix t_string
        emission_matrix = stringToMatrix e_string

    return (Model transition_matrix emission_matrix)

{-
Convert the given string into an HMMMatrix
-}
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


{-
Find the 2 strings of the emission and transition matrix from given string
-}
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

{-
Return index right before target string
-}
findString :: String -> String -> Int -> Int
findString "" target _ = error (target ++ "not found")
findString str target i
    | take (length target) str == target = i
    | otherwise = findString (tail str) target i+1

{-
Convert given string into an array of strings given a splitter string
-}
stringToStringArray :: String -> String -> [String]
stringToStringArray "" _ = []
stringToStringArray str splitter =
    let
        w = findSeparated str splitter
        new_str = drop (length w + 1) str
    in
        w : stringToStringArray new_str splitter

{-
Find the first string before the first splitter
-}
findSeparated :: String -> String -> String
findSeparated "" _ = ""
findSeparated str splitter
    | take (length splitter) str == splitter = ""
    | otherwise = head str : findSeparated (tail str) splitter