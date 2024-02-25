-- steps: load data, read file, new data type, calculate value


--compute emission probability
--enum of all the states
type WrdCat = ""
data taggedModel = [(String, WrdCat)]
data emissionTable = [[Fractional]]
--compute transition probability
-- get number of wrds with a tag
emission_prob :: taggedModel emissionTable -> emissionTable
emission_prob model em = 
    | [] _ = []
    | [(wrd, cat) : rest] em =
        let count = count cat model
        let wrdCatCount = count (wrd, cat) model
        let e_prob = wrdCatCount / count
            in
                emission_prob rest (em:e_prob)

count :: String -> taggedModel -> Fractional
count _ [] = 0
count wrd ((e, c):rest) =
    | wrd==c = 1 + count wrd rest
    | otherwise = count wrd rest
--Viterbi algorithm