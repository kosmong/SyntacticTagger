-- This project will attempt to split a paragraph into its phrasal category.
-- Need events and states for HMM
-- Phrasal category is ['B-NP', 'I-NP', 'B-VP', 'B-PP', 'I-VP', 'O', 'B-SBAR', ß'B-ADJP', 'I-ADJP', 'B-ADVP', 'I-ADVP', 'I-PP', 'B-PRT', 'I-SBAR', 'B-CONJP', 'I-CONJP', 'B-INTJ', 'B-LST', 'I-LST']
-- States are the words

module SyntacticTagger where
-- import Data.HMM
import Control.Monad
import Data.Array
import System.IO

-- hmm2 = simpleHMM [1,2] "AGCT"

-- dnaArray = listArray (1,20) "AAAAGGGGCTCTCTCCAACC"
-- hmm4 = baumWelch hmm3 dnaArray 3

-- ma :: String -> String