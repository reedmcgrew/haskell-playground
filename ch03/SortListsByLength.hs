-- Experiment with the Ordering Data Type
import Data.Ord
import Data.List

-- Comparison Function
comp :: [a] -> [a] -> Ordering
comp l1 l2
    | (length l1) > (length l2) = GT
    | (length l2) > (length l1) = LT
comp _ _ = EQ
                    
-- Lists sorting Function
sortLists l = sortBy comp l
