quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        largerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ largerSorted