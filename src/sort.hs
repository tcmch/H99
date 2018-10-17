quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted = quicksort [a | a <- xs, a <= x]
	    biggerSorted = quicksort [a | a <- xs, a > x]
	in smallerSorted ++ [x] ++ biggerSorted

mergesort :: (Ord a) => [a] -> [a]
mergesort [x] = [x]
mergesort [x, y] = if x>y then [y,x] else [x,y]
mergesort (xs) =
	let frontpart = mergesort (take ((length xs)/2) xs)
	    backpart = mergesort (drop ((length xs)/2) xs)
	in frontpart ++ backpart