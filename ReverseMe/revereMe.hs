-- Solution

reverseMe :: String -> String
reverseMe [] = []  -- When the string becomes empty, we return.
reverseMe xs = helperReverse xs -- Calls our helper to reverse our string

helperReverse :: [a] -> [a] -- A helper function to reverse out string. 
helperReverse [] = [] -- When the string becomes empty, we return.
helperReverse xs = last xs : helperReverse (init xs) -- recursivly calls the string, getting the last element each time. (init xs) returns the list without the last element each time.

-- We could also just use the function reverse but my making our own we get to see how it works :)
