score :: Float -> Float -> Int
score x y
  | distance <= 1 = 10
  | distance <= 5 = 5
  | distance <= 10 = 1
  | otherwise = 0
  where
    distance = sqrt(x * x + y * y)

-- ===============================================================

-- So we take in a Floating number and a Floating number that outputs a Int.
-- The first thing we do is get the radius of both x and y then sqrt the total of the both of them. 
    -- sqrt is a function in haskell http://www.zvon.org/other/haskell/Outputprelude/sqrt_f.html, Takes in a floating point number and returns its squared version.
    -- EX: sqrt 25 = 5 because 5 * 5 = 25
-- We then store this in distance and comapre it to our cases above. 
