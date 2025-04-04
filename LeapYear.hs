-- A leap year (in the Gregorian calendar) occurs:
-- When a year is divisble by 4
-- Unless the year is divisible by 100? In tha case its not a leap year unless its divisble by 400!
-- Hint: 2000 was a leap year!

-- Create a leap year function to calcualte what is and whats not.
isLeapYear :: Integer -> Bool
isLeapYear x 
  | mod x 400 == 0 = True -- We first check to see if our leap year is modable by 400. If so then it is a leap year.
  | mod x 100 == 0 = False -- If the above is False then we check to see if its modable by 100. If so it is NOT a leap year! 
  | mod x 4 == 0 = True -- However if the above is False then we check to see if it is a leap year again. If the calcualtion equals 0 then it is a leap year!
  | otherwise = False -- If none are True then its not a leap year confirmed. 
-- =================================================================================================================================================

Explanation : I used guards here because of the mutiple cases. I use the mod function here to calculate the year. There are many ways to use mod in Haskell.
here are 2 ways you can use it.

mod x y -- You use it as a function with 2 arguments, the number "x" you want to mod and "y" the number you want to mod by.
x `mod` y -- You can also use it as a infix notation, it does the same thing. Imagine it like when you do "x + y" or "x - y"


Here are some visual examples!!!!

EX1: 2000 % 400 = 0 (Confirmed Leap year!!!)
 
EX2: 1987 % 400 = 387...
     1987 % 100 = 87...
     1987 % 4 = 3... (Confimred NOT a Leap year)

EX3: 2025 % 400 = 25...
     2025 % 100 = 25...
     2025 % 4 = 1... (Confirmed NOT a Leap year)

EX4: 1900 % 400 = 300..
     1900 % 100 = 0 ... (Oh maybe its not a leap year?)
     1900 % 4 = 0 (Oh wait it is a Leap year!!! Gotta be carful with when you do your cases to avoid checking the wrong thing at the wrong time!)
