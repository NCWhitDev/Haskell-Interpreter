createToken :: String -> [Token]
createToken "" = []                                     -- Empty String
createToken('(':xs) = LPar : createToken xs             -- Left par
createToken(')':xs) = RPar : createToken xs             -- Right par
createToken('.':xs) = Dot : createToken xs              -- Dot
createToken('\':xs) = Backslash : createToken xs        -- Backslash
createToken('-':xs) = SubOp : createToken xs            -- The subtraction operator is represented by a minus.
createToken('Y':xs) = YComb : createToken xs            -- The Y-combinator is represented by Y.

createToken('i':'f':'P':'o':'s':'i':'t':'i':'v':'e':xs) = IfPositiveK : lexer xs  -- IfPositive
createToken('t':'h':'e':'n':xs) = ThenK : lexer xs                                -- Then
createToken('e':'l':'s':'e':xs) = ElseK : lexer xs                                -- Else

createToken (x:xs) | isSpace x = lexer xs                                                       -- if its a Space, we skip it. 
createToken (x:xs) | isLower x = VSym v : lexer r where (v,r) = span isAlpha(x:xs)              -- Uses isLower 'alphabet only', we create a token, then we grab alphabet char from string as long as they are alphabet chars. [VSym "v"]
createToken (x:xs) | isDigit x = CSym (read v) : lexer r where (v,r) = span isDigit(x:xs)       -- Uses isDigit 'Int characters only', we create a token (we read v : Which turns a char into int if able) we grab as long as its ints. [CYsm v]
createToken (_:xs) = [ Err (take 10 xs) ]                                                       -- Anything else is a Err token
