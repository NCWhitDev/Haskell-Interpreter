import Data.Char
import Data.List

type Vars = String
type TVars = String

type Constr = (Types,Types)
type Cxt = [(Vars,Types)]
type TSub = [(TVars,Types)]

data Types = Ints | Func Types Types | TVar TVars
    deriving (Show,Eq)
    
-- /\ ::= \/ | /\ /\ | \ \/ /\ | C Int | /\ - /\ | IfPos(/\,/\,/\) | Y
data Terms = Var Vars | App Terms Terms | Abs Vars Terms
    | Num Integer | Sub Terms Terms | IfPos Terms Terms Terms | Y
    deriving (Show,Eq)

data Token = VSym String | CSym Integer
    | SubOp | IfPositiveK | ThenK | ElseK | YComb
    | LPar | RPar | Dot | Backslash
    | Err String | PT Terms
    deriving (Eq,Show)

-- *************************************************************************************************************************************************************************

-- Step 1: Lexer
lexer :: String -> [Token]
lexer "" = []                               -- Empty String
lexer('(':xs) = LPar : lexer xs             -- Left par
lexer(')':xs) = RPar : lexer xs             -- Right par
lexer('.':xs) = Dot : lexer xs              -- Dot
lexer('\\':xs) = Backslash : lexer xs       -- Backslash
lexer('-':xs) = SubOp : lexer xs            -- The subtraction operator is represented by a minus.
lexer('Y':xs) = YComb : lexer xs            -- The Y-combinator is represented by Y.

lexer('i':'f':'P':'o':'s':'i':'t':'i':'v':'e':xs) = IfPositiveK : lexer xs  -- IfPositive
lexer('t':'h':'e':'n':xs) = ThenK : lexer xs                                -- Then
lexer('e':'l':'s':'e':xs) = ElseK : lexer xs                                -- Else

lexer (x:xs) | isSpace x = lexer xs                                                       -- if its a Space, we skip it. 
lexer (x:xs) | isLower x = VSym v : lexer r where (v,r) = span isAlpha(x:xs)              -- Uses isLower 'alphabet only', we create a token, then we grab alphabet char from string as long as they are alphabet chars. [VSym "v"]
lexer (x:xs) | isDigit x = CSym (read v) : lexer r where (v,r) = span isDigit(x:xs)       -- Uses isDigit 'Int characters only', we create a token (we read v : Which turns a char into int if able) we grab as long as its ints. [CYsm v]
lexer (_:xs) = [ Err (take 10 xs) ]                                                       -- Anything else is a Err token

-- *************************************************************************************************************************************************************************

-- shift–reduce helper function creates PT Tokens!!!
shift :: [Token] -> [Token] -> [Token]
shift (VSym x : s) q = shift(PT(Var x) : s) q                                                -- Variable rule onto stack
shift (CSym x : s) q = shift(PT(Num x) : s) q                                                -- Integer rule onto stack
shift (YComb : s) q = shift(PT(Y) : s) q                                                     -- Y Comb rule onto stack

shift (PT t3 : ElseK : PT t2 : ThenK : PT t1 : IfPositiveK : s) q = shift(PT(IfPos t1 t2 t3): s) q    -- if 3 then 1 else 1  _ + 2 ?? Is it (ifPos 3 then 1 else 3 + 2) Does else bind to just 3 or 3 + 2

shift (PT n1 : SubOp : PT n2 : s) q = shift(PT(Sub n2 n1): s) q                             -- Subtraction rule : Sub Terms Terms
shift (PT t2 : PT t1 : s) q = shift (PT(App t1 t2) : s) q                                   -- Application rule : App Terms Terms

shift (PT t : Dot : PT(Var x) : Backslash : s) q@(RPar : _) = shift (PT(Abs x t): s) q      -- Abstraction rule: Vars Terms         (as long as RPar)     (//x.x)3

shift (PT t : Dot : PT(Var x) : Backslash : s) [] = shift (PT(Abs x t): s) []               -- Abstraction rule: Vars Terms         (No more queue if its this so [])
shift (RPar : PT t : LPar : s) q = shift (PT t : s) q                                       -- Parenthesis type (x)
shift (Err e : s) q = [Err e]                                                               -- Shift–reduce Error
shift s (c:q) = shift (c:s) q                                                                -- Shifting, moving from queue to stack. Works for stuff that wont shift-reduce. We would still need to put them on the stack to make Err. 
shift s [] = s                                                                              -- Shift–reduce empty...return stack.

-- *************************************************************************************************************************************************************************
-- Step 2: Parser, Takes a list of tokens and attempts to produce a term out of them.
-- If no parse then go RIGHT (Error String)
-- If parse is possible then LEFT (Terms) 
parserCheck :: [Token] -> Either Terms String
parserCheck [PT x] = Left x                                                   -- Correctly parsed Terms!!!
parserCheck [Err x]  = Right ("Unable to correctly create a token: " ++ x)    -- Unable to make token in lexer :(
parserCheck xs = Right ("Unable to correctly parse list: " ++ show xs)        -- Unable to correctly parse list

-- *************************************************************************************************************************************************************************

-- Step 3: Implement the functions that preform substitutions on types, constraints and obtain all the variables occuring in types and contexts.
-- substituting type variable inside a type
-- HELPER FUNCTIONS

-- (Helper Function: TSub with types that outputs a type) type TSub = [(TVars,Types)]
typeSubst :: (TVars, Types) -> Types -> Types                                            -- Type substitution, get the most general types for each type.
typeSubst (tv,ty) Ints = Ints                                                            -- Pretty simple, just int. Thats the type
typeSubst (tv,ty) (Func t1 t2) = Func (typeSubst (tv,ty) t1) (typeSubst (tv,ty) t2)      -- We don't know func general types so we call typesubst on t1 and t2 to get them.
typeSubst (tv,ty) (TVar a) | tv == a = ty                                                -- When type variable is the same as our type variable then its the same type.
                           | otherwise = (TVar a)                                        -- otherwise they are not, we return our type variable

-- (Helper Function: TSub with constraints that outputs a constraint) substituting type variable inside a constraint                                        
constrSubst :: (TVars,Types) -> Constr -> Constr                                         -- type Constr = (Types,Types)
constrSubst (tv,ty) (lhs,rhs) = (typeSubst (tv,ty) lhs, typeSubst (tv,ty) rhs)           -- Constraint substitution, all you do is typesubst the left and right of Constr (Types, Types)


-- (Helper Function: get Type variables) The function extracting type variables from a type
getTVars :: Types -> [TVars]                                                             -- type TVars = String
getTVars Ints = []                                                                       -- Ints does not contain variables
getTVars (TVar a) = [a] 
getTVars (Func a b) = getTVars a ++ getTVars b                                           -- Func contains 2 variables: a b, we call getTvars on them and concat the list together


-- (Helper Function: get Type variables in context) The function extracting Type variables from a context
getTVarsCxt :: Cxt -> [TVars]                                                            -- type Cxt = [(Vars,Types)]
getTVarsCxt [] = []                                                                      -- When list is empty, we return.
getTVarsCxt ((v,a) : g) = getTVars a ++ getTVarsCxt g                                    -- Given context which is a list of tuples containing (Variables, types). We call geTvars on types and ...


-- *************************************************************************************************************************************************************************
-- !!! IMPORTANT !!

-- (Helper Function: All Variables) A list containing infinitely many variables
allVars :: [Vars]
allVars = tail vars where
  vars = "" : expand vars
  expand (v:vs) = map (\c -> v ++ [c]) ['a'..'z'] ++ expand vs

-- (Helper Function: Fresh new Variables) A function generating new variables outside the given context
freshVar :: [Vars] -> Vars
freshVar xs =
  let is = map (\x -> elemIndex x allVars) xs -- list of Maybe Int
      maxIndex [] = 0
      maxIndex (Nothing:ns) = maxIndex ns
      maxIndex (Just n:ns) = max n (maxIndex ns)
   in allVars !! (maxIndex is  + 1)

-- Step 4: Implement a function, that generates a list of constraints for a given context, term, and target type. 
genConstrs :: Cxt -> Terms -> Types -> [Constr]
genConstrs g (Var x) ty =  case lookup x g of            -- Look up variable context, https://zvon.org/other/haskell/Outputprelude/lookup_f.html
    Just b -> [(b,ty)]                                   -- If we find b (Var x) then we return a list containing (Var x, type) --> (x, Int)
    Nothing -> error ("Hey programmer, I could'nt find a variable in the context: " ++ x ++ "\n Gamma = " ++ show g)

genConstrs g (Num x) ty = [(Ints, ty)]                      -- Num is Ints type, we return a list containing a (Ints,type)

genConstrs g (App s t) ty =                                 -- Generated constraints for Application
    let tvars = getTVarsCxt g ++ getTVars ty                -- Our known contxt
        newType = freshVar tvars                            -- Newly created type not in context
        csm1 = genConstrs g s (Func (TVar newType) ty)      -- Generate constraints for A -> B
        cms2 = genConstrs g t (TVar newType)                -- Generate constraints for B
       in csm1 ++ cms2                                      -- new [Constr]

genConstrs g (Abs x t) (Func a b) =  genConstrs ((x,a) : g) t b                                  -- Generated constraints for Abstraction w/Function
genConstrs g (Abs x t) a =                                                                       -- Generated constraints for Abstraction, when it isnt a function but variables. 
    let tvars = getTVarsCxt g ++ getTVars a                                                       -- context of all known types
        nt1 = freshVar tvars                                                                     -- constructed new type thats not of context (tvars) -> into context
        nt2 = freshVar (nt1:tvars)                                                               -- constructed new type thats not of context (nt1 and tvars) -> into context
        g' = (x,(TVar nt1)) : g                                                                  -- new context
    in (a,(Func (TVar nt1) (TVar nt2))) : genConstrs g' t (TVar nt2)                             -- We determined (a, (Func (Tvar nt1)(Tvar nt2))) are of the same type! 
                                                                                                    -- | We call genConstrs on our new context with term t and type nt2 |

genConstrs g (Sub s t) ty = (ty,Ints) : genConstrs g s Ints ++ genConstrs g t Ints              -- Generated constraints for Subtraction, the type WILL be Ints

genConstrs g (IfPos s t u) ty =                                                                 -- Generated constraints for (IfPositive s then t else u)
    let tvars = getTVarsCxt g ++ getTVars ty                                                  -- context of all known types
        csm1 = genConstrs g s Ints                                                              -- Type Ints for s
        csm2 = genConstrs g t ty                                                                -- generated constraintd for t
        csm3 = genConstrs g u ty                                                                -- generated constraintd for u
    in csm1 ++ csm2 ++ csm3                                                                     -- new [Constr]

genConstrs g (Y) ty = 
    let tvars = getTVarsCxt g ++ getTVars ty
        a' = freshVar tvars
    in [(ty,(Func (Func (TVar a') (TVar a')) (TVar a')))]                                       -- same thing for tt and ff

-- *************************************************************************************************************************************************************************

-- !!! IMPORTANT !!
-- Step 5: Implment the functions below, 
-- The func unify takes a list of constraints and produces a type substitution, 
--  assigning type variables to types. It can raise an error in case of a circular constraint or type error. 
unify :: [Constr] -> [(TVars,Types)]
unify [] = []                                                                           -- When Constr list is empty we return empty list. We done.
unify ((lhs,rhs): xs) | lhs == rhs = unify xs                                           -- If left hand side type and right hand side type equal, unify list
unify ((TVar a, rhs) : xs)                                                      
    | elem a (getTVars rhs) = error ("This is a circular type!!!" ++ show a)            -- If Type varient a is a type in rhs then it is a circular type. A circular type is a type that requires itself in its definition.
    | otherwise = (a,rhs) : unify (map (constrSubst (a, rhs)) xs)                       -- otherwise we keep our type a and map our constraints if they are the same type.
unify ((lhs,TVar a) : cs) = unify ((TVar a,lhs):cs)
unify ((Func a1 b1, Func a2 b2) : xs) = unify ((a1,a2) : (b1,b2) : xs)
unify (c:cs) = error ("ERROR: This is a type error: " ++ show c)                         -- You tried to prepend c to [Constraints] and couldnt becuase of the type.


-- (Helper Function: grab from Tsub, get a type) applying a list of type substitutions, in sequence, to a given type 
-- type TSub = [(TVars,Types)]
tsubstList :: TSub -> Types -> Types                                                     -- 
tsubstList tsub t = foldl (flip typeSubst) t tsub

-- !!! IMPORTANT !!
-- The func infer takes a fully parsed PCF term, computes the most general type for that term! So cool! 
infer :: Terms -> Types
infer t = tsubstList(unify(genConstrs [] t (TVar "a"))) (TVar "a")                      
-- we take our term and generate constraints -> [Constr] --> unify -> [(TVars,Types)] --> tsubstList -> type: to get the most general type from our term.

-- *************************************************************************************************************************************************************************

-- (Helper Function: Free Variables) The function extracting term variables from a term 
freeVar :: Terms -> [Vars]
freeVar (Var x) = [x]
freeVar (App s t) = freeVar s ++ freeVar t
freeVar (Abs x t) = filter (/= x) (freeVar t)

-- Step 5: First implment subst, where subst (x,t) s replaces every occurrence of the variable x in the term s by the term t.
subst :: (Vars,Terms) -> Terms -> Terms
subst (x,t) (Var v) = if v == x then t else (Var v)                                         -- Var (substitute v), if v == x then its substituted as far it can be, otherwise we call substitute again
subst (x,t) (Num v) = t                                                                     -- Num?
subst (x,t) (App s1 s2) = App (subst (x,t) s1) (subst (x,t) s2)                             -- App (substitute s1) (substitute s2)

subst (x,t) (Abs y r)                                                                       -- Abs y (substitute r)
    | not (elem y (freeVar t)) = Abs y (subst (x,t) r)                                      -- If y is not a new variable , then we don't worry and subst r in Abs y (subst (x,t) r)
    | otherwise = let z = freshVar (x : y : freeVar t ++ freeVar r)                           -- If y is a newly created variable then, z is our newly created variable ????
                      r' = subst (y, Var z) r                                                 -- r' is = subsitution of (Vars, Terms) Terms -> ("y", Var z) r
                  in Abs z (subst (x,t) r')                                                   -- Now we have a newly created variable z and a newly made term r'. We then do substitute -> Abs z (subst (x,t) r')

subst (x,t) (Sub t1 t2) = Sub (subst (x,t) t1) (subst (x,t) t2)                             -- Sub ((substitute t1)(substitute t2)
subst (x,t) (IfPos t1 t2 t3) = IfPos (subst (x,t) t1) (subst (x,t) t2) (subst (x,t) t3)     -- IfPos (substitute t1) (substitute t2) (substitute t3)
subst (x,t) (Y) = t                                                                         -- YCombinator is constant

-- Implement reduction!!! That preforms one step of parrellel reduction, This function should recursively descend down the term until it finds a pattern that
--  matches the left side of one of the rewrite rules. See 4.2.1 - 4.2.2 for reduction rules :)
red :: Terms -> Terms                                                       -- We reduce terms here. We technically don't know what they are, but given a Term we can assume maybe something could be reduced.
red (Abs x t) = Abs x (red t)                                               -- Abstraction x (reduction on t):  //x.yx
red (Sub t1 t2) = Sub (red t1) (red t2)                                     -- Sub (reduction on t1) (reduction on t2)
red (IfPos t1 t2 t3) = IfPos (red t1) (red t2) (red t3)                     -- IfPositive (reduction on t1) (reduction on t2) (reduction on t3)
red (App s t) = App (red s) (red t)                                         -- Application (reduction on s) (reduction on t): (//x.x)(//y.y)
red (App (Abs x s) t) = subst (x,t) s                                       -- Application (Abstraction x s) t: 
red (App Y t) = App t (App Y t)                                             -- YCombinator 
red s = s                                                                   -- Can't be reduced anymore! Fully reduced! Stuff like Variables, Integers, etc.

-- (4.2.4. Multistep reduction) Reduction Recursively till end :)
redALL :: Terms -> Terms
redALL t = if red t == t then t else redALL t' where t' = (red t)           -- if reduction is complete then return t, otherwise we create a new version and call again.

-- *********************************************************************************************************

-- IO

