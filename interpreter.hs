import Data.List

-- If no outer parentheses exist leave unchanged
removeOuterBrackets :: [Char] -> [Char]
removeOuterBrackets str 
  | beginning == "(" && end == ")"
    = middle
  | otherwise 
    = str
  where
    (beginning, headless)
      = splitAt 1 str
    (middle, end)
      = splitAt ((length headless) - 1) headless

bracket :: [Char] -> [Char]
bracket str 
  = "(" ++ str ++ ")"

data Term 
  = Var Char | Lambda Char Term | App Term Term
  deriving
    Eq

instance Show Term where
  show t
    = removeOuterBrackets (show' t)
    where 
      show' 
        = bracket . show'' 
      show'' (Lambda x (Var x'))
        | x == x' 
            = "I"
      show'' (Lambda x (Lambda y (Var x')))
        | x == x'
            = "K"
      show'' (Lambda x (Lambda y (Lambda z (App (App (Var x') (Var z')) (App (Var y') (Var z''))))))
        | x == x' && y == y' && z == z' && z' == z''
            = "S"
      show'' (Var var) 
        = [var]
      show'' (Lambda var t1)
        | head == "\\"
            = "\\" ++ [var] ++ tail
        | otherwise
            = "\\" ++ [var] ++ "." ++ str
        where
          str
            = removeOuterBrackets (show' t1)
          (head, tail) 
            = splitAt 1 str
      show'' (App (Var var) (Var var'))
        = [var] ++ [var']
      show'' (App t1 t2)
        -- An abstraction as the left-hand of the application should not be
        -- de-bracketed
        | str1 !! 1 == '\\' 
            = str1 ++ str2
        | otherwise 
            = (removeOuterBrackets str1) ++ str2
        where
          str1
            = show' t1
          str2 
            = debracketIfSingleton (show' t2)
          -- We don't need brackets around a single variable in the right hand
          -- side of an application
          -- Pre: expect str to be bracketed
          debracketIfSingleton str
            | length str == 3
                = removeOuterBrackets str
            | otherwise
                = str

i :: Term
i 
  = Lambda 'x' 
      (Var 'x')

k :: Term
k 
  = Lambda 'x' (Lambda 'y' (Var 'x'))

s :: Term 
s
  = Lambda 'x' (
      Lambda 'y' (
        Lambda 'z' 
          (App (App (Var 'x') (Var 'z')) (App (Var 'y') (Var 'z'))
        )
      )
    )

y :: Term
y 
  = Lambda 'f' (App t1 t1)
  where 
    t1 
      = Lambda 'x' (App (Var 'f') (App (Var 'x') (Var 'x')))

-- Returns a list of free variables in a term
freeVars :: Term -> [Char]
freeVars (Var var)
  = [var]
freeVars (App t1 t2) 
  = (freeVars t1) ++ (freeVars t2)
freeVars (Lambda var t1)
  = filter (/= var) (freeVars t1)

-- Given a list of variable names, return a fresh variable name. Number of
-- distinct chars is limited, so invalid when all variable names are exhaused.
-- We work with only lowercase alphabetical characters, to avoid confusion with
-- the shorhand combinators S, K and I
fresh :: [Char] -> Char
fresh 
  = head . (['a'..'z'] \\)

-- Replaces all instances of var with t2 in the given term
substitute :: Term -> Char -> Term -> Term
substitute (Var var') var t2
  | var' == var
    = t2
  | otherwise 
    = Var var' 
substitute (App t1 t1') var t2
  = App (substitute t1 var t2) (substitute t1' var t2)
substitute (Lambda var' t1) var t2
  -- Could return alpha-equivalent (Lambda var' t1) , but changing variable
  -- name makes the result of the substitution more explicit
  | var' == var
    = Lambda var (substitute t1 var' (Var var)) 
  | not (elem var' free)
    = Lambda var' (substitute t1 var t2)
    -- Construct alpha-equivalent function first, before making substitution
  | otherwise
    = substitute (Lambda new (substitute t1 var' (Var new))) var t2
    where
      free 
        = freeVars t2
      new 
        = fresh free

-- Attempt to reduce term to (Beta) normal form. Not guaranteed to terminate!
normalise :: Term -> Term
normalise (Var var)
  = (Var var)
normalise (Lambda var t1)
  = Lambda var (normalise t1)
normalise (App (Lambda var t1) t2)
  = normalise (substitute t1 var (normalise t2))
normalise (App t1 t2)
  | t1 /= r1 || t2 /= r2
    = normalise (App r1 r2)
  | otherwise 
    = App r1 r2
  where 
    [r1, r2] = map normalise [t1, t2]
