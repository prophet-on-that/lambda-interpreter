data Term 
  = Var Char | Lambda Char Term | App Term Term
  deriving
    Show

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

-- Returns a list of free variables in a term
freeVars :: Term -> [Char]
freeVars (Var var)
  = [var]
freeVars (App t1 t2) 
  = (freeVars t1) ++ (freeVars t2)
freeVars (Lambda var t1)
  = filter (/= var) (freeVars t1)

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
  | not (elem var' (freeVars t2))
    = Lambda var' (substitute t1 var t2)

-- Beta-reduction of a term
reduce :: Term -> Term
reduce (App (Lambda var t1) t2)
  = substitute t1 var t2
reduce t1
  = t1
