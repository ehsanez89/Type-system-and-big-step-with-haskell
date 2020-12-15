module ExpLang

where

--Expr
data Expr = TRUE 
         | FALSE 
         | IfThenElse Expr Expr Expr 
         | Zero 
         | Succ Expr
         | Pred Expr 
         | IsZero Expr deriving (Eq, Ord)




--Instance Show         
instance Show Expr where
    show TRUE = "true"
    show FALSE = "false"
    show (IfThenElse e e1 e2) = "if "++show e++" then "++show e1++" else "++show e2
    show Zero = "0"
    show (Succ t) = "succ "++show t
    show (Pred t) = "pred "++show t
    show (IsZero t) = "iszero "++show t



--Declare isNum
isNum :: Expr -> Bool
isNum Zero     = True
isNum (Succ n) = isNum n
isNum _        = False




--Function BigStep
bigStep :: Expr -> Expr
bigStep TRUE = TRUE
bigStep FALSE = FALSE
bigStep Zero = Zero
bigStep (Succ e) | n <- bigStep e , isNum n = Succ n
bigStep (Pred e) | Zero <- bigStep e = Zero
bigStep (Pred e) | Succ n <- bigStep e , isNum n = n
bigStep (IsZero e) | Zero <- bigStep e = TRUE
bigStep (IsZero e) | Succ n <- bigStep e , isNum n = FALSE
bigStep (IfThenElse e0 e1 e2) | TRUE <- bigStep e0 = bigStep e1
bigStep (IfThenElse e0 e1 e2) | FALSE <- bigStep e0 = bigStep e2


--Type
data Type = NatType | BoolType deriving (Eq)



--Check whether expressions are wellTyped or not
wTyped :: Expr -> Bool
wTyped e | Just _ <- typeOf e = True
wTyped e | otherwise = False



--Types of expressions
typeOf :: Expr -> Maybe Type
typeOf TRUE = Just BoolType
typeOf FALSE = Just BoolType
typeOf Zero = Just NatType
typeOf (Succ e) | Just NatType <- typeOf e = Just NatType
typeOf (Pred e) | Just NatType <- typeOf e = Just NatType
typeOf (IsZero e) | Just NatType <- typeOf e = Just BoolType
typeOf (IfThenElse e0 e1 e2) | Just BoolType <- typeOf e0, Just t1 <- typeOf e1, Just t2 <- typeOf e2, (t1) == (t2) = Just t1
typeOf _ = Nothing

