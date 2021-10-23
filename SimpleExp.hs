module SimpleExp where
import GHC.Natural (Natural, naturalFromInteger)
import System.Directory.Internal.Prelude (Ord)
import GHC.Num (Num)
data SimpleExp
    = N {n :: Natural}
    | Add SimpleExp SimpleExp
    | Times SimpleExp SimpleExp
    deriving (Eq)

instance Ord SimpleExp where
    e1 <= e2 = bigStep e1 <= bigStep e2

instance Num SimpleExp where
    e1 + e2         = Add e1 e2
    e1 * e2         = Times e1 e2
    abs e1          = e1
    signum e1       = 1
    fromInteger     = N . naturalFromInteger
    negate          = undefined 

instance Show SimpleExp where
    show (N n)                  = show n
    show (Add (N n1) (N n2))    = show n1 ++ " + " ++ show n2
    show (Add (N n) e)          = show n ++ " + (" ++ show e ++ ")"
    show (Add e (N n))          = "(" ++ show n ++ ") + " ++ show n
    show (Add e1 e2)            = "(" ++ show e1 ++ ") + (" ++ show e2 ++ ")"
    show (Times (N n1) (N n2))  = show n1 ++ " * " ++ show n2
    show (Times (N n) e)        = show n ++ " * (" ++ show e ++ ")"
    show (Times e (N n))        = "(" ++ show n ++ ") * " ++ show n
    show (Times e1 e2)          = "(" ++ show e1 ++ ") * (" ++ show e2 ++ ")"

bigStep :: SimpleExp -> Natural
bigStep (N n)           = n                        -- B-NUM
bigStep (Add e1 e2)     = bigStep e1 + bigStep e2  -- B_ADD
bigStep (Times e1 e2)   = bigStep e1 * bigStep e2  -- B_MULT

smallStep :: SimpleExp -> SimpleExp
smallStep (Add (N n1) (N n2))   = N (n1 + n2)               -- S-ADD
smallStep (Add (N n) e)         = Add (N n) (smallStep e)   -- S-RIGHT-ADD
smallStep (Add e1 e2)           = Add (smallStep e1) e2     -- S-LEFT-ADD
smallStep (Times (N n1) (N n2)) = N (n1 * n2)               -- S-MULT
smallStep (Times (N n) e)       = Times (N n) (smallStep e) -- S-RIGHT-MULT
smallStep (Times e1 e2)         = Times (smallStep e1) e2   -- S-LEFT-MULT
smallStep _                     = error "Normal form cannot reduce further"

ex = 3 + (2 + 1) :: SimpleExp
q1 = (4 + 1) + (2 + 2) :: SimpleExp