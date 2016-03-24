module Interpreter
( SymTable(..)
, ListMap(..)
, numEval
) where

import Ast

class (Num a, Eq a) => Divisible a where
    divide :: a -> a -> a

instance Divisible Int where
    divide a b = a `div` b

instance Divisible Integer where
    divide a b = a `div` b

instance Divisible Float where
    divide a b = a / b

instance Divisible Double where
    divide a b = a / b

class SymTable m where
    start :: m a
    value :: m a -> String -> Either String a
    update :: m a -> String -> a -> m a

infixr 5 :=
data ListMap a = Empty | (String, a) := (ListMap a) deriving(Show)

instance SymTable ListMap where
    start = Empty
    
    value Empty key = Left $ "undefined variable"
    value (x := xs) key
        | (pass, val) <- x, pass == key = Right val
        | otherwise = value xs key
    
    update Empty key val = (key, val) := Empty
    update (x := xs) key val
        | (pass, _) <- x, pass == key = (key, val) := xs
        | otherwise = x := (update xs key val)


data TreeMap a = Leaf | Node (String, a) (TreeMap a) (TreeMap a) deriving(Show)

instance SymTable TreeMap where
    start = Leaf
    
    value Leaf key = Left $ "undefined variable"
    value (Node (pass, val) left right) key
        | pass == key = Right val
        | pass < key = value right key
        | otherwise = value left key
    
    update Leaf key val = Node (key, val) Leaf Leaf
    update (Node x@(pass, _) left right) key val
        | pass == key = Node (key, val) left right
        | pass < key = Node x left (update right key val)
        | otherwise = Node x (update left key val) right


{--
interpretCommand :: (SymTable m, Num a, Ord a) => m a 
    -> [a] 
    -> Command a 
    -> ((Either String [a]), m a, [a])
interpretCommand mem (x:xs) (Input var) = (Either [], update mem var x, xs)
interpretCommand mem input (Print var) = (value mem var, mem, input)
interpretCommand mem input (Seq []) = (Either [], mem, input)
interpretCommand mem input (Seq (x:xs))
    | (Either out :: [a]) <- output = concatRes out $ interpretCommand newMem inputLeft (Seq xs)
    | (Either err :: String) <- output = (output, newMem, inputLeft)
    where (output, newMem, inputLeft) = interpretCommand mem input xs
interpretCommand mem input (var `Assign` numExp) = (Either [], newMem, input)
    -- HAY que hacer handling en caso de que una variable no exista
    where newMem = update mem var (numEval mem numExp)
interpretCommand mem input (Cond [] []) = (Either [], mem, input)
interpretCommand mem input (Cond [] [y]) = interpretCommand mem input y
interpretCommand mem input (Cond (x:xs) (y:ys))
    | boolEval x = interpretCommand mem input y
    | otherwise = interpretCommand mem input (Cond xs ys)
interpretCommand _ _ (Cond _ _) = error("Condicional sin cuerpo")
interpretCommand mem input (Loop boolExp body)
    | not boolEval boolExp = (Either [], mem, input)
    | otherwise =
        case (output) of
        (Either out :: [a]) -> concatRes out $ interpretCommand newMem inputLeft body
        (Either err :: String) -> (output, newMem, inputLeft)
    where (output, newMem, inputLeft) = interpretCommand mem input body
--}

--declare Either as Monad

boolEval :: SymTable m => m a -> BoolExp a -> Bool
boolEval mem (NOT x) = 

numEval :: (Divisible a, SymTable m) => m a -> NumExp a -> Either String a
numEval _ (Const a) = Right a
numEval mem (Var a) = value mem a
numEval mem (a `Minus` b) = evalMonads (-) (numEval mem a) (numEval mem b)
numEval mem (a `Plus` b) = evalMonads (+) (numEval mem a) (numEval mem b)
numEval mem (a `Times` b) = evalMonads (*) (numEval mem a) (numEval mem b)
numEval mem (a `Div` b)
    | (Right 0) <- divisor = Left "division by zero"
    | otherwise = evalMonads divide (numEval mem a) divisor
    where divisor = (numEval mem b)

evalMonads :: Monad m => (a -> a -> a) -> m a -> m a -> m a
evalMonads f m1 m2 = do
    a <- m1
    b <- m2
    return $ f a b
    