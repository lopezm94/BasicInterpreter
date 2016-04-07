module Interpreter
( SymTable(..)
, ListMap(..)
, TreeMap(..)
, interpretCommand
, interpretProgram
, evalMonads
, boolEval
, numEval
, seqExecute
, mydiv
) where

import Ast

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

interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input comms = res
    where (res, _, _) = interpretCommand Leaf input comms 

interpretCommand :: (Num a, Ord a, SymTable m) => m a 
    -> [a] 
    -> Command a 
    -> ((Either String [a]), m a, [a])
interpretCommand mem (x:xs) (Input var) = (Right [], update mem var x, xs)
interpretCommand mem input (Print var)
    | (Left err) <- eval = (Left err, mem, input)
    | (Right val) <- eval = (Right [val], mem, input)
    where eval = value mem var
interpretCommand mem input (Seq []) = (Right [], mem, input)
interpretCommand mem input (Seq (x:xs)) = seqExecute mem input x (Seq xs)
interpretCommand mem input (var `Assign` numExp)
    | (Left err) <- eval = (Left err, mem, input)
    | (Right val) <- eval = (Right [], update mem var val, input)
    where eval = numEval mem numExp
interpretCommand mem input (Cond [] []) = (Right [], mem, input)
interpretCommand mem input (Cond [] [y]) = interpretCommand mem input y
interpretCommand mem input (Cond (x:xs) (y:ys))
    | (Left err) <- cond = (Left err, mem, input)
    | (Right True) <- cond = interpretCommand mem input y
    | otherwise = interpretCommand mem input (Cond xs ys)
    where cond = boolEval mem x
interpretCommand _ _ (Cond _ _) = error("Conditional w/o body")
interpretCommand mem input (Loop boolExp body)
    | (Left err) <- cond = (Left err, mem, input)
    | (Right False) <- cond = (Right [], mem, input)
    | otherwise = seqExecute mem input body (Loop boolExp body)
    where cond = boolEval mem boolExp

seqExecute :: (Num a, Ord a, SymTable m) => m a
    -> [a] 
    -> Command a
    -> Command a
    -> ((Either String [a]), m a, [a])
seqExecute mem input act next =
    let (output, actMem, actInput) = interpretCommand mem input act
        (nextOutput, nextMem, nextInput) = interpretCommand actMem actInput next
    in  case (output) of
        (Left err) -> (Left err, actMem, actInput)
        (Right _) -> (evalMonads (++) output nextOutput, nextMem, nextInput)


boolEval :: (Num a, Ord a, SymTable m) => m a -> BoolExp a -> Either String Bool
boolEval mem (NOT x) = evalMonads (\x y -> not y) (Right True) (boolEval mem x)
boolEval mem (x `OR` y) = evalMonads (||) (boolEval mem x) (boolEval mem y)
boolEval mem (x `AND` y) = evalMonads (&&) (boolEval mem x) (boolEval mem y)
boolEval mem (x `Gt` y) = evalMonads (>) (numEval mem x) (numEval mem y)
boolEval mem (x `Eq` y) = evalMonads (==) (numEval mem x) (numEval mem y)

numEval :: (Num a, Ord a, SymTable m) => m a -> NumExp a -> Either String a
numEval _ (Const x) = Right x
numEval mem (Var x) = value mem x
numEval mem (x `Minus` y) = evalMonads (-) (numEval mem x) (numEval mem y)
numEval mem (x `Plus` y) = evalMonads (+) (numEval mem x) (numEval mem y)
numEval mem (x `Times` y) = evalMonads (*) (numEval mem x) (numEval mem y)
numEval mem (x `Div` y)
    | (Right 0) <- divisor = Left "division by zero"
    | otherwise = evalMonads mydiv (numEval mem x) divisor
    where divisor = (numEval mem y)

evalMonads :: Monad m => (a -> a -> b) -> m a -> m a -> m b
evalMonads f m1 m2 = do
    a <- m1
    b <- m2
    return $ f a b
    
mydiv :: (Num a, Ord a) => a -> a -> a
mydiv x y = 
    let
        absx = abs x
        absy = abs y
        sign = (signum x) * (signum y)
        zero = x-x
        unit = abs (signum y)
    in  sign * (mycount zero unit $ takeWhile (absy<=) $ iterate (flip (-) absy) absx)

mycount  :: (Num a, Ord a) => a -> a -> [a] -> a
mycount zero unit list = foldl (\acc _ -> acc+unit) zero list