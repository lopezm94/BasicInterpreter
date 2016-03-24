module Ast
( Ident
, Command(..)
, BoolExp(..)
, NumExp(..)
, readCommand
, parseNumExp
) where


type Ident = String

data NumExp a =
    Const a |
    Var Ident |
    (NumExp a) `Div` (NumExp a) |
    (NumExp a) `Times` (NumExp a) |
    (NumExp a) `Plus` (NumExp a) |
    (NumExp a) `Minus` (NumExp a)

instance Show a => Show (NumExp a) where
    show (Const x) = show x
    show (Var x) = x
    show (ex1 `Div` ex2) = show ex1 ++ " / " ++ show ex2
    show (ex1 `Times` ex2) = show ex1 ++ " * " ++ show ex2
    show (ex1 `Plus` ex2) = show ex1 ++ " + " ++ show ex2
    show (ex1 `Minus` ex2) = show ex1 ++ " - " ++ show ex2


data BoolExp a =
    NOT (BoolExp a) |
    (BoolExp a) `OR` (BoolExp a) |
    (BoolExp a) `AND` (BoolExp a) |
    (NumExp a) `Gt` (NumExp a) |
    (NumExp a) `Eq` (NumExp a)

instance Show a => Show (BoolExp a) where
    show (NOT ex) = "NOT " ++ show ex
    show (ex1 `OR` ex2) = show ex1 ++ " OR " ++ show ex2
    show (ex1 `AND` ex2) = show ex1 ++ " AND " ++ show ex2
    show (ex1 `Gt` ex2) = show ex1 ++ " > " ++ show ex2
    show (ex1 `Eq` ex2) = show ex1 ++ " = " ++ show ex2


data Command a =
    Input Ident |
    Print Ident |
    Seq [Command a] |
    Ident `Assign` (NumExp a) |
    Cond [BoolExp a] [Command a] |
    Loop (BoolExp a) (Command a)

instance Show a => Show (Command a) where
    show = prettyPrint 0

prettyPrint :: Show a => Int -> Command a -> String
prettyPrint n (Input var) = ident n ++ "INPUT " ++ var ++ ";\n"
prettyPrint n (Print var) = ident n ++ "PRINT " ++ var ++ ";\n"
prettyPrint n (var `Assign` ex) = ident n ++ var ++ " := " ++ show ex ++ ";\n"
prettyPrint n (Seq xs) = concatMap (prettyPrint n) xs
prettyPrint n (Cond x y) = printIfClauses n x y True
prettyPrint n (Loop ex command) = header ++ body ++ end
    where
        header = ident n ++ "WHILE " ++ show ex ++ "\n"++ ident n ++ "DO\n"
        body = prettyPrint (n+1) command
        end = ident n ++ "END\n"

printIfClauses :: Show a => Int -> [BoolExp a] -> [Command a] -> Bool -> String
printIfClauses n [] [] False = ident n ++ "END\n"
printIfClauses n [] (command:[]) False = header ++ body ++ nextClauses
    where
        header = ident n ++ "ELSE\n"
        body = prettyPrint (n+1) command
        nextClauses = (printIfClauses :: Int -> [BoolExp ()] -> [Command ()] -> Bool -> String) n [] [] False
printIfClauses n (x:xs) (command:ys) False = header ++ body ++ nextClauses
    where
        header = ident n ++ "ELIF " ++ show x ++ " THEN\n"
        body = prettyPrint (n+1) command
        nextClauses = printIfClauses n xs ys False
printIfClauses n (x:xs) (command:ys) True = header ++ body ++ nextClauses
    where
        header = ident n ++ "IF " ++ show x ++ " THEN\n"
        body = prettyPrint (n+1) command
        nextClauses = printIfClauses n xs ys False

ident :: Int -> String
ident n = replicate (2*n) ' '




grabThisBody :: Int -> [String] -> ([String],[String])
grabThisBody 0 ("END":xs) = ([],[])
grabThisBody 0 ("ELIF":xs) = ([],"ELIF":xs)
grabThisBody 0 ("ELSE":xs) = ([],"ELSE":xs)
grabThisBody n ("END":xs) = ("END":ys, zs)
    where (ys, zs) = grabThisBody (n-1) xs
grabThisBody n (x:xs)
    | x == "IF" || x == "WHILE" = (x:ys, zs)
    where (ys, zs) = grabThisBody (n+1) xs
grabThisBody n (x:xs) = (x:ys,zs)
    where (ys, zs) = grabThisBody n xs

grabCondition :: Int -> [String] -> ([String],[String])
grabCondition 0 ("END":xs) = (["END"],xs)
grabCondition n ("END":xs) = ("END":ys, zs)
    where (ys, zs) = grabCondition (n-1) xs
grabCondition n (x:xs)
    | x == "IF" || x == "WHILE" = (x:ys, zs)
    where (ys, zs) = grabCondition (n+1) xs
grabCondition n (x:xs) = (x:ys,zs)
    where (ys, zs) = grabCondition n xs

grabInstruction :: [String] -> ([String],[String])
grabInstruction prog = (instruction, rest)
    where (instruction, (";":rest)) = span (/=";") prog

parseNumExp :: Read a => [String] -> NumExp a
parseNumExp xs
    | not (null rest) && head rest == "+" = parsedLeftNumExp `Plus` (parseNumExp (tail rest))
    | not (null rest) && head rest == "-" = parsedLeftNumExp `Minus` (parseNumExp (tail rest))
    where
        notNumOp x = x/="+" && x/="-"
        (leftNumExp, rest) = span notNumOp xs
        parsedLeftNumExp = parseNumExp leftNumExp
parseNumExp xs
    | not (null rest) && head rest == "*" = parsedLeftNumExp `Times` (parseNumExp (tail rest))
    | not (null rest) && head rest == "/" = parsedLeftNumExp `Div` (parseNumExp (tail rest))
    where
        notNumOp x = x/="*" && x/="/"
        (leftNumExp, rest) = span notNumOp xs
        parsedLeftNumExp = parseNumExp leftNumExp
parseNumExp (num@(digit:rest):[])
    | '0' <= digit && digit <= '9' = Const $ read num
parseNumExp (x:[]) = Var x

parseBoolExp :: Read a => [String] -> BoolExp a
parseBoolExp ("NOT":xs) = NOT (parseBoolExp xs)
parseBoolExp xs
    | not (null rest) && head rest == "OR" = parsedLeftBoolExp `OR` (parseBoolExp (tail rest))
    | not (null rest) && head rest == "AND" = parsedLeftBoolExp `AND` (parseBoolExp (tail rest))
    where
        notBoolOp x = x/="OR" && x/="AND"
        (leftBoolExp, rest) = span notBoolOp xs
        parsedLeftBoolExp = parseBoolExp leftBoolExp
parseBoolExp xs
    | not (null rest) && head rest == ">" = parsedLeftNumExp `Gt` (parseNumExp (tail rest))
    | not (null rest) && head rest == "=" = parsedLeftNumExp `Eq` (parseNumExp (tail rest))
    where
        notBoolOp x = x/=">" && x/="="
        (leftNumExp, rest) = span notBoolOp xs
        parsedLeftNumExp = parseNumExp leftNumExp

parse :: Read a => [String] -> Command a
parse [] = Cond [] []
parse ("IF":xs) = Cond (thisExpression : ys) (thisCommand : zs)
    where
        (expression, ("THEN":rest)) = span (/="THEN") xs
        (thisBody, nextClause) = grabThisBody 0 rest
        thisExpression = parseBoolExp expression
        thisCommand = Seq $ map parse $ blocks thisBody
        (Cond ys zs) = parse nextClause
parse ("ELIF":xs) = Cond (thisExpression : ys) (thisCommand : zs)
    where
        (expression, ("THEN":rest)) = span (/="THEN") xs
        (thisBody, nextClause) = grabThisBody 0 rest
        thisExpression = parseBoolExp expression
        thisCommand = Seq $ map parse $ blocks thisBody
        (Cond ys zs) = parse nextClause
parse ("ELSE":xs) = Cond [] $ [Seq (map parse (blocks (init xs)))]
parse ("WHILE":xs) = Loop (parseBoolExp expression) $ Seq $ map parse $ blocks $ init rest
    where (expression, ("DO":rest)) = span (/="DO") xs
parse (x:y:xs)
    | x == "INPUT" = Input y
    | x == "PRINT" = Print y
    | y == ":=" = Assign x $ parseNumExp xs


blocks :: [String] -> [[String]]
blocks [] = []
blocks prog@(x:xs)
    | x == "IF" =
        let (block, rest) = grabCondition 0 xs
        in ("IF" : block) : blocks rest
    | x == "WHILE" =
        let (block, rest) = grabCondition 0 xs
        in ("WHILE" : block) : blocks rest
    | otherwise =
        let (block, rest) = grabInstruction prog
        in block : blocks rest

putCol :: [String] -> [String]
putCol [] = []
putCol (x:"END":xs)
    | x /= ";" && x /= "END" = x : ";" : "END" : putCol xs
putCol (x:xs) = x : putCol xs

separateCol :: String -> String
separateCol [] = []
separateCol (';':xs) = ' ' : ';' : separateCol xs
separateCol (x:xs) = x : separateCol xs

prepare :: String -> [String]
prepare = putCol . words . separateCol

readCommand :: Read a => String -> Command a
readCommand input = Seq $ map parse $ blocks $ prepare input