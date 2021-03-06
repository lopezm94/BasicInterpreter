module CopyDetection
( expand
, eraseInstructions
, simplify
, injectInto
, fixFormat
) where

import Ast

--Doesn't support ELIF notation.

isomorfic :: Command a -> Command a -> Bool
(Seq []) `isomorfic` (Seq []) = True
(Loop _ a) `isomorfic` (Loop _ b) = a `isomorfic` b
(Cond _ [a]) `isomorfic` (Cond _ [b]) = a `isomorfic` b
(Cond _ [a,b]) `isomorfic` (Cond _ [c,d]) =
    (a `isomorfic` c && b `isomorfic` d) || (a `isomorfic` d && b `isomorfic` c)
x `isomorfic` y = False

instance Eq a => Eq (Command a) where
    x == y = (simplify x) `isomorfic` (simplify y)

expand :: Command a -> Command a
expand (Seq (x:xs)) =
    let (Seq xspanded) = expand (Seq xs)
    in  Seq $ (expand x) : xspanded
expand (Cond [cond] [body])
    | (a `OR` b) <- cond =
        Cond [a] [expandedBody, Seq [expand (Cond [b] [expandedBody])]]
    | (a `AND` b) <- cond =
        let newBody = Seq $ [expand (Cond [b] [expandedBody])]
        in  Cond [a] [newBody]
    | otherwise = Cond [cond] [expandedBody]
    where expandedBody = expand body
expand (Cond [cond] [body, bodyElse])
    | (a `OR` b) <- cond =
        let expandedElse = expand bodyElse
        in  Cond [a] [expandedBody, Seq [expand (Cond [b] [expandedBody, expandedElse])]]
    | (a `AND` b) <- cond =
        let newBody = Seq $ [expand (Cond [b] [expandedBody])]
        in  Cond [a] [newBody]
    | otherwise = Cond [cond] [expandedBody]
    where expandedBody = expand body

expand (Loop cond body) = Loop cond $ expand body
expand command = command

simplify :: Command a -> Command a
simplify command = foldr injectInto (Seq []) list
    where (Seq list) = eraseInstructions $ fixFormat command

--Transform to a format understood by erase and inject functions
fixFormat :: Command a -> Command a
fixFormat (Seq []) = Seq []
fixFormat (Seq (x:rest))
    | (Seq stuff) <- x =
        let (Seq act) = fixFormat x
        in  Seq (act ++ elems)
    | otherwise = Seq ((fixFormat x) : elems)
    where (Seq elems) = fixFormat $ Seq rest
fixFormat (Loop cond body) = Loop cond $ fixFormat $ Seq [body]
fixFormat (Cond conds bodies) = Cond conds $ map fixFormat $ map Seq $ map (:[]) bodies
fixFormat instruction = instruction

eraseInstructions :: Command a -> Command a
eraseInstructions (Seq []) = Seq []
eraseInstructions (Seq (x:xs))
    | (Cond conds bodies) <- x = Seq $ (Cond conds (map eraseInstructions bodies)) : rest
    | (Loop cond body) <- x = Seq $ (Loop cond (eraseInstructions body)) : rest
    | otherwise = Seq rest
    where (Seq rest) = eraseInstructions $ Seq xs
eraseInstructions _ = error ("Format not supported")

injectInto :: Command a -> Command a -> Command a
injectInto (Seq []) acc = acc
injectInto (Seq list) acc = foldr injectInto acc list
injectInto (Loop cond body) acc = Cond [cond] [newBody, acc]
    where newBody = Loop cond $ Cond [NOT cond] [injectInto body acc]
injectInto (Cond conds bodies) acc =
    let newBodies = map ((flip injectInto) acc) bodies
    in  Cond conds newBodies
injectInto _ _ = error ("Format not supported")