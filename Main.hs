import Ast
import Interpreter

main = do
    sample <- return "INPUT Y;\nINPUT X;\nIF X > 0 OR X = 0 AND NOT 0 > Y THEN\nZ := 1;\nWHILE X > Y\nDO\nZ := Z * X;\nX := X - 1\nEND\nELSE\nZ := 0;\nEND\nPRINT Z;\n"
    print $ (readCommand :: String -> Command Int) sample
    print "With TreeMap"
    print $ interpretCommand (start :: (TreeMap Int)) [0,4] ((readCommand :: String -> Command Int) sample)
    print "With ListMap"
    print $ interpretCommand (start :: (ListMap Int)) [0,4] ((readCommand :: String -> Command Int) sample)