import Ast
import Interpreter
import CopyDetection

main = do
    
    sample <- return "INPUT Y;\nINPUT X;\nIF X > 0 OR X = 0 AND NOT 0 > Y THEN\nZ := 1;\nWHILE X > Y\nDO\nZ := Z * X;\nX := X - 1\nEND\nELSE\nZ := 0;\nEND\nWHILE X > Y\nDO\nZ := Z * X;\nX := X - 1\nEND\nPRINT Z;\n"
    sample2 <- return "INPUT Y;\nINPUT X;\nIF X > 0 OR X = 0 AND NOT 0 > Y THEN\nZ := 1;\nWHILE X > Y\nDO\nZ := Z * X;\nX := X - 1\nEND\nWHILE X > Y\nDO\nZ := Z * X;\nX := X - 1\nEND\nELSE\nZ := 0;\nWHILE X > Y\nDO\nZ := Z * X;\nX := X - 1\nEND\nEND\nPRINT Z;\n"
    print $ (readCommand :: String -> Command Int) sample
    
    print "Equality"
    print (((readCommand :: String -> Command Int) sample2) == ((readCommand :: String -> Command Int) sample))
    
    print "Transformed Tree"
    print $ expand $ (readCommand :: String -> Command Int) sample
    
    print "Transformed Tree - erased"
    print $ eraseInstructions $ expand $ (readCommand :: String -> Command Int) sample
    
    print "Transformed Tree - simplified"
    print $ simplify $ expand $ (readCommand :: String -> Command Int) sample
    
    print "Transformed Tree - simplified ... Sample 2"
    print $ simplify $ expand $ (readCommand :: String -> Command Int) sample2
    
    print "With TreeMap"
    print $ interpretCommand (start :: (TreeMap Int)) [0,4] ((readCommand :: String -> Command Int) sample)
    print "With ListMap"
    print $ interpretCommand (start :: (ListMap Int)) [0,4] ((readCommand :: String -> Command Int) sample)
    print "With interpretProgram"
    print $ interpretProgram [0,4] ((readCommand :: String -> Command Int) sample)