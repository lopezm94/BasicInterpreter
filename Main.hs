import Ast
import Interpreter

main = do
    print $ numEval (update (start :: (ListMap Int)) "X" 1) $ parseNumExp ["1","+","2","*","10","+","10","/","1","*","X"]
    sample <- return "INPUT X;\nINPUT Y;\nIF X * 1 + Z > 0 + 1 OR X = 0 OR NOT 0 > Y THEN\nZ := 1 * 2;\nWHILE X > Y\nDO\nX := X - 1;\nZ := Z * Z + 5 * 40\nEND\nELIF 1 > 0 THEN\nZ := 0;\nELSE\nY := 1;\n END\nPRINT Z;"
    print $ (readCommand :: String -> Command Int) sample