import Hlogic
import qualified Prelude
import Prelude(Bool(True, False), (==), (++))
import IO

test name result expected =
  if result == expected
    then do
           Prelude.putStrLn ("Test " ++ Prelude.show name ++ " passed.")
           Prelude.return True
    else do
           Prelude.putStrLn ("Test " ++ Prelude.show name ++ " failed:\n"      ++
                             "    Expected: " ++ Prelude.show expected ++ "\n" ++
                             "    Got:      " ++ Prelude.show result)
           Prelude.return False

main = do
  test "AND   (1)" (False &&  False) False
  test "AND   (2)" (False &&  True)  False
  test "AND   (3)" (True  &&  False) False
  test "AND   (4)" (True  &&  True)  True
  
  test "OR    (1)" (False ||  False) False
  test "OR    (2)" (False ||  True)  True
  test "OR    (3)" (True  ||  False) True
  test "OR    (4)" (True  ||  True)  True
  
  test "NAND  (1)" (False ~&& False) True
  test "NAND  (2)" (False ~&& True)  True
  test "NAND  (3)" (True  ~&& False) True
  test "NAND  (4)" (True  ~&& True)  False
  
  test "NOR   (1)" (False ~|| False) True
  test "NOR   (2)" (False ~|| True)  False
  test "NOR   (3)" (True  ~|| False) False
  test "NOR   (4)" (True  ~|| True)  False
  
  test "XOR   (1)" (False  ^  False) False
  test "XOR   (2)" (False  ^  True)  True
  test "XOR   (3)" (True   ^  False) True
  test "XOR   (4)" (True   ^  True)  False
  
  test "XNOR  (1)" (False ~^  False) True
  test "XNOR  (2)" (False ~^  True)  False
  test "XNOR  (3)" (True  ~^  False) False
  test "XNOR  (4)" (True  ~^  True)  True

