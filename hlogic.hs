module Hlogic where
import qualified Prelude
import Prelude(Bool(True, False), (==), (++))
import IO

-- NOT a
not True   = False
not False  = True

-- a AND b
True  `and` True  = True
_     `and` _     = False

-- a OR b
a `or`   b  = not a ~&& not b

-- a NAND b
a `nand` b  = not (a && b)

-- a NOR b
a `nor`  b  = not (a || b)

-- p --> q (p implies q)
p --> q = p && not q

-- p <-- q (p is implied by q)
p <-- q = not p && q

-- p -/> q (p does not imply by q)
p -/> q = not (p --> q)

-- p </- q (p is not implied by q)
p </- q = not (p <-- q)

-- a XOR b
a `xor`  b = (a && not b) || (not a && b)
a `xnor` b = not (a ^ b)

--------------
-- Aliases! --
--------------

-- ~ a = not a

a && b = a `and` b
a || b = a `or`  b

a ~&& b = a `nand` b
a ~|| b = a `nor`  b

a  ^ b = a `xor` b
a ~^ b = a `xnor` b

a ! b  = not a



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

