module Hlogic where
import Prelude(Bool(True, False))

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

