module Hlogic where
import qualified Prelude
import Prelude(Bool(True, False))

-- NOT a
not True   = False
not False  = True

-- a AND b
True  `and` True  = True
_     `and` _     = False

-- a OR b
--False `or`  False = False
--_     `or`  _     = True
a `or`   b  = (not a) `nand` (not b)

-- a NOR b
a `nor`  b  = not (a `or`  b)

-- a NAND b
a `nand` b  = not (a `and` b)

-- p --> q (p implies q)
p --> q = p `and` (not q)
--True --> False = False
--_    --> _     = True

-- p <-- q (p is implied by q)
p <-- q = (not p) `and` q
--False <-- True = False
--_     <-- _    = True

-- p -/> q (p does not imply by q)
p -/> q = not (p --> q)
--True -/> False = True
--_    -/> _     = False

-- p </- q (p is not implied by q)
p </- q = not (p <-- q)
--False </- True = True
--_     </- _    = False

-- a XOR b
a `xor`  b = (a `and` not b) `or` (not a `and` b)
a `xnor` b = not (a `xor` b)

--------------
-- Aliases! --
--------------

~ a = not a

a `&&` b = a `and` b
a `||` b = a `or`  b
a `^`  b = a `xor` b


