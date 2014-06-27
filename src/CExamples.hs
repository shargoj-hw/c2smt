module CExamples where

import           System.IO.Unsafe
import           Utils

getDataFile name =
  parseFromString
   $ unsafePerformIO
   $ do let filename = "../c-examples/" ++ name
        readFile filename

-- Loopy programs:
loop1 = getDataFile "loop1.c"
loop1do = getDataFile "loop1do.c"
loop2 = getDataFile "loop2.c"
forloop = getDataFile "forloop.c"

-- Do we handle declarations well?
multiDecl = getDataFile "multiDecl.c"
complicatedDecl = getDataFile "complicatedDecl.c"

-- Do our ifs look good?
ifreturn1 = getDataFile "ifreturn1.c"
ifreturn2 = getDataFile "ifreturn2.c"
ifreturn3 = getDataFile "ifreturn3.c"
ifreturn4 = getDataFile "ifreturn4.c"

-- Professor Wahl's Examples
example1 = getDataFile "example1.c"
example2 = getDataFile "example2.c"
example3 = getDataFile "example3.c"
example4 = getDataFile "example4.c"
example5 = getDataFile "example5.c"
