// (C) Copyright: Martin Richards  July 1997

MODULE unpreload

GET "mcpl.h"
GET "mcli.h"

FUN start : => // unpreload [name...] 
               // unpreload all
  LET v = VEC 100

  UNLESS rdargs(",,,,,,,,,,ALL/S",v,100) DO
  { writes("Parameters no good for UNPRELOAD\n")
    RETURN 20
  }

  IF v!10 DO { unpreload 0; RETURN 0 }
  FOR i = 0 TO 9 UNLESS v!i=0 DO unpreload(v!i)
  RETURN 0
.
FUN unpreload : name =>
  LET p = @cli_preloadlist

  WHILE !p DO { LET q = !p
                TEST name=0 OR compstring(name, @q!2)=0
                THEN { unloadseg(q!1)
                       !p := !q
                       freevec q
                       IF name RETURN
                     }
                ELSE p := q
              }

  IF name DO writef("Unable to unpreload %s\n", name)
.

