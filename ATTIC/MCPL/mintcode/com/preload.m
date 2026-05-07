// (c) Copyright: M. Richards  July 1997

MODULE preload

GET "mcpl.h"
GET "mcli.h"

FUN start : =>      // preload [name] commandname
  LET v = VEC 50

  UNLESS rdargs(",,,,,,,,,", v, 50) DO
  { writes "Parameters no good for PRELOAD\n"
    RETURN 20
  }

  UNLESS v!0 DO wrpreloadlist()
  FOR i = 0 TO 9 UNLESS v!i=0 DO preload(v!i)
  RETURN 0
.
FUN wrpreloadlist : =>
  LET p = cli_preloadlist
  IF p=0 DO writes "No preloaded commands\n"
  UNTIL p=0 DO
  { LET q = p!1
    LET name = @ p!2
    LET size = 0
    WHILE q DO { size +:= q!1
                 q := !q
               }
    writef("%15t  size %5d bytes\n", name, size*Bpw)
    p := !p
  }
.
FUN preload : name =>
  LET module = loadseg name
  LET p = cli_preloadlist
  LET len = 0
  WHILE name%len DO len++
  UNLESS module DO { writef("Unable to preload %s\n", name)
                     RETURN
                   }
  IF p DO { IF compstring(name, @p!2)=0 DO
            { unloadseg(p!1)
              p!1 := module
              RETURN
            }
            p := !p
          }
  p := getvec(3 + len/Bpw)
  UNLESS p DO { writef("Unable to preload %s\n", name)
                RETURN
              }
  p!0 := cli_preloadlist
  p!1 := module
  FOR i = 0 TO len DO (@p!2)%i := name%i
  cli_preloadlist := p
.
