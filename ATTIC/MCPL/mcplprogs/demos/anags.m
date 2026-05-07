GET "mcpl.h"

GLOBAL   count:200

MANIFEST Mbits=#x77777777,
         Lbits=#x11111111

FUN start : => try #x32110000
               try #x11230000
               try #x20103010
               try #x11111111
               try #x22220000
               try #x12340000
               try #x02340000
               0
.
FUN try
: freqs => writef("Letter frequencies %8x", freqs)
           count := 0
           anags(freqs)
           writef(" gives %5d anagrams\n", count)
.
FUN anags
:     0 => count++
: freqs => LET poss = (freqs+Mbits)>>3 & Lbits
           UNTIL poss=0 DO { LET bit = poss & -poss
                             poss := poss-bit
                             anags(freqs-bit)
                           }
.


