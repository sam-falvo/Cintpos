MODULE prog

GET "mcpl.h"

STATIC count, all

FUN try
: ?, =all,  ? => count++

: ld, row, rd => LET poss = ~(ld | row | rd) & all
                 WHILE poss DO
                 { LET bit = poss & -poss
                   poss -:= bit
                   try( (ld|bit)<<1, row|bit, (rd|bit)>>1 )
                 }
.

FUN start : =>
  all := 1
  FOR i = 1 TO 14 DO 
  { count := 0
    try(0, 0, 0)
    writef("There are %7d solutions to %2d-queens problem\n",
                      count,           i )
    all := 2*all + 1
  }
.
