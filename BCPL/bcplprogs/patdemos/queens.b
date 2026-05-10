GET "libhdr"

GLOBAL {
  count:ug
  all
  try
}

LET try
: ?, =all, ? BE count +:= 1

: ld, row, rd BE { LET poss = ~(ld | row | rd) & all
                   WHILE poss DO
                   { LET bit = poss & -poss
                     poss -:= bit
                     try( (ld|bit)<<1, row|bit, (rd|bit)>>1 )
		   }
                 }

LET start : => VALOF
{ all := 1
  FOR i = 1 TO 12 DO 
  { count := 0
    try(0, 0, 0)
    writef("There are %5i solutions to %2i-queens problem*n",
                      count,           i )
    all := 2*all + 1
  }
  RESULTIS 0
}
