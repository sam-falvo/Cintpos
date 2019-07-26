GET "mcpl.h"


MANIFEST  A=1,    B=1<<1, C=1<<2, D=1<<3, E=1<<4,
          F=1<<5, G=1<<6, H=1<<7, I=1<<8, J=1<<9

FUN f : => 0

STATIC

  all   = A+B+C+D+E+F+G+H+I+J,

  terms = [ [A+C,  E], [A+C+I,0], [A+C,  J], [A+I,  C], [A+D,  G],
            [A,  D+E], [A+H,  E], [A+J,  F], [B,  A+J], [D+H,  A],
            [0,A+D+G], [F,  A+E], [F,  A+I], [0,A+F+J], [J,  A+I],
            [B+D,  F], [B,  G+I], [B,  H+I], [C,  B+F], [D,  B+H],
            [0,B+F+G], [0,B+F+J], [H+J,  B], [J,  B+I], [C+F,  D],
            [C+E,  H], [C+G,  J], [C,  G+J], [D,  C+E], [0,C+D+H],
            [G,  C+H], [D+F,  H], [D,  G+I], [D,  G+J], [D+J,  H],
            [E+F,  D], [F,  D+E], [E+G,  I], [G+H,  E], [I,  E+J],
            [F+I,  H], [0,F+G+I], [H,  F+I],
            0
          ]

FUN start : =>
  writef("Trying to solve a SAT problem with 43 terms\n\n")
  try(0, 0, 0, all, 0, 0)
  RETURN 0
 

FUN try : i, tset, fset, avail, ttried, ftried =>

// i       the term number, the term is in terms!i
// tset    set of variables currently true
// fset    set of variables currently false
// avail   set of variables that can still available for setting
// ttried  set of variables that has been tried set true
// ftried  set of variables that has been tried set false

  LET term = ?

  // Find the first term that is not yet satisfied
  term := terms!i++ REPEATWHILE term AND (term!0&tset OR term!1&fset)

  UNLESS term DO { writef("Solution found:  ")
                   prterm(tset, fset)
                   newline()
                   RETURN
                 }

  LET pterm=term!0, nterm=term!1

  LET tposs = pterm & avail & ~ttried

  // tposs is the set of variables that can satisfy this term
  // by being set to true

  UNTIL tposs=0 DO
  { LET pbit = tposs & -tposs // Choose a variable to set true
    tposs  := tposs  - pbit
    ttried := ttried + pbit
    try(i, tset+pbit, fset, avail-pbit, ttried, ftried)
  }

  LET fposs = nterm & avail & ~ftried

  // fposs contains all ways this term can be satisfied
  // by setting a variable to false

  UNTIL fposs=0 DO
  { LET nbit = fposs & -fposs // Choose a variable to set true
    fposs  -:= nbit
    ftried +:= nbit
    try(i, tset, fset+nbit, avail-nbit, ttried, ftried)
  }


FUN prterm : tset, fset => 
{ LET i = 0
  WHILE tset|fset DO
  { wrch( tset&1 -> 'A'+i,
          fset&1 -> 'a'+i, ' ')
    tset, fset >>:= 1, 1
    i++
  }
}

/* outputs:

Solving a SAT problem with 43 terms

Solution found:  ACDFbeghij
Solution found:  ADEFHbcgij
Solution found:  ADFbeghij

*/
