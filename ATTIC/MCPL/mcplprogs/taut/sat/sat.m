GET "mcpl.h"


MANIFEST
  A =1,    B =A<<2, C =B<<2, D =C<<2, E =D<<2, // Positive settings
  F =E<<2, G =F<<2, H =G<<2, I =H<<2, J =I<<2,
  An=A<<1, Bn=B<<1, Cn=C<<1, Dn=D<<1, En=E<<1, // Negative settings
  Fn=F<<1, Gn=G<<1, Hn=H<<1, In=I<<1, Jn=J<<1,

  All   = A+B+C+D+E+F+G+H+I+J,
  Upb   = 42

STATIC             // A 3-SAT problem with 43 terms

  terms = [ A +C +En, A +C +I , A +C +Jn, A +Cn+I , A +D +Gn,
            A +Dn+En, A +En+H , A +Fn+J , An+B +Jn, An+D +H ,
            An+Dn+Gn, An+En+F , An+F +In, An+Fn+Jn, An+In+J ,
            B +D +Fn, B +Gn+In, B +Hn+In, Bn+C +Fn, Bn+D +Hn,
            Bn+Fn+Gn, Bn+Fn+Jn, Bn+H +J , Bn+In+J , C +Dn+F ,
            C +E +Hn, C +G +Jn, C +Gn+Jn, Cn+D +En, Cn+Dn+Hn,
            Cn+G +Hn, D +F +Hn, D +Gn+In, D +Gn+Jn, D +Hn+J ,
            Dn+E +F , Dn+En+F , E +G +In, En+G +H , En+I +Jn,
            F +Hn+I , Fn+Gn+In, Fn+H +In
          ]


FUN start : =>
  writef("Trying to solve a SAT problem with %d terms\n\n", Upb+1)

  try(0, 0, All, 0)
  RETURN 0
 

FUN try : i, settings, avail, tried =>

  IF i>Upb DO { writef "Solution found:  " 
                prterm settings 
                newline()
                RETURN
              }

  LET term = terms!i

  IF term & settings DO     // Test if term is already satisfied
  { try(i+1, settings, avail, tried)
    RETURN
  }

  // The term is not yet satisfied
  LET poss = term & (#b11*avail) & ~tried

  // poss contains all ways in which the term can be satisfied

  UNTIL poss=0 DO // Try them in turn
  { LET bit = poss & -poss

    poss  -:= bit
    tried +:= bit
    TEST bit & avail
    THEN try(i+1, settings+bit, avail-bit,   tried)
    ELSE try(i+1, settings+bit, avail-bit/2, tried)
  }

FUN prterm : settings => FOR let = 'A' TO 'J' DO
                         { MATCH settings & 3 
                           : 0 => wrch '-'
                           : 1 => wrch let
                           : 2 => wrch(let + 'a' - 'A')
                           : 3 => wrch '?'
                           .
                           settings >>:= 2
                         }

/* The program outputs:

Trying to solve a SAT problem with 43 terms

Solution found AbCDeFghij
Solution found Ab-DeFghij
Solution found AbcDEFgHij

*/
   












