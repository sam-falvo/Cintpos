GET "mcpl.h"

MANIFEST
  P1=1,    P2=P1*2, P3=P2*2, P4=P3*2, P5=P4*2, P6=P5*2, 
  P7=P6*2, P8=P7*2, P9=P8*2, Pa=P9*2, Pb=Pa*2, Pc=Pb*2, 

  All=P1+P2+P3+P4+P5+P6+P7+P8+P9+Pa+Pb+Pc,

                        A=1,   A1=A<<1,A2=A<<2,A3=A<<3,A4=A<<4,
Bz=A<<4,By=A<<5,Bx=A<<6,B=A<<7,B1=B<<1,B2=B<<2,B3=B<<3, 
        Cy=B<<5,Cx=B<<6,C=B<<7,C1=C<<1,C2=C<<2,
                Dx=C<<6,D=C<<7,D1=D<<1,
                        E=D<<7

STATIC 
  border = [
    #1004020100, #0402010040, #0201004020, #0100402010,  // 0
                 #0040201004, #4020100402, #2010040201,
    #1004020100, #0402010040, #0201004020, #0100402010,  // 1
                 #0040201004, #4020100402, #2010040201,
    #1004020100, #0402010040, #0201004020, #0100402010,  // 2
                 #0040201004, #4020100402, #2010040201,
    #1004020100, #0402010040, #0201004020, #0100402010,  // 3
                 #0040201004, #4020100402, #2010040201,
    #1004020100, #0402010040, #0201004020, #0100402010,  // 4
                 #0040201004, #4020100402, #2010040201,
    #1004020100, #0402010040, #0201004020, #0100402010,  // 5
                 #0040201004, #4020100402, #2010040201,
    #7004020100, #7402010040, #7601004020, #7700402010,  // 6
                 #7740201004, #7760100402, #7770040201,
    #7774020100, #7776010040, #7777004020, #7777402010,  // 7
                 #7777601004, #7777700402, #7777740201,
    #7777760100, #7777770040, #7777774020, #7777776010,  // 8
                 #7777777004, #7777777402, #7777777601,
    #7777777700, #7777777740, #7777777760, #7777777770,  // 9
                 #7777777774, #7777777776, #7777777777 ],
  count=0

FUN start : =>
  count := 0
  try(0, 0, 0, border!0, 0)
  writef("\nThe total number of solutions is %d\n", count)
  RETURN 0

FUN try : bits, piece, p, brd, used =>

  IF brd&bits OR used&piece RETURN

  brd, used +:= bits, piece

  IF used=All DO { count++; 
                   writef("solution %4d\n", count)
                   RETURN
                 }

  WHILE brd&1 DO { brd>>:=1; p++ }
  brd |:= border!p

  UNLESS (A1+A2)&brd DO
  { try(A+A1+A2+A3+A4, P2, p, brd, used)
    try(A+A1+A2+A3+ B, P3, p, brd, used)
    try(A+A1+A2+A3+B1, Pb, p, brd, used)
    try(A+A1+A2+A3+B2, Pb, p, brd, used)
    try(A+A1+A2+A3+B3, P3, p, brd, used)
    try(A+A1+A2+ B+Bx, P4, p, brd, used)
    try(A+A1+A2+ B+B1, P5, p, brd, used)
    try(A+A1+A2+ B+B2, P7, p, brd, used)
    try(A+A1+A2+ B+ C, P8, p, brd, used)
    try(A+A1+A2+B1+B2, P5, p, brd, used)
    try(A+A1+A2+B1+C1, P6, p, brd, used)
    try(A+A1+A2+B2+B3, P4, p, brd, used)
    try(A+A1+A2+B2+C2, P8, p, brd, used)
  }
  UNLESS (A1+ B)&brd DO
  { try(A+A1+ B+Bx+By, P4, p, brd, used)
    try(A+A1+ B+Bx+Cx, P9, p, brd, used)
    try(A+A1+ B+Bx+B1, P5, p, brd, used)
    try(A+A1+ B+Bx+ C, P1, p, brd, used)
    try(A+A1+ B+B1+B2, P5, p, brd, used)
    try(A+A1+ B+B1+ C, P5, p, brd, used)
    try(A+A1+ B+B1+C1, P5, p, brd, used)
    try(A+A1+ B+ C+Cx, Pc, p, brd, used)
    try(A+A1+ B+ C+C1, P7, p, brd, used)
    try(A+A1+ B+ C+ D, P3, p, brd, used)
  }
  UNLESS (A1+B1)&brd DO
  { try(A+A1+B1+B2+B3, P4, p, brd, used)
    try(A+A1+B1+B2+C2, P9, p, brd, used)
//  try(A+A1+B1+B2+C1, P1, p, brd, used)
    try(A+A1+B1+C1+ C, P7, p, brd, used)
    try(A+A1+B1+C1+C2, Pc, p, brd, used)
    try(A+A1+B1+C1+D1, P3, p, brd, used)
  }
  UNLESS  (B+Bx)&brd DO
  { try(A+ B+Bx+By+Bz, P3, p, brd, used)
    try(A+ B+Bx+By+Cy, Pc, p, brd, used)
    try(A+ B+Bx+By+B1, Pb, p, brd, used)
    try(A+ B+Bx+By+ C, P6, p, brd, used)
    try(A+ B+Bx+By+Cx, P1, p, brd, used)
    try(A+ B+Bx+Cx+Cy, P9, p, brd, used)
    try(A+ B+Bx+Cx+ C, P5, p, brd, used)
    try(A+ B+Bx+Cx+Dx, P4, p, brd, used)
//  try(A+ B+Bx+Cx+B1, P1, p, brd, used)
    try(A+ B+Bx+B1+B2, Pb, p, brd, used)
    try(A+ B+Bx+B1+ C, Pa, p, brd, used)
//  try(A+ B+Bx+B1+C1, P1, p, brd, used)
    try(A+ B+Bx+ C+ D, Pb, p, brd, used)
//  try(A+ B+Bx+ C+C1, P1, p, brd, used)
  }
  UNLESS  (B+B1)&brd DO
  { try(A+ B+B1+B2+A2, P7, p, brd, used)
    try(A+ B+B1+B2+B3, P3, p, brd, used)
    try(A+ B+B1+B2+ C, P6, p, brd, used)
    try(A+ B+B1+B2+C2, Pc, p, brd, used)
//  try(A+ B+B1+B2+C1, P1, p, brd, used)
    try(A+ B+B1+ C+C1, P5, p, brd, used)
    try(A+ B+B1+ C+ D, Pb, p, brd, used)
//  try(A+ B+B1+ C+Cx, P1, p, brd, used)
    try(A+ B+B1+C1+C2, P9, p, brd, used)
    try(A+ B+B1+C1+D1, P4, p, brd, used)
  }
  UNLESS  (B+ C)&brd DO
  { try(A+ B+ C+Cx+Cy, P8, p, brd, used)
    try(A+ B+ C+Cx+Dx, P4, p, brd, used)
    try(A+ B+ C+Cx+C1, P6, p, brd, used)
    try(A+ B+ C+Cx+ D, Pb, p, brd, used)
    try(A+ B+ C+C1+C2, P8, p, brd, used)
    try(A+ B+ C+C1+ D, Pb, p, brd, used)
    try(A+ B+ C+C1+D1, P4, p, brd, used)
    try(A+ B+ C+ D+Dx, P3, p, brd, used)
    try(A+ B+ C+ D+D1, P3, p, brd, used)
    try(A+ B+ C+ D+ E, P2, p, brd, used)
  }
