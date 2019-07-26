GET "mcpl.h"

GLOBAL
   n : 200,
   mx0, my0, m0,
   mx1, my1, m1,
   mx2, my2, m2,
   mx3, my3, m3,
   mx4, my4, m4,
   mx5, my5, m5,
   mx6, my6, m6,
   mx7, my7, m7

/*
STATIC pts = [ [ 10,  05],
               [ 20,  10],
               [ 30,  15],
               [ 40,  20],
               [ 50,  25],
               [ 60,  30],
               [ 70,  35],
               [ 80,  40],
               [ 90,  45],
               0]
*/
FUN start : =>
   n := 0

   mappoints fbox
.

FUN mappoints
: f => f( 100,  000)
       f(-000,  050)
       f( 000, -050)
       f(-100, -000)
.
FUN fbox : x, y => 
 TEST n=0 THEN { mx0, my0, m0 := x, y, x
                 mx1, my1, m1 := x, y, x+y
                 mx2, my2, m2 := x, y, y
                 mx3, my3, m3 := x, y, x+y
                 mx4, my4, m4 := x, y, x
                 mx5, my5, m5 := x, y, x-y
                 mx6, my6, m6 := x, y, y
                 mx7, my7, m7 := x, y, x-y
               }
          ELSE { IF m0<x DO mx0, my0, m0 := x, y, x
                 IF m1<x DO mx1, my1, m1 := x, y, x
                 IF m2<x DO mx2, my2, m2 := x, y, x
                 IF m3<x DO mx3, my3, m3 := x, y, x
                 IF m4<x DO mx4, my4, m4 := x, y, x
                 IF m5<x DO mx5, my5, m5 := x, y, x
                 IF m6<x DO mx6, my6, m6 := x, y, x
                 IF m7<x DO mx7, my7, m7 := x, y, x
               }
.









