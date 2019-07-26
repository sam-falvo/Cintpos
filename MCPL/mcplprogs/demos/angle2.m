GET "mcpl.h"

GLOBAL
  n: 200,
  sx, sy, cgx, cgy,
  sx1, sy1,
  sx2, sy2,
  sx3, sy3,
  sx4, sy4
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
   sx, sy, n := 0, 0, 0

   mappoints fmean

   cgx, cgy := sx/n, sy/n
   writef("\ncgx = %d    cgy = %d    n = %d\n", cgx, cgy, n)

   sx1, sy1 := 0, 0
   sx2, sy2 := 0, 0
   sx3, sy3 := 0, 0
   sx4, sy4 := 0, 0

   mappoints fangle

   sx := sx1+sx2+sx3+sx4
   sy := sy1+sy2+sy3+sy4

   LET ax = 90-90*(sx1-sx2)/(sx1+sy1+sx2+sy2)
   IF ax>90 DO ax := 90 - ax
   LET ay = 90*(sy1-sy4)/(sx1+sy1+sx4+sy4)


   writef("sx1 = %d sy1 = %d\n", sx1, sy1)
   writef("sx2 = %d sy2 = %d\n", sx2, sy2)
   writef("sx3 = %d sy3 = %d\n", sx3, sy3)
   writef("sx4 = %d sy4 = %d\n", sx4, sy4)

   writef("ax = %d ay = %d\n", ax, ay)

   LET angle =  (ax*sy + ay*sx)/(sy+sx)

   writef("angle = %d\n", angle)
.
FUN mappoints
: f => f( 100,  000)
       f(-000,  050)
       f( 000, -050)
       f(-100, -000)
.
/*
FUN mappoints
: f, p => UNTIL !p=0 DO { f(p!0!0, p!0!1)
                          p+++
                        }
.
*/
FUN fmean : x,y => sx, sy, n := sx+x, sy+y, n+1
.
FUN fangle : x, y =>
   x, y := x-cgx, y-cgy
   TEST x>0 THEN TEST y>0 THEN sx1,sy1 := sx1+x, sy1+y
                          ELSE sx4,sy4 := sx4+x, sy4-y
            ELSE TEST y>0 THEN sx2,sy2 := sx2-x, sy2+y
                          ELSE sx3,sy3 := sx3-x, sy3-y
.
