GET "mcpl.h"

GLOBAL sw : 200, sx, sy, res, cgx, cgy, n

FUN mappoints
: f => f( 10,  05)
       f( 20,  10)
       f( 30,  15)
       f( 40,  20)
       f( 50,  25)
       f( 60,  30)
       f( 70,  35)
       f( 80,  40)
       f( 90,  45)
.
FUN start : =>
   sx, sy, n := 0, 0, 0

   mappoints fmean

   cgx, cgy := sx/n, sy/n
   writef("\ncgx = %d    cgy = %d    n = %d\n", cgx, cgy, n)

   sw, sx := 0, 0
   mappoints fanglex

   res := sx*64/sw

   writef("anglex = %d\n", 64-res)

   sw, sy := 0, 0
   mappoints fangley

   res := sy*64/sw

   writef("angley = %d\n", res)
.

FUN fmean : x,y => sx, sy, n := sx+x, sy+y, n+1
.
FUN fanglex : x, y =>
   x, y := x-cgx, y-cgy
   TEST y>0 THEN sw, sx := sw+y, sx+x
            ELSE sw, sx := sw-y, sx-x
   TEST x>0 THEN sw := sw+x
            ELSE sw := sw-x
.
FUN fangley : x, y =>
   x, y := x-cgx, y-cgy
   TEST x>0 THEN sw, sy := sw+x, sy+y
            ELSE sw, sy := sw-x, sy-y
   TEST y>0 THEN sw := sw+y
            ELSE sw := sw-y
.
