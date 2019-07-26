/*The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
  with some minor modifications for speed.
  comp.graphics.algorithms FAQ 
  References:
    [Gems IV]  pp. 24-46
    [O'Rourke] pp. 233-238
    [Glassner:RayTracing] */

GET "mcpl.h"

FUN pnpoly : npol, xp, yp, x, y =>
  LET j=npol-1, c=FALSE

  FOR i = 0 TO npol-1 DO
  { IF (yp!i <= y < yp!j OR yp!j <= y < yp!i) AND
	x < (xp!j - xp!i) * (y - yp!i) / (yp!j - yp!i) + xp!i DO
      c := NOT c
    j := i
  }
  RETURN c
.
FUN start : =>
  LET npol=20, count=0

  LET xp= [000,100,100,000,000,100,-50,-100,-100,-200,
          -250,-200,-150,-50,100,100,000,-50,-100,-50]
  LET yp= [000,000,100,100,200,300,200,300,000,-50,
          -100,-150,-200,-200,-150,-100,-50,-100,-100,-50]

//  FOR i = 1 TO 100000 DO
  FOR i = 1 TO 100 DO
  { IF pnpoly(npol,xp,yp, 050, 050) DO count++;
    IF pnpoly(npol,xp,yp, 050, 150) DO count++;
    IF pnpoly(npol,xp,yp, -50, 150) DO count++;
    IF pnpoly(npol,xp,yp, 075, 225) DO count++;
    IF pnpoly(npol,xp,yp,   0, 201) DO count++;
    IF pnpoly(npol,xp,yp, -50, 250) DO count++;
    IF pnpoly(npol,xp,yp,-100, -50) DO count++;
    IF pnpoly(npol,xp,yp,-150,  50) DO count++;
    IF pnpoly(npol,xp,yp,-225,-100) DO count++;
    IF pnpoly(npol,xp,yp, 050, -25) DO count++;
    IF pnpoly(npol,xp,yp, 050,-125) DO count++;
    IF pnpoly(npol,xp,yp, -50,-250) DO count++;
  }

  writef("count %d\n", count);
  RETURN 0
.
