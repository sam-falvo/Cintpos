/*
This is a simple demonstration of drawing in 3D.
It was also used as a test harness to help design a 3d model
of a Tigermoth for the flight simulator.

Implemented by Martin Richards (c) January 2012

History

12/03/2018
Extensively modified to use floating point and the new FLT feature.


Notes

There are three coordinate systems.

(t,w,l) are for points of the model in the direction of thrust, left wing
        and lift.

(n,w,h) are world coordinates in direction north, west and up.

(sx,sy,sz) are coordinates of points on the screen with distance to the
           right in sx, distance up in sy and depth in sz, greater depths
           are more negative.

The model can be rotated about its origin using a rotation matrix as follows

     ( n )    ( ctn  cwn  cln)   ( t )
     ( w ) =  ( ctw  cww  clw) x ( w )
     ( h )    ( cth  cwh  clh)   ( l )

Note that (ctn, ctw, cth) are the world coordinate of (1,0,0) in the model,
and       (cwn, cww, cwh) are the world coordinate of (0,1,0) in the model,
and       (cln, clw, clh) are the world coordinate of (0,0,1) in the model.

The elements of the rotation matrix are all direction cosines. For
instance, ctn is the cosine of the angle between the model's t axis
and north.

The model is viewed by the eye located on the north-south axis at a
distance eyedist south of the world origin. It is oriented so that
points on the west-east axis appear on a horizontal line halfway down
the screen.  The field of view is chosen so that a horizontal line of
length eyedist in a plane eyedist from the eye will exactly fit the
width of the screen. Points on the have horizontal and vertical positions
scaled by the factor eyedist/d where d is the z distance of the point
from the eye.


*/

GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the SDL BCPL library
.
GET "libhdr"
GET "sdl.h"

MANIFEST {
  FLT Sps = 20.0     // Steps per second
}

GLOBAL {
  done:ug

  object       // =0 for an aircraft, =1 for a hollow cube
               // =2 coloured triangles, =3 for the tiger moth

  stepping     // =FALSE if not rotating the object

  FLT c_elevator  // Range -1.0 to +1.0
  FLT c_aileron   // Range -1.0 to +1.0
  FLT c_rudder    // Range -1.0 to +1.0

  FLT ctn; FLT ctw; FLT cth    // Direction cosines of direction t
  FLT cwn; FLT cww; FLT cwh    // Direction cosines of direction w (left)
  FLT cln; FLT clw; FLT clh    // Direction cosines of direction l

  FLT eyedist                  // Eye x or y distance from aircraft

  FLT rtdot; FLT rwdot; FLT rldot // Model rotation rates about t, w and l axes

  cdrawtriangle3d   // (x1,y1,z1, x2,y2,z2, x3,y3,z3)
  cdrawquad3d       // (x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4)
                    // All floating point values.
}

// Insert the definition of drawtigermoth()
GET "drawtigermoth.b"

LET inprod(FLT a, FLT b, FLT c,
           FLT x, FLT y, FLT z) =
  // Return the cosine of the angle between two unit vectors.
  a*x + b*y + c*z

AND rotate(FLT t, FLT w, FLT l) BE
{ // Rotate the orientation of the aircraft
  // t, w and l are assumed to be small and cause
  // rotation about axis t, w, l. Positive values cause
  // anti-clockwise rotations about their axes.

  LET FLT tn = inprod(1.0,  -l,   w,  ctn,cwn,cln)
  LET FLT wn = inprod(  l, 1.0,  -t,  ctn,cwn,cln)
  LET FLT ln = inprod( -w,   t, 1.0,  ctn,cwn,cln)

  LET FLT tw = inprod(1.0,  -l,   w,  ctw,cww,clw)
  LET FLT ww = inprod(  l, 1.0,  -t,  ctw,cww,clw)
  LET FLT lw = inprod( -w,   t, 1.0,  ctw,cww,clw)

  LET FLT th = inprod(1.0,  -l,   w,  cth,cwh,clh)
  LET FLT wh = inprod(  l, 1.0,  -t,  cth,cwh,clh)
  LET FLT lh = inprod( -w,   t, 1.0,  cth,cwh,clh)

  ctn, ctw, cth := tn, tw, th
  cwn, cww, cwh := wn, ww, wh
  cln, clw, clh := ln, lw, lh

  // Make minor corrections to ensure that the axes are orthogonal and
  // of unit length.
  adjustlength(@ctn);      adjustlength(@cwn);      adjustlength(@cln) 
  adjustortho(@ctn, @cwn); adjustortho(@ctn, @cln); adjustortho(@cwn, @cln)
}

AND radius(FLT x, FLT y, FLT z) = VALOF
{ LET FLT rsq = x*x + y*y + z*z
  RESULTIS sys(Sys_flt, fl_sqrt, rsq)
}

AND adjustlength(v) BE
{ // This helps to keep vector v of unit length
  LET FLT x, FLT y, FLT z = v!0, v!1, v!2
  LET FLT r = radius(x,y,z)
  v!0 := x / r
  v!1 := y / r
  v!2 := z / r
}

AND adjustortho(a, b) BE
{ // This helps to keep the unit vector b orthogonal to a
  LET FLT a0, FLT a1, FLT a2 = a!0, a!1, a!2
  LET FLT b0, FLT b1, FLT b2 = b!0, b!1, b!2
  LET FLT corr = inprod(a0,a1,a2, b0,b1,b2)
  b!0 := b0 - a0 * corr
  b!1 := b1 - a1 * corr
  b!2 := b2 - a2 * corr
}

LET step() BE
{ // Apply rotational forces
  rtdot := -c_aileron  * 20.0 / Sps
  rwdot := -c_elevator * 20.0 / Sps
  rldot :=  c_rudder   * 20.0 / Sps

  rotate(rtdot/Sps, rwdot/Sps, rldot/Sps)
}

AND plotcraft() BE
{ LET FLT z, FLT d = 0.0, 5.0
  IF depthv FOR i = 0 TO screenxsize*screenysize-1 DO
    depthv!i := maxdepth
/*
                               //                                 l   t
                               //                                 C  A
  setcolour(maprgb(255,  0, 0))// Red,   orthogonal to t          | /
  cdrawtriangle3d(  z,  z,  z, // O                               |/
                    z,  d,  z, // B                       wB------O
                    z,  z,  d) // C
  setcolour(maprgb(  0,255, 0))// Green, orthogonal to w
  cdrawtriangle3d(  z,  z,  z, // O
                    z,  z,  d, // C
                    d,  z,  z) // A
  setcolour(maprgb(  0, 0,255))// Blue, orthogonal to l
  cdrawtriangle3d(  z,  z,  z, // O
                    d,  z,  z, // A
                    z,  d,  z) // B
*/
  IF object=0 DO
  { // Simple missile

    // Directions xyz  (right hand orientation)
    
    //   t  direction of thrust
    //   w  direction of left wing
    //   l  direction of lift

    //  The body is a hollow square tube of diameter 2.0
    //  and length 10.0
    setcolour(maprgb(64,128,64))    // Body base
    cdrawquad3d(-10.0,  1.0, -1.0,  //  G                        A-------------B
                  0.0,  1.0, -1.0,  //  C                       /|            /|
                  0.0, -1.0, -1.0,  //  D                      / |           / |
                -10.0, -1.0, -1.0)  //  H                     /  |      o   /  |
    setcolour(maprgb(40, 80,140))   // Body right side       /   |         /   |
    cdrawquad3d(-10.0, -1.0, -1.0,  //  H                   /    |        /    |
                  0.0, -1.0, -1.0,  //  D                  /     C-------/-----D
                  0.0, -1.0,  1.0,  //  B                 /     /       /     /
                -10.0, -1.0,  1.0)  //  F                /     /       /     /
    setcolour(maprgb(140, 30,100))  // Body top         E-------------F     /
    cdrawquad3d(-10.0,  1.0,  1.0,  //  E               |    /        |    /
                  0.0,  1.0,  1.0,  //  A               |   /         |   /
                  0.0, -1.0,  1.0,  //  B               |  /          |  /
                -10.0, -1.0,  1.0)  //  F               | /           | /
    setcolour(maprgb(240, 180, 30)) // Body left side   |/            |/
    cdrawquad3d(-10.0,  1.0, -1.0,  //  G               G-------------H
                  0.0,  1.0, -1.0,  //  C
                  0.0,  1.0,  1.0,  //  A
                -10.0,  1.0,  1.0)  //  E

    // The nose is a pyramid of length 12.0 on a 2.0x2.0 square base.
    
    setcolour(maprgb(255,  0,255))     // Nose base
    cdrawtriangle3d(  0.0,  1.0, -1.0,
                     12.0,  0.0,  0.0,
                      0.0, -1.0, -1.0)
    setcolour(maprgb(255,100, 55))     // Nose right side
    cdrawtriangle3d(  0.0, -1.0, -1.0,
                     12.0,  0.0,  0.0,
                      0.0, -1.0,  1.0)
    setcolour(maprgb(255,  0,255))     // Nose top
    cdrawtriangle3d(  0.0,  1.0,  1.0,
                     12.0,  0.0,  0.0,
                      0.0, -1.0,  1.0)
    setcolour(maprgb( 55,150,255))     // Nose left side
    cdrawtriangle3d(  0.0,  1.0, -1.0,
                     12.0,  0.0,  0.0,
                      0.0,  1.0,  1.0)
  }

  IF object=1 DO
  { // Tigermoth
    drawtigermoth()
  }
}

AND cdrawquad3d(FLT t1, FLT w1, FLT l1,
                FLT t2, FLT w2, FLT l2,
                FLT t3, FLT w3, FLT l3,
                FLT t4, FLT w4, FLT l4) BE
{ cdrawtriangle3d(t1,w1,l1,  t2,w2,l2,  t3,w3,l3)
  cdrawtriangle3d(t1,w1,l1,  t4,w4,l4,  t3,w3,l3)
}

AND cdrawtriangle3d(FLT t1, FLT w1, FLT l1,
                    FLT t2, FLT w2, FLT l2,
                    FLT t3, FLT w3, FLT l3) BE
{ LET FLT rn1 = t1*ctn + w1*cwn + l1*cln        // Rotated coordinates
  LET FLT rw1 = t1*ctw + w1*cww + l1*clw
  LET FLT ru1 = t1*cth + w1*cwh + l1*clh

  LET FLT rn2 = t2*ctn + w2*cwn + l2*cln
  LET FLT rw2 = t2*ctw + w2*cww + l2*clw
  LET FLT ru2 = t2*cth + w2*cwh + l2*clh

  LET FLT rn3 = t3*ctn + w3*cwn + l3*cln
  LET FLT rw3 = t3*ctw + w3*cww + l3*clw
  LET FLT ru3 = t3*cth + w3*cwh + l3*clh

  LET FLT sx1, FLT sy1, FLT sz1 = ?,?,?         // Screen coordinates
  LET FLT sx2, FLT sy2, FLT sz2 = ?,?,?
  LET FLT sx3, FLT sy3, FLT sz3 = ?,?,?

  UNLESS screencoords(rn1, rw1, ru1, @sx1) RETURN
  UNLESS screencoords(rn2, rw2, ru2, @sx2) RETURN
  UNLESS screencoords(rn3, rw3, ru3, @sx3) RETURN

//newline()
//writef("t1=%13.3f w1=%13.3f l1=%13.3f*n", t1, w1, l1)
//writef("t2=%13.3f w2=%13.3f l2=%13.3f*n", t2, w2, l2)
//writef("t3=%13.3f w3=%13.3f l3=%13.3f*n", t3, w3, l3)

//writef("ctn=%6.3f cwn=%6.3f cln=%6.3f radius=%8.3f*n",
//        ctn, cwn, cln, radius(ctn,cwn,cln))
//writef("ctw=%6.3f cww=%6.3f cly=%6.3f radius=%8.3f*n",
//        ctw, cww, clw, radius(ctw,cww,clw))
//writef("cth=%6.3f cwh=%6.3f clz=%6.3f radius=%8.3f*n",
//        cth, cwh, clh, radius(cth,cwh,clh))

//writef("sx1=%13.3f sy1=%13.3f sz1=%13.3f*n", sx1, sy1, sz1)
//writef("sx2=%13.3f sy2=%13.3f sz2=%13.3f*n", sx2, sy2, sz2)
//writef("sx3=%13.3f sy3=%13.3f sz3=%13.3f*n", sx3, sy3, sz3)

  drawtriangle3d(sx1, sy1, sz1,
                 sx2, sy2, sz2,
                 sx3, sy3, sz3)

//updatescreen()
//delay(1000)
//abort(1000)
}

AND screencoords(FLT n, FLT w, FLT u, v) = VALOF
{ // This calculates the screen coordinate of point (n,w,u) when viewed
  // by the eye. If the point is in view, it sets v!0, v!1 and v!2 to
  // the screen and depths coordinates (floating point) and return TRUE.

  LET FLT d = eyedist + n     // The n distance from the eye to the point
  LET FLT scale = eyedist/2.0 // Giving a field of view of about 27 degrees.
  LET FLT sfac = fscreenxsize/scale // x and y magnification at n=0
  LET pfac = d >= 10.0 -> eyedist/d, 1.0 // Perspective factor
  
  LET FLT sx = fscreencentrex - (w * sfac)
  LET FLT sy = fscreencentrey + (u * sfac)
  LET FLT sz =  -(n+eyedist)
  
  //writef("  n=%13.3f  w=%13.3f  u=%13.3f    sfac=%13.3f  pfac=%13.3f*n",
  //          n, w, u,  sfac, pfac)

  IF d<10.0 RESULTIS FALSE // Not sufficiently in front.

  // Apply the perspective transform
  sx := sx * pfac
  sy := sy * pfac

v!0, v!1, v!2 := sx, sy, sz
//writef("v!0= %13.3f  v!1= %13.3f  v!2= %13.3f*n",  sx,  sy,  sz)
//abort(1119)
  RESULTIS TRUE
}

AND plotscreen() BE
{ fillsurf(maprgb(100,100,255))
  plotcraft()
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ DEFAULT:
    LOOP

  CASE sdle_keydown:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:  LOOP

      CASE 'Q': done := TRUE
                LOOP

      CASE 'S': // Select next object to display
                object := (object + 1) MOD 2
                writef("*nObject %n selected*n", object)
                LOOP

      CASE 'P': // Toggle stepping
                stepping := ~stepping
                LOOP

      CASE 'R': // Reset the orientation and rotation rate
                ctn, ctw, cth       :=  0.0, 1.0, 0.0
                cwn, cww, cwh       := -1.0, 0.0, 0.0
                cln, clw, clh       :=  0.0, 0.0, 1.0
		
                rtdot, rwdot, rldot :=  0.0, 0.0, 0.0
                LOOP

      CASE 'N': // Reduce eye distance
                eyedist := eyedist*5 / 6
                IF eyedist<5.0 DO eyedist := 5.0
                LOOP

      CASE 'F': // Increase eye distance
                eyedist := eyedist * 6 / 5
                LOOP

      CASE ',':
      CASE '<': c_rudder := c_rudder - 0.05
                IF c_rudder<-1.0 DO c_rudder := -1.0
                //writef("c_rudder=%6.3f*n", c_rudder)
                LOOP

      CASE '.':
      CASE '>': c_rudder := c_rudder + 0.05
                IF c_rudder> 1.0 DO c_rudder := 1.0
                //writef("c_rudder=%6.3f*n", c_rudder)
                LOOP

      CASE sdle_arrowup:
                c_elevator := c_elevator+0.05
                IF c_elevator> 1.0 DO c_elevator := 1.0
                //writef("c_elevator=%6.3f*n", c_elevator)
                LOOP
      CASE sdle_arrowdown:
                c_elevator := c_elevator-0.05
                IF c_elevator< -1.0 DO c_elevator := -1.0
                //writef("c_elevator=%6.3f*n", c_elevator)
                LOOP
      CASE sdle_arrowright:
                c_aileron := c_aileron+0.05
                IF c_aileron> 1.0 DO c_aileron := 1.0
                //writef("c_aileron=%6.3f*n", c_aileron)
                LOOP
      CASE sdle_arrowleft:
                c_aileron := c_aileron-0.05
                IF c_aileron< -1.0 DO c_aileron := -1.0
                //writef("c_aileron=%6.3f*n", c_aileron)
                LOOP
    }

  CASE sdle_quit:
    writef("QUIT*n");
    done := TRUE
    LOOP
}

LET start() = VALOF
{ LET argv = VEC 50

  UNLESS rdargs("object/n", argv, 50) DO
  { writef("Bad argument for draw3d*n")
    RESULTIS 0
  }
  object := 0
  IF argv!0 DO object := !argv!0
  UNLESS 0<=object<=1 DO
  { writef("Bad object number %n*n", object)
    RESULTIS 0
  }
  
  // The initial direction cosines giving the orientation of
  // the object.
  ctn, ctw, cth := 1.0, 0.0, 0.0
  cwn, cww, cwh := 0.0, 1.0, 0.0
  cln, clw, clh := 0.0, 0.0, 1.0

  eyedist := 80.0  // Eye distance from the object.
  object := 0  // Missile 
  //object := 1  // Tigermoth
  stepping := TRUE
  // Initial rate of rotation about each axis
  rtdot, rwdot, rldot := 0.0, 0.0, 0.0
  c_elevator, c_aileron, c_rudder := 0.0, 0.0, 0.0

//testinprod(-1.0, -2.0, -3.0,   100.0,  10.0,  1.0)
//testinprod( 1.0, 2.0, 3.0,   100.0,  10.0,  1.0)
//writef("FIX  123.456 = %i6*n", FIX -123.456)
//writef("FIX -123.456 = %i6*n", FIX -123.456)
//writef("FIX  123.789 = %i6*n", FIX  123.789)
//RESULTIS 0

  initsdl()
  mkscreen("Draw 3D Demo", 800, 500)
  updatescreen()
  
  fscreenxsize, fscreenysize := FLOAT screenxsize, FLOAT screenysize
//writef("fscreenxsize=%13.3f fscreenysize=%13.3f*n", fscreenxsize, fscreenysize)
  fscreencentrex := fscreenxsize / 2.0
  fscreencentrey := fscreenysize / 2.0
  
  done := FALSE

  UNTIL done DO
  { processevents()
    IF stepping DO step()
    plotscreen()
    updatescreen()
    sdldelay(20)
  }

  writef("*nQuitting*n")
  sdldelay(0_20)
  closesdl()
  RESULTIS 0
}
