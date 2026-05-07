/*
This is a simple demonstration of drawing in 3D.
It was also used as a test harness to help design a 3d model
of a Tigermoth for the flight simulator.

Implemented by Martin Richards (c) January 2012

History

12/03/2018
Extensively modified to use floating point and the new FLT feature.
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
  FLT c_thrust    // Range  0.0 to +1.0

  FLT ctx; FLT cty; FLT ctz    // Direction cosines of direction t
  FLT cwx; FLT cwy; FLT cwz    // Direction cosines of direction w (left)
  FLT clx; FLT cly; FLT clz    // Direction cosines of direction l

  FLT cetx; FLT cety; FLT cetz // Eye direction cosines of direction t (forward)
  FLT cewx; FLT cewy; FLT cewz // Eye direction cosines of direction w (left)
  FLT celx; FLT cely; FLT celz // Eye direction cosines of direction l (up)

  FLT eyex; FLT eyey; FLT eyez // Position of the eye relative to the aircraft.
  FLT eyedist                  // Eye x or y distance from aircraft

  FLT rtdot; FLT rwdot; FLT rldot // Rotation rates about t, w and l axes

  // Rotational forces about the aircraft axes.
  FLT rft     // Rotational force about t axis
  FLT rfw     // Rotational force about w axis
  FLT rfl     // Rotational force about l axis

  cdrawtriangle3d   // (x1,y1,z1, x2,y2,z2, x3,y3,z3)
  cdrawquad3d       // (x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4)
                    // All floating point values.
}

// Insert the definitition of drawtigermoth()
GET "drawtigermoth.b"

LET testinprod(x,y,z,  a,b,c) BE
{
  writef("inprod(%13.1f %13.1f %13.1f %13.1f %13.1f %13.1f => %13.1f*n",
          x,y,z, a,b,c, inprod(x,y,z, a,b,c))
}

AND inprod(FLT a, FLT b, FLT c,
           FLT x, FLT y, FLT z) =
  // Return the cosine of the angle between two unit vectors.
  a*x + b*y + c*z

AND rotate(FLT t, FLT w, FLT l) BE
{ // Rotate the orientation of the aircraft
  // t, w and l are assumed to be small and cause
  // rotation about axis t, w, l. Positive values cause
  // anti-clockwise rotations about their axes.

  LET FLT tx = inprod(1.0,  -l,   w,  ctx,cwx,clx)
  LET FLT wx = inprod(  l, 1.0,  -t,  ctx,cwx,clx)
  LET FLT lx = inprod( -w,   t, 1.0,  ctx,cwx,clx)

  LET FLT ty = inprod(1.0,  -l,   w,  cty,cwy,cly)
  LET FLT wy = inprod(  l, 1.0,  -t,  cty,cwy,cly)
  LET FLT ly = inprod( -w,   t, 1.0,  cty,cwy,cly)

  LET FLT tz = inprod(1.0,  -l,   w,  ctz,cwz,clz)
  LET FLT wz = inprod(  l, 1.0,  -t,  ctz,cwz,clz)
  LET FLT lz = inprod( -w,   t, 1.0,  ctz,cwz,clz)

  ctx, cty, ctz := tx, ty, tz
  cwx, cwy, cwz := wx, wy, wz
  clx, cly, clz := lx, ly, lz

  // Make minor corrections to ensure that the axes are orthogonal and
  // of unit length.
  adjustlength(@ctx);      adjustlength(@cwx);      adjustlength(@clx) 
  adjustortho(@ctx, @cwx); adjustortho(@ctx, @clx); adjustortho(@cwx, @clx)
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
{ IF depthscreen FOR i = 0 TO screenxsize*screenysize-1 DO
    depthscreen!i := FLOAT maxint

  IF object=0 DO
  { // Simple missile

    setcolour(maprgb(64,128,64))     // Body base
    cdrawquad3d(-10.0,  1.0, -1.0,
                  0.0,  1.0, -1.0,
                  0.0, -1.0, -1.0,
                -10.0, -1.0, -1.0)
    setcolour(maprgb(40, 80,140))     // Body right side
    cdrawquad3d(-10.0, -1.0, -1.0,
                  0.0, -1.0, -1.0,
                  0.0, -1.0,  1.0,
                -10.0, -1.0,  1.0)
    setcolour(maprgb(140, 30,100))     // Body top
    cdrawquad3d(-10.0,  1.0,  1.0,
                  0.0,  1.0,  1.0,
                  0.0, -1.0,  1.0,
                -10.0, -1.0,  1.0)
    setcolour(maprgb(240, 180, 30))    // Body left side
    cdrawquad3d(-10.0,  1.0, -1.0,
                  0.0,  1.0, -1.0,
                  0.0,  1.0,  1.0,
                -10.0,  1.0,  1.0)
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

AND cdrawquad3d(FLT x1, FLT y1, FLT z1,
                FLT x2, FLT y2, FLT z2,
                FLT x3, FLT y3, FLT z3,
                FLT x4, FLT y4, FLT z4) BE
{ LET FLT rx1 = x1*ctx + y1*cwx + z1*clx
  LET FLT ry1 = x1*cty + y1*cwy + z1*cly
  LET FLT rz1 = x1*ctz + y1*cwz + z1*clz

  LET FLT rx2 = x2*ctx + y2*cwx + z2*clx
  LET FLT ry2 = x2*cty + y2*cwy + z2*cly
  LET FLT rz2 = x2*ctz + y2*cwz + z2*clz

  LET FLT rx3 = x3*ctx + y3*cwx + z3*clx
  LET FLT ry3 = x3*cty + y3*cwy + z3*cly
  LET FLT rz3 = x3*ctz + y3*cwz + z3*clz

  LET FLT rx4 = x4*ctx + y4*cwx + z4*clx
  LET FLT ry4 = x4*cty + y4*cwy + z4*cly
  LET FLT rz4 = x4*ctz + y4*cwz + z4*clz

  LET FLT sx1, FLT sy1, FLT sz1 = ?,?,?
  LET FLT sx2, FLT sy2, FLT sz2 = ?,?,?
  LET FLT sx3, FLT sy3, FLT sz3 = ?,?,?
  LET FLT sx4, FLT sy4, FLT sz4 = ?,?,?

  UNLESS screencoords(rx1-eyex, ry1-eyey, rz1-eyez, @sx1) RETURN
  UNLESS screencoords(rx2-eyex, ry2-eyey, rz2-eyez, @sx2) RETURN
  UNLESS screencoords(rx3-eyex, ry3-eyey, rz3-eyez, @sx3) RETURN
  UNLESS screencoords(rx4-eyex, ry4-eyey, rz4-eyez, @sx4) RETURN

  drawquad3d(FIX sx1, FIX sy1, sz1,
             FIX sx2, FIX sy2, sz2,
             FIX sx3, FIX sy3, sz3,
             FIX sx4, FIX sy4, sz4)
}

AND cdrawtriangle3d(FLT x1, FLT y1, FLT z1,
                    FLT x2, FLT y2, FLT z2,
                    FLT x3, FLT y3, FLT z3) BE
{ LET FLT rx1 = x1*ctx + y1*cwx + z1*clx
  LET FLT ry1 = x1*cty + y1*cwy + z1*cly
  LET FLT rz1 = x1*ctz + y1*cwz + z1*clz

  LET FLT rx2 = x2*ctx + y2*cwx + z2*clx
  LET FLT ry2 = x2*cty + y2*cwy + z2*cly
  LET FLT rz2 = x2*ctz + y2*cwz + z2*clz

  LET FLT rx3 = x3*ctx + y3*cwx + z3*clx
  LET FLT ry3 = x3*cty + y3*cwy + z3*cly
  LET FLT rz3 = x3*ctz + y3*cwz + z3*clz

  LET FLT sx1, FLT sy1, FLT sz1 = ?,?,?
  LET FLT sx2, FLT sy2, FLT sz2 = ?,?,?
  LET FLT sx3, FLT sy3, FLT sz3 = ?,?,?

  UNLESS screencoords(rx1-eyex, ry1-eyey, rz1-eyez, @sx1) RETURN
  UNLESS screencoords(rx2-eyex, ry2-eyey, rz2-eyez, @sx2) RETURN
  UNLESS screencoords(rx3-eyex, ry3-eyey, rz3-eyez, @sx3) RETURN

//newline()
//writef("x1=%13.3f y1=%13.3f z1=%13.3f*n", x1, y1, z1)
//writef("x2=%13.3f y2=%13.3f z2=%13.3f*n", x2, y2, z2)
//writef("x3=%13.3f y3=%13.3f z3=%13.3f*n", x3, y3, z3)

//writef("ctx=%6.3f cwx=%6.3f clx=%6.3f radius=%8.3f*n", ctx, cwx, clx, radius(ctx,cwx,clx))
//writef("cty=%6.3f cwy=%6.3f cly=%6.3f radius=%8.3f*n", cty, cwy, cly, radius(cty,cwy,cly))
//writef("ctz=%6.3f cwz=%6.3f clz=%6.3f radius=%8.3f*n", ctz, cwz, clz, radius(ctz,cwz,clz))

//writef("sx1=%13.3f sy1=%13.3f sz1=%13.3f*n", sx1, sy1, sz1)
//writef("sx2=%13.3f sy2=%13.3f sz2=%13.3f*n", sx2, sy2, sz2)
//writef("sx3=%13.3f sy3=%13.3f sz3=%13.3f*n", sx3, sy3, sz3)

  drawtriangle3d(FIX sx1, FIX sy1, sz1,
                 FIX sx2, FIX sy2, sz2,
                 FIX sx3, FIX sy3, sz3)
  //writef("sx1=%5i sy1=%5i sz1=%13.1f*n", FIX sx1, FIX sy1, sz1)
  //writef("sx2=%5i sy2=%5i sz2=%13.1f*n", FIX sx2, FIX sy2, sz2)
  //writef("sx3=%5i sy3=%5i sz2=%13.1f*n", FIX sx3, FIX sy3, sz3)

  //updatescreen()
//delay(1000)
//abort(1000)
}

AND screencoords(FLT x, FLT y, FLT z, v) = VALOF
{ // If the point (x,y,z) is in view, set v!0, v!1 and v!2 to
  // the integer screen coordinates and depth and return TRUE
  // otherwise return FALSE
  LET FLT sx = x*cewx + y*cewy + z*cewz // Horizontal
  LET FLT sy = x*celx + y*cely + z*celz // Vertical
  LET FLT sz = x*cetx + y*cety + z*cetz // Depth
  LET fscreensize = fscreenxsize<=fscreenysize -> fscreenxsize, fscreenysize

//writef("screencoords: x=%13.3f  y=%13.3f  z=%13.3f*n", x,y,z)
//writef("cetx=%6.3f  cety=%6.3f  cetz=%6.3f*n", cetx,cety,cetz)
//writef("cewx=%6.3f  cewy=%6.3f  cewz=%6.3f*n", cewx,cewy,cewz)
//writef("celx=%6.3f  cely=%6.3f  celz=%6.3f*n", celx,cely,celz)
//writef("eyex=%13.3f  eyey=%13.3f  eyez=%13.3f*n", eyex,eyey,eyez)
//writef("  sx=%13.3f    sy=%13.3f    sz=%13.3f*n", sx,sy,sz)

  // Test that the point is in view, ie at least 1.0ft in front
  // and no more than about 27 degrees (inverse tan 1/2) from the
  // direction of view.
  IF sz<1 &  sz*sz / 2 >= sx*sx + sy*sy
    RESULTIS FALSE

  // A point screensize pixels away from the centre of the screen is
  // 45 degrees from the direction of view.
  // Note that many pixels in this range are off the screen.
  v!0 := fscreenxsize * 0.5 - fscreensize * (sx / sz) * 2
  v!1 := fscreenysize * 0.5 + fscreensize * (sy / sz) * 2
  v!2 := sz // This distance into the screen in arbitrary units, used
            // for hidden surface removal.
//writef("screencoords:  v!0=%13.3f  v!1=%13.3f  v!2=%13.3f)*n", v!0, v!1, v!2)
//abort(1119)
  RESULTIS TRUE
}

AND plotscreen() BE
{ fillsurf(maprgb(100,100,255))
  seteyeposition()
  plotcraft()
}

AND seteyeposition() BE
{ cetx, cety, cetz :=  1.0, 0.0, 0.0
  cewx, cewy, cewz :=  0.0, 1.0, 0.0
  celx, cely, celz :=  0.0, 0.0, 1.0
  eyex, eyey, eyez :=  -eyedist, 0.0, 0.0   // Relative eye position
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
                ctx, cty, ctz       :=  0.0, 1.0, 0.0
                cwx, cwy, cwz       := -1.0, 0.0, 0.0
                clx, cly, clz       :=  0.0, 0.0, 1.0
                rtdot, rwdot, rldot :=  0.0, 0.0, 0.0
                LOOP

      CASE 'N': // Reduce eye distance
                eyedist := eyedist*5 / 6
                IF eyedist<5.0 DO eyedist := 5.0
                LOOP

      CASE 'F': // Increase eye distance
                eyedist := eyedist * 6 / 5
                LOOP

      CASE 'Z': c_thrust := c_thrust-0.05
                IF c_thrust<0.0 DO c_thrust := 0.0
                //writef("c_thrust=%6.3f*n", c_thrust)
                LOOP

      CASE 'X': c_thrust := c_thrust+0.05
                IF c_thrust>1.0 DO c_thrust := 1.0
                //writef("c_thrust=%6.3f*n", c_thrust)
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
{ // The initial direction cosines giving the orientation of
  // the object.
  ctx, cty, ctz := 1.0, 0.0, 0.0  // The cosines are scaled with
  cwx, cwy, cwz := 0.0, 1.0, 0.0  // six decimal digits
  clx, cly, clz := 0.0, 0.0, 1.0  // after to decimal point.

  eyedist := 50.0  // Eye distance from the object.
  object := 0  // Missile 
  //object := 1  // Tigermoth
  stepping := TRUE
  // Initial rate of rotation about each axis
  rtdot, rwdot, rldot := 0.0, 0.0, 0.0
  c_elevator, c_aileron, c_rudder, c_thrust := 0.0, 0.0, 0.0, 0.0

//testinprod(-1.0, -2.0, -3.0,   100.0,  10.0,  1.0)
//testinprod( 1.0, 2.0, 3.0,   100.0,  10.0,  1.0)
//writef("FIX  123.456 = %i6*n", FIX -123.456)
//writef("FIX -123.456 = %i6*n", FIX -123.456)
//writef("FIX  123.789 = %i6*n", FIX  123.789)
//RESULTIS 0

  initsdl()
  mkscreen("Draw 3D Demo", 800, 500)

  fscreenxsize, fscreenysize := FLOAT screenxsize, FLOAT screenysize
//writef("fscreenxsize=%13.3f fscreenysize=%13.3f*n", fscreenxsize, fscreenysize)

  done := FALSE

  UNTIL done DO
  { processevents()
    IF stepping DO step()
    plotscreen()
    updatescreen()
    sdldelay(20)
  }

  writef("*nQuitting*n")
  sdldelay(0_100)
  closesdl()
  RESULTIS 0
}


