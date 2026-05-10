/*

This program uses the arith library to calculate the diffraction
pattern caused by a point light source at infinity observed by a
telescope with an aperture of 100mm and focal length of 1000mm.

Implemented by Martin Richards (c) June 2016

It does this by considering many rays passing through an assumed
perfect circular objective lens of diameter 100mm causing the wave
front to become spherical with a radius of 1000mm centred at the focal
point. The effect of rays reaching nearby points on the focal plane
are summed taking account of their different phases.  The resulting
intensities are plotted as a graph to show the size of the central
spot and the radius of some of the surrounding diffraction rings. The
program assumes the wavelength of the light is 550nm.  The central
spot is called the Airy disk named after George Airy who was the first
to give a mathematical explanation in 1834.

Theory states that the radius of the innermost dark ring should be

r = 1.22 * lambda * F / A

where lambda is the wave length of the light (=550nm)
      F is the focal length (=1000mm)
      A      is the appeture (=100mm)
      (The ration F/A is commonly called the F number of
      a camera or telescope)

so

r = 1.22 * (550 * 10^-6) * 1000 / 100
  = 0.00671mm

This program confirms this figure.

The Rayleigh criterion of barely being able to resolve two close stars
is when the centre of the airy disk of one of the stars is on the edge
of the airy disk of the other. Increasing the magnification of the image
will not help.

*/

MANIFEST {
  ArithGlobs=350
  numupb = 2+10 // Allow a maximum precision of about 40 decimal digits.
}

GET "libhdr"
GET "arith.h"   // Insert the arith high precision library
GET "arith.b"

GET "sdl.h"     // Insert the SDL library
GET "sdl.b"

GLOBAL {
  stdin:ug
  stdout

  tracing

  // Colours
  c_black
  c_white
  c_gray
  c_blue
  c_red

  screen // For the SDL graphics
  fmt    // The graphics format

  // All numerical values use high precision numbers.

  pvx; pvy; pvz  // Vextors holding the coordinates of points
                 // on the spherical wave front touching the objective

  pvcount        // Count of the number of point in pvx, pvy and pvz.

  qvx            // X coordinates of points in the focal plane
  qvcount        // Count of the number of points in qvx
  intensityv     // Diffraction intensity of points in the focal plane

  spacev
  spacet
  spacep

  centrex  // Position of (0,0) in the SDL screen
  centrey
}

/*

The z axis is the axis of the telescope in direction from the
objective lens towards the mirror. The x axis is to the right when
viewing the objective in the z direction, and y is upwards. The origin
is on the z axis in the plane of the thin objective lens.

The focal point has coordinates (0, 0, F), where F = 1000mm.
*/

MANIFEST {
  pvupb = 101*100 // UPB of the vectors holding the coordinated
                  // of points on the spherical wave front.

  nupb = 2+8      // The size of most high precision numbers used
                  // in the calculation. This setting allows about
                  // 32 decimal digits of precision.

  spacevupb = 1000000

  F = 1000   // The focal length
  A =  100   // The aperture
  Ar = A/2   // Objective lens radius
}

LET drawdot(x, y) BE
{ // Draw a 3x3 dot at (x,y) relative to (centrex,centrey).
  // This function is used to plot points on the graph.
  LET sx = centrex + x
  LET sy = centrey + y
  drawfillrect(sx-1, sy-1, sx+1, sy+1)
  updatescreen()
}

LET initscreen() BE
{ initsdl()
  mkscreen("Airy Diffraction Pattern", 800, 400)
  // Define some colours
  c_black := maprgb(  0,   0,   0)
  c_white := maprgb(255, 255, 255)
  c_gray  := maprgb(200, 200, 200)
  c_blue  := maprgb(  0,   0, 255)
  c_red   := maprgb(255,   0,   0)

  // Choose the screen position of (0,0)
  centrex := screenxsize/2
  centrey := 60

writef("screenxsize=%n screenysize=%n*n", screenxsize,screenysize)
writef("centrex=%n centrey=%n*n", centrex, centrey)

  fillsurf(c_gray)
  updatescreen()
}

LET start() = VALOF
{ LET argv = VEC 50
  
  stdin  := input()
  stdout := output()

  UNLESS rdargs("-t/s", argv, 50) DO
  { writef("Bad arguments for cataopt*n")
    RESULTIS 0
  }

  tracing := argv!0   // -t/s

  initscreen()

  spacev := getvec(spacevupb)
  spacet := spacev+spacevupb
  spacep := spacet

  mkfront()
  drawgraph()

  freevec(spacev)

  writef("Space used = %n out of %n*n", spacet-spacep, spacevupb)
  RESULTIS 0
}

AND newvec(upb) = VALOF
{ LET p = spacep - upb - 1
  IF p<spacev DO
  { writef("*nMore space needed*n")
    abort(999)
    RESULTIS 0
  }
  spacep := p
  FOR i = 0 TO upb DO p!i := 0
  RESULTIS p
}

AND newnum(upb) = newvec(upb)

AND mkfront() BE
{ // Create the coordinates of all the points on the
  // spherical wave front.
  LET t1 = VEC nupb
  AND t2 = VEC nupb
  AND t3 = VEC nupb

  MANIFEST { step=4 }

  // step controls the number of points chosen in the objective lens.
  // The larger step is the faster the program runs but the resulting
  // graph becomes less accurate.

  pvcount := 0
  pvx := newvec(pvupb) // These will hold the x, y and z coordinated
  pvy := newvec(pvupb) // of points on the spherical wave front.
  pvz := newvec(pvupb)

  FOR x = 0 TO +Ar BY step DO
    FOR y = 0 TO +Ar BY step IF x*x+y*y <= Ar*Ar DO
  { LET Fnum = TABLE FALSE, 1, F // F as a high precision number.

    // (x,y,0) is a point in the first quadrant of the objective lens
    LET dx = VEC nupb
    AND dy = VEC nupb
    AND dz = VEC nupb

    LET nx, ny, nz = ?, ?, ?  // Three numbers will hold the x,y and z 
                              // coordinates of a point on the spherical
                              // wave front

    //writef("*nx=%i3  y=%i3*n", x, y)
    //abort(1000)

    settok(    x, dx,nupb)  // Direction of the line from the focal point
    settok(    y, dy,nupb)  // to the point (x,y,0) in the objective lens.
    settok(-1000, dz,nupb)

    //writef("dx= "); prnum(dx, nupb)
    //writef("dy= "); prnum(dy, nupb)
    //writef("dz= "); prnum(dz, nupb)

    normalize(@dx,nupb)

    //writef("*nAfter normalisation we have directions cosines*n")
    //writef("dx= "); prnum(dx, nupb)
    //writef("dy= "); prnum(dy, nupb)
    //writef("dz= "); prnum(dz, nupb)
    //newline()
    // (dx,dy,dz) is now a unit vector in direction focal point to (x,y,0)

    // Multiply dx by F
    mulbyk(F, dx,nupb)
    //writef("Multiply dx by F where F=%n*n", F)
    //writef("dx=  ");prnum(dx,nupb)
    //newline()

    // Multiply dy by F
    mulbyk(F, dy,nupb)
    //writef("Multiply dy by F where F=%n*n", F)
    //writef("dy=  ");prnum(dy,nupb)
    //newline()

    mulbyk(F, dz,nupb)
    //writef("Multiply dz by F where F=%n*n", F)
    //writef("dz=  ");prnum(dz,nupb)
    //newline()

    // Add the coordinates (0,0,1000) of the focal point
    add(Fnum,2, dz,nupb, t1,nupb)
    copy(t1,nupb, dz,nupb)
    //writef("Add F to the z coordinate where F=%n*n", F)
    //writef("dz=  ");prnum(dz,nupb)

    // (dx,dy,dz) is a point on the spherical wave front in
    // the first quadrant.

    nx := newnum(nupb)  // Allocate the numbers to hold the
    ny := newnum(nupb)  // x, y and z coordinates of a point
    nz := newnum(nupb)  // on the spherical wave front.

    copy(dx,nupb, nx,nupb)
    copy(dy,nupb, ny,nupb)
    copy(dz,nupb, nz,nupb)

    // Store these numbers in pvx, pvy and pvz.
    pvcount := pvcount+1
    pvx!pvcount := nx     // A point in the first quadrant
    pvy!pvcount := ny     // ie nx>=0 and ny>=0
    pvz!pvcount := nz
    //writef("Wave front point %i4 for (%i3,%i3)*n", pvcount, x,y)
    //writef("x= "); prnum(nx, 4)
    //writef("y= "); prnum(ny, 4)
    //writef("z= "); prnum(nz, 4)

    IF x=0 & y=0 LOOP

    UNLESS x=0 DO
    { nx := newnum(nupb)  // Allocate the numbers to hold the
      ny := newnum(nupb)  // x, y and z coordinates of a point
      nz := newnum(nupb)  // on the spherical wave front.

      copy(dx,nupb, nx,nupb)
      copy(dy,nupb, ny,nupb)
      copy(dz,nupb, nz,nupb)

      nx!0 := TRUE          // Negate just x -- second quadrant
      pvcount := pvcount+1
      pvx!pvcount := nx     // A point in the second quadrant
      pvy!pvcount := ny     // ie nx<0 and ny>=0
      pvz!pvcount := nz
      //writef("Wave front point %i4 for (%i3,%i3)*n", pvcount, -x,y)
      //writef("x= "); prnum(nx, 4)
      //writef("y= "); prnum(ny, 4)
      //writef("z= "); prnum(nz, 4)
    }

    IF x>0 & y>0 DO
    { nx := newnum(nupb)  // Allocate the numbers to hold the
      ny := newnum(nupb)  // x, y and z coordinates of a point
      nz := newnum(nupb)  // on the spherical wave front.

      copy(dx,nupb, nx,nupb)
      copy(dy,nupb, ny,nupb)
      copy(dz,nupb, nz,nupb)

      nx!0, ny!0 := TRUE, TRUE // Negate x and y -- third quadrant
      pvcount := pvcount+1
      pvx!pvcount := nx     // A point in the third quadrant
      pvy!pvcount := ny     // ie nx<0 and ny<0
      pvz!pvcount := nz
      //writef("Wave front point %i4 for (%i3,%i3)*n", pvcount, -x,-y)
      //writef("x= "); prnum(nx, 4)
      //writef("y= "); prnum(ny, 4)
      //writef("z= "); prnum(nz, 4)
    }

    IF x>=0 & y>0 DO
    { nx := newnum(nupb)  // Allocate the numbers to hold the
      ny := newnum(nupb)  // x, y and z coordinates of a point
      nz := newnum(nupb)  // on the spherical wave front.

      copy(dx,nupb, nx,nupb)
      copy(dy,nupb, ny,nupb)
      copy(dz,nupb, nz,nupb)

      ny!0 := TRUE          // Negate just y -- Fourth quadrant
      pvcount := pvcount+1
      pvx!pvcount := nx     // A point in the fourth quadrant
      pvy!pvcount := ny     // is nx>=0 and ny<0
      pvz!pvcount := nz
      //writef("Wave front point %i4 for (%i3,%i3)*n", pvcount, x,-y)
      //writef("x= "); prnum(nx, 4)
      //writef("y= "); prnum(ny, 4)
      //writef("z= "); prnum(nz, 4)
    }
  }

  writef("Number of points on the wave front = %n*n", pvcount) 
//abort(1001)
}

AND drawgraph() BE
{
//writef("Calling moveto*n")
  moveto(0, centrey)
//writef("Calling drawto*n")
  drawto(screenxsize, centrey)
  moveto(centrex, 0)
  drawto(centrex, screenysize)
//writef("Calling updatescreen*n")

  setcolour(c_black)
  moveto(centrex- 671*3/10, centrey-20)
  drawto(centrex- 671*3/10, centrey+20)
  plotf (centrex- 671*3/10 - 40, centrey-40, "-0.00671mm")

  moveto(centrex+ 671*3/10, centrey-20)
  drawto(centrex+ 671*3/10, centrey+20)
  plotf (centrex+ 671*3/10 - 40, centrey-40, "+0.00671mm")

  updatescreen()
//abort(1002)

  // Plot the intensity points
  FOR r = 0 TO 126 BY 1 DO // r is in units of 0.0001mm
  { LET fx = VEC nupb
    LET fy = VEC nupb
    LET fz = VEC nupb
    LET t1 = VEC nupb
    LET lambda = VEC nupb  // To hold the wave length 550nm in mm
    LET angle = 0
    LET sum = 0

    str2num("0.000550", lambda,nupb)  // Average wave length of visible light
    //writef("lambda= "); prnum(lambda, nupb)

    settok(r, fx,nupb)
    UNLESS r=0 DO fx!1 := fx!1 - 1 // Divide fx by 10000
    setzero(fy,nupb)
    settok(1000, fz,nupb)

    // Iterate through all the points on the wave front disc
    FOR i = 1 TO pvcount DO
    { LET x = pvx!i     // Coordinates of the next point
      LET y = pvy!i
      LET z = pvz!i

      LET dx = VEC nupb
      AND dy = VEC nupb
      AND dz = VEC nupb
      AND d  = VEC nupb

      AND inc = VEC nupb

      //writef("*nr=%i4 i=%i4*n", r, i)
      //writef("fx=  "); prnum(fx, nupb)
      //writef("fy=  "); prnum(fy, nupb)
      //writef("fz=  "); prnum(fz, nupb)

      //writef("x=   "); prnum(x,  nupb)
      //writef("y=   "); prnum(y,  nupb)
      //writef("z=   "); prnum(z,  nupb)

//writef("Calling sub(x,nupb, fx,nupb, dx,nupb)*n")
      //writef("x=   "); prnum(x,  nupb)
      //writef("fx=  "); prnum(fx, nupb)

      sub(x,nupb, fx,nupb, dx,nupb)
      //writef("dx=  "); prnum(fx, nupb)
//abort(1001)
      sub(y,nupb, fy,nupb, dy,nupb)
//abort(1002)
      sub(z,nupb, fz,nupb, dz,nupb)
//abort(1003)
      sub(x,nupb, fx,nupb, dx,nupb)
      //writef("dx=  "); prnum(dx, nupb)
//IF i=54 DO abort(5544)
      //writef("dy=  "); prnum(dy, nupb)
      //writef("dz=  "); prnum(dz, nupb)
      radius(@dx,nupb, d,nupb)
      //writef("d=   "); prnum(d, nupb)

      sub(d,nupb, fz,nupb, inc,nupb)
      //writef("inc= "); prnum(inc, nupb)
      div(inc,nupb, lambda,nupb, t1,nupb)
      //writef("t1= "); prnum(t1, nupb)

      // Set angle to the first 4 decimal digits after the decimal point

      angle := 0
      IF t1!1>=0 DO angle := t1!(t1!1+2)

      // angle is in the range 0 to 9999
//writef("fx=  "); prnum(fx, nupb)
//writef("t1=  "); prnum(t1, nupb)
      //writef("angle=%i4*n", angle)
//abort(1000)
      { LET fangle = sys(Sys_flt, fl_float, angle)
        LET x = sys(Sys_flt, fl_cos, 2.0 #* 3.14159 #* fangle #/ 10000.0)
        LET cosangle = sys(Sys_flt, fl_fix, x #* 10000.0)
        sum := sum + cosangle
        //writef("%i4/%i4: %i7  sum=%i9*n", i, pvcount, cosangle, sum)
      }
//IF i>=53 DO
      //abort(1001)
    }

    sum := sum / pvcount
    sum := sum*sum/10000
    writef("r=%7.4d  intensity= %i6*n", r, sum)
//abort(1001)
    setcolour(c_black)
    drawdot(+r*3, sum/30)
    drawdot(-r*3, sum/30)
    updatescreen()
//abort(1002)
  }
}





