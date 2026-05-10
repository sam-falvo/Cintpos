/*

This program is demonstration of ray tracing through lenses and
mirrors. As a case study this program concentrates on the design of a
Hamiltonian catadioptric telescope consisting of a convex objective
made of crown glass and a mirror made of flint glass silvered on the
back. All the surfaces are spherical but the resulting spherical
aberration can be minimised by careful placement of the mirror and the
choice of the radii of four optical surface. At the same time
chromatic aberration can also be minimized.

Analysis of optical instruments is renowned for requiring high
precision arithmetic, so this program uses the arith library to
perform the calculations to sufficient precision.  Currently I am
using high precisiom floating point numbers with with about 40
significant decimal digits while allowing the library functions to use
upto 64. The precision constants can be changed easily if different
precisions are required.

This program must be run using a BCPL system that includes the SDL
graphics library since it draws an image on the focal plane of points
at infinity from three directions on or near the axis of the telescope
for both blue and red light. For each direction, 17 rays are chosen
using different entry points through the objective lens. Since
spherical and chromatic aberration cannot be fully corrected the
images contain scatterings of blue and red dots. The program attempts
to improve iteratively the geometry of the telescope to minimise this
scattering. The iteration only changes the radii of the spherical
front and rear surfaces of the objective lens and the front and rear
surfaces of the mirror. From a well chosen initial setting the program
can find a near optimum design for the telescope. If we can obtain a
design that causes the scattering to be no larger than the size of the
corresponding airy disk, further optimization will not improve the
resolution of the telescope.

*/


MANIFEST {
  ArithGlobs=350
  numupb = 2+18    // Allow a maximum precision of about 72 decimal digits
}

GET "libhdr"
GET "arith.h"
GET "arith.b"

GET "sdl.h"
GET "sdl.b"
.
MANIFEST {
  ArithGlobs=350
  numupb = 2+18    // Allow a maximum precision of about 72 decimal digits
}

GET "libhdr"
GET "arith.h"

GET "sdl.h"

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

  R1  // Objective lens front radius
  prevR1
  C1  // Centre of objective lens front
  R2  // Objective lens rear radius
  prevR2
  C2  // Centre of objective lens rear surface
  R3  // Concave mirror front radius
  prevR3
  C3  // Centre of concave mirror front surface
  R4  // Concave mirror silvered surface radius
  prevR4
  C4  // Centre of concave mirror silvered surface
  T1  // Objective thickness
  T2  // Mirror thickness

  Rad1 // Radius of objective, typically 60mm
  Rad2 // Radius of mirror, typically 50mm
  MirrorRadius // Actual mirror radius

  D    // The distance between the objective and mirror.
       // Typically 500mm
  F    // The z coordinate of the focus plane. Typically z=0

  N1   // Objective glass refractive index for current colour
  N2   // Mirror glass refractive index for current colour

  deltaR1
  deltaR2
  deltaR3
  deltaR4
  factor           // A power of ten used in computing next delta
  spotsizesq       // Square of current spot size or -1
  spotsize         // Current spot size or -1
  bestspotsizesq   // Square of best spot size or -1
  bestspotsize     // Current best spot size or -1

  // The controls allow the user to change these value making
  // it possible to reduce the size of image on the focus plane.
  // The focus plane is at the position that minimised the diameter
  // of the outer image.

  // Directions at small angles in the y-z plane
  dir0cx; dir0cy; dir0cz // Parallel to the telescope axis.
  dir1cx; dir1cy; dir1cz // about 1/8 degrees of the axis.
  dir2cx; dir2cy; dir2cz // about 1/4 degrees off the axis.

  Inx; Iny; Inz    // Coordinates of the entry point in plane z=0
  Outx; Outy; Outz // Coordinates of the exit point on the front surface of the mirror
  outdircx; outdircy; outdircz // Direction of the out ray

  Arad  // Radius of the A circle in the objective, typically 50mm
  Brad  // Radius of the B circle in the objective, typically 45mm

  root2 // Two useful conatants
  one

  spacev
  spacet
  spacep

  currentline // A byte vector with upb=255, used
              // when reading catageometry.txt

  iterations  // Number of iterations to perform

  spot0vx     // Dot coordinates is the focal plane
  spot0vy     // resulting from point light sources
  spot1vx     // from three directions, 0, 1/8 and
  spot1vy     // 1/4 degree from the axis. Note the
  spot2vx     // moon has a angular radius of
  spot2vy     // about 1/4 degree.
  
  spot0cgx    // CG of each spot image
  spot0cgy
  spot1cgx
  spot1cgy
  spot2cgx
  spot2cgy

  geometrystream // Used when reading or writing catageometry.txt
                 // This holds the latest setting of R1 to R4.

  // The refractive indices of crown and flint glass
  crownblue
  crownred
  flintblue
  flintred

  centrex  // Centre of the SDL screen
  centrey
}

/*

The z axis is the axis of the telescope in direction from the
objective lens to mirror. The x axis is to the right when viewing the
objective in the z direction, and y is upwards. The origin is on the z
axis in the plane where the objective fron and rear surfaces meet. The
separation D is the z coordinate of where the silvered surface of the
mirror intersects the z axis. The focal plane is at z=0 and the setting
od D=500mm give a focal length of 1000mm.

*/

MANIFEST {

  nupb = 2+15 // Size of most high precision numbers, allowing
              // about 60 decimal digits of precision.
  // If p is a number
  //   p!0          TRUE is negative, = FALSE otherwise
  //   p!1          The exponent, ie the power of 10000 to multiple or
  //                divide the fractinal part by.
  //   p!2 .. p!upb is the fractional part representing a
  //                vaule in the range 0 to 1.0

  spacevupb = 10000
}

LET drawdot(x, y) BE
{ // Draw a 3x3 dot at (x,y) relative to ((centrex,centrey)
  LET sx = centrex + x
  LET sy = centrey + y
  //LET sx = centrex + x/10
  //LET sy = centrey + y/10 - 200
  LET sx = centrex + 10*x
  LET sy = centrey + 10*y + 2200
//writef("drawdot: x=%n y=%n sx=%n sy=%n*n", x,y, sx,sy)
  drawfillrect(sx-1, sy-1, sx+1, sy+1) // Draw a 3x3 dot
  updatescreen()
}

LET initscreen() BE
{ initsdl()
  mkscreen("Catadioptric", 500, 500)

  // Define some colours
  c_black := maprgb(  0,   0,   0)
  c_white := maprgb(255, 255, 255)
  c_gray  := maprgb(200, 200, 200)
  c_blue  := maprgb(  0,   0, 255)
  c_red   := maprgb(255,   0,   0)

  // Choose the screen position of (0,0)
  centrex := screenxsize/2
  centrey := screenysize - 60
//writef("centrex=%n centrey=%n*n", centrex,centrey)
//abort(1000)
  fillsurf(c_gray)
  updatescreen()
}

LET start() = VALOF
{ LET argv = VEC 50
  LET str = VEC 255/bytesperword
  currentline := str

  stdin  := input()
  stdout := output()

  UNLESS rdargs("n/n,-t/s", argv, 50) DO
  { writef("Bad arguments for cataopt*n")
    RESULTIS 0
  }

  iterations := 1000
  IF argv!0 DO iterations := !argv!0
  tracing := argv!1   // -t/s

  spacev := getvec(spacevupb)
  spacet := spacev + spacevupb
  spacep := spacet

  UNLESS spacev DO
  { writef("*nERROR: More memory is needed*n")
    RESULTIS 0
  }

  //writef("*niterations = %n*n", iterations)

  initscreen()

  // Analyse the catadioptric telescope, hopefully optimising its
  // geometry.
  telescope()

  IF spacev DO
  { writef("Space used is %n out of %n*n", spacet-spacep, spacevupb) 
    freevec(spacev)
  }

  RESULTIS 0
}

AND intersect(dir, point, c, r, t1, t2) = VALOF
{ // This calculates the intersection points of a line and
  // a sphere.
  // dir!0 dir!1 and dir!2 are the direction cosines of the ray
  // point is a point on the ray
  // c is the z coordinate of the centre of the lens surface
  // r is the radius of the lens surface
  // point + t1*dir and point + t2*dir are the intersection points, if any
  // The result is TRUE is t1 and t2 exist.

  // The calulation is done using numbers with upper bound nupb.

  // A point on the ray has coordinates
  // x = point!0 + dir!0*t 
  // y = point!1 + dir!1*t 
  // z = point!2 + dir!2*t

  // These must be on the surface of the sphere, so
  // x^2 + y^2 + (z-c)^2 = r^2
  // This give a quadratic of the form At^2 + Bt + C = 0 

  // where

  // A = dx^2 + dy%2 + dz^2 = 1

  // B = 2(xdx +ydy + (z-c)dz)

  // C = x^2 + y^2 + (z-c)^2 - r^2

  // giving the solutions:  t = (-B +/- sqrt(B^2 - 4AC))/2A

  LET x    = point!0
  LET y    = point!1
  LET z    = point!2
  LET dx   = dir!0
  LET dy   = dir!1
  LET dz   = dir!2
  LET tmp1 = VEC nupb
  LET tmp2 = VEC nupb
  LET tmp3 = VEC nupb
  LET tmp4 = VEC nupb
  LET tmp5 = VEC nupb
  LET B    = VEC nupb
  LET C    = VEC nupb

  //newline()
  //writef("intersect*n")
  //writef("Calculating A = 1*n")
  //writef("Calculating B = 2(xdx +ydy + (z-c)dz)*n")

  mul(x,nupb, dx,nupb, tmp1,nupb)       // tmp1 = xdx
  //writef("x:                  "); prnum(x,nupb)
  //writef("dx:                 "); prnum(dx,nupb)
  //writef("xdx:                "); prnum(tmp1,nupb)
  mul(y,nupb, dy,nupb, tmp2,nupb)       // tmp2 = ydy
  //writef("y:                  "); prnum(y,nupb)
  //writef("dy:                 "); prnum(dy,nupb)
  //writef("ydy:                "); prnum(tmp2,nupb)
  sub(z,nupb, c,nupb, tmp3,nupb)        // tmp3 = z - c
  //writef("z:                  "); prnum(z,nupb)
  //writef("c:                  "); prnum(c,nupb)
  //writef("z-c:                "); prnum(tmp3,nupb)
  //writef("dz:                 "); prnum(dz,nupb)
  mul(tmp3,nupb, dz,nupb, tmp4,nupb)    // tmp4 = (z - c)dz
  //writef("(z-c)dz:            "); prnum(tmp4,nupb)

//writef("Calling add(tmp1,nupb, tmp2,nupb, tmp3,nupb)*n")
  //writef("xdx:                "); prnum(tmp1,nupb)
  //writef("ydy:                "); prnum(tmp2,nupb)
  add(tmp1,nupb, tmp2,nupb, tmp3,nupb)  // tmp3 = xdx + ydy
  //writef("xdx:                "); prnum(tmp1,nupb)
  //writef("ydy:                "); prnum(tmp2,nupb)
  //writef("xdx+ydy:            "); prnum(tmp3,nupb)
//abort(1111)
  add(tmp3,nupb, tmp4,nupb, B,nupb)     // B = xdx + ydy + (z - c)dz
  //writef("xdx+ydy+(z-c)dz:    "); prnum(B,nupb)
  mulbyk(2, B,nupb)           // B = 2(xdx + ydy + (z - c)dz)
  //writef("2(xdx+ydy+(z-c)dz): "); prnum(B,nupb)
//abort(6653)

  //writef("*nCalculating C = x^2 + y^2 + (z-c)^2 - r^2*n")

  mul(x,nupb, x,nupb, tmp1,nupb)        // tmp1 = x^2
  //writef("x^2:                "); prnum(tmp1,nupb)
  mul(y,nupb, y,nupb, tmp2,nupb)        // tmp2 = y^2
  //writef("y^2:                "); prnum(tmp2,nupb)
  sub(z,nupb, c,nupb, tmp3,nupb)        // tmp3 = z-c
  //writef("z:                  "); prnum(z,nupb)
  //writef("c:                  "); prnum(c,nupb)
  //writef("z-c:                "); prnum(tmp3,nupb)
  mul(tmp3,nupb, tmp3,nupb, tmp4,nupb)  // tmp4 = (z-c)^2
  //writef("(z-c)^2:            "); prnum(tmp4,nupb)
  mul(r,nupb, r,nupb, tmp5,nupb)        // tmp5 = r^2
  //writef("r:                  "); prnum(r,nupb)
  //writef("r^2:                "); prnum(tmp5,nupb)
  add(tmp1,nupb, tmp2,nupb, C,nupb)     // C = x^2+y^2
  //writef("x^2+y^2:            "); prnum(C,nupb)
  add(C,nupb, tmp4,nupb, tmp1,nupb)     // tmp1 = x^2+y^2+(z-c)^2
  //writef("x^2+y^2+(z-c)^2:    "); prnum(tmp1,nupb)
  sub(tmp1,nupb, tmp5,nupb, C,nupb)     // C = x^2+y^2+(z-c)^2-r^2
  //writef("x^2+y^2+(z-c)^2-r^2:"); prnum(C,nupb)

  //newline()           // Simple test of quadratic solution.
  //settok(3, B,nupb)
  //settok(-4, C,nupb)
  //writef("A = 1*n")
  //writef("B =        "); prnum(B,nupb)
  //writef("C =        "); prnum(C,nupb)
//abort(6654)
  //writef("*nSolving t^2 + Bt + C = 0*n*n")
  //writef("*nie  t = (-B +/- sqrt(B^2 - 4C))/2*n*n")

  mul(B,nupb, B,nupb, tmp1,nupb)    // tmp1 = B^2
  //writef("B^2 =               "); prnum(tmp1,nupb)
  mulbyk(4, C,nupb)                 // C = 4C
  //writef("4C =                "); prnum(C,nupb)
  sub(tmp1,nupb, C,nupb, tmp2,nupb) // tmp2 = B^2 - 4C
  //writef("B^2-4C =            "); prnum(tmp2,nupb)
  sqrt(tmp2,nupb, tmp3,nupb)        //tmp3 = sqrt(B^2 - 4C)
  //writef("sqrt(B^2-4C) =      "); prnum(tmp3,nupb)
  neg(B,nupb)                       // B = -B
  //writef("-B =                "); prnum(B,nupb)
  //writef("sqrt(B^2-4C) =      "); prnum(tmp3,nupb)
  sub(B,nupb, tmp3,nupb, t1,nupb)
  divbyk(2, t1,nupb)                // t1 = (-B - sqrt(B^2 - 4C))/2
  //writef("t1 =       "); prnum(t1,nupb)
  //writef("sqrt(B^2-4C) =      "); prnum(tmp3,nupb)
  add(B,nupb, tmp3,nupb, t2,nupb)
  divbyk(2, t2,nupb)                // t1 = (-B + sqrt(B^2 - 4C))/2
  //writef("t2 =       "); prnum(t2,nupb)
//abort(6655)
  RESULTIS TRUE
}


AND refract(indir, outdir, point, c, index) = VALOF
{ // indir  the in  direction cosines of the in ray
  // outdir the out direction cosines of the out ray
  // point  the entry point P on the lens surface
  // c      the z coordinate of the lens surface centre
  // index  the refactive index air to glass about 1.6
  //                            glass to air about 1/1.6
  // All numbers have upb = nupb

  LET ndx  = VEC nupb // The surface normal direction cosines
  AND ndy  = VEC nupb // with the same z sign as for indir.
  AND ndz  = VEC nupb
  AND costheta = VEC numupb // The in ray
  AND sintheta = VEC numupb
  AND cosphi   = VEC numupb // The out ray
  AND sinphi   = VEC numupb
  LET p1x  = VEC nupb // a point P1 on the in ray
  AND p1y  = VEC nupb // costheta away from P
  AND p1z  = VEC nupb
  LET p2x  = VEC nupb // a point P2 on the in normal
  AND p2y  = VEC nupb // at distance 1 from P
  AND p2z  = VEC nupb
  LET q1x  = VEC nupb // a point Q1 on the out normal
  AND q1y  = VEC nupb // cosphi away from P
  AND q1z  = VEC nupb
  LET q2x  = VEC nupb // a point Q2 on the out ray
  AND q2y  = VEC nupb // at distance 1 from P
  AND q2z  = VEC nupb

  // Note that P-P1-P2 and P-Q1-Q2 are both right
  // angled triangles and lie in the same plane, and that
  // P1-P2 is of length sintheta and Q1-Q2 is of length sinphi.
  // By Snell's law, the ratio of these lengths is the
  // refractive index. 

  AND tmp1 = VEC numupb
  AND tmp2 = VEC numupb
  AND tmp3 = VEC numupb
  AND tmp4 = VEC numupb

  copy(point!0,nupb, ndx,nupb)
  copy(point!1,nupb, ndy,nupb)
  sub (point!2,nupb, c,nupb, ndz,nupb)
  // (point!0, point!1 and point!2) is the (x,y,z) coordinates of
  // the entry point on the lens. 
  // (ndx,ndy,ndz) is a vector parallel to the normal.

  // Ensure that the normal and indir have the same z sign,
  // negating (ndx,ndy,ndz) if necessary
  UNLESS indir!2!0=ndz!0 DO
  { // The z signs are different, so negate the normal.
    //writef("indir!2= "); prnum(indir!2,nupb)
    //writef("ndz=     "); prnum(ndz,nupb)
    //writef("So negate the normal*n")
    neg(ndx)
    neg(ndy)
    neg(ndz)
  }

  //writef("*nIn refract applying Snell's law*n")
  //writef("Coordinates of the entry point on the lens surface*n")
  //writef("point!0=  "); prnum(point!0,nupb)
  //writef("point!1=  "); prnum(point!1,nupb)
  //writef("point!2=  "); prnum(point!2,nupb)

  //writef("The direction cosines of the normal at this point*n")
  //writef("with the z component having the same sign as*n")
  //writef("the z coordinate of the incident ray*n")
  //writef("ndx=      "); prnum(ndx,nupb)
  //writef("ndy=      "); prnum(ndy,nupb)
  //writef("ndz=      "); prnum(ndz,nupb)

  //writef("*nCalling normalize*n")
  normalize(@ndx,nupb) // Direction cosines of the normal
  //writef("ndx=      "); prnum(ndx,nupb)
  //writef("ndy=      "); prnum(ndy,nupb)
  //writef("ndz=      "); prnum(ndz,nupb)
  // (ndx,ndy,ndz) are now direction cosines.

  //writef("indir!0=  "); prnum(indir!0,nupb)
  //writef("indir!1=  "); prnum(indir!1,nupb)
  //writef("indir!2=  "); prnum(indir!2,nupb)
  //writef("indr!2 and ndz should have the same signs*n")
  //newline()
  inprod(indir,nupb, @ndx,nupb, costheta,numupb)
  // theta is the angle between the in ray and the normal.
  //writef("costheta= "); prnum(costheta,numupb)
  mul(costheta,numupb, costheta,numupb, tmp1,numupb)
  //writef("costheta^2= "); prnum(tmp1,numupb)
//abort(987)
  sub(one,nupb, tmp1,numupb, tmp3,numupb)
  //writef("1-costheta^2= "); prnum(tmp3,numupb)
  sqrt(tmp3,numupb, sintheta,numupb)
  //writef("sintheta is sqrt(1 - costheta^2)*n")
  //writef("sintheta= "); prnum(sintheta,numupb)

  div(sintheta,numupb, index,nupb, sinphi,numupb) // Apply Snell's law
  //writef("Snell's law gives:*n")
  //writef("sinphi=   "); prnum(sinphi,numupb)

  mul(sinphi,numupb, sinphi,numupb, tmp1,numupb)
  sub(one,nupb, tmp1,numupb, tmp3,numupb)
  sqrt(tmp3,numupb, cosphi,numupb)
  //writef("cosphi is sqrt(1 - sinphi^2)*n")
  //writef("cosphi=   "); prnum(cosphi,numupb)

  // P1 is on the in ray at distance 1 from point
  sub(point!0,nupb, indir!0,nupb, p1x,nupb)
  sub(point!1,nupb, indir!1,nupb, p1y,nupb)
  sub(point!2,nupb, indir!2,nupb, p1z,nupb)

  //writef("*nPoint P1 is on the in ray at distance 1 from P*n")
  //writef("p1x=      "); prnum(p1x,nupb)
  //writef("p1y=      "); prnum(p1y,nupb)
  //writef("p1z=      "); prnum(p1z,nupb)

  // Point P2 is on the normal at distance costheta from P
  //writef("*nPoint P2 is on the normal at distance costheta from P*n")
  //newline()
  mul(ndx,nupb, costheta,numupb, tmp1,numupb)
  sub(point!0,nupb, tmp1,numupb, p2x,nupb)
  //writef("tmp1=     "); prnum(tmp1,numupb)
  //writef("p2x=      "); prnum(p2x,nupb)
  mul(ndy,nupb, costheta,numupb, tmp1,numupb)
  sub(point!1,nupb, tmp1,numupb, p2y,nupb)
  //writef("tmp1=     "); prnum(tmp1,numupb)
  //writef("p2y=      "); prnum(p2y,nupb)
  mul(ndz,nupb, costheta,numupb, tmp1,numupb)
  sub(point!2,nupb, tmp1,numupb, p2z,nupb)
  //writef("tmp1=     "); prnum(tmp1,numupb)
  //writef("p2z=      "); prnum(p2z,nupb)
  //writef("P1 and P2 should both be on the same side of the lens surface*n")
  //newline()

  // Point Q1 is on the normal at distance cosphi from P.
  //writef("*nPoint Q1 is on the normal at distance cosphi from P*n")
  mul(ndx,nupb, cosphi,numupb, tmp1,numupb)
  add(point!0,nupb, tmp1,numupb, q1x,nupb)
  mul(ndy,nupb, cosphi,numupb, tmp1,numupb)
  add(point!1,nupb, tmp1,numupb, q1y,nupb)
  mul(ndz,nupb, cosphi,numupb, tmp1,numupb)
  add(point!2,nupb, tmp1,numupb, q1z,nupb)
  //newline()
  //writef("q1x=      "); prnum(q1x,nupb)
  //writef("q1y=      "); prnum(q1y,nupb)
  //writef("q1z=      "); prnum(q1z,nupb)

  // Calculate Q2 = Q1 + (P2-P1)/index
  //writef("*nPoint Q2 = Q1 + (P2-P1)/index*n")
  sub(p2x,nupb, p1x,nupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,numupb)
  div(tmp1,numupb, index,nupb, tmp2,numupb)
  //writef("tmp2=     "); prnum(tmp2,numupb)
  add(q1x,nupb, tmp2,numupb, q2x,nupb)
  //writef("q2x=      "); prnum(q2x,nupb)

  sub(p2y,nupb, p1y,nupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,numupb)
  div(tmp1,numupb, index,nupb, tmp2,numupb)
  //writef("tmp2=     "); prnum(tmp2,numupb)
  add(q1y,nupb, tmp2,numupb, q2y,nupb)
  //writef("q2y=      "); prnum(q2y,nupb)

  sub(p2z,nupb, p1z,nupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,numupb)
  div(tmp1,numupb, index,nupb, tmp2,numupb)
  //writef("tmp2=     "); prnum(tmp2,numupb)
  add(q1z,nupb, tmp2,numupb, q2z,nupb)
  //writef("q2z=      "); prnum(q2z,nupb)
//abort(980)

  //writef("q2x=      "); prnum(q2x,nupb)
  //writef("q2y=      "); prnum(q2y,nupb)
  //writef("q2z=      "); prnum(q2z,nupb)
//abort(981)

  //writef("point!0=  "); prnum(point!0,nupb)
  //writef("point!1=  "); prnum(point!1,nupb)
  //writef("point!2=  "); prnum(point!2,nupb)

//  writef("*nCalculating the out direction cosines*n*n")
  //newline()
  sub(q2x,nupb, point!0,nupb, outdir!0,nupb)
  sub(q2y,nupb, point!1,nupb, outdir!1,nupb)
  sub(q2z,nupb, point!2,nupb, outdir!2,nupb)
  //writef("outdir!0=  "); prnum(outdir!0,nupb)
  //writef("outdir!1=  "); prnum(outdir!1,nupb)
  //writef("outdir!2=  "); prnum(outdir!2,nupb)
  normalize(outdir,nupb)


  //writef("*nThe out ray direction cosines are:*n")
  //writef("outdir!0= "); prnum(outdir!0,nupb)
  //writef("outdir!1= "); prnum(outdir!1,nupb)
  //writef("outdir!2= "); prnum(outdir!2,nupb)
//abort(6655)
  RESULTIS TRUE
}

AND reflect(indir, outdir, point, c) = VALOF
{ // This computes the direction of a reflected ray
  // indir  holds the direction cosines of the in ray
  // outdir will hold the direction cosines of the reflected ray
  // point  holds the coordinates of the interection point on the mirror surface
  // c      is the z coordinate of the centre of the mirror surface
  // The result is TRUE if the reflection is successful.
  LET costheta = VEC nupb
  LET Nx = VEC nupb // The normal at the intersection point
  AND Ny = VEC nupb
  AND Nz = VEC nupb
  LET Ax = VEC nupb
  AND Ay = VEC nupb
  AND Az = VEC nupb
  LET Bx = VEC nupb
  AND By = VEC nupb
  AND Bz = VEC nupb
  LET Cx = VEC nupb
  AND Cy = VEC nupb
  AND Cz = VEC nupb
  AND tmp1 = VEC nupb

  // Calculate cos theta
  copy(point!0,nupb, Nx,nupb)
  copy(point!1,nupb, Ny,nupb)
  sub(point!2,nupb, c,nupb, Nz,nupb)
  normalize(@Nx,nupb)
  //writef("Nx=       "); prnum(Nx,nupb)
  //writef("Ny=       "); prnum(Ny,nupb)
  //writef("Nz=       "); prnum(Nz,nupb)
  inprod(indir,nupb, @Nx,nupb, costheta,nupb)
  //writef("costheta= "); prnum(costheta,nupb)

  // Calculate the coordinated of B = point - N * costheta
  mul(Nx,nupb, costheta,nupb, tmp1,nupb)
  sub(point!0,nupb, tmp1,nupb, Bx,nupb)
  mul(Ny,nupb, costheta,nupb, tmp1,nupb)
  sub(point!1,nupb, tmp1,nupb, By,nupb)
  mul(Nz,nupb, costheta,nupb, tmp1,nupb)
  sub(point!2,nupb, tmp1,nupb, Bz,nupb)
  //writef("Bx=       "); prnum(Bx,nupb)
  //writef("By=       "); prnum(By,nupb)
  //writef("Bz=       "); prnum(Bz,nupb)
  
  // Calculate the coordinates of A = point - indir
  sub(point!0,nupb, indir!0,nupb, Ax,nupb)
  sub(point!1,nupb, indir!1,nupb, Ay,nupb)
  sub(point!2,nupb, indir!2,nupb, Az,nupb)
  //writef("Ax=       "); prnum(Ax,nupb)
  //writef("Ay=       "); prnum(Ay,nupb)
  //writef("Az=       "); prnum(Az,nupb)

  // Calculate the coordinates of C = 2B - A
  mulbyk(2, Bx,nupb)
  mulbyk(2, By,nupb)
  mulbyk(2, Bz,nupb)
  sub(Bx,nupb, Ax,nupb, Cx,nupb)
  sub(By,nupb, Ay,nupb, Cy,nupb)
  sub(Bz,nupb, Az,nupb, Cz,nupb)

  // Calculate the direction of the reflected ray normalize(C - point)

  sub(Cx,nupb, point!0,nupb, outdir!0,nupb)
  sub(Cy,nupb, point!1,nupb, outdir!1,nupb)
  sub(Cz,nupb, point!2,nupb, outdir!2,nupb)
  normalize(outdir,nupb)

  RESULTIS TRUE
}

AND raytrace(indir, inpoint, crownstr, flintstr, focalx, focaly) = VALOF
{ // Trace a ray all the way through the telescope
  // The ray passes through the following
  //  (1) the front surface of the objective lens, crown glass
  //  (2) the rear surface of the objective lens
  //  (3) the front surface of the mirror, flint glass
  //  (4) the reflective surface of the mirror
  //  (5) back through the front surface of the mirror

  // The result is TRUE if the raytracing was successful.
  // focalx is the x coordinate of the image point at the focal plane
  // focaly is the y coordinate of the image point at the focal plane
  LET t1    = VEC nupb
  LET t2    = VEC nupb
  LET tmp1  = VEC nupb
  LET tmp2  = VEC nupb
  LET tmp3  = VEC nupb
  LET indx  = VEC nupb // Private in direction
  AND indy  = VEC nupb
  AND indz  = VEC nupb
  LET inptx = VEC nupb // Point on an in ray
  AND inpty = VEC nupb
  AND inptz = VEC nupb
  LET x     = VEC nupb // For intersection points
  AND y     = VEC nupb
  AND z     = VEC nupb
  LET outdx = VEC nupb
  AND outdy = VEC nupb
  AND outdz = VEC nupb

  AND index = VEC nupb // Refractive index

  // indir    direction cosines of incident ray.
  // inpoint  the point at z=0 to which the ray is aimed.
  // crownstr the refractive index of blue or red light of crown glass
  // flintstr the refractive index of blue or red light of flint glass
  // focalx   will be given the x coordinate of the image point in
  //          the focal plane (z=0).
  // focaly   will be given the y coordinate of the image point in
  //          the focal plane (z=0).
  // The result is TRUE if the raytracing was successful.

  //writef("*nraytrace entered*n")
  //writef("Crown glass refractive index: %s*n", crownstr)
  //writef("Flint glass refractive index: %s*n*n", flintstr)

  //writef("indir!0:   "); prnum(indir!0,nupb)
  //writef("indir!1:   "); prnum(indir!1,nupb)
  //writef("indir!2:   "); prnum(indir!2,nupb)
  //writef("inpoint!0: "); prnum(inpoint!0,nupb)
  //writef("inpoint!1: "); prnum(inpoint!1,nupb)
  //writef("inpoint!2: "); prnum(inpoint!2,nupb)


  // Front surface of the objective lens

  copy(indir!0,nupb, indx,nupb) // In direction to front surface of objective
  copy(indir!1,nupb, indy,nupb)
  copy(indir!2,nupb, indz,nupb)

  copy(inpoint!0,nupb, inptx,nupb)   // the intersection point on the front surface
  copy(inpoint!1,nupb, inpty,nupb)
  copy(inpoint!2,nupb, inptz,nupb)

  UNLESS intersect(@indx, @inptx, C1, R1, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with front surface of objective successful*n")

  // Select the negative root
  IF t2!0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=        "); prnum(t1,nupb)
//abort(5454)
  IF tracing DO
    writef("*nSo the objective front surface intersection point (x,y,z) is:*n")
  
  //writef("indir!0=    "); prnum(indir!0,nupb)
  //writef("t1=   "); prnum(t1,nupb)
  mul(indx,nupb, t1,nupb, tmp1,nupb)
  //writef("indir!0**t1=   "); prnum(tmp1,nupb)
  //writef("inpoint!0=    "); prnum(inpoint!0,nupb)
  add(inptx,nupb, tmp1,nupb, x,nupb)
  IF tracing DO
  { writef("x=         "); prnum(x, nupb) }

  mul(indy,nupb, t1,nupb, tmp1,nupb)
  add(inpty,nupb, tmp1,nupb, y,nupb)
  IF tracing DO
  { writef("y=         "); prnum(y, nupb) }

  mul(indz,nupb, t1,nupb, tmp1,nupb)
  add(inptz,nupb, tmp1,nupb, z,nupb)
  IF tracing DO
  { writef("z=         "); prnum(z, nupb) }
  //newline()

  //writef("*nObjective front z= "); prnum(z, nupb)

  // Apply Snell's law to obtain the transmitted direction

  // indx,indy,indz hold the direction cosines of the in ray
  // outdir will hold the direction cosines of the out ray
  // inpoint is the entry point on the lens surface
  // C1 is the z coordinate of the centre of the lens front surface
  // index is the refractive index. For air to glass this is about 1.6
  // for glass to air it is about 1/1.6

  str2num(crownstr, index,nupb)

  //newline()
  //writef("indir!0=  "); prnum(indir!0,nupb)
  //writef("indir!1=  "); prnum(indir!1,nupb)
  //writef("indir!2=  "); prnum(indir!2,nupb)
  //writef("x=        "); prnum(x,nupb)
  //writef("y=        "); prnum(y,nupb)
  //writef("z=        "); prnum(z,nupb)

  refract(@indx, @outdx, @x, C1, index)

  //writef("outdx=    "); prnum(outdx,nupb)
  //writef("outdy=    "); prnum(outdy,nupb)
  //writef("outdz=    "); prnum(outdz,nupb)
//abort(8877)


  // Now deal with the rear surface of the objective lens

  str2num(crownstr, tmp1,nupb)
  div(one,nupb, tmp1,nupb, index,nupb) // Refractive index glass to air

  //writef("*n*nDealing with the rear surface of the objective lens*n*n")
  //writef("index=  "); prnum(index,nupb)

  copy(outdx,nupb, indx,nupb) // Out ray of front surface becomes
  copy(outdy,nupb, indy,nupb) // the in ray of the rear surface
  copy(outdz,nupb, indz,nupb)

  copy(x,nupb, inptx,nupb)   // the intersection point on the front surface
  copy(y,nupb, inpty,nupb)
  copy(z,nupb, inptz,nupb)

  UNLESS intersect(@indx, @inptx, C2, R2, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with rear surface of objective successful*n")
  // Select the positive root
  UNLESS t2!0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=        "); prnum(t1,nupb)

  IF tracing DO
    writef("Objective rear surface intersection point (x,y,z) is:*n")
  
  //writef("indx=    "); prnum(indx,nupb)
  //writef("t1=   "); prnum(t1,nupb)
  mul(indir!0,nupb, t1,nupb, tmp1,nupb)
  //writef("indx**t1=   "); prnum(tmp1,nupb)
  //writef("inptx=    "); prnum(inptx,nupb)
  add(inptx,nupb, tmp1,nupb, x,nupb)
  IF tracing DO
  { writef("x=         "); prnum(x, nupb) }

  mul(indy,nupb, t1,nupb, tmp1,nupb)
  add(inpty,nupb, tmp1,nupb, y,nupb)
  IF tracing DO
  { writef("y=         "); prnum(y, nupb) }

  mul(indz,nupb, t1,nupb, tmp1,nupb)
  add(inptz,nupb, tmp1,nupb, z,nupb)
  IF tracing DO
  { writef("z=         "); prnum(z, nupb) }
  //newline()
//abort(8855)
  //writef("Objective rear  z= "); prnum(z, nupb)

  // Calculate the new out direction
  refract(@indx, @outdx, @x, C2, index)
  //newline()

//abort(8866)

  // Now deal with the front surface of the mirror.

  str2num(flintstr, index,nupb)

  //writef("*n*nDealing with the front surface of the mirror*n*n")
  //writef("index=  "); prnum(index,nupb)

  copy(outdx,nupb, indx,nupb)
  copy(outdy,nupb, indy,nupb)
  copy(outdz,nupb, indz,nupb)

  copy(x,nupb, inptx,nupb)   // The intersection point on the rear surface of the objective lens
  copy(y,nupb, inpty,nupb)
  copy(z,nupb, inptz,nupb)

  UNLESS intersect(@indx, @inptx, C3, R3, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with front surface of the mirror*n")
  // Select the positive root
  UNLESS t2!0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=        "); prnum(t1,nupb)

  IF tracing DO
    writef("The mirror front surface intersection point (x,y,z) is:*n")
  
  //writef("indx=    "); prnum(indx,nupb)
  //writef("t1=   "); prnum(t1,nupb)
  mul(indx,nupb, t1,nupb, tmp1,nupb)
  //writef("indx**t1=   "); prnum(tmp1,nupb)
  //writef("inptx=    "); prnum(inptx,nupb)
  add(inptx,nupb, tmp1,nupb, x,nupb)
  IF tracing DO
  { writef("x=         "); prnum(x, nupb) }

  mul(indy,nupb, t1,nupb, tmp1,nupb)
  add(inpty,nupb, tmp1,nupb, y,nupb)
  IF tracing DO
  { writef("y=         "); prnum(y, nupb) }

  mul(indz,nupb, t1,nupb, tmp1,nupb)
  add(inptz,nupb, tmp1,nupb, z,nupb)
  IF tracing DO
  { writef("z=         "); prnum(z, nupb) }
  //newline()

  //writef("Mirror front    z= "); prnum(z, nupb)


  // Calculate the distance from the z axis
  mul(x,nupb, x,nupb, tmp1,nupb)  //tmp1 = x^2
  mul(y,nupb, y,nupb, tmp2,nupb)  //tmp2 = y^2
  add(tmp1,nupb, tmp2,nupb, tmp3,nupb) // tmp3 = x^2 + y^2
  sqrt(tmp3,nupb, tmp1,nupb) // tmp1 = the radius
  IF numcmp(tmp1,nupb, MirrorRadius,nupb) > 0 DO
    copy(tmp1,nupb, MirrorRadius,nupb)
  //writef("tmp1=          "); prnum(tmp1,nupb)
  //writef("Mirror radius= "); prnum(MirrorRadius,nupb)
//abort(3100)
  // Calculate the new out direction
  refract(@indx, @outdx, @x, C3, index)
  //newline()




  // Now deal with the reflecting surface of the mirror.

  //writef("*n*nDealing with the reflecting surface of the mirror*n*n")

  copy(outdx,nupb, indx,nupb) // Out direction of the front surface is
  copy(outdy,nupb, indy,nupb) // the in direction to the silvered surface
  copy(outdz,nupb, indz,nupb)

  copy(x,nupb, inptx,nupb)   // the intersection point on the front surface of the mirror
  copy(y,nupb, inpty,nupb)
  copy(z,nupb, inptz,nupb)

  UNLESS intersect(@indx, @inptx, C4, R4, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with reflective surface of the mirror*n")
  // Select the positive root
  UNLESS t2!0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=        "); prnum(t1,nupb)

  IF tracing DO
    writef("The mirror reflective surface intersection point (x,y,z) is:*n")
  
  //writef("indx=    "); prnum(indx,nupb)
  //writef("t1=   "); prnum(t1,nupb)
  mul(indx,nupb, t1,nupb, tmp1,nupb)
  //writef("indx**t1=   "); prnum(tmp1,nupb)
  //writef("inptx=    "); prnum(inptx,nupb)
  add(inptx,nupb, tmp1,nupb, x,nupb)
  IF tracing DO
  { writef("x=         "); prnum(x, nupb) }

  mul(indy,nupb, t1,nupb, tmp1,nupb)
  add(inpty,nupb, tmp1,nupb, y,nupb)
  IF tracing DO
  { writef("y=         "); prnum(y, nupb) }

  mul(indz,nupb, t1,nupb, tmp1,nupb)
  add(inptz,nupb, tmp1,nupb, z,nupb)
  IF tracing DO
  { writef("z=         "); prnum(z, nupb) }
  //newline()

  //writef("Mirror rear     z= "); prnum(z, nupb)

  // Calculate the new out direction
  reflect(@indx, @outdx, @x, C4)
  //newline()




  // Now deal with the front surface of the mirror again.

  str2num(flintstr, tmp1,nupb)
  div(one,nupb, tmp1,nupb, index,nupb) // Refractive index glass to air

  //writef("*n*nDealing with the front surface of the mirror again*n*n")
  //writef("index=  "); prnum(index,nupb)

  copy(outdx,nupb, indx,nupb)
  copy(outdy,nupb, indy,nupb)
  copy(outdz,nupb, indz,nupb)

  copy(x,nupb, inptx,nupb)   // the intersection point on the front surface
  copy(y,nupb, inpty,nupb)
  UNLESS intersect(@indx, @inptx, C3, R3, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with rear surface of objective successful*n")
  // Select the smaller root
  IF numcmp(t2,nupb, t1,nupb)<0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=        "); prnum(t1,nupb)

  IF tracing DO
    writef("The mirror front surface intersection point (x,y,z) is:*n")
  
  //writef("indx=    "); prnum(indx,nupb)
  //writef("t1=   "); prnum(t1,nupb)
  mul(indx,nupb, t1,nupb, tmp1,nupb)
  //writef("indx**t1=   "); prnum(tmp1,nupb)
  //writef("inptx=    "); prnum(inptx,nupb)
  add(inptx,nupb, tmp1,nupb, x,nupb)
  IF tracing DO
  { writef("x=         "); prnum(x, nupb) }

  mul(indy,nupb, t1,nupb, tmp1,nupb)
  add(inpty,nupb, tmp1,nupb, y,nupb)
  IF tracing DO
  { writef("y=         "); prnum(y, nupb) }

  mul(indz,nupb, t1,nupb, tmp1,nupb)
  add(inptz,nupb, tmp1,nupb, z,nupb)
  IF tracing DO
  { writef("z=         "); prnum(z, nupb) }
  //newline()

  //writef("Mirror front    z= "); prnum(z, nupb)

  // Calculate the new out direction
  //writef("Calling refract*n")
  refract(@indx, @outdx, @x, C3, index)
  //newline()
  //writef("outdx=     "); prnum(outdx,nupb)
  //writef("outdy=     "); prnum(outdy,nupb)
  //writef("outdz=     "); prnum(outdz,nupb)

  div(z,nupb, outdz,nupb, tmp1,nupb)
  mul(outdx,nupb, tmp1,nupb, tmp2,nupb)
  sub(x,nupb, tmp2,nupb, focalx,nupb)
  mul(outdy,nupb, tmp1,nupb, tmp2,nupb)
  sub(y,nupb, tmp2,nupb, focaly,nupb)
//writef("focalx=%n focaly=%n*n", focalx, focaly)
  IF tracing DO
  { writef("Focalx= "); prnum(focalx,nupb) }
  IF tracing DO
  { writef("Focaly= "); prnum(focaly,nupb) }
  //abort(1888)
  RESULTIS TRUE
}

AND newvec(upb) = VALOF
{ LET p = spacep - upb - 1
  IF p<spacev DO
  { writef("*nMore space needed*n")
    abort(999)
    RESULTIS 0
  }
  spacep := p
  FOR i = 0 TO nupb DO p!i := 0
  RESULTIS p
}

AND newnum(upb) = newvec(upb)

AND telescope() BE
{ LET tmp1 = VEC nupb
  AND tmp2 = VEC nupb
  AND tmp3 = VEC nupb
  AND tmp4 = VEC nupb
  AND tmp5 = VEC nupb
  LET ptcount = 0      // Current number of points in the current spot
  LET prevmaxvalue = VEC nupb // To hold the square of the maximum point separation
  LET delta = VEC nupb        // Current change in a radius value
  LET initdelta = VEC nupb    // Current initial setting of delta
  LET prevR = VEC nupb
  LET failcount = -1 // Unset

  // The idea is to try changing each radius R1 to R4 by a small amount delta
  // either positive or negative. If maxvalue improves do the same again with
  // a larger delta. If it does not improve work on the next R. If the current
  // initdelta does not cause any improvement on any R reduce the size of initdelta.

  // Periodically the radii R1 to R4 are output to the file catageometry.txt
  // saving the previous version in optgeometry1.txt

  // If the file catageometry.txt exists it is used to set the initial values
  // of R1 to R4

  // Typically every time cataopt runs these radii are improved.


//writef("telescope entered*n")

  // Refractive indices.
  // The objective glass is crown and the mirror glass is flint.

  //                crown        flint
  // blue 486 nm    1.51690      1.6321
  // red  640 nm    1.50917      1.6161

  crownblue := "1.51690"
  crownred  := "1.50917"
  flintblue := "1.6321"
  flintred  := "1.6161"

  //newline()

  // Directions 0, 1 and 2 are small angles in the y-z plane
  // used by test in rays.

  writef("*nRay direction cosines for directions 0, 1 and 2*n*n")

  dir0cx := newnum(nupb) // Direction parallel to the telescope axis
  dir0cy := newnum(nupb)
  dir0cz := newnum(nupb)
  setzero(dir0cx,nupb)
  setzero(dir0cy,nupb)
  settok(1000, dir0cz,nupb)
  normalize(@dir0cx,nupb)

  writef("dir0cx: "); prnum(dir0cx, nupb)
  writef("dir0cy: "); prnum(dir0cy, nupb)
  writef("dir0cz: "); prnum(dir0cz, nupb)
  newline()

  dir1cx := newnum(nupb)
  dir1cy := newnum(nupb)
  dir1cz := newnum(nupb)
  settok(   0, dir1cx,nupb) // Direction about 1/8 degree off the telescope axis.
  settok(  -1, dir1cy,nupb) // Note that 1 in 60 is about 1 degree
  settok(8*60, dir1cz,nupb)
  normalize(@dir1cx,nupb)
  writef("dir1cx: "); prnum(dir1cx,nupb)
  writef("dir1cy: "); prnum(dir1cy,nupb)
  writef("dir1cz: "); prnum(dir1cz,nupb)
  newline()

  dir2cx := newnum(nupb)
  dir2cy := newnum(nupb)
  dir2cz := newnum(nupb)
  settok(   0, dir2cx,nupb) // Direction about 1/4 degree off the telescope axis.
  settok(  -1, dir2cy,nupb) // This is a field of view about the size of the moon.
  settok(4*60, dir2cz,nupb) // Note that 1 in 60 is about 1 degree
  normalize(@dir2cx,nupb)
  writef("dir2cx: "); prnum(dir2cx,nupb)
  writef("dir2cy: "); prnum(dir2cy,nupb)
  writef("dir2cz: "); prnum(dir2cz,nupb)
  newline()
//abort(1000)
  Arad := newnum(nupb)  // Radius of the A circle in the objective
  settok(50, Arad,nupb)
  //writef("Arad:   "); prnum(Arad,5)
  Brad := newnum(nupb)  // Radius of the B circle in the objective
  settok(25, Brad,nupb)
  //writef("Brad:   "); prnum(Brad,5)

  R1 := newnum(nupb)  // Objective lens front radius
  prevR1 := newnum(nupb)
  C1 := newnum(nupb)  // Centre of objective lens front
  R2 := newnum(nupb)  // Objective lens rear radius
  prevR2 := newnum(nupb)
  C2 := newnum(nupb)  // Centre of objective lens rear surface
  R3 := newnum(nupb)  // Concave mirror front radius
  prevR3 := newnum(nupb)
  C3 := newnum(nupb)  // Centre of concave mirror front surface
  R4 := newnum(nupb)  // Concave mirror silvered surface radius
  prevR4 := newnum(nupb)
  C4 := newnum(nupb)  // Centre of concave mirror silvered surface
  T1 := newnum(nupb)  // Objective thickness
  T2 := newnum(nupb)  // Mirror thickness

  Rad1 := newnum(nupb) // Radius of objective, typically 60mm
  Rad2 := newnum(nupb) // Radius of mirror, typically 50mm
  MirrorRadius := newnum(nupb) // Actual radius of mirror

  D := newnum(nupb)    // The distance between the objective and mirror.
                   // This is typically 500mm.
  F := newnum(nupb)    // The z coordinate of the focus plane, typically F=0
                   // This typically give a focal length of 1000mm.

  Inx := newnum(nupb)  // The direction cosines of an in going ray to a
  Iny := newnum(nupb)  // lens or mirror.
  Inz := newnum(nupb)

  Outx := newnum(nupb) // The direction cosines of an out going ray to a
  Outy := newnum(nupb) // lens or mirror.
  Outz := newnum(nupb)

  outdircx := newnum(nupb)
  outdircy := newnum(nupb)
  outdircz := newnum(nupb)

  deltaR1 := newnum(nupb) // Used to test a nearby setting of R1 to R4
  deltaR2 := newnum(nupb)
  deltaR3 := newnum(nupb)
  deltaR4 := newnum(nupb)

  root2 := newnum(nupb)
  one   := newnum(nupb)

  spot0vx := newvec(16+16+2-1) // Space for points of spot0
  spot0vy := newvec(16+16+2-1)
  spot1vx := newvec(16+16+2-1) // Space for points of spot1
  spot1vy := newvec(16+16+2-1)
  spot2vx := newvec(16+16+2-1) // Space for points of spot2
  spot2vy := newvec(16+16+2-1)

  // Note that for spot0 the x coordinates in the focal plane
  // are held in spot0vx. The table of subscripts is as follows

  //  0 to  7  A circle Blue dots
  //  8 to 15  A circle Red  dots
  // 16 to 23  B circle Blue dots
  // 24 to 31  B circle Red  dots
  // 32        C ray    Blue dot
  // 33        C ray    Red  dot

  // spot0vy holds the y coordinates of spot0 dots

  // spot1vx, spot1vy, spot2vx and spot2vy hold the coordinates
  // of the spot1 and spot2 Blue and Red dots

  FOR i = 0 TO 16+16+2-1 DO
  { // Allocate space for all the spot dot coordinates
    spot0vx!i := newnum(nupb)
    spot0vy!i := newnum(nupb)
    spot1vx!i := newnum(nupb)
    spot1vy!i := newnum(nupb)
    spot2vx!i := newnum(nupb)
    spot2vy!i := newnum(nupb)
  }

  // Allocate numbers for the spot centres of gravity
  spot0cgx := newnum(nupb)
  spot0cgy := newnum(nupb)
  spot1cgx := newnum(nupb)
  spot1cgy := newnum(nupb)
  spot2cgx := newnum(nupb)
  spot2cgy := newnum(nupb)

  bestspotsizesq := newnum(nupb)
  bestspotsize   := newnum(nupb)
  spotsizesq := newnum(nupb)
  spotsize   := newnum(nupb)

  N1 := newnum(nupb)   // Objective glass refractive index for current colour
  N2 := newnum(nupb)   // Mirror glass refractive index for current colour

  UNLESS N2 DO
  { writef("More space needed*n")
    abort(999)
    RETURN
  }


  // All numbers have been created successfully

  // Set the unchanginging geometry of the telescope

  // The Objective lens front surface
  settok(50, Rad1,nupb)     // lens diameter = 2*50 = 100mm
  //writef("Rad1=   "); prnum(Rad1,5)

  settok(  4, T1,nupb)   // Objective lens thickness 4mm at centre.
  //writef("T1=     "); prnum(T1,5)

  settok(  4, T2,nupb)   // Mirror thickness 4mm at centre.
  //writef("T2=     "); prnum(T2,5)

  settok(500, D,nupb)    // z coordinate of the mirror silvered surface
  //writef("D=      "); prnum(D,5)

  settok(2, tmp1,nupb)
  sqrt(tmp1,nupb, root2,nupb)
  //writef("root2= "); prnum(root2,nupb)

  settok(1, one,nupb)

  // Initialize the setting of the lens and mirror surface radii.
  // These are overwritten if file catagemetry.txt exists.

  // It seems that the initial settings of R1 to R4 often cause
  // the iterations to lead to a false minimum. So some
  // experimention was needed before a good result was obtained.

  //str2num("2000", R1,nupb)  // Objective front surface radius
  //str2num("5000", R2,nupb)  // Objective rear surface radius
  //str2num("1100", R3,nupb)  // Radius of mirror front surface
  //str2num("1200", R4,nupb)  // Radius of mirror silvered surface
  // This give a spot size of about 0.0627

  //str2num("1988.0964", R1,nupb)
  //str2num("5011.4216", R2,nupb)
  //str2num("1101.4742", R3,nupb)
  //str2num("1197.2792", R4,nupb)
  // This give a spot size of about ????

  //str2num("2983.9124 9986", R1,nupb)
  //str2num("4016.7284 7477", R2,nupb)
  //str2num(" 795.8206 0106", R3,nupb)
  //str2num("1004.8255 8657", R4,nupb)
  // This give a spot size of about 0.0273

  //str2num("2985.0978", R1,nupb)
  //str2num("4020.4995", R2,nupb)
  //str2num(" 792.0045", R3,nupb)
  //str2num("1001.2200", R4,nupb)
  // This give a spot size of about 0.0278

  str2num("3000", R1,nupb)
  str2num("3000", R2,nupb)
  str2num(" 900", R3,nupb)
  str2num("1000", R4,nupb)
  // This give a spot size of about 0.0???

  //str2num("2997.6008", R1,nupb)
  //str2num("4002.2990", R2,nupb)
  //str2num(" 794.9465", R3,nupb)
  //str2num("1002.9717", R4,nupb)
  // This give a spot size of about 0.0???

  //str2num("2997.6008", R1,nupb)
  //str2num("4002.2990", R2,nupb)
  //str2num(" 794.9465", R3,nupb)
  //str2num("1004.9717", R4,nupb)
  // This give a spot size of about 0.0???

  //str2num("2775.8197 3531", R1,nupb)
  //str2num("4309.1358 6069", R2,nupb)
  //str2num(" 797.2550 1869", R3,nupb)
  //str2num("1007.5214 4239", R4,nupb)
  // This give a spot size of about 0.0258

  // To show that this is close to the theortical optimum for a
  // telescope with an aperture of 100m consider the following.

  // Light is electromagnetic radiation but, unlike radio waves
  // which have wave lengths measured in metres, visible light
  // has a wave length measured in nano-metres. Blue light is
  // typically 486nm and red light is about 640nm.

  // If we consider a point source of light with a wave length of
  // 550 at infinity on the axis of a optically perfect telescope,
  // rays passing through every point the objective lens will be in
  // phase when they reach the focus point. As a result of
  // diffraction the image is not a tiny spot but a rather larger
  // spot surrounded by light and dark rings. A point of the
  // innermost dark ring is where the path lengths from opposite
  // edges of the objective lens differ by about one wave length of
  // the colour being considered. This is because rays being received
  // at this point are out of phase with other rays and so are
  // partially cancelled. The size of the bright spot at the centre
  // is thus somewhat smaller than the size of the innermost dark
  // diffraction ring. By applying simple geometry we can estimate
  // the radius of the innermost diffraction ring as (1000/100)*(550/2)
  // which is about 2750nm. This is 2750*10^-9m = 2750^10^-6mm
  // = 0.00275mm. The diameter of the bright spot will thus be about
  // 0.0055mm. Since our best spot size of 0.0216mm is only about
  // 4 times as large as this, we are close to the theoretical limit
  // of a telescope with an aperture of 100mm.

  // As a rule of thumb the maximum usable magnification of a
  // telescope is between about 1 and 1.2 times its aperture in
  // millimetres. In practice it is far less that because of
  // the disturbance caused the atmosphere.

  factor  := 13       // A power of ten
                      // The initial values of the deltas are random
                      // integers in the range 0 to 9999 which are then divided
                      // by 10^factor.
                      // In the original version of this algorithm,
                      // factor was incremented every time no improvement
                      // is made after 100 iterations.
                      // The current version leaves factor unchanged but
                      // doubles the delta values each time they are found
                      // to reduce the spot size.
                      // Every time an improvement is made, factor, R1, R2, R3
                      // and R4 are written to the file catageometry.txt.
                      // If this file exists it is used to set the starting
                      // values the next time cataopt is run.

  // For now ignore catageometry.txt
  geometrystream := findinput("catageometry.txt")

  UNLESS geometrystream DO
  { writef("Calling wrgeometry()*n")
    wrgeometry()
    geometrystream := findinput("catageometry.txt")
  }

  IF geometrystream DO
  { // File catageometry.txt exists so update factor, R1, R2, R3 and R4
    // from values in this file.
    selectinput(geometrystream)

    factor := readn()
    rdch()

    readline(currentline)
    str2num(currentline, R1,nupb)

    readline(currentline)
    str2num(currentline, R2,nupb)

    readline(currentline)
    str2num(currentline, R3,nupb)

    readline(currentline)
    str2num(currentline, R4,nupb)

    endstream(geometrystream)

    writef("*nInitial state set from file catageometry.txt*n*n")
  }

  writef("factor = %n*n", factor)
  writef("R1=     "); prnum(R1, nupb)
  writef("R2=     "); prnum(R2, nupb)
  writef("R3=     "); prnum(R3, nupb)
  writef("R4=     "); prnum(R4, nupb)
//abort(1022)

  settok(1000, bestspotsizesq,nupb) // Unset the best spot size

again:  // Enter here every time a new setting of R1 to R4 is being tried.

  // Save current values of R1 to R4
  copy(R1,nupb, prevR1,nupb)
  copy(R2,nupb, prevR2,nupb)
  copy(R3,nupb, prevR3,nupb)
  copy(R4,nupb, prevR4,nupb)

newdelta:

  TEST failcount=0
  THEN { // Previous setting of the delta values caused an improvement
         // so double them.
         //writef("*nfailcount=0 so double the delta values*n")
         mulbyk(2, deltaR1,nupb)
         mulbyk(2, deltaR2,nupb)
         mulbyk(2, deltaR3,nupb)
         mulbyk(2, deltaR4,nupb)
       }
  ELSE { // The previous setting, if any, made no improvement
         // so choose a new random setting, ensuring that
         // deltaR3 > deltaR4
         LET k = factor

         //writef("*nChoosing a new random setting of the delta values*n")
         settok(randno(9999)-5000, deltaR1,nupb)
         settok(randno(9999)-5000, deltaR2,nupb)
         settok(randno(9999)-5000, deltaR3,nupb)
         settok(randno(9999)-5000, deltaR4,nupb)

         // Quite often only change R1 and R2 or R3 and R4.
         IF randno(101)>30 DO
         TEST randno(101)>50
         THEN { setzero(deltaR1,nupb)
                setzero(deltaR2,nupb)
//writef("Clear deltaR1 and deltaR2*n")
              }
         ELSE { setzero(deltaR3,nupb)
                setzero(deltaR4,nupb)
//writef("Clear deltaR3 and deltaR4*n")
              }

         // Divide the delta values by 10^factor
         WHILE k>=4 DO
         { deltaR1!1 := deltaR1!1 - k/4 // Divide by 10000^(k/4)
           deltaR2!1 := deltaR2!1 - k/4 // Divide by 10000^(k/4)
           deltaR3!1 := deltaR3!1 - k/4 // Divide by 10000^(k/4)
           deltaR4!1 := deltaR4!1 - k/4 // Divide by 10000^(k/4)
           k := k MOD 4
         }

         UNTIL k<=0 DO
         { divbyk(10, deltaR1,nupb)
           divbyk(10, deltaR2,nupb)
           divbyk(10, deltaR3,nupb)
           divbyk(10, deltaR4,nupb)
           k := k-1
         }
       }

//  writef("failcount=%n, factor=%n*n", failcount, factor)
//  writef("*nThe new delta values are:*n*n")
//  writef("deltaR1= "); prnum(deltaR1, nupb)
//  writef("deltaR2= "); prnum(deltaR2, nupb)
//  writef("deltaR3= "); prnum(deltaR3, nupb)
//  writef("deltaR4= "); prnum(deltaR4, nupb)

  // Add the delta values to the radii.
  add(deltaR1,nupb, prevR1,nupb, R1,nupb)
  add(deltaR2,nupb, prevR2,nupb, R2,nupb)
  add(deltaR3,nupb, prevR3,nupb, R3,nupb)
  add(deltaR4,nupb, prevR4,nupb, R4,nupb)

  //writef("*nThe new settings R1 to R4 are as follows:*n*n")
  //writef("R1= "); prnum(R1, nupb)
  //writef("R2= "); prnum(R2, nupb)
  //writef("R3= "); prnum(R3, nupb)
  //writef("R4= "); prnum(R4, nupb)
//abort(1501)

  IF R1!0 | R2!0 | R3!0 | R4!0 DO
  { // One of the radii has become negative 
    writef("One of R1 to R4 has become negative, so choose a different delta*n")
    writef("R1= "); prnum(R1, nupb)
    writef("R2= "); prnum(R2, nupb)
    writef("R3= "); prnum(R3, nupb)
    writef("R4= "); prnum(R4, nupb)
    failcount := failcount+1
    abort(9999)
    GOTO newdelta
  }

  // Insist that R3 is greater than R4
  IF numcmp(R3,nupb, R4,nupb) > 0 DO
  { failcount := failcount+1
    writef("R3 is greater than R4*n")
    writef("R3= "); prnum(R3, nupb)
    writef("R4= "); prnum(R4, nupb)
abort(1000)
    failcount := failcount+1
    GOTO newdelta
  }

  //writef("*nThe new delta values are:*n*n")
  //writef("deltaR1= "); prnum(deltaR1, nupb)
  //writef("deltaR2= "); prnum(deltaR2, nupb)
  //writef("deltaR3= "); prnum(deltaR3, nupb)
  //writef("deltaR4= "); prnum(deltaR4, nupb)

  //writef("*nTrying the following setting of R1 to R4*n")
  //writef("R1= "); prnum(R1, nupb)
  //writef("R2= "); prnum(R2, nupb)
  //writef("R3= "); prnum(R3, nupb)
  //writef("R4= "); prnum(R4, nupb)

  //writef("spot size squared      "); prnum(spotsizesq,nupb)
  //writef("best spot size squared "); prnum(bestspotsizesq,nupb)

//abort(7765)
  // Initialize the focal plane image
  fillsurf(c_gray)
  //setcolour(c_blue)
  //plotf(centrex-80, 50, "Focal Plane Points")

  setcolour(c_black)
  plotf(centrex-85,110, "R1          %i4.%z4 %z4 %z4 mm*n",
        prevR1!2, prevR1!3, prevR1!4, prevR1!5)
  plotf(centrex-85, 90, "R2          %i4.%z4 %z4 %z4 mm*n",
        prevR2!2, prevR2!3, prevR2!4, prevR2!5)
  plotf(centrex-85, 70, "R3          %i4.%z4 %z4 %z4 mm*n",
        prevR3!2, prevR3!3, prevR3!4, prevR3!5)
  plotf(centrex-85, 50, "R4          %i4.%z4 %z4 %z4 mm*n",
        prevR4!2, prevR4!3, prevR4!4, prevR4!5)

  TEST spotsize!1=0
  THEN plotf(centrex-85, 30, "Spot size 0.%z4 %z4 %z4 %z4 mm",
             spotsize!2, spotsize!3, spotsize!4, spotsize!5)
  ELSE plotf(centrex-85, 30, "Spot size   %i4.%z4 %z4 %z4 mm",
             spotsize!2, spotsize!3, spotsize!4, spotsize!5)

  TEST bestspotsize!1=0
  THEN plotf(centrex-85, 10, "Best size 0.%z4 %z4 %z4 %z4 mm",
             bestspotsize!2, bestspotsize!3, bestspotsize!4, bestspotsize!5)
  ELSE plotf(centrex-85, 10, "Best size   %i4.%z4 %z4 %z4 mm",
             bestspotsize!2, bestspotsize!3, bestspotsize!4, bestspotsize!5)

  setcolour(c_black)
  moveto(0, centrey)
  drawby( screenxsize,   0)
  moveto(centrex, 0)
  drawby(0, screenysize)

  updatescreen()


  //writef("*nTesting new radii*n")
//abort(1600)
  sqrt(spotsizesq,nupb, spotsize,nupb)
  //writef("spotsize=     "); prnum(spotsize,nupb)
  sqrt(bestspotsizesq,nupb, bestspotsize,nupb)
  //writef("bestspotsize= "); prnum(bestspotsize,nupb)
  //newline()
  //writef("R1= "); prnum(R1, nupb)
  //writef("R2= "); prnum(R2, nupb)
  //writef("R3= "); prnum(R3, nupb)
  //writef("R4= "); prnum(R4, nupb)
//abort(3000)
  // Calculate the values that depend on the radii R1 to R4

  // Calculate the z coordinate of the objective front surface centre
  copy(T1,nupb, tmp1,nupb)           // The thickness of the objective lens
  divbyk(2, tmp1,nupb)                  // Half of its thickness
  //writef("T1/2=   "); prnum(tmp1,nupb)

  sub(R1,nupb, tmp1,nupb, C1,nupb)
  //writef("C1=     "); prnum(C1,nupb) // The z coordinate of centre of the
                                     // objective front surface.

  sub(tmp1,nupb, R2,nupb, C2,nupb)
  //writef("C2=     "); prnum(C2,nupb) // The z coordinate of centre of the
                                     // objective rear surface.

  sub(D,nupb, T2,nupb, tmp1,nupb)
  sub(tmp1,nupb, R3,nupb, C3,nupb)   // Centre of mirror front surface
  //writef("C3=     "); prnum(C3,nupb)

  sub( D,nupb, R4,nupb, C4,nupb)     // Centre of mirror silvered surface
  //writef("C4=     "); prnum(C4,nupb)

  // Now trace several rays through the telescope

  setzero(spotsizesq,nupb) // Initial setting of spotsizesq
  ptcount := 0             // The current spot has no points yet.

  setzero(MirrorRadius,nupb)

  // Mark all dot coordinates as unset so that calcsposize
  // will only use those that have been defined.
//abort(1333)
  FOR i = 0 TO 16+16+2-1 DO
  { // Unset all the rays

    // Only need to mark the x coordinates
    spot0vx!i!1 := 1
    spot1vx!i!1 := 1
    spot2vx!i!1 := 1
  }

  // The rate of convergence is greatly improved by commenting
  // out most of the call of doray, but at least two should be
  // left in. Leaving them mostly uncommented causes a prettier
  // picture to be drawn.

  //doray(0, 'A', 0)
  //doray(0, 'A', 1)
  //doray(0, 'A', 2)
  //doray(0, 'A', 3)
  //doray(0, 'A', 4)
  //doray(0, 'A', 5)
  //doray(0, 'A', 6)
  //doray(0, 'A', 7)

  //doray(0, 'C', 0)

  //doray(1, 'A', 0)
  //doray(1, 'A', 1)
  //doray(1, 'A', 2)
  //doray(1, 'A', 3)
  //doray(1, 'A', 4)
  //doray(1, 'A', 5)
  //doray(1, 'A', 6)
  //doray(1, 'A', 7)

  //doray(1, 'C', 0)

  doray(2, 'A', 0)
  //doray(2, 'A', 1)
  doray(2, 'A', 2)
  //doray(2, 'A', 3)
  doray(2, 'A', 4)
  //doray(2, 'A', 5)
  doray(2, 'A', 6)
  //doray(2, 'A', 7)

  doray(2, 'C', 0)

//abort(5656)
  //writef("*nMirrorRadius= "); prnum(MirrorRadius, nupb)
  //newline()
//abort(5100)

  // Calculate the CGs of the spots and their sizes, updating spotsizesq
  // if necessary.
  calcspotsize(0)
//writef("After call of calcspotsize(0)*n")
  //writef("spotsizesq=         "); prnum(spotsizesq,nupb)
  calcspotsize(1)
//writef("After call of calcspotsize(1)*n")
  //writef("spotsizesq=         "); prnum(spotsizesq,nupb)
  calcspotsize(2)
//writef("After call of calcspotsize(2)*n")
  
  //writef("spotsizesq=         "); prnum(spotsizesq,nupb)

  sqrt(spotsizesq,nupb, tmp1,nupb)
  //writef("spotsize=           "); prnum(tmp1,nupb)

//abort(8888)

  sqrt(spotsizesq,nupb, spotsize,nupb)

  TEST numcmp(spotsizesq,nupb, bestspotsizesq,nupb) < 0
  THEN {
         //writef("bestspotsizesq= "); prnum(bestspotsizesq, nupb)
         //writef("spotsizesq=     "); prnum(spotsizesq, nupb)
         //writef("Copy spotsizesq to bestspotsizesq*n")
         copy(spotsizesq,nupb, bestspotsizesq,nupb)
         copy(spotsize,nupb,   bestspotsize,nupb)

         newline()
         writef("Spot size has reduced, iteration=%n*n", iterations)
         writef("bestspotsize= "); prnum(bestspotsize,nupb)
//abort(6666)

         wrgeometry()
         failcount := 0
         iterations := iterations-1
         //writef("Iterations left %n*n", iterations)
       }
  ELSE { // No improvement so re-instate the previous radii.
         copy(prevR1,nupb, R1,nupb)
         copy(prevR2,nupb, R2,nupb)
         copy(prevR3,nupb, R3,nupb)
         copy(prevR4,nupb, R4,nupb)
         failcount := failcount+1
         writef("This delta failed, factor=%n failcount=%n iterations=%n*n",
                factor, failcount, iterations)
         writef("spotsize=     "); prnum(spotsize,nupb)
         writef("bestspotsize= "); prnum(bestspotsize,nupb)
//abort(5555)
         IF failcount>100 DO
         { // Make the delta values smaller
           factor := factor + 1
           IF factor > 12 RETURN // Return from telescope
           failcount := 0
           writef("*n*nSetting new factor=%n*n", factor)
           GOTO again
         }
       }

  IF iterations>0 GOTO again

fin:
//abort(7777)
}

AND calcspotsize(spotno) BE
{ // Calculate the CG of spot spotno and its size squared
  // If this is larger than spotsizesq, update spotsizesq.
  LET tmp1 = VEC nupb
  LET tmp2 = VEC nupb
  LET tmp3 = VEC nupb
  LET tmp4 = VEC nupb
  LET tmp5 = VEC nupb
  LET tmp6 = VEC nupb
  LET cgx  = VEC nupb
  LET cgy  = VEC nupb
  LET px = spotno=0 -> spot0vx,
           spotno=1 -> spot1vx,
                       spot2vx
  LET py = spotno=0 -> spot0vy,
           spotno=1 -> spot1vy,
                       spot2vy

  LET dotcount = 0

  //writef("*ncalcspotsize: spotno=%n*n", spotno)
  FOR i = 0 TO 16+16+2-1 DO
  { //writef("%i2 x= ", i); prnum(px!i,nupb)
    //writef("%i2 y= ", i); prnum(py!i,nupb)
  }
//abort(1100)
  setzero(cgx,nupb)
  setzero(cgy,nupb)
//abort(4333)
  FOR i = 0 TO 16+16+2-1 UNLESS px!i!1=1 DO
  { dotcount := dotcount+1
    add(px!i,nupb, cgx,nupb, tmp1,nupb)
    copy(tmp1,nupb, cgx,nupb)
    add(py!i,nupb, cgy,nupb, tmp1,nupb)
    copy(tmp1,nupb, cgy,nupb)
    //writef("i=%i2*n", i)
    //writef("px!i= "); prnum(px!i,nupb)
    //writef("cgx=  "); prnum(cgx,nupb)
    //writef("py!i= "); prnum(py!i,nupb)
    //writef("cgy=  "); prnum(cgy,nupb)
    //abort(7000)
  }

  UNLESS dotcount RETURN

  divbyk(dotcount, cgx,nupb)
  divbyk(dotcount, cgy,nupb)
  //writef("CG of spot %n, dotcount=%n*n", spotno, dotcount)
  //writef("cgx= "); prnum(cgx, 4)
  //writef("cgy= "); prnum(cgy, 4)
//abort(3000)

  // Calculate the radius squared
  FOR i = 0 TO 16+16+2-1 DO
  { LET x = px!i
    LET y = py!i
    IF x!1=1 LOOP
//abort(5555)
    sub(x,nupb, cgx,nupb, tmp2,nupb)        // tmp2 = x-cgx
    //writef("cgx=                   "); prnum(cgx, nupb)
    //writef("x=                     "); prnum(x, 5)
    //writef("x-cgx=                 "); prnum(tmp2, nupb)
    mul(tmp2,nupb, tmp2,nupb, tmp3,nupb)    // tmp3 = (x-cgx)^2
    //writef("(x-cgx)^2=             "); prnum(tmp3, nupb)

    //writef("cgy=                   "); prnum(cgy, nupb)
    //writef("y=                     "); prnum(y, 5)
    sub(y,nupb, cgy,nupb, tmp4,nupb)        // tmp4 = y-cgy
    //writef("y-cgy=                 "); prnum(tmp4, nupb)
    mul(tmp4,nupb, tmp4,nupb, tmp5,nupb)    // tmp5 = (y-cgy)^2
    //writef("(y-cgy)^2=             "); prnum(tmp5, nupb)

    add(tmp3,nupb, tmp5,nupb, tmp6,nupb)    // tmp6 = (x-cgx)^2 + (y-cgy)^2
    //writef("(x-cgx)^2 + (y-cgy)^2= "); prnum(tmp6, nupb)
    //writef("spot%n i=%i2 spotsizesq= ", spotno, i); prnum(spotsizesq, nupb)
    IF numcmp(spotsizesq,nupb, tmp6,nupb) < 0 DO copy(tmp6,nupb, spotsizesq,nupb) 
    //writef("spot%n i=%i2 spotsizesq= ", spotno, i); prnum(spotsizesq, nupb)
    //sqrt(spotsizesq,nupb, spotsize,nupb)
    //writef("spot%n i=%i2 spotsize=   ", spotno, i); prnum(spotsize, nupb)
  }
  sqrt(spotsizesq,nupb, spotsize,nupb)
  //writef("spot%n spotsize=   ", spotno); prnum(spotsize, 5)
//abort(1277)
}

AND wrgeometry() BE
{ // Create file catageometry.txt with the current settings
  // of factor and  R1 to R4.
  LET filename = "catageometry.txt"
  //writef("Calling findoutput(*"%s*")*n", filename)
  geometrystream := findoutput(filename)
  UNLESS geometrystream DO
  { writef("*nUnable to create file: *"%s*"*n", filename)
    abort(999)
    RETURN
  }
  selectoutput(geometrystream)
  writef("%n*n", factor)
  prnum(R1,8)
  prnum(R2,8)
  prnum(R3,8)
  prnum(R4,8)
  endstream(geometrystream)
  selectoutput(stdout)
}

AND readline(str) BE
{ LET len = 0

  { LET ch = rdch()
    IF ch = '*n' | ch=endstreamch | len>=255 BREAK
    len := len + 1
    str%len := ch
  } REPEAT

  str%0 := len
}

AND doray(n, ch, pos) BE // direction, radius ch, point number
{ LET dir = n=0 -> @dir0cx,
            n=1 -> @dir1cx,
                   @dir2cx
  LET radius = ch='A' -> Arad, // Outer circle
               ch='B' -> Brad, // Inner circle
                         0     // Centre point
  LET tmp1   = VEC nupb
  LET raddiv = VEC nupb
  LET focalx = ?
  AND focaly = ?

  LET px = n=0 -> spot0vx,
           n=1 -> spot1vx,
                  spot2vx 

  LET py = n=0 -> spot0vy,
           n=1 -> spot1vy,
                  spot2vy

  // Calculate the position of the dot coordinates
  px, py := px+pos, py+pos // Add the position (0 to 7) in the circle
  IF ch = 'A' DO px, py := px+ 0, py+ 0  // pos of dot pos circle A 
  IF ch = 'B' DO px, py := px+16, py+16  // pos of dot pos circle B
  IF ch = 'C' DO px, py := px+32, py+32  // pos of then dot ray C 
//abort(9999)
  TEST radius=0
  THEN setzero(raddiv,nupb)
  ELSE div(radius,nupb, root2,nupb, raddiv,nupb)

//writef("doray: n=%n ch='%c' pos=%n*n", n, ch, pos)
//writef("root2=  "); prnum(root2,nupb)
//writef("raddiv= "); prnum(raddiv,nupb)

  setzero(Inz,nupb)     // All entry points are in the plane z=0
  SWITCHON pos INTO
  { DEFAULT: RETURN
    CASE 0: setzero(Inx,nupb)
            TEST radius=0
            THEN setzero(Iny,nupb)
            ELSE copy(radius,nupb, Iny,nupb)
            ENDCASE
    CASE 1: copy(raddiv,nupb, Inx,nupb)
            copy(raddiv,nupb, Iny,nupb)
            ENDCASE
    CASE 2: copy(radius,nupb, Inx,nupb)
            setzero(Iny,nupb)
            ENDCASE
    CASE 3: copy(raddiv,nupb, Inx,nupb)
            copy(raddiv,nupb, Iny,nupb); Iny!0 := TRUE
            ENDCASE
    CASE 4: setzero(Inx,nupb)
            copy(radius,nupb, Iny,nupb); Iny!0 := TRUE
            ENDCASE
    CASE 5: copy(raddiv,nupb, Inx,nupb); Inx!0 := TRUE
            copy(raddiv,nupb, Iny,nupb); Iny!0 := TRUE
            ENDCASE
    CASE 6: copy(radius,nupb, Inx,nupb); Inx!0 := TRUE
            setzero(Iny,nupb)
            ENDCASE
    CASE 7: copy(raddiv,nupb, Inx,nupb); Inx!0 := TRUE
            copy(raddiv,nupb, Iny,nupb)
            ENDCASE
  }

//writef("*nIncident ray intersection with the plane z=0*n")
//writef("Inx=   "); prnum(Inx,nupb)
//writef("Iny=   "); prnum(Iny,nupb)
//writef("Inz=   "); prnum(Inz,nupb)

//writef("*nDirection of the incident ray*n")
//writef("dir!0= "); prnum(dir!0,nupb)
//writef("dir!1= "); prnum(dir!1,nupb)
//writef("dir!2= "); prnum(dir!2,nupb)

  //writef("*nEntry point %c%n, Direction %n, Blue*n", ch, pos, n)
  focalx, focaly := px!0, py!0 // Location of a blue dot
  raytrace(dir, @Inx, crownblue, flintblue, focalx, focaly)

  //newline()
  //writef("%c%n Blue x= ",ch,pos); prnum(focalx,nupb)
  //writef("%c%n Blue y= ",ch,pos); prnum(focaly,nupb)
 
  setcolour(c_blue)
  //IF pos<4 DO
    drawdot(scale(focalx), scale(focaly))
//abort(2000)
//RETURN
  //writef("*nEntry point %c%n, Direction %n, Red*n", ch, pos, n)

  TEST ch='C'
  THEN focalx, focaly := px!1, py!1 // Location of a red dot for C
  ELSE focalx, focaly := px!8, py!8 // Location of a red dot for A or B
  raytrace(dir, @Inx, crownred, flintred, focalx, focaly)

  //newline()
  //writef("%c%n Red  x= ",ch,pos); prnum(focalx,nupb)
  //writef("%c%n Red  y= ",ch,pos); prnum(focaly,nupb)
 
  setcolour(c_red)
  //IF pos<4 DO
    drawdot(scale(focalx), scale(focaly))

  IF tracing DO
    abort(5000)
}

AND scale(num) = VALOF
{ LET res = ?
  LET e = num!1
//writef("scale: num= "); prnum(num, 5)
  IF e>1 DO res := 9999_9999
  IF e=1 DO res := num!2 * 10000 + num!3
  IF e=0 DO res := num!2
  IF e<-1 DO res := 0
  IF num!0 DO res := -res
  // res is in unit of 1/10000 mm
  res := res/100
  // res is now in unit of 1/100 mm
//writef("scale: res=%n*n", res)
  RESULTIS res
}

