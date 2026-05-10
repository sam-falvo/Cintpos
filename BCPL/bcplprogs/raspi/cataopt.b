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
using high precisiom floating point numbers with with about 60
significant decimal digits while allowing the library functions to use
upto 72. The precision constants can be changed easily if different
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
surfaces of the mirror. The effective focal length of the telescope is
dependent on the radii of its optical surfaces and if the focal plane
is at the position of the objective lens and the mirror is 500mm away
then the focal length can be anything form 500mm to 1000mm. The
optimisation techniques tries to mininise the size of spots caused by
light arriving at different directions but this tends shorter focal
lengths. To allows us to optimise a telescope with a focal length of
1000mm we make it attempt to minimise the distance between points
belonging to a spot and the position in the focal plane where the spot
should be located. For this focal length the image of a star 1/4
degree from the axis should be centred at appproximately
1000*(1/60)*(1/4)=4.166mm from the axis. To achieve this the
separation between the objective lens and the mirror must le larger
than 500mm. This program uses a separation of 700mm. It turns out that
for a well chosen initial setting the program can find a near optimum
design for the telescope. If we can obtain a design that causes the
scattering to be no larger than the size of the corresponding airy
disk for a telescope with aperture 100mm and focal length 1000mm,
further optimization will not improve its resolution.

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

// Compile the arith library as a separate section.
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

  spotmag          // This specified the magnification of spot
                   // drawn by drawdot.
  pausing          // Pause when the geometry improves
  tracing
  reduced          // =TRUE when the geometry improves

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

  MirrorRadius // Actual mirror radius

  D    // The distance between the objective and mirror.
       // Typically about 700mm
  F    // The z coordinate of the focus plane. Typically F=0

  deltaR1; R1ch
  deltaR2; R2ch
  deltaR3; R3ch
  deltaR4; R4ch
  
  factor           // A power of ten used in computing the next delta
  initfactor       // This hold the value of the f argument
  
  spotsize         // Current spot size or -1

  dist             // Distance from y average to theoretical y centre.
  spotvalue        // Set to spotsize+5*dist
  bestspotvalue    // The best spotvalue so far
  bestdist         // Set to dist of the best spotvalue.
  bestspotsize     // Set to spotsize of the best spotvalue.

  // Directions at small angles in the y-z plane
  dir0cx; dir0cy; dir0cz // Parallel to the telescope axis.
  dir1cx; dir1cy; dir1cz // about 1/8 degrees of the axis.
  dir2cx; dir2cy; dir2cz // about 1/4 degrees off the axis.

  Inx; Iny; Inz    // Coordinates of the entry point in plane z=0
  Outx; Outy; Outz // Coordinates of the exit point on the front
                   // surface of the mirror.
  outdircx; outdircy; outdircz // Direction of the out ray

  Arad  // Radius of the A circle in the objective, typically 50mm
  Brad  // Radius of the B circle in the objective, typically 25mm

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
  
  geometrystream // Used when reading or writing catageometry.txt
                 // This holds the latest setting of R1 to R4.

  // The refractive indices of crown and flint glass
  // for both air to glass and glass to air.
  crownblueindex; crownblueinvindex
  crownredindex;  crownredinvindex
  flintblueindex; flintblueinvindex
  flintredindex;  flintredinvindex

  centrex  // Centre of the SDL screen
  centrey
}

/*

The z axis is the axis of the telescope in direction from the
objective lens to mirror. The x axis is to the right when viewing the
objective in the z direction, and y is upwards. The origin is on the z
axis in the plane where the objective front and rear surfaces meet. The
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
  //                value in the range 0 to 1.0

  spacevupb = 10000

  Blue=1     // Specifying colours used in raytrace.
  Red=2
}

LET drawdot(dir, x, y) BE
{ // Draw a 3x3 dot at (x,y) relative to (centrex,centrey)
  // x and y are in units of 1/10000mm.
  // dir = 0, 1 or 2 specifying the direction of the
  // incoming ray.
  // Direction 1 the image spot on the focal plane is centred
  // at x=0 and y = - 1000*/(8*60) = -2.0833mm
  // For direction 2 the y coordinate is -4.1666mm

  LET spotcentrex = centrex
  LET spotcentrey = centrey - muldiv(screenysize, 2_0833*dir, 8_0000)

  //writef("x=%9.4d    y=%9.4d dir=%n spotmag=%n*n", x, y, dir, spotmag)
  //writef("Spot centre = (%i4, %i4) in screen coordinates*n",
  //        spotcentrex, spotcentrey)

  // Place the dot relative to the origin
  //writef("x=%9.4d    y=%9.4d*n", x, y)
  y := y + 2_0833*dir
  //writef("x=%9.4d    y=%9.4d relative to spot centre*n", x, y)

  // Magnify the spot dot
  x, y := x * spotmag, y * spotmag
  //writef("x=%9.4d    y=%9.4d relative to spot centre after magnification*n", x, y)

  // These are in units of 1/10000mm relative to the spot centre.

  // Convert to screen coordinates assuming screenysize is
  // equivalent to 8mm
  x := spotcentrex + (screenysize * x) / 8_0000 
  y := spotcentrey + (screenysize * y) / 8_0000 
  //writef("x=%i6    y=%i6 in screen coordinates*n", x, y)

  drawfillrect(x-1, y-1, x+1, y+1) // Draw a 3x3 dot

  setcolour(c_black)
  moveto(spotcentrex-10, spotcentrey)
  drawto(spotcentrex+10, spotcentrey)

  updatescreen()
  IF tracing DO
  {  writef("drawdot: x=%n y=%n*n", x,y)
     //abort(1077)
  }
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
  fillsurf(c_gray)
  updatescreen()
}

LET start() = VALOF
{ LET argv = VEC 50
  LET str = VEC 255/bytesperword
  currentline := str

  stdin  := input()
  stdout := output()

  UNLESS rdargs("mag/n,n/n,f/n,-p/s,-t/s", argv, 50) DO
  { writef("Bad arguments for cataopt*n")
    RESULTIS 0
  }

  spotmag := 20
  IF argv!0 DO spotmag := !argv!0       // mag/n

  iterations := 1000
  IF argv!1 DO iterations := !argv!1    // n/n

  initfactor := 0
  IF argv!2 DO initfactor := !argv!2    // f/n
  
  pausing := argv!3                     // -p/s
  tracing := argv!4                     // -t/s

  spacev := getvec(spacevupb)
  spacet := spacev + spacevupb
  spacep := spacet

  UNLESS spacev DO
  { writef("*nERROR: More memory is needed*n")
    RESULTIS 0
  }

  //writef("*niterations = %n*n", iterations)

  initscreen()
  fillsurf(c_gray)
  setcolour(c_black)

  // Analyse the catadioptric telescope, hopefully optimising its
  // geometry.

  telescope()

  IF spacev DO
  { writef("Space used is %n out of %n*n", spacet-spacep, spacevupb) 
    freevec(spacev)
  }

  RESULTIS 0
}

AND intersect(dir, P, c, r, t1, t2) = VALOF
{ // This calculates the intersection points of a line and
  // a sphere.
  // dir!0,dir!1,dir!2 are the direction cosines of the line
  // P!0,P!1,P!2 is a point on it.
  // c is the z coordinate of the centre of the lens surface
  // r is the radius of the lens surface.
  // P+t1*dir and P+t2*dir are the intersection points, if any.
  // The result is TRUE is t1 and t2 exist.

  // All the numbers above have upperbound nupb.

  // A point on the line has coordinates
  // x = P!0 + dir!0*t 
  // y = P!1 + dir!1*t 
  // z = P!2 + dir!2*t

  // These must be on the surface of the sphere, so
  // x^2 + y^2 + (z-c)^2 = r^2

  // This gives a quadratic of the form At^2 + Bt + C = 0 

  // where

  // A = dx^2 + dy%2 + dz^2 = 1

  // B = 2(xdx +ydy + (z-c)dz)

  // C = x^2 + y^2 + (z-c)^2 - r^2

  // giving the solutions:  t = (-B +/- sqrt(B^2 - 4AC))/2A
  // and since A=1          t = (-B +/- sqrt(B^2 - 4C))/2
  LET x    = P!0
  LET y    = P!1
  LET z    = P!2
  LET dx   = dir!0
  LET dy   = dir!1
  LET dz   = dir!2
  LET tmp1 = VEC numupb
  LET tmp2 = VEC numupb
  LET tmp3 = VEC numupb
  LET tmp4 = VEC numupb
  LET tmp5 = VEC numupb
  LET B    = VEC nupb
  LET C    = VEC nupb

  //newline()
  //writef("intersect*n")
  //writef("Calculating B = 2(xdx +ydy + (z-c)dz)*n")

  mul(x,nupb, dx,nupb, tmp1,numupb)       // tmp1 = x dx
  //writef("x:                  "); prnum(x,nupb)
  //writef("dx:                 "); prnum(dx,nupb)
  //writef("xdx:                "); prnum(tmp1,numupb)
  mul(y,nupb, dy,nupb, tmp2,numupb)       // tmp2 = y dy
  //writef("y:                  "); prnum(y,nupb)
  //writef("dy:                 "); prnum(dy,nupb)
  //writef("ydy:                "); prnum(tmp2,numupb)
  sub(z,nupb, c,nupb, tmp3,numupb)        // tmp3 = z - c
  //writef("z:                  "); prnum(z,nupb)
  //writef("c:                  "); prnum(c,nupb)
  //writef("z-c:                "); prnum(tmp3,numupb)
  //writef("dz:                 "); prnum(dz,nupb)
  mul(tmp3,numupb, dz,nupb, tmp4,numupb)  // tmp4 = (z - c)dz
  //writef("(z-c)dz:            "); prnum(tmp4,numupb)

//writef("Calling add(tmp1,numupb, tmp2,numupb, tmp3,numupb)*n")
  //writef("xdx:                "); prnum(tmp1,numupb)
  //writef("ydy:                "); prnum(tmp2,numupb)
  add(tmp1,numupb, tmp2,numupb, tmp3,numupb)  // tmp3 = xdx + ydy
  //writef("xdx:                "); prnum(tmp1,numupb)
  //writef("ydy:                "); prnum(tmp2,numupb)
  //writef("xdx+ydy:            "); prnum(tmp3,numupb)
//abort(1111)
  add(tmp3,numupb, tmp4,numupb, B,nupb)     // B = xdx + ydy + (z - c)dz
  //writef("xdx+ydy+(z-c)dz:    "); prnum(B,nupb)
  mulbyk(2, B,nupb)           // B = 2(xdx + ydy + (z - c)dz)
  //writef("2(xdx+ydy+(z-c)dz): "); prnum(B,nupb)
//abort(6653)

  //writef("*nCalculating C = x^2 + y^2 + (z-c)^2 - r^2*n")

  mul(x,nupb, x,nupb, tmp1,numupb)        // tmp1 = x^2
  //writef("x^2:                "); prnum(tmp1,numupb)
  mul(y,nupb, y,nupb, tmp2,numupb)        // tmp2 = y^2
  //writef("y^2:                "); prnum(tmp2,numupb)
  sub(z,nupb, c,nupb, tmp3,numupb)        // tmp3 = z-c
  //writef("z:                  "); prnum(z,nupb)
  //writef("c:                  "); prnum(c,nupb)
  //writef("z-c:                "); prnum(tmp3,numupb)
  mul(tmp3,nupb, tmp3,nupb, tmp4,nupb)    // tmp4 = (z-c)^2
  //writef("(z-c)^2:            "); prnum(tmp4,numupb)
  mul(r,nupb, r,nupb, tmp5,numupb)        // tmp5 = r^2
  //writef("r:                  "); prnum(r,nupb)
  //writef("r^2:                "); prnum(tmp5,numupb)
  add(tmp1,numupb, tmp2,numupb, C,nupb)   // C = x^2+y^2
  //writef("x^2+y^2:            "); prnum(C,nupb)
  add(C,nupb, tmp4,numupb, tmp1,numupb)   // tmp1 = x^2+y^2+(z-c)^2
  //writef("x^2+y^2+(z-c)^2:    "); prnum(tmp1,numupb)
  sub(tmp1,numupb, tmp5,numupb, C,nupb)   // C = x^2+y^2+(z-c)^2-r^2
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

  mul(B,nupb, B,nupb, tmp1,numupb)        // tmp1 = B^2
  //writef("B^2 =               "); prnum(tmp1,numupb)
  mulbyk(4, C,nupb)                       // C = 4C
  //writef("4C =                "); prnum(C,nupb)
  sub(tmp1,numupb, C,nupb, tmp2,numupb)   // tmp2 = B^2 - 4C
  //writef("B^2-4C =            "); prnum(tmp2,numupb)
  sqrt(tmp2,numupb, tmp3,numupb)          //tmp3 = sqrt(B^2 - 4C)
  //writef("sqrt(B^2-4C) =      "); prnum(tmp3,numupb)
  neg(B,nupb)                             // B = -B
  //writef("-B =                "); prnum(B,nupb)
  //writef("sqrt(B^2-4C) =      "); prnum(tmp3,numupb)
  sub(B,nupb, tmp3,numupb, t1,nupb)
  divbyk(2, t1,nupb)                // t1 = (-B - sqrt(B^2 - 4C))/2
  //writef("t1 =       "); prnum(t1,nupb)
  //writef("sqrt(B^2-4C) =      "); prnum(tmp3,numupb)
  add(B,nupb, tmp3,numupb, t2,nupb)
  divbyk(2, t2,nupb)                // t2 = (-B + sqrt(B^2 - 4C))/2
  //writef("t2 =       "); prnum(t2,nupb)
//abort(6655)
  RESULTIS TRUE
}

AND refract(indir, P, c, invindex, outdir) = VALOF
{ // indir!0,indir!1,indir!2 are the direction cosines
  //                         of the in ray.
  // P!0,P!1,P!2 are the coordinates of the entry
  //             point P on the lens surface.
  // c      is the z coordinate of the lens surface centre.
  // invindex  is the inverse of the refactive index air to glass.
  // outdir!0,outdir!1,outdir!2 will be the direction cosines
  //                            of the out ray.
  // All numbers have upper bound nupb.
  // The result is TRUE if refract is successful.

  LET indx = indir!0 // The direction cosines of the in ray.
  AND indy = indir!1
  AND indz = indir!2

  LET Px = P!0 // The coordinates of the entry point.
  AND Py = P!1
  AND Pz = P!2

  LET outdx = outdir!0 // To hold the direction cosines of the out ray.
  AND outdy = outdir!1
  AND outdz = outdir!2

  LET ndx      = VEC nupb   // The surface normal direction cosines
  AND ndy      = VEC nupb   // with the same z sign as for indir.
  AND ndz      = VEC nupb

  AND costheta = VEC numupb // The in ray
  AND sintheta = VEC numupb
  AND cosphi   = VEC numupb // The out ray
  AND sinphi   = VEC numupb

  LET P1x      = VEC nupb   // The point P1 on the in ray
  AND P1y      = VEC nupb   // costheta away from P
  AND P1z      = VEC nupb

  LET P2x      = VEC nupb   // The point P2 on the in normal
  AND P2y      = VEC nupb   // at distance 1 from P
  AND P2z      = VEC nupb

  LET Q1x      = VEC nupb   // The point Q1 on the out normal
  AND Q1y      = VEC nupb   // cosphi away from P
  AND Q1z      = VEC nupb

  LET Q2x      = VEC nupb   // The point Q2 on the out ray
  AND Q2y      = VEC nupb   // at distance 1 from P
  AND Q2z      = VEC nupb

  // Note that P-P1-P2 and P-Q1-Q2 are both right
  // angle triangles lying in the same plane, and that
  // P1-P2 has length sintheta and Q1-Q2 has length sinphi.
  // By Snell's law, the ratio of these lengths is the
  // refractive index.
  // ie sintheta = sinphi / index = sinphi * invindex

  AND tmp1 = VEC numupb
  AND tmp2 = VEC numupb
  AND tmp3 = VEC numupb
  AND tmp4 = VEC numupb

  copy(Px,nupb,         ndx,nupb)
  copy(Py,nupb,         ndy,nupb)
  sub (Pz,nupb, c,nupb, ndz,nupb)
  // (Px,Py,Pz are the coordinates of
  // the entry point on the lens. 
  // (ndx,ndy,ndz) is a vector parallel to the normal.

  // Ensure that the normal and indir have the same z sign,
  // negating (ndx,ndy,ndz) if necessary.
  UNLESS indz!0=ndz!0 DO
  { // The z signs are different, so negate the normal.
    //writef("indz=    "); prnum(indz,8)
    //writef("ndz=     "); prnum(ndz,8)
    //writef("So negate the normal*n")
    neg(ndx)
    neg(ndy)
    neg(ndz)
  }

  //writef("*nIn refract applying Snell's law*n")
  //writef("Coordinates of the entry point on the lens surface*n")
  //writef("Px=   "); prnum(Px,8)
  //writef("Py=   "); prnum(Py,8)
  //writef("Pz=   "); prnum(Pz,8)

  //writef("ndx=      "); prnum(ndx,8)
  //writef("ndy=      "); prnum(ndy,8)
  //writef("ndz=      "); prnum(ndz,8)

  //writef("*nCalling normalize*n")
  normalize(@ndx,nupb) // Direction cosines of the normal
  //writef("The direction cosines of the normal at this point*n")
  //writef("with the z component having the same sign as*n")
  //writef("the z coordinate of the incident ray*n")
  //writef("ndx=      "); prnum(ndx,8)
  //writef("ndy=      "); prnum(ndy,8)
  //writef("ndz=      "); prnum(ndz,8)
  // (ndx,ndy,ndz) are now direction cosines.

  //writef("indx=     "); prnum(indx,8)
  //writef("indy=     "); prnum(indy,8)
  //writef("indz=     "); prnum(indz,8)
  //writef("indz and ndz should have the same signs*n")
  //newline()
  inprod(indir,nupb, @ndx,nupb, costheta,numupb)
  // theta is the angle between the incident ray and the normal.
  //writef("costheta= "); prnum(costheta,numupb)
  mul(costheta,numupb, costheta,numupb, tmp1,numupb)
  //writef("costheta^2= "); prnum(tmp1,8)
//abort(987)
  sub(one,nupb, tmp1,numupb, tmp3,numupb)
  //writef("1-costheta^2= "); prnum(tmp3,8)
  sqrt(tmp3,numupb, sintheta,numupb)
  //writef("sintheta is sqrt(1 - costheta^2)*n")
  //writef("sintheta= "); prnum(sintheta,8)

  mul(sintheta,numupb, invindex,nupb, sinphi,numupb) // Apply Snell's law
  //writef("Snell's law gives:*n")
  //writef("sinphi=   "); prnum(sinphi,8)

  mul(sinphi,numupb, sinphi,numupb, tmp1,numupb)
  sub(one,nupb, tmp1,numupb, tmp3,numupb)
  sqrt(tmp3,numupb, cosphi,numupb)
  //writef("cosphi is sqrt(1 - sinphi^2)*n")
  //writef("cosphi=   "); prnum(cosphi,8)

  // P1 is on the in ray at distance 1 from P
  sub(Px,nupb, indx,nupb, P1x,nupb)
  sub(Py,nupb, indy,nupb, P1y,nupb)
  sub(Pz,nupb, indz,nupb, P1z,nupb)

  //writef("*nPoint P1 is on the in ray at distance 1 from P*n")
  //writef("P1x=      "); prnum(P1x,8)
  //writef("P1y=      "); prnum(P1y,8)
  //writef("P1z=      "); prnum(P1z,8)

  // Point P2 is on the normal at distance costheta from P
  //writef("*nPoint P2 is on the normal at distance costheta from P*n")
  //newline()
  mul(ndx,nupb, costheta,numupb, tmp1,numupb)
  sub(Px,nupb, tmp1,numupb, P2x,nupb)
  //writef("tmp1=     "); prnum(tmp1,8)
  //writef("P2x=      "); prnum(P2x,8)
  mul(ndy,nupb, costheta,numupb, tmp1,numupb)
  sub(Py,nupb, tmp1,numupb, P2y,nupb)
  //writef("tmp1=     "); prnum(tmp1,8)
  //writef("P2y=      "); prnum(P2y,8)
  mul(ndz,nupb, costheta,numupb, tmp1,numupb)
  sub(Pz,nupb, tmp1,numupb, P2z,nupb)
  //writef("tmp1=     "); prnum(tmp1,8)
  //writef("P2z=      "); prnum(P2z,8)
  //writef("P1 and P2 should both be on the same side of the lens surface*n")
  //newline()

  // Point Q1 is on the normal at distance cosphi from P.
  //writef("*nPoint Q1 is on the normal at distance cosphi from P*n")
  mul(ndx,nupb, cosphi,numupb, tmp1,numupb)
  add(Px,nupb, tmp1,numupb, Q1x,nupb)
  mul(ndy,nupb, cosphi,numupb, tmp1,numupb)
  add(Py,nupb, tmp1,numupb, Q1y,nupb)
  mul(ndz,nupb, cosphi,numupb, tmp1,numupb)
  add(Pz,nupb, tmp1,numupb, Q1z,nupb)
  //newline()
  //writef("Q1x=      "); prnum(Q1x,8)
  //writef("Q1y=      "); prnum(Q1y,8)
  //writef("Q1z=      "); prnum(Q1z,8)

  // Calculate Q2 = Q1 + (P2-P1)*invindex
  //writef("*nPoint Q2 = Q1 + (P2-P1)**invindex*n")
  sub(P2x,nupb, P1x,nupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,8)
  mul(tmp1,numupb, invindex,nupb, tmp2,numupb)
  //writef("tmp2=     "); prnum(tmp2,8)
  add(Q1x,nupb, tmp2,numupb, Q2x,nupb) // Q2x = Q1x + (P2x-P1x)*invindex
  //writef("Q2x = Q1x + (P2x-P1x)/index*n")
  //writef("Q2x=      "); prnum(Q2x,8)

  sub(P2y,nupb, P1y,nupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,8)
  mul(tmp1,numupb, invindex,nupb, tmp2,numupb)
  //writef("tmp2=     "); prnum(tmp2,8)
  add(Q1y,nupb, tmp2,numupb, Q2y,nupb) // Q2y = Q1y + (P2y-P1y)*invindex
  //writef("Q2y = Q1y + (P2y-P1y)/index*n")
  //writef("Q2y=      "); prnum(Q2y,8)

  sub(P2z,nupb, P1z,nupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,8)
  mul(tmp1,numupb, invindex,nupb, tmp2,numupb)
  //writef("tmp2=     "); prnum(tmp2,8)
  add(Q1z,nupb, tmp2,numupb, Q2z,nupb) // Q2x = Q1z + (P2z-P1z)*invindex
  //writef("Q2z = Q1z + (P2z-P1z)/index*n")
  //writef("Q2z=      "); prnum(Q2z,8)
//abort(980)

  //writef("Q2x=      "); prnum(Q2x,8)
  //writef("Q2y=      "); prnum(Q2y,8)
  //writef("Q2z=      "); prnum(Q2z,8)
//abort(981)

  //writef("Px=   "); prnum(Px,8)
  //writef("Py=   "); prnum(Py,8)
  //writef("Pz=   "); prnum(Pz,8)

  //writef("*nCalculating the out direction cosines*n*n")

  //newline()
  sub(Q2x,nupb, Px,nupb, outdx,nupb)
  sub(Q2y,nupb, Py,nupb, outdy,nupb)
  sub(Q2z,nupb, Pz,nupb, outdz,nupb)
  //writef("outdx=  "); prnum(outdx,8)
  //writef("outdy=  "); prnum(outdy,8)
  //writef("outdz=  "); prnum(outdz,8)
  //writef("About to call normalize*n")
//abort(6543)
  normalize(outdir,nupb)


  //writef("*nThe out ray direction cosines are:*n")
  //writef("outdx=    "); prnum(outdx,8)
  //writef("outdy=    "); prnum(outdy,8)
  //writef("outdz=    "); prnum(outd!z,8)
//abort(6655)
  RESULTIS TRUE
}

AND reflect(indir, P, c, outdir) = VALOF
{ // This computes the direction cosines of a reflected ray.
  // indir!0,indir!1,indir!2  hold the direction cosines of the in ray.
  // P!0,P!1,P!2 hold the coordinates of the intersection point on
  //             the mirror surface.
  // c           is the z coordinate of the centre of the mirror surface.
  // outdir!0,outdir!1,outdir!2 will hold the direction cosines
  //                            of the reflected ray.
  // All the numbers above have upperbound nupb.
  // The result is TRUE if the reflection is successful.
  LET costheta = VEC numupb

  LET indx = indir!0 // The direction cosines of the incident ray.
  AND indy = indir!1
  AND indz = indir!2

  LET Px = P!0 // The coordinates of the entry point.
  AND Py = P!1
  AND Pz = P!2

  LET outdx = outdir!0 // To hold the out direction cosines.
  AND outdy = outdir!1
  AND outdz = outdir!2

  LET Nx = VEC nupb // The direction cosines of the normal
  AND Ny = VEC nupb // at the intersection point P.
  AND Nz = VEC nupb

  LET Ax = VEC nupb // The coordinates of the point on the
  AND Ay = VEC nupb // inray one unit from P
  AND Az = VEC nupb

  LET Bx = VEC nupb // The coordinates of the point on the
  AND By = VEC nupb // the normal costheta from P
  AND Bz = VEC nupb

  LET Cx = VEC nupb // The coordinates of the point on the
  AND Cy = VEC nupb // reflected ray one unit from P.
  AND Cz = VEC nupb // Note B is the mid point of A and C.

  AND tmp1 = VEC numupb

//writef("*nreflect: nupb=%n*n",nupb)
  //writef("Px=       "); prnum(Px,8)
  //writef("Py=       "); prnum(Py,8)
  //writef("Pz=       "); prnum(Pz,8)
  //writef("c=        "); prnum(c,8)
  // Compute the normal.
//writef("Before calling normalize*n")
  copy(Px,nupb,        Nx,nupb)
  //writef("Nx=       "); prnum(Nx,8)
  copy(Py,nupb,        Ny,nupb)
  //writef("Ny=       "); prnum(Ny,8)
  sub(Pz,nupb, c,nupb, Nz,nupb)
  //writef("Nz=       "); prnum(Nz,8)
  normalize(@Nx,nupb)
//writef("After calling normalize*n")
  //writef("Nx=       "); prnum(Nx,8)
  //writef("Ny=       "); prnum(Ny,8)
  //writef("Nz=       "); prnum(Nz,8)

  // Calculate the coordinates of A = P - indir
  sub(Px,nupb, indx,nupb, Ax,nupb)
  sub(Py,nupb, indy,nupb, Ay,nupb)
  sub(Pz,nupb, indz,nupb, Az,nupb)
  //writef("Ax=       "); prnum(Ax,8)
  //writef("Ay=       "); prnum(Ay,8)
  //writef("Az=       "); prnum(Az,8)

  // Calculate the coordinated of B = point - N * costheta
  inprod(indir,nupb, @Nx,nupb, costheta,numupb)
  //writef("costheta= "); prnum(costheta,8)
  mul(Nx,nupb, costheta,numupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,8)
  sub(Px,nupb, tmp1,numupb, Bx,nupb)
  //writef("Bx=       "); prnum(Bx,8)
  mul(Ny,nupb, costheta,numupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,8)
  sub(Py,nupb, tmp1,numupb, By,nupb)
  //writef("By=       "); prnum(By,8)
  mul(Nz,nupb, costheta,numupb, tmp1,numupb)
  //writef("tmp1=     "); prnum(tmp1,8)

  sub(Pz,nupb, tmp1,numupb, Bz,nupb)
  //writef("Bx=       "); prnum(Bx,8)
  //writef("By=       "); prnum(By,8)
  //writef("Bz=       "); prnum(Bz,8)
  
  // Calculate the coordinates of C = 2B - A
  mulbyk(2, Bx,nupb)
  mulbyk(2, By,nupb)
  mulbyk(2, Bz,nupb)
  sub(Bx,nupb, Ax,nupb, Cx,nupb)
  sub(By,nupb, Ay,nupb, Cy,nupb)
  sub(Bz,nupb, Az,nupb, Cz,nupb)
  //writef("Cx=       "); prnum(Cx,8)
  //writef("Cy=       "); prnum(Cy,8)
  //writef("Cz=       "); prnum(Cz,8)

  // Calculate the direction of the reflected ray normalize(C - P)

  sub(Cx,nupb, Px,nupb, outdx,nupb)
  sub(Cy,nupb, Py,nupb, outdy,nupb)
  sub(Cz,nupb, Pz,nupb, outdz,nupb)
  normalize(outdir,nupb)

  RESULTIS TRUE
}

AND raytrace(indir, P, colour, focalx, focaly) = VALOF
{ // Trace a ray all the way through the telescope
  // indir!0,indir!1,indir!2 are the dirction cosines of the
  //                         incoming ray.
  // P!0,P!1,P!2 are the coordinates of a point on the incoming ray.
  // colour is either Blue or Red.
  // focalx,focaly are the coordinates in the focal plane resulting
  //               from the incoming ray.
  // The result is TRUE if the raytracing was successful.

  // The ray passes through the following
  //  (1) the front surface of the objective lens, crown glass
  //  (2) the rear surface of the objective lens
  //  (3) the front surface of the mirror, flint glass
  //  (4) the reflective surface of the mirror
  //  (5) back through the front surface of the mirror

  LET t1    = VEC nupb
  LET t2    = VEC nupb
  LET tmp1  = VEC numupb
  LET tmp2  = VEC numupb
  LET tmp3  = VEC numupb

  LET indx  = VEC nupb // Private in direction cosines
  AND indy  = VEC nupb
  AND indz  = VEC nupb

  LET inptx = VEC nupb // Point on an in ray
  AND inpty = VEC nupb
  AND inptz = VEC nupb

  LET x     = VEC nupb // For intersection points
  AND y     = VEC nupb
  AND z     = VEC nupb

  LET outdx = VEC nupb // Direction cosines of an out ray.
  AND outdy = VEC nupb
  AND outdz = VEC nupb

  AND invindex = ?   // The inverse of the refractive index of
                     // the current surface depending on colour
                     // and crown or flint glass.

  //writef("*nraytrace entered*n")

  // Front surface of the objective lens

  copy(indir!0,nupb, indx,nupb) // In direction to front surface of objective
  copy(indir!1,nupb, indy,nupb)
  copy(indir!2,nupb, indz,nupb)

  copy(P!0,nupb, inptx,nupb) // A point on the incident ray.
  copy(P!1,nupb, inpty,nupb)
  copy(P!2,nupb, inptz,nupb)

  //writef("indx=      "); prnum(indx,8)
  //writef("indy=      "); prnum(indy,8)
  //writef("indz=      "); prnum(indz,8)
  //writef("P!0:       "); prnum(P!0,8)
  //writef("P!1:       "); prnum(P!1,8)
  //writef("P!2:       "); prnum(P!2,8)

  UNLESS intersect(@indx, @inptx, C1, R1, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with front surface of objective successful*n")

  // Select the negative root
  IF t2!0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=          "); prnum(t1,8)

  IF tracing DO
    writef("*nObjective front surface intersection point (x,y,z) is:*n")
  
  //writef("indx=         "); prnum(indx,8)
  //writef("t1=           "); prnum(t1,8)
  mul(indx,nupb, t1,nupb, tmp1,numupb)
  //writef("indx**t1=      "); prnum(tmp1,8)
  //writef("inptx=        "); prnum(inptx,8)
  add(inptx,nupb, tmp1,numupb, x,nupb)
  IF tracing DO
  { writef("x=            "); prnum(x,8) }

  mul(indy,nupb, t1,nupb, tmp1,numupb)
  add(inpty,nupb, tmp1,numupb, y,nupb)
  IF tracing DO
  { writef("y=            "); prnum(y,8) }

  mul(indz,nupb, t1,nupb, tmp1,numupb)
  add(inptz,nupb, tmp1,numupb, z,nupb)
  IF tracing DO
  { writef("z=            "); prnum(z,8) }
  //newline()

  // Apply Snell's law to obtain the transmitted direction

  // indx,indy,indz hold the direction cosines of the in ray.
  // outdir will hold the direction cosines of the out ray.
  // (x,y,z) are the coordinates of the entry point on the
  //         objective lens front surface.
  // C1 is the z coordinate of the centre of the lens front surface.
  // invindex is the inverse of the refractive index.

  //newline()
  //writef("Applying Snell's law*n")
  //writef("indx=     "); prnum(indx,8)
  //writef("indy=     "); prnum(indy,8)
  //writef("indz=     "); prnum(indz,8)
  //writef("x=        "); prnum(x,8)
  //writef("y=        "); prnum(y,8)
  //writef("z=        "); prnum(z,8)

  // Set the air to glass refractive index for crown glass
  // depending on the colour.
  invindex := colour=Blue -> crownblueinvindex, crownredinvindex

  refract(@indx, @x, C1, invindex, @outdx)

  //writef("outdx=    "); prnum(outdx,8)
  //writef("outdy=    "); prnum(outdy,8)
  //writef("outdz=    "); prnum(outdz,8)
//abort(8877)


  // Now deal with the rear surface of the objective lens.

  //writef("*n*nDealing with the rear surface of the objective lens*n*n")

  copy(outdx,nupb, indx,nupb) // Out ray of front surface becomes
  copy(outdy,nupb, indy,nupb) // the in ray of the rear surface.
  copy(outdz,nupb, indz,nupb)

  copy(x,nupb, inptx,nupb)   // The intersection point on the front surface
  copy(y,nupb, inpty,nupb)   // is a point on the in ray of the rear surface.
  copy(z,nupb, inptz,nupb)

  UNLESS intersect(@indx, @inptx, C2, R2, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with rear surface of objective successful*n")
  // Select the positive root.
  UNLESS t2!0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=        "); prnum(t1,nupb)

  IF tracing DO
    writef("*nObjective rear surface intersection point (x,y,z) is:*n")
  
  //writef("indx=    "); prnum(indx,8)
  //writef("t1=   "); prnum(t1,8)
  mul(indx,nupb, t1,nupb, tmp1,numupb)
  //writef("indx**t1=   "); prnum(tmp1,8)
  //writef("inptx=    "); prnum(inptx,8)
  add(inptx,nupb, tmp1,numupb, x,nupb)
  IF tracing DO
  { writef("x=         "); prnum(x,8) }

  mul(indy,nupb, t1,nupb, tmp1,numupb)
  add(inpty,nupb, tmp1,numupb, y,nupb)
  IF tracing DO
  { writef("y=         "); prnum(y,8) }

  mul(indz,nupb, t1,nupb, tmp1,numupb)
  add(inptz,nupb, tmp1,numupb, z,nupb)
  IF tracing DO
  { writef("z=         "); prnum(z,8) }
  //newline()
//abort(8855)

  //newline()
  //writef("indx=     "); prnum(indx,8)
  //writef("indy=     "); prnum(indy,8)
  //writef("indz=     "); prnum(indz,8)
  //writef("x=        "); prnum(x,8)
  //writef("y=        "); prnum(y,8)
  //writef("z=        "); prnum(z,8)

  // Set the inverse of the glass to air refractive index for
  // crown glass depending on the colour.
  invindex := colour=Blue -> crownblueindex, crownredindex

  // Calculate the new out direction.
  refract(@indx, @x, C2, invindex, @outdx)

  //newline()
  //writef("outdx=     "); prnum(outdx,8)
  //writef("outdy=     "); prnum(outdy,8)
  //writef("outdz=     "); prnum(outdz,8)

//abort(8866)

  // Now deal with the front surface of the mirror.

  //writef("*n*nDealing with the front surface of the mirror*n*n")
  //writef("index=  "); prnum(index,nupb)

  copy(outdx,nupb, indx,nupb)
  copy(outdy,nupb, indy,nupb)
  copy(outdz,nupb, indz,nupb)

  copy(x,nupb, inptx,nupb)   // The intersection point on the
  copy(y,nupb, inpty,nupb)   // rear surface of the objective lens.
  copy(z,nupb, inptz,nupb)

  UNLESS intersect(@indx, @inptx, C3, R3, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with front surface of the mirror*n")
  // Select the positive root, one of t1 or t2 is positive.
  IF t1!0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=        "); prnum(t1,nupb)

  IF tracing DO
    writef("*nThe mirror front surface intersection point (x,y,z) is:*n")
  
  //writef("indx=    "); prnum(indx,nupb)
  //writef("t1=   "); prnum(t1,nupb)
  mul(indx,nupb, t1,nupb, tmp1,nupb)
  //writef("indx**t1=   "); prnum(tmp1,nupb)
  //writef("inptx=    "); prnum(inptx,nupb)
  add(inptx,nupb, tmp1,nupb, x,nupb) // x = inptx + t1*indx
  IF tracing DO
  { writef("x=         "); prnum(x, 8) }

  mul(indy,nupb, t1,nupb, tmp1,nupb)
  add(inpty,nupb, tmp1,nupb, y,nupb) // y = inpty + t1*indy
  IF tracing DO
  { writef("y=         "); prnum(y, 8) }

  mul(indz,nupb, t1,nupb, tmp1,nupb)
  add(inptz,nupb, tmp1,nupb, z,nupb) // z = inptz + t1*indz
  IF tracing DO
  { writef("z=         "); prnum(z, 8) }
  //newline()

  // Calculate the distance from the z axis.
  mul(x,nupb, x,nupb, tmp1,numupb)  //tmp1 = x^2
  mul(y,nupb, y,nupb, tmp2,numupb)  //tmp2 = y^2
  add(tmp1,numupb, tmp2,numupb, tmp3,numupb) // tmp3 = x^2 + y^2
  sqrt(tmp3,numupb, tmp1,numupb) // tmp1 = the radius

  //newline()
  //writef("Dist to z axis="); prnum(tmp1,8)

  IF numcmp(tmp1,numupb, MirrorRadius,nupb) > 0 DO
    copy(tmp1,numupb, MirrorRadius,nupb)

  IF tracing DO
  { writef("*nMirror radius= "); prnum(MirrorRadius,8) }
//abort(3100)


  //newline()
  //writef("indx=     "); prnum(indx,8)
  //writef("indy=     "); prnum(indy,8)
  //writef("indz=     "); prnum(indz,8)
  //writef("x=        "); prnum(x,8)
  //writef("y=        "); prnum(y,8)
  //writef("z=        "); prnum(z,8)

  // Set the air to glass refractive index for flint glass
  // depending on the colour.
  invindex := colour=Blue -> flintblueinvindex, flintredinvindex

  // Calculate the new out direction
  refract(@indx, @x, C3, invindex, @outdx)

  //newline()
  //writef("outdx=     "); prnum(outdx,8)
  //writef("outdy=     "); prnum(outdy,8)
  //writef("outdz=     "); prnum(outdz,8)

//abort(8867)

  // Now deal with the reflecting surface of the mirror.

  //writef("*n*nDealing with the reflecting surface of the mirror*n*n")

  copy(outdx,nupb, indx,nupb) // Out direction of the front surface is
  copy(outdy,nupb, indy,nupb) // the in direction to the silvered surface.
  copy(outdz,nupb, indz,nupb)

  copy(x,nupb, inptx,nupb)    // The intersection point on the front
  copy(y,nupb, inpty,nupb)    // surface of the mirror.
  copy(z,nupb, inptz,nupb)

  UNLESS intersect(@indx, @inptx, C4, R4, t1, t2) RESULTIS FALSE

  //writef("*nIntersection with reflective surface of the mirror*n")
  // Select the positive root. One of t1 or t2 is positive.
  IF t1!0 DO copy(t2,nupb, t1,nupb)
  //writef("t1=        "); prnum(t1,nupb)

  IF tracing DO
    writef("*nThe mirror reflective surface intersection point (x,y,z) is:*n")
  
  //writef("indx=    "); prnum(indx,nupb)
  //writef("t1=   "); prnum(t1,nupb)
  mul(indx,nupb, t1,nupb, tmp1,nupb)
  //writef("indx**t1=   "); prnum(tmp1,nupb)
  //writef("inptx=    "); prnum(inptx,nupb)
  add(inptx,nupb, tmp1,nupb, x,nupb)  // x = inptx + t1*indx
  IF tracing DO
  { writef("x=         "); prnum(x, 8) }

  mul(indy,nupb, t1,nupb, tmp1,nupb)
  add(inpty,nupb, tmp1,nupb, y,nupb)  // y = inpty + t1*indy
  IF tracing DO
  { writef("y=         "); prnum(y, 8) }

  mul(indz,nupb, t1,nupb, tmp1,nupb)
  add(inptz,nupb, tmp1,nupb, z,nupb)  // y = inptz + t1*indz
  IF tracing DO
  { writef("z=         "); prnum(z, 8) }
  //newline()

  // Calculate the new out direction.
  reflect(@indx, @x, C4, @outdx)

  //writef("outdx=     "); prnum(outdx,8)
  //writef("outdy=     "); prnum(outdy,8)
  //writef("outdz=     "); prnum(outdz,8)

//abort(8868)

  // Now deal with the front surface of the mirror again.

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
  //writef("t1=        "); prnum(t1,8)

  IF tracing DO
    writef("*nThe mirror front surface intersection point (x,y,z) is:*n")
  
  //writef("indx=    "); prnum(indx,8)
  //writef("t1=   "); prnum(t1,8)
  mul(indx,nupb, t1,nupb, tmp1,nupb)
  //writef("indx**t1=   "); prnum(tmp1,8)
  //writef("inptx=    "); prnum(inptx,8)
  add(inptx,nupb, tmp1,nupb, x,nupb)
  IF tracing DO
  { writef("x=         "); prnum(x, 8) }

  mul(indy,nupb, t1,nupb, tmp1,nupb)
  add(inpty,nupb, tmp1,nupb, y,nupb)
  IF tracing DO
  { writef("y=         "); prnum(y,8) }

  mul(indz,nupb, t1,nupb, tmp1,nupb)
  add(inptz,nupb, tmp1,nupb, z,nupb)
  IF tracing DO
  { writef("z=         "); prnum(z,8) }
  //newline()

  //writef("Mirror front    z= "); prnum(z,8)

  //newline()
  //writef("indx=     "); prnum(indx,8)
  //writef("indy=     "); prnum(indy,8)
  //writef("indz=     "); prnum(indz,8)
  //writef("x=        "); prnum(x,8)
  //writef("y=        "); prnum(y,8)
  //writef("z=        "); prnum(z,8)

  // Set the inverse of the glass to air refractive index for flint glass
  // depending on the colour.
  invindex := colour=Blue -> flintblueindex, flintredindex

  // Calculate the new out direction
  //writef("Calling refract*n")
  refract(@indx, @x, C3, invindex, @outdx)

  //newline()
  //writef("outdx=     "); prnum(outdx,8)
  //writef("outdy=     "); prnum(outdy,8)
  //writef("outdz=     "); prnum(outdz,8)

//abort(8868)

  TEST iszero(outdz,nupb)
  THEN { // In the exceptional case where outdz is zero,
         // focalx and focaly are just x and y.
         copy(x,nupb, focalx,nupb)
         copy(y,nupb, focaly,nupb)
       }
  ELSE { div(z,nupb, outdz,nupb, tmp1,numupb)
         mul(outdx,nupb, tmp1,numupb, tmp2,numupb)
         sub(x,nupb, tmp2,numupb, focalx,nupb)
         mul(outdy,nupb, tmp1,numupb, tmp2,numupb)
         sub(y,nupb, tmp2,numupb, focaly,nupb)
       }

  //IF tracing DO
  //{ newline()
  //  writef("Focalx= "); prnum(focalx,8)
  //  writef("Focaly= "); prnum(focaly,8)
  //  //abort(1888)
  //}
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

  LET delta = VEC nupb        // Current change in a radius value
  LET initdelta = VEC nupb    // Current initial setting of delta
  LET failcount = -1          // Unset

  // The idea is to try changing each radius R1 to R4 by a small amount delta
  // either positive or negative. The distance between the average y coordinates
  // of an image spot and the theoretical position of its centre assuming a
  // focal length of 1000mm is placed in dist. spotsize+dist is placed in
  // spotvalue giving a measure of how good the optics are. If spotvalue reduces,
  // the current delta values are doubled and a new setting of R1 to R4 tried,
  // otherwise start again with a new set of small random delta values, and
  // start again.

  // Every time spotvalue reduces the radii R1 to R4 are output to the
  // file catageometry.txt.

  // If the file catageometry.txt exists it is used to set the initial values
  // of R1 to R4.

  // Typically every time cataopt runs these radii are improved.

  // Directions 0, 1 and 2 are small angles in the y-z plane
  // used to generate test in rays.

  //writef("*nRay direction cosines for directions 0, 1 and 2*n*n")

  dir0cx := newnum(nupb) // Direction parallel to the telescope axis
  dir0cy := newnum(nupb)
  dir0cz := newnum(nupb)
  settok(0, dir0cx,nupb)
  settok(0, dir0cy,nupb)
  settok(1, dir0cz,nupb)

  //writef("dir0cx: "); prnum(dir0cx, 8)  // Direction 0
  //writef("dir0cy: "); prnum(dir0cy, 8)
  //writef("dir0cz: "); prnum(dir0cz, 8)
  //newline()

  dir1cx := newnum(nupb)
  dir1cy := newnum(nupb)
  dir1cz := newnum(nupb)
  settok(  0, dir1cx,nupb) // Direction about 1/8 degree off the telescope axis.
  settok( -1, dir1cy,nupb) // Note that 1 in 60 is about 1 degree
  settok(480, dir1cz,nupb) // so 1 in 480 is about 1/8 degree
  normalize(@dir1cx,nupb)

  //writef("dir1cx: "); prnum(dir1cx,8)  // Direction 1
  //writef("dir1cy: "); prnum(dir1cy,8)
  //writef("dir1cz: "); prnum(dir1cz,8)
  //newline()

  dir2cx := newnum(nupb)
  dir2cy := newnum(nupb)
  dir2cz := newnum(nupb)
  settok(  0, dir2cx,nupb) // Direction about 1/4 degree off the telescope axis.
  settok( -1, dir2cy,nupb) // This is a field of view about the size of the moon.
  settok(240, dir2cz,nupb) // Note that 1 in 60 is about 1 degree
                           // so 1 in 240 is about 1/4 degree
  normalize(@dir2cx,nupb)

  //writef("dir2cx: "); prnum(dir2cx,8)  // Direction 2
  //writef("dir2cy: "); prnum(dir2cy,8)
  //writef("dir2cz: "); prnum(dir2cz,8)
  //newline()

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

  MirrorRadius := newnum(nupb) // Radius of mirror

  D := newnum(nupb)    // The distance between the objective and mirror.
                       // This is typically 700mm.
  F := newnum(nupb)    // The z coordinate of the focus plane, typically F=0
                       // This typically give a focal length of 1000mm.

  Inx := newnum(nupb)  // The direction cosines of an in going ray to a
  Iny := newnum(nupb)  // lens or mirror surface.
  Inz := newnum(nupb)

  Outx := newnum(nupb) // The direction cosines of an out going ray to a
  Outy := newnum(nupb) // lens or mirror surface.
  Outz := newnum(nupb)

  outdircx := newnum(nupb)
  outdircy := newnum(nupb)
  outdircz := newnum(nupb)

  deltaR1 := newnum(nupb) // Used to make small changes to R1 to R4
  deltaR2 := newnum(nupb)
  deltaR3 := newnum(nupb)
  deltaR4 := newnum(nupb)

  root2 := newnum(nupb)
  one   := newnum(nupb)

  // Allocate numbers for the refractive indexes
  crownblueindex := newnum(nupb)
  crownredindex  := newnum(nupb)
  flintblueindex := newnum(nupb)
  flintredindex  := newnum(nupb)

  // Allocate numbers for the inverse refractive indexes
  crownblueinvindex := newnum(nupb)
  crownredinvindex  := newnum(nupb)
  flintblueinvindex := newnum(nupb)
  flintredinvindex  := newnum(nupb)

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

  bestspotsize   := newnum(nupb)
  spotsize       := newnum(nupb)
  dist           := newnum(nupb)
  bestdist       := newnum(nupb)
  bestspotvalue  := newnum(nupb)
  spotvalue      := newnum(nupb)

  UNLESS spotsize DO
  { writef("More space needed*n")
    abort(999)
    RETURN
  }

  // All numbers have been created successfully

  // Set the unchanginging geometry of the telescope

  settok(  4, T1,nupb)   // Objective lens thickness 4mm at centre.
  //writef("T1=     "); prnum(T1,5)

  settok(  4, T2,nupb)   // Mirror thickness 4mm at centre.
  //writef("T2=     "); prnum(T2,5)

  settok(700, D,nupb)    // z coordinate of the mirror silvered surface
  //writef("D=      "); prnum(D,5)

  settok(2, tmp1,nupb)
  sqrt(tmp1,nupb, root2,nupb)
  //writef("root2= "); prnum(root2,nupb)

  settok(1, one,nupb)

  // Refractive indices.
  // The objective glass is crown and the mirror glass is flint.

  //                crown        flint
  // blue 486 nm    1.51690      1.6321
  // red  640 nm    1.50917      1.6161

  str2num("1.51690", crownblueindex,nupb)
  str2num("1.50917", crownredindex,nupb)
  str2num("1.6321",  flintblueindex,nupb)
  str2num("1.6161",  flintredindex,nupb)

  inv(crownblueindex,nupb, crownblueinvindex,nupb)
  inv(crownredindex,nupb,  crownredinvindex,nupb)
  inv(flintblueindex,nupb, flintblueinvindex,nupb)
  inv(flintredindex,nupb,  flintredinvindex,nupb)

  IF tracing DO
  { writef("crownblueindex=     "); prnum(crownblueindex, 6)
    writef("crownredindex=      "); prnum(crownredindex,  6)
    writef("flintblueindex=     "); prnum(flintblueindex, 6)
    writef("flintredindex=      "); prnum(flintredindex,  6)

    writef("crownblueinvindex=  "); prnum(crownblueinvindex, 6)
    writef("crownredinvindex=   "); prnum(crownredinvindex,  6)
    writef("flintblueinvindex=  "); prnum(flintblueinvindex, 6)
    writef("flintredinvindex=   "); prnum(flintredinvindex,  6)
//abort(5678)
  }

  // Initialize the setting of the lens and mirror surface radii.
  // These are overwritten if file catagemetry.txt exists.

  // It seems that the initial settings of R1 to R4 often cause
  // the iterations to lead to a false minimum. So some
  // experimention was needed before a good result was obtained.

  //str2num("1500", R1,nupb)  // Objective front surface radius
  //str2num("5000", R2,nupb)  // Objective rear surface radius
  //str2num(" 900", R3,nupb)  // Radius of mirror front surface
  //str2num("1500", R4,nupb)  // Radius of mirror silvered surface
  // This give a spot size of about 0.0173mm after many hours

  str2num("+0.1525 7003 4464 3985 5800 0000 0000 E1", R1,nupb)
  str2num("+0.5110 8768 3328 7262 7400 0000 0000 E1", R2,nupb)
  str2num("+0.0926 3690 8587 3005 7300 0000 0000 E1", R3,nupb)
  str2num("+0.1505 3915 2720 2536 6100 0000 0000 E1", R4,nupb)

  // This give a spot radius of about 0.0173mm

  //str2num("+0.1531 6904 5301 4326 5100 0000 0000 E1", R1,nupb)
  //str2num("+0.5061 4787 8214 4032 0700 0000 0000 E1", R2,nupb)
  //str2num("+0.0925 7270 4585 7604 6700 0000 0000 E1", R3,nupb)
  //str2num("+0.1504 2641 8416 5610 7700 0000 0000 E1", R4,nupb)

  // This give a spot radius of about 0.0174mm

  //str2num("+0.1545 1166 8077 8501 2000 0000 0000 E1", R1,nupb)
  //str2num("+0.4969 1626 0425 6302 7000 0000 0000 E1", R2,nupb)
  //str2num("+0.0946 9427 9298 7956 8000 0000 0000 E1", R3,nupb)
  //str2num("+0.1523 9989 0724 6298 1000 0000 0000 E1", R4,nupb)
  // This give a spot radius of about 0.0217
  // 0.0217mm for A2 and 0.0153mm for A1 and 0.0082mm for A0
  // The Airy disc radius is 0.00671mm

  // To show that this is close to the theortical optimum for a
  // telescope with an aperture of 100m consider the following.

  // Light is electromagnetic radiation but, unlike radio waves
  // which have wave lengths measured in metres, visible light
  // has a wave length measured in nano-metres. Blue light is
  // typically 486nm and red light is about 640nm.

  // If we consider a point source of light with a wave length of
  // 550nm at infinity on the axis of a optically perfect telescope,
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
  // 0.0055mm. Since our best spot size of 0.0093mm is not much
  // larger than this, so we are close to the theoretical limit
  // of a telescope with an aperture of 100mm.

  // As a rule of thumb the maximum usable magnification of a
  // telescope is between about 1 and 1.2 times its aperture in
  // millimetres. In practice it is far less that because of
  // the disturbance caused the atmosphere.

  factor  := 5        // A power of ten
                      // The initial values of the deltas are random
                      // integers in the range 0 to 9999 which are then divided
                      // by 10^factor.
                      // factor was incremented every time no improvement
                      // is made after 300 iterations.
                      // The delta values are doubled every time spotvalue
                      // reduces.
                      // Every time an improvement is made, factor, R1, R2, R3
                      // and R4 are written to the file catageometry.txt.
                      // If this file exists it is used to set the starting
                      // values the next time cataopt is run.

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

  IF initfactor>0 DO factor := initfactor

  writef("factor = %n*n", factor)
  writef("R1=     "); prnum(R1, 8)
  writef("R2=     "); prnum(R2, 8)
  writef("R3=     "); prnum(R3, 8)
  writef("R4=     "); prnum(R4, 8)
//abort(1022)

  settok(100, bestspotsize,nupb)  // Unset bestspotsize
  settok(  0, dist,nupb)          // Unset dist
  settok(  9, bestdist,nupb)      // Unset bestdist
  settok( 99, bestspotvalue,nupb) // Unset bestspotvalue

  setzero(spotsize,nupb)
  setzero(dist,nupb)
  setzero(spotvalue,nupb)

  setzero(deltaR1,nupb) // This causes the first setting of
  setzero(deltaR2,nupb) // the spotsize to correspond to the
  setzero(deltaR3,nupb) // initial setting of R1 to R4 before
  setzero(deltaR4,nupb) // any deltas are applied.
  R1ch, R2ch, R3ch, R4ch := '#', '#', '#', '#'
  reduced := TRUE

again: // Enter here if R1 to R4 have their initial values or
       // have values that improved the gemometry of the telescope.

  // Save current values of R1 to R4
  copy(R1,nupb, prevR1,nupb)
  copy(R2,nupb, prevR2,nupb)
  copy(R3,nupb, prevR3,nupb)
  copy(R4,nupb, prevR4,nupb)

  // prevR1 to prevR4 are the best values so far.

newdelta:

  TEST reduced
  THEN { // Previous setting of the delta values caused an
         // improvement so double them.
         //writef("*nreduced=TRUE so double the delta values*n")
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
         settok((randno(9999)-5000) | 1, deltaR1,nupb)
         settok((randno(9999)-5000) | 1, deltaR2,nupb)
         settok((randno(9999)-5000) | 1, deltaR3,nupb)
         settok((randno(9999)-5000) | 1, deltaR4,nupb)
         // Note that "| 1" above ensures all the deltas are nonzero.
         R1ch, R2ch, R3ch, R4ch := '#', '#', '#', '#'

         // 70% of the time only change R1 and R2 or R3 and R4.
         IF randno(101)<=70 DO
         TEST randno(101)<=50
         THEN { setzero(deltaR1,nupb)
                setzero(deltaR2,nupb)
                R1ch, R2ch := ' ', ' '
//writef("Clear deltaR1 and deltaR2*n")
              }
         ELSE { setzero(deltaR3,nupb)
                setzero(deltaR4,nupb)
                R3ch, R4ch := ' ', ' '
//writef("Clear deltaR3 and deltaR4*n")
              }

         // Divide the delta values by 10^factor
         IF k>=4 DO
         { LET e = k/4
           // Divide each delta by 10000^e
           deltaR1!1 := deltaR1!1 - e
           deltaR2!1 := deltaR2!1 - e
           deltaR3!1 := deltaR3!1 - e
           deltaR4!1 := deltaR4!1 - e
           k := k MOD 4
         }

         // Divide each delta by 10^k
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
//  writef("deltaR1= "); prnum(deltaR1, 8)
//  writef("deltaR2= "); prnum(deltaR2, 8)
//  writef("deltaR3= "); prnum(deltaR3, 8)
//  writef("deltaR4= "); prnum(deltaR4, 8)

  // Add the delta values to the radii.
  add(deltaR1,nupb, prevR1,nupb, R1,nupb)
  add(deltaR2,nupb, prevR2,nupb, R2,nupb)
  add(deltaR3,nupb, prevR3,nupb, R3,nupb)
  add(deltaR4,nupb, prevR4,nupb, R4,nupb)

  //writef("*nThe new settings R1 to R4 are as follows:*n*n")
  //writef("R1= "); prnum(R1, 8)
  //writef("R2= "); prnum(R2, 8)
  //writef("R3= "); prnum(R3, 8)
  //writef("R4= "); prnum(R4, 8)
//abort(1501)

  IF R1!0 | R2!0 | R3!0 | R4!0 DO
  { // One of the radii has become negative 
    writef("One of R1 to R4 has become negative, so choose a different delta*n")
    writef("R1= "); prnum(R1, 8)
    writef("R2= "); prnum(R2, 8)
    writef("R3= "); prnum(R3, 8)
    writef("R4= "); prnum(R4, 8)
    failcount := failcount+1
    abort(9999)
    GOTO newdelta
  }

  // Insist that R3 is greater than R4
  IF numcmp(R3,nupb, R4,nupb) > 0 DO
  { failcount := failcount+1
    writef("R3 is greater than R4*n")
    writef("R3= "); prnum(R3, 8)
    writef("R4= "); prnum(R4, 8)
abort(1000)
    failcount := failcount+1
    GOTO newdelta
  }

  //writef("*nThe new delta values are:*n*n")
  //writef("deltaR1= "); prnum(deltaR1, 8)
  //writef("deltaR2= "); prnum(deltaR2, 8)
  //writef("deltaR3= "); prnum(deltaR3, 8)
  //writef("deltaR4= "); prnum(deltaR4, 8)

  //writef("*nTrying the following setting of R1 to R4*n")
  //writef("R1= "); prnum(R1, 8)
  //writef("R2= "); prnum(R2, 8)
  //writef("R3= "); prnum(R3, 8)
  //writef("R4= "); prnum(R4, 8)

  // Plot the current setting of R1 to R4
  // and the previous spotsize and distance.

  //writef("*nTesting new radii*n")
//abort(1600)
  //writef("dist=         "); prnum(dist,8)
  //writef("bestdist=     "); prnum(bestdist,8)
  //writef("spotsize=     "); prnum(spotsize,8)
  //writef("bestspotsize= "); prnum(bestspotsize,8)
  //newline()
//abort(6543)
  //writef("R1= "); prnum(R1, 8)
  //writef("R2= "); prnum(R2, 8)
  //writef("R3= "); prnum(R3, 8)
  //writef("R4= "); prnum(R4, 8)
//abort(3000)
  // Calculate the values that depend on the radii R1 to R4

  // Calculate the z coordinate of the objective front surface centre
  copy(T1,nupb, tmp1,nupb)           // The thickness of the objective lens
  divbyk(2, tmp1,nupb)               // Half of its thickness
  //writef("T1/2=   "); prnum(tmp1,8)

  sub(R1,nupb, tmp1,nupb, C1,nupb)   // Centre of the objective front surface.
  //writef("C1=     "); prnum(C1,8)

  sub(tmp1,nupb, R2,nupb, C2,nupb)   // Centre of the objective rear surface.
  //writef("C2=     "); prnum(C2,8)

  sub(D,nupb, T2,nupb, tmp1,nupb)
  sub(tmp1,nupb, R3,nupb, C3,nupb)   // Centre of mirror front surface
  //writef("C3=     "); prnum(C3,8)

  sub( D,nupb, R4,nupb, C4,nupb)     // Centre of mirror silvered surface
  //writef("C4=     "); prnum(C4,8)

  // Mark all dot coordinates as unset so that calcspotsize
  // can tell which have been defined by doray.
//abort(1333)
  FOR i = 0 TO 16+16+2-1 DO
  {  // Unset image dots have x set to 100.
    settok(100, spot0vx!i,nupb)
    settok(100, spot1vx!i,nupb)
    settok(100, spot2vx!i,nupb)
  }

  // The rate of convergence is greatly improved by commenting
  // out most of the calls of doray, but at least two should be
  // left in. Leaving them mostly uncommented causes a prettier
  // picture to be drawn. If you leave only two call of doray,
  // it is probably best to leave:
  //   doray(2,'A',0)  and   doray(2,'A',4).

  // Now trace several rays through the telescope, storing the
  // image dots in the focal plane coordinate vectors.

  setzero(MirrorRadius,nupb)

  doray(0, 'A', 0)
  doray(0, 'A', 1)
  doray(0, 'A', 2)
  doray(0, 'A', 3)
  doray(0, 'A', 4)
  doray(0, 'A', 5)
  doray(0, 'A', 6)
  doray(0, 'A', 7)

  //doray(0, 'B', 0)
  //doray(0, 'B', 1)
  //doray(0, 'B', 2)
  //doray(0, 'B', 3)
  //doray(0, 'B', 4)
  //doray(0, 'B', 5)
  //doray(0, 'B', 6)
  //doray(0, 'B', 7)

  doray(0, 'C', 0)

  doray(1, 'A', 0)
  doray(1, 'A', 1)
  doray(1, 'A', 2)
  doray(1, 'A', 3)
  doray(1, 'A', 4)
  doray(1, 'A', 5)
  doray(1, 'A', 6)
  doray(1, 'A', 7)

  //doray(1, 'B', 0)
  //doray(1, 'B', 1)
  //doray(1, 'B', 2)
  //doray(1, 'B', 3)
  //doray(1, 'B', 4)
  //doray(1, 'B', 5)
  //doray(1, 'B', 6)
  //doray(1, 'B', 7)

  doray(1, 'C', 0)

  doray(2, 'A', 0)
  doray(2, 'A', 1)
  doray(2, 'A', 2)
  doray(2, 'A', 3)
  doray(2, 'A', 4)
  doray(2, 'A', 5)
  doray(2, 'A', 6)
  doray(2, 'A', 7)

  //doray(2, 'B', 0)
  //doray(2, 'B', 1)
  //doray(2, 'B', 2)
  //doray(2, 'B', 3)
  //doray(2, 'B', 4)
  //doray(2, 'B', 5)
  //doray(2, 'B', 6)
  //doray(2, 'B', 7)

  doray(2, 'C', 0)

//abort(5656)
  IF tracing DO
  { writef("*nMirrorRadius= "); prnum(MirrorRadius,8)
  newline()
//abort(5100)
  }

  // Calculate spotsize and dist.

  setzero(spotsize,nupb)
  setzero(dist,nupb)

  calcspotsize(0)
  calcspotsize(1)
  calcspotsize(2)

  // For the current setting of R1 to R4, spotsize is now
  // the size of the largest of the images from the selected
  // directions, and dist is greatest distance the
  // average y is from the theoretical centre of its spot.

  // Set spotvalue to the larger of spotsize and dist
  // It is a measure of how good the optics of the telescope is.

  //writef("dist=            "); prnum(dist, 8)
  //writef("spotsize=        "); prnum(spotsize, 8)

  copy(spotsize,nupb, spotvalue,nupb)
  IF numcmp(spotsize,nupb, dist,nupb) < 0 DO
    copy(dist,nupb, spotvalue,nupb)

  //add(spotsize,nupb, dist,nupb, spotvalue,nupb)
  //writef("spotsize=        "); prnum(spotsize, 8)
  //writef("spotvalue=       "); prnum(spotvalue, 8)
  //writef("bestspotvalue=   "); prnum(bestspotvalue, 8)

  IF pausing DO abort(5555)

  // Initialize the focal plane image
  fillsurf(c_gray)

  setcolour(c_black)
  drawf(10,310, "spotmag = %n", spotmag)
  drawf(10,290, "factor  = %n", factor)

  { LET n, f1, f2 = 0, 0, 0
    IF dist!1= 1 DO n, f1, f2 := dist!2, dist!3, dist!4
    IF dist!1= 0 DO n, f1, f2 :=      0, dist!2, dist!3
    IF dist!1=-1 DO n, f1, f2 :=      0,      0, dist!2
    drawf(10,270, "dist =%i2.%z4 %z4 mm", n,f1, f2)
    n, f1, f2 := 0, 0, 0
    IF bestdist!1= 1 DO n, f1, f2 := bestdist!2, bestdist!3, bestdist!4
    IF bestdist!1= 0 DO n, f1, f2 :=          0, bestdist!2, bestdist!3
    IF bestdist!1=-1 DO n, f1, f2 :=          0,          0, bestdist!2
    drawf(10,250, "best =%i2.%z4 %z4 mm", n,f1, f2)
  }

  drawf(centrex-85,130, "R1%c         %i4.%z4 %z4 %z4 mm*n",
        R1ch, R1!2, R1!3, R1!4, R1!5)
  drawf(centrex-85,110, "R2%c         %i4.%z4 %z4 %z4 mm*n",
        R2ch, R2!2, R2!3, R2!4, R2!5)
  drawf(centrex-85, 90, "R3%c         %i4.%z4 %z4 %z4 mm*n",
        R3ch, R3!2, R3!3, R3!4, R3!5)
  drawf(centrex-85, 70, "R4%c         %i4.%z4 %z4 %z4 mm*n",
        R4ch, R4!2, R4!3, R4!4, R4!5)

  { LET n,f1,f2,f3,f4 = 9, 0, 0, 0, 0
    IF dist!1= 1 DO n,f1,f2,f3,f4 := dist!2,dist!3,dist!4,dist!5,dist!6
    IF dist!1= 0 DO n,f1,f2,f3,f4 :=      0,dist!2,dist!3,dist!4,dist!5
    IF dist!1=-1 DO n,f1,f2,f3,f4 :=      0,     0,dist!2,dist!3,dist!4
    IF dist!1=-3 DO n,f1,f2,f3,f4 :=      0,     0,     0,dist!2,dist!3
    drawf(centrex-85,50, "dist     %i2.%z4 %z4 %z4 %z4 mm", n,f1,f2,f3,f4)
  }

  IF spotsize!1=1 TEST spotsize!2>9
  THEN drawf(centrex-85, 30, "Spot size   %i4.%z4 %z4 %z4 mm",
             spotsize!2, spotsize!3, spotsize!4, spotsize!5)
  ELSE drawf(centrex-85, 30, "Spot size %n.%z4 %z4 %z4 %z4 mm",
             spotsize!2, spotsize!3, spotsize!4, spotsize!5, spotsize!6)
  IF spotsize!1=0 DO
    drawf(centrex-85, 30, "Spot size 0.%z4 %z4 %z4 %z4 mm",
          spotsize!2, spotsize!3, spotsize!4, spotsize!5)
  IF spotsize!1=-1 DO
    drawf(centrex-85, 30, "Spot size 0.0000 %z4 %z4 %z4 mm",
          spotsize!2, spotsize!3, spotsize!4)

  IF bestspotsize!1=1 TEST bestspotsize!2>9
  THEN drawf(centrex-85, 10, "Spot size   %i4.%z4 %z4 %z4 mm",
             bestspotsize!2, bestspotsize!3, bestspotsize!4, bestspotsize!5)
  ELSE drawf(centrex-85, 10, "Spot size %n.%z4 %z4 %z4 %z4 mm",
             bestspotsize!2, bestspotsize!3, bestspotsize!4,
             bestspotsize!5, bestspotsize!6)
  IF bestspotsize!1=0 DO
    drawf(centrex-85, 10, "Best size 0.%z4 %z4 %z4 %z4 mm",
          bestspotsize!2, bestspotsize!3, bestspotsize!4, bestspotsize!5)
  IF bestspotsize!1=-1 DO
    drawf(centrex-85, 10, "Best size 0.0000 %z4 %z4 %z4 mm",
          bestspotsize!2, bestspotsize!3, bestspotsize!4)

  setcolour(c_black)
  moveto(0, centrey)
  drawby( screenxsize,   0)
  moveto(centrex, 0)
  drawby(0, screenysize)

  updatescreen()

  // If spotvalue is smaller than bestspotvalue, the optics of
  // the telescope has improved, so the current settings of
  // {\tt R1} to {\tt R4} are are remembered and bestspotsize,
  // bestdist and bestspotvalue updated.


  TEST numcmp(spotvalue,nupb, bestspotvalue,nupb) < 0
  THEN {  reduced := TRUE
          writef("%i4 Spotvalue has reduced*n", iterations)

         //writef("%i4 spotvalue=     ", iterations); prnum(spotvalue,8)
         //writef("%i4 bestspotvalue= ", iterations); prnum(bestspotvalue,8)

         //writef("%i4 spotsize=      ", iterations); prnum(spotsize,8)
         //writef("%i4 bestspotsize=  ", iterations); prnum(bestspotsize,8)

         //writef("%i4 dist=          ", iterations); prnum(dist,8)
         //writef("%i4 bestdist=      ", iterations); prnum(bestdist,8)

         copy(spotsize,nupb,   bestspotsize,nupb)
         copy(dist,nupb,       bestdist,nupb)
         copy(spotvalue,nupb,  bestspotvalue,nupb)
         wrgeometry()
         iterations := iterations-1
         failcount := 0
       }
  ELSE { // No improvement so re-instate the previous radii.
         reduced := FALSE
         copy(prevR1,nupb, R1,nupb)
         copy(prevR2,nupb, R2,nupb)
         copy(prevR3,nupb, R3,nupb)
         copy(prevR4,nupb, R4,nupb)
         failcount := failcount+1
         writef("%i4 This delta failed, failcount=%n*n", iterations, failcount)
         //writef("spotsize=     "); prnum(spotsize,8)
         //writef("bestspotsize= "); prnum(bestspotsize,8)

         IF failcount>500 DO
         { // Make the delta values smaller
           factor := factor + 1
           IF factor > 14 RETURN // Return from telescope
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
{ // This function finds average y coordinate for the current spot
  // placing it in avgy. It then calculates its distance from the
  // theoretical centre of the spot, dist is updated.

  // It then inspects each dot belonging to the specified spot. If
  // its distance from (0,avgy) is greater than spotsize, spotsize
  // is updated.

  // The algorithm attempts to minimise both dist and spotsize
  // placing more significance on dist since this is a measure
  // of how close the focal length is to 1000mm.

  LET tmp1 = VEC nupb
  LET tmp2 = VEC nupb
  LET tmp3 = VEC nupb
  LET tmp4 = VEC nupb
  LET tmp5 = VEC nupb
  LET tmp6 = VEC nupb
  LET avgy = VEC nupb
  LET count = 0        // Count of dots in this spot
  LET spotsizesq = VEC nupb

  // Set the theoretical centre of the specified spot to (0,cgy).
  LET cgy  = spotno=0 ->
               (TABLE FALSE, 0, 0, 0000), //  0.0000 for direction 0
             spotno=1 ->
               (TABLE TRUE,  1, 2, 0833), // -2.0833 for direction 1
               (TABLE TRUE,  1, 4, 1666)  // -4.1666 for direction 2
  // Note that cgy has upb=3.

  // Select the coordinate vectors for the specified spot
  LET px = spotno=0 -> spot0vx,
           spotno=1 -> spot1vx,
                       spot2vx
  LET py = spotno=0 -> spot0vy,
           spotno=1 -> spot1vy,
                       spot2vy
  setzero(avgy,nupb)

  // Calculate avgy and hence dist
  FOR i = 0 TO 16+16+2-1 DO
  { // i =  0 to  7   Blue A rays
    // i =  8 to 15   Red  A rays
    // i = 16 to 23   Blue B rays
    // i = 24 to 31   Red  B rays
    // i = 32 to 33   Blue and Red C rays
    LET x = px!i
    LET y = py!i

    IF x!1=1 & x!2=100 LOOP // An unset dot has x=100.

    add(y,nupb, avgy,nupb, tmp1,nupb)
    copy(tmp1,nupb, avgy,nupb)
    count := count+1
//writef("%i2 count=%i2 y= ",i, count); prnum(y, 5)
//writef("avgy=            ",i); prnum(avgy, 5)
  }

  //writef("count=%n*n", count)

  IF count DO
  { divbyk(count, avgy,nupb) // Compute the average
//writef("average y=       "); prnum(avgy, 5)
    sub(cgy,3, avgy,nupb, tmp1,nupb)
    //writef("tmp1=                  "); prnum(tmp1,5)
    // Take its absolute value
    tmp1!0 := FALSE
    //writef("centre dist= "); prnum(tmp1, 5)
    // If greater than dist, update dist.
    IF numcmp(tmp1,nupb, dist,nupb) > 0 DO copy(tmp1,nupb, dist,nupb)
    //writef("dist=        "); prnum(dist, 5)
//abort(8888)
  }

  //writef("dist=                  "); prnum(dist, 5)
//abort(8888)
  setzero(spotsizesq,nupb)

  // Calculate the radius squared
  FOR i = 0 TO 16+16+2-1 DO
  { // i =  0 to  7   Blue A rays
    // i =  8 to 15   Red  A rays
    // i = 16 to 23   Blue B rays
    // i = 24 to 31   Red  B rays
    // i = 32 to 33   Blue and Red C rays
    LET x = px!i
    LET y = py!i

    IF x!1=1 & x!2=100 LOOP // An unset dot has x coordinate equal to 100.

    //writef("x=                     "); prnum(x, 8)
    mul(x,nupb, x,nupb, tmp1,nupb)    // tmp1 = x^2
    //writef("x^2=                   "); prnum(tmp1, 8)

    //writef("avgy=                  "); prnum(avgy, 8)
    //writef("y=                     "); prnum(y, 8)
    sub(y,nupb, avgy,nupb, tmp2,nupb)        // tmp2 = y-avgy
    //writef("y-avgy=                "); prnum(tmp2, 8)
    mul(tmp2,nupb, tmp2,nupb, tmp3,nupb)    // tmp3 = (y-avgy)^2
    //writef("(y-avgy)^2=            "); prnum(tmp3, 8)

    add(tmp1,nupb, tmp3,nupb, tmp4,nupb)    // tmp4 = x^2 + (y-avgy)^2
    //writef("spot%n i=%i2 x^2 + (y-avgy)^2= ", spotno, i); prnum(tmp4, 8)

    //sqrt(tmp4,nupb, tmp5,nupb)
    //writef("tmp5=                  "); prnum(tmp5, 8)

    IF numcmp(tmp4,nupb, spotsizesq,nupb) > 0 DO copy(tmp4,nupb, spotsizesq,nupb) 
//abort(1276)
  }

  // spotsizesq is the square of the largest distance.

  sqrt(spotsizesq,nupb, tmp1,nupb)
  //writef("spot%n size=       ", spotno); prnum(tmp1,8)

  // tmp1 is the largest distance for the current spot.

  // If it is larger than spotsize, update spotsize.
  IF numcmp(tmp1,nupb, spotsize,nupb) > 0 DO copy(tmp1,nupb, spotsize,nupb)

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

  writef("*nGives spotsize: "); prnum(spotsize, 8)

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

  IF tracing DO
  { writef("*ndoray: %c%n pos=%n*n", ch, n, pos)
    writef("root2=  "); prnum(root2,8)
    writef("raddiv= "); prnum(raddiv,8)
  }

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
//writef("Inx=   "); prnum(Inx,8)
//writef("Iny=   "); prnum(Iny,8)
//writef("Inz=   "); prnum(Inz,8)

//writef("*nDirection of the incident ray*n")
//writef("dir!0= "); prnum(dir!0,8)
//writef("dir!1= "); prnum(dir!1,8)
//writef("dir!2= "); prnum(dir!2,8)

  //writef("*nEntry point %c%n, Direction %n, Blue*n", ch, pos, n)
  focalx, focaly := px!0, py!0 // Location of a blue dot
  raytrace(dir, @Inx, Blue, focalx, focaly)

  IF tracing DO
  { newline()
    writef("%c%n Blue x= ",ch,pos); prnum(focalx,8)
    writef("%c%n Blue y= ",ch,pos); prnum(focaly,8)
  }

  setcolour(c_blue)
  //IF pos<4 DO
    drawdot(n, scale(focalx), scale(focaly))

  //IF tracing DO
  //  abort(5000)

  //writef("*nEntry point %c%n, Direction %n, Red*n", ch, pos, n)

  TEST ch='C'
  THEN focalx, focaly := px!1, py!1 // Location of a red dot for C
  ELSE focalx, focaly := px!8, py!8 // Location of a red dot for A or B
  raytrace(dir, @Inx, Red, focalx, focaly)

  IF tracing DO
  { newline()
    writef("%c%n Red  x= ",ch,pos); prnum(focalx,8)
    writef("%c%n Red  y= ",ch,pos); prnum(focaly,8)
  }

  setcolour(c_red)
  //IF pos<4 DO
    drawdot(n, scale(focalx), scale(focaly))

  //IF tracing DO
  //  abort(5001)
}

AND scale(num) = VALOF
{ LET res = ?
  LET e = num!1
  IF e>1 DO res := 9999_9999
  IF e=1 DO res := num!2 * 10000 + num!3
  IF e=0 DO res := num!2
  IF e<0 DO res := 0
  IF num!0 DO res := -res
  // res is in unit of 1/10000 mm
//writef("scale: num= "); prnum(num, 5)
//writef("scale: res=%n*n", res)
  RESULTIS res
}

