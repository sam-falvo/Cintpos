/*

This program creates the file gltiger.mdl representing a
tigermoth aircraft and the land in .mdl format for use by
the OpenGL program gltiger.b

Implemented by Martin Richards (c) February 2014

History

08/05/18
Extensively modified to generate the new .mdl model file, using
floating point and the FLT feature.

OpenGL vertex data is stored as follows

vec3 position -- t(direction of thrust), w(direction of left wing),
              -- and l(diretion of lift)   floating point values
vec3 colour   -- r, g, b     floating point values
int  k        -- =1 rudder,
                 =2 elevator,
                 =3 left aileron,
                 =4 right aileron
                 =5 landscape and runway
float d       -- distance from hinge in inches, to be multiplied
                 by the sine or cosine of control surface angle.

The program outputs vertex and index items representing triangles. It
used a self entending vector for the vertices. A hash table of vertices
is maintained so that all the vertices in the vertex vector can be distinct.

The new version of the .mdl language is as follows.

// Vertices of the tigermoth model are represented by 8 values
//        n w l  r g b  k d
// These are all 32-bit floating point numbers.

//    k = 0.0   fixed surface, ie not rudder, elevator or aileron.
//    k = 1.0   rudder         d is the distance from the rudder hinge
//    k = 2.0   elevator       d is the distance from the elevator hinge
//    k = 3.0   left aileron   d is the distance from the aileron hinge
//    k = 4.0   right aileron  d is the distance from the aileron hinge
//    k = 5.0   Unlike triangles on the aircraft, triangles on the land
//              surface do not move and do not need rotation. However
//              their image on the screen is affected by the position
//              and orientation of the camera.
// Syntax

// vs n         Set the upb of the vertex vector
// is n         Set the upb of the index vector
// r   n        Set the current red component
// g   n        Set the current green component
// b   n        Set the current blue component
// k   n        Set current k value
// d   n        Set current k value
// v   x y z    Specify a vertex with coords xyz using the current
//              settings of rgb and kd
// i n          Set an index vector element
// ds n         Set the upb of the display vector
// t mode n offset  Set the mode, n and offset of a display triplet.

*/

GET "libhdr"
GET "glmanifests.h"

GLOBAL {
 stdin:ug
 stdout

 FLT cur_r; FLT cur_g; FLT cur_b  // The current colour.
 FLT cur_k; FLT cur_d             // The current k and d values

 addvertex   // Find or create a vertex, returning the vertex number
 vertexcount // Index of the next vertex to be created
 indexcount  // Count of index values
 hashtab     // hash table for verices

 spacev  // To hold vertices to be placed in the hash table
 spacet
 spacep
 newvec

 includeland
 tracing
 tostream
}

MANIFEST {
// Vertex structure
v_next=0          // List of vertices in vertex number order.
v_x; v_y; v_z
v_r; v_g; v_b
v_k; v_d          // Control surface, distance from hinge.
v_n               // Vertex number.
v_chain           // Hash chain to allow efficient lookup.
v_size            // Number of words in a vertex node.
v_upb = v_size-1

// Vertices are held in a hash table so that their vertex numbers 
// can be reused.
hashtabsize = 541
hashtabupb = hashtabsize-1

spaceupb = 100_000 * v_size

FLT runwaylength =    600.000
FLT runwaywidth  =     40.000
FLT landsize     = 20_000.000

FLT Left  =  1.0
FLT Right = -1.0
}

LET start() = VALOF
{ LET stdin = input()
  LET stdout = output()
  LET toname = "gltiger.mdl"  // The default target file. 
  LET ht = VEC hashtabsize
  LET argv = VEC 50

  vertexcount := 0
  indexcount := 0

  // Initialize the vertex list.

  cur_r, cur_g, cur_b := -1.0, -1.0, -1.0  // The current colour.
  cur_k, cur_d        := -1.0, -1.0        // The current k and d values

  hashtab := ht
  FOR i = 0 TO hashtabupb DO hashtab!i := 0

  UNLESS rdargs("to/k,-land/s,-t/s", argv, 50) DO
  { writef("Bad arguments for mktigermothmodel*n")
    RESULTIS 0
  }

  IF argv!0 DO toname := argv!0   // to/k
  includeland := argv!1           // -land/s
  tracing := argv!2               // -t/s

  deletefile(toname)
  tostream := findinoutput(toname)
  UNLESS toname DO
  { writef("trouble with file: %s*n", toname)
    RESULTIS 0
  }

  spacev := getvec(spaceupb)
  spacet := @spacev!spaceupb
  spacep := spacet

  UNLESS spacep DO
  { writef("Unable to allocate %n words of space*n")
    GOTO fin
  }
  
  selectoutput(tostream)

  mktigermothmodel()

  endstream(tostream)
  selectoutput(stdout)
  writef("Space used %n out of %n*n", spacet-spacep, spacet)

fin:
  IF spacev DO freevec(spacev)
  RESULTIS 0
}

AND newvec(upb) = VALOF
{ LET p = spacep - upb - 1
  IF p < spacev DO
  { writef("error: spacev is not large enough*n")
    abort(999) 
  }
  spacep := p
//writef("newvec: spacev=%n spacep=%n*n", spacev, spacep)
  RESULTIS p
}

AND rgb(r, g, b) BE // r, g and b are integers in range 0..255
{ // First scale r, g and b to range 0.0 .. 1.0
  LET FLT new_r = FLOAT r / 255
  LET FLT new_g = FLOAT g / 255
  LET FLT new_b = FLOAT b / 255
  UNLESS new_r=cur_r & new_g=cur_g & new_b=cur_b DO
  { cur_r, cur_g, cur_b := new_r, new_g, new_b
    //writef("rgb %6.3f %6.3f %6.3f*n", cur_r, cur_g, cur_b)
  }
}

AND kd(FLT k, FLT d) BE
{ UNLESS k=cur_k & d=cur_d DO
  { //writef("kd %6.3f %6.3f*n", k, d)
    cur_k, cur_d := k, d
  }
}

AND vertex(FLT t, FLT w, FLT l) = VALOF
{ // Find vertex t,w,l,  cur_g,cur_g,cur_b,  cur_k,cur_d
  // creating it if necessary and return its vertex number.

  LET hashval = ((FIX t * 1234 +
                  FIX w * 2345 +
                  FIX l * 3456 +
                  FIX cur_r * 4567 +
                  FIX cur_g * 5678 +
                  FIX cur_b * 6789 +
                  FIX cur_k * 4321 +
                  FIX cur_d * 4321) >> 1) MOD hashtabsize
  LET p = hashtab!hashval
//writef("vertex: t=%9.3f  w=%9.3f  l=%9.3f  hashval=%i3 p=%n*n",
//        t, w, l, hashval, p)
//IF FALSE DO
  WHILE p DO // Search down the hash chain
  { IF p!v_x=t & p!v_y=w & p!v_z=l &
       p!v_r=cur_r & p!v_g=cur_g & p!v_b=cur_b &
       p!v_k=cur_k & p!v_d=cur_d RESULTIS p!v_n // Vertex found
    p := p!v_chain
  }

  // Matching vertex node not found, so create a new one.
  p := newvec(v_upb)
//  writef("vertex: new p = %n*n", p)
//abort(1000)
  p!v_x, p!v_y, p!v_z := t, w, l
  p!v_r, p!v_g, p!v_b := cur_r, cur_g, cur_b
  p!v_k, p!v_d        := cur_k, cur_d
  p!v_n := vertexcount
  // Insert the new vertex node into its hash chain.
  p!v_chain := hashtab!hashval
  hashtab!hashval := p

  writef("x%7.3f y%7.3f z%7.3f ", t,w,l)
  writef("r%5.3f g%5.3f b%5.3f ", cur_r, cur_g, cur_b)
  writef("k%5.3f d%7.3f // %i3*n", cur_k, cur_d, vertexcount)
  vertexcount := vertexcount+1
  RESULTIS p!v_n
}

AND vertexrgb(FLT t, FLT w, FLT l, r,g,b) = VALOF
{ rgb(r,g,b)
  RESULTIS vertex(t,w,l)
}

AND vertexkd(FLT t, FLT w, FLT l, k,d) = VALOF
{ kd(k, d)
  RESULTIS vertex(t,w,l)
}

AND landvertex(FLT n, FLT w, FLT h, r,g,b) = VALOF
{ rgb(r,g,b)
  RESULTIS vertexkd(n,w,h, 5.0, 0.0)
}

AND triangle(a,b,c, d,e,f, g,h,i) BE
{ // a, b, c are in directions forward, left and up
  // store as openGL t,w,l which are forward, left, up.
  // ie set t,w,l to a,b,c
  // do the same for def and ghi
  LET v0, v1, v2 = ?, ?, ?
  kd(0.0, 0.0)
  v0 := vertex(a,b,c)
  v1 := vertex(d,e,f)
  v2 := vertex(g,h,i)
  // Output three index values
  writef("i%n i%n i%n*n", v0, v1, v2)
  indexcount := indexcount+3
}

AND quad(a,b,c, d,e,f, g,h,i, j,k,l) BE
{ // a, b, c are in directions forward, left and up
  // store as openGL t,w,l which are forward, left, up
  // ie set x,y,z to a,b,c
  // do the same for def, ghi and jkl
  LET v0, v1, v2, v3 = ?, ?, ?, ?
  kd(0.0, 0.0)
  v0 := vertex(a,b,c)
  v1 := vertex(d,e,f)
  v2 := vertex(g,h,i)
  v3 := vertex(j,k,l)
  writef("i%n i%n i%n*n", v0, v1, v2)
  writef("i%n i%n i%n*n", v0, v2, v3)
  indexcount := indexcount+6
}

AND trianglekd(FLT a, FLT b, FLT c, FLT k1, FLT d1,
               FLT d, FLT e, FLT f, FLT k2, FLT d2,
               FLT g, FLT h, FLT i, FLT k3, FLT d3) BE
{ // a, b, c are in directions forward, left and up
  // store as openGL t,w,l which are forward, left, up
  // ie set x, y, z to a, b, c
  // do the same for def and ghi
  LET v0, v1, v2 = ?, ?, ?
  v0 := vertexkd(a,b,c, k1,d1)
  v1 := vertexkd(d,e,f, k2,d2)
  v2 := vertexkd(g,h,i, k3,d3)
  writef("i%n i%n i%n*n", v0, v1, v2)
  indexcount := indexcount+3
}

AND quadkd(FLT a, FLT b, FLT c, FLT k1, FLT d1,
           FLT d, FLT e, FLT f, FLT k2, FLT d2,
           FLT g, FLT h, FLT i, FLT k3, FLT d3,
           FLT j, FLT k, FLT l, FLT k4, FLT d4) BE
{ // a, b, c are in directions forward, left and up
  // store as openGL t,w,l which are forward, left, up
  // ie set x, y, z to a, b, c
  // do the same for def, ghi and jkl
  LET v0, v1, v2, v3 = ?, ?, ?, ?
  v0 := vertexkd(a,b,c, k1,d1)
  v1 := vertexkd(d,e,f, k2,d2)
  v2 := vertexkd(g,h,i, k3,d3)
  v3 := vertexkd(j,k,l, k4,d4)
  writef("i%n i%n i%n*n", v0, v1, v2)
  writef("i%n i%n i%n*n", v0, v2, v3)
  indexcount := indexcount+6
}

AND triangleland(FLT x1, FLT y1, FLT z1, r1,g1,b1,
                 FLT x2, FLT y2, FLT z2, r2,g2,b2,
                 FLT x3, FLT y3, FLT z3, r3,g3,b3) BE
{ // 3D coords and colours of the the vertices of a triangle
  // of landscape or runway (ie with k=5.0 and d=0.0
  LET v0, v1, v2 = ?, ?, ?
  kd(5.0, 0.0)
  v0 := vertexrgb(x1,y1,z1, r1,g1,b1)
  v1 := vertexrgb(x2,y2,z2, r2,g2,b2)
  v2 := vertexrgb(x3,y3,z3, r3,g3,b3)
  writef("i%n i%n i%n*n", v0, v1, v2)
  indexcount := indexcount+3
}

AND quadland(FLT x1, FLT y1, FLT z1, r1,g1,b1,
             FLT x2, FLT y2, FLT z2, r2,g2,b2,
             FLT x3, FLT y3, FLT z3, r3,g3,b3,
             FLT x4, FLT y4, FLT z4, r4,g4,b4) BE
{ // 3D coords and colours of the the vertices of a quad
  // of landscpe or runway
  LET v0, v1, v2, v3 = ?, ?, ?, ?
  kd(5.0, 0.0)
  v0 := vertexrgb(x1,y1,z1, r1,g1,b1)
  v1 := vertexrgb(x2,y2,z2, r2,g2,b2)
  v2 := vertexrgb(x3,y3,z3, r3,g3,b3)
  v3 := vertexrgb(x4,y4,z4, r4,g4,b4)
  writef("i%n i%n i%n*n", v0, v1, v2)
  writef("i%n i%n i%n*n", v0, v2, v3)
  indexcount := indexcount+6
}

AND mktigermothmodel() BE
{ // The origin is the centre of the tigermoth
  // For landscape and the runway, the origin is the start of the runway

  // The tigermoth coordinates are as follows

  // first  t is the distance forward of the centre of gravity
  // second w is the distance left of the centre of gravity
  // third  l is the distance above the centre of gravity

  // This first line will be overwritten at the end when the
  // upper bounds of the vertex and index vectors are known,
  writef("vs ???????  is ???????          *n")

  writef("// Cockpit floor*n")
  rgb(90,80,30)
  kd(0.0, 0.0)
  quad( 1.000, 0.800, 0.000,
        1.000,-0.800, 0.000,
       -5.800,-0.800, 0.000,
       -5.800, 0.800, 0.000)

  wings(Left)
  wings(Right)


  writef("// Wheel strut left*n")
  rgb(80,80,80)
  strut(-0.768,  1.000, -2.000,
        -0.068,  2.000, -3.800)

  writef(" // Wheel strut diag left*n")
  rgb(80,80,80)
  strut( 1.600,  1.000, -2.000,
        -0.168,  2.000, -3.800)

  writef("// Wheel strut centre left*n")
  rgb(80,80,80)
  strut(-0.500,  0.000, -2.900,
        -0.168,  2.000, -3.800)

  writef("// Wheel strut right*n")
  rgb(80,80,80)
  strut(-0.768, -1.000, -2.000,
        -0.068, -2.000, -3.800)

  writef("// Wheel strut diag right*n")
  rgb(80,80,80)
  strut( 1.600, -1.000, -2.000,
        -0.168, -2.000, -3.800)

  writef("// Wheel strut centre right*n")
  rgb(80,80,80)
  strut(-0.500, -0.000, -2.900,
        -0.168, -2.000, -3.800)

  writef("// Right wheel*n")
  wheel(-0.268, -2.265, -3.800)
  writef("// Left wheel*n")
  wheel(-0.268, +2.265, -3.800)

  fueltank()

  writef("// Fuselage*n")

  writef("// Prop shaft*n")
  rgb(40,40,50)
  triangle( 5.500, 0.000,  0.000,
            4.700, 0.200,  0.300,
            4.700, 0.200, -0.300)
  rgb(60,60,40)
  triangle( 5.500, 0.000,  0.000,
            4.700, 0.200, -0.300,
            4.700,-0.200, -0.300)
  rgb(40,40,50)
  triangle( 5.500, 0.000,  0.000,
            4.700,-0.200, -0.300,
            4.700,-0.200,  0.300)
  rgb(60,60,40)
  triangle( 5.500, 0.000,  0.000,
            4.700,-0.200,  0.300,
            4.700, 0.200,  0.300)

  writef("// Engine front lower centre*n")
  rgb(140,140,160)
  triangle( 5.000, 0.000,  0.000,
            4.500, 0.350, -1.750,
            4.500,-0.350, -1.750)

  writef("// Engine front lower left*n")
  rgb(140,120,130)
  triangle( 5.000, 0.000,  0.000,
            4.500, 0.350, -1.750,
            4.500, 0.550,  0.000)

  writef("// Engine front lower right*n")
  rgb(140,120,130)
  triangle( 5.000, 0.000,  0.000,
            4.500,-0.350, -1.750,
            4.500,-0.550,  0.000)

  writef("// Engine front upper centre*n")
  rgb(140,140,160)
  triangle( 5.000, 0.000, 0.000,
            4.500, 0.350, 0.500,
            4.500,-0.350, 0.500)

  writef("// Engine front upper left and right*n")
  rgb(100,140,130)
  triangle( 5.000, 0.000, 0.000,
            4.500, 0.350, 0.500,
            4.500, 0.550, 0.000)
  triangle( 5.000, 0.000, 0.000,
            4.500,-0.350, 0.500,
            4.500,-0.550, 0.000)

  writef("// Engine left lower*n")
  rgb(80,80,60)
  quad( 1.033, 1.000,  0.000,
        1.800, 1.000, -2.000,
        4.500, 0.350, -1.750,
        4.500, 0.550,  0.000)

  writef(" // Engine right lower*n")
  rgb(80,100,60)
  quad( 1.033,-1.000,  0.000,
        1.800,-1.000, -2.000,
        4.500,-0.350, -1.750,
        4.500,-0.550,  0.000)

   writef("// Engine top left*n")
  rgb(100,130,60)
  quad(  1.033, 0.750,  0.950,
         1.033, 1.000,  0.000,
         4.500, 0.550,  0.000,
         4.500, 0.350,  0.500)

   writef("// Engine top centre*n")
  rgb(130,160,90)
  quad(  1.033, 0.750,  0.950,
         1.033,-0.750,  0.950,
         4.500,-0.350,  0.500,
         4.500, 0.350,  0.500)

   writef("// Engine top right*n")
  rgb(100,130,60)
  quad(  1.033,-0.750,  0.950,
         1.033,-1.000,  0.000,
         4.500,-0.550,  0.000,
         4.500,-0.350,  0.500)

   writef("// Engine bottom*n")
  rgb(100,80,50)
  quad(  4.500, 0.350, -1.750,
         4.500,-0.350, -1.750,
         1.800,-1.000, -2.000,
         1.800, 1.000, -2.000)


  writef("// Front cockpit left*n")
  rgb(120,140,60)
  quad( -2.000, 1.000,  0.000,
        -2.000, 0.853,  0.600,
        -3.300, 0.853,  0.600,
        -3.300, 1.000,  0.000)

  writef(" // Front cockpit right*n")
  rgb(120,140,60)
  quad( -2.000,-1.000,  0.000,
        -2.000,-0.853,  0.600,
        -3.300,-0.853,  0.600,
        -3.300,-1.000,  0.000)

  writef("// Top front left*n")
  rgb(100,120,40)
  quad(  1.033, 0.750,  0.950,
        -2.000, 0.750,  1.000,
        -2.000, 1.000,  0.000,
         1.033, 1.000,  0.000)

  writef("// Top front middle*n")
  rgb(120,140,60)
  quad(  1.033, 0.750,  0.950,
         1.033,-0.750,  0.950,
        -2.000,-0.750,  1.000,
        -2.000, 0.750,  1.000)

  writef("// Top front right*n")
  rgb(100,120,40)
  quad(  1.033,-0.750,  0.950,
        -2.000,-0.750,  1.000,
        -2.000,-1.000,  0.000,
         1.033,-1.000,  0.000)


  writef(" // Front wind shield*n")
  rgb(180,200,150)
  quad( -1.300, 0.450,  1.000, // Centre
        -2.000, 0.450,  1.400,
        -2.000,-0.450,  1.400,
        -1.300,-0.450,  1.000)
  rgb(220,220,180)
  triangle( -1.300, 0.450,  1.000, // Left
            -2.000, 0.450,  1.400,
            -2.000, 0.650,  1.000)
  triangle( -1.300,-0.450,  1.000, // Right
            -2.000,-0.450,  1.400,
            -2.000,-0.650,  1.000)


  writef("// Top left middle*n")
  rgb(120,165,90)
  quad( -3.300, 0.750,  1.000,
        -3.300, 1.000,  0.000,
        -4.300, 1.000,  0.000,
        -4.300, 0.750,  1.000)

  writef("// Top centre middle*n")
  rgb(120,140,60)
  quad( -3.300, 0.750,  1.000,
        -3.300,-0.750,  1.000,
        -4.300,-0.750,  1.000,
        -4.300, 0.750,  1.000)

  writef("// Top right middle*n")
  rgb(130,160,90)
  quad( -3.300,-0.750,  1.000,
        -3.300,-1.000,  0.000,
        -4.300,-1.000,  0.000,
        -4.300,-0.750,  1.000)

  writef("// Rear cockpit left*n")
  rgb(120,140,60)
  quad( -4.300, 1.000,  0.000,
        -4.300, 0.840,  0.600,
        -5.583, 0.770,  0.600,
        -5.583, 1.000,  0.000)

  writef("// Rear wind shield*n")
  rgb(180,200,150)
  quad( -3.600, 0.450,  1.000, // Centre
        -4.300, 0.450,  1.400,
        -4.300,-0.450,  1.400,
        -3.600,-0.450,  1.000)
  rgb(220,220,180)
  triangle( -3.600, 0.450,  1.000, // Left
            -4.300, 0.450,  1.400,
            -4.300, 0.650,  1.000)
  triangle( -3.600,-0.450,  1.000, // Right
            -4.300,-0.450,  1.400,
            -4.300,-0.650,  1.000)

  writef("// Rear cockpit right*n")
  rgb(110,140,70)
  quad( -4.300,-1.000,  0.000,
        -4.300,-0.840,  0.600,
        -5.583,-0.770,  0.600,
        -5.583,-1.000,  0.000)
  writef("// Lower left middle*n")
  rgb(140,110,70)
  quad(  1.033, 1.000,  0.000,
         1.800, 1.000, -2.000,
        -3.583, 1.000, -2.238,
        -3.300, 1.000,  0.000)

  rgb(155,100,70)
  triangle( -3.300, 1.000,  0.000,
            -3.583, 1.000, -2.238,
            -5.583, 1.000,  0.000)

  writef("// Bottom middle*n")
  rgb(120,100,60)
  quad(  1.800, 1.000, -2.000,
        -3.583, 1.000, -2.238,
        -3.583,-1.000, -2.238,
         1.800,-1.000, -2.000)

  writef(" // Lower right middle*n")
  rgb(140,100,70)
  quad(  1.033,-1.000,  0.000,
         1.800,-1.000, -2.000,
        -3.583,-1.000, -2.238,
        -3.300,-1.000,  0.000)

  rgb(120,100,70)
  triangle( -3.300,-1.000,  0.000,
            -3.583,-1.000, -2.238,
            -5.583,-1.000,  0.000)

  writef(" // Lower left back*n")
  rgb(165,115,80)
  quad( -5.583, 1.000,  0.000,
       -16.000, 0.050,  0.000,
       -16.000, 0.050, -0.667,
        -3.583, 1.000, -2.238)

  writef(" // Bottom back*n")
  rgb(130,90,60)
  quad( -3.583, 1.000, -2.238,
       -16.000, 0.050, -0.667,
       -16.000,-0.050, -0.667,
        -3.583,-1.000, -2.238)

  writef("// Lower right back*n")
  rgb(150,140,80)
  quad( -5.583,-1.000,  0.000,
       -16.000,-0.050,  0.000,
       -16.000,-0.050, -0.667,
        -3.583,-1.000, -2.238)

  writef("// Top left back*n")
  rgb(130,125,85)
  triangle( -5.583, 0.650,  0.950,
            -5.583, 1.000,  0.000,
           -16.000, 0.050,  0.000)

  writef("// Top centre back*n")
  rgb(130,160,90)
  quad( -5.583, 0.650,  0.950,
        -5.583,-0.650,  0.950,
       -16.000,-0.050,  0.000,
       -16.000, 0.050,  0.000)

  writef("// Top right back*n")
  rgb(130,120,80)
  triangle( -5.583,-0.650,  0.950,
            -5.583,-1.000,  0.000,
           -16.000,-0.050,  0.000)

  writef("// End back*n")
  rgb(120,165,95)
  quad(-16.000, 0.050,  0.000,
       -16.000,-0.050,  0.000,
       -16.000,-0.050, -0.667,
       -16.000, 0.050, -0.667)

  writef("// Fin*n")

  rgb(170,180,80)
  quad(-14.000, 0.000, 0.000,      // Fin
       -16.000, 0.050, 0.000,
       -16.000, 0.100, 1.000,
       -15.200, 0.000, 1.000)
  quad(-14.000, 0.000, 0.000,      // Fin
       -16.000,-0.050, 0.000,
       -16.000,-0.100, 1.000,
       -15.200, 0.000, 1.000)
    
  rgb(70,120,40)
  quadkd(-15.200, 0.000, 1.000, 1.0,-0.800, // Rudder R1 left
         -16.000, 0.100, 1.000, 1.0,-0.100,
         -16.800, 0.000, 3.100, 1.0, 0.800,
         -16.000, 0.000, 2.550, 0.0, 0.000)
  quadkd(-15.200, 0.000, 1.000, 1.0,-0.800, // Rudder R1 right
         -16.000,-0.100, 1.000, 1.0,-0.100,
         -16.800, 0.000, 3.100, 1.0, 0.800,
         -16.000, 0.000, 2.550, 0.0, 0.000)

  rgb(90,100,50)
  trianglekd(-16.000, 0.100, 1.000, 0.0, 0.000,
             -15.200, 0.000, 1.000, 1.0,-0.800,
             -16.000,-0.100, 1.000, 0.0, 0.000)
  rgb(70, 80,40)
  quadkd(-16.000, 0.100, 1.000, 0.0, 0.000, // Rudder R2 left
         -16.800, 0.000, 3.100, 1.0, 0.800,
         -17.566, 0.000, 2.600, 1.0, 1.566,
         -17.816, 0.000, 1.667, 1.0, 1.816)
  quadkd(-16.000,-0.100, 1.000, 0.0, 0.000, // Rudder R2 right
         -16.800, 0.000, 3.100, 1.0, 0.800,
         -17.566, 0.000, 2.600, 1.0, 1.566,
         -17.816, 0.000, 1.667, 1.0, 1.866)
  rgb(70,120,40)
  quadkd(-16.000, 0.100, 1.000, 0.0, 0.000, // Rudder R3 left
         -17.816, 0.000, 1.667, 1.0, 1.816,
         -17.816, 0.000, 1.000, 1.0, 1.816,
         -17.566, 0.000, 0.000, 1.0, 1.566)
  quadkd(-16.000,-0.100, 1.000, 0.0, 0.000, // Rudder R3 right
         -17.816, 0.000, 1.667, 1.0, 1.816,
         -17.816, 0.000, 1.000, 1.0, 1.816,
         -17.566, 0.000, 0.000, 1.0, 1.566)
  rgb(70, 80,40)
  quadkd(-16.000, 0.100, 1.000, 0.0, 0.000, // Rudder R4 left
         -17.566, 0.000, 0.000, 1.0, 1.566,
         -17.000, 0.000,-0.583, 1.0, 1.000,
         -16.000, 0.000,-0.667, 0.0, 0.000)
  quadkd(-16.000,-0.100, 1.000, 0.0, 0.000, // Rudder R5 right
         -17.566, 0.000, 0.000, 1.0, 1.566,
         -17.000, 0.000,-0.583, 1.0, 1.000,
         -16.000, 0.000,-0.667, 0.0, 0.000)

  writef("// Tail skid*n")
  rgb(40, 40, 40)
  quadkd(-16.000, -0.050, -0.667, 0.0,  0.000,
         -16.000,  0.000, -0.627, 0.0,  0.000,
         -16.500,  0.000, -0.860, 1.0,  0.500,
         -16.500, -0.050, -0.900, 1.0,  0.500)
  rgb(70, 70, 70)
  quadkd(-16.000,  0.050, -0.667, 0.0,  0.000,
         -16.000,  0.000, -0.627, 0.0,  0.000,
         -16.500,  0.000, -0.860, 1.0,  0.500,
         -16.500,  0.050, -0.900, 1.0,  0.500)

  rgb(50, 60, 50)
  trianglekd(-16.500,  0.050, -0.900, 1.0,  0.500,
             -16.500,  0.000, -0.860, 1.0,  0.500,
             -16.700,  0.050, -0.900, 1.0,  0.700)
  rgb(30, 50, 40)
  trianglekd(-16.500, -0.050, -0.900, 1.0,  0.500,
             -16.500,  0.000, -0.860, 1.0,  0.500,
             -16.700, -0.050, -0.900, 1.0,  0.700)
  rgb(30, 30, 30)
  trianglekd(-16.700, -0.050, -0.900, 1.0,  0.700,
             -16.500,  0.000, -0.860, 1.0,  0.500,
             -16.700,  0.050, -0.900, 1.0,  0.700)

  writef("// Tailplane and elevator*n")

  rgb(120,180,50)
  triangle(-16.000, 0.000, 0.100,
           -13.900, 0.600, 0.000,
           -13.900,-0.600, 0.000)
  triangle(-16.000, 0.000,-0.100,
           -13.900, 0.600, 0.000,
           -13.900,-0.600, 0.000)

  rgb(120,200,50)
  quad(-16.000, 2.800, 0.100, // Left tailplane upper
       -13.900, 0.600, 0.000,
       -14.600, 2.800, 0.000,
       -16.000, 4.500, 0.000)
  rgb(120,180,50)
  triangle(-16.000, 0.000, 0.100,
           -13.900, 0.600, 0.000,
           -16.000, 2.800, 0.100)
  rgb(100,200,50)
  quad(-16.000, 2.800,-0.100, // Left tailplane lower
       -13.900, 0.600, 0.000,
       -14.600, 2.800, 0.000,
       -16.000, 4.500, 0.000)
  rgb(120,200,70)
  triangle(-16.000, 0.000,-0.100,
           -13.900, 0.600, 0.000,
           -16.000, 2.800,-0.100)

  rgb(120,200,50)
  quad(-16.000,-2.800, 0.100, // Right tailplane upper
       -13.900,-0.600, 0.000,
       -14.600,-2.800, 0.000,
       -16.000,-4.500, 0.000)
  rgb(120,180,50)
  triangle(-16.000, 0.000, 0.100,
           -13.900,-0.600, 0.000,
           -16.000,-2.800, 0.100)
  rgb(100,200,50)
  quad(-16.000,-2.800,-0.100, // Right tailplane lower
       -13.900,-0.600, 0.000,
       -14.600,-2.800, 0.000,
       -16.000,-4.500, 0.000)
  rgb(120,200,70)
  triangle(-16.000, 0.000,-0.100,
           -13.900,-0.600, 0.000,
           -16.000,-2.800,-0.100)

  rgb(165,100,50)
  quadkd(-16.000, 0.000, 0.100, 0.0, 0.000, // Left elevator
         -17.200, 0.600, 0.000, 2.0, 1.200, // pt 1
         -17.500, 0.900, 0.000, 2.0, 1.500, // pt 2
         -16.000, 2.800, 0.100, 0.0, 0.000)
  quadkd(-16.000, 0.000,-0.100, 0.0, 0.000, // Left elevator
         -17.200, 0.600, 0.000, 2.0, 1.200, // pt 1
         -17.500, 0.900, 0.000, 2.0, 1.500, // pt 2
         -16.000, 2.800,-0.100, 0.0, 0.000)

  rgb(150,140,100)
  trianglekd(-16.000, 0.000, -0.100, 0.0, 0.000,
             -17.200, 0.600,  0.000, 2.0, 1.200,
             -16.000, 0.000,  0.100, 0.0, 0.000)

  rgb(170,150,80) 
  quadkd(-16.000, 2.800, 0.100, 0.0, 0.000, // Left elevator
         -17.500, 0.900, 0.000, 2.0, 1.500, // pt 2
         -17.666, 2.000, 0.000, 2.0, 1.666, // pt 3
         -17.650, 3.500, 0.000, 2.0, 1.650) // pt 4
  quadkd(-16.000, 2.800,-0.100, 0.0, 0.000, // Left elevator
         -17.500, 0.900, 0.000, 2.0, 1.500, // pt 2
         -17.666, 2.000, 0.000, 2.0, 1.666, // pt 3
         -17.650, 3.500, 0.000, 2.0, 1.650) // pt 4

  rgb(120,170,60) 
  quadkd(-16.000, 2.800, 0.100, 0.0, 0.000, // Left elevator
         -17.650, 3.500, 0.000, 2.0, 1.650, // pt 4
         -17.200, 4.650, 0.000, 2.0, 1.200, // pt 5
         -16.700, 4.833, 0.000, 2.0, 0.700) // pt 6
  quadkd(-16.000, 2.800,-0.100, 0.0, 0.000, // Left elevator
         -17.650, 3.500, 0.000, 2.0, 1.650, // pt 4
         -17.200, 4.650, 0.000, 2.0, 1.200, // pt 5
         -16.700, 4.833, 0.000, 2.0, 0.700) // pt 6

  rgb(160,120,40) 
  quadkd(-16.000, 2.800, 0.100, 0.0, 0.000, // Left elevator
         -16.700, 4.833, 0.000, 2.0, 0.700, // pt 6
         -16.300, 4.750, 0.000, 2.0, 0.300, // pt 7
         -16.000, 4.500, 0.000, 0.0, 0.000) // pt 8
  quadkd(-16.000, 2.800,-0.100, 0.0, 0.000, // Left elevator
         -16.700, 4.833, 0.000, 2.0, 0.700, // pt 6
         -16.300, 4.750, 0.000, 2.0, 0.300, // pt 7
         -16.000, 4.500, 0.000, 0.0, 0.000) // pt 8

  rgb(165,100,50)
  quadkd(-16.000, 0.000, 0.100, 0.0, 0.000, // Right elevator
         -17.200,-0.600, 0.000, 2.0, 1.200, // pt 1
         -17.500,-0.900, 0.000, 2.0, 1.500, // pt 2
         -16.000,-2.800, 0.100, 0.0, 0.000)
  quadkd(-16.000, 0.000,-0.100, 0.0, 0.000, // Right elevator
         -17.200,-0.600, 0.000, 2.0, 1.200, // pt 1
         -17.500,-0.900, 0.000, 2.0, 1.500, // pt 2
         -16.000,-2.800,-0.100, 0.0, 0.000)

  rgb(140,130,90)
  trianglekd(-16.000, 0.000, -0.100, 0.0, 0.000,
             -17.200,-0.600,  0.000, 2.0, 1.200,
             -16.000, 0.000,  0.100, 0.0, 0.000)

  rgb(170,150,80) 
  quadkd(-16.000,-2.800, 0.100, 0.0, 0.000, // Right elevator
         -17.500,-0.900, 0.000, 2.0, 1.500, // pt 2
         -17.666,-2.000, 0.000, 2.0, 1.666, // pt 3
         -17.650,-3.500, 0.000, 2.0, 1.650) // pt 4
  quadkd(-16.000,-2.800,-0.100, 0.0, 0.000, // Right elevator
         -17.500,-0.900, 0.000, 2.0, 1.500, // pt 2
         -17.666,-2.000, 0.000, 2.0, 1.666, // pt 3
         -17.650,-3.500, 0.000, 2.0, 1.650) // pt 4

  rgb(120,170,60) 
  quadkd(-16.000,-2.800, 0.100, 0.0, 0.000, // Right elevator
         -17.650,-3.500, 0.000, 2.0, 1.650, // pt 4
         -17.200,-4.650, 0.000, 2.0, 1.200, // pt 5
         -16.700,-4.833, 0.000, 2.0, 0.700) // pt 6
  quadkd(-16.000,-2.800,-0.100, 0.0, 0.000, // Right elevator
         -17.650,-3.500, 0.000, 2.0, 1.650, // pt 4
         -17.200,-4.650, 0.000, 2.0, 1.200, // pt 5
         -16.700,-4.833, 0.000, 2.0, 0.700) // pt 6

  rgb(160,120,40) 
  quadkd(-16.000,-2.800, 0.100, 0.0, 0.000, // Right elevator
         -16.700,-4.833, 0.000, 2.0, 0.700, // pt 6
         -16.300,-4.750, 0.000, 2.0, 0.300, // pt 7
         -16.000,-4.500, 0.000, 2.0, 0.000) // pt 8
  quadkd(-16.000,-2.800,-0.100, 0.0, 0.000, // Right elevator
         -16.700,-4.833, 0.000, 2.0, 0.700, // pt 6
         -16.300,-4.750, 0.000, 2.0, 0.300, // pt 7
         -16.000,-4.500, 0.000, 0.0, 0.000) // pt 8

  rgb(165,100,50) 
  quadkd(-16.000, 0.000, 0.100, 0.0, 0.000, // Right elevator
         -17.200,-0.600, 0.000, 2.0, 1.200, // pt 1
         -17.500,-0.900, 0.000, 2.0, 1.500, // pt 2
         -16.000,-2.800, 0.100, 0, 0.000)
  quadkd(-16.000, 0.000,-0.100, 0.0, 0.000, // Right elevator
         -17.200,-0.600, 0.000, 2.0, 1.200, // pt 1
         -17.500,-0.900, 0.000, 2.0, 1.500, // pt 2
         -16.000,-2.800,-0.100, 0.0, 0.000)

  IF includeland DO
  { // Construct the landscape and runway
    writef("// Runway*n")

    { MANIFEST { ns = 50_000
                 ws =  5_000
               }

      FOR n = 0 TO 600_000-ns BY ns DO
        FOR w = -20_000 TO 20_000-ws BY ws DO
        { LET m = (17*n XOR 5*w)>>1
          LET r = 150 + m MOD 23
          LET g = 160 + m MOD 13
          LET b = 170 + m MOD 37
          quadland( FLOAT(n),       FLOAT(w), 1.000, r, g, b,
                    FLOAT(n),    FLOAT(w+ws), 1.000, r, g, b,
                    FLOAT(n+ns), FLOAT(w+ws), 1.000, r, g, b,
                    FLOAT(n+ns),    FLOAT(w), 1.000, r, g, b)
        }
    }
    writef("// The land*n")
    // Plot a square region of land
    plotland(-5_000.000, -5_000.000, 10_000.000)
  }

fin:
  writef("ds 2*n")                 // Only one display item.
  writef("t %n %n %n*n",
          GL_TRIANGLES,   // Draw Triangles
	  indexcount,     // Number of index elements to use
	  0)              // Starting ant index subscript position 0.

  // Write the upper bounds of the vertex and index vectors at the start
  // of the file
  UNLESS rewindstream(cos) DO
  { sawritef("rewindstream failed*n")
    abort(999)
  }
  writef("vs %i7  is %i7*n", 8*vertexcount-1, indexcount-1)
}

AND wings(FLT side) BE
{ // side=1.0 for left wings and -1.0 for right wings
  LET ailcontrol = side>0.0 -> 3.0, 4.0

  writef("// Lower wing*n")
  rgb(165,165,30)        // Under surface
  quad(-0.500, side*1.000, -2.000,  // Panel A
       -3.767, side*1.000, -2.218,
       -4.396, side*6.000, -1.745, 
       -1.129, side*6.000, -1.527)

  rgb(155,150,40)
  quad(-3.767, side*1.000, -2.218,  // Panel B
       -4.917, side*1.000, -2.294,
       -5.546, side*6.000, -1.821,
       -4.396, side*6.000, -1.745) 

  rgb(160,165,50)
  quad(-1.129, side*6.000, -1.527,  // Panel C
       -4.396, side*6.000, -1.745,
       -5.147,side*14.166, -1.179,
       -1.880,side*14.166, -0.961)

  rgb(155,155,20)            // Under surface
  quadkd(-4.396, side*6.000, -1.745, 0.0, 0.000, // Panel D Aileron
         -5.546, side*6.000, -1.821, ailcontrol, 1.150,
         -6.297,side*13.766, -1.255, ailcontrol, 1.150,
         -5.147,side*14.166, -1.179, 0.0, 0.000)

  writef("// Lower wing upper surface*n")
  rgb(120,140,60)

  quad(-0.500, side*1.000, -2.000,  // Panel A1
       -1.500, side*1.000, -1.800,
       -2.129, side*6.000, -1.327, 
       -1.129, side*6.000, -1.527)

  rgb(120,130,50)
  quad(-1.500, side*1.000, -1.800,  // Panel A2
       -3.767, side*1.000, -2.118,
       -4.396, side*6.000, -1.645, 
       -2.129, side*6.000, -1.327)

  rgb(110,140,50)
  quad(-3.767, side*1.000, -2.118,  // Panel B
       -4.917, side*1.000, -2.294,
       -5.546, side*6.000, -1.821,
       -4.396, side*6.000, -1.645) 

  rgb(120,140,60)
  quad(-1.129, side*6.000, -1.527,  // Panel C1
       -2.129, side*6.000, -1.327,
       -2.880,side*14.166, -0.761,
       -1.880,side*14.166, -0.961)

  rgb(120,130,50)
  quad(-2.129, side*6.000, -1.327,  // Panel C2
       -4.396, side*6.000, -1.645,
       -5.147,side*14.166, -1.079,
       -2.880,side*14.166, -0.761)

  rgb(110,140,40)
  triangle(-3.767, side*1.000, -2.118, // Gusset low wing root
           -4.917, side*1.000, -2.294,
           -3.767, side*1.000, -2.218)

  rgb(120,140,60)
  quadkd(-4.396, side*6.000, -1.645, 0.0, 0.000,  // Panel D Aileron
         -5.546, side*6.000, -1.821, ailcontrol, 1.150,
         -6.297,side*13.766, -1.255, ailcontrol, 1.150,
         -5.147,side*14.166, -1.079, 0.0, 0.000)

  rgb(120,130,70)
  trianglekd(-4.396, side*6.000, -1.745, 0.0, 0.000, // Aileron fixed end
             -5.546, side*6.000, -1.821, 0.0, 0.000,
             -4.396, side*6.000, -1.645, 0.0, 0.000)

  rgb(110,120,50)
  trianglekd(-4.396, side*6.000, -1.745, 0.0, 0.000, // Aileron near end
             -5.546, side*6.000, -1.821, ailcontrol, 1.150,
             -4.396, side*6.000, -1.645, 0.0, 0.000)

  rgb(100,140,60)
  trianglekd(-5.147, side*14.166, -1.079, 0.0, 0.000, // Aileron tip end
             -6.297, side*13.766, -1.255, ailcontrol, 1.150,
             -5.147, side*14.166, -1.179, 0.0, 0.000)

   writef("// Lower wing tip*n")
  rgb(130,150,60)
  triangle(-1.880,side*14.167,-0.961,//-1.006,   // T1
           -2.880,side*14.167,-0.761,
           -3.880,side*14.467,-0.980)
  rgb(130,150,60)
  triangle(-2.880,side*14.167,-0.761,
           -5.147,side*14.167,-1.079,
           -3.880,side*14.467,-0.980)
  rgb(160,160,40)
  triangle(-5.147,side*14.167,-1.079,
           -5.147,side*14.167,-1.179,
           -3.880,side*14.467,-0.980)
  rgb(170,170,50)
  triangle(-5.147,side*14.167,-1.179,
           -1.880,side*14.167,-0.961,
           -3.880,side*14.467,-0.980)

  writef("// Upper wing*n")
  rgb(200,200,30)           // Under surface
  quad( 1.333, side*1.000,  2.900,
       -1.967, side*1.000,  2.671,
       -3.297,side*14.167,  3.671, 
        0.003,side*14.167,  3.894)
  rgb(190,210,40)
  quad(-1.967, side*1.000,  2.671,
       -3.084, side*2.200,  2.606,
       -4.414,side*13.767,  3.645, 
       -3.297,side*14.167,  3.671)

  rgb(150,170,90)           // Top surface
  quad( 1.333, side*1.000,  2.900, // Panel A1
        0.333, side*1.000,  3.100,
       -0.997,side*14.167,  4.094, 
        0.003,side*14.167,  3.894)

  rgb(140,160,80)           // Top surface
  quad( 0.333, side*1.000,  3.100, // Panel A2
       -1.967, side*1.000,  2.771,
       -3.297,side*14.167,  3.771, 
       -0.997,side*14.167,  4.094)

  rgb(150,170,90)           // Top surface
  quad(-1.967, side*1.000,  2.771, // Panel B
       -3.084, side*2.200,  2.606,
       -4.414,side*13.767,  3.645, 
       -3.297,side*14.167,  3.771)

  rgb(140,180,100)
  triangle(-1.967, side*1.000, 2.771,  // Top wing root gusset
           -3.084, side*2.200, 2.606,
           -1.967, side*1.000, 2.671)

  writef("// Upper wing tip*n")
  rgb(130,150,60)
  triangle( 0.003,side*14.167, 3.894,
           -0.997,side*14.167, 4.094,
           -1.997,side*14.467, 3.874)
  rgb(130,150,60)
  triangle(-0.997,side*14.167, 4.094,
           -3.297,side*14.167, 3.771,
           -1.997,side*14.467, 3.874)
  rgb(160,160,40)
  triangle(-3.297,side*14.167, 3.771,
           -3.297,side*14.167, 3.671,
           -1.997,side*14.467, 3.874)
  rgb(170,170,50)
  triangle(-3.297,side*14.167, 3.671,
            0.003,side*14.167, 3.894,
           -1.997,side*14.467, 3.874)
  rgb(130,150,60)
  triangle(-3.297,side*14.167, 3.671,
           -4.414,side*13.767,  3.645, 
           -3.297,side*14.167, 3.771)


  writef("// Wing root strut forward right*n")
  rgb(80,80,80)
  strut(0.433, side*0.950,  2.900,
        0.433, side*1.000,  0.000)

  writef(" // Wing root strut rear right*n")
  rgb(80,80,80)
  strut(-1.967, side*0.950,  2.671,
        -1.068, side*1.000,  0.000)

  writef("// Wing root strut diag right*n")
  rgb(80,80,80)
  strut( 0.433, side*0.950,  2.900,
        -1.068, side*1.000,  0.000)

  writef("// Wing strut forward right*n")
  rgb(80,80,80)
  strut(-2.200, side*10.000, -1.120,
        -0.300, side*10.000,  3.445)

  writef("// Wing strut rear right*n")
  rgb(80,80,80)
  strut(-4.500, side*10.000, -1.260,
        -2.500, side*10.000,  3.410)
}

AND fueltank() BE
{ LET FLT ft1, FLT fl1 =  1.333, 2.900
  LET FLT ft2, FLT fl2 =  1.100, 3.180
  LET FLT ft3, FLT fl3 =  0.500, 3.400
  LET FLT ft4, FLT fl4 = -0.700, 3.210
  LET FLT ft5, FLT fl5 = -1.967, 2.771
  LET FLT ft6, FLT fl6 = -1.967, 2.671
  LET FLT ft7, FLT fl7 = -0.700, 2.600
  LET FLT ft8, FLT fl8 =  0.500, 2.700

  writef("// Fueltank top forward*n")
  rgb(200,200,230)

  quad( 1.333,  1.000,  fl1, // Top forward
        1.333, -1.000,  fl1,
        1.100, -1.000,  fl2,
        1.100,  1.000,  fl2)

  writef("// Fueltank top middle forward*n")
  rgb(190,200,220)
  quad( 1.100, -1.000,  fl2, // Top middle forward
        1.100,  1.000,  fl2,
        0.500,  1.000,  fl3,
        0.500, -1.000,  fl3)

  writef("// Fueltank top middle rear*n")
  rgb(200,200,220)
  quad( 0.500,  1.000,  fl3, // Top middle rear
        0.500, -1.000,  fl3,
       -0.700, -1.000,  fl4,
       -0.700,  1.000,  fl4)

  writef("// Fueltank top back*n")
  rgb(190,190,210)
  quad(ft4, -1.000,  fl4, // Top back
       ft4,  1.000,  fl4,
       ft5,  1.000,  fl5,
       ft5, -1.000,  fl5)

  writef("// Fueltank back*n")
  rgb(210,220,240)
  quad(ft5, -1.000,  fl5, // Under back
       ft5,  1.000,  fl5,
       ft6,  1.000,  fl6,
       ft6, -1.000,  fl6)

  writef("// Fueltank under rear*n")
  rgb(180,200,200)
  quad(ft6, -1.000,  fl6, // Under back
       ft6,  1.000,  fl6,
       ft7,  1.000,  fl7,
       ft7, -1.000,  fl7)

  writef("// Fueltank under middle*n")
  rgb(170,190,190)
  quad(ft7, -1.000,  fl7, // Under middle
       ft7,  1.000,  fl7,
       ft8,  1.000,  fl8,
       ft8, -1.000,  fl8)

  writef("// Fueltank under front*n")
  rgb(200,210,220)
  quad(ft8, -1.000,  fl8, // Under front
       ft8,  1.000,  fl8,
       ft1,  1.000,  fl1,
       ft1, -1.000,  fl1)

  writef("// Fueltank back*n")
  rgb(220,190,250)
  quad(ft5, -1.000,  fl5, // back
       ft5,  1.000,  fl5,
       ft6,  1.000,  fl6,
       ft6, -1.000,  fl6)


  writef("// Fueltank left side*n")
  rgb(200,220,230)
  quad(ft1,  1.000, fl1,   // Forward
       ft2,  1.000, fl2,
       ft3,  1.000, fl3,
       ft8,  1.000, fl8)
rgb(215,235,200)
  quad(ft3,  1.000, fl3,   // Middle
       ft4,  1.000, fl4,
       ft7,  1.000, fl7,
       ft8,  1.000, fl8)
rgb(210,220,240)
  quad(ft4,  1.000, fl4,   // Rear
       ft5,  1.000, fl5,
       ft6,  1.000, fl6,
       ft7,  1.000, fl7)

  writef("// Fueltank right side*n")
  rgb(200,220,230)
  quad( 1.333, -1.000, fl1,   // Forward
        1.100, -1.000, fl2,
        0.500, -1.000, fl3,
        0.500, -1.000, fl8)
rgb(215,235,200)
  quad( 0.500, -1.000, fl3,   // Middle
       -0.700, -1.000, fl4,
       -0.700, -1.000, fl7,
        0.500, -1.000, fl8)
rgb(210,220,240)
  quad(ft4, -1.000, fl4,   // Rear
       ft5, -1.000, fl5,
       ft6, -1.000, fl6,
       ft7, -1.000, fl7)
}

AND wheel(tpos, wpos, lpos) BE
{ rgb(60,20,20)
  //      t             w        l
  quad( tpos,       wpos+0.200, lpos,      // top back left
        tpos-0.500, wpos+0.250, lpos,
        tpos-0.350, wpos+0.250, lpos+0.350,
        tpos,       wpos+0.250, lpos+0.500)
  quad( tpos-0.500, wpos+0.250, lpos,
        tpos-0.650, wpos      , lpos,
        tpos-0.500, wpos,       lpos+0.500,
        tpos-0.350, wpos+0.250, lpos+0.350)
  quad( tpos-0.350, wpos+0.250, lpos+0.350,
        tpos-0.500, wpos      , lpos+0.500,
        tpos      , wpos,       lpos+0.650,
        tpos,       wpos+0.250, lpos+0.500)

  quad( tpos,       wpos-0.200, lpos,       // top back right
        tpos-0.500, wpos-0.250, lpos,
        tpos-0.350, wpos-0.250, lpos+0.350,
        tpos,       wpos-0.250, lpos+0.500)
  quad( tpos-0.500, wpos-0.250, lpos,
        tpos-0.650, wpos      , lpos,
        tpos-0.500, wpos,       lpos+0.500,
        tpos-0.350, wpos-0.250, lpos+0.350)
  quad( tpos-0.350, wpos-0.250, lpos+0.350,
        tpos-0.500, wpos      , lpos+0.500,
        tpos      , wpos,       lpos+0.650,
        tpos,       wpos-0.250, lpos+0.500)

  quad( tpos,       wpos+0.200, lpos,      // top front left
        tpos+0.500, wpos+0.250, lpos,
        tpos+0.350, wpos+0.250, lpos+0.350,
        tpos,       wpos+0.250, lpos+0.500)
  quad( tpos+0.500, wpos+0.250, lpos,
        tpos+0.650, wpos      , lpos,
        tpos+0.500, wpos,       lpos+0.500,
        tpos+0.350, wpos+0.250, lpos+0.350)
  quad( tpos+0.350, wpos+0.250, lpos+0.350,
        tpos+0.500, wpos      , lpos+0.500,
        tpos      , wpos,       lpos+0.650,
        tpos,       wpos+0.250, lpos+0.500)

  quad( tpos,       wpos-0.200, lpos,       // top front right
        tpos+0.500, wpos-0.250, lpos,
        tpos+0.350, wpos-0.250, lpos+0.350,
        tpos,       wpos-0.250, lpos+0.500)
  quad( tpos+0.500, wpos-0.250, lpos,
        tpos+0.650, wpos      , lpos,
        tpos+0.500, wpos,       lpos+0.500,
        tpos+0.350, wpos-0.250, lpos+0.350)
  quad( tpos+0.350, wpos-0.250, lpos+0.350,
        tpos+0.500, wpos      , lpos+0.500,
        tpos      , wpos,       lpos+0.650,
        tpos,       wpos-0.250, lpos+0.500)


  quad( tpos,       wpos+0.200, lpos,      // bottom back left
        tpos-0.500, wpos+0.250, lpos,
        tpos-0.350, wpos+0.250, lpos-0.350,
        tpos,       wpos+0.250, lpos-0.500)
  quad( tpos-0.500, wpos+0.250, lpos,
        tpos-0.650, wpos      , lpos,
        tpos-0.500, wpos,       lpos-0.500,
        tpos-0.350, wpos+0.250, lpos-0.350)
  quad( tpos-0.350, wpos+0.250, lpos-0.350,
        tpos-0.500, wpos      , lpos-0.500,
        tpos      , wpos,       lpos-0.650,
        tpos,       wpos+0.250, lpos-0.500)

  quad( tpos,       wpos-0.200, lpos,       // bottom back right
        tpos-0.500, wpos-0.250, lpos,
        tpos-0.350, wpos-0.250, lpos-0.350,
        tpos,       wpos-0.250, lpos-0.500)
  quad( tpos-0.500, wpos-0.250, lpos,
        tpos-0.650, wpos      , lpos,
        tpos-0.500, wpos,       lpos-0.500,
        tpos-0.350, wpos-0.250, lpos-0.350)
  quad( tpos-0.350, wpos-0.250, lpos-0.350,
        tpos-0.500, wpos      , lpos-0.500,
        tpos      , wpos,       lpos-0.650,
        tpos,       wpos-0.250, lpos-0.500)

  quad( tpos,       wpos+0.200, lpos,      // bottom front left
        tpos+0.500, wpos+0.250, lpos,
        tpos+0.350, wpos+0.250, lpos-0.350,
        tpos,       wpos+0.250, lpos-0.500)
  quad( tpos+0.500, wpos+0.250, lpos,
        tpos+0.650, wpos      , lpos,
        tpos+0.500, wpos,       lpos-0.500,
        tpos+0.350, wpos+0.250, lpos-0.350)
  quad( tpos+0.350, wpos+0.250, lpos-0.350,
        tpos+0.500, wpos      , lpos-0.500,
        tpos      , wpos,       lpos-0.650,
        tpos,       wpos+0.250, lpos-0.500)

  quad( tpos,       wpos-0.200, lpos,       // bottom front right
        tpos+0.500, wpos-0.250, lpos,
        tpos+0.350, wpos-0.250, lpos-0.350,
        tpos,       wpos-0.250, lpos-0.500)
  quad( tpos+0.500, wpos-0.250, lpos,
        tpos+0.650, wpos      , lpos,
        tpos+0.500, wpos,       lpos-0.500,
        tpos+0.350, wpos-0.250, lpos-0.350)
  quad( tpos+0.350, wpos-0.250, lpos-0.350,
        tpos+0.500, wpos      , lpos-0.500,
        tpos      , wpos,       lpos-0.650,
        tpos,       wpos-0.250, lpos-0.500)
  }

AND strut(FLT t1, FLT w1, FLT l1, FLT t4, FLT w4, FLT l4) BE
{ LET FLT t2 = (3*t1+t4)/4
  LET FLT w2 = (3*w1+w4)/4
  LET FLT l2 = (3*l1+l4)/4
  LET FLT t3 = (3*t4+t1)/4
  LET FLT w3 = (3*w4+w1)/4
  LET FLT l3 = (3*l4+l1)/4
  LET FLT ta, FLT wa = 0.050, 0.030
  LET FLT tb, FLT wb = 0.110, 0.050

  rgb(80,80,80)
  quad(t1-ta,w1,l1, t1,w1+wa,l1, t2,w2+wb,l2, t2-tb,w2,l2)
  rgb(85,75,80)
  quad(t1-ta,w1,l1, t1,w1-wa,l1, t2,w2-wb,l2, t2-tb,w2,l2)
  rgb(85,80,85)
  quad(t1,w1+wa,l1, t1+ta,w1,l1, t2+tb,w2,l2, t2,w2+wb,l2)
  rgb(75,80,80)
  quad(t1,w1-wa,l1, t1+ta,w1,l1, t2+tb,w2,l2, t2,w2-wb,l2)

  rgb(90,80,80)
  quad(t2-tb,w2,l2, t2,w2+wb,l2, t3,w3+wb,l3, t3-tb,w3,l3)
  rgb(95,75,80)
  quad(t2,w2+wb,l2, t2+tb,w2,l2, t3+tb,w3,l3, t3,w3+wb,l3)
  rgb(90,85,80)
  quad(t2+tb,w2,l2, t2,w2-wb,l2, t3,w3-wb,l3, t3+tb,w3,l3)
  rgb(80,80,85)
  quad(t2,w2-wb,l2, t2-tb,w2,l2, t3-tb,w3,l3, t3,w3-wb,l3)

  rgb(80,80,80)
  quad(t4-ta,w4,l4, t4,w4+wa,l4, t3,w3+wb,l3, t3-tb,w3,l3)
  rgb(85,75,80)
  quad(t4-ta,w4,l4, t4,w4-wa,l4, t3,w3-wb,l3, t3-tb,w3,l3)
  rgb(85,80,85)
  quad(t4,w4+wa,l4, t4+ta,w4,l4, t3+tb,w3,l3, t3,w3+wb,l3)
  rgb(75,80,80)
  quad(t4,w4-wa,l4, t4+ta,w4,l4, t3+tb,w3,l3, t3,w3-wb,l3)
}




AND height(n, w) = VALOF
{ // Make it zero on or near the runway or more than landsize away
  // from the runway.
  // Make the height small near the runway and typically larger
  // away from the runway, but keep it small near the coast.
  // n is the distance north
  // w is the distance west
  LET halfsize = landsize/2
  LET h = randheight(n, w,
                     -halfsize, +halfsize, // x coords
                     -halfsize, +halfsize, // y coords
                      0, 0, 0, 0)          // corner heights
  LET dist = (ABS(n - runwaylength/2)) + (ABS(w))
  // dist is the manhatten distance from the centre of the runway.
  LET factor = ?   // Will be in the range 0 to 1.000 depending on dist
  LET d1, d2 = 600_000, 3_000_000
  LET d3 = landsize - dist
  IF dist <= d1 DO factor := 0
  IF dist >= d2 DO factor := 1_000
  IF d1<dist<d2 DO factor := muldiv(1_000, dist-d1, d2-d1)
  // factor is a function of dist. Below d1 it is zero. Between
  // d1 and d2 it grows linearly to 1.000. Above d2 it remains at 1.000.
//sawritef("dist=%9.3d  factor=%6.3d h=%i9*n", dist, factor, h)
  IF d3<=0 RESULTIS 0  // Above the sea
  h := muldiv(h, factor, 1.000) / 1000

  IF d3 < 2_000_000 DO
  { // 0 < d3 < 2.000.000
    // So over land near the coast.
    // Reduce the height appropriately.
    h := muldiv(h, d3, 2_000_000)
  } 
sawritef("n=%i9 w=%i9 h=%i9  h^2=%i9*n", n, w, h, h*h)
abort(1267)
  RESULTIS (h * h)
}

AND randvalue(x, y, max) = VALOF
{ LET a = 123*x >> 1
  LET b = 541*y >> 3
  LET hashval = ABS((a*b XOR b XOR #x1234567)/3)
  hashval := hashval MOD (max+1)
//sawritef("randvalue: (%i9 %i9 %i9) => %i4*n", x, y, max, hashval) 
  RESULTIS hashval
}

AND randheight(x, y, x0, x1, y0, y1, h0, h1, h2, h3) = VALOF
{ // Return a random height depending on x and y only.
  // The result is in the range 0 to 1000
  LET k0, k1, k2, k3 = ?, ?, ?, ?
  LET size = x1-x0
  LET sz   = size>1_000_000 -> 1_000_000, size/2
  LET sz2  = sz/2

  TEST sz < 100_000
  THEN { // Use linear interpolation based on the heights
         // of the corners.
         // The formula is
         //     h = a + bp + cq + dpq
         // where a = h0
         //       b = h1 - h0
         //       c = h2 - h0
         //       d = h3 - h2 - h1 + h0
         //       p = (x-x0)/(x1-x0) 
         // and   q = (y-y0)/(y1-y0) 
         // This formula agrees with the heights at four the vertices,
         // and for fixed x it is linear in y, and vice-versa.
         LET a = h0
         LET b = h1-h0
         LET c = h2-h0
         LET d = h3-h2-h1+h0
         b := muldiv(b, x-x0, x1-x0)
         c := muldiv(c, y-y0, y1-y0)
         d := muldiv(muldiv(d, x-x0, x1-x0), y-y0, y1-y0)
         RESULTIS a+b+c+d
       }
  ELSE { // Calculate the heights of the vertices of the 1/2 sized square
         // containing x,y.
         LET mx = (x0+x1)/2
         LET my = (y0+y1)/2
         LET mh = (h0+h1+h2+h3)/4 + randvalue(mx, my, sz) - sz2
         TEST x<mx
         THEN TEST y<my
              THEN { // Lower left
                     LET k1 = (h0+h1)/2 + randvalue(mx, y0, sz) - sz2
                     LET k2 = (h0+h2)/2 + randvalue(x0, my, sz) - sz2
                     h1, h2, h3 := k1, k2, mh
                     x1, y1 := mx, my
                     LOOP
                   }
              ELSE { // Upper left
                     LET k0 = (h0+h2)/2 + randvalue(x0, my, sz) - sz2
                     LET k3 = (h2+h3)/2 + randvalue(mx, y1, sz) - sz2
                     h0, h1, h3 := k0, mh, k3 
                     x1, y0 := mx, my
                     LOOP
                   }
         ELSE TEST y<my
              THEN { // Lower right
                     LET k0 = (h0+h1)/2 + randvalue(mx, y0, sz) - sz2
                     LET k3 = (h1+h3)/2 + randvalue(x1, my, sz) - sz2
                     h0, h2, h3 := k0, mh, k3
                     x0, y1 := mx, my
                     LOOP
                   }
              ELSE { // Upper right
                     LET k1 = (h1+h3)/2 + randvalue(x1, my, sz) - sz2
                     LET k2 = (h0+h2)/2 + randvalue(mx, y1, sz) - sz2
                     h0, h1, h2 := mh, k1, k2
                     x0, y0 := mx, my
                     LOOP
                   }
       }
} REPEAT

AND plotland(n, w, size) BE
{ LET sz = size/80
RETURN
/*
  // First plot the sea at level 0
  rgb(50, 0, 200) // Red blue
//size := 1_000_000
  triangleland(-size, -size, -2000, 50, 0, 200,
               +size, -size, -2000, 50, 0, 200,
               +size, +size, -2000, 50, 0, 200)
  triangleland(-size, -size, -2000, 50, 0, 200,
               +size, +size, -2000, 50, 0, 200,
               +size, +size, -2000, 50, 0, 200)

//RETURN

  FOR i = 0 TO 79 DO
  { LET n0 = n + i*sz
    LET n1 = n0 + sz
    FOR j = 0 TO 79 DO
    { LET w0 = w + j*sz
      LET w1 = w0 + sz
      LET h0 = height(n0, w0)
      LET h1 = height(n0, w1)
      LET h2 = height(n1, w1)
      LET h3 = height(n1, w0)
      LET r, g, b = redfn(n0,w0,h0), greenfn(n0,w0,h0), bluefn(n0,w0,h0)
      IF h0<=0 DO r, g, b := 50, 100, 200
//sawritef("calling qualdland(%n,%n,%n,...)*n", n0, w0, h0)
      //quadland(n0,w0,h0, r, g, b,
      //         n0,w1,h1, r, g, b,
      //         n1,w1,h2, r, g, b,
      //         n1,w0,h3, r, g, b)
      triangleland( FLOAT n0, FLOAT w0, FLOAT h0, r,  g, b,
                    FLOAT n0, FLOAT w1, FLOAT h1, r,  g, b,
                    FLOAT n1, FLOAT w1, FLOAT h2, r,  g, b)
      triangleland( FLOAT n0, FLOAT w0, FLOAT h0, r XOR 16,  g XOR 16, b XOR 16,
                    FLOAT n1, FLOAT w1, FLOAT h2, r XOR 16,  g XOR 16, b XOR 16,
                    FLOAT n1, FLOAT w0, FLOAT h3, r XOR 16,  g XOR 16, b XOR 16)

    }
  } 
*/
}

AND plotland1(x0, y0, sx, sy, h0, h1, h2, h3) BE
{ // This construct a rectangle of land with its south western corner
  // at (x0,y0) using world coordinates. The east-west size of the
  // square is sx, and sy is the north-south size. The vertices are
  // numbered 0 to 3 anticlockwise starting ar (x0,y0). 
  LET x2, y2 = x0+sx, y0+sy

  TEST sx > 1000.000
  THEN { FOR i = 0 TO 9 DO
         { LET xa = (x0 * (10-i) + x2 *  i   ) / 10
           LET xb = (x0 * ( 9-i) + x2 * (i+1)) / 10
           LET sx1 = xb-xa

           LET ha = (h0 * (10-i) + h1 *  i   ) / 10
           LET hb = (h0 * ( 9-i) + h1 * (i+1)) / 10
           LET hc = (h2 * ( 9-i) + h2 * (i+1)) / 10
           LET hd = (h2 * (10-i) + h3 *  i   ) / 10

           ha := ha + height(xa, y0, sx1)
           hb := hb + height(xa, y0, sx1)
           hc := hc + height(xb, y2, sx1)
           hd := hd + height(xb, y2, sx1)

           FOR j = 0 TO 9 DO
           { LET ya = (y0 * (10-j) + y2 *  j   ) / 10
             LET yb = (y0 * ( 9-j) + y2 * (j+1)) / 10
             LET sy1 = yb-ya

             LET ka = (ha * (10-j) + hd *  j   ) / 10
             LET kb = (hb * ( 9-j) + hc * (j+1)) / 10
             LET kc = (hb * ( 9-j) + hc * (j+1)) / 10
             LET kd = (ha * (10-j) + hd *  j   ) / 10

             ka := ka + height(xa, ya, sy1)
             kb := kb + height(xb, ya, sy1)
             kc := kc + height(xb, yb, sy1)
             kd := kd + height(xa, yb, sy1)

             plotland(xa, ya, sx1, sy1, ka, kb, kc, kd)
           }
         }
       }
  ELSE { LET r, g, b = redfn(x0,y0,h0), greenfn(x0,y0,h0), bluefn(x0,y0,h0)
sawritef("calling qualdland(%n,%n,%n,...)*n", x0, y0, h0)
         quadland(x0,y0,h0, r, g, b,
                  x0,y2,h1, r, g, b,
                  x2,y2,h2, r, g, b,
                  x2,y0,h3, r, g, b)
       }
}

AND redfn(x,y,h)  = VALOF
{ LET col = 10 + h/3_000 +
            ((x * 12345)>>1) MOD  17 +
            ((y * 23456)>>1) MOD  37 +
            ((h * 34567)>>1) MOD  53
  IF col > 255 DO col := 255
  RESULTIS col
}

AND greenfn(x,y,h) = VALOF
{ LET col = 150 + h/3_000 +
            ((x * 123456)>>1) MOD  17 +
            ((y * 234567)>>1) MOD  37 +
            ((h * 345678)>>1) MOD  53
  IF col > 255 DO col := 255
  RESULTIS col
}

AND bluefn(x,y,h) = VALOF
{ LET col = 20 + h/3.000 +
            ((x * 1234567)>>1) MOD  17 +
            ((y * 2345678)>>1) MOD  37 +
            ((h * 3456789)>>1) MOD  53
  IF col > 255 DO col := 255
  RESULTIS col
}



