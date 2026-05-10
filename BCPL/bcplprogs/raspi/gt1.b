/*
################ STILL UNDER DEVELOPMENT ########################

This is a flight simulator for a Tigermoth Biplane implemented in
BCPL using the OpenGL Graphics library.

Implemented by Martin Richards (c) July 2014

The BCPL GL library is in g/gl.b with header g/gl.h and is designed to
work unchanged with either OpenGL using SDL or OpenGL ES using EGL and
some SDL features.


History

08/05/18
Extensively modified to use 32-bit floating point and the FLT feature.

20/12/14
Modified the cube to be like a square missile with control surfaces.
It will display a rotating tigermoth by default. 

03/12/14
Began conversion to use floating point numbers.



Command argument:

-d        Turn on debugging

Controls:

Q  causes quit
P  Output debugging info to the terminal
S  Stop/start the stepping the image

Rotational controls

Right/left arrow Increase/decrease rotation rate about direction of thrust
Up/Down arrow    Increase/decrease rotation rate about direction of left wing
>  <             Increase/decrease rotation rate about direction of lift

R   L            Increase/decrease cgndot
U   D            Increase/decrease cgwdot
F   B            Increase/decrease cghdot

0,1,2,3,4,5,6,7  Set eye direction -- the eye is always looking at
                                      the CG of the aircraft.

8,9              Increase/decrease eye height
+,-              Increase/decrease eye distance

The transformations

Three coordinate systems are used in this program.

The first specifies point (t,w,l) on the aircraft where t is the
distance from the centre of gravity (CG) forward in the direction of
thrust. w is the distance from the CG in the direction of the left
wing, and l is the distance in the direction of lift. These three
directions are at right angles to each other. Mathematicians describe
them as orthogonal.

The second coordinate system (n,w,h) describes points using real world
coordinates. n is the distance north of the origin, w is the distance
west of the origin and h is the distance (height) above the
origin. The origin is chosen to be in the centre line of the runway at
its southern most end. The runway is aligned from south to north.

The third coordinate system (x,y,z) describes points as displayed on
the screen. In this system the origin is the centre of the screen. x
is the distance to the right of the origin and y is the distance above
the origin, and z is the distance from the origin towards the
viewer. Thus the further a point is from the viewer the more negative
will be its z component. These z components are used by the graphics
hardware to remove surfaces that are hidden behind other surfaces.

The orientation of the aircraft is specified by the followin nine
direction cosines.

  ctn; ctw; cth   // Direction cosines of direction t
  cwn; cww; cwh   // Direction cosines of direction w
  cln; clw; clh   // Direction cosines of direction l

  cgn; cgw; cgh   // Coordinates of the CG

  eyedirection    // =0 means the eye is looking horizontally
                  //    in the direction of thrust.
  eyerelh         // Relative to cgh
  eyedistance         holds the distance between the eye and
                      the CG of the aircraft.


  eyepn, eyepw, eyeph specify the real world coordinates of the
                      point (P) the eye is focussing on. P is
                      often the CG of the aircraft.

  eyen, eyew, eyeh    specify real world coordinates of a point
                      on the line of sight of the eye.

Since standard BCPL now supports floating point operations and the
latest Raspberry Pi (Model B-2) has proper support for floating point
this program will phase out scales fixed point arithmetic and use
floating point instead. This is a simple but extensive change.
*/

GET "libhdr"
GET "gl.h"
GET "gl.b"          // Insert the library source code
.
GET "libhdr"
GET "gl.h"

GLOBAL {
  done:ug
  stepping
  debug
  glprog
  Vshader
  Fshader

  VertexLoc       // Attribute variable locations
  ColorLoc
  DataLoc         // data[0]=ctrl  data[1]=value

  ModelMatrixLoc  // Uniform variable locations
  LandMatrixLoc
  ControlLoc

  FLT CosElevator
  FLT SinElevator
  FLT CosRudder
  FLT SinRudder
  FLT CosAileron
  FLT SinAileron

  modelfile // Holds the name of the model file, typically tigermothmodel.mdl

  // The following variables are floating point number

  FLT ctn; FLT ctw; FLT cth   // Direction cosines of direction t
  FLT cwn; FLT cww; FLT cwh   // Direction cosines of direction w
  FLT cln; FLT clw; FLT clh   // Direction cosines of direction l

  FLT rtdot; FLT rwdot; FLT rldot    // Anti-clockwise rotation rates
                         // about the t, w and l axes
 
  FLT cgn; FLT cgw; FLT cgh          // Coordinates of the CG of the aircraft
                         // in feet as floating point numbers
  FLT cgndot; FLT cgwdot; FLT cghdot // CG velocity

  FLT mass                   // Aircraft mass in lbs
  FLT moit                   // Moment of inertia about t axis in ft-lbs
  FLT moiw                   // Moment of inertia about w axis
  FLT moil                   // Moment of inertia about l axis

  // The following are relative to the aircraft CG
  // These are used to check contact with the ground.
  FLT wingtiplt        // Left lower wing tip t coord
  FLT wingtiplw        // Left lower wing tip w coord
  FLT wingtipll        // Left lower wing tip l coord
  FLT wingtiprt        // Right lower wing tip t coord
  FLT wingtiprw        // Right lower wing tip w coord
  FLT wingtiprl        // Right lower wing tip l coord
  FLT wheellt          // Left wheel tip t coord
  FLT wheellw          // Left wheel tip w coord
  FLT wheelll          // Left wheel tip l coord
  FLT wheelrt          // Right wheel tip t coord
  FLT wheelrw          // Right wheel tip w coord
  FLT wheelrl          // Right wheel tip l coord
  FLT skidt            // Tail skid t coord
  FLT skidw            // Tail skid w coord
  FLT skidl            // Tail skid l coord


  eyedirection     // =0 to =7
  FLT eyerelh      // height of the eye relative to cgh

  FLT eyen; FLT eyew; FLT eyeh // Coordinates of a point on the line of sight
                   // from to eye to the centre of the aircraft.
  FLT eyedistance      // The distance between the eye and the centre of
                   // the aircraft.

  // The next four variables must be in consecutive locations
  // since @vvec is passed to loadmodel.
  vvec       // Vector of 32-bit floating point numbers
             // holding the vertex attributes.
  vvecsize   // The number of numbers in vvec.
  ivec       // Vector of 16-bit unsigned integers
  ivecsize   // The number of 16-bit values in ivec
  dvec       // The display items vector
  dvecsize   // The number of display items. (3 index values per item).

  VertexBuffer
  IndexBuffer

  LandMatrix       // The matrix used by the vertex shader
                   // to transform the vertex coordinates of points
                   // on the land to screen coordinates.
  ModelMatrix      // The matrix used by the vertex shader
                   // to transform the vertex coordinates of points
                   // on the model to screen coordinates.
  WorkMatrix       // is used when constructing the projection matrix.
}

LET get16(v, i) = VALOF
{ LET w = 0
  LET p = 2*i
  LET a, b = v%p, v%(p+1)
  (@w)%0 := 1
  TEST (w & 1) = 0
  THEN RESULTIS (a<<8) + b // Big ender m/c 
  ELSE RESULTIS (b<<8) + a // Little ender m/c 
}

LET start() = VALOF
{ LET m1   = VEC 15     // For the ModelMatrix
  LET m2   = VEC 15     // For the LandMatrix
  LET m3   = VEC 15     // For the WorkMatrix
  LET argv = VEC 50
  LET modelfile = "gltiger.mdl"

  ModelMatrix, LandMatrix, WorkMatrix := m1, m2, m3

  UNLESS rdargs("-d/s", argv, 50) DO
  { writef("Bad arguments for gltiger*n")
    RETURN
  }

  debug := argv!0                  // -d/s

  writef("start: calling glInit*n")
  UNLESS glInit() DO
  { writef("*nOpenGL not available*n")
    RESULTIS 0
  }

  writef("start: Calling glMkScreen*n")
  // Create an OpenGL window
  screenxsize := glMkScreen("Tigermoth flight simulator", 800, 680)
  screenysize := result2
  UNLESS screenxsize DO
  { writef("*nUnable to create an OpenGL window*n")
    RESULTIS 0
  }
  writef("Screen Size is %n x %n*n", screenxsize, screenysize)

  writef("start: calling glMkProg  ")
  glprog := sys(Sys_gl, gl_MkProg)
  writef("=> glprog=%n*n", glprog);

  IF glprog<0 DO
  { writef("*nUnable to create a GL program*n")
    RESULTIS 0
  }

  sys(Sys_gl, gl_test)
  abort(1000)
  RESULTIS 0

  // Read and Compile the vertex shader
  writef("start: calling CompileV(%n,gltigerVshader.sdr) ",glprog)
  Vshader := Compileshader(glprog, TRUE, "gltigerVshader.sdr")
  writef("=> Vshader=%n*n", Vshader)

  // Read and Compile the fragment shader
  writef("start: calling CompileF(%n,gltstFshader.sdr) ",glprog)
  Fshader := Compileshader(glprog, FALSE, "gltigerFshader.sdr")
  writef("=> Fshader=%n*n", Fshader)

  // Link the program
  writef("start: calling glLinkProg(%n)*n", glprog)
  UNLESS sys(Sys_gl, gl_LinkProgram, glprog) DO
  { writef("*nUnable to link a GL program*n")
    RESULTIS 0
  }

  //writef("start: calling glUseProgram(%n)*n", glprog)
  sys(Sys_gl, gl_UseProgram, glprog)

  // Get attribute locations after linking
  VertexLoc := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vVertex")
  ColorLoc  := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vColor")
  DataLoc   := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vData")

  writef("VertexLoc=%n*n", VertexLoc)
  writef("ColorLoc=%n*n",  ColorLoc)
  writef("DataLoc=%n*n",   DataLoc)

  // Get uniform locations after linking
  LandMatrixLoc  := sys(Sys_gl, gl_GetUniformLocation, glprog, "landmatrix")
  ModelMatrixLoc := sys(Sys_gl, gl_GetUniformLocation, glprog, "modelmatrix")
  ControlLoc     := sys(Sys_gl, gl_GetUniformLocation, glprog, "control")

  writef("LandMatrixLoc=%n*n",  LandMatrixLoc)
  writef("ModelMatrixLoc=%n*n", ModelMatrixLoc)
  writef("ControlLoc=%n*n",     ControlLoc)

  //writef("start: calling glDeleteShader(%n)*n", Vshader)
  //glDeleteShader(Vshader)
  //writef("start: calling glDeleteShader(%n)*n", Fshader)
  //glDeleteShader(Fshader)

  // Load model

  writef("Calling loadmodel file=%s*n", modelfile)
  UNLESS loadmodel(modelfile, @vvec) DO
  { writef("*nUnable to load model: %s*n", modelfile)
    RESULTIS 0
  }

  IF debug DO
  { // Output the vertex and index data
    // as a debugging aid
    writef("*nVertex data*n")
    FOR i = 0 TO vvecsize-1 DO
    { IF i MOD 8 = 0 DO writef("*n%i3: ", i)
      writef(" %8.3f", vvec!i)
    }
    writef("*n*nIndex data*n")
    FOR i = 0 TO ivecsize-1 DO
    { IF i MOD 10 = 0 DO writef("*n%i6: ", i)
      writef(" %i5", get16(ivec, i))
    }
    writef("*n*nDisplay date*n")
    FOR i = 0 TO dvecsize-1 BY 3 DO
      writef(" %i5  %i5  %i5*n", dvec!i, dvec!(i+1), dvec!(i+2))
    newline()
  }

  mass := 1800.0    // Aircraft mass in pound
  moit :=  200.0    // Moment of inertia about t axis in ft-pounds
  moiw :=  400.0    // Moment of inertia about w axis in ft-pounds
  moil :=  500.0    // Moment of inertia about l axis in ft-pounds

  // The following are relative to the aircraft origin.

  wingtiplt :=  -1.880        // Left lower wing tip t coord
  wingtiplw :=  14.166        // Left lower wing tip w coord
  wingtipll :=  -0.961        // Left lower wing tip l coord

  wingtiprt :=  -1.880        // Right lower wing tip t coord
  wingtiprw := -14.166        // Right lower wing tip w coord
  wingtiprl :=  -0.961        // Right lower wing tip l coord

  wheellt := -0.268           // Left wheel tip t coord
  wheellw :=  2.100           // left wheel tip w coord
  wheelll := -3.800-0.700     // Left wheel tip l coord

  wheelrt := -0.268           // Right wheel tip t coord
  wheelrw := -2.100           // Right wheel tip w coord
  wheelrl := -3.800-0.700     // Right wheel tip l coord

  skidt := -16.500            // Tail skid t coord
  skidw :=     0.0            // Tail skid w coord
  skidl :=  -3.800-0.700      // Tail skid l coord



  IF debug DO
  { // Output the vertex and index data
    // as a debugging aid
    writef("*nVertex data*n")
    FOR i = 0 TO vvecsize-1 DO
    { IF i MOD 8 = 0 DO writef("*n%i3: ", i)
      writef(" %8.3f", vvec!i)
    }
    writef("*n*nIndex data*n")
    FOR i = 0 TO ivecsize-1 DO
    { IF i MOD 10 = 0 DO writef("*n%i6: ", i)
      writef(" %i5", get16(ivec, i))
    }
    writef("*n*nDisplay data item*n")
    FOR i = 0 TO dvecsize-1 BY 3 DO
      writef(" %i5  %i5  %i5*n", dvec!i, dvec!(i+1), dvec!(i+2))
    newline()
  }

  sys(Sys_gl, gl_Enable, GL_DEPTH_TEST) // This call is neccessary
  sys(Sys_gl, gl_DepthFunc, GL_LESS)    // This the default

  // Pixel written if incoming depth < buffer depth
  // This assumes positive Z is into the screen, but
  // remember the depth test is performed after all other
  // transformations have been done.

  // Setup the model using OpenGL objects in the graphics server's
  // memory.
  writef("start: vvecsize=%n*n", vvecsize)
  VertexBuffer := sys(Sys_gl, gl_GenVertexBuffer, vvecsize, vvec)
  // VertexBuffer is the name (a positive integer) of the vertex buffer.

  // Tell GL the positions in vvec of the xyz fields,
  // ie the first 3 words of each 8 word item in vvec
  writef("start: gl_EnableVertexAttribArray  VertexLoc==%n*n", VertexLoc)
  // VertexLoc is the location of the variable g_vVertex used
  // by the vertex shader.
  sys(Sys_gl, gl_EnableVertexAttribArray, VertexLoc);
  sys(Sys_gl, gl_VertexData,
              VertexLoc,     // Attribute number for xyz data
              3,             // 3 floats for xyz
              8,             // 8 floats per vertex item in vertexData
              0)             // Offset in words of the xyz data

  writef("start: VertexData xyz data copied to graphics object %n*n", VertexBuffer)

  // Tell GL the positions in vvec of the rgb fields,
  // ie the second 3 words of each 8 word item in VertexData
  sys(Sys_gl, gl_EnableVertexAttribArray, ColorLoc);
  sys(Sys_gl, gl_VertexData,
              ColorLoc,      // Attribute number rgb data
              3,             // 3 floats for rgb data
              8,             // 8 floats per vertex item in vertexData
              3)             // Offset in words of the rgb data

  writef("start: ColourData rgb data copied to graphics object %n*n", VertexBuffer)

  // Tell GL the positions in vvec of the kd fields,
  // ie word 6 of each 8 word item in vvec
  sys(Sys_gl, gl_EnableVertexAttribArray, DataLoc);
  sys(Sys_gl, gl_VertexData,
              DataLoc,       // Attribute number kd data
              2,             // 2 floats for kd data
              8,             // 8 floats per vertex item in vertexData
              6)             // Offset in words of the kd data

  writef("start: VertexData kd data copied to graphics object %n*n", VertexBuffer)

  freevec(vvec) // Free vvec since all its elements have
                // been sent to the graphics server.
  vvec := 0

  writef("start: ivecsize=%n*n", ivecsize)
  writef("start: GenIndexBuffer    ivec=%n ivecsize=%n*n", ivec, ivecsize)
  IndexBuffer  := sys(Sys_gl, gl_GenIndexBuffer, ivec, ivecsize)

  writef("start: ivec copied to graphics memory object %n*n", IndexBuffer)

  freevec(ivec) // Free ivec since all its elements have
                // been sent to the graphics server.
  ivec := 0

  // Initialise the state

  done     := FALSE
  stepping := FALSE

  cgn,    cgw,    cgh    :=  0.0, 0.0, 20.0
  cgndot, cgwdot, cghdot :=  0.0, 0.0,  0.0


  // Set the initial direction cosines to orient t, w and l in
  // directions -z, -x and y, ie viewing the aircraft from behind.

  ctn, ctw, cth :=   1.0, 0.0, 0.0
  cwn, cww, cwh :=   0.0, 1.0, 0.0
  cln, clw, clh :=   0.0, 0.0, 1.0

  rtdot, rwdot, rldot := 0.0, 0.0, 0.0
  //rtdot := 0.002 // Rotate the model slowly
  //rwdot := 0.003 // Rotate the model slowly
  rldot := -0.002 // Rotate the model slowly

  eyedirection :=  0         // Direction of thrust
  eyerelh      :=  0.0       // Relative to cgh
  eyedistance  := 40.000

  eyen, eyew, eyeh := 1.0, 0.0, 0.0


  IF debug DO
  { setvec( WorkMatrix, 16,
                   2.0,  0.0,  0.0,  0.0,
                   0.0,  1.0,  0.0,  0.0,
                   0.0,  0.0,  1.0,  0.0,
                   0.0,  0.0,  0.0, 10.0
                 )
    setvec( LandMatrix, 16,
                   1.0,  2.0,  3.0,  4.0,
                   5.0,  6.0,  7.0,  8.0,
                   9.0, 10.0, 11.0, 12.0,
                  13.0, 14.0, 15.0, 16.0
                 )
    newline()
    prmat(WorkMatrix)
    writef("times*n")
    prmat(LandMatrix)
    sys(Sys_gl, gl_M4mulM4, WorkMatrix, LandMatrix, LandMatrix)
    writef("gives*n")
    prmat(LandMatrix)
    abort(1000)
  }

//sawritef("Entering main loop*n")

  UNTIL done DO
  { processevents()
//writef("returned from processevents()*n")
    // Only rotate the object if not stepping
    UNLESS stepping DO
    { // If not stepping adjust the orientation of the model.
      rotate(rtdot, rwdot, rldot)

      // Move the centre of the model
      cgn := cgn + cgndot
      cgw := cgw + cgwdot
      cgh := cgh + cghdot
    }

    // We now construct the matrix LandMatrix to transform
    // points in real world coordinated to screen coordinates

    // We assume the eye is looking directly towards the centre
    // of gravity of the model.

    // First rotate world coordinate (n,w,u) to
    // screen coodinates (x,y,z)
    // ie  n -> -z
    //     w -> -x
    //     u ->  y
    // and translate the aircraft and land to place the aircraft CG
    // to the origin

    SWITCHON eyedirection INTO
    { DEFAULT:
      CASE 0: eyen, eyew := -1.000,  0.000; ENDCASE
      CASE 1: eyen, eyew := -0.707, -0.707; ENDCASE
      CASE 2: eyen, eyew :=  0.0,   -1.000; ENDCASE
      CASE 3: eyen, eyew :=  0.707, -0.707; ENDCASE
      CASE 4: eyen, eyew :=  1.0,    0.000; ENDCASE
      CASE 5: eyen, eyew :=  0.707,  0.707; ENDCASE
      CASE 6: eyen, eyew :=  0.0,    1.000; ENDCASE
      CASE 7: eyen, eyew := -0.707,  0.707; ENDCASE
    }

    eyeh := eyerelh

    // Matrix to move aircraft and land so that the CG of
    // the aircraft is at the origin

    setvec( LandMatrix, 16,
                      1.0,  0.0,  0.0, 0.0,   // column 1
                      0.0,  1.0,  0.0, 0.0,   // column 2
                      0.0,  0.0,  1.0, 0.0,   // column 3
                     -cgn, -cgw, -cgh, 1.0    // column 4
             )

    // Rotate the model and eye until the eye is on the z axis
    
    { LET FLT en, FLT ew, FLT eh = eyen, eyew, eyeh
      LET FLT oq = glRadius2(en, ew) 
      LET FLT op = glRadius3(en, ew, eh)
      LET FLT cos_theta = -en / oq 
      LET FLT sin_theta = -ew / oq 
      LET FLT cos_phi   =  oq / op 
      LET FLT sin_phi   =  eh / op 

      // Rotate anti-clockwise about h axis by angle theta
      // to move the eye onto the nh plane.
      setvec( WorkMatrix, 16,
                  cos_theta, -sin_theta, 0.0, 0.0,   // column 1
                  sin_theta,  cos_theta, 0.0, 0.0,   // column 2
                        0.0,        0.0, 1.0, 0.0,   // column 3
                        0.0,        0.0, 0.0, 1.0    // column 4
               )
//sawritef("Rotation matrix R1*n")
//prmat(LandMatrix)
//abort(1000)
      sys(Sys_gl, gl_M4mulM4, WorkMatrix, LandMatrix, LandMatrix)

      //newline()
      //writef("eyen=%6.3d eyew=%6.3d eyeh=%6.3d*n", eyen, eyew, eyeh)
      //writef("cgn= %6.3d cgw= %6.3d cgh= %6.3d*n", cgn,   cgw,  cgh)
      //writef("cos and sin of theta and phi: "); prv(@cos_theta); newline()
      //writef("Matrix to rotate and translate the model*n")
      //writef("and move the eye into the yz plane*n")
      //dbmatrix(LandMatrix)

      // Rotate clockwise about w axis by angle phi
      // to move the eye onto the n axis. 
      setvec( WorkMatrix, 16,
            cos_phi, 0.0, -sin_phi, 0.0,    // column 1
                0.0, 1.0,      0.0, 0.0,    // column 2
            sin_phi, 0.0,  cos_phi, 0.0,    // column 3
                0.0, 0.0,      0.0, 1.0     // column 4
               )
//sawritef("Rotation matrix R2*n")
//prmat(WorkMatrix)
//abort(1000)
      sys(Sys_gl, gl_M4mulM4, WorkMatrix, LandMatrix, LandMatrix)

      //newline()
      //writef("Matrix to rotate and translate the model*n")
      //writef("and move the eye onto the z axis*n")
      //dbmatrix(LandMatrix)
    }

// Matrix to transform world coordinates (n,w,h) to
// to screen coordinated ((x,y,z)
// ie x = -w
//    y =  h
//    z = -n

    setvec(WorkMatrix, 16,
                    0.0,  0.0, -1.0, 0.0,   // column 1
                   -1.0,  0.0,  0.0, 0.0,   // column 2
                    0.0,  1.0,  0.0, 0.0,   // column 3
                    0.0,  0.0,  0.0, 1.0)   // column 4

    sys(Sys_gl, gl_M4mulM4, WorkMatrix, LandMatrix, LandMatrix)


    // Change the origin to the eye position on the z axis by
    // moving the model eyedistance in the negative z direction.
    setvec( WorkMatrix, 16,
              1.0, 0.0,          0.0, 0.0, // column 1
              0.0, 1.0,          0.0, 0.0, // column 2
              0.0, 0.0,          1.0, 0.0, // column 3
              0.0, 0.0, -eyedistance, 1.0) // column 4

//sawritef("Change to eye origin matrix*n")
//prmat(WorkMatrix)
//abort(1000)
    sys(Sys_gl, gl_M4mulM4, WorkMatrix, LandMatrix, LandMatrix)

    //newline()
    //writef("Matrix to rotate and translate the model*n")
    //writef("and move the eye onto the z axis*n")
    //writef("and move the eye a distance in the z direction*n")
    //dbmatrix(LandMatrix)

    // The perspective matrix can be set using the glSetPerspective library
    // function defined in g/gl.b

    glSetPerspective(WorkMatrix,
                     FLOAT screenxsize / FLOAT screenysize, // Aspect ratio
                        1.0, // Field of view at unit distance
                        0.1, // Distance to near limit
                     5000.0) // Distance to far limit

//sawritef("work matrix*n")
//prmat(WorkMatrix)
//sawritef("Projection matrix*n")
//prmat(LandMatrix)
    sys(Sys_gl, gl_M4mulM4, WorkMatrix, LandMatrix, LandMatrix)
//sawritef("final Projection matrix*n")
//dbmatrix(LandMatrix)

//  writef("*nn=%8.3f f=%8.3f l=%8.3f r=%8.3f b=%8.3f t=%8.3f*n",
//            n,f,l,r,b,t)

    // Send the LandMatrix to uniform variable "landmatrix" for
    // use by the vertex shader transform land points.
    sys(Sys_gl, gl_UniformMatrix4fv, LandMatrixLoc, glprog, LandMatrix)

    // Set the model rotation matrix from model
    // coordinates (t,w,l) to world coordinates (x,y,z)
    setvec( ModelMatrix, 16,
                    ctn,  ctw, cth, 0.0,  // column 1
                    cwn,  cww, cwh, 0.0,  // column 2
                    cln,  clw, clh, 0.0,  // column 3
                    0.0,  0.0, 0.0, 1.0)  // column 4

    ///newline()
    ///writef("Matrix to rotate the model*n")
    ///dbmatrix(LandMatrix)

    // Set the model's centre to (cgn,cgw,cgh)
    setvec( WorkMatrix, 16,
                  1.0, 0.0, 0.0, 0.0,    // column 1
                  0.0, 1.0, 0.0, 0.0,    // column 2
                  0.0, 0.0, 1.0, 0.0,    // column 3
                  cgn, cgw, cgh, 1.0)     // column 4

//sawritef("Translation matrix*n")
//prmat(WorkMatrix)
//abort(1000)

    sys(Sys_gl, gl_M4mulM4, WorkMatrix, ModelMatrix, ModelMatrix)

    //newline()
    //writef("Matrix to rotate and translate the model*n")
    //dbmatrix(ModelMatrix)
    //abort(1000)

    // Now apply the projection transformation to the model matrix
    sys(Sys_gl, gl_M4mulM4, LandMatrix, ModelMatrix, ModelMatrix)

    // Send the ModelMatrix to uniform variable "modelmatrix" for
    // use by the vertex shader to transform points of the model.
    sys(Sys_gl, gl_UniformMatrix4fv, ModelMatrixLoc, glprog, ModelMatrix)

    // Calculate the cosines and sines of the control surfaces.
    { LET RudderAngle = -rldot * 100.0
      CosRudder := sys(Sys_flt, fl_cos, RudderAngle)
      SinRudder := sys(Sys_flt, fl_sin, RudderAngle)
//writef("RudderAngle = %9.3f  cos=%5.3f   sin=%5.3f*n",
//        RudderAngle, CosRudder, SinRudder)
    }

    { LET ElevatorAngle = rwdot * 100.0
      CosElevator := sys(Sys_flt, fl_cos, ElevatorAngle)
      SinElevator := sys(Sys_flt, fl_sin, ElevatorAngle)
//writef("ElevatorAngle = %9.3f  cos=%5.3f   sin=%5.3f*n",
//        ElevatorAngle, CosElevator, SinElevator)
    }

    { LET AileronAngle = rtdot * 100.0
      CosAileron := sys(Sys_flt, fl_cos, AileronAngle)
      SinAileron := sys(Sys_flt, fl_sin, AileronAngle)
    }

    // Send them to the graphics hardware as elements of the
    // uniform matrix "control" for use by the vertex shader.
    { LET control = VEC 15

      control!00 :=  CosRudder    // 0 0
      control!01 :=  SinRudder    // 0 1
      control!02 :=  CosElevator  // 0 2
      control!03 :=  SinElevator  // 0 3
      control!04 :=  CosAileron   // 1 0
      control!05 :=  SinAileron   // 1 1
      FOR i = 6 TO 15 DO control!i := 0.0

      // Send the control values to the graphics hardware
      sys(Sys_gl, gl_UniformMatrix4fv, ControlLoc, glprog, control)
    }

    // Draw a new image
    sys(Sys_gl, gl_ClearColour, 160, 160, 250, 255)
    sys(Sys_gl, gl_ClearBuffer) // Clear colour and depth buffers

    drawmodel()

IF FALSE DO
    FOR i = -1 TO 1 BY 2 DO
    { // Draw half size images either side -- Does not work yet
      setvec( LandMatrix, 16,
                   ctn/10.0,  ctw/10.0, cth/10.0, 0.0, // column 1
                   cwn/10.0,  cww/10.0, cwh/10.0, 0.0, // column 2
                   cln/10.0,  clw/10.0, clh/10.0, 0.0, // column 3
         cgn+0.450*(FLOAT i),      cgw,      cgh, 1.0) // column 4

      glSetPerspective(WorkMatrix, 1.0, 0.5, 0.1, 5000.0)
      sys(Sys_gl, gl_M4mulM4, WorkMatrix, LandMatrix, LandMatrix)

      // Send the matrix to uniform variable "matrix" for use
      // by the vertex shader.
      sys(Sys_gl, gl_UniformMatrix4fv, ModelMatrixLoc, glprog, LandMatrix)

      drawmodel()
    }

    sys(Sys_gl, gl_SwapBuffers)

    delay(0_020) // Delay for 1/50 sec
//abort(1000)
  }

  sys(Sys_gl, gl_DisableVertexAttribArray, VertexLoc)
  sys(Sys_gl, gl_DisableVertexAttribArray, ColorLoc)
  sys(Sys_gl, gl_DisableVertexAttribArray, DataLoc)

  delay(0_050)
  sys(Sys_gl, gl_Quit)

  RESULTIS 0
}

AND Compileshader(prog, isVshader, filename) = VALOF
{ // Create and compile a shader whose source code is
  // in a given file.
  // isVshader=TRUE if compiling a vertex shader
  // isVshader=FALSE if compiling a fragment shader
  LET oldin = input()
  LET oldout = output()
  LET buf = 0
  LET shader = 0
  LET ramstream = findinoutput("RAM:")
  LET instream = findinput(filename)
  UNLESS ramstream & instream DO
  { writef("Compileshader: Trouble with i/o streams*n")
    RESULTIS -1
  }

  //Copy shader program to RAM:
  //writef("Compiling shader %s*n", filename)
  selectoutput(ramstream)
  selectinput(instream)

  { LET ch = rdch()
    IF ch=endstreamch BREAK
    wrch(ch)
  } REPEAT

  wrch(0) // Place the terminating byte

  selectoutput(oldout)
  endstream(instream)
  selectinput(oldin)

  buf := ramstream!scb_buf

  shader := sys(Sys_gl,
                (isVshader -> gl_CompileVshader, gl_CompileFshader),
                prog,
                buf)

//writef("Compileshader: shader=%n*n", shader)
  endstream(ramstream)
  RESULTIS shader
}

AND drawmodel() BE
{ // Draw the primitives using vertex and index data held in
  // graphics objects as specified by the display items in dvec.
  FOR p = 0 TO dvecsize-1 DO
  { LET d = @dvec!(3*p)
    LET mode   = d!0  // Points, Lines, Linestrip, etc.
    LET size   = d!1  // Number of index elements.
    LET offset = d!2  // Offset in the index vector.

//writef("drawmodel: mode=%n, offset=%n size=%n*n", mode, offset, size)

    sys(Sys_gl, gl_DrawElements,
                   mode,     // 1=points, 2=lines, 3=linestrip, etc
                   size,     // Number of index elements to use.
                   2*offset) // Index start position in bytes.
  }
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ DEFAULT:
    //writef("processevents: Unknown event type = %n*n", eventtype)
    LOOP

  CASE sdle_keydown:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:  LOOP

      CASE 'Q': done := TRUE
                LOOP

      CASE 'A': abort(5555)
                LOOP

      CASE 'P': // Print direction cosines and other data
                newline()
                writef("xyz=   %9.3f %9.3f %9.3f*n",
                       cgn,cgw,cgh)
                writef("ct     %9.6f %9.6f %9.6d rtdot=%9.6d*n",
                       ctn,ctw,cth, rtdot)
                writef("cw     %9.6f %9.6f %9.6d rwdot=%9.6d*n",
                       cwn,cww,cwh, rwdot)
                writef("cl     %9.6f %9.6f %9.6d rldot=%9.6d*n",
                       cln,clw,clh, rldot)
                newline()
                writef("eyedirection %n*n", eyedirection)
                writef("eyepos %9.3f %9.3f %9.3d*n",
                        eyen, eyew, eyeh)
                writef("eyedistance = %9.3f*n", eyedistance)
                LOOP

      CASE 'S': stepping := ~stepping
                LOOP

      CASE 'L': // Increase cgwdot
                cgwdot := cgwdot #+ 0.05
                LOOP

      CASE 'R': // Decrease cgwdot
                cgwdot := cgwdot #- 0.05
                LOOP

      CASE 'U': // Increase cghdot
                cghdot := cghdot #+ 0.05
                LOOP

      CASE 'D': // Decrease cghdot
                cghdot := cghdot #- 0.05
                LOOP

      CASE 'F': // Increase cgndot
                cgndot := cgndot #+ 0.05
                LOOP

      CASE 'B': // Decrease cgndot
                cgndot := cgndot #- 0.05
                LOOP

      CASE '0':
      CASE '1':
      CASE '2':
      CASE '3':
      CASE '4':
      CASE '5':
      CASE '6':
      CASE '7': eyedirection := eventa2 - '0'
                LOOP

      CASE '8': eyerelh := eyerelh #+ 0.1; LOOP
      CASE '9': eyerelh := eyerelh #+ #- 0.1; LOOP

      CASE '=':
      CASE '+': eyedistance := eyedistance #* 1.1; LOOP

      CASE '_':
      CASE '-': IF eyedistance#>=1.0 DO
                   eyedistance := eyedistance #/ 1.1
                LOOP

      CASE '>':CASE '.':    rldot := rldot #+ 0.0005; LOOP
      CASE '<':CASE ',':    rldot := rldot #- 0.0005; LOOP

      CASE sdle_arrowdown:  rwdot := rwdot #+ 0.0005; LOOP
      CASE sdle_arrowup:    rwdot := rwdot #- 0.0005; LOOP

      CASE sdle_arrowleft:  rtdot := rtdot #+ 0.0005; LOOP
      CASE sdle_arrowright: rtdot := rtdot #- 0.0005; LOOP
    }
    LOOP

  CASE sdle_quit:             // 12
    writef("QUIT*n");
    sys(Sys_gl, gl_Quit)
    LOOP

  CASE sdle_videoresize:      // 14
    //writef("videoresize*n", eventa1, eventa2, eventa3)
    LOOP
}

AND inprod(a,b,c, x,y,z) =
  // Return the cosine of the angle between two unit vectors.
  a #* x #+ b #* y #+ c #* z

AND rotate(t, w, l) BE
{ // Rotate the orientation of the aircraft
  // t, w and l are assumed to be small and cause
  // rotation about axis t, w, l. Positive values cause
  // anti-clockwise rotations about their axes.

  LET tx = inprod(1.0, #-l,   w,  ctn,cwn,cln)
  LET wx = inprod(  l, 1.0, #-t,  ctn,cwn,cln)
  LET lx = inprod(#-w,   t, 1.0,  ctn,cwn,cln)

  LET ty = inprod(1.0, #-l,   w,  ctw,cww,clw)
  LET wy = inprod(  l, 1.0, #-t,  ctw,cww,clw)
  LET ly = inprod(#-w,   t, 1.0,  ctw,cww,clw)

  LET tz = inprod(1.0, #-l,   w,  cth,cwh,clh)
  LET wz = inprod(  l, 1.0, #-t,  cth,cwh,clh)
  LET lz = inprod(#-w,   t, 1.0,  cth,cwh,clh)

  ctn, ctw, cth := tx, ty, tz
  cwn, cww, cwh := wx, wy, wz
  cln, clw, clh := lx, ly, lz

  adjustlength(@ctn);      adjustlength(@cwn);      adjustlength(@cln) 
  adjustortho(@ctn, @cwn); adjustortho(@ctn, @cln); adjustortho(@cwn, @cln)
}

AND adjustlength(v) BE
{ // Make v a vector of unit length
  LET FLT r = glRadius3(v!0, v!1, v!2)
  v!0 := v!0 / r
  v!1 := v!1 / r
  v!2 := v!2 / r
}

AND adjustortho(a, b) BE
{ // Attempt to keep the unit vector b orthogonal to a
  LET FLT a0, FLT a1, FLT a2 = a!0, a!1, a!2
  LET FLT b0, FLT b1, FLT b2 = b!0, b!1, b!2
  LET FLT corr = a0*b0 + a1*b1 + a2*b2
  b!0 := b0 - a0 * corr
  b!1 := b1 - a1 * corr
  b!2 := b2 - a2 * corr
}

AND prmat(m) BE
{ prf8_3(m! 0)
  prf8_3(m! 4)
  prf8_3(m! 8)
  prf8_3(m!12)
  newline()
  prf8_3(m! 1)
  prf8_3(m! 5)
  prf8_3(m! 9)
  prf8_3(m!13)
  newline()
  prf8_3(m! 2)
  prf8_3(m! 6)
  prf8_3(m!10)
  prf8_3(m!14)
  newline()
  prf8_3(m! 3)
  prf8_3(m! 7)
  prf8_3(m!11)
  prf8_3(m!15)
  newline()
}

AND prv(v) BE
{ prf8_3(v!0)
  prf8_3(v!1)
  prf8_3(v!2)
  prf8_3(v!3)
}

AND prf8_3(x) BE writef(" %8.3f", x)

AND dbmatrix(m) BE //IF FALSE DO
{ LET x,y,z,w = ?,?,?,?
  LET v = @x
  LET n, p, one = #-0.5, #+0.5, 1.0
  prmat(m); newline()
  x,y,z,w := 1.0,0.0,0.0,1.0
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  x,y,z,w := 0.0,1.0,0.0,1.0
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  x,y,z,w := 0.0,0.0,1.0,1.0
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()

  x,y,z,w := n,n,p,one
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  x,y,z,w := p,n,p,one
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  x,y,z,w := p,n,n,one
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  x,y,z,w := n,n,n,one
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()

  x,y,z,w := n,p,p,one
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  x,y,z,w := p,p,p,one
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  x,y,z,w := p,p,n,one
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  x,y,z,w := n,p,n,one
  prv(v); sys(Sys_gl, gl_M4mulV, m, v, v); writef(" => "); prv(v); newline()
  newline()
}
