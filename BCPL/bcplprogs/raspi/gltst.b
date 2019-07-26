/*
This program is a simple demonstration of the OpenGL interface.

The BCPL GL library is in g/gl.b with header g/gl.h and is designed to
work unchanged with either OpenGL using SDL or OpenGL ES using EGL.

Implemented by Martin Richards (c) July 2014

History

03/05/18
Extensively modified to use floating point and the FLT feature.

23/03/15
Simplified this program to only display gltst.mdl with limited control.

20/12/14
Modified the cube to be like a square missile with control surfaces.

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
Up/Down arrow    Increase/decrease rotation rate about direction of right wing
>  <             Increase/decrease rotation rate about direction of lift

0,1,2,3,4,5,6,7  Set eye direction -- The eye is always looking
                                      towards the origin.
+,-              Increase/decrease eye distance

The transformations

The model is represented using three axes t (the direction of thrust),
w the direction of the left wing and l (the direction of lift,
orthogonal to t and w). These use the right hand convention, ie t is
forward, w is left and l is up.

Real world coordinate use axes x (right), y(up) and z(towards the
viewer). These also use the right hand convention.

  ctx; cty; ctz   // Direction cosines of direction t
  cwx; cwy; cwz   // Direction cosines of direction w
  clx; cly; clz   // Direction cosines of direction l

  eyex, eyey, eyez specify a point on the line of sight
                   between the eye and the origin. The line of
                   sight is towards the origin from this point.

  eyedistance holds the distance between the eye and the origin.

Since standard BCPL now supports floating point operations and the
latest Raspberry Pi (Model B-2 and later) has proper support for
floating point this program now uses floating point and the FLT
feature.
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

  VertexLoc  // Attribute variable locations
  ColorLoc
  DataLoc    // data[0]=ctrl  data[1]=value

  MatrixLoc  // Uniform variable locations
  ControlLoc

  FLT CosElevator
  FLT SinElevator
  FLT CosRudder
  FLT SinRudder
  FLT CosAileron
  FLT SinAileron

  modelfile // Holds the name of the model file, typically gltst.mdl

  FLT ctx; FLT cty; FLT ctz   // Direction cosines of direction t
  FLT cwx; FLT cwy; FLT cwz   // Direction cosines of direction w
  FLT clx; FLT cly; FLT clz   // Direction cosines of direction l

  FLT rtdot; FLT rwdot; FLT rldot // Anti-clockwise rotation rates
                                  // about the t, w and l axes
 
  FLT eyex; FLT eyey; FLT eyez // Coordinates of a point on the
                               // line of sight from to eye to
                               // the origin (0.0, 0.0, 0.0).
  FLT eyedistance              // The distance between the eye
                               // and the origin.

  FLT cent; FLT cenw; FLT cenl 

  // The next variables must be in consecutive locations
  // since @vvec is passed to loadmodel.
  vvec       // Vector of 32-bit floating point numbers
             // holding the vertex attributes.
  vvecsize   // The number of numbers in vvec.
  ivec       // Vector of 16-bit unsigned integers
  ivecsize   // The number of 16-bit integers in ivec
  dvec       // The display items vector
  dvecsize   // The number of values in dvec

  VertexBuffer// To hold all the vertex data we ever need.
  IndexBuffer // To hold all the index data we ever need.

  projectionMatrix // is the matrix used by the vertex shader
                   // to transform the vertex coordinates to
                   // screen coordinates.
  workMatrix       // is used when constructing the projection matrix.
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
{ LET m1 = VEC 15
  LET m2 = VEC 15
  LET argv = VEC 50
  LET modelfile = "tigermothmodel.mdl"

  projectionMatrix, workMatrix := m1, m2

  UNLESS rdargs("-d/s,-a/s", argv, 50) DO
  { writef("Bad arguments for gltst*n")
    RETURN
  }

  debug := argv!0                      // -d/s
  IF argv!1 DO                         // -a/s
    modelfile := "gltst.mdl"

  UNLESS glInit() DO
  { writef("*nOpenGL not available*n")
    RESULTIS 0
  }

  writef("start: calling glMkScreen*n")
  // Create an OpenGL window
  screenxsize := glMkScreen("OpenGL First Test", 800, 680)
  screenysize := result2
  UNLESS screenxsize DO
  { writef("*nUnable to create an OpenGL window*n")
    RESULTIS 0
  }
  writef("Screen Size is %n x %n*n", screenxsize, screenysize)

  glprog := sys(Sys_gl, GL_MkProg)
  writef("=> glprog=%n*n", glprog);

  IF glprog<0 DO
  { writef("*nUnable to create a GL program*n")
    RESULTIS 0
  }

  // Read and Compile the vertex shader
  writef("start: calling CompileV(%n,gltstVshader.sdr) ",glprog)
  Vshader := Compileshader(glprog, TRUE, "gltstVshader.sdr")
  writef("=> Vshader=%n*n", Vshader)

  // Read and Compile the fragment shader
  writef("start: calling CompileF(%n,gltstFshader.sdr) ",glprog)
  Fshader := Compileshader(glprog, FALSE, "gltstFshader.sdr")
  writef("=> Fshader=%n*n", Fshader)

  // Link the program
  writef("start: calling glLinkProg(%n)*n", glprog)
  UNLESS sys(Sys_gl, GL_LinkProgram, glprog) DO
  { writef("*nUnable to link a GL program*n")
    RESULTIS 0
  }

  //writef("start: calling glUseProgram(%n)*n", glprog)
  sys(Sys_gl, GL_UseProgram, glprog)

  // Get attribute locations after linking
  VertexLoc := sys(Sys_gl, GL_GetAttribLocation, glprog, "g_vVertex")
  ColorLoc  := sys(Sys_gl, GL_GetAttribLocation, glprog, "g_vColor")
  DataLoc   := sys(Sys_gl, GL_GetAttribLocation, glprog, "g_vData")

  writef("VertexLoc=%n*n", VertexLoc)
  writef("ColorLoc=%n*n",  ColorLoc)
  writef("DataLoc=%n*n",   DataLoc)

  // Get uniform locations after linking
  MatrixLoc      := sys(Sys_gl, GL_GetUniformLocation, glprog, "matrix")
  ControlLoc     := sys(Sys_gl, GL_GetUniformLocation, glprog, "control")

  writef("MatrixLoc=%n*n",  MatrixLoc)
  writef("ControlLoc=%n*n", ControlLoc)

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
    writef("*n*nDisplay data item*n")
    FOR i = 0 TO dvecsize-1 BY 3 DO
      writef(" %i5  %i5  %i5*n", dvec!i, dvec!(i+1), dvec!(i+2))
    newline()
  }

  sys(Sys_gl, GL_Enable, GL_DEPTH_TEST) // This call is neccessary
  sys(Sys_gl, GL_DepthFunc, GL_LESS)    // This the default

  // A pixel written if incoming depth < buffer depth
  // This assumes positive Z is into the screen, but
  // remember the depth test is performed after all other
  // transformations have been done.

  // Setup the model using OpenGL objects in the graphics server's
  // memory.
  writef("start: vvecsize=%n*n", vvecsize)
  VertexBuffer := sys(Sys_gl, GL_GenVertexBuffer, vvecsize, vvec)
  // VertexBuffer is the name (a positive integer) of the vertex buffer.

  // Tell GL the positions in vvec of the xyz fields,
  // ie the first 3 words of each 8 word item in vvec
  writef("start: GL_EnableVertexAttribArray  VertexLoc==%n*n", VertexLoc)
  // VertexLoc is the location of the variable g_vVertex used
  // by the vertex shader.
  sys(Sys_gl, GL_EnableVertexAttribArray, VertexLoc);
  sys(Sys_gl, GL_VertexData,
              VertexLoc,     // Attribute number for xyz data
              3,             // 3 floats for xyz
              8,             // 8 floats per vertex item in vertexData
              0)             // Offset in words of the xyz data

  writef("start: VertexData xyz data copied to graphics object %n*n", VertexBuffer)

  // Tell GL the positions in vvec of the rgb fields,
  // ie the second 3 words of each 8 word item in vvec
  sys(Sys_gl, GL_EnableVertexAttribArray, ColorLoc);
  sys(Sys_gl, GL_VertexData,
              ColorLoc,      // Attribute number rgb data
              3,             // 3 floats for rgb data
              8,             // 8 floats per vertex item in vertexData
              3)             // Offset in words of the rgb data

  writef("start: ColourData rgb data copied to graphics object %n*n", VertexBuffer)

  // Tell GL the positions in vvec of the kd fields,
  // ie word 6 of each 8 word item in vvec
  sys(Sys_gl, GL_EnableVertexAttribArray, DataLoc);
  sys(Sys_gl, GL_VertexData,
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
  IndexBuffer  := sys(Sys_gl, GL_GenIndexBuffer, ivec, ivecsize)

  writef("start: IndexData copied to graphics memory object %n*n", IndexBuffer)

  freevec(ivec) // Free ivec since all its elements have
                // been sent to the graphics server.
  ivec := 0

  // Initialise the state

  done     := FALSE
  stepping := FALSE

  // Set the initial direction cosines to orient t, w and l in
  // directions -z, -x and y, ie viewing the aircraft from behind.

  ctx, cty, ctz :=   0.0,  0.0, -1.0
  cwx, cwy, cwz :=  -1.0,  0.0,  0.0
  clx, cly, clz :=   0.0,  1.0,  0.0

  rtdot, rwdot, rldot := 0.000, 0.001, 0.002 // Rotate the model slowly

  cent, cenw, cenl := 0.0, 0.0, 0.0 // position in the aircraft to
                                    // place at the centre of the screen.

  eyex, eyey, eyez := 0.0, 0.0, 1.0

  eyedistance := 80.000

  IF debug DO
  { setvec( workMatrix, 16,
                   2.0,  0.0,  0.0,  0.0,   // Col 0
                   1.0,  1.0,  1.0,  1.0,   // Col 1
                   0.0,  0.0,  1.0,  0.0,   // Col 2
                   0.0,  0.0,  0.0, 10.0)   // Col 3

    setvec( projectionMatrix, 16,
                   1.0,  2.0,  3.0,  4.0,   // Col 0
                   5.0,  6.0,  7.0,  8.0,   // Col 1
                   9.0, 10.0, 11.0, 12.0,   // Col 2
                  13.0, 14.0, 15.0, 16.0)   // Col 3

    newline()
    prmat(workMatrix)
    writef("times*n")
    prmat(projectionMatrix)
    sys(Sys_gl, GL_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)
    writef("gives*n")
    prmat(projectionMatrix)
    abort(1000)
  }

  UNTIL done DO
  { processevents()

    // Only rotate the object if not stepping
    UNLESS stepping DO
    { // If not stepping adjust the orientation of the model.
      rotate(rtdot, rwdot, rldot)
    }

    // Move the model frwad, left and up by specified amounts
    setvec( projectionMatrix, 16,
                1.0,   0.0,  0.0, 0.0,  // column 1
                0.0,   1.0,  0.0, 0.0,  // column 2
                0.0,   0.0,  1.0, 0.0,  // column 3
                cent, cenw, cenl, 1.0)  // column 4


    // Set the model rotation matrix from model
    // coordinates (t,w,l) to world coordinates (x,y,z)
    setvec( workMatrix, 16,
                    ctx,   cty,  ctz, 0.0,  // column 1
                    cwx,   cwy,  cwz, 0.0,  // column 2
                    clx,   cly,  clz, 0.0,  // column 3
                    0.0,   0.0,  0.0, 1.0)  // column 4

    sys(Sys_gl, GL_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)


    // Rotate the model and eye until the eye is on the z axis

    { LET FLT ex, FLT ey, FLT ez = eyex, eyey, eyez
      LET FLT oq = glRadius2(ex, ez) 
      LET FLT op = glRadius3(ex, ey, ez)
      LET FLT cos_theta = ez / oq 
      LET FLT sin_theta = ex / oq 
      LET FLT cos_phi   = oq / op 
      LET FLT sin_phi   = ey / op 

      // Rotate anti-clockwise about Y axis by angle theta
      setvec( workMatrix, 16,
                  cos_theta, 0.0, sin_theta, 0.0,   // column 1
                        0.0, 1.0,       0.0, 0.0,   // column 2
                 -sin_theta, 0.0, cos_theta, 0.0,   // column 3
                        0.0, 0.0,       0.0, 1.0    // column 4
               )

      sys(Sys_gl, GL_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)

      // Rotate clockwise about X axis by angle phi
      setvec( workMatrix, 16,
                1.0,     0.0,       0.0, 0.0,    // column 1
                0.0, cos_phi,  -sin_phi, 0.0,    // column 2
                0.0, sin_phi,   cos_phi, 0.0,    // column 3
                0.0,     0.0,       0.0, 1.0)    // column 4

      sys(Sys_gl, GL_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)

      // Change the origin to the eye position on the z axis by
      // moving the model eyedistance in the negative z direction.
      setvec( workMatrix, 16,
                1.0, 0.0,           0.0, 0.0, // column 1
                0.0, 1.0,           0.0, 0.0, // column 2
                0.0, 0.0,           1.0, 0.0, // column 3
                0.0, 0.0,  -eyedistance, 1.0  // column 4
              )

      sys(Sys_gl, GL_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)
    }

    { // Define the truncated pyramid for the view projection
      // using the frustrum transformation.
      LET FLT n, FLT f = 0.1, 5000.0
      LET FLT fan, FLT fsn = f+n, f-n
      LET FLT n2 = 2.0*n
      LET FLT l,   FLT r   = -0.5, 0.5
      LET FLT ral, FLT rsl =  r+l, r-l
      LET FLT b,   FLT t   = -0.5, 0.5 
      LET FLT tab, FLT tsb =  t+b, t-b

      LET FLT aspect = FLOAT screenxsize / FLOAT screenysize
      LET FLT fv = 2.0 / 0.5  // Half field of view at unit distance
      setvec( workMatrix, 16,
             fv/aspect,  0.0,             0.0,  0.0, // column 1
                   0.0,   fv,             0.0,  0.0, // column 2
                   0.0,  0.0,     (f+n)/(n-f), -1.0, // column 3
                   0.0,  0.0, (2.0*f*n)/(n-f),  0.0  // column 4
               )

      // This perspective matrix could be set more conveniently using
      // glSetPerspective library function defined in g/gl.b
      //glSetPerspective(workMatrix,
      //                     aspect, // Aspect ratio
      //                        0.5, // Field of view at unit distance
      //                        0.1, // Distance to near limit
      //                     5000.0) // Distance to far limit

      sys(Sys_gl, GL_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)
    }


    // Send the resulting matrix to the uniform variable "matrix" for
    // use by the vertex shader.
    sys(Sys_gl, GL_UniformMatrix4fv, MatrixLoc, glprog, projectionMatrix)

    // Calculate the cosines and sines of the control surfaces.
    { LET FLT RudderAngle = - rldot * 75.0
      CosRudder := sys(Sys_flt, fl_cos, RudderAngle)
      SinRudder := sys(Sys_flt, fl_sin, RudderAngle)
    }

    { LET FLT ElevatorAngle = rwdot * 100.0
      CosElevator := sys(Sys_flt, fl_cos, ElevatorAngle)
      SinElevator := sys(Sys_flt, fl_sin, ElevatorAngle)
    }

    { LET FLT AileronAngle = rtdot * 100.0
      CosAileron := sys(Sys_flt, fl_cos, AileronAngle)
      SinAileron := sys(Sys_flt, fl_sin, AileronAngle)
    }

    // Send them to the graphics hardware as elements of the
    // uniform 4x4 matrix "control" for use by the vertex shader.
    { LET control = VEC 15
      FOR i = 0 TO 15 DO control!i := 0.0

      control!00 :=  CosRudder    // 0 0
      control!01 :=  SinRudder    // 0 1
      control!02 :=  CosElevator  // 0 2
      control!03 :=  SinElevator  // 0 3
      control!04 :=  CosAileron   // 1 0
      control!05 :=  SinAileron   // 1 1

      // Send the control values to the graphics hardware.
      sys(Sys_gl, GL_UniformMatrix4fv, ControlLoc, glprog, control)
    }

    // Draw a new image
    sys(Sys_gl, GL_ClearColour, 130, 130, 250, 255)
    sys(Sys_gl, GL_ClearBuffer) // Clear colour and depth buffers

    drawmodel()

    sys(Sys_gl, GL_SwapBuffers)
    delay(0_020) // Delay for 1/50 sec
  }

  sys(Sys_gl, GL_DisableVertexAttribArray, VertexLoc)
  sys(Sys_gl, GL_DisableVertexAttribArray, ColorLoc)
  sys(Sys_gl, GL_DisableVertexAttribArray, DataLoc)

  freevec(dvec) // Free the display items vector.
  delay(0_050)
  sys(Sys_gl, GL_Quit)

  RESULTIS 0
}

AND Compileshader(prog, isVshader, filename) = VALOF
{ // Create and compile a shader whose source code is
  // in a given file.
  // isVshader=TRUE  if compiling a vertex shader
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
                (isVshader -> GL_CompileVshader, GL_CompileFshader),
                prog,
                buf)

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

    sys(Sys_gl, GL_DrawElements,
                mode,     // 1=points, 2=lines, 3=linestrip, etc
                size,     // Number of index elements to use.
                2*offset) // The start position (bytes) in the index vector.
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

      // Move the aircraft relative to the centre of the screen,
      // 6 inches each time.
      CASE 'F': cent := cent + 0.5; LOOP  // Foward in direction t
      CASE 'B': cent := cent - 0.5; LOOP  // Backward
      CASE 'L': cenw := cenw + 0.5; LOOP  // To the left in direction w
      CASE 'R': cenw := cenw - 0.5; LOOP  // To the right
      CASE 'U': cenl := cenl + 0.5; LOOP  // Upward indirection l
      CASE 'D': cenl := cenl - 0.5; LOOP  // Downward

      CASE 'P': // Print direction cosines and other data
                newline()
                writef("ct     %9.6f %9.6f %9.6f rtdot=%9.6f*n",
                               ctx,  cty,  ctz,  rtdot)
                writef("cw     %9.6f %9.6f %9.6f rwdot=%9.6f*n",
                               cwx,  cwy,  cwz,  rwdot)
                writef("cl     %9.6f %9.6f %9.6f rldot=%9.6f*n",
                               clx,  cly,  clz,  rldot)
                newline()
                writef("eyepos %9.3f %9.3f %9.3f*n",
                               eyex, eyey, eyez)
                writef("eyedistance = %9.3f*n", eyedistance)
                LOOP

      CASE 'S': stepping := ~stepping
                LOOP

      CASE '0': eyex, eyez :=  0.000,  1.000; LOOP
      CASE '1': eyex, eyez :=  0.707,  0.707; LOOP
      CASE '2': eyex, eyez :=  1.000, -0.000; LOOP
      CASE '3': eyex, eyez :=  0.707, -0.707; LOOP
      CASE '4': eyex, eyez :=  0.000, -1.000; LOOP
      CASE '5': eyex, eyez := -0.707, -0.707; LOOP
      CASE '6': eyex, eyez := -1.000,  0.000; LOOP
      CASE '7': eyex, eyez := -0.707,  0.707; LOOP

      CASE '=':
      CASE '+': eyedistance := eyedistance * 1.1; LOOP

      CASE '_':
      CASE '-': IF eyedistance >= 1.0 DO
                   eyedistance := eyedistance / 1.1
                LOOP

      CASE '>':CASE '.':    rldot := rldot + 0.0005
                            IF rldot> 0.0060 DO rldot :=  0.0060
                            LOOP
      CASE '<':CASE ',':    rldot := rldot - 0.0005
                            IF rldot<-0.0060 DO rldot := -0.0060
                            LOOP

      CASE sdle_arrowdown:  rwdot := rwdot + 0.0005
                            IF rwdot> 0.0060 DO rwdot :=  0.0060
                            LOOP
      CASE sdle_arrowup:    rwdot := rwdot - 0.0005
                            IF rwdot<-0.0060 DO rwdot := -0.0060
                            LOOP

      CASE sdle_arrowleft:  rtdot := rtdot + 0.0005
                            IF rtdot> 0.0060 DO rtdot :=  0.0060
                            LOOP
      CASE sdle_arrowright: rtdot := rtdot - 0.0005
                            IF rtdot<-0.0060 DO rtdot := -0.0060
                            LOOP
    }
    LOOP

  CASE sdle_quit:             // 12
    writef("QUIT*n");
    sys(Sys_gl, GL_Quit)
    LOOP
}

AND rotate(FLT t, FLT w, FLT l) BE
{ // Rotate the orientation of the aircraft
  // t, w and l are assumed to be small and cause
  // rotation about axis t, w, l. Positive values cause
  // anti-clockwise rotations about their axes.

  LET FLT tx =    ctx -  l*cwx + w*clx
  LET FLT wx =  l*ctx +    cwx - t*clx
  LET FLT lx = -w*ctx +  t*cwx +   clx

  LET FLT ty =    cty -  l*cwy + w*cly
  LET FLT wy =  l*cty +    cwy - t*cly
  LET FLT ly = -w*cty +  t*cwy +   cly

  LET FLT tz =    ctz -  l*cwz + w*clz
  LET FLT wz =  l*ctz +    cwz - t*clz
  LET FLT lz = -w*ctz +  t*cwz +   clz

  ctx, cty, ctz := tx, ty, tz
  cwx, cwy, cwz := wx, wy, wz
  clx, cly, clz := lx, ly, lz

  adjustlength(@ctx);      adjustlength(@cwx);      adjustlength(@clx) 
  adjustortho(@ctx, @cwx); adjustortho(@ctx, @clx); adjustortho(@cwx, @clx)
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
{ // m is a 4x4 matrix as a sequence of columns.
  writef(" %8.3f %8.3f %8.3f %8.3f*n", m!0, m!4, m! 8, m!12)
  writef(" %8.3f %8.3f %8.3f %8.3f*n", m!1, m!5, m! 9, m!13)
  writef(" %8.3f %8.3f %8.3f %8.3f*n", m!2, m!6, m!10, m!14)
  writef(" %8.3f %8.3f %8.3f %8.3f*n", m!3, m!7, m!11, m!15)
}

AND prv(v) BE
{ // v is a vector of four elements.
  writef(" %8.3f %8.3f %8.3f %8.3f*n", v!0, v!1, v!2, v!3)
}


