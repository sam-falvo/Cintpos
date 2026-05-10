/*
This program is a simple demonstration of the OpenGL interface.

The BCPL GL library is in g/gl.b with header g/gl.h and is designed to
work unchanged with either OpenGL using SDL or OpenGL ES using EGL.

Implemented by Martin Richards (c) Apr 2020

History

07/10/2019
More systematic changes to the GL interface.
Not yet working.

25/09/2019
Modified to work with 32 and 64 bit BCPL on 32 and 64 bit machines.
 
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

The model is represented using three axes t (the direction of thrust),
w the direction of the left wing and l (the direction of lift,
orthogonal to t and w). These use the right hand convention, ie t is
forward, w is left and l is up.

Real world coordinate use axes x(right), y(up) and z(towards the
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

  // The next six variables must be in consecutive locations
  // since @vvec is passed to loadmodel.
  vvec       // Vector of floating point numbers
             // holding the vertex attributes.
  vvecupb    // The number of numbers in vvec.
  ivec       // Vector of 16-bit unsigned integers
  ivecupb    // The number of values in ivec
  dvec       // The display items vector
  dvecupb    // The number of display items. (3 index values per item).

  model      // Normally points to [vvec, vvecupb, ivec, ivecupb, dvec, dvecupb]
             // It is set to 0 if load model fails.
  
  glprog
  Vshader
  Fshader

  VertexLoc       // Attribute variable locations
  ColorLoc
  DataLoc         // data[0]=ctrl  data[1]=value

  MatrixLoc       // Uniform variable locations
  LandMatrixLoc
  ControlLoc

  FLT CosElevator
  FLT SinElevator
  FLT CosRudder
  FLT SinRudder
  FLT CosAileron
  FLT SinAileron

  modelfilename // Holds the name of the model file, typically gltst.mdl

  FLT ctx; FLT cty; FLT ctz   // Direction cosines of direction t
  FLT cwx; FLT cwy; FLT cwz   // Direction cosines of direction w
  FLT clx; FLT cly; FLT clz   // Direction cosines of direction l

  FLT rtdot; FLT rwdot; FLT rldot    // Anti-clockwise rotation rates
                         // about the t, w and l axes
 
  FLT eyex; FLT eyey; FLT eyez // Coordinates of a point on the
                               // line of sight from to eye to
                               // the origin (0.0, 0.0, 0.0).
  FLT eyedistance              // The distance between the eye
                               // and the origin.

  FLT cent; FLT cenw; FLT cenl 

  VertexBuffer// To hold all the vertex data we ever need.
  IndexBuffer // To hold all the index data we ever need.

  projectionMatrix // is the matrix used by the vertex shader
                   // to transform the vertex coordinates to
                   // screen coordinates.
  workMatrix       // is used when constructing the projection matrix.
}

LET concatstr(s1, s2, s3) = VALOF
{ LET len1, len2 = s1%0, s2%0
  LET pos = 0
  FOR i = 1 TO len1 DO { pos := pos+1; s3%pos := s1%i }
  FOR i = 1 TO len2 DO { pos := pos+1; s3%pos := s2%i }
  s3%0 := pos
  RESULTIS s3
}

LET start() = VALOF
{ LET m1 = VEC 15  // For the projectionmatrix
  LET m2 = VEC 15  // For the workmatrix
  LET argv = VEC 50
  LET modelfilename = "gltiger.mdl"
  LET str = VEC 20 // For the window title

  projectionMatrix, workMatrix := m1, m2

  UNLESS rdargs("-d/s,-a/s", argv, 50) DO
  { writef("Bad arguments for gltst*n")
    RETURN
  }

  debug := argv!0                             // -d/s
  IF argv!1 DO                                // -a/s
    modelfilename := "gltst.mdl"

  UNLESS glInit() DO
  { writef("*nOpenGL not available*n")
    RESULTIS 0
  }

  writef("start: calling glMkScreen*n")
  // Create an OpenGL window
  screenxsize := glMkScreen(concatstr("OpenGL First Test:  ",
                                       modelfilename, str),
                            800, 680)
  screenysize := result2
  UNLESS screenxsize DO
  { writef("*nUnable to create an OpenGL window*n")
    RESULTIS 0
  }
  writef("Screen Size is %n x %n*n", screenxsize, screenysize)
  glprog := sys(Sys_gl, gl_MkProg)
  writef("gltst: glprog=%n*n", glprog);

  IF glprog<0 DO
  { writef("*nUnable to create a GL program*n")
    RESULTIS 0
  }

  // Read and Compile the vertex shader
  writef("glstst: calling Compileshader(%n,TRUE,*"gltstVshader.sdr*") *n",
          glprog)
  Vshader := Compileshader(glprog, TRUE, "gltstVshader.sdr")
  writef("gltst: Vshader=%n*n", Vshader)

// Read and Compile the fragment shader
  writef("gltst: calling Compileshader(%n,FALSE,gltstFshader.sdr) *n",glprog)
  Fshader := Compileshader(glprog, FALSE, "gltstFshader.sdr")
  writef("gltst: Fshader=%n*n", Fshader)
//abort(8344)

  // Link the program
  writef("gltst: calling glLinkProg(%n)*n", glprog)
  UNLESS sys(Sys_gl, gl_LinkProgram, glprog)=-1 DO
  { writef("*ngltst: Unable to link a GL program*n")
    RESULTIS 0
  }

  writef("start: calling glUseProgram(%n)*n", glprog)
  sys(Sys_gl, gl_UseProgram, glprog)

  // Get attribute locations after linking
  VertexLoc := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vVertex")
  ColorLoc  := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vColor")
  DataLoc   := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vData")

  writef("VertexLoc=%n*n", VertexLoc)
  writef("ColorLoc=%n*n",  ColorLoc)
  writef("DataLoc=%n*n",   DataLoc)

  // Get uniform locations after linking
  MatrixLoc      := sys(Sys_gl, gl_GetUniformLocation, glprog, "matrix")
  ControlLoc     := sys(Sys_gl, gl_GetUniformLocation, glprog, "control")

  writef("gltst: MatrixLoc=%n*n",  MatrixLoc)
  writef("gltst: ControlLoc=%n*n", ControlLoc)

  // Load model
  writef("gltst: Calling loadmodel file=%s*n", modelfilename)
  model := loadmodel(modelfilename, @vvec)
  UNLESS model DO
  { writef("*ngltst: Unable to load model: %s*n", modelfilename)
    RESULTIS 0
  }

  // If loadmodel is successful model is set to to points to the 6
  // consecutive global variables in the global variables vvec, vvecupb,
  // ivec, ivecupb, dvec and dvecupb.
  // If loadmodel fails it returns zero.
  // The vertex data is in vvec!0 to vvec!vvecupb
  // The index data is in ivec!0 to ivec!ivecupb
  // The display data is in dvec!0 to dvec!dvecupb
  
  IF debug DO
  { // Output the vertex and index data as a debugging aid.
    // #### This currently only works on 32-bit BCPL. ####
    writef("*nVertex data*n")  // This vector holds 32-bit floats
    FOR i = 0 TO vvecupb DO
    { IF i MOD 8 = 0 DO writef("*n%i3: ", i/8)
      writef(" %5.2f", vvec!i)
    }
    writef("*n*nIndex data*n") // This vector hold 16-bit integers
    FOR i = 0 TO ivecupb DO
    { IF i MOD 10 = 0 DO writef("*n%i6: ", i)
      writef(" %i3", ivec!i)
    }
    writef("*n*nDisplay data items*n") // This holds 32-bit values
    FOR i = 0 TO dvecupb BY 3 DO
      writef(" %i3  %i3  %i3*n", dvec!i, dvec!(i+1), dvec!(i+2))
    newline()
  }
//abort(5555)

  sys(Sys_gl, gl_Enable, GL_DEPTH_TEST) // This call is neccessary
  // GL_LESS is the default relation.
//  sys(Sys_gl, gl_DepthFunc, GL_LESS)    // This is the default

  // Positive Z is into the screen, so a pixel is written with
  // depth < buffer depth takes precedence. ????
  // Remember that the depth test is performed after all other
  // transformations have been done.


  writef("start: Call gl_GenVertexBuffer to create the GL Vertex Buffer*n")
  writef("       and copy its data from user to GL space*n")
  VertexBuffer := sys(Sys_gl, gl_GenVertexBuffer, vvecupb+1, vvec)
  // VertexBuffer is the name (integer >0) of the vertex buffer.
  writef("start: The id of the GL VertexBuffer is %n*n", VertexBuffer)
//abort(1001)

  // Tell GL the positions in vvec of the xyz fields,
  // ie 3 words from position 0 of each 8 word item in vvec
  writef("start: Calling gl_EnableVertexAttribArray for xyz data*n")
  sys(Sys_gl, gl_EnableVertexAttribArray, VertexLoc);
  writef("start: Calling gl_VertexData to copy xyz data to GL space*n")
  sys(Sys_gl, gl_VertexData,
              VertexLoc,     // Attribute number for xyz data
              3,             // 3 floats for xyz
              8,             // 8 floats per vertex item in vertexData
              0)             // Offset in 32 bit words of the xyz data

  // Tell GL the positions in vvec of the rgb fields,
  // ie 3 words from position 3 of each 8 word item in vvec
  writef("start: Calling gl_EnableVertexAttribArray for rgb data*n")
  sys(Sys_gl, gl_EnableVertexAttribArray, ColorLoc);
  writef("start: Calling gl_VertexData to copy rgb data to GL space*n")
  sys(Sys_gl, gl_VertexData,
              ColorLoc,      // Attribute number rgb data
              3,             // 3 floats for rgb data
              8,             // 8 floats per vertex item in vertexData
              3)             // Offset in words of the rgb data

  // Tell GL the positions in vvec of the kd fields,
  // ie 2 words from osition 6 of each 8 word item in vvec
  writef("start: Calling gl_EnableVertexAttribArray for kd data*n")
  sys(Sys_gl, gl_EnableVertexAttribArray, DataLoc);
  writef("start: Calling gl_VertexData to copy kd data to GL space*n")
  sys(Sys_gl, gl_VertexData,
              DataLoc,       // Attribute number kd data
              2,             // 2 floats for kd data
              8,             // 8 floats per vertex item in vertexData
              6)             // Offset in words of the kd data

  freevec(vvec) // Free vvec since all its elements have
                // been sent to the graphics server.
  vvec := 0

  writef("start: ivecupb=%n*n", ivecupb)
  writef("start: Calling GenIndexBuffer    ivec=%n ivecupb+1=%n*n",
          ivec, ivecupb+1)
  IndexBuffer  := sys(Sys_gl, gl_GenIndexBuffer, ivecupb+1, ivec)

  writef("start: IndexData copied to graphics memory object %n*n", IndexBuffer)

  freevec(ivec) // Free ivec since all its elements have
                // been sent to the graphics server.
  ivec := 0
//abort(9221)
  // Initialise the state

  done     := FALSE
  stepping := TRUE

  // Set the initial direction cosines to orient t, w and l in
  // directions -z, -x and y, ie viewing the aircraft from behind.

  ctx, cty, ctz :=   0.0,  0.0, -1.0
  cwx, cwy, cwz :=  -1.0,  0.0,  0.0
  clx, cly, clz :=   0.0,  1.0,  0.0

  rtdot, rwdot, rldot := 0.0000, 0.0001, 0.0001 // Rotate the model slowly

  cent, cenw, cenl := 0.0, 0.0, 0.0 // position in the aircraft to
                                    // place at the centre of the screen.

  eyex, eyey, eyez := 0.0, 0.0, 1.0

  eyedistance := 80.000

  IF FALSE & debug DO
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
    sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)
    writef("gives*n")
    prmat(projectionMatrix)
    //abort(1000)
  }

  UNTIL done DO
  { processevents()
    // Only rotate the object if not stepping
    IF stepping DO
    { // If not stepping adjust the orientation of the model.
      rotate(rtdot, rwdot, rldot)
    }

    // Move the model forward, left and up by specified amounts
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

    sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)

//writef("ctx=%9.6f, cty=%9.6f, ctz=%9.6f*n", ctx, cty, ctz)

    // Rotate the model and eye until the eye is on the z axis
//eyex, eyey, eyez := 0.0, 0.0, -50.0
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

      sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)

      // Rotate clockwise about X axis by angle phi
      setvec( workMatrix, 16,
                1.0,     0.0,       0.0, 0.0,    // column 1
                0.0, cos_phi,  -sin_phi, 0.0,    // column 2
                0.0, sin_phi,   cos_phi, 0.0,    // column 3
                0.0,     0.0,       0.0, 1.0)    // column 4

      sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)

      // Change the origin to the eye position on the z axis by
      // moving the model eyedistance in the negative z direction.
      setvec( workMatrix, 16,
                1.0, 0.0,           0.0, 0.0, // column 1
                0.0, 1.0,           0.0, 0.0, // column 2
                0.0, 0.0,           1.0, 0.0, // column 3
                0.0, 0.0,  -eyedistance, 1.0  // column 4
              )

      sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)
    }

    { // Define the truncated pyramid for the view projection
      // using the frustrum transformation.
      LET FLT aspect = FLOAT screenxsize / FLOAT screenysize
/*
      LET FLT n, FLT f = 0.1, 5000.0
      LET FLT fan, FLT fsn = f+n, f-n
      LET FLT n2 = 2.0*n
      LET FLT l,   FLT r   = -0.5, 0.5
      LET FLT ral, FLT rsl =  r+l, r-l
      LET FLT b,   FLT t   = -0.5, 0.5 
      LET FLT tab, FLT tsb =  t+b, t-b

      LET FLT fv = 2.0 / 0.5  // Half field of view at unit distance
      setvec( workMatrix, 16,
             fv/aspect,  0.0,             0.0,  0.0, // column 1
                   0.0,   fv,             0.0,  0.0, // column 2
                   0.0,  0.0,     (f+n)/(n-f), -1.0, // column 3
                   0.0,  0.0, (2.0*f*n)/(n-f),  0.0  // column 4
            )
writef("Perspective matrix by hand*n")
prmat(workMatrix)
*/
      // This perspective matrix could be set more conveniently using
      // glSetPerspective library function defined in g/gl.b
      
      glSetPerspective(workMatrix,
                              0.5, // Field of view = the distance from
			           // the centre of thr window to the top
				   // at umit distance.
                           aspect, // Aspect ratio = width/heght
                              0.1, // Distance to near limit
                           5000.0) // Distance to far limit

//writef("Perspective matrix created by glSetPerspective*n")
//prmat(workMatrix)
//abort(1000)
      sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)
//writef("Perspective matrix created by glSetPerspective*n")
//prmat(workMatrix)
//writef("Resulting matrix sent to GL*n")
//prmat(projectionMatrix)
//abort(1000)
    }


    // Send the resulting matrix to the uniform variable "matrix" for
    // use by the vertex shader.
    sys(Sys_gl, gl_UniformMatrix4fv, MatrixLoc, glprog, projectionMatrix)


    // Calculate the cosines and sines of the control surfaces.
    { LET FLT RudderAngle = - rldot *5.0* 75.0
      CosRudder := sys(Sys_flt, fl_cos, RudderAngle)
      SinRudder := sys(Sys_flt, fl_sin, RudderAngle)
    }

    { LET FLT ElevatorAngle = rwdot * 500.0
      CosElevator := sys(Sys_flt, fl_cos, ElevatorAngle)
      SinElevator := sys(Sys_flt, fl_sin, ElevatorAngle)
    }

    { LET AileronAngle = rtdot * 500.0
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
      //FOR i = 6 TO 15 DO control!i := 0.0

      // Send the control values to the graphics hardware
      sys(Sys_gl, gl_UniformMatrix4fv, ControlLoc, glprog, control)
    }


    // Draw a new image
    // Clear colour and depth buffers
    sys(Sys_gl, gl_Clear, GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    sys(Sys_gl, gl_ClearColour, 0.705, 0.705, 0.980, 1.0)

    drawmodel()

    sys(Sys_gl, gl_SwapBuffers)
    delay(0_020) // Delay for 1/50 sec
//abort(1003)
  }

  sys(Sys_gl, gl_DisableVertexAttribArray, VertexLoc)
  sys(Sys_gl, gl_DisableVertexAttribArray, ColorLoc)
  sys(Sys_gl, gl_DisableVertexAttribArray, DataLoc)

  freevec(dvec) // Free the display items vector.
  //delay(0_050)
  sys(Sys_gl, gl_Quit)

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
                (isVshader -> gl_CompileVshader, gl_CompileFshader),
                prog,
                buf)

  endstream(ramstream)
  RESULTIS shader
}

AND drawmodel() BE
{ // Draw the primitives using vertex and index data held in
  // graphics objects as specified by the display items in dvec.
  FOR p = 0 TO dvecupb BY 3 DO
  { LET d = @dvec!(p)
    LET mode   = d!0  // Points, Lines, Linestrip, etc.
    LET size   = d!1  // Number of index elements.
    LET offset = d!2  // Offset in the index vector.

//writef("drawmodel: p=%n mode=%n, size=%n offset=%n*n", p, mode, size, offset)
    sys(Sys_gl, gl_DrawElements,
                mode,     // eg GL_TRIANGLES
                size,     // Number of index elements to use.
                offset)   // The start position (bytes) in the index vector.
//abort(6475)
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

      CASE '>':CASE '.':    rldot := rldot + 0.0001
                            IF rldot> 0.0060 DO rldot :=  0.0060
                            LOOP
      CASE '<':CASE ',':    rldot := rldot - 0.0001
                            IF rldot<-0.0060 DO rldot := -0.0060
                            LOOP

      CASE sdle_arrowdown:  rwdot := rwdot + 0.0001
                            IF rwdot> 0.0060 DO rwdot :=  0.0060
                            LOOP
      CASE sdle_arrowup:    rwdot := rwdot - 0.0001
                            IF rwdot<-0.0060 DO rwdot := -0.0060
                            LOOP

      CASE sdle_arrowleft:  rtdot := rtdot + 0.0001
                            IF rtdot> 0.0060 DO rtdot :=  0.0060
                            LOOP
      CASE sdle_arrowright: rtdot := rtdot - 0.0001
                            IF rtdot<-0.0060 DO rtdot := -0.0060
                            LOOP
    }
    LOOP

  CASE sdle_quit:             // 12
    writef("QUIT*n");
    sys(Sys_gl, gl_Quit)
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
  writef(" %12.7f %12.7f %12.7f %12.7f*n", m!0, m!4, m! 8, m!12)
  writef(" %12.7f %12.7f %12.7f %12.7f*n", m!1, m!5, m! 9, m!13)
  writef(" %12.7f %12.7f %12.7f %12.7f*n", m!2, m!6, m!10, m!14)
  writef(" %12.7f %12.7f %12.7f %12.7f*n", m!3, m!7, m!11, m!15)
}

AND prv(v) BE
{ // v is a vector of four elements.
  writef(" %12.7f %12.7f %12.7f %12.7f*n", v!0, v!1, v!2, v!3)
}


