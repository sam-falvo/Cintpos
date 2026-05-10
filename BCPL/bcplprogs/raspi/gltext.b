/*
This program is a simple demonstration how to do 2D SDL style drawing
including writing text when using OpenGL.

The BCPL GL library is in g/gl.b with header g/gl.h and is designed to
work unchanged with either OpenGL using SDL or OpenGL ES using EGL. It
will be upgraded to work with all versions of OpenGL including
versions 3.1 and 4.5. On some sytems this program uses the SDL library
to create the OpenGL context. The intent is that it will work with
either SDL1.2 and SDL2.

Implemented by Martin Richards (c) Feb 2021

This program is basically gltst.b but includes a plane of RGBA pixels
filling the entire window at the front. These pixels use the A
component to control whether they are transparent or not. The pixels
in this plane are held a texture buffer.

This pixel plane will be used to display the positions of the controls
and some text indicating the values of certain variables. Since copying
the texture data from user to GL space can be expensive this will perhaps
only be done once every 10 or so frames.

History

16/02/2021
Started then initial implementation.


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
  vvecupb    // The number of values in vvec.
  ivec       // Vector of 32-bit unsigned integers
  ivecupb    // The number of values in ivec
  dvec       // The display items vector
  dvecupb    // The number of display items. (3 values per item).

  model      // Normally points to [vvec, vvecupb, ivec, ivecupb, dvec, dvecupb]
             // It is set to 0 if load model fails.

  pxlxsize   // Variables for the pixel texture
  pxlysize
  pxlv
  
  glprog
  Vshader
  Fshader

  VertexLoc       // Attribute variable locations
  ColorLoc
  DataLoc         // data[0]=ctrl  data[1]=value

  MatrixLoc       // Uniform variable locations
  ControlLoc

  FLT CosElevator
  FLT SinElevator
  FLT CosRudder
  FLT SinRudder
  FLT CosAileron
  FLT SinAileron

  modelfilename // Holds the name of the model file, typically gltext.mdl

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

  VertexBuffer// To hold the vertex data for the model.
  IndexBuffer // To hold the index data for the model.

  projectionMatrix // is the matrix used by the vertex shader
                   // to transform the vertex coordinates of
		   // the model to screen coordinates.
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
{ LET rc = 0
  LET m1 = VEC 15  // For the 4x4 projectionmatrix
  LET m2 = VEC 15  // For the 4x4 workmatrix
  LET argv = VEC 50
  LET modelfilename = "gltiger.mdl"
  LET str = VEC 20 // For the window title

  projectionMatrix, workMatrix := m1, m2
  pxlv := 0
  
  UNLESS rdargs("-d/s,-a/s", argv, 50) DO
  { writef("Bad arguments for gltext*n")
    RETURN
  }

  debug := argv!0                             // -d/s
  
  IF argv!1  DO                               // -a/s
    modelfilename := "gltext.mdl"

  rc := glInit()
  //writef("glInit return %n*n", rc)
  
  UNLESS rc DO
  { writef("*nOpenGL not available*n")
    RESULTIS 0
  }

  //writef("start: calling glMkScreen*n")
  // Create an OpenGL window
  screenxsize := glMkScreen(concatstr("OpenGL First Test:  ",
                                       modelfilename, str),
                            1024, 512)
  screenysize := result2
  UNLESS screenxsize DO
  { writef("*nUnable to create an OpenGL window*n")
    RESULTIS 0
  }
  //writef("Screen Size is %n x %n*n", screenxsize, screenysize)
  glprog := sys(Sys_gl, gl_MkProg)
  //writef("gltext: glprog=%n*n", glprog);

  IF glprog<0 DO
  { writef("*nUnable to create a GL program*n")
    RESULTIS 0
  }

  // Read and Compile the vertex shader
  //writef("glstst: calling Compileshader(%n,TRUE,*"gltextVshader.sdr*") *n",
  //        glprog)
  Vshader := Compileshader(glprog, TRUE, "gltextVshader.sdr")
  //writef("gltext: Vshader=%n*n", Vshader)

// Read and Compile the fragment shader
  //writef("gltext: calling Compileshader(%n,FALSE,gltextFshader.sdr) *n",glprog)
  Fshader := Compileshader(glprog, FALSE, "gltextFshader.sdr")
  //writef("gltext: Fshader=%n*n", Fshader)
//abort(8344)

  // Link the program
  //writef("gltext: calling glLinkProg(%n)*n", glprog)
  UNLESS sys(Sys_gl, gl_LinkProgram, glprog)=-1 DO
  { writef("*ngltext: Unable to link a GL program*n")
    RESULTIS 0
  }

  //writef("start: calling glUseProgram(%n)*n", glprog)
  sys(Sys_gl, gl_UseProgram, glprog)

  // Get attribute locations after linking
  VertexLoc := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vVertex")
  ColorLoc  := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vColor")
  DataLoc   := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vData")

  //writef("VertexLoc=%n*n", VertexLoc)
  //writef("ColorLoc=%n*n",  ColorLoc)
  //writef("DataLoc=%n*n",   DataLoc)

  // Get uniform locations after linking
  MatrixLoc      := sys(Sys_gl, gl_GetUniformLocation, glprog, "matrix")
  ControlLoc     := sys(Sys_gl, gl_GetUniformLocation, glprog, "control")

  //writef("gltext: MatrixLoc=%n*n",  MatrixLoc)
  //writef("gltext: ControlLoc=%n*n", ControlLoc)

  // Load model
  //writef("gltext: Calling loadmodel file=%s*n", modelfilename)
  model := loadmodel(modelfilename, @vvec)
  // Note that vvec, vvecupb, ivec, ivecupb dvec and dvecupn are
  // consecutive global variables, and @vvec points to the first of them.
  
  UNLESS model DO
  { writef("*ngltext: Unable to load model: %s*n", modelfilename)
    RESULTIS 0
  }

  // If loadmodel fails it returns zero.
  // If this call of loadmodel is successful, the 6 global variables
  // vvec, vvecupb, ivec, ivecupb, dvec and dvecupb will be set.
  // The vertex data  is in vvec!0 to vvec!vvecupb
  // The index data   is in ivec!0 to ivec!ivecupb
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
  sys(Sys_gl, gl_DepthFunc, GL_LESS)    // This is the default

  // Positive Z is into the screen, so a pixel is written with
  // depth < buffer depth takes precedence. ????
  // Remember that the depth test is performed after all other
  // transformations have been done.


  //writef("start: Call gl_GenVertexBuffer to create the GL Vertex Buffer*n")
  //writef("       and copy its data from user to GL space*n")
  VertexBuffer := sys(Sys_gl, gl_GenVertexBuffer, vvecupb+1, vvec)
  // VertexBuffer is the name (integer >0) of the vertex buffer.
  //writef("start: The id of the GL VertexBuffer is %n*n", VertexBuffer)
//abort(1001)

  // Tell GL the positions in vvec of the xyz fields,
  // ie 3 words from position 0 of each 8 word item in vvec
  //writef("start: Calling gl_EnableVertexAttribArray for xyz data*n")
  sys(Sys_gl, gl_EnableVertexAttribArray, VertexLoc);
  //writef("start: Calling gl_VertexData to copy xyz data to GL space*n")
  sys(Sys_gl, gl_VertexData,
              VertexLoc,     // Attribute number for xyz data
              3,             // 3 floats for xyz
              8,             // 8 floats per vertex item in vertexData
              0)             // Offset in 32 bit words of the xyz data

  // Tell GL the positions in vvec of the rgb fields,
  // ie 3 words from position 3 of each 8 word item in vvec
  //writef("start: Calling gl_EnableVertexAttribArray for rgb data*n")
  sys(Sys_gl, gl_EnableVertexAttribArray, ColorLoc);
  //writef("start: Calling gl_VertexData to copy rgb data to GL space*n")
  sys(Sys_gl, gl_VertexData,
              ColorLoc,      // Attribute number rgb data
              3,             // 3 floats for rgb data
              8,             // 8 floats per vertex item in vertexData
              3)             // Offset in words of the rgb data

  // Tell GL the positions in vvec of the kd fields,
  // ie 2 words from osition 6 of each 8 word item in vvec
  //writef("start: Calling gl_EnableVertexAttribArray for kd data*n")
  sys(Sys_gl, gl_EnableVertexAttribArray, DataLoc);
  //writef("start: Calling gl_VertexData to copy kd data to GL space*n")
  sys(Sys_gl, gl_VertexData,
              DataLoc,       // Attribute number kd data
              2,             // 2 floats for kd data
              8,             // 8 floats per vertex item in vertexData
              6)             // Offset in words of the kd data

  freevec(vvec) // Free vvec since all its elements have
                // been sent to the graphics server.
  vvec := 0

  //writef("start: ivecupb=%n*n", ivecupb)
  //writef("start: Calling GenIndexBuffer    ivec=%n ivecupb+1=%n*n",
  //        ivec, ivecupb+1)
  IndexBuffer  := sys(Sys_gl, gl_GenIndexBuffer, ivecupb+1, ivec)

  //writef("start: IndexData copied to graphics memory object %n*n", IndexBuffer)

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

  // Choose the size of the pixel texture
  pxlxsize, pxlysize := 4096, 4096
  //writef("pxlxsize=%n pxlysize=%n screenxsize=%n screenysize=%n*n",
  //        pxlxsize,   pxlysize,   screenxsize,   screenysize)

  UNTIL pxlxsize <= screenxsize DO pxlxsize := pxlxsize / 2
  UNTIL pxlysize <= screenysize DO pxlysize := pxlysize / 2
  //writef("pxlxsize=%n pxlysize=%n*n", pxlxsize, pxlysize)
  pxlv := getvec(pxlxsize*pxlysize-1)
  UNLESS pxlv DO
  { writef("Unable to allocate pxlv*n")
    GOTO ret
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
      FOR i = 6 TO 15 DO control!i := 0.0

      // Send the control values (ie matrix) to the graphics hardware
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

ret:
  IF pxlv DO freevec(pxlv)
  
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
                offset)   // The start subscript in the index vector.
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


//############ Code to draw on the pixel plane. ##################

/********************
AND draw_artificial_horizon() BE
{ // This function draws the artificial horizon.
  // The n and w components of the direction of thrust t are used
  // to make a horizontal vector (n,w,0) which is above or below
  // the direction of thrust. This is then scaled to make it of
  // unit length. Suppose the resulting vector is d = (dn,dw,0).
  // Let P be a point in direction d 100 ft from the aircraft's CG,
  // ie (100dn, 100dw,0).  This point will be above or below the
  // line in direction t from the CG.
  // P (cgn+100dn,cgw+100dw,cgh) is in world coordinates.
  // The artificial horizon is made up of four line segments
  // A-B, B-C, C-D and D-E where A, B, D and E are on
  // the horizontal line passing through P at right angles to d.
  // A is 30ft to the left of P and E is 30ft to the right of P.
  // On the screen, B, C and D form an equilateral triangle half
  // way between A and E.

  // The direction of motion is represented by a small circle at
  // point X which has coordinates (cgn+100xn,cgw+100xw,cgh+100xh)
  // where (xn,xw,xh) is a unit vector in direction
  // (cgndot,cgwdot,cghdot). The screen position of X is calculated
  // using the same orthogonal projection as the points A, B, C, D
  // and E.


  LET px, py = ?, ?  // For screen coordinates
  LET ax, ay = ?, ?  // For screen coordinates
  LET bx, by = ?, ?  // For screen coordinates
  LET cx, cy = ?, ?  // For screen coordinates
  LET dx, dy = ?, ?  // For screen coordinates
  LET ex, ey = ?, ?  // For screen coordinates
  LET FLT n,  FLT w,  FLT h  =  ctn, ctw, 0.0  // A horizontal vector
  LET FLT a,  FLT b,  FLT c  =    ?,   ?,   ?  // Unit vector orthogonal to (n,w,h)
  LET FLT Pn, FLT Pw, FLT Ph =    ?,   ?,   ?
  LET FLT An, FLT Aw, FLT Ah =    ?,   ?,   ?
  LET FLT En, FLT Ew, FLT Eh =    ?,   ?,   ?
  LET FLT Xn, FLT Xw, FLT Xh =    ?,   ?,   ?  // A point in direction
                                               // (cgndot,cgwdot,cghdot).

  setcolour(col_white)

  //{ moveto(100,200)
  //  drawto(110,210)
  //}
//updatescreen()
//abort(1002)


  adjustlength(@n)  // Make (n,w,0) a unit vector, direction d.

  // Make a unit vector in direction A->E (orthogonal to d).
  a, b, c := w, -n, 0.0

  // Set P to be 100ft from CG in direction d
  Pn, Pw, Ph := cgn+100*n,  cgw+100*w,  cgh // A point on the horizon
                                            // 100ft from CG.
  // Set A 30ft left of from P.
  An, Aw, Ah := Pn-30*a, Pw-30*b, Ph
  // Set A 30ft left of from P.
  En, Ew, Eh := Pn+30*a, Pw+30*b, Ph

  //    A-----------B  P  D----------E
  //                 \   /
  //                   C
  //
  // AE is othogonal to the line from CG to P.
  // 

  orthocoords(An-cgn, Aw-cgw, Ah-cgh, @ax)
  orthocoords(En-cgn, Ew-cgw, Eh-cgh, @ex)
  px, py := (ax+ex)/2, (ay+ey)/2
  bx, by := px + (ax-ex)*5/60, py + (ay-ey)*5/60
  dx, dy := px - (ax-ex)*5/60, py - (ay-ey)*5/60
  // BCD is an equilateral triangle with sides of length 10,
  // CP has length appoximately 8.66.
  // (ey-ay, ax-ay) is a vector of length 60 in direction PC
  // so the screen coordinates of C can be calculated as follows.
  cx, cy := px+(ey-ay)*8_66/60_00, py+(ax-ex)*8_66/60_00
  // We can now draw the artificial horizon
  moveto(ax,ay)
  drawto(bx,by)
  drawto(cx,cy)
  drawto(dx,dy)
  drawto(ex,ey)

  // Set (n,w,h) to be a point in direction (cgndot,cgwdot,cghdot).
  n, w, h :=  cgndot, cgwdot, cghdot
  // Make (n,w,h) a unit vector
  adjustlength(@n)
  // X is the centre of the direction of motion circle.
  Xn, Xw, Xh := cgn+100*n, cgw+100*w, cgh+100*h

//drawf(20, 85, "Xn=%i6 Xn=%i6 Dn=%i6", FIX (Xn-cgn), FIX (Xw-cgw), FIX (Xh-cgh))
  IF orthocoords(Xn-cgn, Xw-cgw, Xh-cgh, @px) DO
  { drawcircle(px, py, 5)
//writef("Draw circle at %n %n*n", px,py)
  }
//updatescreen()
//abort(1001)
}

AND drawcontrols() BE
{ LET mx = screenxsize/2
  LET my = screenysize - 70 //- 100

  seteyeposition()

  fillsurf(col_blue)

  setcolour(col_lightcyan)
  
  drawstr(240, 50, done -> "Quitting", "Tiger Moth Flight Simulator")

  setcolour(col_lightgray) // Draw runway line
  moveto(mx-1, my)
  drawby(0, FIX(3000.0/100.0))
  moveto(mx,   my)
  drawby(0, FIX(3000.0/100.0))
  moveto(mx+1, my)
  drawby(0, FIX(3000.0/100.0))

  { LET dx =    FIX(ctn*20)  // Orientation of the aircraft
    LET dy =    FIX(ctw*20)
    LET sdx =   dx / 10      // Ground speed of the aircraft
    LET sdy =   dy / 10
    LET x  = mx-FIX(cgw/100)
    LET y  = my+FIX(cgn/100)
    LET tx  = x+5*dy/8
    LET ty  = y-5*dx/8
    setcolour(col_red)       // Draw aircraft symbol
    moveto(x-dy/4, y+dx/4)   // Fuselage
    drawby(+dy, -dx)
    moveto( x-dx/2,  y-dy/2) // Wings
    drawby(+dx, +dy)
    moveto(tx, ty)           // Tail
    moveby(dx/4, dy/4)
    drawby(-dx/2, -dy/2)
  }

  // Draw the controls
  setcolour(col_darkgray)
  drawfillrect(screenxsize-20-100, screenysize-20-100, // Joystick
               screenxsize-20,     screenysize-20)
  drawfillrect(screenxsize-50-100, screenysize-20-100, // Throttle
               screenxsize-30-100, screenysize-20)
  drawfillrect(screenxsize-20-100, screenysize-50-100, // Rudder
               screenxsize-20,     screenysize-30-100)

  IF crashed DO
  { setcolour(col_red)
    drawf(mx-50, my+50, "CRASHED")
  }

  setcolour(col_green)     // Real world velocity
  moveto(mx, my)
  drawby(-FIX(cgwdot/10), FIX(cgndot/10))

  { LET pos = FIX(80 * throttle)
    setcolour(col_red)
    drawfillrect(screenxsize-45-100, pos+screenysize-15-100,
                 screenxsize-35-100, pos+screenysize- 5-100)
  }

  { LET pos = FIX(45 * rudder)
    setcolour(col_red)
    drawfillrect(pos+screenxsize-25-50, -5+screenysize-40-100,
                 pos+screenxsize-15-50, +5+screenysize-40-100)
  }

  { LET posx = FIX(45 * aileron)
    LET posy = FIX(45 * elevator)
    setcolour(col_red)
    drawfillrect(posx+screenxsize-25-50, posy+screenysize-25-50,
                 posx+screenxsize-15-50, posy+screenysize-15-50)
  }

  setcolour(col_white)

  IF debugging DO
  { 
    drawf(20, my+ 15, "rpm=%6.1f target rpm=%6.1f thrust=%8.3f",
                       rpm, targetrpm, thrust)
    drawf(20, my,     "Throttle=%6.3f Elevator=%6.3f Aileron=%6.3f Rudder=%6.3f",
                       throttle,      elevator,      aileron,      rudder)
    drawf(20, my- 15, "cgn=    %13.3f cgw=   %13.3f cgh=   %13.3f", cgn,   cgw,   cgh)
    drawf(20, my- 30, "cgndot= %13.3f cgwdot=%13.3f cghdot=%13.3f", cgndot,cgwdot,cghdot)
    drawf(20, my- 45, "tdot=   %13.3f wdot=  %13.3f ldot=  %13.3f", tdot,  wdot,  ldot)
    drawf(20, my- 60, "ctn= %7.3f ctw= %7.3f cth= %7.3f", ctn,   ctw,   cth)
    drawf(20, my- 75, "cwn= %7.3f cww= %7.3f cwh= %7.3f", cwn,   cww,   cwh)
    drawf(20, my- 90, "cln= %7.3f clw= %7.3f clh= %7.3f", cln,   clw,   clh)
    drawf(20, my-105, "ft=     %13.3f fw=    %13.3f fl=    %13.3f", ft,    fw,    fl)
    drawf(20, my-120, "rft=    %13.3f rfw=   %13.3f rfl=   %13.3f", rft,   rfw,   rfl)
    drawf(20, my-135, "rtdot=  %13.3f rwdot= %13.3f rldot= %13.3f", rtdot, rwdot, rldot)
    drawf(20, my-150, "steprate=%8.3f", steprate)

    drawf(20, 130, "tdot=%13.3f  ldot=%13.3f => angle=%6.1f airspeed=%13.3f",
                    tdot, ldot, atl, radius2(tdot, ldot))
    drawf(20, 115, "atl=%6.1f rdtab(atl,lifttab)=%9.3f", atl, rdtab(atl,lifttab))

  }

  IF plotusage DO
  { drawf(20, 20, "CPU usage = %3i%%", usage)
  }

  { LET heading = - FIX (angle(ctn,ctw))
    IF heading < 0 DO heading := 360 + heading
    drawf(20, 5, "      RPM %i4  Speed %3i mph  Altiude %i5 ft  Heading %i3",
          FIX rpm, FIX (tdot/mph2fps), FIX cgh, heading)
  }
//updatescreen()
}

AND plotscreen() BE
{ LET mx = screenxsize /  2
  LET my = screenysize - 70

  fillsurf(col_lightblue)

  setcolour(col_lightcyan)
  setcolour(col_red)
  //abort(1999)
  drawstr(240, 50, done -> "Quitting", "Tiger Moth Flight Simulator")
  updatescreen()
//delay(1000)

  drawcontrols()

  setcolour(col_gray)
  moveto(mx, my)
  drawby(0, FIX(cgh/100))

  setcolour(col_majenta)
  moveto(mx+200, my)
  drawby(FIX(ctn * 20.0), FIX(ctw * 20.0))

  //draw_artificial_horizon()

  //drawgroundpoints()

  IF eyedir DO plotcraft()
  updatescreen()
//abort(1090)

}

AND seteyeposition1() BE
{ cetn, cetw, ceth :=  One, Zro, Zro
  cewn, ceww, cewh :=  Zro, One, Zro
  celn, celw, celh :=  Zro, Zro, One
  eyen, eyew, eyeh :=  -eyedist,   Zro, Zro   // Relative eye position
}

AND seteyeposition() BE
{ LET FLT d1 = eyedist
  LET FLT d2 = d1 * 0.707
  LET FLT d3 = d2 / 3

  cetn, cetw, ceth :=  One, Zro, Zro
  cewn, ceww, cewh :=  Zro, One, Zro
  celn, celw, celh :=  Zro, Zro, One
  eyen, eyew, eyeh :=  -eyedist,   Zro, Zro   // Relative eye position


UNLESS 0<=eyedir<=8 DO eyedir := 1

  IF hatdir & sdlmsecs()>hatmsecs+100 DO
  { eyedir := FIX((angle(ctn, ctw)+360.0+22.5) / 45.0) & 7
    // dir = 0  heading N
    // dir = 1  heading NE
    // dir = 2  heading E
    // dir = 3  heading SE
    // dir = 4  heading S
    // dir = 5  heading SW
    // dir = 6  heading W
    // dir = 7  heading NW
    SWITCHON hatdir INTO
    { DEFAULT:
      CASE #b0001:                     ENDCASE // Forward
      CASE #b0011: eyedir := eyedir+1; ENDCASE // Forward right
      CASE #b0010: eyedir := eyedir+2; ENDCASE // Right
      CASE #b0110: eyedir := eyedir+3; ENDCASE // Backward right
      CASE #b0100: eyedir := eyedir+4; ENDCASE // Backward
      CASE #b1100: eyedir := eyedir+5; ENDCASE // Backward left
      CASE #b1000: eyedir := eyedir+6; ENDCASE // Left
      CASE #b1001: eyedir := eyedir+7; ENDCASE // Forward left
    }
    eyedir := (eyedir & 7) + 1
    hatdir := 0

//writef("ctn=%9.3f ctw=%9.3f eyedir=%9.1f*n", ctn, ctw, eyedir)
//abort(1009) 
  }

  SWITCHON eyedir INTO
  { DEFAULT:

    CASE 0: // Pilot's view
      cetn, cetw, ceth := ctn, ctw, cth
      cewn, ceww, cewh := cwn, cww, cwh
      celn, celw, celh := cln, clw, clh

      eyen, eyew, eyeh := Zro, Zro, Zro   // Relative eye position
      RETURN

     CASE 1: // North
       cetn, cetw, ceth :=  One, Zro, Zro
       cewn, ceww, cewh :=  Zro, One, Zro
       celn, celw, celh :=  Zro, Zro, One
       eyen, eyew, eyeh :=  -d1, Zro,  d3   // Relative eye position
       RETURN

     CASE 2: // North east
       cetn, cetw, ceth :=  D45, D45, Zro
       cewn, ceww, cewh := -D45, D45, Zro
       celn, celw, celh :=  Zro, Zro, One
       eyen, eyew, eyeh :=  -d2, -d2,  d3   // Relative eye position
       RETURN

     CASE 3: // East
       cetn, cetw, ceth :=  Zro, One, Zro
       cewn, ceww, cewh := -One, Zro, Zro
       celn, celw, celh :=  Zro, Zro, One
       eyen, eyew, eyeh :=  Zro, -d1,  d3   // Relative eye position
       RETURN

     CASE 4: // South east
       cetn, cetw, ceth := -D45, D45, Zro
       cewn, ceww, cewh := -D45,-D45, Zro
       celn, celw, celh :=  Zro, Zro, One
       eyen, eyew, eyeh :=   d2, -d2,  d3   // Relative eye position
       RETURN

     CASE 5: // South
       cetn, cetw, ceth := -One,  Zro, Zro
       cewn, ceww, cewh :=  Zro, -One, Zro
       celn, celw, celh :=  Zro,  Zro, One
       eyen, eyew, eyeh :=   d1,  Zro,  d3   // Relative eye position
       RETURN

     CASE 6: // South west
       cetn, cetw, ceth :=-D45,-D45, Zro
       cewn, ceww, cewh := D45,-D45, Zro
       celn, celw, celh := Zro, Zro, One
       eyen, eyew, eyeh :=  d2,  d2,  d3   // Relative eye position
       RETURN

     CASE 7: // West
       cetn, cetw, ceth := Zro,-One, Zro
       cewn, ceww, cewh := One, Zro, Zro
       celn, celw, celh := Zro, Zro, One
       eyen, eyew, eyeh := Zro,  d1,  d3   // Relative eye position

       RETURN

     CASE 8: // North west
       cetn, cetw, ceth := D45,-D45, Zro
       cewn, ceww, cewh := D45, D45, Zro
       celn, celw, celh := Zro, Zro, One
       eyen, eyew, eyeh := -d2,  d2,  d3   // Relative eye position
       RETURN
  }
}

**********************************/


