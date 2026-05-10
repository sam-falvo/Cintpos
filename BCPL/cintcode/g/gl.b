/*
############### UNDER DEVELOPMENT #####################

This library provides functions that interface with the OpenGL
Graphics library. Most of the OpenGL functions provided by this
library are invoked by calls of the form:

res := sys(Sys_gl, fno, a1, a2, a3, a4,...)

provided by the function glfn defined in cintcode/sysc/glfn.c

These do not involve code in this file, but OpenGL constants such as
GL_ARRAY_BUFFER or GL_VERTEX_SHADER used in these calls are declared
in the file g/glmanifests.h created by the program
sysc/mkglmanifests.c to ensure that the constants have the values
required by OpenGL.

All these calls work with both OpenGL ES and most versions of
OpenGL.

This file contains functions such as mkwindow that use non OpenGL
libraries such as SDL, glut or EGL. Hopefully, the user will not need
to know which of these support libraries are being used.

The calls of sys(Sys_gl, fno,...) allow many OpenGL functions to be
called. To add another OpenGL function the following steps are
required.

1) Add a manifest declaration, such as gl_GenTextures, to g/gl.h and
   a corressponding #define in sysc/cintsys.h.
2) Insert a suitable case in the switch in sysc/glfn.c. For instance
   case gl_GenTextures: // (n, textures)
   { glGenTextures((GLsizei)a[1]. (GLuint *)(&W[a[2]]));
     return -1;
   }
3) If new a new OpenGL constants are needed  needed, add a line
   such as
        w("GL_TEXTURE_2D=%n*n", GL_TEXTURE_2D
   to sysc/mkglmanifests-h.c. This file is compiled and run when
   needed to create a new version of g/glmanifests.h. 

Implemented by Martin Richards (c) May 2020

Change history:

01/05/2020
This file is undergoing major redevelopment.

23/04/18
Changed load model to use the new .mdl format.
Extensively changed to use the FLT feature.

15/07/13
Started adding OpenGL functions.

26/08/12
Initial implementation.

This library allows the BCPL user to perform OpenGL operations and
access keboard, mouse and joystick events. In due course sound
features will probably be added. The initial implementation assume
that 32-bit BCPL with the FLT feature is being used. The interface
with OpenGL makes extensive use of 32-bit floating point.

This library should be included as a separate section.  Such programs
typically have the following structure.

GET "libhdr"
MANIFEST { g_glbase=nnn  }  // Only used if the default setting of 450 in
                            // libhdr is not suitable.
                            // Note that sdl.h also has 450 as the default
                            // value, so GET "gl.h" and "sdl.h" should
                            // not occur together.
GET "gl.h"
GET "gl.b"                  // Insert the library source code
.
GET "libhdr"
MANIFEST { g_glbase=nnn  }  // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "gl.h"
<Rest of the program>

This function defines the following functions.

glInit()
glMkScreen(titla, xsize, ysize)
glSetPerspective(mat4, FLT fovy, FLT aspect, FLT n, FLT f)
glRadius2(FLT x, FLT y)
glRadius3(FLT x, FLT y,FLT z)
getevent()
loadmodel(filename, modelv)
//push32(v, i, upb, val)
//put32(v, i, val)
//get32(v, i)
//push16(v, i, upb, val)
//put16(v, i, val)
//get16(v, i)

glMainEventLoop()


*/

LET glInit() = VALOF
{ // Return TRUE if OpenGL is successfully initialised.
  LET n = sys(Sys_gl, gl_Init)
  //writef("gl_Init returned %n*n", n)
  UNLESS n DO
  { LET mes = VEC 256/bytesperword
    mes%0 := 0

    //sys(Sys_gl, gl_GetError, mes)
    //sawritef("*nglInit unable to initialise OpenGL: %s*n", mes)
  }

  RESULTIS n
}

AND glMkScreen(title, xsize, ysize) = VALOF
{ // Create an OpenGL window and GL context with given title
  // and size. If successful it returns the x size of the
  // window and sets result2 to the y size. It also displays
  // the window as a pale blue rectangle of the specified
  // size with its title.
  // On failure it reurns 0.
  
  LET ok = ?
  LET mes = VEC 256/bytesperword
  mes%0 := 0

  //writef("glMkScreen: Creating an OpenGL window %n x %n*n", xsize, ysize)

  screenxsize, screenysize := xsize, ysize

  //writef("MkScreen: calling sys(Sys_gl, gl_MkScreen, %s, %n %n)*n",
  //        title, xsize, ysize)

  screenxsize := sys(Sys_gl, gl_MkScreen, title, xsize, ysize)
  screenysize := result2
  screenaspect := FLOAT screenxsize / FLOAT screenysize

  //writef("gl_MkScreen: returned screen size %n x %n aspect %8.6f*n",
  //       screenxsize, screenysize, screenaspect)

  UNLESS screenxsize>0 DO
  { sys(Sys_gl, gl_GetError, mes)
    writef("Unable to create an OpenGL screen: %s*n", mes)
    RESULTIS 0
  }

  // Normally when a pixel is written it overrides the the colour
  // at that pixel position even if its alpha value is not 1.0.
  // However we can tell OpenGL the blend the colour of the new
  // pixel with the previous colour at that position.
  sys(Sys_gl, gl_BlendFunc, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  sys(Sys_gl, gl_Enable, GL_BLEND)

  result2 := screenysize
  RESULTIS screenxsize
}

// The follow function has been change to make its arguments in the
// order as for gluPerspective in the glu library. This is an incompatible
// change but only affects g/gl.b, raspi/{gltiger.b gltst.b} and
// genome/dnaplot.b.
AND glSetPerspective(mat4, FLT fovy, FLT aspect, FLT n, FLT f) BE
//AND glSetPerspective(mat4, FLT aspect, FLT fov, FLT n, FLT f) BE
{ // This sets the 4x4 matrix to transform coordinates in a specified
  // frustum to coordinates of normalised screen aces with the z axis
  // passing through the centre of the window.
  // fov is the field of view specified as the distance at z=1 between
  // the centre of the screen and the upper edge of the window.
  // ie fovy = height/(2*n)
  // aspect = width/height ie width = height*aspect
  // n and f are the distances to the near and far clipping plane.
  // The matrix we require is
  //   ( 2*n/width           0             0               0 )
  //   (         0  2*n/height             0               0 )
  //   (         0           0  -(f+n)/(f-n)  -(2*f*n)/(f-n) )
  //   (         0           0            -1              0  )
  // It is calculated as follows.
  LET FLT fv = 2.0 / fovy

  setvec(mat4, 16,  fv/aspect, 0.0,           0.0,  0.0,   // Column 1
                   0.0,         fv,           0.0,  0.0,   // Column 2
                   0.0,        0.0,   (f+n)/(n-f), -1.0,   // Column 3
                   0.0,        0.0, (2*f*n)/(n-f),  0.0)   // Column 4
}

AND glRadius2(FLT x, FLT y) = sys(Sys_flt, fl_sqrt, x*x + y*y)

AND glRadius3(FLT x, FLT y, FLT z) = sys(Sys_flt, fl_sqrt, x*x + y*y + z*z)

AND tok2str(tok) = VALOF SWITCHON tok INTO
{ DEFAULT:      RESULTIS "?"

  CASE s_vs:    RESULTIS "vs"
  CASE s_x:     RESULTIS "x"
  CASE s_y:     RESULTIS "y"
  CASE s_z:     RESULTIS "z"
  CASE s_r:     RESULTIS "r"
  CASE s_g:     RESULTIS "g"
  CASE s_b:     RESULTIS "b"
  CASE s_k:     RESULTIS "k"
  CASE s_d:     RESULTIS "d"

  CASE s_is:    RESULTIS "is"
  CASE s_i:     RESULTIS "i"

  CASE s_ds:    RESULTIS "ds"
  CASE s_t:     RESULTIS "t"
  
  CASE s_num:   RESULTIS "num"
  CASE s_eof:   RESULTIS "eof"
}

AND getevent() = sys(Sys_gl, gl_pollevent, @eventtype)

AND error(mes, a, b, c) BE
{ // For error found by loadmodel.
  writef("ERROR near line %n: ", lineno)
  writef(mes, a, b, c)
  newline()
  abort(999)
}

AND rdnum() = VALOF
{ LET res = lexval
  UNLESS token=s_num DO error("Number expected")
  lex()
  RESULTIS res
}

AND rdflt32() = VALOF
{ LET x = rdnum()
  IF ON64 RESULTIS sys(Sys_flt, fl_64to32, x)
  RESULTIS x
}

AND loadmodel(filename, modelv) = VALOF
{ // This function reads a .mdl file specifying the vertices,
  // indices and display items of a model.
  // modelv is a vector with 6 elements to hold the details of
  //          the model being read. It returns modelv if successful,
  //          and if so it sets the following
  // modelv!0 will be a vector of floating point numbers representing
  //          vertices consisting of: x,y,z, r,g,b, k and d giving
  //          the location, colour and k and d values of each vertex.
  // nodelv!1 will be the upb of modelv!0
  // modelv!2 will be the index vector of vertex numbers
  // nodelv!3 will be the upb of modelv!2
  // modelv!4 will be the vector holding display triplets of the
  //          form [m, n, i] where
  //          m is the mode of primitive eg 5 = Triangles.
  //          n is the number of index elements to use.
  //          i is a position in the index vector
  // modelv!5 will be the upb of modelv!4
  
  // Syntax of .mdl files

  // vs n         n is the upb of vertex vector
  //              the elements are 32 bit floats.
  //              The first vertex is at subscript position zero.
  // x n          n is the x coordinate in a vertex
  // y n          n is the y coordinate in a vertex
  // z n          n is the z coordinate in a vertex
  // r n          n is the red component in a vertex
  // g n          n is the green component in a vertex
  // b n          n is the blue component in a vertex
  // k n          n is the k value in a vertex
  // d n          n is the d value in a vertex

  // is n         n is the upb of index vector of 32-bit integers.
  // i n          n is an index vector element

  // ds n         n is the upb of the display vector.
  // t mode n p   set a display triplet
  //              where mode   is 0   points
  //                              1   seperate lines
  //                              2   line loop
  //                              3   line strip
  //                              4   triangles
  //                              5   triangle strip
  //                              6   triangle fan
  //                    n     is the number of index values belonging
  //                          to this display item. For example if two
  //                          triangles are being drawn n will be 6.
  //              and   p     is a subscript of the first index value
  //                          belonging this display item.
  // z            end of file
  
  LET res = TRUE
  LET n = 0
  LET stdin = input()
  LET instream = findinput(filename)

  LET curr_r, curr_g, curr_b = -1, -1, -1 // Initially unset
  LET curr_k, curr_d = -1, -1             // Initially unset

  // Declare variables for the vertex, index and display vectors.
  LET vv, vvupb = 0, 0  // vertices, each vertex is [x,y,z, r,g,b, k,d]
  // The first vertex item will start at subscript position 0.
  // Vertices are numbered by consective integers starting at zero.
  LET iv, ivupb = 0, 0  // index vector. If triangles are being modelled
                        // three indices are used to identif the vertices.
  LET dv, dvupb = 0, 0  // display items, each item is [mode, n, i]
                        // mode specifies the kind of objects being draw
			// n    id the number of objects to draw
			// i    identifies the first vertex of the first
			//      object to draw.
  LET vpos = 0          // first free position in the Vertex vector.
  LET ipos = 0          // first free position in the Index vector.
  LET dpos = 0          // first free position in the Display items vector.

  lineno := 1 // The first line has lineno=1

  UNLESS instream DO
  { error("Trouble with file %s", filename)
    RESULTIS FALSE
  }

  selectinput(instream)

  ch := rdch()

nxt:
//sawritef("loadmodel: about to call lex()*n"); checkpos(ch)
  lex()

  UNTIL token=s_eof SWITCHON token INTO
  { DEFAULT:  writef("line %n: Bad model file*n", lineno)
              res := FALSE
              GOTO ret

    CASE s_vs:  // Set vupb and allocate space
                // This is a vector of floating point numbers to hold the
		// vertex items [x,y,z, r,g,b, k,d]
      lex()
      n := FIX rdnum()
      vvupb := n//+32 // Why as 32 ?????
      //writef("vs: %n*n", n)
//abort(1009)
      vv := getvec(vvupb)
      UNLESS vv DO
      { writef("Unable to allocate v with upb=%n*n", vvupb)
        res := 0
	GOTO ret
      }
      FOR i = 0 TO vvupb DO vv!i := 0
      LOOP
 
    CASE s_x: // These are all floating point elements of
    CASE s_y: // a vertex
    CASE s_z:
    CASE s_r:
    CASE s_g:
    CASE s_b:
    CASE s_k:
    CASE s_d:
      lex()
      
      IF vv=0 DO
      { writef("Vertex given before the vertex vector is allocated*n")
        abort(999)
	res := 0
	GOTO ret
      }
      
      IF vpos>vvupb DO
      { writef("Too much data for the vertex vector, vpos=%n vvupb=%n*n",
                vpos, vvupb)
        abort(999)
	res := 0
	GOTO ret
      }

      vv!vpos := rdflt32()
      vpos := vpos+1
      LOOP
      
    CASE s_is:          // Set ivupb and allocate space
      lex()
      ivupb := FIX rdnum()
      // The index vector will hold 32-bit integers
      iv := getvec(ivupb)
      UNLESS iv DO
      { writef("Unable to allocate iv with upb=%n*n", ivupb)
        abort(999)
	res := FALSE
	GOTO ret
      }
      FOR i = 0 TO ivupb DO iv!i := 0
//abort(1000)
      LOOP

    CASE s_ds:          // Set dvupb and allocate its vector
      lex()
      dvupb := FIX rdnum()
      dv := getvec(dvupb) // dv is a vector of BCPLWORDS
                          // Typically quite small.
      UNLESS dv DO
      { writef("Unable to allocate dv with upb=%n*n", dvupb)
	res := FALSE
	GOTO ret
      }
      FOR i = 0 TO dvupb DO dv!i := 0 // Clear the display vector for safety
//abort(1000)
      LOOP

    CASE s_t:          // Set a display vector triplet
      UNLESS dv DO
      { writef("Display item given before the display vector is allocated*n")
        abort(999)
        res := FALSE
	GOTO ret
      }
      IF dpos+2 > dvupb DO
      { error("Too many dvec items, dpos=%n dvupb=%n*n", dpos, dvupb)
        abort(999)
        res := FALSE
	GOTO ret
      }
      lex()
      dv!(dpos+0) := FIX rdnum()         // mode
      dv!(dpos+1) := FIX rdnum()         // n
      dv!(dpos+2) := FIX rdnum()         // offset

      dpos := dpos+3
      LOOP

    CASE s_i:       // An index vector value
      lex()
      
      UNLESS iv DO
      { writef("Index value given before the index vector is allocate*n")
        abort(999)
        res := FALSE
	GOTO ret
      }

      IF ipos>ivupb DO
      { writef("Too many index values, ipos=%n ivupb=%n*n", ipos, ivupb)
        abort(999)
        res := FALSE
	GOTO ret
      }

      iv!ipos := FIX rdnum()
      ipos := ipos+1
      LOOP

    CASE s_eof:
      token := s_eof
      ENDCASE
  }

  UNLESS vv & iv & dv DO
  { error("One or more of v, ivec or dvec is missing")
    res := FALSE
    GOTO ret
  }
   
  modelv!0, modelv!1 := vv, vpos-1   // Vertex vector and its upb
  modelv!2, modelv!3 := iv, ipos-1   // Index vector and its upb
  modelv!4, modelv!5 := dv, dpos-1   // Display vector and its upb
 
ret:
  IF instream DO endstream(instream)
  selectinput(stdin)
  RESULTIS res
}

AND checkfor(tok, mess) BE UNLESS token=tok DO
{ writef("ERROR: %s token=%s tok=%s", mess, tok2str(token), tok2str(tok))
  lex()
}

AND lex() BE
{ SWITCHON ch INTO
  { DEFAULT:
      error("line %n: Bad character '%c' in model file", lineno, ch)
      ch := rdch()
      LOOP

    CASE endstreamch:
      token := s_eof     // marks the end of file.
      RETURN

    CASE '/': // Skip over comments
      UNTIL ch='*n' | ch=endstreamch DO ch := rdch()
      LOOP

    CASE '*n':
      lineno := lineno+1

    CASE '*s':
      ch := rdch()
      LOOP

    CASE 'v':
      ch := rdch()
      UNLESS ch='s' DO
      { writef("Bad vs directive*n")
        abort(999)
      }
      token := s_vs
      ch := rdch()
      RETURN

    CASE 'x': token := s_x; ch := rdch(); RETURN
    CASE 'y': token := s_y; ch := rdch(); RETURN
    CASE 'z': token := s_z; ch := rdch(); RETURN
    CASE 'r': token := s_r; ch := rdch(); RETURN
    CASE 'g': token := s_g; ch := rdch(); RETURN
    CASE 'b': token := s_b; ch := rdch(); RETURN
    CASE 'k': token := s_k; ch := rdch(); RETURN
    CASE 'd': ch := rdch()
              TEST ch='s' THEN { token := s_ds; ch := rdch() }
	                  ELSE { token := s_d }
	      RETURN 
    CASE 'i': ch := rdch()
              TEST ch='s' THEN { token := s_is; ch := rdch() }
	                  ELSE { token := s_i }
	      RETURN 
    CASE 't': token := s_t; ch := rdch(); RETURN
     
    CASE '-': CASE '+':
    CASE '0': CASE '1': CASE '2': CASE '3': CASE '4': 
    CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
      unrdch()

      lexval := readflt()
      IF result2 DO
      { error("Bad floating point number")
        abort(999)
      }
      // Re-read the terminating character
      ch := rdch()
      token := s_num
      RETURN
  }
} REPEAT

//AND push32(v, i, upb, val) = VALOF
//{ // v is a vector of 32 bit elements
//  // i is a subscript into this vector
//  //writef("push32: i=%n upb=%n val=%10.3f ON64=%n*n", i, upb, val, ON64)
//  IF i > upb DO
//  { error("Unable to push a 32-bit value, upb=%n", upb)
//    RESULTIS i+1
//  }
//  TEST ON64 THEN put32(v, i, val)
//            ELSE v!i := val
//  //writef("push32: i=%n val=%10.3f*n", i, get32(v, i))
//  RESULTIS i+1
//}

//AND put32(v, i, val) BE
//{ // v is a vector of 32 bit elements
//  // i is a subscript into this vector
//  LET w = 0
//  LET p = 4*i // Byte position relative to v, 4 bytes per 32 bit word
//  LET a, b, c, d = val&255, (val>>8) & 255, (val>>16) & 255, (val>>24) & 255
//  (@w)%0 := 1
//  TEST (w & 1) = 0
//  THEN v%p, v%(p+1), v%(p+2), v%(p+3) := d, c, b, a // Big ender m/c 
//  ELSE v%p, v%(p+1), v%(p+2), v%(p+3) := a, b, c, d // Little ender m/c 
//}

//AND get32(v, i) = VALOF
//{ // v is a vector of 32 bit elements
//  // i is a subscript into this vector
//  LET w = 1
//  LET p = 4*i // Byte position relative to v, 4 bytes per 32 bit word
//  LET a, b, c, d = v%p, v%(p+1), v%(p+2), v%(p+3)
//  TEST (w & 1) = 0
//  THEN RESULTIS (a<<24) + (b<<16) + (c<<8) +  d // Big ender m/c 
//  ELSE RESULTIS (d<<24) + (c<<16) + (b<<8) +  a // Little ender m/c 
//}

//AND push16(v, i, upb, val) = VALOF
//{ // v is a vector of 16 bit elements
//  // i is a subscript into this vector
//  IF i > upb DO
//  { error("Unable to push a 16-bit value, upb=%n", upb)
//    RESULTIS i+1
//  }
//  put16(v, i, val)
//  RESULTIS i+1
//}

//AND put16(v, i, val) BE
//{ // v is a vector of 16 bit elements
//  // i is a subscript into this vector
//  LET w = 0
//  LET p = 2*i // Byte position relative to v
//  LET a, b = val&255, (val>>8) & 255
//  (@w)%0 := 1
//  TEST (w & 1) = 0
//  THEN v%p, v%(p+1) := b, a // Big ender m/c 
//  ELSE v%p, v%(p+1) := a, b // Little ender m/c 
//}

//AND get16(v, i) = VALOF
//{ // v is a vector of 16 bit elements
//  // i is a subscript into this vector
//  LET w = 1
//  LET p = 2*i // Byte position relative to v
//  LET a, b = v%p, v%(p+1)
//  (@w)%0 := 1
//  TEST (w & 1) = 0
//  THEN RESULTIS (a<<8) + b // Big ender m/c 
//  ELSE RESULTIS (b<<8) + a // Little ender m/c 
//}



//############################################

AND drawstring(x, y, s) BE
{ moveto(x, y)
  FOR i = 1 TO s%0 DO drawch(s%i)
}

AND plotf(x, y, form, a, b, c, d, e, f, g, h) BE
{ // This is like writef but writes to position (x,y)
  // on the screen.
  LET oldwrch = wrch
  LET s = VEC 256/bytesperword
  plotfstr := s
  plotfstr%0 := 0
  wrch := plotwrch
  writef(form, a, b, c, d, e, f, g, h)
  wrch := oldwrch
  drawstring(x, y, plotfstr)
}

AND plotwrch(ch) BE
{ LET strlen = plotfstr%0 + 1
  plotfstr%strlen := ch
  plotfstr%0 := strlen 
}


AND initsdl1() = VALOF
{ 
  leftxv, rightxv := 0, 0


  currx,   curry := 0, 0
  miny, maxy := 0, 0
  // Successful
  RESULTIS TRUE
}

AND pxlrgba(r, g, b, a) = ((a<<8 | b)<<8 | g)<<8 | r // #Xaabbggrr


AND closesdl1() BE
{ IF leftxv  DO freevec(leftxv)
  IF rightxv DO freevec(rightxv)
}

AND setcolour(col) BE currcolour := col

AND setcolourkey(col) BE colourkey := col // Pixels of this colour are not
                                          // written.

AND moveto(x, y)   BE currx, curry := x, y
AND moveby(dx, dy) BE moveto(currx+dx, curry+dy)

AND drawto(x1, y1) BE
{ LET x, y = currx, curry
  LET x0, y0 = x, y
  // Draw a line from (x0,y0) to (x1,y1) using currcolour.
  // Leave (currx,curry) - (x1,y1).
  // This function used Bresenham's algorithm to draw a 2D line.
  LET dx  = ABS(x1-x0)
  AND dy  = ABS(y1-y0)
  LET sx  = x0 < x1 -> 1, -1
  LET sy  = y0 < y1 -> 1, -1
  LET err = dx-dy
  LET e2  = ?

  { drawpoint(x, y)
    IF x=x1 & y=y1 DO
    { currx, curry := x, y
      RETURN
    }
    e2 := 2*err
    IF e2 > -dy DO err, x := err-dy, x+sx
    IF e2 <  dx DO err, y := err+dx, y+sy
  } REPEAT
}

AND drawby(dx, dy) BE drawto(currx+dx, curry+dy)

AND drawch(ch) BE TEST ch='*n'
THEN { currx, curry := 10, curry-14
     }
ELSE { FOR line = 0 TO 11 DO
         write_ch_slice(currx, curry+11-line, ch, line)
       currx := currx+9
     }

AND write_ch_slice(x, y, ch, line) BE
{ // Writes the horizontal slice of the given 8x12 character.
  LET cx, cy = currx, curry
  LET i = (ch&#x7F) - '*s'
  // 3*i = subscript of the character in the following table.
  LET charbase = TABLE // Still under development !!!
         #x00000000, #x00000000, #x00000000, // space
         #x18181818, #x18180018, #x18000000, // !
         #x66666600, #x00000000, #x00000000, // "
         #x6666FFFF, #x66FFFF66, #x66000000, // #
         #x7EFFD8FE, #x7F1B1BFF, #x7E000000, // $
         #x06666C0C, #x18303666, #x60000000, // %
         #x3078C8C8, #x7276DCCC, #x76000000, // &
         #x18181800, #x00000000, #x00000000, // '
         #x18306060, #x60606030, #x18000000, // (
         #x180C0606, #x0606060C, #x18000000, // )
         #x00009254, #x38FE3854, #x92000000, // *
         #x00000018, #x187E7E18, #x18000000, // +
         #x00000000, #x00001818, #x08100000, // ,
         #x00000000, #x007E7E00, #x00000000, // -
         #x00000000, #x00000018, #x18000000, // .
         #x06060C0C, #x18183030, #x60600000, // /
         #x386CC6C6, #xC6C6C66C, #x38000000, // 0
         #x18387818, #x18181818, #x18000000, // 1
         #x3C7E6206, #x0C18307E, #x7E000000, // 2
         #x3C6E4606, #x1C06466E, #x3C000000, // 3
         #x1C3C3C6C, #xCCFFFF0C, #x0C000000, // 4
         #x7E7E6060, #x7C0E466E, #x3C000000, // 5
         #x3C7E6060, #x7C66667E, #x3C000000, // 6
         #x7E7E0606, #x0C183060, #x40000000, // 7
         #x3C666666, #x3C666666, #x3C000000, // 8
         #x3C666666, #x3E060666, #x3C000000, // 9
         #x00001818, #x00001818, #x00000000, // :
         #x00001818, #x00001818, #x08100000, // ;
         #x00060C18, #x30603018, #x0C060000, // <
         #x00000000, #x7C007C00, #x00000000, // =
         #x00603018, #x0C060C18, #x30600000, // >
         #x3C7E0606, #x0C181800, #x18180000, // ?
         #x7E819DA5, #xA5A59F80, #x7F000000, // @
         #x3C7EC3C3, #xFFFFC3C3, #xC3000000, // A
         #xFEFFC3FE, #xFEC3C3FF, #xFE000000, // B
         #x3E7FC3C0, #xC0C0C37F, #x3E000000, // C
         #xFCFEC3C3, #xC3C3C3FE, #xFC000000, // D
         #xFFFFC0FC, #xFCC0C0FF, #xFF000000, // E
         #xFFFFC0FC, #xFCC0C0C0, #xC0000000, // F
         #x3E7FE1C0, #xCFCFE3FF, #x7E000000, // G
         #xC3C3C3FF, #xFFC3C3C3, #xC3000000, // H
         #x18181818, #x18181818, #x18000000, // I
         #x7F7F0C0C, #x0C0CCCFC, #x78000000, // J
         #xC2C6CCD8, #xF0F8CCC6, #xC2000000, // K
         #xC0C0C0C0, #xC0C0C0FE, #xFE000000, // L
         #x81C3E7FF, #xDBC3C3C3, #xC3000000, // M
         #x83C3E3F3, #xDBCFC7C3, #xC1000000, // N
         #x7EFFC3C3, #xC3C3C3FF, #x7E000000, // O
         #xFEFFC3C3, #xFFFEC0C0, #xC0000000, // P
         #x7EFFC3C3, #xDBCFC7FE, #x7D000000, // Q
         #xFEFFC3C3, #xFFFECCC6, #xC3000000, // R
         #x7EC3C0C0, #x7E0303C3, #x7E000000, // S
         #xFFFF1818, #x18181818, #x18000000, // T
         #xC3C3C3C3, #xC3C3C37E, #x3C000000, // U
         #x81C3C366, #x663C3C18, #x18000000, // V
         #xC3C3C3C3, #xDBFFE7C3, #x81000000, // W
         #xC3C3663C, #x183C66C3, #xC3000000, // X
         #xC3C36666, #x3C3C1818, #x18000000, // Y
         #xFFFF060C, #x183060FF, #xFF000000, // Z
         #x78786060, #x60606060, #x78780000, // [
         #x60603030, #x18180C0C, #x06060000, // \
         #x1E1E0606, #x06060606, #x1E1E0000, // ]
         #x10284400, #x00000000, #x00000000, // ^
         #x00000000, #x00000000, #x00FFFF00, // _
         #x30180C00, #x00000000, #x00000000, // `
         #x00007AFE, #xC6C6C6FE, #x7B000000, // a
         #xC0C0DCFE, #xC6C6C6FE, #xDC000000, // b
         #x00007CFE, #xC6C0C6FE, #x7C000000, // c
         #x060676FE, #xC6C6C6FE, #x76000000, // d
         #x00007CFE, #xC6FCC0FE, #x7C000000, // e
         #x000078FC, #xC0F0F0C0, #xC0000000, // f
         #x000076FE, #xC6C6C6FE, #x7606FE7C, // g
         #xC0C0DCFE, #xC6C6C6C6, #xC6000000, // h
         #x18180018, #x18181818, #x18000000, // i
         #x0C0C000C, #x0C0C0C7C, #x38000000, // j
         #x00C0C6CC, #xD8F0F8CC, #xC6000000, // k
         #x00606060, #x6060607C, #x38000000, // l
         #x00006CFE, #xD6D6D6D6, #xD6000000, // m
         #x0000DCFE, #xC6C6C6C6, #xC6000000, // n
         #x00007CFE, #xC6C6C6FE, #x7C000000, // o
         #x00007CFE, #xC6FEFCC0, #xC0000000, // p
         #x00007CFE, #xC6FE7E06, #x06000000, // q
         #x0000DCFE, #xC6C0C0C0, #xC0000000, // r
         #x00007CFE, #xC07C06FE, #x7C000000, // s
         #x0060F8F8, #x6060607C, #x38000000, // t
         #x0000C6C6, #xC6C6C6FE, #x7C000000, // u
         #x0000C6C6, #x6C6C6C38, #x10000000, // v
         #x0000D6D6, #xD6D6D6FE, #x6C000000, // w
         #x0000C6C6, #x6C386CC6, #xC6000000, // x
         #x0000C6C6, #xC6C6C67E, #x7606FE7C, // y
         #x00007EFE, #x0C3860FE, #xFC000000, // z
         #x0C181808, #x18301808, #x18180C00, // {
         #x18181818, #x18181818, #x18181800, // |
         #x30181810, #x180C1810, #x18183000, // }
         #x00000070, #xD1998B0E, #x00000000, // ~
         #xAA55AA55, #xAA55AA55, #xAA55AA55  // rubout

  IF i>=0 DO charbase := charbase + 3*i

  // charbase points to the three words giving the
  // pixels of the character.
  { LET col = currcolour
    LET w = VALOF SWITCHON line INTO
    { CASE  0: RESULTIS charbase!0>>24
      CASE  1: RESULTIS charbase!0>>16
      CASE  2: RESULTIS charbase!0>> 8
      CASE  3: RESULTIS charbase!0
      CASE  4: RESULTIS charbase!1>>24
      CASE  5: RESULTIS charbase!1>>16
      CASE  6: RESULTIS charbase!1>> 8
      CASE  7: RESULTIS charbase!1
      CASE  8: RESULTIS charbase!2>>24
      CASE  9: RESULTIS charbase!2>>16
      CASE 10: RESULTIS charbase!2>> 8
      CASE 11: RESULTIS charbase!2
    }

    IF (w & #b10000010) > 0 DO drawpoint(x+0, y)
    IF (w & #b01000010) > 0 DO drawpoint(x+1, y)
    IF (w & #b00100010) > 0 DO drawpoint(x+2, y)
    IF (w & #b00010010) > 0 DO drawpoint(x+3, y)
    IF (w & #b00001010) > 0 DO drawpoint(x+4, y)
    IF (w & #b00000100) > 0 DO drawpoint(x+5, y)
    IF (w & #b00000010) > 0 DO drawpoint(x+6, y)
    IF (w & #b00000001) > 0 DO drawpoint(x+7, y)

//writef("writeslice: ch=%c line=%i2 w=%b8 bits=%x8 %x8 %x8*n",
//        ch, line, w, charbase!0, charbase!1, charbase!2)

  }

  currx, curry := cx, cy
}

AND drawpoint(x, y) BE UNLESS currcolour = colourkey DO
{ // Draw a pixel in the pixel plane
  // (0, 0) is the bottom left point on the surface
  IF 0<=x<pxlxsize & 0<=y<pxlysize DO
    pxlv!(pxlxsize*y + x) := currcolour
}


AND fillpxlplane(col) BE
{ FOR p = pxlv TO pxlxsize*pxlysize + pxlv - 1 DO !p := col
}

AND drawstr(x, y, s) BE
{ moveto(x, y)
  FOR i = 1 TO s%0 DO drawch(s%i)
}

AND drawf(x, y, form,
               a, b, c, d, e, f, g, h,i, j, k, l, m, n, o, p, q, r, s, t) BE
{ LET oldwrch = wrch
  LET s = VEC 256/bytesperword
  drawfstr := s
  drawfstr%0 := 0
  wrch := drawwrch
  writef(form, a, b, c, d, e, f, g, h,i, j, k, l, m, n, o, p, q, r, s, t)
  wrch := oldwrch
  drawstr(x, y, drawfstr)
}

AND drawwrch(ch) BE
{ LET strlen = drawfstr%0 + 1
  drawfstr%strlen := ch
  drawfstr%0 := strlen 
}

AND setlims(x0,y0, x1,y1) BE
{ // This function is used by drawtriangle to draw a filled 2D triangle.
  // It sets elements of leftxv and rightxv to the smallest and largest
  // values of x for each y when the line from (x0,y0) to (x1,y1) is
  // drawn provided 0 <= y < pxlysize.

  LET dx = ABS(x1-x0)
  AND dy = ABS(y1-y0)

  LET x, y  = x0, y0
  LET smax = dx + dy      // The sum of the x and y steps
  LET s    = 0            // number of steps so far

  LET sx = x0<x1 -> 1, -1 // Unit step in the x direction
  LET sy = y0<y1 -> 1, -1 // Unit step in the y direction
  LET err = dx-dy

  { // Start of loop stepping currx, curry or both.
    LET e2 = 2*err

    IF 0 <= y < pxlysize DO
    { // y is in range.
      IF leftxv !y > x DO leftxv !y := x
      IF rightxv!y < x DO rightxv!y := x
      IF miny > y DO miny := y
      IF maxy < y DO maxy := y
    }

    IF s>=smax RETURN // All pixels of the line have been processed.

    IF e2 > -dy DO
    { err := err - dy
      x := x + sx     // Unit step in the x direction
      s := s+1
    }
    IF e2 < dx DO
    { err := err + dx
      y := y + sy     // Unit step in the y direction
      s := s+1
    }
  } REPEAT
}

AND alloc2dvecs() BE
{ LET upb = pxlysize-1

  { leftxv  := getvec(upb)
    rightxv := getvec(upb)
    UNLESS leftxv & rightxv DO
    { sawritef("Unable to allocate leftxv and rightxv, pxlysize=%i3*n",
                pxlysize)
      abort(999)
    }
  }
  FOR y = 0 TO upb DO              // Initialise the relevant elements
    leftxv!y, rightxv!y := upb, 0  // of leftxv and rightxv.
}

AND drawtriangle(x1,y1, x2,y2, x3,y3) BE
{ // Draw a 2D triangle filled with currcol.

  // Ensure that leftxv and rightxv are allocated.
  UNLESS leftxv DO alloc2dvecs()

  // miny and maxy will hold the least and greatest y value in the range
  //0 to pxlysize-1 of any pixel in the triangle.. If no such
  // pixels exists, miny will be greater than maxy.

  miny, maxy := pxlysize, -1
  
  // The edges of the triangle are processed in turn. Whenever a pixel
  // on an edge is found with a y value between 0 and pxlysize-1, if
  // the x value is less than leftxv!y this is replaced by  the x value
  // of the pixel. Similarly, rightxv!y is conditionally updated. At the
  // end all pixels between (leftxv!y,y) and (rightxv!y,y) lie in the
  // triangle and those with x values between 0 and pxlxsize-1 will
  // be written to the screen. The calls of setlims also set miny and
  // maxy to the lowest and highest y values in the range 0
  // to pxlysize-1 that have pixels in the triangle.
  // 

  setlims(x1,y1, x2,y2)
  setlims(x2,y2, x3,y3)
  setlims(x3,y3, x1,y1)

  FOR y = miny TO maxy DO
  { // For each y value in the triangle draw the raster line at that level.
    // This code works even when none of the raster line pixels are on the
    // screen.
    moveto(leftxv !y, y)
    drawto(rightxv!y, y)
    // Reset these entries ready for another triangle to be drawn later.
    leftxv!y, rightxv!y := pxlysize-1, 0
  }
  
  // Drawing a triangle does not change the current position.
}

AND drawquad(x1,y1, x2,y2, x3,y3, x4,y4) BE
{ // A quad is drawn as two tiangles filled with currcol.
  drawtriangle(x1,y1, x2,y2, x3,y3)
  drawtriangle(x2,y2, x3,y3, x4,y4)
}

AND drawrect(x0, y0, x1, y1) BE
{ // Draw a 2D rectangle edges.
  LET xmin, xmax = x0, x1
  LET ymin, ymax = y0, y1
  IF xmin>xmax DO xmin, xmax := x1, x0
  IF ymin>ymax DO ymin, ymax := y1, y0

  FOR x = xmin TO xmax DO
  { drawpoint(x, ymin)
    drawpoint(x, ymax)
  }
  FOR y = ymin+1 TO ymax-1 DO
  { drawpoint(xmin, y)
    drawpoint(xmax, y)
  }
  currx, curry := x0, y0
}

AND drawfillrect(x0,y0, x1,y1) BE
{ // Draw a 2D rectangle filled with currcol.
  LET xmin, xmax = x0, x1
  LET ymin, ymax = y0, y1
  IF xmin>xmax DO xmin, xmax := x1, x0
  IF ymin>ymax DO ymin, ymax := y1, y0

  FOR y = ymin+1 TO ymax-1 FOR x = xmin TO xmax DO drawpoint(x, y)

  currx, curry := x0, y0
}

AND drawrndrect(x0,y0, x1,y1, radius) BE
{ LET xmin, xmax = x0, x1
  LET ymin, ymax = y0, y1
  LET r = radius
  LET f, ddf_x, ddf_y, x, y = ?, ?, ?, ?, ?

  IF xmin>xmax DO xmin, xmax := x1, x0
  IF ymin>ymax DO ymin, ymax := y1, y0
  IF r<0 DO r := 0
  IF r+r>xmax-xmin DO r := (xmax-xmin)/2
  IF r+r>ymax-ymin DO r := (ymax-ymin)/2

  FOR x = xmin+r TO xmax-r DO
  { drawpoint(x, ymin)
    drawpoint(x, ymax)
  }
  FOR y = ymin+r+1 TO ymax-r-1 DO
  { drawpoint(xmin, y)
    drawpoint(xmax, y)
  }
  // Now draw the rounded corners
  // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  f := 1 - r
  ddf_x := 1
  ddf_y := -2 * r
  x := 0
  y := r

  drawpoint(xmax, ymin+r)
  drawpoint(xmin, ymin+r)
  drawpoint(xmax, ymax-r)
  drawpoint(xmin, ymax-r)

  WHILE x<y DO
  { // ddf_x = 2*x + 1
    // ddf_y = -2 * y
    // f = x*x + y*y - radius*radius + 2*x - y + 1
    IF f>=0 DO
    { y := y-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    x := x+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    drawpoint(xmax-r+x, ymax-r+y) // octant 2
    drawpoint(xmin+r-x, ymax-r+y) // Octant 3
    drawpoint(xmax-r+x, ymin+r-y) // Octant 7
    drawpoint(xmin+r-x, ymin+r-y) // Octant 6
    drawpoint(xmax-r+y, ymax-r+x) // Octant 1
    drawpoint(xmin+r-y, ymax-r+x) // Octant 4
    drawpoint(xmax-r+y, ymin+r-x) // Octant 8
    drawpoint(xmin+r-y, ymin+r-x) // Octant 5
  }

  currx, curry := x0, y0
}

AND drawfillrndrect(x0, y0, x1, y1, radius) BE
{ LET xmin, xmax = x0, x1
  LET ymin, ymax = y0, y1
  LET r = radius
  LET f, ddf_x, ddf_y, x, y = ?, ?, ?, ?, ?
  LET lastx, lasty = 0, 0

  IF xmin>xmax DO xmin, xmax := x1, x0
  IF ymin>ymax DO ymin, ymax := y1, y0
  IF r<0 DO r := 0
  IF r+r>xmax-xmin DO r := (xmax-xmin)/2
  IF r+r>ymax-ymin DO r := (ymax-ymin)/2

  FOR x = xmin TO xmax FOR y = ymin+r TO ymax-r DO
  { drawpoint(x, y)
    drawpoint(x, y)
  }

  // Now draw the rounded corners
  // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  f := 1 - r
  ddf_x := 1
  ddf_y := -2 * r
  x := 0
  y := r

  drawpoint(xmax, ymin+r)
  drawpoint(xmin, ymin+r)
  drawpoint(xmax, ymax-r)
  drawpoint(xmin, ymax-r)

  WHILE x<y DO
  { // ddf_x = 2*x + 1
    // ddf_y = -2 * y
    // f = x*x + y*y - radius*radius + 2*x - y + 1
    IF f>=0 DO
    { y := y-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    x := x+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    drawpoint(xmax-r+x, ymax-r+y) // octant 2
    drawpoint(xmin+r-x, ymax-r+y) // Octant 3
    drawpoint(xmax-r+x, ymin+r-y) // Octant 7
    drawpoint(xmin+r-x, ymin+r-y) // Octant 6
    drawpoint(xmax-r+y, ymax-r+x) // Octant 1
    drawpoint(xmin+r-y, ymax-r+x) // Octant 4
    drawpoint(xmax-r+y, ymin+r-x) // Octant 8
    drawpoint(xmin+r-y, ymin+r-x) // Octant 5

    UNLESS x=lastx DO
    { FOR fx = xmin+r-y+1 TO xmax-r+y-1 DO
      { drawpoint(fx, ymax-r+x)
        drawpoint(fx, ymin+r-x)
      }
      lastx := x
    }
    UNLESS y=lasty DO
    { FOR fx = xmin+r-x+1 TO xmax-r+x-1 DO
      { drawpoint(fx, ymax-r+y)
        drawpoint(fx, ymin+r-y)
      }
    }
  }

  currx, curry := x0, y0
}

AND drawcircle(x0,y0, radius) BE
{ // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  LET f = 1 - radius
  LET ddf_x = 1
  LET ddf_y = -2 * radius
  LET x = 0
  LET y = radius
  drawpoint(x0, y0+radius)
  drawpoint(x0, y0-radius)
  drawpoint(x0+radius, y0)
  drawpoint(x0-radius, y0)

  WHILE x<y DO
  { // ddf_x = 2*x + 1
    // ddf_y = -2 * y
    // f = x*x + y*y - radius*radius + 2*x - y + 1
    IF f>=0 DO
    { y := y-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    x := x+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    drawpoint(x0+x, y0+y)
    drawpoint(x0-x, y0+y)
    drawpoint(x0+x, y0-y)
    drawpoint(x0-x, y0-y)
    drawpoint(x0+y, y0+x)
    drawpoint(x0-y, y0+x)
    drawpoint(x0+y, y0-x)
    drawpoint(x0-y, y0-x)
  }
}

AND drawfillcircle(x0, y0, radius) BE
{ // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  LET f = 1 - radius
  LET ddf_x = 1
  LET ddf_y = -2 * radius
  LET x = 0
  LET y = radius
  LET lastx, lasty = 0, 0
  drawpoint(x0, y0+radius)
  drawpoint(x0, y0-radius)
  FOR x = x0-radius TO x0+radius DO drawpoint(x, y0)

  WHILE x<y DO
  { // ddf_x = 2*x + 1
    // ddf_y = -2 * y
    // f = x*x + y*y - radius*radius + 2*x - y + 1
    IF f>=0 DO
    { y := y-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    x := x+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    drawpoint(x0+x, y0+y)
    drawpoint(x0-x, y0+y)
    drawpoint(x0+x, y0-y)
    drawpoint(x0-x, y0-y)
    drawpoint(x0+y, y0+x)
    drawpoint(x0-y, y0+x)
    drawpoint(x0+y, y0-x)
    drawpoint(x0-y, y0-x)
    UNLESS x=lastx DO
    { FOR fx = x0-y+1 TO x0+y-1 DO
      { drawpoint(fx, y0+x)
        drawpoint(fx, y0-x)
      }
      lastx := x
    }
    UNLESS y=lasty DO
    { FOR fx = x0-x+1 TO x0+x-1 DO
      { drawpoint(fx, y0+y)
        drawpoint(fx, y0-y)
      }
      lasty := y
    }
  }
}



