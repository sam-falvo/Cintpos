/*
############### UNDER DEVELOPMENT #####################

This library provides some functions that interface with the OpenGL
Graphics library that should work with both OpenGL ES using EGL and
the full version of OpenGL using SDL. The intention is for BCPL
programs to work without change under either version of OpenGL.

This will be compiled with one of the following conditional
compilation options set.

  OpenGL       for the full OpenGL library used with SDL
  OpenGLES     for OpenGL ES in the Raspberry Pi

Implemented by Martin Richards (c) Jan 2014

Most OpenGL functions are invoked using sys with calls of the form:

res := sys(Sys_gl, fno, a1, a2, a3, a4,...)

This is implemented by the function glfn defined 
in cintcode/sysc/glfn.c

Change history:

23/04/18
Changed load model to use the new .mdl format.
Extensively changed to use the FLT feature.

15/07/13
Started adding OpenGL functions.

26/08/12
Initial implementation.

This library provide the BCPL interface to the OpenGL features. Even
if OpenGL is called from EGL and not SDL, the SDL features will be
available providing access to keyboard, mouse and joy stick events,
and possibly sound features. Most OpenGL library functions are invoked
using call such as sys(Sys_gl, GL_CompileVshader, prog, cstr) where
the extra arguments are 32-bit values some of which are floating point
numbers. This interface is implemented by the C function glfn defined
in cintsys/stsc/glfn.c. Other useful functions are defined in this
file.  

This library should be included as a separate section for programs
that need it. Such programs typically have the following structure.

Note that most OpenGL functions are called via the sys function
as in: sys(Sys_gl, GL_BindAttribLocation, prog, loc, name).
The extra arguments are all 32-bit values some of which may be
floating point. The code in sysc/glfn.c converts them where necessary
to the types required by OpenGL.

GET "libhdr"
MANIFEST { g_glbase=nnn  }  // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "gl.h"
GET "gl.b"                  // Insert the library source code
.
GET "libhdr"
MANIFEST { g_glbase=nnn  }  // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "gl.h"
Rest of the program
 
*/

LET glInit() = VALOF
{ LET mes = VEC 256/bytesperword
  mes%0 := 0

  UNLESS sys(Sys_gl, GL_Init) DO
  { //sys(Sys_gl, GL_getError, mes)
    sawritef("*nglInit unable to initialise OpenGL: %s*n", mes)
    RESULTIS FALSE
  }

  RESULTIS TRUE  // Successful return
}

AND glMkScreen(title, xsize, ysize) = VALOF
{ // Create an OpenGL window with given title and size
  LET mes = VEC 256/bytesperword
  mes%0 := 0

  //writef("glMkScreen: Creating an OpenGL window*n")

  screenxsize, screenysize := xsize, ysize

  //writef("MkScreen: calling sys(Sys_gl, GL_MkScreen, %s, %n %n)*n",
  //        title, xsize, ysize)

  screenxsize := sys(Sys_gl, GL_MkScreen, title, xsize, ysize)
  screenysize := result2

  writef("GL_MkScreen: returned screen size %n x %n*n",
         screenxsize, screenysize)

  UNLESS screenxsize>0 DO
  { //sys(Sys_gl, GL_GetError, mes)
    writef("Unable to create an OpenGL screen: *n", mes)
    RESULTIS 0
  }

  result2 := screenysize
  RESULTIS screenxsize
}

AND glSetPerspective(mat4, FLT aspect, FLT fov, FLT n, FLT f) BE
{ // The field of view is given as a field of view at unit distance
  // ie field of view is 45 degrees if fov=2.0
  // aspect = width/height of screen in pixels
  LET FLT fv = 2.0 / fov

  setvec(mat4, 16,  fv, 0.0,           0.0,  0.0,   // Column 1
                   0.0,  fv,           0.0,  0.0,   // Column 2
                   0.0, 0.0,   (f+n)/(n-f), -1.0,   // Column 3
                   0.0, 0.0, (2*f*n)/(n-f),  0.0)   // Column 4
}

AND glRadius2(FLT x, FLT y) = VALOF
{ LET FLT a = x*x + y*y
  RESULTIS sys(Sys_flt, fl_sqrt, a)
}

AND glRadius3(FLT x, FLT y, FLT z) = VALOF
{ LET FLT a = x*x + y*y + z*z
  RESULTIS sys(Sys_flt, fl_sqrt, a)
}

AND tok2str(tok) = VALOF SWITCHON tok INTO
{ DEFAULT:      RESULTIS "?"
  CASE s_vs:    RESULTIS "vs"
  CASE s_v:     RESULTIS "v"
  CASE s_is:    RESULTIS "is"
  CASE s_ds:    RESULTIS "ds"
  CASE s_d:     RESULTIS "d"
  CASE s_rgb:   RESULTIS "rgb"
  CASE s_kd:    RESULTIS "kd"
  CASE s_num:   RESULTIS "num"
  CASE s_eof:   RESULTIS "eof"
}

AND getevent() = VALOF
{ //writef("gl: Calling sys(Sys_sdl, GL_pollevent...)*n")
  RESULTIS sys(Sys_gl, GL_pollevent, @eventtype)
}

AND error(mes, a, b, c) BE
{ // For error found by loadmodel.
  writef("ERROR nearline %n: ", lineno)
  writef(mes, a, b, c)
  newline()
}

AND rdnum() = VALOF
{ LET res = lexval
  UNLESS token=s_num DO error("Number expected")
  lex()
  RESULTIS res
}

AND loadmodel(filename, modelv) = VALOF
{ // This function reads a .mdl file specifying the vertices and
  // indices of a model. It returns TRUE if successful.

  // Syntax

  // vs n         the size of the vertex data vector
  // is n         set the size of the index vector
  // rgb r b g    set the current colour
  // kd  k d      set the k and d values
  // v x y z      set a vertex using current colour and kd values
  // n            set another index vector element
  // ds n         Allocate the display items vector. n is the number of
  //              display items required.
  // d mode size offset   set a display item
  //                      where mode   is 1   points
  //                                      2   seperate lines
  //                                      3   line strip
  //                                      4   line loop
  //                                      5   triangles
  //                                      6   triangle strip
  //                                      7   triangle fan
  //                            size   is the number of index value belonging
  //                                   to this display item.
  //                      and   offset is a subscript of the index vector
  // z            end of file
  
  // loadmodel updates

  // modelv!0 to point to the vertex data.
  // modelv!1 the number of values in the vertex data.
  // modelv!2 to point to the index vector of 16-bit values.
  // modelv!3 the number of 16-bit values in the index vector.
  // modelv!4 to point to the display items vector.
  // modelv!5 the number of words in the display items vector.

  LET res = TRUE
  LET stdin = input()
  LET instream = findinput(filename)

  // Once an item type has been chosen its values are given as a sequence
  // of vertices, until another item type is given. If the previous item type
  // was greater than zero, its length is filled in.

  LET curr_r, curr_g, curr_b = 0.0, 0.0, 0.0
  LET curr_k, curr_d = 0.0, 0.0

  // Declare self expanding vectors for the
  LET vvec, vvecupb = 0, 0  // vertices, each vertex is [x,y,z, r,g,b, k,d]
  LET ivec, ivecupb = 0, 0  // index vector
  LET dvec, dvecupb = 0, 0  // display items, each item is [mode, i, size]
  LET vpos = 0              // current position in the Vertex vector.
  LET ipos = 0              // current position in the Index vector.
  LET dpos = 0              // current position in the Display items vector.

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

    CASE s_vs:          // Set vvecupb and allocate space
      lex()
      vvecupb := FIX rdnum()
//writef("lineno=%i3 token=%s lexval=%13.3f*n", lineno, tok2str(token), lexval)
writef("vs: vvecupb=%n*n", vvecupb)
//abort(1000)
      vvec := getvec(vvecupb)
      UNLESS vvec DO
      { writef("Unable to allocate vvec with upb=%n*n", vvecupb)
        RESULTIS FALSE
      }
      LOOP
 
    CASE s_v:           // A vertex
    { LET val = ?
      IF vvec=0 DO
      { writef("Vertex given before the vector is allocated*n")
        abort(999)
      }
      IF vpos+8>vvecupb DO
      { writef("Too many vvec items  vpos=%n vvecupb=%n*n", vpos, vvecupb)
        abort(999)
      }
      lex()
      val := rdnum()
      vpos := push32(vvec, vpos, vvecupb, val)     // x
      val := rdnum()
      vpos := push32(vvec, vpos, vvecupb, val)     // y
      val := rdnum()
      vpos := push32(vvec, vpos, vvecupb, val)     // z

      vpos := push32(vvec, vpos, vvecupb, curr_r)  // curr_r
      vpos := push32(vvec, vpos, vvecupb, curr_g)  // curr_g
      vpos := push32(vvec, vpos, vvecupb, curr_b)  // curr_b

      vpos := push32(vvec, vpos, vvecupb, curr_k)  // curr_k
      vpos := push32(vvec, vpos, vvecupb, curr_d)  // curr_d
//writef("v:")
//FOR i = -8 TO -1 DO writef(" %10.3f", vvec!(i+vpos))
//newline()
//abort(1000)
      LOOP
    }

    CASE s_is:          // Set ivec upb and allocate space
      lex()
      ivecupb := FIX rdnum()
//writef("i: ivecupb=%n*n", ivecupb)
      ivec := getvec(ivecupb/2)
      UNLESS ivec DO
      { writef("Unable to allocate ivec with upb=%n*n", ivecupb)
        RESULTIS FALSE
      }
//abort(1000)
      LOOP

    CASE s_ds:          // Set dvec size and allocate its vector
      lex()
      dvecupb := 3 * FIX rdnum()
//writef("d: dvecupb=%n*n", dvecupb)
      dvec := getvec(dvecupb)
      UNLESS dvec DO
      { writef("Unable to allocate dvec with upb=%n*n", dvecupb)
        RESULTIS FALSE
      }
      FOR i = 0 TO dvecupb DO dvec!i := 0 // Clear the dvec vector
//abort(1000)
      LOOP

    CASE s_d:          // Set a display vector item
      UNLESS dvec DO
      { writef("Display item given before the vector is allocated*n")
        abort(999)
      }
      IF dpos+3 > dvecupb DO
      { error("Too many dvec items, dpos=%n dvecupb=%n*n", dpos, dvecupb)
        abort(999)
      }
      lex()
      dvec!dpos := FIX rdnum()         // mode
      dpos := dpos+1
      dvec!dpos := FIX rdnum()         // n
      dpos := dpos+1
      dvec!dpos := FIX rdnum()         // offset
      dpos := dpos+1
      LOOP

    CASE s_rgb:        // Set the current colour
      lex()
      curr_r := rdnum()
      curr_g := rdnum()
      curr_b := rdnum()
//writef("rgb: %8.3f %8.3f %8.3f*n", curr_r, curr_g, curr_b)
//abort(1000)
      LOOP

    CASE s_kd:         // Set the current kd values
      lex()
      curr_k := rdnum()
      curr_d := rdnum()
//writef("kd: %8.3f %8.3f*n", curr_k, curr_d)
//abort(1000)
      LOOP

    CASE s_num:       // An index vector value
    { LET val = 0
      UNLESS ivec DO
      { writef("Index value given before the index vector is allocate*n")
        abort(999)
      }
      IF ipos>ivecupb DO
      { writef("Too many index values, ipos=%n ivecupb=%n*n", ipos, ivecupb)
        abort(999)
      }
      val := FIX rdnum()
      ipos := push16(ivec, ipos, ivecupb, val)
      LOOP
    }

    CASE s_eof:
      //writef("eof*n")
      token := s_eof
      ENDCASE
  }

  UNLESS vvec & ivec & dvec DO
  { error("One or more of vvec, ivec or dvec is missing")
    res := FALSE
    GOTO ret
  }
   
  modelv!0, modelv!1 := vvec, vvecupb
  modelv!2, modelv!3 := ivec, ivecupb
  modelv!4, modelv!5 := dvec, dvecupb
 
ret:
  IF instream DO endstream(instream)
  selectinput(stdin)
//writef("Returning from loadmodel*n")
  RESULTIS res
}

AND checkfor(tok, mess) BE
{ UNLESS token=tok DO
  { writef("ERROR: %s token=%s tok=%s", mess, tok2str(token), tok2str(tok))
  }
  lex()
}

AND lex() BE
{ SWITCHON ch INTO
  { DEFAULT:
      error("line %n: Bad character '%c' in model file", lineno, ch)
      ch := rdch()
      LOOP

    CASE 'z':            // A debugging aid
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
      token := s_v       // A vertex
      ch := rdch()
      IF ch='s' DO       // Upb of vvec
      { token := s_vs
        ch := rdch()
      }
      RETURN

    CASE 'i':            // Size of index vector.
      ch := rdch()
      UNLESS ch='s' DO error("Bad 'is' statement")
      token := s_is
      ch := rdch()
      RETURN

    CASE 'd':
      token := s_d       // A diplay item
      ch := rdch()
      IF ch='s' DO
      { token := s_ds    // Number of the display items
        ch := rdch()
      }
      RETURN

    CASE 'r':
      ch := rdch()
      IF ch='g' DO
      { ch := rdch()
        IF ch='b' DO
        { token := s_rgb  // rgb
          ch := rdch()
          RETURN
        }
      }
      error("Bad 'rgb' token")
      LOOP

    CASE 'k':
      ch := rdch()
      IF ch='d' DO
      { token := s_kd     // kd
        ch := rdch()
        RETURN
      }
      writef("Bad 'kd' token")
      LOOP

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

AND push32(v, i, upb, val) = VALOF
{ IF i > upb DO
  { error("Unable to push a 32-bit value, upb=%n", upb)
    RESULTIS i+1
  }
  v!i := val
  RESULTIS i+1
}

AND push16(v, i, upb, val) = VALOF
{ IF i > upb DO
  { error("Unable to push a 16-bit value, upb=%n", upb)
    RESULTIS i+1
  }
  put16(v, i, val)
  RESULTIS i+1
}

AND get16(v, i) = VALOF
{ LET w = 0
  LET p = 2*i
  LET a, b = v%p, v%(p+1)
  (@w)%0 := 1
  TEST (w & 1) = 0
  THEN RESULTIS (a<<8) + b // Big ender m/c 
  ELSE RESULTIS (b<<8) + a // Little ender m/c 
}

AND put16(v, i, val) BE
{ LET w = 0
  LET p = 2*i
  LET a, b = val&255, (val>>8) & 255
  (@w)%0 := 1
  TEST (w & 1) = 0
  THEN v%p, v%(p+1) := b, a // Big ender m/c 
  ELSE v%p, v%(p+1) := a, b // Little ender m/c 
}

/*
// The following functions will be available when I discover
// how to implement drawpoint under OpenGL.

AND drawch(ch) BE TEST ch='*n'
THEN { currx, curry := 10, curry-14
     }
ELSE { FOR line = 0 TO 11 DO
         write_ch_slice(currx, curry+11-line, ch, line)
       currx := currx+9
     }

AND write_ch_slice(x, y, ch, line) BE
{ // Writes the horizontal slice of the given character.
  // Character are 8x12
  LET cx, cy = currx, curry
  LET i = (ch&#x7F) - '*s'
  // 3*i = subscript of the character in the following table.
  LET charbase = TABLE // Still under development !!!
         #X00000000, #X00000000, #X00000000, // space
         #X18181818, #X18180018, #X18000000, // !
         #X66666600, #X00000000, #X00000000, // "
         #X6666FFFF, #X66FFFF66, #X66000000, // #
         #X7EFFD8FE, #X7F1B1BFF, #X7E000000, // $
         #X06666C0C, #X18303666, #X60000000, // %
         #X3078C8C8, #X7276DCCC, #X76000000, // &
         #X18181800, #X00000000, #X00000000, // '
         #X18306060, #X60606030, #X18000000, // (
         #X180C0606, #X0606060C, #X18000000, // )
         #X00009254, #X38FE3854, #X92000000, // *
         #X00000018, #X187E7E18, #X18000000, // +
         #X00000000, #X00001818, #X08100000, // ,
         #X00000000, #X007E7E00, #X00000000, // -
         #X00000000, #X00000018, #X18000000, // .
         #X06060C0C, #X18183030, #X60600000, // /
         #X386CC6C6, #XC6C6C66C, #X38000000, // 0
         #X18387818, #X18181818, #X18000000, // 1
         #X3C7E6206, #X0C18307E, #X7E000000, // 2
         #X3C6E4606, #X1C06466E, #X3C000000, // 3
         #X1C3C3C6C, #XCCFFFF0C, #X0C000000, // 4
         #X7E7E6060, #X7C0E466E, #X3C000000, // 5
         #X3C7E6060, #X7C66667E, #X3C000000, // 6
         #X7E7E0606, #X0C183060, #X40000000, // 7
         #X3C666666, #X3C666666, #X3C000000, // 8
         #X3C666666, #X3E060666, #X3C000000, // 9
         #X00001818, #X00001818, #X00000000, // :
         #X00001818, #X00001818, #X08100000, // ;
         #X00060C18, #X30603018, #X0C060000, // <
         #X00000000, #X7C007C00, #X00000000, // =
         #X00603018, #X0C060C18, #X30600000, // >
         #X3C7E0606, #X0C181800, #X18180000, // ?
         #X7E819DA5, #XA5A59F80, #X7F000000, // @
         #X3C7EC3C3, #XFFFFC3C3, #XC3000000, // A
         #XFEFFC3FE, #XFEC3C3FF, #XFE000000, // B
         #X3E7FC3C0, #XC0C0C37F, #X3E000000, // C
         #XFCFEC3C3, #XC3C3C3FE, #XFC000000, // D
         #XFFFFC0FC, #XFCC0C0FF, #XFF000000, // E
         #XFFFFC0FC, #XFCC0C0C0, #XC0000000, // F
         #X3E7FE1C0, #XCFCFE3FF, #X7E000000, // G
         #XC3C3C3FF, #XFFC3C3C3, #XC3000000, // H
         #X18181818, #X18181818, #X18000000, // I
         #X7F7F0C0C, #X0C0CCCFC, #X78000000, // J
         #XC2C6CCD8, #XF0F8CCC6, #XC2000000, // K
         #XC0C0C0C0, #XC0C0C0FE, #XFE000000, // L
         #X81C3E7FF, #XDBC3C3C3, #XC3000000, // M
         #X83C3E3F3, #XDBCFC7C3, #XC1000000, // N
         #X7EFFC3C3, #XC3C3C3FF, #X7E000000, // O
         #XFEFFC3C3, #XFFFEC0C0, #XC0000000, // P
         #X7EFFC3C3, #XDBCFC7FE, #X7D000000, // Q
         #XFEFFC3C3, #XFFFECCC6, #XC3000000, // R
         #X7EC3C0C0, #X7E0303C3, #X7E000000, // S
         #XFFFF1818, #X18181818, #X18000000, // T
         #XC3C3C3C3, #XC3C3C37E, #X3C000000, // U
         #X81C3C366, #X663C3C18, #X18000000, // V
         #XC3C3C3C3, #XDBFFE7C3, #X81000000, // W
         #XC3C3663C, #X183C66C3, #XC3000000, // X
         #XC3C36666, #X3C3C1818, #X18000000, // Y
         #XFFFF060C, #X183060FF, #XFF000000, // Z
         #X78786060, #X60606060, #X78780000, // [
         #X60603030, #X18180C0C, #X06060000, // \
         #X1E1E0606, #X06060606, #X1E1E0000, // ]
         #X10284400, #X00000000, #X00000000, // ^
         #X00000000, #X00000000, #X00FFFF00, // _
         #X30180C00, #X00000000, #X00000000, // `
         #X00007AFE, #XC6C6C6FE, #X7B000000, // a
         #XC0C0DCFE, #XC6C6C6FE, #XDC000000, // b
         #X00007CFE, #XC6C0C6FE, #X7C000000, // c
         #X060676FE, #XC6C6C6FE, #X76000000, // d
         #X00007CFE, #XC6FCC0FE, #X7C000000, // e
         #X000078FC, #XC0F0F0C0, #XC0000000, // f
         #X000076FE, #XC6C6C6FE, #X7606FE7C, // g
         #XC0C0DCFE, #XC6C6C6C6, #XC6000000, // h
         #X18180018, #X18181818, #X18000000, // i
         #X0C0C000C, #X0C0C0C7C, #X38000000, // j
         #X00C0C6CC, #XD8F0F8CC, #XC6000000, // k
         #X00606060, #X6060607C, #X38000000, // l
         #X00006CFE, #XD6D6D6D6, #XD6000000, // m
         #X0000DCFE, #XC6C6C6C6, #XC6000000, // n
         #X00007CFE, #XC6C6C6FE, #X7C000000, // o
         #X00007CFE, #XC6FEFCC0, #XC0000000, // p
         #X00007CFE, #XC6FE7E06, #X06000000, // q
         #X0000DCFE, #XC6C0C0C0, #XC0000000, // r
         #X00007CFE, #XC07C06FE, #X7C000000, // s
         #X0060F8F8, #X6060607C, #X38000000, // t
         #X0000C6C6, #XC6C6C6FE, #X7C000000, // u
         #X0000C6C6, #X6C6C6C38, #X10000000, // v
         #X0000D6D6, #XD6D6D6FE, #X6C000000, // w
         #X0000C6C6, #X6C386CC6, #XC6000000, // x
         #X0000C6C6, #XC6C6C67E, #X7606FE7C, // y
         #X00007EFE, #X0C3860FE, #XFC000000, // z
         #X0C181808, #X18301808, #X18180C00, // {
         #X18181818, #X18181818, #X18181800, // |
         #X30181810, #X180C1810, #X18183000, // }
         #X00000070, #XD1998B0E, #X00000000, // ~
         #XAA55AA55, #XAA55AA55, #XAA55AA55  // rubout

  IF i>=0 DO charbase := charbase + 3*i

  // charbase points to the three words giving the
  // pixels of the character.
  { LET col = colour
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

    IF ((w >> 7) & 1) = 1 DO drawpoint(x,   y)
    IF ((w >> 6) & 1) = 1 DO drawpoint(x+1, y)
    IF ((w >> 5) & 1) = 1 DO drawpoint(x+2, y)
    IF ((w >> 4) & 1) = 1 DO drawpoint(x+3, y)
    IF ((w >> 3) & 1) = 1 DO drawpoint(x+4, y)
    IF ((w >> 2) & 1) = 1 DO drawpoint(x+5, y)
    IF ((w >> 1) & 1) = 1 DO drawpoint(x+6, y)
    IF (w & 1)        = 1 DO drawpoint(x+7, y)

//writef("writeslice: ch=%c line=%i2 w=%b8 bits=%x8 %x8 %x8*n",
//        ch, line, w, charbase!0, charbase!1, charbase!2)

  }

  currx, curry := cx, cy
}

AND drawstring(x, y, s) BE
{ moveto(x, y)
  FOR i = 1 TO s%0 DO drawch(s%i)
}

AND plotf(x, y, form, a, b, c, d, e, f, g, h) BE
{ LET oldwrch = wrch
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
*/

