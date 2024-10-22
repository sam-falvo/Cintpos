/*
############### UNDER DEVELOPMENT #####################

This library provides some functions that interface with the SDL
Graphics libary.

Implemented by Martin Richards (c) May 2014

Change history:

12/03/2018
Modified the 3D functions to use floating point for the depth.

26/08/12
Initial implementation.

15/07/13
Started adding OpenGL functions.


It should typically be included as a separate section for programs that
need it. Such programs typically have the following structure.

GET "libhdr"
MANIFEST { g_sdlbase=nnn  } // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "sdl.h"
GET "sdl.b"                 // Insert the library source code
.
GET "libhdr"
MANIFEST { g_sdlbase=nnn  } // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "sdl.h"
Rest of the program
 
*/

LET inprod(v1, v2) = VALOF
{ LET FLT x1, FLT y1, FLT z1 = v1!0, v1!1, v1!2
  LET FLT x2, FLT y2, FLT z2 = v2!0, v2!1, v2!2
  RESULTIS x1*x2 + y1*y2 + z1*z2
}

LET crossprod(v1, v2, v3) BE
{ LET FLT x1, FLT y1, FLT z1 = v1!0, v1!1, v1!2
  LET FLT x2, FLT y2, FLT z2 = v2!0, v2!1, v2!2
  v3!0 := y1*z2 - z1*y2
  v3!1 := z1*x2 - x1*z2
  v3!2 := x1*y2 - y1*x2
}

LET standardize(v) BE
{ LET FLT x, FLT y, FLT z = v!0, v!1, v!2
  LET FLT r = sys(Sys_flt, fl_sqrt, x*x+y*y+z*z)
  UNLESS r < 1e-10 DO
  { v!0 := x/r
    v!1 := y/r
    v!2 := z/r
  }
}

LET initsdl() = VALOF
{ LET mes = VEC 256/bytesperword

  IF sys(Sys_sdl, sdl_init, sdl_init_everything) DO
  { sys(Sys_sdl, sdl_geterror, mes)
    writef("Unable to initialise SDL: %s*n", mes)
    RESULTIS FALSE
  }

//  writef("Number of joysticks %2i*n", sys(Sys_sdl, sdl_numjoysticks))
  joystick := sys(Sys_sdl, sdl_joystickopen, 0)
//  writef("Number of axes      %2i*n", sys(Sys_sdl, sdl_joysticknumaxes, joystick))
//  writef("Number of buttons   %2i*n", sys(Sys_sdl, sdl_joysticknumbuttons, joystick))

  lefts,  rights  := 0, 0
  leftds, rightds := 0, 0
  depthscreen := 0

  // Successful
  RESULTIS TRUE
}

AND mkscreen(title, xsize, ysize) = VALOF
{ // Create a screen surface with given title and size
  LET mes = VEC 256/bytesperword
  mes%0 := 0

  screenxsize,  screenysize  := xsize, ysize
  fscreenxsize, fscreenysize := FLOAT xsize, FLOAT ysize

  screen := sys(Sys_sdl, sdl_setvideomode, screenxsize, screenysize, 32, sdl_SWSURFACE)

  UNLESS screen DO
  { sys(Sys_sdl, sdl_geterror, mes)
    writef("Unable to set video mode: %s*n", mes)
    RESULTIS 0
  }

  { // Surface info structure
    LET flags, fmt, w, h, pitch, pixels, cliprect, refcount =
            0,   0, 0, 0,     0,      0,        0,        0
    sys(Sys_sdl, sdl_getsurfaceinfo, screen, @flags)

    format := fmt
  }

  setcaption(title)
  selectsurface(screen, xsize, ysize)
}

AND maprgb(r, g, b) = sys(Sys_sdl, sdl_maprgb, format, r, g, b)

AND setcaption(title) BE sys(Sys_sdl, sdl_wm_setcaption, title, 0)

AND closesdl() BE
{ IF lefts       DO freevec(lefts)
  IF rights      DO freevec(rights)
  IF leftds      DO freevec(leftds)
  IF rightds     DO freevec(rightds)
  IF depthscreen DO freevec(depthscreen)
  sys(Sys_sdl, sdl_quit)
}

AND setcolour(col) BE
  currcolour, prevdrawn := col, FALSE

AND setcolourkey(surf, col) BE
  sys(Sys_sdl, sdl_setcolourkey, surf, col)

AND selectsurface(surf, xsize, ysize) BE
  currsurf, currxsize, currysize := surf, xsize, ysize


AND moveto(x, y) BE
  currx, curry, prevdrawn := x, y, FALSE

AND moveto3d(x, y, FLT z) BE
  currx, curry, currz, prevdrawn := x, y, z, FALSE

AND drawpoint(x, y) BE
{ // (0, 0) is the bottom left point on the surface
//writef("drawpoint: x=%i3  currx=%i3 currxsize=%i3  prevdrawn=%n*n", x, currx, currxsize, prevdrawn)
//writef("drawpoint: y=%i3  curry=%i3 currysize=%i3*n", y, curry, currysize)
  //UNLESS prevdrawn
  //UNLESS x=currx & y=curry 
  IF 0<=x<currxsize & 0<=y<currysize DO
  { sys(Sys_sdl, sdl_fillrect, currsurf, x, currysize-y, 1, 1, currcolour)
//writef("Drawpoint: drawing point at x=%i3  y=%i3*n", x, y)
  }
  currx, curry, prevdrawn := x, y, TRUE
//updatescreen()
//abort(1003)
}

AND drawpoint3d(x, y, FLT z) BE
{ // (0, 0) is the bottom left point on the surface
  // z is more negative as the depth into the screen increases.
//IF y<2 DO writef("drawpoint3d: (%i3,%i3,%9.2f)*n", x,y,z)
  IF 0<=x<currxsize & 0<=y<currysize UNLESS x=currx &
                                            y=curry &
                                            z=currz & FALSE & // ??????
                                            prevdrawn DO
  { // (x,y,z) is a point on the screen and not previously drawn.
    LET p = @(depthscreen!(x+y*currxsize))
    IF z >= !p DO
    { // This point is less deep than the previous point, if any,
      // at pixel location (x,y), so draw it.
      !p := z
      sys(Sys_sdl, sdl_fillrect, currsurf, x, currysize-y, 1, 1, currcolour)
    }
  }
  currx, curry, currz, prevdrawn := x, y, z, TRUE
}

AND moveby(dx, dy) BE moveto(currx+dx, curry+dy)
AND drawby(dx, dy) BE drawto(currx+dx, curry+dy)

AND moveby3d(dx, dy, FLT dz) BE moveto3d(currx+dx, curry+dy, currz+dz)
AND drawby3d(dx, dy, FLT dz) BE drawto3d(currx+dx, curry+dy, currz+dz)


AND getevent() = VALOF
{ //writef("Calling pollevent*n")
  RESULTIS sys(Sys_sdl, sdl_pollevent, @eventtype)
}

AND sdldelay(msecs) BE // Delay using the SDL delay mechanism
  sys(Sys_sdl, sdl_delay, msecs)


AND sdlmsecs() =     // returns msecs since start of run
  sys(Sys_sdl, sdl_getticks)


AND hidecursor() = sys(Sys_sdl, sdl_hidecursor)

AND showcursor() = sys(Sys_sdl, sdl_showcursor)

AND updatescreen() BE  // Display the screen
  sys(Sys_sdl, sdl_flip, screen)

AND mksurface(w, h) = VALOF
{ //writef("mksurface: w=%n h=%n*n", w, h)
  RESULTIS sys(Sys_sdl, sdl_mksurface, format, w, h)
}

AND freesurface(surf) BE sys(Sys_sdl, sdl_freesurface, surf)

AND blitsurf(src, dst, x, y) BE
{ // Blit the source surface to the specified position
  // in the destination surface
  LET dx, dy, dw, dh = x, currysize-y-1, 0, 0
  sys(Sys_sdl, sdl_blitsurface, src, 0, dst, @dx)
}

AND blitsurfrect(src, srcrect, dst, x, y) BE
{ // Blit the specified rectangle from the source surface to
  // the specified position in the destination surface
  LET dx, dy, dw, dh = x, currysize-y-1, 0, 0
  sys(Sys_sdl, sdl_blitsurface, src, srcrect, dst, @dx)
}

AND fillsurf(col) BE
  sys(Sys_sdl, sdl_fillsurf, currsurf, col)

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

AND drawto(x, y) BE
{ // This is Bresenham's algorithm to draw a line in 2D.
  LET dx  = ABS(x-currx)
  AND dy  = ABS(y-curry)
  LET sx  = currx < x -> 1, -1
  LET sy  = curry < y -> 1, -1
  LET err = dx-dy
  LET e2  = ?
//writef("*nDrawto: currx=%i4 curry=%i4  x=%i4 y=%i4*n", currx, curry, x, y)

  { drawpoint(currx, curry)
//updatescreen()
//writef("drawto: currx=%i4 curry=%i4*n", currx, curry)
    IF currx=x & curry=y RETURN
    prevdrawn := FALSE // About to change currx and/or curry
//writef("drawto: currx=%i4 curry=%i4  x=%i4 y=%i4*n", currx, curry, x, y)
    e2 := 2*err
    IF e2 > -dy DO
    { err := err - dy
      currx := currx + sx
    }
    IF e2 < dx DO
    { err := err + dx
      curry := curry + sy
    }
  } REPEAT
}

AND drawto3d(x, y, FLT z) BE
{ // This is Bresenham's algorithm to draw a line in 3D.
  LET dx = ABS(x-currx)
  AND dy = ABS(y-curry)
  LET sx = currx<x -> 1, -1
  LET sy = curry<y -> 1, -1
  LET py = curry<y -> currxsize, -currxsize
  LET x0, y0, FLT z0 = currx, curry, currz
  LET err = dx-dy
  LET e2 = ?

  // Allocate the depth screen if necessary.
  UNLESS depthscreen DO
  { LET maxdepth = -1e10
    depthscreen := getvec(currxsize*currysize-1)
    FOR i = 0 TO currxsize*currysize-1 DO
      depthscreen!i := maxdepth
  }

//IF y<0 DO
//{ writef("drawto3d: x=%n  y=%n  z=%n*n", x,y,z)
//  abort(1237)
//}
  { drawpoint3d(currx,curry,currz)
//writef("drawto3d: x=%n  y=%n  z=%n*n", currx,curry,currz)
    IF currx=x & curry=y RETURN
    e2 := 2*err
    IF e2 > -dy DO
    { err := err - dy
      currx := currx+sx
    }
    IF e2 < dx DO
    { err := err + dx
      curry := curry + sy
    }
    TEST dx>=dy
    THEN currz := z0 + (z-z0) * FLOAT (currx-x0) / FLOAT (x-x0)
    ELSE currz := z0 + (z-z0) * FLOAT (curry-y0) / FLOAT (y-y0)
  } REPEAT
}

AND setlims(x, y) BE
{ // This is used by drawtriangle and drawquad.
  // It is based on Bresenham's algorithm.
  // This function sets elements of lefts and rights
  // to the smallest and largest values of x for each y
  // when the line from (currx,curry) to (x,y) is drawn.
  // lefts and rights both range from 0 to currysize-1.
  // It may change miny and maxy.
  LET dx = ABS(x-currx)
  AND dy = ABS(y-curry)
  LET sx = currx<x -> 1, -1
  LET sy = curry<y -> 1, -1
  LET err = dx-dy

  IF curry<miny DO miny := curry
  IF curry>maxy DO maxy := curry

//writef("Setlims: currx=%i3 curry=%i3 x=%i3 y=%i3 miny=%i3  maxy=%i3*n",
//        currx, curry, x, y, miny, maxy)
//abort(3456)
  { LET e2 = 2*err

//writef("setlims: currysize=%i3*n", currysize)
//  FOR i = 0 TO currysize-1 UNLESS lefts!i=maxint DO
//    writef("%i3: left=%i3 right=%i3*n", i, lefts!i, rights!i)
//writef("currx=%i3 curry=%i3 left=%x8 right=%x8*n",
//        currx, curry, lefts!curry, rights!curry)

    IF 0 <= curry < currysize DO
    { IF currx< lefts!curry DO lefts !curry := currx
      IF currx>rights!curry DO rights!curry := currx
    }

//  FOR i = 0 TO currysize-1 UNLESS lefts!i=maxint DO
//    writef("%i3: left=%i3 right=%i3*n", i, lefts!i, rights!i)
//writef("x=%i3 y=%i3  currx=%i3 curry=%i3 left=%i3 right=%i3*n",
//        x, y, currx, curry, lefts!curry, rights!curry)

    IF currx=x & curry=y RETURN

    IF e2 > -dy DO
    { err := err - dy
      currx := currx + sx
    }
    IF e2 < dx DO
    { err := err + dx
      curry := curry + sy
    }
  } REPEAT
}

AND alloc2dvecs() BE
{ UNLESS lefts DO
  { lefts  := getvec(currysize-1)
    rights := getvec(currysize-1)
//writef("alloc2dvecs: lefts=%n rights=%n*n", lefts, rights)
    UNLESS lefts & rights DO
    { sawritef("Unable to allocate lefts and rights, currysize=%i3*n",
                currysize)
      abort(999)
    }
  }
//writef("alloc2dvecs: lefts=%n rights=%n*n", lefts, rights)
  FOR i = 0 TO currysize-1 DO
    lefts!i, rights!i := maxint, minint
}

AND drawquad(x1,y1, x2,y2, x3,y3, x4,y4) BE
{ alloc2dvecs()
//writef("drawquad: (%i3,%i3) (%i3,%i3) (%i3,%i3) (%i3,%i3)*n", x1,y1, x2,y2, x3,y3, x4,y4)

  miny, maxy := currysize, -1

//writef("drawquad: maxint=%x8 minint=%x8*n", maxint, minint)
//  FOR i = 0 TO currysize-1 UNLESS lefts!i=maxint DO
//    writef("%i3: left=%i3 right=%i3*n", i, lefts!i, rights!i)

  moveto(x1,y1)
  setlims(x2,y2)
//FOR y = miny TO maxy DO
//  writef("%i3: left=%i3 right=%i3*n", y, lefts!y, rights!y)
  setlims(x3,y3)
  setlims(x4,y4)
  setlims(x1,y1)
//FOR y = miny TO maxy DO
//  writef("%i3: left=%i3 right=%i3*n", y, lefts!y, rights!y)

//writef("drawquad: miny=%i3 maxy=%i3*n", miny, maxy)
//abort(1001)

  IF miny<0 DO miny := 0
  IF maxy>=currysize DO maxy := currysize-1

  FOR y = miny TO maxy DO
  { LET minx = lefts!y
    LET maxx = rights!y
    IF minx<0 DO minx := 0
    IF maxx>=currxsize DO maxx := currxsize-1

//IF minx<10 DO writef("drawquad:moveto %i3 %i3*n", minx, y)
    moveto(minx, y)
//IF maxx<10 DO writef("drawquad:drawto %i3 %i3*n", maxx, y)
    drawto(maxx, y)
    lefts!y, rights!y := maxint, minint
  }
  moveto(x1,y1)
}

AND drawtriangle(x1,y1, x2,y2, x3,y3) BE
{ alloc2dvecs()

  miny, maxy := maxint, minint

  moveto(x1,y1)
  setlims(x2,y2)
  setlims(x3,y3)
  setlims(x1,y1)

  FOR y = miny TO maxy DO
  { moveto(lefts!y, y)
    drawto(rights!y, y)
    lefts!y, rights!y := maxint, minint
  }
  moveto(x1,y1)
}

AND setlims3d(x, y, FLT z) BE
{ // This is used by drawtriangle3d and drawquad3d
  // It is based on Bresenham's algorithm
  LET dx = ABS(x-currx)
  AND dy = ABS(y-curry)
  LET x0, y0, FLT z0 = currx, curry, currz
  LET sx = currx<x -> 1, -1
  LET sy = curry<y -> 1, -1
  LET err = dx-dy

  { LET e2 = 2*err

    IF 0<=curry<currysize DO
    { IF curry<miny DO miny := curry
      IF curry>maxy DO maxy := curry


      IF currx <= lefts!curry DO
      { lefts!curry := currx
        //IF leftds!curry > currz DO  // Bug???
          leftds!curry := currz
      }
      IF currx >= rights!curry DO
      { rights!curry := currx
        //IF rightds!curry > currz DO  // Bug???
          rightds!curry := currz
      }
    }
    IF currx=x & curry=y RETURN

    IF e2 > -dy DO
    { err := err - dy
      currx := currx + sx
      IF dx>=dy DO
      { currz := z0 + (z-z0) * FLOAT (currx-x0) / FLOAT (x-x0)
      }
    }
    IF e2 < dx DO
    { err := err + dx
      curry := curry + sy
      IF dy>dx DO
      { currz := z0 + (z-z0) * FLOAT (curry-y0) / FLOAT (y-y0)
      }
    }
  } REPEAT
}

AND alloc3dvecs() BE
{ UNLESS lefts DO
  { lefts  := getvec(currysize-1)
    rights := getvec(currysize-1)
    FOR y = 0 TO currysize-1 DO
      lefts!y, rights!y := maxint, minint
  }

  UNLESS leftds DO
  { leftds  := getvec(currysize-1)
    rightds := getvec(currysize-1)
    FOR y = 0 TO currysize-1 DO
      leftds!y, rightds!y := maxint, maxint
  }

  UNLESS depthscreen DO
  { depthscreen := getvec(currxsize*currysize-1)
    FOR i = 0 TO currxsize*currysize-1 DO
      depthscreen!i := 1e10
  }
}

AND drawquad3d(x1, y1, FLT z1,
               x2, y2, FLT z2,
               x3, y3, FLT z3,
               x4, y4, FLT z4) BE
{ // Draw a filled convex quadralateral
  // The points are assumed to be coplanar or nearly so.
  // Note that the depth components are floating point
  // The x-y coordinates are integer screen coordinates.
  alloc3dvecs()

  miny, maxy := maxint, minint

  moveto3d (x1,y1,z1)
  setlims3d(x2,y2,z2)
  setlims3d(x3,y3,z3)
  setlims3d(x4,y4,z4)
  setlims3d(x1,y1,z1)

  FOR y = miny TO maxy DO
  { moveto3d( lefts!y, y, leftds!y)
    drawto3d(rights!y, y, rightds!y)

    lefts!y,  rights!y  := maxint, minint
    leftds!y, rightds!y := maxint, maxint
  }
  moveto3d(x1,y1,z1)
}

AND drawtriangle3d(x1, y1, FLT z1,
                   x2, y2, FLT z2,
                   x3, y3, FLT z3) BE
{ // Note that the depth components are floating point
  // The x-y coordinates are integer screen coordinates.
  alloc3dvecs()

  miny, maxy := maxint, minint

  moveto3d (x1,y1,z1)
  setlims3d(x2,y2,z2)
  setlims3d(x3,y3,z3)
  setlims3d(x1,y1,z1)

  FOR y = miny TO maxy DO
  { moveto3d( lefts!y, y, leftds!y)
    drawto3d(rights!y, y, rightds!y)
    
    lefts!y,  rights!y  := maxint, minint
    leftds!y, rightds!y := 1e10, 1e10
  }
  moveto3d(x1,y1,z1)
}

AND drawrect(x0, y0, x1, y1) BE
{ LET xmin, xmax = x0, x1
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

AND drawfillrect(x0, y0, x1, y1) BE
{ LET xmin, xmax = x0, x1
  LET ymin, ymax = y0, y1
  IF xmin>xmax DO xmin, xmax := x1, x0
  IF ymin>ymax DO ymin, ymax := y1, y0

  sys(Sys_sdl, sdl_fillrect, currsurf,
      xmin, currysize-ymax, xmax-xmin+1, ymax-ymin+1, currcolour)

  currx, curry := x0, y0
}

AND drawroundrect(x0,y0,x1,y1,radius) BE
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

AND drawfillroundrect(x0, y0, x1, y1, radius) BE
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

AND drawcircle(x0, y0, radius) BE
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

AND drawfillcircle1(x, y, radius) BE
{ // (x,y) is the centre
  x := x-radius
  y := y+radius
  IF y<radius DO y := radius
  IF y>=currysize-radius DO y := currysize-radius
  sys(Sys_sdl, sdl_drawfillcircle, currsurf, x, currysize-y, radius, currcolour)
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

AND getmousestate() = VALOF
{ writef("*ngetmousestate: not available*n")
  abort(999)
}
