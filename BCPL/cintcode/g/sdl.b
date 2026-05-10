/*
This library provides some functions that interface with the SDL
Graphics libary.

Implemented by Martin Richards (c) Aug 2020

Change history:

28/07/2025
Added a call to datstamp in sdldelay to update the date and time fields
in the rootnode.

04/06/2021
Started to implement a 18x12 font as an alternative to the 12x8 one.
selectfont(12) or selectfont(18) selects the new font by setting
fontH, fontW and fonttab. I will probably ad a 24x16 font later.

12/09/2019
Modified for 32 and 64 bit BCPL running on 32 or 64 bit
machines.

12/03/2018
Modified the 3D functions to use floating point for the depth.

26/08/2012
Initial implementation.

15/07/2013
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

LET inprod(v, w) = VALOF
{ // Return |v| x |w| x cosine of the angle between v and w.
  LET FLT vx, FLT vy, FLT vz = v!0, v!1, v!2
  LET FLT wx, FLT wy, FLT wz = w!0, w!1, w!2
  RESULTIS vx*wx + vy*wy + vz*wz
}

AND crossprod(v, w, r) BE
{ // Set r to a vector othogonal to v and w and
  // of length |v| x |w| x sine of the angle between v and w.
  LET FLT vx, FLT vy, FLT vz = v!0, v!1, v!2
  LET FLT wx, FLT wy, FLT wz = w!0, w!1, w!2
  r!0 := vy*wz - vz*wy
  r!1 := vz*wx - vx*wz
  r!2 := vx*wy - vy*wx
}

AND standardize(v) BE
{ // Set v to a vector of unit length without changing its direction
  // but if |v| is too small set it to (1.0, 0.0, 0.0).
  LET FLT x, FLT y, FLT z = v!0, v!1, v!2
  LET FLT r = sys(Sys_flt, fl_sqrt, x*x+y*y+z*z)
  TEST r < sys(Sys_flt, fl_mk, 1, -10) //1e-10
  THEN { // Arbitrarily set v to (1,0,0) if r is too small.
         v!0 := 1.0
         v!1 := 0.0
         v!2 := 0.0
       }
  ELSE { v!0 := x/r
         v!1 := y/r
         v!2 := z/r
       }
}

AND distance(p, q) = VALOF
{ LET FLT dx = p!0 #- q!0
  LET FLT dy = p!1 #- q!1
  LET FLT dz = p!2 #- q!2
  RESULTIS sys(Sys_flt, fl_sqrt, dx*dx + dy*dy + dz*dz)
}

AND radius(v) = VALOF
{ // Return |v|
  LET FLT x, FLT y, FLT z = v!0, v!1, v!2
  RESULTIS sys(Sys_flt, fl_sqrt, x*x + y*y + z*z)
}

AND interpolate(p, p1,p2, q1,q2) = VALOF
{ LET res = interpolate1(p, p1,p2, q1,q2)
  //writef("interpolate(%n, %n,%n, %n,%n) => %n*n", p, p1,p2, q1,q2, res)
  RESULTIS res
}

AND interpolate1(p, p1,p2, q1,q2) = VALOF
{ // Return q1 + (q2-q1) * (p-p1) / (p2-p1) avoiding overflow
  // We assume p is between p1 and p2
  // This function uses integer arithmetic.

  IF p1=p2 RESULTIS q1 // An unlikely special case

  // Check that overflow cannot happen
  IF -1_000_000_000 <= p1 <= 1_000_000_000 &
     -1_000_000_000 <= p2 <= 1_000_000_000 &
     -1_000_000_000 <= q1 <= 1_000_000_000 &
     -1_000_000_000 <= q2 <= 1_000_000_000 DO
   { // Overflow cannot occur so use the simple formula.
     // This is the normal case.
     RESULTIS q1 + muldiv(q2-q1, p-p1, p2-p1)
   }
   
  // Overflow is just possible so we must be careful.
  
  // Ensure p1 is less than  p2
  IF p1>p2 RESULTIS interpolate(p, p2,p1, q2,q1)
  
  // p1 < p2
  IF (p1 XOR p2) < 0 |
     (q1 XOR q2) < 0 DO
  { // p1 and p2 have different signs or
    // q1 and q2 have different signs, so overflow is possible.
    // Try halving the interval size.
    LET midp = p1/2 + p2/2
    LET midq = q1/2 + q2/2
    LET res = 0
    //writef("interpolate: halving the interval, midp=%n midq=%n*n", midp, midq)
    TEST p<=midp THEN res := interpolate(  p,     p1,  midp,  q1,  midq)
                 ELSE res := interpolate(p-midp, midp,  p2,  midq,  q2)
//abort(4499)
    RESULTIS res
  }

  // Overflow is not possible so use the simple formula.
  RESULTIS q1 + muldiv(q2-q1, p-p1, p2-p1)
}

AND tstinterpolate(p, p1,p2, q1,q2) BE
{ LET res = interpolate(p, p1,p2, q1,q2)
  writef("interpolate(%n, %n,%n, %n,%n) => %n*n", p, p1,p2, q1,q2, res)
}

LET initsdl() = VALOF
{ // Initialise the interface with the SDL library.
  LET mes = VEC 256/bytesperword
//sawritef("initsdl: entered*n")
  setdepthlimits(1, 1_000_000_000)  // Default settings in pixels

  // Select the default font
  selectfont(12)
  
  IF FALSE DO
  { tstinterpolate(    150,    100,    200,    2000,   3000)
    tstinterpolate(   -150,   -100,   -200,    2000,   3000)
    tstinterpolate(    150,    100,    200,   -2000,  -3000)
    tstinterpolate(   -150,   -100,   -200,   -2000,  -3000)

    tstinterpolate( 1_500_000_000, 1_000_000_000, 2_000_000_000, 2000, 3000)
    tstinterpolate(-1_500_000_000,-1_000_000_000,-2_000_000_000, 2000, 3000)
    tstinterpolate( 1_500_000_000, 1_000_000_000, 2_000_000_000,-2000,-3000)
    tstinterpolate(-1_500_000_000,-1_000_000_000,-2_000_000_000,-2000,-3000)
    RESULTIS FALSE
  }
  
  IF sys(Sys_sdl, sdl_init, sdl_init_everything) DO
  { mes%0 := 0
    sys(Sys_sdl, sdl_geterror, mes)
    writef("Unable to initialise SDL: %s*n", mes)
    RESULTIS FALSE
  }

  //writef("Number of joysticks %2i*n", sys(Sys_sdl, sdl_numjoysticks))
  //sys(Sys_sdl, sdl_joystickopen, 0, @joystick)
  //writef("Number of axes      %2i*n",
  //       sys(Sys_sdl, sdl_joysticknumaxes, @joystick))
  //writef("Number of buttons   %2i*n",
  //        sys(Sys_sdl, sdl_joysticknumbuttons, @joystick))

  // Initialise variables for 2D drawing.
  
  currx, curry := 0, 0

  // Initialise variables for 3D drawing.
  
  currx3d, curry3d, currsz3d := 0, 0, 0
  leftzv, rightzv := 0, 0  // For depth values at the ends of raster lines 

  setdepthlimits(1, 1_000_000_000)  // Default settings in pixels
  depthvupb := currxsize*currysize-1
  depthv := 0                       // depthv is only allocated by mkscreen3D

  maxdepth :=  1_000_000_000        // The scaled maximum scaled integer depth value
  mindepth := -1_000_000_000        // The scaled minimum scaled integer depth value

  // Initialise variables for both 2D and 3D drawing.
  
  leftxv, rightxv := 0, 0 // For vectors holding the  x limits of the raster lines
  miny, maxy := 0, -1
  
  currxsize, currysize := 0, 0 // No screen yet

// Successful
  RESULTIS TRUE
}

AND mkscreen(title, xsize, ysize) = VALOF
{ // Create a screen surface with given title and size and
  // initialise the variables for 2D drawing.
  // If successful it updates screen and screae1 with the machine
  // address used by the SDL C library to represent the screen.
  // The result is TRUE if successful.
  LET ok = ?
  LET mes = VEC 256/bytesperword
  mes%0 := 0
//sawritef("mkscreen: title=*"%s*"   xsize=%n ysize=%n*n*n",
//          title, xsize, ysize)

  screenxsize,  screenysize  := xsize, ysize
  fscreenxsize, fscreenysize := FLOAT xsize, FLOAT ysize

  TEST screenxsize>300 THEN selectfont(18)
                       ELSE selectfont(12)

  depthv    :=  0  // Only needed for 3D drawing
  depthvupb := -1

  ok := sys(Sys_sdl, sdl_setvideomode,
                     screenxsize,
                     screenysize,
	             32,              // Bits per pixel
                     sdl_SWSURFACE,   // In system memory
                     //sdl_HWSURFACE,   // In video memory
                     @screen
                   )

  UNLESS ok DO
  { // Copy the error message into mes as a BCPL string.
    sys(Sys_sdl, sdl_geterror, mes)
    writef("Unable to set video mode: %s*n", mes)
    RESULTIS FALSE
  }

  { // Surface info structure
    LET flags = 0
    LET fmt, fmt1 = 0, 0
    LET w, h, pitch = 0, 0, 0
    LET pixels, pixels1 = 0, 0
    LET cliprectx, cliprecty, cliprectw, cliprecth = 0, 0, 0, 0
    LET refcount = 0

    sys(Sys_sdl, sdl_getsurfaceinfo, @screen, @flags)

    format, format1 := fmt, fmt1
  }

  setcaption(title)
  selectsurface(@screen, xsize, ysize)

  alloc2dvecs()
  
  RESULTIS TRUE
}

AND mkscreen3d(title, xsize, ysize) = VALOF
{ // This create the screen and initialise the variariables for
  // 2D and 3D drawing.
  
  // Initialise 2D drawing first.
  UNLESS mkscreen(title, xsize, ysize) RESULTIS FALSE

// Finally initialise the extra variables for 3D drawing.
  depthvupb := xsize*ysize-1
  alloc3dvecs()

  RESULTIS TRUE
}

AND maprgb(r, g, b) = sys(Sys_sdl, sdl_maprgb, @format, r, g, b)

AND setcaption(title) BE sys(Sys_sdl, sdl_wm_setcaption, title, 0)

AND closesdl() BE
{ IF leftxv  DO freevec(leftxv)
  IF rightxv DO freevec(rightxv)
  IF leftzv  DO freevec(leftzv)
  IF rightzv DO freevec(rightzv)
  IF depthv  DO freevec(depthv)
  sys(Sys_sdl, sdl_quit)
}

AND setcolour(col) BE currcolour := col

AND setcolourkey(surfptr, col) BE
  sys(Sys_sdl, sdl_setcolourkey, surfptr, col)

AND mksurface(w, h, surfptr) = VALOF
{ // Create a new surface with given width and height.
  LET ok = sys(Sys_sdl, sdl_mksurface, @format, w, h, surfptr)
  RESULTIS ok
}

AND freesurface(surfptr) BE sys(Sys_sdl, sdl_freesurface, surfptr)

AND selectsurface(surfptr, xsize, ysize) BE
{
//writef("selectsurface(surfptr, %n, %n) entered*n", xsize, ysize)
  currsurf, currsurf1 := surfptr!0, surfptr!1
  currxsize, currysize := xsize, ysize
  curryupb := currysize-1
}

AND getevent() = VALOF
{ //writef("Calling sdl_pollevent*n")
  RESULTIS sys(Sys_sdl, sdl_pollevent, @eventtype)
}

AND Comploop() BE
{ LET t0 = rtn_msecs!rootnode
  writef("*nComploop ind sdl.b started "); deplete(cos)
  FOR i = 0 TO 50_000_000 DO LOOP
  writef("done after %6.3d secs*n", rtn_msecs!rootnode - t0)
}



AND sdldelay(msecs) BE // Delay using the SDL delay mechanism
{ sys(Sys_sdl, sdl_delay, msecs)
  datstamp(@rtn_days!rootnode)
}

AND sdlmsecs() =     // returns msecs since start of run
  sys(Sys_sdl, sdl_getticks)

AND hidecursor() = sys(Sys_sdl, sdl_hidecursor)

AND showcursor() = sys(Sys_sdl, sdl_showcursor)

AND updatescreen() BE  // Display the screen
  sys(Sys_sdl, sdl_flip, @screen)

AND blitsurf(srcptr, dstptr, x, y) BE
{ // Blit the source surface to the specified position
  // in the destination surface
  //LET dx, dy, dw, dh = x, currysize-y-1, 0, 0
  LET dx, dy, dw, dh = x, screenysize-y-1, 0, 0
  //sawritef("blisurf: calling sdl_blitsurface dx=%n dy=%n*n", dx, dy)
  //sawritef("blisurf: src=%n -> [%n %n]*n", srcptr, srcptr!0, srcptr!1)
  //sawritef("blisurf: dst=%n -> [%n %n]*n", dstptr, dstptr!0, dstptr!1)
  sys(Sys_sdl, sdl_blitsurface, srcptr, 0, dstptr, @dx)
//  abort(5678)
}

AND blitsurfrect(srcptr, srcrect, dstptr, x, y) BE
{ // Blit the specified rectangle from the source surface to
  // the specified position in the destination surface.
  // srcdect is typically = 0
  LET dx, dy, dw, dh = x, currysize-y-1, 0, 0
  sys(Sys_sdl, sdl_blitsurface, srcptr, 0, dstptr, @dx)
}

AND fillsurf(col) BE // For both 2D and 3D drawing
{ sys(Sys_sdl, sdl_fillsurf, @currsurf, col)
  IF depthv FOR p = 0 TO depthvupb DO depthv!p := maxdepth
}

//AND movesurf(surfptr,dx,dy)

AND getmousestate() = VALOF
{ writef("*ngetmousestate: not available*n")
  abort(999)
}



// 2D drawing functions

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

AND drawpoint(x, y) BE
{ // Draw a 2D point
  // (0, 0) is the bottom left point on the surface
  IF 0<=x<currxsize & 0<=y<currysize DO
    sys(Sys_sdl, sdl_fillrect, @currsurf, x, currysize-y, 1, 1, currcolour)
}

AND selectfont(h) BE TEST h=18
  THEN { fontW, fontH       := 12, 18
         charHsep, charVsep :=  3,  4
	 charLmargin := 10
         write_ch_slice := write_ch_slice18
       }
  ELSE { fontW, fontH       :=  8, 12
         charHsep, charVsep :=  2,  3
	 charLmargin := 10
         write_ch_slice := write_ch_slice12
       }

AND drawch(ch) BE TEST ch='*n'
THEN { currx := charLmargin
       curry := curry-fontH-charVsep
     }
ELSE { FOR line = 0 TO fontH-1 DO
         write_ch_slice(currx, curry+fontH-1-line, ch, line)
       currx := currx + fontW + charHsep
     }

AND write_ch_slice12(x, y, ch, line) BE
{ // Writes the horizontal slice of the given 8x12 character.
  LET cx, cy = currx, curry
  LET offset = 3 * ((ch&#x7F) - '*s')
  // offset is the subscript for the character in the following table.
  LET bitmap = offset + TABLE // Each character has 12 8-bit slices

         #x00000000, //  + - - - + - - -         32 space
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
	 #x00000000, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
	 #x00000000, //  + - - - + - - - = =     base line
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +
	 	 
         #x18181818, //  + - - X X - - -         33 !
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
	 #x18180018, //  + - - X X - - -
	             //  + - - X X - - -
	             //  + - - - + - - -
	             //  + + + X X + + +
	 #x18000000, //  + - - X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +
	 
         #x66666600, //  + X X - + X X -         34 "
	             //  + X X - + X X -
	             //  + X X - + X X -
	             //  + + + + + + + +
	 #x00000000, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
	 #x00000000, //  + - - - + - - -
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +
	 
         
         #x6666FFFF, //  + X X - + X X -         35 #
	             //  + X X - + X X -
	             //  x X X X X X X X
	             //  x X X X X X X X
	 #x66FFFF66, //  + X X - + X X -
	             //  x X X X X X X X
	             //  x X X X X X X X
	             //  + X X + + X X +
	 #x66000000, //  + X X - + X X -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
	 
         #x7EFFD8FE, //  + X X X X X X -         36 $
	             //  X X X X X X X X
	             //  X X - X X - - -
	             //  X X X X X X X +
	 #x7F1B1BFF, //  + X X X X X X X
	             //  + - - X X - X X
	             //  + - - X X - X X
	             //  X X X X X X X X
	 #x7E000000, //  + X X X X X X -
	             //  + - - - + - - -
	             //  x - - - + - - -
	             //  + + + + + + + +
	 
         #x06666C0C, //  + - - - + X X -         37 %
	             //  + - X X + - X X
	             //  + - X X X X - -
	             //  + - - - X X - +
	 #x18303666, //  + - - X X - - -
	             //  + - X X + - - -
	             //  + - X X + X X -
	             //  + X X - + X X -
	 #x60000000, //  + X X - + - - -
	             //  + - - - + - - -
	             //  x - - - + - - -
	             //  + + + + + + + +

         #x3078C8C8, //  + - X X + - - -         38 &
	             //  + X X X X - - -
	             //  X X - - X - - -
	             //  X X + + X + + +
         #x7276DCCC, //  + X X X + - X -
	             //  + X X X + X X -
	             //  X X - X X X - -
	             //  X X + + X X + +
         #x76000000, //  + X X X + X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +


         #x18181800, //  + - - X X - - -         39 '
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + + + + + +
         #x00000000, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x00000000, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +




         #x18306060, //  + - - X X - - -         40 (
	             //  + - X X + - - -
	             //  + X X - + - - -
	             //  + X X + + + + +
         #x60606060, //  + X X - + - - -
	             //  + X X - + - - -
	             //  + X X - + - - -
	             //  + X X - + + + +
         #x60301800, //  + X X - + - - - = =
                     //  + - X X + - - -
                     //  + - - X X - - -
                     //  + + + + + + + +

         #x180C0606, //  + - - X X - - -         41 )
	             //  + - - - X X - -
	             //  + - - - + X X -
	             //  + + + + + X X +
         #x06060606, //  + - - - + X X -
	             //  + - - - + X X  -
	             //  + - - - + X X -
	             //  + + + + + X X +
         #x060C1800, //  + - - - + X X - = =
                     //  + - - - X X - -
                     //  + - - X X - - -
                     //  + + + + + + + +

         #x00009254, //  + - - - + - - -         42 *
	             //  + - - - + - - -
	             //  X - - X + - X -
	             //  + X + X + X + +
         #x38FE3854, //  + - X X X - - -
	             //  X X X X X X X -
	             //  + - X X X - - -
	             //  + X + X + X + +
         #x92000000, //  X - - X + - X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00000018, //  + - - - + - - -         43 +
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + X X + + +
         #x187E7E18, //  + - - X X - - -
	             //  + X X X X X X -
	             //  + X X X X X X -
	             //  + + + X X + + +
         #x18000000, //  + - - X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00000000, //  + - - - + - - -         44 ,
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x00001818, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x08100000, //  + - - X + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00000000, //  + - - - + - - -         45 -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x007E7E00, //  + - - - + - - -
	             //  + X X X X X X -
	             //  + X X X X X X -
	             //  + + + + + + + +
         #x00000000, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +
         
         #x00000000, //  + - - - + - - -         46 .
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x00000018, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + X X + + +
         #x18000000, //  + - - X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x06060C0C, //  + - - - + X X -         47 /
	             //  + - - - + X X -
	             //  + - - - X X - -
	             //  + + + + X X + +
         #x18183030, //  + - - X X - - -
	             //  + - - X X - - -
	             //  + - X X + - - -
	             //  + + X X + + + +
         #x60600000, //  + X X - + - - - = =
                     //  + X X - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x386CC6C6, //  + - X X X - - -         48 0
	             //  + X X - X X - -
	             //  X X - - + X X -
	             //  X X + + + X X +
         #xC6C6C66C, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  + X X + X X + +
         #x38000000, //  + - X X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +
	 
         #x18387818, //  + - - X X - - -         49 1
	             //  + - X X X - - -
	             //  + X X X X - - -
	             //  + + + X X + + +
         #x18181818, //  + - - X X - - -
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x18000000, //  + - - X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3C7E6606, //  + - X X X X - -         50 2
	             //  + X X X X X X -
	             //  + X X - + X X -
	             //  + + + + + X X +
         #x0C18307E, //  + - - - X X - -
	             //  + - - X X - - -
	             //  + - X X + - - -
	             //  + X X X X X X +
         #x7E000000, //  + X X X X X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3C6E4606, //  + - X X X X - -         51 3
	             //  + X X - X X X -
	             //  + X - - + X X -
	             //  + + + + + X X +
         #x1C06466E, //  + - - X X X - -
	             //  + - - - + X X -
	             //  + X - - + X X -
	             //  + X X + X X X +
         #x3C000000, //  + - X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x1C3C3C6C, //  + - - X X X - -         52 4
	             //  + - X X X X - -
	             //  + - X X X X - -
	             //  + X X + X X + +
         #xCCFFFF0C, //  X X - - X X - -
	             //  X X X X X X X X
	             //  X X X X X X X X
	             //  + + + + X X + +
         #x0C000000, //  + - - - X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x7E7E6060, //  + X X X X X X -         53 5
	             //  + X X X X X X -
	             //  + X X - + - - -
	             //  + X X + + + + +
         #x7C0E466E, //  + X X X X X - -
	             //  + - - - X X X -
	             //  + X - - + X X -
	             //  + X X + X X X +
         #x3C000000, //  + - X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3C7E6060, //  + - X X X X - -         54 6
	             //  + X X X X X X -
	             //  + X X - + - - -
	             //  + X X + + - - +
         #x7C66667E, //  + X X X X X - -
	             //  + X X - + X X -
	             //  + X X - + X X -
	             //  + X X X X X X +
         #x3C000000, //  + - X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x7E7E0606, //  + X X X X X X -         55 7
	             //  + X X X X X X -
	             //  + - - - + X X -
	             //  + + + + + X X +
         #x0C183060, //  + - - - X X - -
	             //  + - - X X - - -
	             //  + - X X + - - -
	             //  + X X + + + + +
         #x40000000, //  + X - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3C666666, //  + - X X X X - -         56 8
	             //  + X X - + X X -
	             //  + X X - + X X -
	             //  + X X + + X X +
         #x3C666666, //  + - X X X X - -
	             //  + X X - + X X -
	             //  + X X - + X X -
	             //  + X X + + X X +
         #x3C000000, //  + - X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3C666666, //  + - X X X X - -         57 9
	             //  + X X - + X X -
	             //  + X X - + X X -
	             //  + X X + + X X +
         #x3E060666, //  + - X X X X X -
	             //  + - - - + X X -
	             //  + - - - + X X -
	             //  + X X + + X X +
         #x3C000000, //  + - X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00001818, //  + - - - + - - -         58 :
	             //  + - - - + - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x00001818, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x00000000, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00001818, //  + - - - + - - -         59 ;
	             //  + - - - + - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x00001818, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x08100000, //  + - - - X - - - = =
                     //  + - - X + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00060C18, //  + - - - + - - -         60 <
	             //  + - - - + X X -
	             //  + - - - X X - -
	             //  + + + X X + + +
         #x30603018, //  + - X X + - - -
	             //  + X X - + - - -
	             //  + - X X + - - -
	             //  + + + X X + + +
         #x0C060000, //  + - - - X X - - = =
                     //  + - - - + X X -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0000007C, //  + - - - + - - -         61 =
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + X X X X X X +
         #x7C007C7C, //  + X X X X X X -
	             //  + - - - + - - -
	             //  + X X X X X X -
	             //  + X X X X X X +
         #x00000000, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00603018, //  + - - - + - - -         62 >
	             //  + X X - + - - -
	             //  + - X X + - - -
	             //  + + + X X + + +
         #x0C060C18, //  + - - - X X - -
	             //  + - - - + X X -
	             //  + - - - X X - -
	             //  + + + X X + + +
         #x30600000, //  + - X X + - - - = =
                     //  + X X - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3C7E0606, //  + - X X X X - -         63 ?
	             //  + X X X X X X -
	             //  + - - - + X X -
	             //  + + + + + X X +
         #x0C181800, //  + - - - X X - -
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + + + + + +
         #x18180000, //  + - - X X - - - = =
                     //  + - - X X - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x7E819DA5, //  + X X X X X X -         64 @
	             //  X - - - + - - X
	             //  X - - X X X - X
	             //  X + X + + X + X
         #xA5A59F80, //  X - X - + X - X
	             //  X - X - + X - X
	             //  X - - X X X X X
	             //  X + + + + + + +
         #x7F000000, //  + X X X X X X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3C7EC3C3, //  + - X X X X - -         65 A
	             //  + X X X X X X -
	             //  X X - - + - X X
	             //  X X + + + + X X
	 #xFFFFC3C3, //  X X X X X X X X
	             //  X X X X X X X X
	             //  X X - - + - X X
	             //  X X + + + + X X
	 #xC3000000, //  X X - - + - X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +
	 	 
         #xFEFFC3FE, //  X X X X X X X -         66 B
	             //  X X X X + X X X
	             //  X X     + - X X
	             //  X X X X X X X +
         #xFEC3C3FF, //  X X X X X X X -
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X X X X X X X
         #xFE000000, //  X X X X X X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3E7FC3C0, //  + - X X X X X -         67 C
	             //  + X X X X X X X
	             //  X X - - + - X X
	             //  X X + + + + + +
         #xC0C0C37F, //  X X - - + - - -
	             //  X X - - + - - -
	             //  X X - - + - X X
	             //  + X X X X X X X
         #x3E000000, //  + - X X X X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xFCFEC3C3, //  X X X X X X - -         68 D
	             //  X X X X X X X -
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xC3C3C3FE, //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X X X X X X +
         #xFC000000, //  X X X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xFFFFC0FC, //  X X X X X X X X         69 E
	             //  X X X X X X X X
	             //  X X - - + - - -
	             //  X X X X X X + +
         #xFCC0C0FF, //  X X X X X X - -
	             //  X X - - + - - -
	             //  X X - - + - - -
	             //  X X X X X X X X
         #xFF000000, //  X X X X X X X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xFFFFC0FC, //  X X X X X X X X         70 F
	             //  X X X X X X X X
	             //  X X - - + - - -
	             //  X X X X X X + +
         #xFCC0C0C0, //  X X X X X X - -
	             //  X X - - + - - -
	             //  X X - - + - - -
	             //  X X + + + + + +
         #xC0000000, //  X X - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x3E7FE1C0, //  + - X X X X X -         71 G
	             //  + X X X X X X X
	             //  X X X - + - - X
	             //  X X + + + + + +
         #xCFCFE3FF, //  X X - - X X X X
	             //  X X - - X X X X
	             //  X X X - + - X X
	             //  X X X X X X X X
         #x7E000000, //  + X X X X X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xC3C3C3FF, //  X X - - + - X X         72 H
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X X X X X X X
         #xFFC3C3C3, //  X X X X X X X X
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xC3000000, //  X X - - + - X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x18181818, //  + - - X X - - -         73 I
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x18181818, //  + - - X X - - -
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x18000000, //  + - - X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x7F7F0C0C, //  + X X X X X X X         74 J
	             //  + X X X X X X X
	             //  + - - - X X - -
	             //  + + + + X X + +
         #x0C0CCCFC, //  + - - - X X - -
	             //  + - - - X X - -
	             //  X X - - X X - -
	             //  X X X X X X + +
         #x78000000, //  + X X X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xC2C6CCD8, //  X X - - + - X -         75 K
	             //  X X - - + X X -
	             //  X X - - X X - -
	             //  X X + X X + + +
         #xF0F8CCC6, //  X X X X + - - -
	             //  X X X X X - - -
	             //  X X - - X X - -
	             //  X X + + + X X +
         #xC2000000, //  X X - - + - X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xC0C0C0C0, //  X X - - + - - -         76 L
	             //  X X - - + - - -
	             //  X X - - + - - -
	             //  X X + + + + + +
         #xC0C0C0FE, //  X X - - + - - -
	             //  X X - - + - - -
	             //  X X - - + - - -
	             //  X X X X X X X +
         #xFE000000, //  X X X X X X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x81C3E7FF, //  X - - - + - - X         77 M
	             //  X X - - + - X X
	             //  X X X - + X X X
	             //  X X X X X X X X
         #xDBC3C3C3, //  X X - X X - X X
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xC3000000, //  X X - - + - X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x83C3E3F3, //  X - - - + - X X         78 N
	             //  X X - - + - X X
	             //  X X X - + - X X
	             //  X X X X + + X X
         #xDBCFC7C3, //  X X - X X - X X
	             //  X X - - X X X X
	             //  X X - - + X X X
	             //  X X + + + + X X
         #xC1000000, //  X X - - + - - X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x7EFFC3C3, //  + X X X X X X -         79 O
	             //  X X X - + X X X
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xC3C3C3E7, //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X X + + X X X
         #x7E000000, //  + X X X X X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xFEFFC3C3, //  X X X X X X X -         80 P
	             //  X X X X X X X X
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xFFFEC0C0, //  X X X X X X X X
	             //  X X X X X X X -
	             //  X X - - + - - -
	             //  X X + + + + + +
         #xC0000000, //  X X - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x7EFFC3C3, //  + X X X X X X -         81 Q
	             //  X X X X X X X X
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xDBCFC6FF, //  X X - X X - X X
	             //  X X - - X X X X
	             //  X X - - + X X -
	             //  X X X X X X X X
         #x7B000000, //  + X X X X - X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xFEFFC3C3, //  X X X X X X X -         82 R
	             //  X X X X X X X X
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xFFFECCC6, //  X X X X X X X X
	             //  X X X X X X X -
	             //  X X - - X X - -
	             //  X X + + + X X +
         #xC3000000, //  X X - - + - X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x7EC3C0C0, //  + X X X X X X -         83 S
	             //  X X - - + - X X
	             //  X X - - + - - -
	             //  X X + + + + + +
         #x7E0303C3, //  + X X X X X X -
	             //  + - - - + - X X
	             //  + - - - + - X X
	             //  X X + + + + X X
         #x7E000000, //  + X X X X X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xFFFF1818, //  X X X X X X X X         84 T
	             //  X X X X X X X X
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x18181818, //  + - - X X - - -
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x18000000, //  + - - X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xC3C3C3C3, //  X X - - + - X X         85 U
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xC3C3C37E, //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  + X X X X X X -
         #x3C000000, //  + - X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x81C3C366, //  X - - - + - - X         86 V
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  + X X + + X X +
         #x663C3C18, //  + X X - + X X -
	             //  + - X X X X - -
	             //  + - X X X X - -
	             //  + + + X X + + +
         #x18000000, //  + - - X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xC3C3C3C3, //  X X - - + - X X         87 W
	             //  X X - - + - X X
	             //  X X - - + - X X
	             //  X X + + + + X X
         #xDBFFE7C3, //  X X - X X - X X
	             //  X X X X X X X X
	             //  X X X - + X X X
	             //  X X + + + + X X
         #x81000000, //  X - - - + - - X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xC3C3663C, //  X X - - + - X X         88 X
	             //  X X - - + - X X
	             //  + X X - + X X -
	             //  + + X X X X + +
         #x183C66C3, //  + - - X X - - -
	             //  + - X X X X - -
	             //  + X X - + X X -
	             //  X X + + + + X X
         #xC3000000, //  X X - - + - X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xC3C36666, //  X X - - + - X X         89 Y
	             //  X X - - + - X X
	             //  + X X - + X X -
	             //  + X X + + X X +
         #x3C3C1818, //  + - X X X X - -
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x18000000, //  + - - X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xFFFF060C, //  X X X X X X X X         90 Z
	             //  X X X X X X X X
	             //  + - - - + X X -
	             //  + + + + X X + +
         #x183060FF, //  + - - X X - - -
	             //  + - X X + - - -
	             //  + X X - + - - -
	             //  X X X X X X X X
         #xFF000000, //  X X X X X X X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x78786060, //  + X X X X - - -         91 [
	             //  + X X X X - - -
	             //  + X X - + - - -
	             //  + X X + + + + +
         #x60606060, //  + X X - + - - -
	             //  + X X - + - - -
	             //  + X X - + - - -
	             //  + X X + + + + +
         #x78780000, //  + X X X X - - - = =
                     //  + X X X X - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x60603030, //  + X X - + - - -         92 \
	             //  + X X - + - - -
	             //  + - X X + - - -
	             //  + + X X + + + +
         #x18180C0C, //  + - - X X - - -
	             //  + - - X X - - -
	             //  + - - - X X - -
	             //  + + + + X X + +
         #x06060000, //  + - - - + X X - = =
                     //  + - - - + X X -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x1E1E0606, //  + - - X X X X -         93 ]
	             //  + - - X X X X -
	             //  + - - - + X X -
	             //  + + + + + X X +
         #x06060606, //  + - - - + X X -
	             //  + - - - + X X -
	             //  + - - - + X X -
	             //  + + + + + X X +
         #x1E1E0000, //  + - - X X X X - = =
                     //  + - - X X X X -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x183C66C3, //  + - - X X - - -         94 ^
	             //  + - X X X X - -
	             //  + X X - + X X -
	             //  X X + + + + X X
         #x00000000, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x00000000, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00000000, //  + - - - + - - -         95 _
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x00000000, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x00FFFF00, //  + - - - + - - - = =
                     //  X X X X X X X X
                     //  X X X X X X X X
                     //  + + + + + + + +

         #x30180C00, //  + - x x + - - -         96 `
	             //  + - - x x - - -
	             //  + - - - x x - -
	             //  + + + + + + + +
         #x00000000, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x00000000, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00007AFE, //  + - - - + - - -         97 a
	             //  + - - - + - - -
	             //  + X X X X - X -
	             //  X X X X X X X +
         #xC6C6C6FE, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X X X X X X +
         #x7B000000, //  + X X X X - X X = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xC0C0DCFE, //  X X - - + - - -         98 b
	             //  X X - - + - - -
	             //  X X - X X X - -
	             //  X X X X X X X +
         #xC6C6C6FE, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X X X X X X +
         #xDC000000, //  X X - X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00007CFE, //  + - - - + - - -         99 c
	             //  + - - - + - - -
	             //  + X X X X X - -
	             //  X X X X X X X +
         #xC6C0C6FE, //  X X - - + X X -
	             //  X X - - + - - -
	             //  X X - - + X X -
	             //  X X X X X X X +
         #x7C000000, //  + X X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x060676FE, //  + - - - + X X -        100 d
	             //  + - - - + X X -
	             //  + X X X + X X -
	             //  X X X X X X X +
         #xC6C6C6FE, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X X X X X X +
         #x76000000, //  + X X X + X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00007CFE, //  + - - - + - - -        101 e
	             //  + - - - + - - -
	             //  + X X X X X - -
	             //  X X X X X X X +
         #xC6FCC0FE, //  X X - - + X X -
	             //  X X X X X X - -
	             //  X X - - + - - -
	             //  X X X X X X X -
         #x7C000000, //  + X X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x000078FC, //  + - - - + - - -        102 f
	             //  + - - - + - - -
	             //  + X X X X - - -
	             //  X X X X X X + +
         #xC0F0F0C0, //  X X - - + - - -
	             //  X X X X + - - -
	             //  X X X X + - - -
	             //  X X + + + + + +
         #xC0000000, //  X X - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +


         #x000076FE, //  + - - - + - - -        103 g
	             //  + - - - + - - -
	             //  + X X X + X X -
	             //  X X X X X X X -
	 #xC6C6C6FE, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X X X X X X +
	 #x7606FE7C, //  + X X X + X X - = =
                     //  + - - - + X X -
                     //  X X X X X X X -
                     //  + X X X X X + +
	 	 
         #xC0C0DCFE, //  X X - - + - - -        104 h
	             //  X X - - + - - -
	             //  X X - X X X - -
	             //  X X X X X X X +
         #xC6C6C6C6, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X + + + X X +
         #xC6000000, //  X X - - + X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x18180038, //  + - - X X - - -        105 i
	             //  + - - X X - - -
	             //  + - - - + - - -
	             //  + + X X X + + +
         #x18181818, //  + - - X X - - -
	             //  + - - X X - - -
	             //  + - - X X - - -
	             //  + + + X X + + +
         #x0C000000, //  + - - - X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0C0C001C, //  + - - - X X - -        106 J
	             //  + - - - X X   - -
	             //  + - - - + - - -
	             //  + + + X X X + +
         #x0C0C0C7C, //  + - - - X X - -
	             //  + - - - X X - -
	             //  + - - - X X - -
	             //  + X X X X X + +
         #x38000000, //  + - X X X - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00C0C6CC, //  + - - - + - - -        107 k
	             //  X X - - + - - -
	             //  X X - - + X X -
	             //  X X + + X X + +
         #xD8F0F8CC, //  X X - X X - - -
	             //  X X X X + - - -
	             //  X X X X X - - -
	             //  X X + + X X + +
         #xC6000000, //  X X - - + X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xE0606060, //  X X X - + - - -        108 l
	             //  + X X - + - - -
	             //  + X X - + - - -
	             //  + X X + + + + +
         #x6060607C, //  + X X - + - - -
	             //  + X X - + - - -
	             //  + X X - + - - -
	             //  + X X X X X + +
         #x38000000, //  + - X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00006CFE, //  + - - - + - - -        109 m
	             //  + - - - + - - -
	             //  + X X - X X - -
	             //  X X X X X X X +
         #xD6D6D6D6, //  X X - X + X X -
	             //  X X - X + X X -
	             //  X X - X + X X -
	             //  X X + X + X X +
         #xD6000000, //  X X - X + X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0000DCFE, //  + - - - + - - -        110 n
	             //  + - - - + - - -
	             //  X X - X X X - -
	             //  X X X X X X X +
         #xC6C6C6C6, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X + + + X X +
         #xC6000000, //  X X - - + X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00007CFE, //  + - - - + - - -        111 o
	             //  + - - - + - - -
	             //  + X X X X X - -
	             //  X X X X X X X +
         #xC6C6C6FE, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X X X X X X +
         #x7C000000, //  + X X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00007CFE, //  + - - - + - - -        112 p
	             //  + - - - + - - -
	             //  + X X X X X - -
	             //  X X X X X X X +
         #xC6FEFCC0, //  X X - - + X X -
	             //  X X X X X X X -
	             //  X X X X X X - -
	             //  X X + + + + + +
         #xC0000000, //  X X - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00007CFE, //  + - - - + - - -        113 q
	             //  + - - - + - - -
	             //  + X X X X X - -
	             //  X X X X X X X +
         #xC6FE7E06, //  X X - - + X X -
	             //  X X X X X X X -
	             //  + X X X X X X -
	             //  + + + + + X X +
         #x06000000, //  + - - - + X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0000DCFE, //  + - - - + - - -        114 R
	             //  + - - - + - - -
	             //  X X - X X X - -
	             //  X X X X X X X +
         #xC6C0C0C0, //  X X - - + X X -
	             //  X X - - + - - -
	             //  X X - - + - - -
	             //  X X + + + + + +
         #xC0000000, //  X X - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x00007CFE, //  + - - - + - - -        115 s
	             //  + X X X X X - -
	             //  X X X X X X X -
	             //  X X + + + + + +
         #xF03C06FE, //  X X X X + - - -
	             //  + - X X X X - -
	             //  + - - - + X X -
	             //  X X X X X X X +
         #x7C000000, //  + X X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x6060FCFC, //  + - X X + - - -        116 t
	             //  + - X X + - - -
	             //  X X X X X X - -
	             //  X X X X X X + +
         #x6060603E, //  + - X X + - - -
	             //  + - X X + - - -
	             //  + - X X + - - -
	             //  + - X X X X X +
         #x3C000000, //  + - X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0000C6C6, //  + - - - + - - -        117 u
	             //  + - - - + - - -
	             //  X X - - + X X -
	             //  X X + + + X X +
         #xC6C6C6FE, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X X X X X X +
         #x7C000000, //  + X X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0000C6C6, //  + - - - + - - -        118 v
	             //  + - - - + - - -
	             //  X X - - + X X -
	             //  X X + + + X X +
         #x6C6C6C38, //  + X X - X X - -
	             //  + X X - X X - -
	             //  + X X - X X - -
	             //  + + X X X + + +
         #x10000000, //  + - - X + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0000D6D6, //  + - - - + - - -        119 w
	             //  + - - - + - - -
	             //  X X - x + X X -
	             //  x x + x + x x +
         #xD6D6D6FE, //  x x - x + x x -
	             //  x x - x + x x -
	             //  x x - x + x x -
	             //  x x x x x x x +
         #x6C000000, //  + x x - x x - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0000C6C6, //  + - - - + - - -        120 x
	             //  + - - - + - - -
	             //  X X - - + X X -
	             //  X X + + + X X -
         #x6C386CC6, //  + X X - X X - -
	             //  + - X X X - - -
	             //  + - X X X - - -
	             //  + X X + X X + +
         #xC6000000, //  X X - - + X X - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0000C6C6, //  + - - - + - - -        121 y
	             //  + - - - + - - -
	             //  X X - - + X X -
	             //  X X + + + X X -
         #xC6C6C67E, //  X X - - + X X -
	             //  X X - - + X X -
	             //  X X - - + X X -
	             //  + X X X X X X +
         #x7606FE7C, //  + X X X X X - - = =
                     //  + - - - X X - -
                     //  X X X X X X X -
                     //  + X X X X X + +

         #x00007EFE, //  + - - - + - - -        122 z
	             //  + - - - + - - -
	             //  + X X X X X X -
	             //  X X X X X X X +
         #x0C3860FE, //  + - - - X X - -
	             //  + - X X X - - -
	             //  + X X - + - - -
	             //  X X X X X X X +
         #xFC000000, //  X X X X X X - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x0E1C180C, //  + - - - X X X -        123 {
	             //  + - - X X X - -
	             //  + - - X X - - -
	             //  + + + - X X + +
         #x1830180C, //  + - - X X - - -
	             //  + - X X + - - -
	             //  + - - X X - - -
	             //  + + + - X X + +
         #x181C0E00, //  + - - X X - - - = =
                     //  + - - X X X - -
                     //  + - - - X X X -
                     //  + + + + + + + +

         #x18181818, //  + - - - + - - -        124 |
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x18181818, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
         #x18181800, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #x70381830, //  + X X X + - - -        125 }
	             //  + - X X X - - -
	             //  + - - X X - - -
	             //  + + X X + + + +
         #x180C1830, //  + - - X X - - -
	             //  + - - - X X - -
	             //  + - - X X - - -
	             //  + + X X + + + +
         #x18387000, //  + - - X X - - - = =
                     //  + - X X X - - -
                     //  + X X X + - - -
                     //  + + + + + + + +

         #x00000070, #xD1998B0E, #x00000000, // ~
       //#x00000000, //  + - - - + - - -         dd x
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
       //#x00000000, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
       //#x00000000, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +

         #xAA55AA55, #xAA55AA55, #xAA55AA55  // rubout
       //#x00000000, //  + - - - + - - -         dd x
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
       //#x00000000, //  + - - - + - - -
	             //  + - - - + - - -
	             //  + - - - + - - -
	             //  + + + + + + + +
       //#x00000000, //  + - - - + - - - = =
                     //  + - - - + - - -
                     //  + - - - + - - -
                     //  + + + + + + + +


  // bitmap points to the three words giving the pixels of the character.
  { LET col = currcolour
    LET w = VALOF SWITCHON line INTO
    { CASE  0: RESULTIS bitmap!0>>24
      CASE  1: RESULTIS bitmap!0>>16
      CASE  2: RESULTIS bitmap!0>> 8
      CASE  3: RESULTIS bitmap!0
      CASE  4: RESULTIS bitmap!1>>24
      CASE  5: RESULTIS bitmap!1>>16
      CASE  6: RESULTIS bitmap!1>> 8
      CASE  7: RESULTIS bitmap!1
      CASE  8: RESULTIS bitmap!2>>24
      CASE  9: RESULTIS bitmap!2>>16
      CASE 10: RESULTIS bitmap!2>> 8
      CASE 11: RESULTIS bitmap!2
    }

    IF (w & #b10000000) > 0 DO drawpoint(x+0, y)
    IF (w & #b01000000) > 0 DO drawpoint(x+1, y)
    IF (w & #b00100000) > 0 DO drawpoint(x+2, y)
    IF (w & #b00010000) > 0 DO drawpoint(x+3, y)
    IF (w & #b00001000) > 0 DO drawpoint(x+4, y)
    IF (w & #b00000100) > 0 DO drawpoint(x+5, y)
    IF (w & #b00000010) > 0 DO drawpoint(x+6, y)
    IF (w & #b00000001) > 0 DO drawpoint(x+7, y)

//writef("writeslice: ch=%c line=%i2 w=%b8 bits=%x8 %x8 %x8*n",
//        ch, line, w, bitmap!0, bitmap!1, bitmap!2)

  }

  currx, curry := cx, cy
}

AND write_ch_slice18(x, y, ch, line) BE
{ // Writes the horizontal slice of the given 18x12 character.
  LET cx, cy = currx, curry
  LET offset = 9 * ((ch&#x7F) - '*s')
  // offset is the subscript of the character in the following table.
  LET bitmap = offset + TABLE
         // Each character has 18 12-bit slices packed in 16-bit words

         #x_000_0_000, //  + - - - + - - - + - - -              32 space
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =    base line
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
 	

         #x_0E0_0_0E0, //  + - - - X X X - + - - -              33 !
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_0E0_0_000, //  + - - - X X X - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_198_0_198, //  + - - X X - - X X - - -              34 "
	               //  + - - X X - - X X - - -
	 #x_198_0_090, //  + - - X X - - X X - - -
	               //  + + + + X + + X + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_18C, //  + - - - + - - - + - - -              35 #
	               //  + - - X X - - - X X - -
	 #x_18C_0_18C, //  + - - X X - - - X X - -
	               //  + + + X X + + + X X + +
	 #x_18C_0_7FF, //  + - - X X - - - X X - -
	               //  + X X X X X X X X X X X
	 #x_7FF_0_18C, //  + X X X X X X X X X X X
	               //  + + + X X + + + X X + +
	 #x_18C_0_18C, //  + - - X X - - - X X - -
	               //  + - - X X - - - X X - -
	 #x_7FF_0_7FF, //  + X X X X X X X X X X X
	               //  + X X X X X X X X X X X
	 #x_18C_0_18C, //  + - - X X - - - X X - -
	               //  + - - X X - - - X X - - = = =
	 #x_18C_0_18C, //  + - - X X - - - X X - -
	               //  + + + X X + + + X X + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_060_0_1F8, //  + - - - + X X - + - - -              36 $
	               //  + - - X X X X X X - - -
	 #x_3FC_0_666, //  + - X X X X X X X X - -
	               //  + X X + + X X + + X X +
	 #x_666_0_660, //  + X X - + X X - + X X -
	               //  + X X - + X X - + - - -
	 #x_3F8_0_1FE, //  + - X X X X X X X - - -
	               //  + + + X X X X X X X - +
	 #x_06E_0_666, //  + - - - + X X - X X X -
	               //  + - - - + X X - + X X -
	 #x_666_0_666, //  + X X - + X X - + X X -
	               //  + X X - + X X - + X X +
	 #x_3FC_0_1F8, //  + - X X X X X X X X - -
	               //  + - - X X X X X X - - - = = =
	 #x_060_0_000, //  + - - - + X X  - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_003_0_286, //  + - - - + - - - + X X -              37 %
	               //  + - X X + - - - + X X -
	 #x_7C6_0_CCC, //  + X X X X - - - X X - -
	               //  X X + + X X + + X X - +
	 #x_CCC_0_798, //  X X - - X X - X X - - -
	               //  + X X X X - - X X - - -
	 #x_330_0_030, //  + - X X - - X X + - - -
	               //  + + + + + + X X + + + +
	 #x_060_0_060, //  + - - - + X X - + - - -
	               //  + - - - + X X - + - - -
	 #x_0C0_0_0C0, //  + - - - X X - - + - - -
	               //  + + + + X X + - X X + +
	 #x_19E_0_1B3, //  + - - X X - - X X X X -
	               //  + - - X X - X X + - X X = = =
	 #x_333_0_31E, //  + - X X + - X X + - X X
	               //  + - X X + + + X X X X +
	 #x_60C_0_600, //  + X X - + - - - X X - -
	               //  + X X - + - - - + - - -
	 
         #x_000_0_1F8, //  + - - - + - - - + - - -              38 &
	               //  + - - X X X X X X - - -
	 #x_3FC_0_70C, //  + - X X X X X X X X - -
	               //  + X X X + + + + X X + +
	 #x_60C_0_718, //  + X X - + - - - X X - -
	               //  + X X X + - - X X - - -
	 #x_3F0_0_1E0, //  + - X X X X X X + - - -
	               //  + + + X X X X + + + + +
	 #x_0E3_0_1E6, //  + - - - X X X - + - X X
	               //  + - - X X X X + - X X
	 #x_37C_0_638, //  + - X X + X X X X X - -
	               //  + X X + + + X X X + + +
	 #x_E38_0_E7C, //  X X X - + - X X X - - -
	               //  X X X - + X X X X X - - = = =
	 #x_7EE_0_1C3, //  + X X X X X X - X X X -
	               //  + + + X X X + + + X X
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0E0_0_0E0, //  + - - - X X X - - + - - -              39 '
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X - + + + +
	 #x_040_0_000, //  + - - - + X - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_038_0_070, //  + - - - + - X X X - - -              40 (
	               //  + - - - + X X X + - - -
	 #x_070_0_0E0, //  + - - - + X X X + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_1C0, //  + - - - X X X - + - - -
	               //  + - - X X X - - + - - -
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + + + X X X + + + + + +
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + - - X X X - - + - - -
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + + + X X X + + + + + +
	 #x_1C0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_0E0_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_038_0_000, //  + - - - + - X X X - - -
	               //  + - - - + - - - + - - -
	 
         #x_1C0_0_0E0, //  + - - X X X - - + - - -              41 )
	               //  + - - - X X X - + - - -
	 #x_0E0_0_070, //  + - - - X X X - + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_038, //  + - - - + X X X + - - -
	               //  + - - - + - X X X - -
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + + + + + + X X X + + +
	 #x_038_0_038, //  + - - - + - X X X + - -
	               //  + - - - + - X X X + - -
	 #x_038_0_038, //  + - - - + - X X X + - -
	               //  + + + + + + X X X + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - - = = =
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X - + + + +
	 #x_1C0_0_000, //  + - - X X X - - + - - -
	               //  + - - - + - - - + - - -
	 
	 
         #x_000_0_000, //  + - - - + - - - + - - -              42 *
	               //  + - - - + - - - + - - -
	 #x_000_0_070, //  + - - - + - - - + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_673, //  + - - - + X X X + - - -
	               //  + X X - + X X X + - X X
	 #x_777_0_3FE, //  + X X X - X X X + X X X
	               //  + - X X X X X X X X X +
	 #x_0F8_0_0F8, //  + - - - X X X X X - - -
	               //  + - - - X X X X X - - -
	 #x_0F8_0_3FE, //  + - - - X X X X X - - -
	               //  + + X X X X X X X X X +
	 #x_777_0_673, //  + X X X + X X X + X X X
	               //  + X X - + X X X + - X X = = =
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              43 +
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_7FF_0_7FF, //  + X X X X X X X X X X X
	               //  + X X X X X X X X X X X
	 #x_7FF_0_070, //  + X X X X X X X X X X X
	               //  + + + + + X X X + + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - - = = =
	 #x_070_0_000, //  + - - - + X X X + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              44 ,
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_070, //  + - - - X X X X X - - -
	               //  + - - - X X X X X - - - = = =
	 #x_070_0_060, //  + - - - + X X X + - - -
	               //  + + + + + + X X + + + +
	 #x_0C0_0_180, //  + - - - + X X - + - - -
	               //  + - - - X X - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              45 -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_3FE_0_3FE, //  + - X X X X X X X X X -
	               //  + - X X X X X X X X X -
	 #x_3FE_0_000, //  + - X X X X X X X X X -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              46 .
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_060, //  + - - - + - - - + - - -
	               //  + + + + + X X - + + + +
	 #x_0F0_0_0F0, //  + - - - X X X X + - - -
	               //  + - - - X X X X + - - - = = =
	 #x_060_0_000, //  + - - - + X X - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_007_0_007, //  + - - - + - - - + X X X              47 /
	               //  + - - - + - - - + X X X
	 #x_00E_0_00E, //  + - - - + - - - X X X -
	               //  + + + + + + + + X X X +
	 #x_01C_0_01C, //  + - - - + - - X X X - -
	               //  + - - - + - - X X X - -
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + + + + + + X X X + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + - - X X X - - + - - - = = =
	 #x_380_0_380, //  + - X X X - - - + - - -
	               //  + + X X X + + + + + + +
	 #x_700_0_000, //  + X X X + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_1F8, //  + - - - X X X X + - - -              48 0
	               //  + - - X X X X X X - - -
	 #x_30C_0_30C, //  + - X X + - - - X X - -
	               //  + - X X + + + + X X + +
	 #x_666_0_666, //  + X X - + X X - + X X -
	               //  + X X - + X X - + X X -
	 #x_606_0_606, //  + X X - + - - - + X X -
	               //  + X X + + + + + + X X -
	 #x_606_0_606, //  + X X - + - - - + X X -
	               //  + X X - + - - - + X X -
	 #x_30C_0_30C, //  + - X X + - - - X X - -
	               //  + - X X + + + + X X - +
	 #x_1F8_0_0F0, //  + - - X X X X X X - - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0E0_0_1E0, //  + - - - X X X - + - - -              49 1
	               //  + - - X X X X - + - - -
	 #x_3E0_0_6E0, //  + - X X X X X - + - - -
	               //  + X X - X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_3F8_0_3F8, //  + - X X X X X X X - - -
	               //  + - X X X X X X X - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_3FC, //  + - - - X X X X + - - -              50 2
	               //  + - X X X X X X X X - -
	 #x_71E_0_60E, //  + X X X + - - X X X X -
	               //  + X X + + + + + X X X +
	 #x_00E_0_01C, //  + - - - + - - - X X X -
	               //  + - - - + - - X X X - -
	 #x_038_0_070, //  + - - - + - X X X - - -
	               //  + + + + + X X X + + + +
	 #x_0E0_0_1C0, //  + - - - X X X - + - - -
	               //  + - - X X X - - + - - -
	 #x_380_0_700, //  + - X X X - - - + - - -
	               //  + X X X + + + + + + + +
	 #x_7FE_0_7FE, //  + X X X X X X X X X X -
	               //  + X X X X X X X X X X - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_3FC, //  + - - - X X X X + - - -              51 3
	               //  + - X X X X X X X X - -
	 #x_71E_0_60E, //  + X X X + - - X X X X -
	               //  + X X + + + + + X X X +
	 #x_00E_0_01E, //  + - - - + - - - X X X -
	               //  + - - - + - - X X X X -
	 #x_03C_0_01E, //  + - - - + - X X X X - -
	               //  + + + + + + + X X X X +
	 #x_00E_0_00E, //  + - - - + - - - X X X -
	               //  + - - - + - - - X X X -
	 #x_60E_0_71E, //  + X X - + - - - X X X -
	               //  + X X X + + + X X X X +
	 #x_3FC_0_0F0, //  + - X X X X X X X X - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_00C_0_01C, //  + - - - + - - - X X - -              52 4
	               //  + - - - + - - X X X - -
	 #x_03C_0_07C, //  + - - - + - X X X X - -
	               //  + + + + + X X X X X + +
	 #x_0FC_0_1DC, //  + - - - X X X X X X - -
	               //  + - - X X X - X X X - -
	 #x_39C_0_71C, //  + - X X X - - X X X - -
	               //  + X X X + - - X X X - -
	 #x_E0E_0_FFF, //  X X X - + - - X X X - -
	               //  X X X X X X X X X X X X - -
	 #x_FFF_0_01C, //  X X X X X X X X X X X X
	               //  + + + + + + + X X X + +
	 #x_01C_0_01C, //  + - - - + - - X X X - -
	               //  + - - - + - - X X X - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_7FE_0_7FE, //  + X X X X X X X X X X -              53 5
	               //  + X X X X X X X X X X -
	 #x_700_0_700, //  + X X X + - - - + - - -
	               //  + X X X + + + + + + + +
	 #x_700_0_780, //  + X X X + - - - + - - -
	               //  + X X X X - - - + - - -
	 #x_7F0_0_1FC, //  + X X X X X X X + - - -
	               //  + + + X X X X X X X + +
	 #x_03E_0_00E, //  + - - - + - X X X X X - -
	               //  + - - - + - - - X X X -
	 #x_00E_0_70E, //  + - - - + - - - X X X -
	               //  + X X X + + + + X X X +
	 #x_7FC_0_3FC, //  + X X X X X X X X X - -
	               //  + - X X X X X X X - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_07C_0_1FE, //  + - - - + X X X X X - -              54 6
	               //  + - - X X X X X X X X -
	 #x_386_0_300, //  + - X X X - - - + X X -
	               //  + + X X + + + + + + + +
	 #x_700_0_700, //  + X X X + - - - + - - -
	               //  + X X X + - - - + - - -
	 #x_7F8_0_7FC, //  + X X X X X X X X - - -
	               //  + X X X X X X X X X + +
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + - - - X X X -
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + + + + X X X +
	 #x_3FC_0_0F0, //  + - X X X X X X X X - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_3FE_0_3FE, //  + - X X X X X X X X X -              55 7
	               //  + - X X X X X X X X X -
	 #x_01C_0_01C, //  + - - - + - - X X X - -
	               //  + + + + + + + X X X + +
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + - - - + - X X X - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + + + X X X + + + + + +
	 #x_380_0_380, //  + - X X X - - - + - - -
	               //  + - X X X - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_3FC, //  + - - - X X X X + - - -              56 8
	               //  + - X X X X X X X X - -
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + + + + X X X +
	 #x_70E_0_39C, //  + X X X + - - - X X X -
	               //  + - X X X - - X X X - -
	 #x_0F0_0_3FC, //  + - - - X X X X + - - -
	               //  + + X X X X X X X X - -
	 #x_79E_0_70E, //  + X X X X - - X X X X -
	               //  + X X X + - - - X X X -
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + + + - X X X +
	 #x_3FC_0_0F0, //  + - X X X X X X X X - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_3FC, //  + - - - X X X X + - - -              57 9
	               //  + - X X X X X X X X - -
	 #x_70E_0_70E, //  + X X X - - - - X X X -
	               //  + X X X + + + + X X X +
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + - - - X X X -
	 #x_7FE_0_3FE, //  + X X X X X X X X X X -
	               //  + - X X X X X X X X X +
	 #x_00E_0_00E, //  + - - - + - - - X X X
	               //  + - - - + - - - X X X -
	 #x_00E_0_61E, //  + - - - + - - - X X X -
	               //  + X X + + + + X X X X +
	 #x_7FC_0_3F0, //  + X X X X X X X X X - -
	               //  + - X X X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              58 :
	               //  + - - - + - - - + - - -
	 #x_060_0_0F0, //  + - - - + X X + - - -
	               //  + + + + X X X X + + + +
	 #x_0F0_0_060, //  + - - - X X X X + - - -
	               //  + - - - + X X - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_060, //  + - - - + - - + - - -
	               //  + - - - + X X - + - - -
	 #x_0F0_0_0F0, //  + - - - X X X X + - - -
	               //  + + + + X X X X + + + +
	 #x_060_0_000, //  + - - - + X X - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              59 ;
	               //  + - - - + - - - + - - -
	 #x_060_0_0F0, //  + - - - + X X + - - -
	               //  + + + + X X X X + + + +
	 #x_0F0_0_060, //  + - - - X X X X + - - -
	               //  + - - - + X X - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_060, //  + - - - + - - + - - -
	               //  + - - - + X X - + - - -
	 #x_0F0_0_0F0, //  + - - - X X X X + - - -
	               //  + + + + X X X X + + + +
	 #x_070_0_0E0, //  + - - - + X X X + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_1C0_0_000, //  + - - X X X - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              60 <
	               //  + - - - + - - - + - - -
	 #x_000_0_00E, //  + - - - + - - - + - - -
	               //  + + + + + + + + X X X +
	 #x_03E_0_0F8, //  + - - - + - X X X X X -
	               //  + - - - X X X X X - - -
	 #x_3E0_0_780, //  + - X X X X X - + - - -
	               //  + X X X X + + + + + + +
	 #x_3E0_0_0F8, //  + - X X X X X - + - - -
	               //  + - - - X X X X X - - -
	 #x_07E_0_00E, //  + - - - + - X X X X X -
	               //  + + + + + + + + X X X +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 

         #x_000_0_000, //  + - - - + - - - + - - -              61 =
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_3FC_0_3FC, //  + - X X X X X X X X - -
	               //  + - X X X X X X X X - -
	 #x_3FC_0_000, //  + - X X X X X X X X - -
	               //  + + + + + + + + + + + +
	 #x_000_0_3FC, //  + - - - + - - - + - - -
	               //  + - X X X X X X X X - -
	 #x_3FC_0_3FC, //  + - X X X X X X X X - -
	               //  + + X X X X X X X X + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              62 >
	               //  + - - - + - - - + - - -
	 #x_000_0_700, //  + - - - + - - - + - - -
	               //  + X X X + + + + + - - +
	 #x_7C0_0_1F0, //  + X X X X X - - + - - -
	               //  + - - X X X X X + - - -
	 #x_07C_0_01E, //  + - - - + X X X X X - -
	               //  + - - - + + + X X X X +
	 #x_07C_0_1F0, //  + - - - + X X X X X - -
	               //  + - - X X X X X + - - -
	 #x_7C0_0_700, //  + X X X X X - - + - - -
	               //  + X X X + + + + + - - +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 

         #x_1F8_0_3FC, //  + - - X X X X X X - - -              63 ?
	               //  + - X X X X X X X X - -
	 #x_70E_0_60E, //  + X X X + - - - X X X -
	               //  + X X + + + + + + X X -
	 #x_00E_0_01C, //  + - - - + - - - X X X -
	               //  + - - - + - - X X X - -
	 #x_038_0_070, //  + - - - + - X X X - - -
	               //  + + + + + X X X - + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_000, //  + - - - X X X - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_0E0, //  + - - - + - - - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_3F8_0_7FC, //  + - X X X X X X X - - -              64 @
	               //  + X X X X X X X X X X -
	 #x_C03_0_C03, //  X X - - + - - - + - X X
	               //  X X + + + + + + + + X X
	 #x_CF3_0_DFB, //  X X - - X X X X + - X X
	               //  X X - X X X X X X - X X
	 #x_D9B_0_D9B, //  X X - X X - - X X - X X
	               //  X X + X X + + X X + X X
	 #x_D9B_0_DFF, //  X X - X X - - X X - X X
	               //  X X - X X X X X X X X X
	 #x_CFE_0_C00, //  X X - - X X X X X X X -
	               //  X X + + + + + + + + + +
	 #x_C00_0_7FF, //  X X - - + - - - + - - -
	               //  + X X X X X X X X X X - = = =
	 #x_3FE_0_000, //  + - X X X X X X X X - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_1F8, //  + - - - X X X X + - - -              65 A
	               //  + - - X X X X X X - - -
	 #x_39C_0_70E, //  + - X X X - - X X X - -
	               //  + X X X + + + + X X X +
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  x X X - + - - - + X X X
	 #x_E07_0_FFF, //  X X X - + - - - + X X X
	               //  X X X X X X X X X X X X
	 #x_FFF_0_C03, //  X X X X X X X X X X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_FF0_0_FFC, //  X X X X X X X X + - - -              66 B
	               //  X X X X X X X X X X - -
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X + + + + + X X X +
	 #x_E0E_0_E1C, //  X X X - + - - - X X X -
	               //  X X X - + - - X X X - -
	 #x_FF8_0_FF8, //  X X X X X X X X X - - -
	               //  X X X X X X X X X - + +
	 #x_E1C_0_E0E, //  X X X - + - - X X X + -
	               //  X X X - + - - - X X X -
	 #x_E0E_0_E1E, //  X X X - + - - - X X X -
	               //  X X X - + - - X X X X -
	 #x_FFC_0_FF8, //  X X X X X X X X X X - -
	               //  X X X X X X X X X - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_3FC, //  + - - - X X X X + - - -              67 C
	               //  + - X X X X X X X X - -
	 #x_7FE_0_E07, //  + X X X X X X X X X X -
	               //  X X X + + + + + + X X X
	 #x_E03_0_E00, //  X X X - + - - - + - X X
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X + + + + + + + + +
	 #x_E00_0_E03, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - X X
	 #x_E07_0_7FE, //  X X X - + - - - + X X X
	               //  + X X X X X X X X X X -
	 #x_3FC_0_0F0, //  + - X X X X X X X X - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_FF0_0_FFC, //  X X X X X X X X + - - -              68 D
	               //  X X X X X X X X X X - -
	 #x_FFE_0_E0F, //  X X X X X X X X X X X -
	               //  X X X + + + + + X X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E0F, //  X X X - + - - - X X X X
	               //  X X X X X X X X X X X +
	 #x_FFE_0_FFC, //  X X X X X X X X X X - -
	               //  X X X X X X X X + - - - = = =
	 #x_FF0_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_FFF_0_FFF, //  X X X X X X X X X X X X              69 E
	               //  X X X X X X X X X X X X
	 #x_FFF_0_E00, //  X X X X X X X X X X X X
	               //  X X X + + + + + + + + +
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_FF8_0_FF8, //  X X X X X X X X X - - -
	               //  X X X X X X X X X + + +
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_E00_0_FFF, //  X X X - + - - - + - - -
	               //  X X X X X X X X X X X X
	 #x_FFF_0_FFF, //  X X X X X X X X X X X X
	               //  X X X X X X X X X X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_FFF_0_FFF, //  X X X X X X X X X X X X              69 F
	               //  X X X X X X X X X X X X
	 #x_FFF_0_E00, //  X X X X X X X X X X X X
	               //  X X X + + + + + + + + +
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_FF8_0_FF8, //  X X X X X X X X X - - -
	               //  X X X X X X X X X + + +
	 #x_FF8_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X + + + + + + + + +
	 #x_E00_0_000, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -

         #x_0F0_0_3FC, //  + - - - X X X X + - - -              71 G
	               //  + - X X X X X X X X - -
	 #x_7FE_0_E07, //  + X X X X X X X X X X -
	               //  X X X + + + + + + X X X
	 #x_E03_0_E00, //  X X X - + - - - + - X X
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X + + + + + + + + +
	 #x_E1F_0_E1F, //  X X X - + - - X X X X X
	               //  X X X - + - - X X X X X
	 #x_E07_0_7FF, //  X X X - + - - - + X X X
	               //  + X X X X X X X X X X X
	 #x_3FF_0_0F3, //  + - X X X X X X X X X X
	               //  + - - - X X X X + - X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E07_0_E07,  //  X X X - + - - - + X X X              72 H
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_FFF_0_FFF, //  X X X X X X X X X X X X
	               //  X X X X X X X X X X X X
	 #x_FFF_0_E07, //  X X X X X X X X X X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_1F0_0_1F0, //  + - - X X X X X + - - -              73 I
	               //  + - - X X X X X + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_1F0_0_1F0, //  + - - X X X X X + - - -
	               //  + - - X X X X X - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_1F8_0_1F8, //  + - - X X X X X X - - -              74 J
	               //  + - - X X X X X X - - -
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + + + + + + X X X - + +
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + - - - + - X X X - - -
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + + + + + + X X X - + +
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + - - - + - X X X - - -
	 #x_738_0_338, //  + X X X + - X X X - - -
	               //  + X X X + + X X X - + +
	 #x_3F0_0_1E0, //  + - X X X X X X - - - -
	               //  + - - X X X X - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E0E_0_E0E, //  X X X - + - - - + X X X              75 K
	               //  X X X - + - - - X X X -
	 #x_E1C_0_E38, //  X X X - + - - X X X - -
	               //  X X X + + + X X X + + +
	 #x_E70_0_EE0, //  X X X - + X X X + - - -
	               //  X X X - X X X - + - - -
	 #x_FC0_0_FC0, //  X X X X X X - - + - - -
	               //  X X X X X X + + + + + +
	 #x_EE0_0_E70, //  X X X - X X X - + - - -
	               //  X X X - + X X X + - - -
	 #x_E38_0_E1C, //  X X X - + - X X X - - -
	               //  X X X + + + + X X X + +
	 #x_E0E_0_E07, //  X X X - + - - - X X X -
	               //  X X X - + - - - + X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E00_0_E00, //  X X X - + - - - + - - -              76 L
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X + + + + + + + + +
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X + + + + + + + + +
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_E00_0_FFE, //  X X X - + - - - + - - -
	               //  X X X X X X X X X X X +
	 #x_FFE_0_FFE, //  X X X X X X X X X X X -
	               //  X X X X X X X X X X X - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	  

         #x_C03_0_E07, //  X X - - + - - - + - X X              77 M
	               //  X X X - + - - - + X X X
	 #x_F0F_0_F9F, //  X X X X + - - - X X X X
	               //  X X X X X + + X X X X X
	 #x_FFF_0_EF7, //  X X X X X X X X X X X X
	               //  X X X - X X X X + X X X
	 #x_E67_0_E67, //  X X X - + X X - + X X X
	               //  X X X + + X X + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + + + + - X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E07_0_F07, //  X X X - + - - - + X X X              78 N
	               //  X X X X + - - - + X X X
	 #x_F07_0_F87, //  X X X X + - - - + X X X
	               //  X X X X X + + + + X X X
	 #x_EC7_0_EC7, //  X X X - X X - - + X X X
	               //  X X X - X X - - + X X X
	 #x_E67_0_E67, //  X X X - - X X - + X X X
	               //  X X X + + X X + + X X X
	 #x_E37_0_E3F, //  X X X - + - X X + X X X
	               //  X X X - + - X X + X X X
	 #x_E1F_0_E0F, //  X X X - + - - X X X X X
	               //  X X X + + + + + X X X X
	 #x_E0F_0_E07, //  X X X - + - - - X X X X
	               //  X X X - + - - - + X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_3FC, //  + - - - X X X X + - - -              79 O
	               //  + - X X X X X X X X - -
	 #x_70E_0_606, //  + X X X + - - - X X X -
	               //  + X X + + + + + + X X +
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_606_0_70E, //  + X X - + - - - + X X -
	               //  + X X X + + + + X X X +
	 #x_3FC_0_0F0, //  + - X X X X X X X X - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_FF8_0_FFC, //  X X X X X X X X X - - -              80 P
	               //  X X X X X X X X X X - -
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X + + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X -
	 #x_FFC_0_FF8, //  X X X X X X X X X X - -
	               //  X X X X X X X X X - + +
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_3FC, //  + - - - X X X X + - - -              81 Q
	               //  + - X X X X X X X X - -
	 #x_70E_0_606, //  + X X X + - - - X X X -
	               //  + X X + + + + + + X X +
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E37_0_E37, //  X X X - + - X X + X X X
	               //  X X X - + - X X + X X X
	 #x_61E_0_70E, //  + X X - + - - X X X X -
	               //  + X X X + + + + X X X +
	 #x_3FE_0_0F7, //  + - X X X X X X X X X -
	               //  + - - - X X X X + X X X = = =
	 #x_003_0_000, //  + - - - + - - - + - X X
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_FF8_0_FFC, //  X X X X X X X X X - - -              82 R
	               //  X X X X X X X X X X - -
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X + + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X -
	 #x_FFC_0_FF8, //  X X X X X X X X X X - -
	               //  X X X X X X X X X - + +
	 #x_EE0_0_E70, //  X X X - X X X - + - - -
	               //  X X X - + X X X + - - -
	 #x_E38_0_E1C, //  X X X - + - X X X - - -
	               //  X X X - + - - X X X - -
	 #x_E0E_0_E06, //  X X X - + - - - X X X -
	               //  X X X - + - - - + X X - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_0F0_0_3FC, //  + - - - X X X X + - - -              83 S
	               //  + - X X X X X X X X - -
	 #x_70E_0_606, //  + X X X + - - - X X X -
	               //  + X X + + + + + + X X -
	 #x_600_0_700, //  + X X - + - - - + - - -
	               //  + X X X + - - - + - - -
	 #x_3F0_0_0FC, //  + - X X X X X X + - - -
	               //  + + + + X X X X X X + +
	 #x_00E_0_006, //  + - - - + - - - X X X -
	               //  + - - - + - - - + X X -
	 #x_606_0_70E, //  + X X - + - - - + X X -
	               //  + X X X + + + + X X X +
	 #x_3FC_0_0F0, //  + - X X X X X X X X - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_FFE_0_FFE, //  x x x x x x x x x x x -              84 T
	               //  x x x x x x x x x x x -
	 #x_FFE_0_0E0, //  x x x x x x x x x x x -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E07_0_E07, //  X X X - + - - - + X X X              85 U
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_F0F_0_7FE, //  X X X X + - - - X X X X
	               //  + X X X X X X X X X X +
	 #x_3F0C_0_0F0, //  + - X X X X X X X X - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E07_0_E07, //  X X X - + - - - + X X X              86 V
	               //  X X X - + - - - + X X X
	 #x_E07_0_70E, //  X X X - + - - - + X X X
	               //  + X X X + + + + X X X +
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + - - - X X X -
	 #x_39C_0_39C, //  + - X X X - - X X X - -
	               //  + + X X X + + X X X + +
	 #x_39C_0_198, //  + - X X X - - X X X - -
	               //  + - - X X - - X X - - -
	 #x_1F8_0_0F0, //  + - - X X X X X X - - -
	               //  + + + + X X X X X + + +
	 #x_0F0_0_0F0, //  + - - - X X X X + - - -
	               //  + - - - X X X X + - - - = = =
	 #x_060_0_000, //  + - - - + X X - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E07_0_E07, //  X X X - + - - - + X X X              87 W
	               //  X X X - + - - - + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E67, //  X X X - + - - - + X X X
	               //  X X X - + X X - + X X X
	 #x_E67_0_E67, //  X X X - + X X - + X X X
	               //  X X X + + X X + + X X X
	 #x_EF7_0_FFF, //  X X X - X X X X + X X X
	               //  X X X X X X X X X X X X
	 #x_F9F_0_F0F, //  X X X X X - - X X X X X
	               //  X X X X + + + + X X X X
	 #x_E07_0_C03, //  X X X - + - - - + X X X
	               //  X X - - + - - - + - X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E0E_0_E0E, //  X X X - + - - - X X X -              88 X
	               //  X X X - + - - - X X X -
	 #x_71C_0_71C, //  + X X X + - - X X X - -
	               //  + X X X + + + X X X + +
	 #x_3B8_0_3F8, //  + - X X X - X X X - - -
	               //  + - X X X X X X X - - -
	 #x_1F0_0_1F0, //  + - - X X X X X - - - -
	               //  + + + X X X X X + + + +
	 #x_3F8_0_3B8, //  + - X X X X X X X - - -
	               //  + - X X X - X X X - - -
	 #x_71C_0_71C, //  + X X X + - - X X X - -
	               //  + X X X + + + X X X + +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E0E_0_E0E, //  X X X - + - - - X X X -              89 Y
	               //  X X X - + - - - X X X -
	 #x_71C_0_71C, //  + X X X + - - X X X - -
	               //  + X X X + + + X X X + +
	 #x_3B8_0_3F8, //  + - X X X - X X X - - -
	               //  + - X X X X X X X - - -
	 #x_1F0_0_1F0, //  + - - X X X X X - - - -
	               //  + + + X X X X X + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_FFF_0_FFF, //  X X X X X X X X X X X X              90 Z
	               //  X X X X X X X X X X X X
	 #x_FFF_0_00E, //  X X X X X X X X X X X X
	               //  + + + + + + + + X X X +
	 #x_01C_0_038, //  + - - - + - - X X X - -
	               //  + - - - + - X X X - - -
	 #x_070_0_0E0, //  + - - - + X X X + - - -
	               //  + + + + X X X + + + + +
	 #x_1C0_0_380, //  + - - X X X - - + - - -
	               //  + - X X X - - - + - - -
	 #x_700_0_FFF, //  + X X X + - - - + - - -
	               //  X X X X X X X X X X X X
	 #x_FFF_0_FFF, //  X X X X X X X X X X X X
	               //  X X X X X X X X X X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_1F8_0_1F8, //  + - - X X X X X X - - -              91 [
	               //  + - - X X X X X X - - -
	 #x_1F8_0_1C0, //  + - - X X X X X X - - -
	               //  + + + X X X + + + + + +
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + - - X X X - - + - - -
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + + + X X X + + + + + +
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + - - X X X - - + - - -
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + + + X X X + + + + + +
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + - - X X X - - + - - - = = =
	 #x_1F8_0_1F8, //  + - - X X X X X X - - -
	               //  + + + X X X X X X + + +
	 #x_1F8_0_000, //  + - - X X X X X X - - -
	               //  + - - - + - - - + - - -
	 
         #x_E00_0_E00, //  X X X - + - - - + - - -             92 \
	               //  X X X - + - - - + - - -
	 #x_700_0_700, //  + X X X + - - - + - - -
	               //  + X X X + - + + + + + +
	 #x_380_0_380, //  + - X X X - - - + - - -
	               //  + - X X X - - - + - - -
	 #x_1C0_0_1C0, //  + - - X X X - - + - - -
	               //  + + + X X X - - + + + +
	 #x_0E0_0_0E0, //  + - - - X X X + + - - -
	               //  + - - - X X X - + - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + - + +
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + - - - + - X X X - - - = = =
	 #x_01C_0_01C, //  + - - - + - - X X X - -
	               //  + + + + + + + X X X - -
	 #x_00E_0_000, //  + - - - + - - - X X X -
	               //  + - - - + - - - + - - -
	 
         #x_1F8_0_1F8, //  + - - X X X X X X - - -              93 ]
	               //  + - - X X X X X X - - -
	 #x_1F8_0_038, //  + - - X X X X X X - - -
	               //  + + + + + + X X X + + +
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + - - - + - X X X - - -
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + + + + + + X X X + + +
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + - - - + - X X X - - -
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + + + + + + X X X + + +
	 #x_038_0_038, //  + - - - + - X X X - - -
	               //  + - - - + - X X X - - - = = =
	 #x_1F8_0_1F8, //  + - - X X X X X X - - -
	               //  + + + X X X X X X + + +
	 #x_1F8_0_000, //  + - - X X X X X X - - -
	               //  + - - - + - - - + - - -
	 
         #x_0E0_0_1F0, //  + - - - X X X - + - - -              94 ^
	               //  + - - X X X X X + - - -
	 #x_3B8_0_71C, //  + - X X X - X X X - - -
	               //  + X X X + + + X X X + +
	 #x_E0E_0_000, //  X X X - + - - - X X X -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              95 _
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_FFF, //  + - - - + - - - + - - -
	               //  X X X X X X X X X X X X
	 #x_FFF_0_FFF, //  X X X X X X X X X X X X
	               //  X X X X X X X X X X X X
	 
         #x_E00_0_700, //  X X X - + - - - + - - -              96 `
	               //  + X X X + - - - + - - -
	 #x_380_0_1C0, //  + - X X X - - - + - - -
	               //  + + + X X X + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -              97 a
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_3EE_0_7FE, //  + - X X X X X - X X X -          Top of lc a
	               //  + X X X X X X X X X X -
	 #x_E1E_0_C0E, //  X X X - + - - X X X X -
	               //  X X - + + + + + X X X +
	 #x_C0E_0_C0E, //  X X - - + - - - X X X -  )       Centre of
	               //  X X - - + - - - X X X -  )       LC letters
	 #x_C0E_0_E1E, //  X X - - + - - - X X X -
	               //  X X X + + + + X X X X +
	 #x_7FE_0_3EE, //  + X X X X X X X X X X -
	               //  + - X X X X X - X X X - - = = =  Base line
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E00_0_E00, //  X X X - + - - - + - - -              98 b
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X + + + + + + + + +
	 #x_EF8_0_FFC, //  X X X - X X X X X - - -
	               //  X X X X X X X X X X - -
	 #x_F0E_0_E0E, //  X X X X + - - - X X X -
	               //  X X X - + - - - + X X +
	 #x_E06_0_E06, //  X X X - + - - - + X X -
	               //  X X X - + - - - + X X -
	 #x_E06_0_F06, //  X X X - + - - - + X X -
	               //  X X X X + + + + X X X +
	 #x_FFC_0_EF8, //  X X X X X X X X X X - -
	               //  X X X - X X X X X - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
	 
         #x_000_0_000, //  + - - - + - - - + - - -              99 c
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_3F8_0_7FC, //  + - X X X X X X X - - -
	               //  + X X X X X X X X X - -
	 #x_F0E_0_E00, //  X X X X + - - - X X X -
	               //  X X X + + + + + + + + +
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -
	 #x_E00_0_70E, //  X X X - + - - - + - - -
	               //  X X X X + + + + X X X +
	 #x_7FC_0_3F8, //  + X X X X X X X X X - - -
	               //  + - X X X X X X X - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_00E_0_00E, //  + - - - + - - - X X X -             100 d
	               //  + - - - + - - - X X X -
	 #x_00E_0_00E, //  + - - - + - - - X X X -
	               //  + + + + + + + + X X X -
	 #x_3EE_0_7FE, //  + - X X X X X - X X X -
	               //  + X X X X X X X X X X -
	 #x_E1E_0_C0E, //  X X X - + - - X X X X -
	               //  X X + + + + + + X X X +
	 #x_C0E_0_C0E, //  X X - - + - - - X X X -
	               //  X X - - + - - - X X X -
	 #x_C0E_0_E1E, //  X X - - + - - - X X X -
	               //  X X X + + + + X X X X +
	 #x_7FE_0_3EE, //  + X X X X X X X X X X -
	               //  + - X X X X X - X X X - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             101 e
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_1F8_0_7FC, //  + - X X X X X X X - - -
	               //  + X X X X X X X X X - -
	 #x_F0E_0_E0E, //  X X X X + - - - X X X -
	               //  X X X - + + + + X X X +
	 #x_FFE_0_FFC, //  X X X X X X X X X X X -
	               //  X X X X X X X X X X - -
	 #x_E00_0_F0E, //  X X X - + - - - + - - -
	               //  X X X X + + + + X X X +
	 #x_7FC_0_3F8, //  + X X X X X X X X X - -
	               //  + - X X X X X X X - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
	 
         #x_03C_0_078, //  + - - - + - X X X X - -             102 f
	               //  + - - - + X X X X - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_7FC_0_7FC, //  + X X X X X X X X X - -
	               //  + X X X X X X X X X - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_000, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             103 g
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 #x_000_0_3EE, //  + + + + + + + + + + + +
	               //  + - X X X X X - X X X -
	 #x_7FE_0_E1E, //  + X X X X X X X X X X -
	               //  X X X - + - - X X X X +
	 #x_C0E_0_C0E, //  X X - - + - - - X X X -
	               //  X X - - + - - - X X X -
	 #x_C0E_0_E1E, //  X X - - + - - - X X X -
	               //  X X X + + + + X X X X +
	 #x_7FE_0_3EE, //  + X X X X X X X X X X -
	               //  + - X X X X X - X X X - - = = =
	 #x_00E_0_C1E, //  + - - - + - - - X X X -
	               //  X X + + + + + X X X X +
	 #x_7FC_0_3F8, //  + X X X X X X X X X - -
	               //  + - X X X X X X X - - -
	 
	 
         #x_E00_0_E00, //  X X X - + - - - + - - -             104 h
	               //  X X X - + - - - + - - -
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X + + + + + + + + +
	 #x_EF8_0_FF8, //  X X X - X X X X X - - -
	               //  X X X X X X X X X X - -
	 #x_F0E_0_E0E, //  X X X X + - - - X X X -
	               //  X X X - + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X -
	 #x_E0E_0_F0E, //  X X X - + - - - X X X -
	               //  X X X - + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
	 
         #x_070_0_070, //  + - - - + X X X + - - -             105 i
	               //  + - - - + X X X + - - -
	 #x_070_0_000, //  + - - - + X X X + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_3F0, //  + - - - + - - - + - - -
	               //  + - X X X X X X + - - -
	 #x_3F0_0_070, //  + - X X X X X X - + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_078_0_03E, //  + - - - + X X X X - - -
	               //  + - - - + - X X X X X - = = =
	 #x_01E_0_000, //  + - - - + - - X X X X -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_070_0_070, //  + - - - + X X X + - - -             106 j
	               //  + - - - + X X X + - - -
	 #x_070_0_000, //  + - - - + X X X + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_3F0, //  + - - - + - - - + - - -
	               //  + - X X X X X X + - - -
	 #x_3F0_0_070, //  + - X X X X X X - + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_0F0_0_3E0, //  + - - - X X X X + - - -
	               //  + - X X X X X - + - - - = = =
	 #x_3C0_0_000, //  + - X X X X - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
	 
         #x_700_0_700, //  + X X X + - - - + - - -             107 k
	               //  + X X X + - - - + - - -
	 #x_700_0_707, //  + X X X + - - - + - - -
	               //  + X X X + + + + + X X X
	 #x_70E_0_71C, //  + X X X + - - - X X X -
	               //  + X X X + - - X X X - -
	 #x_738_0_7F0, //  + X X X + - X X X - - -
	               //  + X X X X X X X + + + +
	 #x_7E0_0_7F0, //  + X X X X X X - + - - -
	               //  + X X X X X X X + - - -
	 #x_7B8_0_71C, //  + X X X X - X X X - - -
	               //  + X X X + + + X X X + +
	 #x_70E_0_707, //  + X X X + - - - X X X -
	               //  + X X X + - - - + X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_3F0_0_3F0, //  + - X X X X X X + - - -             108 l
	               //  + - X X X X X X + - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_03F_0_01F, //  + - - - + - X X X X X X
	               //  + - - - + - - X X X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             109 m
	               //  + - - - + - - - + - - -
	 #x_000_0_39C, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_39C_0_7FE, //  + - X X X - - X X X - -
	               //  + X X X X X X X X X X -
	 #x_EF7_0_E67, //  X X X - X X X X + X X X
	               //  X X X + + X X + + X X X
	 #x_E67_0_E67, //  X X X - + X X - + X X X
	               //  X X X - + X X - + X X X
	 #x_E67_0_E07, //  X X X - + X X - + X X X
	               //  X X X + + + + + + X X X
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             110 n
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_EF8_0_FFC, //  X X X - X X X X X - - -
	               //  X X X X X X X X X X - -
	 #x_F9E_0_E0E, //  X X X X X - - X X X X -
	               //  X X X + + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X -
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X + + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             111 o
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_3F8_0_7FC, //  + - X X X X X X X - - -
	               //  + X X X X X X X X X - -
	 #x_F1E_0_E0E, //  X X X X + - - X X X X -
	               //  X X X + + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X -
	 #x_E0E_0_F1E, //  X X X - + - - - X X X -
	               //  X X X X + + + X X X X +
	 #x_7FC_0_3F8, //  + X X X X X X X X X - - -
	               //  + - X X X X X X X - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             112 p
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_EF8_0_FF8, //  X X X - X X X X X - - -
	               //  X X X X X X X X X X - -
	 #x_F1E_0_E0E, //  X X X X + - - X X X X -
	               //  X X X + + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X -
	 #x_E0E_0_F1E, //  X X X - + - - - X X X -
	               //  X X X X + + + X X X X +
	 #x_FFC_0_FF8, //  X X X X X X X X X X - -
	               //  X X X X X X X X X - - - = = =
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X + + + + + + + + +
	 #x_E00_0_E00, //  X X X - + - - - + - - -
	               //  X X X - + - - - + - - -

         #x_000_0_000, //  + - - - + - - - + - - -             113 q 
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_3EE_0_3FE, //  + - X X X X X - X X X -
	               //  + X X X X X X X X X X -
	 #x_F1E_0_70E, //  X X X X + - - X X X X -
	               //  X X X + + + + + X X X +
	 #x_70E_0_70E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X -
	 #x_70E_0_F1E, //  X X X - + - - - X X X -
	               //  X X X X + - - X X X X +
	 #x_7FE_0_3FE, //  + X X X X X X X X X X -
	               //  + - X X X X X X X X X - = = =
	 #x_00E_0_00E, //  + - - + - - - + X X X -
	               //  + - + + + + + + X X X -
	 #x_00E_0_00E, //  + - - + - - - + X X X -
	               //  + - - - + - - - X X X -

	 
         #x_000_0_000, //  + - - - + - - - + - - -             114 r
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_0EE_0_0FF, //  + - - - X X X - X X X -
	               //  + - - - X X X X X X X X
	 #x_0F3_0_0E0, //  + - - - X X X X + - X X
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             115 s
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_0F8_0_3FC, //  + - - - X X X X X - - -
	               //  + - X X X X X X X X - -
	 #x_70E_0_700, //  + X X X + - - - X X X -
	               //  + X X X + + + + + + + +
	 #x_3F8_0_0FC, //  + - X X X X X X X - - -
	               //  + - - - X X X X X X - -
	 #x_00E_0_70E, //  + - - - + - - - X X X -
	               //  + X X X + + + + X X X -
	 #x_3FC_0_0F8, //  + - X X X X X X X X - -
	               //  + - - - X X X X X - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             116 t
	               //  + - - - + - - - + - - -
	 #x_000_0_0E0, //  + - - - + - - - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_7FC_0_7FC, //  + X X X X X X X X X - -
	               //  + X X X X X X X X X + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0F0, //  + - - - X X X - + - - -
	               //  + + + + X X X X + + + +
	 #x_07E_0_03E, //  + - - - + X X X X X X -
	               //  + - - - + - X X X X X - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             117 u
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + - - - X X X -
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + + + + X X X +
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + - - - X X X -
	 #x_70E_0_79E, //  + X X X + - - - X X X -
	               //  + X X X X + + X X X X +
	 #x_3FC_0_0F0, //  + - X X X X X X X X - -
	               //  + - - - X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -

         #x_000_0_000, //  + - - - + - - - + - - -             118 v
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_E0E_0_71C, //  X X X - + - - - X X X -
	               //  + X X X + - - X X X - -
	 #x_71C_0_71C, //  + X X X + - - X X X - -
	               //  + X X X + + + X X X + +
	 #x_3B8_0_3B8, //  + - X X X - X X X - - -
	               //  + - X X X - X X X - - -
	 #x_1F0_0_1F0, //  + - - X X X X X + - - -
	               //  + + + X X X X X + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             119 w
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_E07_0_E67, //  X X X - + - - - + X X X
	               //  X X X + + X X + + X X X
	 #x_E67_0_E67, //  X X X - + X X - + X X X
	               //  X X X - + X X - + X X X
	 #x_E67_0_EF7, //  X X X - + X X - + X X X
	               //  X X X + X X X X + X X X
	 #x_7FE_0_39C, //  + X X X X X X X X X X -
	               //  + - X X X - - X X X - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             120 x
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_E07_0_70E, //  X X X - + - - - + X X X
	               //  + X X X + - - - X X X -
	 #x_39C_0_1F8, //  + - X X X - - X X X - -
	               //  + + + X X X X X X + + +
	 #x_0F0_0_0F0, //  + - - - X X X X + - - -
	               //  + - - - X X X X + - - -
	 #x_1F8_0_39C, //  + - - X X X X X X - - -
	               //  + + X X X + + X X X + +
	 #x_70E_0_E07, //  + X X X + - - - X X X -
	               //  X X X + + - - - + X X X = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             121 y
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_E07_0_E07, //  X X X - + - - - + X X X
	               //  X X X - + - - - + X X X
	 #x_70E_0_70E, //  + X X X + - - - X X X -
	               //  + X X X + + + + X X X +
	 #x_39C_0_39C, //  + - X X X - - X X X - -
	               //  + - X X X - - X X X - -
	 #x_1F8_0_0F8, //  + - - X X X X X X - - -
	               //  + + + + X X X X X + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - - = = =
	 #x_0E0_0_CE0, //  + - - - X X X - + - - -
	               //  X X + + X X X + + + + +
	 #x_FC0_0_780, //  X X X X X X - - + - - -
	               //  + X X X X - - - + - - -
	 
         #x_000_0_000, //  + - - - + - - - + - - -             122 z
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_7FE_0_7FE, //  + X X X X X X X X X X -
	               //  + X X X X X X X X X X -
	 #x_01C_0_038, //  + - - - + - - X X X - -
	               //  + + + + + + X X X + + +
	 #x_070_0_0E0, //  + - - - + X X X + - - -
	               //  + - - - X X X - + - - -
	 #x_1C0_0_380, //  + - - X X X - - + - - -
	               //  + + X X X + + + + + + +
	 #x_3FE_0_7FE, //  + X X X X X X X X X X -
	               //  + X X X X X X X X X X - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_03C_0_078, //  + - - - + - X X X X - -             123 {
	               //  + - - - + X X X X - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_3C0, //  + - - - X X X - + - - -
	               //  + + X X X X + + + + + +
	 #x_780_0_3C0, //  + X X X X - - - + - - -
	               //  + - X X X X - - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_0E0_0_078, //  + - - - X X X - + - - -
	               //  + + + + + X X X X + + +
	 #x_03C_0_000, //  + - - - + - X X X X - -
	               //  + - - - + - - - + - - -
	 
         #x_0E0_0_0E0, //  + - - - X X X - + - - -             124 |
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = =
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_000, //  + - - - X X X - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_3C0_0_1E0, //  + - X X X X - - + - - -             125 }
	               //  + - - X X X X - + - - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - -
	 #x_070_0_03C, //  + - - - + X X X + - - -
	               //  + + + + + + X X X X + +
	 #x_01E_0_03C, //  + - - - + - - X X X X -
	               //  + - - - + - X X X X - -
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + + + + + X X X + + + +
	 #x_070_0_070, //  + - - - + X X X + - - -
	               //  + - - - + X X X + - - - = = =
	 #x_070_0_1E0, //  + - - - + X X X + - - -
	               //  + + + X X X X + + + + +
	 #x_3C0_0_000, //  + - X X X X - - + - - -
	               //  + - - - + - - - + - - -
	 
	 
         #x_000_0_000, //  + - - - + - - - + - - -             126 ~
	               //  + - - - + - - - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_180, //  + - - - + - - - + - - -
	               //  + - - X X - - - + - - -
	 #x_3C0_0_7E1, //  + - X X X X - - + - - -
	               //  + X X X X X X + + + + X
	 #x_E73_0_C3F, //  X X X - + X X X + - X X
	               //  X X - - + - X X X X X X
	 #x_81E_0_00C, //  X - - - + - - X X X X -
	               //  + + + + + + + + X X + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + - - - + - - - + - - -
	 
         #x_E3B_0_E3D, //  X X X - + - X X X - X X           127 rubout
	               //  X X X - + - X X X - - X
	 #x_E3C_0_1C7, //  X X X - + - X X X - - -
	               //  + + + X X X + + + X X X
	 #x_1C7_0_1C7, //  + - - X X X - - + X X X
	               //  + - - X X X - - + X X X
	 #x_E38_0_E38, //  X X X - + - X X X - - - 
	               //  X X X + + + X X X + + -
	 #x_E38_0_1C7, //  X X X - + - X X X - - -
	               //  + - - X X X - - + X X X
	 #x_1C7_0_1C7, //  + - - X X X - - + X X X
	               //  + + + X X X + + + X X X
	 #x_E38_0_E38, //  X X X - + - X X X - - -
	               //  X X X - + - X X X - - - = = =
	 #x_E38_0_1C7, //  X X X - + - X X X - - -
	               //  + + + X X X + + + X X X
	 #x_9C7_0_DC7  //  X - - X X X - - + X X X
	               //  X X - X X X - - + X X X

  // bitmap points to the nine words giving the pixels of the character.
  { LET col = currcolour
    LET w = VALOF SWITCHON line INTO
    { CASE  0: RESULTIS bitmap!0>>16
      CASE  1: RESULTIS bitmap!0
      CASE  2: RESULTIS bitmap!1>>16
      CASE  3: RESULTIS bitmap!1
      CASE  4: RESULTIS bitmap!2>>16
      CASE  5: RESULTIS bitmap!2
      CASE  6: RESULTIS bitmap!3>>16
      CASE  7: RESULTIS bitmap!3
      CASE  8: RESULTIS bitmap!4>>16
      CASE  9: RESULTIS bitmap!4
      CASE 10: RESULTIS bitmap!5>>16
      CASE 11: RESULTIS bitmap!5
      CASE 12: RESULTIS bitmap!6>>16
      CASE 13: RESULTIS bitmap!6
      CASE 14: RESULTIS bitmap!7>>16
      CASE 15: RESULTIS bitmap!7
      CASE 16: RESULTIS bitmap!8>>16
      CASE 17: RESULTIS bitmap!8
    }

    IF (w & #b100000000000) > 0 DO drawpoint(x+ 0, y)
    IF (w & #b010000000000) > 0 DO drawpoint(x+ 1, y)
    IF (w & #b001000000000) > 0 DO drawpoint(x+ 2, y)
    IF (w & #b000100000000) > 0 DO drawpoint(x+ 3, y)
    IF (w & #b000010000000) > 0 DO drawpoint(x+ 4, y)
    IF (w & #b000001000000) > 0 DO drawpoint(x+ 5, y)
    IF (w & #b000000100000) > 0 DO drawpoint(x+ 6, y)
    IF (w & #b000000010000) > 0 DO drawpoint(x+ 7, y)
    IF (w & #b000000001000) > 0 DO drawpoint(x+ 8, y)
    IF (w & #b000000000100) > 0 DO drawpoint(x+ 9, y)
    IF (w & #b000000000010) > 0 DO drawpoint(x+10, y)
    IF (w & #b000000000001) > 0 DO drawpoint(x+11, y)
  }

  currx, curry := cx, cy
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
  // drawn provided 0 <= y < currysize.

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

    IF 0 <= y < currysize DO
    { // y is in range.
      IF leftxv !y > x DO
      { leftxv !y := x
        IF miny > y DO miny := y
        IF maxy < y DO maxy := y
      }
      IF rightxv!y < x DO
      { rightxv!y := x
        IF miny > y DO miny := y
        IF maxy < y DO maxy := y
      }
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
{ //LET curryupb = currysize-1

  leftxv  := getvec(curryupb)
  rightxv := getvec(curryupb)
  UNLESS leftxv & rightxv DO
  { sawritef("getvec failure*n")
    abort(999)
  }

  FOR y = 0 TO curryupb DO               // Initialise the relevant elements
    leftxv!y, rightxv!y := currysize, -1 // of leftxv and rightxv.
}

AND drawtriangle(x1,y1, x2,y2, x3,y3) BE
{ // Draw a 2D triangle.

  // miny and maxy will hold the least and greatest y value in the range
  // 0 to currysize-1 of any pixel in the triangle. If no such
  // pixels exists, miny will be greater than maxy.

  miny, maxy := currysize, -1
  
  // The edges of the triangle are processed in turn. Whenever a pixel
  // on an edge is found with a y value between 0 and currysize-1, if
  // the x value is less than leftxv!y this is replaced by  the x value
  // of the pixel. Similarly, rightxv!y is conditionally updated. At the
  // end all pixels between (leftxv!y,y) and (rightxv!y,y) lie in the
  // triangle and those with x values between 0 and currxsize-1 will
  // be written to the screen. The calls of setlims also set miny and
  // maxy to the lowest and highest y values in the range 0
  // to currysize-1 that have pixels in the triangle.
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
    leftxv!y, rightxv!y := currysize-1, 0
  }
}

AND drawquad(x1,y1, x2,y2, x3,y3, x4,y4) BE
{ // A quad is drawn as two tiangles
  drawtriangle(x1,y1, x2,y2, x3,y3)
  drawtriangle(x2,y2, x3,y3, x4,y4)
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

AND drawfillrect(x0,y0, x1,y1) BE
{ LET xmin, xmax = x0, x1
  LET ymin, ymax = y0, y1
  IF xmin>xmax DO xmin, xmax := x1, x0
  IF ymin>ymax DO ymin, ymax := y1, y0

  sys(Sys_sdl, sdl_fillrect, @currsurf,
      xmin, currysize-ymax, xmax-xmin+1, ymax-ymin+1, currcolour)

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



// 3D drawing functions

AND moveto3d(x, y, sz) BE
  currx3d, curry3d, currsz3d := x, y, sz

AND moveby3d(dx, dy, dsz) BE
{ currx3d  := currx3d  + dx
  curry3d  := curry3d  + dy
  currsz3d := currsz3d + dsz
}

AND drawby3d(dx, dy, dsz) BE
  drawto3d(currx3d + dx, curry3d + dy, currsz3d + dsz)

AND drawpoint3d(x, y, sz) BE
{ // Draw a 3D point on the screen at pixel position (x,y) provided it is
  // not hidden by a previous pixel at this position.
  // Note that depth increases as sz becomes more positive. This is because
  // the axes x, y and z form a left handed set. x is to the right, y is up
  // and so z is away from the camera. Into the screen is thus the positive z
  // direction.
//writef("drawpoint3d: %n %n %n*n", x, y, sz)
  IF 0<=x<currxsize & 0<=y<currysize DO
  { // The point on the screen. Now test whether it is visible.
    LET p = x+y*currxsize // The subscript of pixel (x,y) of depthv.
    IF sz <= depthv!p DO
    { // This point is in front or at the same depth as the previous point,
      // if any, so we must draw it.
      //writef("drawpoint3d: %n %n %n*n", x, y, sz)
      depthv!p := sz // Store the scaled z value in depthv.
      drawpoint(x, y)
    }
  }
}

AND drawto3d(x1, y1, sz1) BE
{ LET x , y , sz  = currx3d, curry3d, currsz3d
  LET x0, y0, sz0 = x, y, sz
  // Draw a 3D line from (x0,y0,sz0) to (x1,y1,sz1)
  
  LET dx = ABS(x1-x0)       // Magnitude of the x distance
  AND dy = ABS(y1-y0)       // Magnitude of the y distance

  LET smax = dx+dy          // The sum of the x and y steps
  LET s    = 0              // number of steps so far
  
  LET sx = x0<x1 -> 1, -1 // Unit step in x direction
  LET sy = y0<y1 -> 1, -1 // Unit step in y direction
   
  LET x, y = x0, y0         // variable to step along the line.
  LET err = dx-dy
  LET e2 = ?

  currx3d, currx3d, currsz3d := x1, y1, sz1

//writef("drawto3d: from (%i2 %i2 %i4) to (%i2 %i2 %i4)*n",
//        x0,y0,sz0, x1,y1,sz1)

  { sz := sz0 + (sz1-sz0)*s/smax
    drawpoint3d(x, y, sz)
    
    IF s>=smax RETURN
    
    e2 := 2*err
    IF e2 > -dy DO err, x, s := err-dy, x+sx, s+1 // Step in the x direction
    IF e2 <  dx DO err, y, s := err+dx, y+sy, s+1 // Step in the y direction
  } REPEAT
}

AND setlims3d(x0,y0,sz0, x1,y1,sz1) BE
{ // This is used by drawtriangle3d when drawing a filled 3D triangle
  // with hidden surface removal.
  // miny, maxy, leftxv, rightxv, leftzv and rightzv have been initialised.
  // Every pixel of the line from (x0,y0) to (x1,y1) will be inspected
  // to find all pixels that are visable.

//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)

  // Ensure y0 and y1 are in the range 0 to curryupb
  IF y0<0 DO
  {
//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
    x0  := interpolate(0, y0,y1,  x0, x1)
    sz0 := interpolate(0, y0,y1, sz0,sz1)
    y0  := 0
//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
//abort(997)
  }
  IF y0>curryupb DO
  {
//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
    x0  := interpolate(curryupb, y0,y1,  x0, x1)
    sz0 := interpolate(curryupb, y0,y1, sz0,sz1)
    y0  := curryupb
//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
//abort(997)
  }
  IF y1<0 DO
  {
//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
    x1  := interpolate(0, y0,y1,  x0, x1)
    sz1 := interpolate(0, y0,y1, sz0,sz1)
    y1  := 0
//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
//abort(997)
  }
  IF y1>curryupb DO
  {
//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
    x1  := interpolate(curryupb, y0,y1,  x0, x1)
    sz1 := interpolate(curryupb, y0,y1, sz0,sz1)
    y1  := curryupb
//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
//abort(997)
  }

//writef("setlims3d(%i5,%i5,%i5,    %i5,%i5,%i5)*n", x0,y0,sz0, x1,y1,sz1)
//writef("y0=%n and y1=%n curryupb=%n*n", y0, y1, curryupb)
//IF y0=111 & y1=69 DO abort(994)
  { LET dx = ABS(x1-x0)       // Distance in x direction.
    AND dy = ABS(y1-y0)       // Distance in y direction.

    LET x, y = x0, y0
    LET smax = dx+dy          // Total number of steps.
    LET s    = 0              // Steps so far.
  
    LET sx = x0<x1 -> 1, -1   // Unit step in the x direction
    LET sy = y0<y1 -> 1, -1   // Unit step in the y direction

    LET err = dx-dy

    { // Start of the loops that steps through all pixels on the line from
      // (x0,y0) to (x1,y1) even when some or all may not be on the screen.
      LET e2 = 2*err


      // Update info about triangle pixels at level y
      //writef("(%n %n) %i2/%i2 ", x, y, s,smax)
      //writef("left %in:%n right %n:%n*n",
      //        leftxv !y, leftzv !y,
      //        rightxv!y, rightzv!y)

      IF leftxv!y >= x DO
      { leftxv!y  := x
        //leftzv!y  := smax -> sz0 + (sz1-sz0)*s/smax, sz0
        leftzv!y  := interpolate(s, 0,smax, sz0,sz1)
        IF y<miny DO miny := y
        IF y>maxy DO maxy := y
      }
      IF rightxv!y < x DO
      { rightxv!y := x
        //rightzv!y := smax -> sz0 + (sz1-sz0)*s/smax, sz0
        rightzv!y  := interpolate(s, 0,smax, sz0,sz1)
        IF y<miny DO miny := y
        IF y>maxy DO maxy := y
      }
IF FALSE DO
IF leftzv!y=500 DO
{    writef("*nleft %n:%n right %n:%n  miny=%n maxy=%n*n",
             leftxv !y, leftzv !y,
             rightxv!y, rightzv!y,
             miny, maxy)
            abort(5556)
}
      IF s=smax DO
      { //newline()
        //abort(4227)
        RETURN
      }

      // More steps are needed.
    
      IF e2 > -dy DO err, x, s := err-dy, x+sx, s+1  // Step in the x direction
      IF e2 <  dx DO err, y, s := err+dx, y+sy, s+1  // Step in the y direction
    } REPEAT
  }
}

AND alloc3dvecs() BE
{ alloc2dvecs()

  // Now allocate and initialise vectors concerned with 3D depth
  leftzv  := getvec(curryupb)
  rightzv := getvec(curryupb)
  depthv  := getvec(depthvupb)

  UNLESS leftzv * rightzv * depthv DO
  { writef("getvec failure*n")
    abort(999)
  }
  
  FOR y = 0 TO curryupb DO
    leftzv!y, rightzv!y := currysize, -1

  FOR i = 0 TO depthvupb DO depthv!i := maxdepth
  
  setdepthlimits(1, 1_000_000_000)
}

AND drawquad3d(x1, y1, z1,
               x2, y2, z2,
	       x3, y3, z3,
	       x4, y4, z4) BE
{ // Draw a filled convex quadrilateral by drawing two triangles.
  drawtriangle3d(x1,y1,z1, x2,y2,z2, x3,y3,z3)
  drawtriangle3d(x1,y1,z1, x3,y3,z3, x4,y4,z4)
}

AND drawtriangle3d(x1, y1, z1,
                   x2, y2, z2,
		   x3, y3, z3) BE
{ // These coordinated use integer screen units. Typically 1 is the
  // distance between pixels.
  // The z values are have been scaled to improve the accuracy of
  // hidden surface removal. Currently 100 units in z correspond
  // to a distance between pixels.
  // Note that neardepth and fardepth are scaled integer depth
  // limits only set by calling the function setdepthlimits.
  
//writef("drawtriangle3d: neardepth = %n  fardepth = %n*n",
//                        neardepth,      fardepth)
//writef("drawtriangle3d: v1 = (%8i, %8i) depth=%8i*n", x1, y1, z1)
//writef("drawtriangle3d: v2 = (%8i, %8i) depth=%8i*n", x2, y2, z2)
//writef("drawtriangle3d: v3 = (%8i, %8i) depth=%8i*n", x3, y3, z3)
//writef("Triangle3d %n %n %n  %n %n %n  %n %n %n*n", x1,y1,z1, x2,y2,z2, x3,y3,z3)
//abort(2567)
  // As depth increases z becomes more positive.

  // Swap vertices to make the first the deepest and the third the least deep.
  // Depth into the screen increases as z becomes more positive. The camera is
  // at depth zero and is pointing in the positive depth direction.
  TEST z1>=z2
  THEN UNLESS z2>=z3 DO
       { TEST z1>=z3 
	 THEN drawtriangle3d(x1,y1,z1, x3,y3,z3, x2,y2,z2)
         ELSE drawtriangle3d(x3,y3,z3, x1,y1,z1, x2,y2,z2)
	 RETURN
       }
  ELSE { TEST z2>=z3
         THEN TEST z1>=z3
              THEN  drawtriangle3d(x2,y2,z2, x1,y1,z1, x3,y3,z3)
              ELSE  drawtriangle3d(x2,y2,z2, x3,y3,z3, x1,y1,z1)
         ELSE drawtriangle3d(x3,y3,z3, x2,y2,z2, x1,y1,z1)
         RETURN
       }

  // v1, v2 and v3 are ordered from deepest to least deep.
  //writef("Vertices are now in depth order: %10i %10i %10i*n", z1, z2, z3)

  // Do not display the triangle if all of its vertices are deeper
  // than fardepth or less deep than near depth.
  IF z3>fardepth | z1<neardepth RETURN

//writef("Triangle3d %n %n %n  %n %n %n  %n %n %n*n", x1,y1,z1, x2,y2,z2, x3,y3,z3)

  IF z1>fardepth TEST z2 > fardepth
  THEN { // Culling with both v1 and v2 deeper than fardepth
         //           1
         //          * *
	 //         *   *
	 //        *     2                    Replacement triangle
	 //       *    *
	 //   ---*---*------  far   =>      ----1**2-------  far
         //     *  *                           *  *
         //    * *                            * *
	 //   **	                     **
         //  3	                            3
	 x1 := interpolate(fardepth, z3,z1, x3,x1)
	 y1 := interpolate(fardepth, z3,z1, y3,y1)
	 z1 := fardepth
	 x2 := interpolate(fardepth, z3,z2, x3,x2)
	 y2 := interpolate(fardepth, z3,z2, y3,y2)
	 z2 := fardepth
	 //writef("Two verteces beyond fardepth*n")
	 //abort(7001)
	 drawtriangle3d(x1,y1,z1, x3,y3,z3, x2,y2,z2)
	 RETURN
       }
  ELSE { // Culling with only v1 deeper than fardepth
         //                   1
         //                  * *              Replacement triangles
	 //          -------*---*---  far     -------1***4---  far
	 //                *     *                  *  *  *
	 //               *       *                *     * *
	 //              *         *     =>       *        **
	 //             *           2            *           2
	 //            *           *            *          *
	 //           *          *             *         *
	 //          *         *              *        *
	 //         *        *               *       *
	 //        *       *                *      *
	 //       *      *                 *     *
         //      *     *                  *    *
         //     *    *                   *   *
	 //    *   *	                *  *
	 //   * *	               * *
	 //  **	                      **
         // 3	                     3
	 LET x4 = interpolate(fardepth, z2,z1, x2,x1)
	 LET y4 = interpolate(fardepth, z2,z1, y2,y1)
	 LET z4 = fardepth
	 x1 := interpolate(fardepth, z3,z1, x3,x1)
	 y1 := interpolate(fardepth, z3,z1, y3,y1)
	 z1 := fardepth
	 //writef("One vertex beyond fardepth*n")
	 //abort(7002)

	 drawtriangle3d(x1,y1,z1, x4,y4,z4, x2,y2,z2)
	 drawtriangle3d(x1,y1,z1, x2,y2,z2, x3,y3,z3)
         RETURN	 
       }
  
  IF z3<neardepth TEST z2<neardepth
  THEN { // Culling with only v1 deeper than neardepth
  
         //                                   Replacement triangle
         //                    1                        1
	 //                   * *           =>         * *
         //                  *   *                    *   *
	 //             ----*-----*--- near      ----3*****2--- near
	 //                *       *
	 //               *         2
	 //              *        *
	 //             *       *
	 //            *      *
	 //           *     *
	 //          *    *
	 //         *   *
	 //        *  *
	 //       * *
         //      3
	 x2 := interpolate(neardepth, z2,z1, x2,x1)
	 y2 := interpolate(neardepth, z2,z1, y2,y1)
	 z2 := neardepth
	 x3 := interpolate(neardepth, z3,z1, x3,x1)
	 y3 := interpolate(neardepth, z3,z1, y3,y1)
	 z3 := neardepth
	 //writef("Two verteces behind neardepth*n")
	 //abort(7003)
	 drawtriangle3d(x1,y1,z1, x3,y3,z3, x2,y2,z2)
	 RETURN
       }
  ELSE { // Culling with both v1 and v2 deeper than neardepth
  
         //                                   Replacement triangles
         //                    1                        1
	 //                   * *                      * *
         //                  *   *                    *   *
	 //                 *     *                  *     *
	 //                *       *                *       *
	 //               *         2              *         2
	 //              *        *      =>       *       **
	 //             *       *                *     * *
	 //            *      *                 *   *  *
	 //           *     *                  * *   *
	 //      ----*----*--------  near  ---3****4-------
	 //         *   *
	 //        *  *
	 //       * *
         //      3
	 LET x4 = interpolate(neardepth, z3,z2, x3,x2)
	 LET y4 = interpolate(neardepth, z3,z2, y3,y2)
	 LET z4 = neardepth
	 x3 := interpolate(neardepth, z3,z1, x3,x1)
	 y3 := interpolate(neardepth, z3,z1, y3,y1)
	 z3 := neardepth
	 //writef("One vertex behind neardepth*n")
	 //abort(7004)
	 drawtriangle3d(x1,y1,z1, x2,y2,z2, x3,y3,z3)
	 drawtriangle3d(x2,y2,z2, x3,y3,z3, x4,y4,z4)
         RETURN	 
       }

//writef("All three vertices are in the range near to far depth*n")

  // leftxv, leftzv, rightxv and rightzv have been allocated
  // and initialised. They will hold integer pixel values
  // representing the region to colour.
  // The elements of leftzv and rightzv will hold scaled values.
  // miny and maxy will hold the range of y values.

//writef("Triangle %n %n %n  %n %n %n  %n %n %n*n", x1,y1,z1, x2,y2,z2, x3,y3,z3)

  miny, maxy := currysize, -1

  FOR y = 0 TO currysize-1 DO leftxv!y, rightxv!y := currxsize, -1 // For safety
  
  setlims3d(x1,y1,z1, x2,y2,z2)
  setlims3d(x2,y2,z2, x3,y3,z3)
  setlims3d(x3,y3,z3, x1,y1,z1)

//chklimits()

  FOR y = miny TO maxy DO
  { // Draw the horizontal line in the triangle at level y.
    LET x0, sz0 = leftxv!y,  leftzv!y
    LET x1, sz1 = rightxv!y, rightzv!y

    //IF x0>currxsize | x1<0 LOOP // None of this raster line is visible
    
    IF x0<0 DO
    { sz0 := interpolate(0, x0, x1, sz0,sz1)
      x0  := 0
    }
    IF x1>=currxsize DO
    { sz1 := interpolate(currxsize-1, x0, x1, sz0,sz1)
      x1  := currxsize-1
    }

    IF FALSE IF x1 > x0+50 | sz0=500 DO
    //IF x0<0 | x1<0 | x0>=currxsize | x1>=currysize DO
    {
writef("drawtriangle3d: neardepth = %n  fardepth = %n*n",
                        neardepth,      fardepth)
writef("drawtriangle3d: v1 = (%8i, %8i) depth=%8i*n", x1, y1, z1)
writef("drawtriangle3d: v2 = (%8i, %8i) depth=%8i*n", x2, y2, z2)
writef("drawtriangle3d: v3 = (%8i, %8i) depth=%8i*n", x3, y3, z3)
      writef("y=%n %n:%n to %n:%n*n", y, x0, sz0, x1, sz1)
      abort(998)
    }
    
//writef("y=%i3:*n", y)
    TEST x0=x1
    THEN { // The line is parallel to the z axis so select the end point
           // with the least depth.
           LET sz = sz0 > sz1 -> sz0, sz1
//writef(" %n:%n*n", x0, sz)
           drawpoint3d(x0, y, sz)
         }

    ELSE { FOR x = x0 TO x1 DO
           { LET sz = interpolate(x, x0,x1, sz0,sz1)
//writef(" %n:%n*n", x, sz)
             drawpoint3d(x, y, sz)
           }
	 }
//writef("*n")
//abort(5558)
//updatescreen()
//delay(20)

    // Unset leftxv and rightxv at level y
    leftxv!y, rightxv!y := currxsize, -1
    leftzv!y, rightzv!y := 0, 0  // Not really necessary.
  }
}

AND chklimits() BE
{
  writef("chklimits: miny=%n maxy=%n*n", miny, maxy)
  FOR y = 0 TO currysize-1 IF leftxv!y<=rightxv!y DO
    writef("y=%i3: %n:%n %n:%n*n", y, leftxv!y, leftzv!y, rightxv!y, rightzv!y)
  newline()
}

AND setdepthlimits(near, far) BE
{ IF near<1 DO near := 1
  IF far > 1_000_000_000 DO far := 1_000_000_000
  neardepth, fardepth := near, far
}



