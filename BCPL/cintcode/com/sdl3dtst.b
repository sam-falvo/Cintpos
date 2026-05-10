/*
This test the sdl 3d drawing functions.

Implemented by Martin Richards (c) Aug 2020

History

08/-8/2020
This tests the use of scaled integer in depthv rather than floating point numbers.
*/

GET "libhdr"
GET "sdl.h"
GET "sdl.b"
.
GET "libhdr"
GET "sdl.h"


LET start() = VALOF
{ LET mes = VEC 256/bytesperword
  LET FLT h, FLT w = 500.0, 600.0
  //LET FLT h, FLT w = 50.0, 60.0
  
  initsdl()
  mkscreen("SDL 3D Test", 800, 700)
  zfac := 100.0
  //zfac :=  10.0 // This shows the effect of a small scaling factor.

 
  FOR i = -5 TO 5 FOR j = -5 TO 5 DO
  { LET col_red   = maprgb(255,  0,  0)
    LET col_green = maprgb(  0,255,  0)
    LET col_blue  = maprgb(150,150,255)
    LET col_black = maprgb( 0,  0,   0)
    LET col_white = maprgb(255,255,255)
    LET FLT x0, FLT y0 = 100.0,       50.0
    LET FLT x1, FLT y1 = 100.0+w/2.0, 50.0+h
    LET FLT x2, FLT y2 = 100.0+w,     50.0
    LET fi, fj = FLOAT i, FLOAT j
    
    fillsurf(col_blue)
    setcolour(col_green)
    drawtriangle3d(x0,y0,0.0,  x1-20.0,y1,0.0,  x2,y2,0.0)
    updatescreen()
    setcolour(col_black)
    drawf( 170, 25, "%i2, %i2, %i2  %i2, %i2, %i2  %i2, %i2, %i2  zfac=%9.2f",
                     FIX x0,       FIX y0,  0,
    	             FIX(x1-20.0), FIX y1,  0,
    		     FIX x2,       FIX y2,  0,
        	     zfac)
    setcolour(col_red)
    drawtriangle3d(x0,y0+fi,fi,  x1+20.0,y1,fj,  x2,y2-fi, -fi)
    updatescreen()
    setcolour(col_white)
    drawf( 170, 10, "%i2, %i2, %i2  %i2, %i2, %i2  %i2, %i2, %i2  zfac=%9.2f",
                     FIX x0,      (FIX y0)+i,  i,
		     (FIX x1)+20, FIX y1,      j,
		     FIX x2,      (FIX y2)-i, -i,
		     zfac)
    updatescreen()
    writef("i=%i2  j=%i2*n", i, j)
    //abort(5557)
    sdldelay(0_500)
  }

  //Quit SDL
  sys(Sys_sdl, sdl_quit)

  writef("End of demo*n")

  RESULTIS 0
}

