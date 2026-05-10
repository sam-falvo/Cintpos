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
  //LET FLT h, FLT w = 600.0, 700.0
  LET FLT h, FLT w = 10.0, 20.0
  
  initsdl()
  mkscreen("SDL 3D Test", 800, 700)
  zfac := 128

 
  { LET col_red   = maprgb(255,  0,  0)
    LET col_green = maprgb(  0,255,  0)
    LET col_blue  = maprgb(150,150,255)
    LET FLT x0, FLT y0 = 100.0,       50.0
    LET FLT x1, FLT y1 = 100.0+w/2.0, 50.0+h
    LET FLT x2, FLT y2 = 100.0+w,     50.0
    
    fillsurf(col_blue)
    setcolour(col_red)
    FOR i = 0 TO 50 DO
    { LET FLT fi = FLOAT i
      drawpoint3d(70.0+fi, 80.0+fi, 25.0-fi)
      updatescreen()
      //sdldelay(200)
    }
    drawtriangle3d(200.0, 100.0,0.0,  300.0, 200.0, 25.0,  400.0, 150.0, -25.0)

    updatescreen()

    //abort(5557)
    sdldelay(1_000)
  }

  //Quit SDL
  sys(Sys_sdl, sdl_quit)

  writef("End of demo*n")

  RESULTIS 0
}

