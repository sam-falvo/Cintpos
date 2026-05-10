/*
This is a simple demo of the BCPL interface to the SDL Graphics library.

Implemented by Martin Richards (c) July 2012

14/09/2019
Modified to run with the new SDL library on 32 and 64 bit BCPL on 32
and 64 bit machines.
*/

GET "libhdr"
GET "sdl.h"
GET "sdl.b"
.
GET "libhdr"
GET "sdl.h"

GLOBAL {
  hello:ug // For the demo.bmp image surface, a machine
  hello1   // address possibly needing two BCPL words.
  
  fmt      // For the screen format, also possibly needing
  fmt1     // two BCPL words.
}

LET start() = VALOF
{ LET mes = VEC 256/bytesperword

  initsdl()
  mkscreen("SDL Demo", 640, 480)

  hello  := 0  // To hold the machine address of the loaded image.
  hello1 := 0
  
  fillsurf(maprgb(200, 100, 80))
  updatescreen()
  sdldelay(500);

  //Load image
  sys(Sys_sdl, sdl_loadbmp, "demo.bmp", @hello)

  //Apply image to screen
  blitsurfrect(@hello, 0, @screen, 0, 450)
  updatescreen()
  //writef("Displaying demo.bmp");
  sdldelay(500);

  fillsurf(maprgb(80, 100, 50)) // Fill the screen with another colour.
  updatescreen()
  sdldelay(500);

  // Draw some shapes
  drawdemo(@format)
  updatescreen()

  //Pause for 2 secs
  sdldelay(2000);

  //Free the loaded image
  freesurface(@hello);

  //Quit SDL
  sys(Sys_sdl, sdl_quit)

  writef("End of demo*n")

  RESULTIS 0
}

AND drawdemo(formatptr) BE
{ LET c_white = maprgb(255, 255, 255)
  LET c_gray  = maprgb(200, 200, 200)
  LET c_dgray = maprgb( 64,  64,  64)
  LET c_cyan  = maprgb( 32, 255, 255)
  LET c_red   = maprgb(255,   0,   0)
  LET c_blue  = maprgb(  0,   0, 255)

  //writef("*ndrawdemo: colours white=%8x gray=%8x dgray=%8x cyan=%8x*n",
  //        c_white, c_gray, c_dgray, c_cyan)

  //sawritef("*ndrawdemo: *
  //          *calling fillrect screenptr=%n rect=%n %n %n %n col=%8x*n",
  //          @screen, 200, 300, 100, 100, c_cyan+c_red)
  delay(500)
  sys(Sys_sdl, sdl_fillrect, @screen, 200,300,100,100, c_cyan)
  updatescreen()
  delay(500)

  //sawritef("*ndrawdemo: calling drawline @screen=%n %n %n %n %n col=%8x*n",
  //                           @screen, 100, 100,  30,   0, c_red)
  delay(500)
  
  sys(Sys_sdl, sdl_drawline, @screen, 100, 100,  30,   0, c_red)
  updatescreen()
  delay(500)

  sys(Sys_sdl, sdl_drawline, @screen,  30,   0, 100, 100, c_white)
  sys(Sys_sdl, sdl_drawline, @screen, 100, 100,  30,   0, c_white)
  sys(Sys_sdl, sdl_drawline, @screen,   0,   0, 300, 200, c_white)
  delay(500)

  setcolour(c_red)
  drawtriangle(300,400, 250,20, 400,450)
  updatescreen()
  delay(500)

  setcolour(c_white)
  drawf(400, 350, "Here is some text")
  updatescreen()
  delay(500)
  
  setcolour(c_blue)
  drawtriangle(150,150, 250,250, 150,350)
  updatescreen()
  delay(2000)
}
