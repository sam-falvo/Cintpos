GET "libhdr"
GET "sdl.h"
GET "sdl.b"                 // Insert the library source code
.
GET "libhdr"
GET "sdl.h"


GLOBAL {
  col_black:ug
  col_blue
  col_green
  col_yellow
  col_red
  col_majenta
  col_cyan
  col_white
  col_darkred
  col_gray
  col_lightyellow
  col_lightred
}

LET start() = VALOF
{ UNLESS initsdl() DO
  { writef("ERROR: Unable to initialise SDL*n")
    RESULTIS 0
  }

  mkscreen("drawf test", 600, 400)
//sawritef("engine returned from mkscreen*n")
  col_black       := maprgb(  0,   0,   0)
  col_blue        := maprgb(  0,   0, 255)
  col_green       := maprgb(  0, 255,   0)
  col_yellow      := maprgb(  0, 255, 255)
  col_red         := maprgb(255,   0,   0)
  col_majenta     := maprgb(255,   0, 255)
  col_cyan        := maprgb(255, 255,   0)
  col_white       := maprgb(255, 255, 255)
  col_darkred     := maprgb(128,   0,   0)
  col_gray        := maprgb( 70,  70,  70)
  col_lightyellow := maprgb(128, 255, 255)
  col_lightred    := maprgb(255, 128, 128)

  fillsurf(col_gray)

  setcolour(col_cyan)
  drawf(250, 30, "Draw test")

  selectfont(12)
  moveto(10, 250)

  setcolour(col_white)

  FOR ch = 32 TO 127 DO
  { IF ch MOD 32 = 0 DO drawch('*n')
    drawch(ch)
  }
  
  selectfont(18)
  moveto(10, 150)

  setcolour(col_red)

  FOR ch = 32 TO 127 DO
  { IF ch MOD 32 = 0 DO drawch('*n')
    drawch(ch)
  }
  

  updatescreen()   //Update the screen
  sdldelay(530_000) //Pause for 10 secs
  closesdl()       //Quit SDL

  RESULTIS 0
}
