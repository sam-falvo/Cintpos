/*
Graphics Library for BCPL

This library allow the user to create a .bmp file representing an
image.

Implemented by Martin Richards (c) August 2012

The header file is g/graphics.h

This file should be included (by GET) after libhdr has been included
and after the manifest g_grfbase has been declared if the default
value declared in libhdr is not suitable. This library uses region of
the global vector from g_grfbase upwards.

For many applications using the SDL or GL libraries is preferable.

21/07/2020
Allowed both 8-bit to 24-bit pixels eliminating the
need for the colour map.

09/12/11
Started inplementation
*/

LET opengraphics(xmax, ymax, mode) = VALOF
{ // xmax is the number of pixels per row
  // ymax is the number of row
  // Allocate the rectangular pixel array and colour map.
  // Return TRUE if successful
  
  xsize, ysize := xmax, ymax
  canvas, palettev := 0, 0

  bmpmode := mode
  UNLESS bmpmode=mode8bit | bmpmode=mode8bitalt DO bmpmode := mode24bit
  
  palettev := 0  // No colour table needed for 24-bit pixels

  IF bmpmode=mode8bit DO
  { // The default colours for 8-bit pixels
    col_black   :=   0
    col_majenta :=  30
    col_blue    :=  70
    col_cyan    := 110
    col_green   := 150
    col_yellow  := 190
    col_red     := 230
    col_white   := 255

    palettev := initpalettev()
    bpp := 1
  }

  IF bmpmode=mode8bitalt DO
  { // Alternative colours for 8-bit pixels
    col_black   :=  0               // Palette position of #x000000
    col_majenta :=  2 + (6*6+0)*6+5 // Palette position of #xFF00FF
    col_blue    :=  2 + (0*6+0)*6+5 // Palette position of #x0000FF
    col_cyan    :=  2 + (0*6+5)*6+5 // Palette position of #x00FFFF
    col_green   :=  2 + (0*6+5)*6+0 // Palette position of #x00FF00
    col_yellow  :=  2 + (6*6+5)*6+0 // Palette position of #xFFFF00
    col_red     :=  2 + (6*6+0)*6+0 // Palette position of #xFF0000
    col_white   :=  255             // Palette position of #xFFFFFF

    palettev := initpalettevalt()
    bpp := 1
  }

  IF bmpmode=mode24bit DO
  { // The colours for 24-bit pixels, #xrrggbb
    col_black   :=   #x000000
    col_majenta :=   #xFF00FF
    col_blue    :=   #x0000FF
    col_cyan    :=   #x00FFFF
    col_green   :=   #x00FF00
    col_yellow  :=   #xFFFF00
    col_red     :=   #xFF0000
    col_white   :=   #xFFFFFF

    bpp := 3
  }

//writef("bpp=%n bmpmode=%n col_blue=%x6*n", bpp, bmpmode, col_blue)
  
  currcolour    := col_black
  currx, curry  := 0, 0
  rowlen        := bpp*xsize               // Without rounding up
  canvassize    := ysize * rowlen          // Number of bytes of bytes in the canvas
  canvasupb     := canvassize/bytesperword // UPB in words of the canvas vector.

  canvas := getvec(canvasupb) // Allocate the canvas vector
  UNLESS canvas RESULTIS FALSE
  //writef("canvas=%n canvassize=%n cansvasupb=%n*n", canvas, canvassize, canvasupb)
  
  // Set the entire canvas to white
  //currcolour := col_white
  //fillrect(0, 0, xsize/2-1, ysize/2-1)
//  abort(1000)
  currcolour := col_white
  FOR x = 0 TO xsize-1 FOR y = 0 TO ysize-1 DO drawpoint(x, y)

  //FOR i = 0 TO canvassize-1 DO
  //{ IF i MOD 15 = 0 DO writef("*n%i4: ", i)
  //  writef(" %x2", canvas%i)
  //}
  //newline()
  //abort(1000)
  RESULTIS TRUE
}

AND closegraphics() BE
{ IF canvas   DO freevec(canvas)
  IF palettev DO freevec(palettev)
}

AND wrgraph(filename) BE
{ // Output the canvas as a .bpm format file to filename, scale 300 DPI
  
  LET xres          = 11811  // Pixels per metre at 300 DPI (1m=39.37ins)
  LET yres          = xres
  LET hdrsize       = 14
  LET infohdrsize   = 40
  LET paletsize     = bmpmode=mode24bit -> 0, 4*256
  LET dataoffset    = hdrsize + infohdrsize + paletsize
  LET stream        = findoutput(filename)
  LET ostream       = output()
  LET rowlenrounded = (rowlen+3) & -4         // Rounded up to a multiple of 4

  UNLESS stream DO
  { writef("Trouble with file: %s*n", filename)
    RETURN
  }

  selectoutput(stream)

  // Example bmp file for a 2x2 image using 24-bit pixels

  //                     0  1
  //                  0  B  G
  //                  1  R  W

  // is the following sequence of bytes

  // 00   42 4D             BM                 The Id field
  // 02   46 00 00 00       70 bytes = 54+16   Size of the BMP file
  // 06   00 00             Unused
  // 08   00 00             Unused
  // 0A   36 00 00 00       54 bytes           Offset of the pixel data
  
  // 0E   28 00 00 00       40 bytes           Number of bytes of DIB data
  // 12   02 00 00 00       2 pixels (left to right)
  // 16   02 00 00 00       2 pixels (bottom to top)
  // 1A   01 00             1 plane            Number of colour planes
  // 1C   18 00             24 bits            Number of bits per pixel
  // 1E   00 00 00 00       0                  BI_RGB, no compression
  // 22   10 00 00 00       16 bytes           Size of raw pixel data including padding
  // 26   13 0B 00 00       2835 pixels/metre horizontal    Base on DPI
  // 2A   13 0B 00 00       2835 pixels/metre vertical      Base on DPI
  // 2E   00 00 00 00       0 colours          Number of colours in the palette
  // 32   00 00 00 00       0 important colours

  //                        In bmp point(0,0) is the top left pixel
  
  // 36   00 00 FF                             Red   pixel at (0,1)
  // 39   FF FF FF                             White pixel at (1,1)
  // 3C   00 00             Padding
  // 3E   FF 00 00                             Blue  pixel at (0,0)
  // 41   00 FF 00                             Green pixel at (1,0)
  // 44   00 00             Padding

  // Write the header
  wr1('B'); wr1('M')               // 00 "BM"
  wr4(dataoffset + canvassize)     // 02 File size in bytes
  wr4(0)                           // 06 Unused
  wr4(dataoffset)                  // 0A File offset of pixel data

  // Write the Info header
  wr4(40)                          // 0E Size of info header = 40
  wr4(xsize)                       // 12 Width in pixels
  wr4(ysize)                       // 16 Height in pixels
  wr2(1)                           // 1A Number of planes, must = 1
  wr2(bmpmode=mode24bit -> 24, 8)  // 1C 8 bits per pixel, could be 16, 24 0r 32
  wr4(0)                           // 1E No compression
  wr4(rowlenrounded*ysize)         // 22 Size of pixel data including padding
  wr4(xres)                        // 26 Horizontal resolution in pixels per meter
  wr4(yres)                        // 2A Vertical   resolution in pixels per meter
  wr4(bmpmode=mode24bit -> 0, 256) // 2E Number of colours actually used, could be 0
  wr4(0)                           // 32 All colours are important, generally ignored

  // Write the palette if using 8-bit pixels
  UNLESS bmpmode=mode24bit FOR i = 0 TO 255 DO wr4(palettev!i)

//sawritef("*nwrgraph: writing picture %nx%n using pixels*n", xsize, ysize)
  FOR y = ysize-1 TO 0 BY -1 DO
  { // Output rows from bottom to top
    LET yrow = y*rowlen 

    FOR x = 0 TO rowlen-1 DO // bpp is 1 or 3
    { LET a = canvas%(yrow + x)
      wr1(a)
    }
    FOR y = rowlen TO rowlenrounded-1 DO wr1(0) // Pad up to next 4 byte boundary
  }

fin:
  IF stream DO endstream(stream)
  selectoutput(ostream)
}

AND initpalettev() = VALOF
{ palettev := getvec(255)
  UNLESS palettev RESULTIS 0

  interpolate(   0,             0,      0,      0,  // black
               col_majenta,   255,      0,    255   // red-blue
             )
  interpolate( col_majenta+1, 255,      0,    255,  // red-blue
               col_blue,        0,      0,    255   // blue
             )
  interpolate( col_blue+1,      0,      0,    255,  // blue
               col_cyan,        0,    255,    255   // blue-green
             )
  interpolate( col_cyan+1,      0,    255,    255,  // blue-green
               col_green,       0,    255,      0   // green
             )
  interpolate( col_green+1,     0,    255,      0,  // green
               col_yellow,    255,    255,      0   // green-red
             )
  interpolate( col_yellow+1,  255,    255,      0,  // green-red
               col_red,       255,      0,      0   // red 
             )
  interpolate (col_red+1,     255,      0,      0,  // red 
                  255,        255,    255,    255   // White
             )
  //sawritef("*nColour table*n")
  //FOR i = 0 TO 255 DO
  //{ IF i MOD 8 = 0 DO sawrch('*n')
  //  sawritef(" %x6", palettev!i)
  //}
  //sawrch('*n')
  RESULTIS palettev    
}

AND interpolate(p, r1, g1, b1,
                q, r2, g2, b2) BE
{ //writef("p=%i3 q=%i3 rgb1=(%n,%n,%n) rgb2(=(%n,%n,%n)*n", p,q, r1,g1,b1, r2,g2,b2)
  FOR i = p TO q DO
  { LET r = (r1*(q-i)+r2*(i-p))/(q-p)
    LET g = (g1*(q-i)+g2*(i-p))/(q-p)
    LET b = (b1*(q-i)+b2*(i-p))/(q-p)
    palettev!i := r<<16 | g<<8 | b
    //writef("%i3: %x6*n", i, palettev!i)
  }
}

AND initpalettevalt() = VALOF
{ // This function creates an alternative palette
  // This is based on red having 7 intensities and green and blue
  // having 6 intensities all equally spaced between 0 and 255.
  // This gives 7x6x6 = 252 diferent colours in palette positions
  // 2 to 253. Palette positions 0 and are bloth black and positions
  // 254 and 255 are both white.
  LET ctab = getvec(255)
  UNLESS ctab RESULTIS 0

  ctab!0 := #x000000
  ctab!1 := #x000000
  
  FOR r = 0 TO 6 FOR g = 0 TO 5 FOR b = 0 TO 5 DO
  { LET p = 2 + (r*6+g)*6+b // Palette position
    LET cr = 255*r/6
    LET cg = 255*g/5
    LET cb = 255*b/5
    ctab!p := cr<<16 | cg<<8 | cb
    //sawritef("r=%n g=%n b=%n    p=%i3 colour=%x6*n", r, g, b, p, ctab!p)
    //abort(1000)
  }
  ctab!254 := #xFFFFFF
  ctab!255 := #xFFFFFF

  //{ sawritef("*nColour table*n")
  //  FOR i = 0 TO 255 DO
  //  { IF i MOD 8 = 0 DO sawrch('*n')
  //    sawritef(" %x6", ctab!i)
  //  }
  //  sawrch('*n')
  //}
  RESULTIS ctab    
}


AND wr1(b) BE
{ binwrch(b)
}

AND wr2(w) BE
{ LET s = @w
  binwrch(s%0)
  binwrch(s%1)
}

AND wr4(w) BE
{ LET s = @w
  binwrch(s%0)
  binwrch(s%1)
  binwrch(s%2)
  binwrch(s%3)
}

AND setcolour(col) BE currcolour := col

AND drawpoint(x, y) BE IF 0<=x<xsize & 0<=y<ysize DO
{ // Plot a pixel at (x,y) with colour currcolour provided that it is in range.
  // Place (0,0) at the bottom left hand corner of the image.
  // Positive x is to the righr.
  // Positive y is up.
  LET p = (ysize-1-y)*rowlen + bpp*x  // Byte position of the pixel

  TEST bmpmode=mode24bit
  THEN { // Write 24-bit pixels
         canvas%(p+0) := currcolour     // Blue
         canvas%(p+1) := currcolour>>8  // Green
         canvas%(p+2) := currcolour>>16 // Red
       }
  ELSE { canvas%p := currcolour          // For 8-bit pixels
       }
}

AND drawpoint33(x, y) BE
{ // Plot a 3x3 point
  FOR i = -1 TO 1 FOR j = -1 TO 1 DO drawpoint(x+i, y+j)
}

AND drawch(ch) BE TEST ch='*n'
THEN { currx, curry := 10, curry-14 // Advance to the start of the next line
     }
ELSE { LET x, y = currx+1, curry
       FOR line = 0 TO 11 DO
         write_ch_slice(currx, curry+11-line, ch, line)
       currx := currx+9
     }


AND write_ch_slice(x, y, ch, line) BE
{ LET col = currcolour  // Save the cuttrny plot colour

  // Writes the horizontal slice of the given character.

  LET i = (ch&#x7F) - '*s'
  LET charbase = TABLE // Needs correction !!!
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
    
    currcolour := (w>>7 & 1) > 0 -> col, col_white
    drawpoint(x+0, y)
    currcolour := (w>>6 & 1) > 0 -> col, col_white
    drawpoint(x+1, y)
    currcolour := (w>>5 & 1) > 0 -> col, col_white
    drawpoint(x+2, y)
    currcolour := (w>>4 & 1) > 0 -> col, col_white
    drawpoint(x+3, y)
    currcolour := (w>>3 & 1) > 0 -> col, col_white
    drawpoint(x+4, y)
    currcolour := (w>>2 & 1) > 0 -> col, col_white
    drawpoint(x+5, y)
    currcolour := (w>>1 & 1) > 0 -> col, col_white
    drawpoint(x+6, y)
    currcolour := (w    & 1) > 0 -> col, col_white
    drawpoint(x+7, y)
    currcolour := col_white
    drawpoint(x+8, y)
  }
  currcolour := col  // Restore the current plot colour
}

AND drawstr(x, y, s) BE
{ moveto(x, y)
  FOR i = 1 TO s%0 DO drawch(s%i)
}

AND drawf(x, y, form, a, b, c, d, e, f, g, h) BE
{ LET oldwrch = wrch
  LET s = VEC 256/bytesperword
  drawfstr := s
  drawfstr%0 := 0
  wrch := drawwrch
  writef(form, a, b, c, d, e, f, g, h)
  wrch := oldwrch
  drawstr(x, y, drawfstr)
}

AND drawwrch(ch) BE
{ LET strlen = drawfstr%0 + 1
  drawfstr%strlen := ch
  drawfstr%0 := strlen 
}

AND moveto(x, y) BE
{ currx, curry := x, y
}

AND moveby(dx, dy) BE
{ currx, curry := currx+dx, curry+dy
}

AND drawto(x, y) BE
{ // This is Bresenham's algorithm
  LET dx = ABS(x-currx)
  AND dy = ABS(y-curry)
  LET sx = currx<x -> 1, -1
  LET sy = curry<y -> 1, -1
  LET err = dx-dy
  LET e2 = ?

  { drawpoint(currx, curry)
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
  } REPEAT
}

AND drawby(dx, dy) BE drawto(currx+dx, curry+dy)

AND drawrect(x, y, w, h) BE
{ LET xmin, xmax = x, x+w
  LET ymin, ymax = y, y+h
  IF xmin>xmax DO { xmin := xmax; xmax := x }
  IF ymin>ymax DO { ymin := ymax; ymax := y }
//sawritef("drawrect: %i4 %i4 %i4 %i4*n",xmin,ymin,xmax,ymax)
  FOR p = xmin TO xmax DO
  { drawpoint(p, ymin)
    drawpoint(p, ymax)
  }
  FOR p = ymin+1 TO ymax-1 DO
  { drawpoint(xmin, p)
    drawpoint(xmax, p)
  }
}

AND fillrect(x, y, w, h) BE
{ LET xmin, xmax = x, x+w
  LET ymin, ymax = y, y+h
  IF xmin>xmax DO { xmin := xmax; xmax := x }
  IF ymin>ymax DO { ymin := ymax; ymax := y }

  FOR p = xmin TO xmax FOR q = ymin TO ymax DO
  { drawpoint(p, q)
  }
}

AND drawrndrect(x, y, w, h, radius) BE
{ LET xmin, xmax = x, x+w
  LET ymin, ymax = y, y+h
  LET r = radius
  LET f, ddf_x, ddf_y, x, y = ?, ?, ?, ?, ?

  IF xmin>xmax DO { xmin := xmax; xmax := x }
  IF ymin>ymax DO { ymin := ymax; ymax := y }

  // Correct the radius if necessary
  IF r<0 DO r := 0
  IF r+r>xmax-xmin DO r := (xmax-xmin)/2
  IF r+r>ymax-ymin DO r := (ymax-ymin)/2

  // First draw everything other than the rounded corners
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
}

AND fillrndrect(x, y, w, h, radius) BE
{ LET xmin, xmax = x, x+w
  LET ymin, ymax = y, y+h
  LET r = radius
  LET f, ddf_x, ddf_y, x, y = ?, ?, ?, ?, ?
  LET lastx, lasty = 0, 0

  IF xmin>xmax DO { xmin := xmax; xmax := x }
  IF ymin>ymax DO { ymin := ymax; ymax := y }

  // Correct the radius is necessary
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
}

AND drawcircle(x, y, radius) BE
{ // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  LET f = 1 - radius
  LET ddf_x = 1
  LET ddf_y = -2 * radius
  LET p = 0
  LET q = radius
  drawpoint(x, y+radius)
  drawpoint(x, y-radius)
  drawpoint(x+radius, y)
  drawpoint(x-radius, y)

  WHILE p<q DO
  { // ddf_x = 2*p + 1
    // ddf_y = -2 * q
    // f = p*p + q*q - radius*radius + 2*p - q + 1
    IF f>=0 DO
    { q := q-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    p := p+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    drawpoint(x+p, y+q)
    drawpoint(x-p, y+q)
    drawpoint(x+p, y-q)
    drawpoint(x-p, y-q)
    drawpoint(x+q, y+p)
    drawpoint(x-q, y+p)
    drawpoint(x+q, y-p)
    drawpoint(x-q, y-p)
  }
}

AND fillcircle(x, y, radius) BE
{ // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  LET f = 1 - radius
  LET ddf_x = 1
  LET ddf_y = -2 * radius
  LET p = 0
  LET q = radius
  LET lastx, lasty = 0, 0
  drawpoint(x, y+radius)
  drawpoint(x, y-radius)
  FOR p = x-radius TO x+radius DO drawpoint(p, y)

  WHILE p<q DO
  { // ddf_x = 2*p + 1
    // ddf_y = -2 * q
    // f = p*p + q*q - radius*radius + 2*p - q + 1
    IF f>=0 DO
    { q := q-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    p := p+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    drawpoint(x+p, y+q)
    drawpoint(x-p, y+q)
    drawpoint(x+p, y-q)
    drawpoint(x-p, y-q)
    drawpoint(x+q, y+p)
    drawpoint(x-q, y+p)
    drawpoint(x+q, y-p)
    drawpoint(x-q, y-p)
    UNLESS p=lastx DO
    { FOR fx = x-q+1 TO x+q-1 DO
      { drawpoint(fx, y+p)
        drawpoint(fx, y-p)
      }
      lastx := p
    }
    UNLESS q=lasty DO
    { FOR fx = x-p+1 TO x+p-1 DO
      { drawpoint(fx, y+q)
        drawpoint(fx, y-q)
      }
      lasty := q
    }
  }
}
