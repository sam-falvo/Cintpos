/*
A simple drawing pakage for BCPL

This library used to create a .bmp files representing
simple images.

Implemented by Martin Richards (c) 11 Sep 2021.

This is a reworking of the old library g/graphics.b

The header file is g/bdrawlib.h

This library uses region of the global vector from g_bdrawbase
upwards.  GET "libhdr.h" declares a default value for g_bdrawbase but
the user can redefine this constant if needed before GETing
bdrawlib.h.

History
06 Jan 2025
Made all drawing functions preserve the value of currpen.

02 Dec 2024
Changed selectfont to take a width 8, 12 or 16 rather than a
height 12, 18 or 24.

*/

LET openbdraw(xmax, ymax) = VALOF
{ // xmax is the number of pixels per row
  // ymax is the number of row
  // Allocate the rectangular pixel array and colour map.
  // Return TRUE if successful
  
  xsize, xupb := xmax, xmax-1
  ysize, yupb := ymax, ymax-1
  canvas := 0

  // The colours for 32-bit pixels, #x00rrggbb
  col_black   :=   #x000000
  col_majenta :=   #xFF00FF
  col_blue    :=   #x0000FF
  col_cyan    :=   #x00FFFF
  col_green   :=   #x00FF00
  col_yellow  :=   #xFFFF00
  col_red     :=   #xFF0000
  col_white   :=   #xFFFFFF

  currcol      := col_black
  currx, curry := 0, 0
  
  canvasupb    := xsize*ysize-1 // UPB in words of the canvas vector.
  canvas := getvec(canvasupb)   // Allocate the canvas vector
 
  UNLESS canvas RESULTIS FALSE

  currpen := penS3
  FOR i = 0 TO canvasupb DO canvas!i := col_white

  selectfont(16)
  // Set pixelspermetre to correspond to 12 16x24 chars per inch.
  // Pixel width of 12 chars is 12*(fontW+charHsep) corresponding to 1 inch.
  // 1 inch = 25.4mm
  // Number of inches per metre is 1000 / 25.4 = 39.37
  // So pixels per metre is 12*(fontW+charHsep) * 39.37

  pixelspermetre := 12 * (fontW+charHsep) * 3937 / 100   // = 8976
  //sawritef("pixelspemetre = %n*n", pixelspermetre)
  //abort(999)
//sawritef("opendraw: xsize=%n ysize=%n*n", xsize, ysize)
  RESULTIS TRUE
}

AND closebdraw() BE
{ IF canvas DO freevec(canvas)
}

AND wrbmp(filename) BE
{ // Output the canvas as a .bpm format file to filename, scale 300 DPI
  
  LET xres          = 11811  // Pixels per metre at 300 DPI (1m=39.37ins)
  LET yres          = xres
  LET hdrsize       = 14
  LET infohdrsize   = 40
  LET paletsize     = 0
  LET dataoffset    = hdrsize + infohdrsize
  LET stream        = findoutput(filename)
  LET ostream       = output()
  LET rowlen        = xsize*3          // 3 bytes per pixel in this bmp format
  LET rowlenrounded = (rowlen+3) & -4  // Rounded up to a multiple of 4
  LET pixeldatasize = rowlenrounded*ysize // Size of pixel data in bytes 

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
  // 22   10 00 00 00       16 bytes           raw pixel data with padding
  // 26   13 0B 00 00       2835 pixels/metre horizontal    Base on DPI
  // 2A   13 0B 00 00       2835 pixels/metre vertical      Base on DPI
  // 2E   00 00 00 00       0 colours          Number of colours in the palette
  // 32   00 00 00 00       0 important colours

  //                        In a bmp file the top left pixel is first
  
  // 36   00 00 FF                             Red   pixel at (0,1)
  // 39   FF FF FF                             White pixel at (1,1)
  // 3C   00 00             Padding, end of first raster line
  
  // 3E   FF 00 00                             Blue  pixel at (0,0)
  // 41   00 FF 00                             Green pixel at (1,0)
  // 44   00 00             Padding, end of last raster line

  // Write the header
  
  wr1('B'); wr1('M')            // 00 "BM"
  wr4(dataoffset+pixeldatasize) // 02 File size in bytes
  wr4(0)                        // 06 Unused
  wr4(dataoffset)               // 0A File offset of pixel data

  // Write the Info header
  wr4(40)                       // 0E Size of info header = 40
  wr4(xsize)                    // 12 Width in pixels
  wr4(ysize)                    // 16 Height in pixels
  wr2(1)                        // 1A Number of planes, must = 1
  wr2(24)                       // 1C 24 bits per pixel
  wr4(0)                        // 1E No compression
  wr4(rowlenrounded*ysize)      // 22 Size of pixel data including padding
  wr4(xres)                     // 26 Horizontal resolution in pixels per meter
  wr4(yres)                     // 2A Vertical   resolution in pixels per meter
  wr4(0)                        // 2E Number of colours actually used, could be 0
  wr4(0)                        // 32 All colours are important, ignored

  // No palette data to write

//sawritef("*nwrbmp: writing picture %nx%n using pixels*n", xsize, ysize)

  FOR y = yupb TO 0 BY -1 DO
  { // Write the rows from bottom to top
    LET yrow = canvas + y*xsize
    LET rowbytelength = 0
    FOR x = 0 TO xupb DO
    { LET col = yrow!x
      wr1(col     & 255 ) // Blue       The order required by bmp
      wr1(col>> 8 & 255 ) // Green
      wr1(col>>16 & 255 ) // Red
      rowbytelength := rowbytelength+1
    }

    // Pad the row with zero bytes until the length is a multiple of 4
    UNTIL rowbytelength MOD 4 = 0 DO
    { wr1(0)
      rowbytelength := rowbytelength+1
    }
  }

fin:
  IF stream DO endstream(stream)
  selectoutput(ostream)
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

AND setcolour(col) BE currcol := col

AND penS1(x, y) BE IF 0<=x<=xupb & 0<=y<=yupb DO
{ // Draw a pixel at (x,y) with colour currcol
  // (x,y) is the position of the bottom left hand pixel
  LET p = (yupb-y)*xsize + x  // Position of the pixel in canvas
  canvas!p := currcol
}

// Square pens from size 2 to 5
AND penS2(x, y) BE FOR i = y-1 TO y   DO penH2(x, i)
AND penS3(x, y) BE FOR i = y-1 TO y+1 DO penH3(x, i)
AND penS4(x, y) BE FOR i = y-2 TO y+1 DO penH4(x, i)
AND penS5(x, y) BE FOR i = y-2 TO y+2 DO penH5(x, i)

// Pens that are horizontal lines of length 2 to 5 pixels
AND penH2(x, y) BE FOR i = x-1 TO x   DO penS1(i, y)
AND penH3(x, y) BE FOR i = x-1 TO x+1 DO penS1(i, y)
AND penH4(x, y) BE FOR i = x-2 TO x+1 DO penS1(i, y)
AND penH5(x, y) BE FOR i = x-2 TO x+2 DO penS1(i, y)

// Pens that are vertical lines pf length 2 to 5 pixels
AND penV2(x, y) BE FOR i = y-1 TO y   DO penS1(x, i)
AND penV3(x, y) BE FOR i = y-1 TO y+1 DO penS1(x, i)
AND penV4(x, y) BE FOR i = y-2 TO y+1 DO penS1(x, i)
AND penV5(x, y) BE FOR i = y-2 TO y+2 DO penS1(x, i)

AND penR24(x, y) BE
{ // Plot the pixel pattern:       # #
  //                               # #
  //                               # o     o is at (x,y)
  //                               # #
  FOR i = y-1 TO y+2 DO penS1(x-1, i)
  FOR i = y-1 TO y+2 DO penS1(x,   i)
}

AND penR42(x, y) BE
{ // Plot the pixel pattern:       # o # #     o is at (x,y)
  //                               # # # #
  FOR i = x-1 TO x+2 DO penS1(i, y-1)
  FOR i = x-1 TO x+2 DO penS1(i, y)
}

AND drawarrow(dir, x, y, len) BE
{ // Draw an arrow head with point at (x,y) of size len
  // ponting in the direction specified by n as follows
  // n=0 right
  // n=05   NE
  // n=1 up
  // n=15   NW
  // n=2 left
  // n=25   SW
  // n=3 down
  // n=35   SE
  LET xl, xc, xr = 0, 0, 0
  LET yl, yc, yr = 0, 0, 0
  LET lenby3 = len/3
  LET lenby4 = len*5/10
  SWITCHON dir INTO
  { DEFAULT: writef("Bad direction %n for arrow*n", dir)
             abort(999)
	     RETURN
    //     *(x-len, y+len/3)
    //     - - * 
    //     - - - - *
    //     - - - - - - *
    //     - - - - - - - - *(x,y)
    //     - - - - - - *
    //     - - - - *
    //     - - * 
    //     *(x-len, y+len/3)
    CASE 0: moveto(x,y); drawby(-len,  lenby3)   // Left side
	    moveto(x,y); drawby(-len, -lenby3)   // Right side
	    RETURN
 
    //     - - - - - - - - - - - - *(x,y)
    //     - - - - - - - - - * - o -
    //     - - - - - - * - - - o - -
    //     - - - * - - - - - o - * -
    //     *(x-len, y-len/4) - - - -
    //     - - - - - - - o - - - - -
    //     - - - - - - o - - - * - -
    //     - - - - - o - - - - - - -
    //     - - - - o - - - - - - - -
    //     - - - o - - - - - * - - -
    //     - - o - - - - - - - - - -
    //     - o - - - - - - - - - - -
    //     o - - - - - - - *(x-len/4, y-len)
    CASE 05:moveto(x,y); drawby(-len,    -lenby4) // Left side
	    moveto(x,y); drawby(-lenby4, -len)    // Right side
	    RETURN

    //     - - - - - - *(x,y)
    //     - - - - - - o - - - - - -
    //     - - - - - * o * - - - - -
    //     - - - - - - o - - - - - -
    //     - - - - * - o - * - - - -
    //     - - - - - - o - - - - - -
    //     - - - * - - o - - * - - -
    //     - - - - - - o - - - - - -
    //     - - * - - - o - - - * - -
    //     - - - - - - o - - - - - -
    //     - *(x-len/3, y-len) - *(x+len/3, y-len)
    CASE 1: moveto(x,y); drawby(-lenby3, -len)   // Left side
	    moveto(x,y); drawby( lenby3, -len)   // Right side
	    RETURN

    //     *(x,y)
    //     - o - *
    //     - - o - - - *
    //     - * - o - - - - - *
    //     - - - - o - - - - - - - *(x+len, y-len/4)
    //     - - - - - o - - - - - - -
    //     - - * - - - o - - - - - -
    //     - - - - - - - o - - - - -
    //     - - - - - - - - o - - - -
    //     - - - * - - - - - o - - -
    //     - - - - - - - - - - o - -
    //     - - - - - - - - - - - o -
    //     - - - - *(x+len/4, y-len)
    CASE 15:moveto(x,y); drawby( len,    -lenby4) // Left side
	    moveto(x,y); drawby( lenby4, -len)    // Right side
	    RETURN

    //     - - - - - - - - *(x+len, y+len/3)
    //     - - - - - - * - -
    //     - - - - * - - - -
    //     - - * - - - - - -
    //     *(x,y)o o o o o o
    //     - - * - - - - - -
    //     - - - - * - - - -
    //     - - - - - - * - -
    //     - - - - - - - - *(x+len, y-len/3)

    CASE 2: moveto(x,y); drawby( len, -lenby3)   // Left side
	    moveto(x,y); drawby( len,  lenby3)   // Right side
	    RETURN

    //     - - - - *(x+len/4, y+len)
    //     - - - - - - - - - - - o -
    //     - - - - - - - - - - o - -
    //     - - - * - - - - - o - - -
    //     - - - - - - - - o - - - -
    //     - - - - - - - o - - - - -
    //     - - * - - - o - - - - - -
    //     - - - - - o - - - - - - -
    //     - - - - o - - - - - - - *(x+len, y+len/4)
    //     - * - o - - - - - *
    //     - - o - - - *
    //     - o - *
    //     *(x,y)
    CASE 25: moveto(x,y); drawby( len,    lenby4) // Left side
	     moveto(x,y); drawby( lenby4, len)    // Right side
	     RETURN

    //     - *(x-len/3, y+len) - *(x+len/3, y+len)
    //     - - - - - - o - - - - - -
    //     - - * - - - o - - - * - -
    //     - - - - - - o - - - - - -
    //     - - - * - - o - - * - - -
    //     - - - - - - o - - - - - -
    //     - - - - * - o - * - - - -
    //     - - - - - - o - - - - - -
    //     - - - - - * o * - - - - -
    //     - - - - - - o - - - - - -
    //     - - - - - - *(x,y)
    CASE 3: moveto(x,y); drawby( lenby3,  len)   // Left side
	    moveto(x,y); drawby(-lenby3,  len)   // Right side
	    RETURN

    //     o - - - - - - - *(x-len/4, y+len)
    //     - o - - - - - - - - - - -
    //     - - o - - - - - - - - - -
    //     - - - o - - - - - * - - -
    //     - - - - o - - - - - - - -
    //     - - - - - o - - - - - - -
    //     - - - - - - o - - - * - -
    //     - - - - - - - o - - - - -
    //     *(x-len, y+len/4) - - - -
    //     - - - * - - - - - o - * -
    //     - - - - - - * - - - o - -
    //     - - - - - - - - - * - o -
    //     - - - - - - - - - - - - *(x,y)
    CASE 35: moveto(x,y); drawby(-lenby4, len)    // Left side
	     moveto(x,y); drawby(-len,    lenby4) // Right side
	     RETURN
  }
}

AND drawgrid(sep, pen, colour) BE
{ LET x = 0
  LET y = 0
  LET prevpen = currpen
  currpen := pen
  currcol := colour
  UNTIL x > xupb DO
  { moveto(x, 0)
    drawto(x, yupb)
    x := x+sep
  }
  UNTIL y > yupb DO
  { moveto(   0, y)
    drawto(xupb, y)
    y := y+sep
  }
  currpen := prevpen
}

AND selectfont(w) BE
{  charLmargin := 10

  IF w=8 DO
  { fontW, fontH       :=  8, 12
    charHsep, charVsep :=  2,  3
    charmidleveloffset :=  7
    charleveloffset    :=  0
    write_ch_slice := write_ch_slice12
    RETURN
  }
  
  IF w=12 DO
  { fontW, fontH       := 12, 18
    charHsep, charVsep :=  2,  4
    charmidleveloffset := 11
    charleveloffset    :=  0
    write_ch_slice := write_ch_slice18
    RETURN
  }
  
  IF w=16 DO
  { fontW, fontH       :=  16, 24
    charHsep, charVsep :=  3,  4
    charmidleveloffset := 14
    charleveloffset    :=  0
    write_ch_slice := write_ch_slice24
    RETURN
  }
  
  writef("Bad argument for fontselect, w=%n*n", w)
  abort(999)
}

AND drawch(ch) BE TEST ch='*n'
THEN { currx := charLmargin
       curry := curry-fontH-charVsep
     }
ELSE { IF fontH=24 DO
       { // Draw vertical slices from left to right
         FOR line = 0 TO fontW-1 DO
           write_ch_slice(currx+line, curry-charleveloffset, ch, line)
         currx := currx + fontW + charHsep
	 RETURN
       }
       // Draw horizontal slices from top to bottom
       FOR line = 0 TO fontH-1 DO
         write_ch_slice(currx, curry+fontH-1-line-charleveloffset, ch, line)
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
	             //  + - - - + - - -     6   midline
	             //  + - - - + - - -
	             //  + + + + + + + +
	 #x00000000, //  + - - - + - - - = = 3   base line
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
  { LET col = currcol
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

    IF (w & #b10000000) > 0 DO penS1(x+0, y)
    IF (w & #b01000000) > 0 DO penS1(x+1, y)
    IF (w & #b00100000) > 0 DO penS1(x+2, y)
    IF (w & #b00010000) > 0 DO penS1(x+3, y)
    IF (w & #b00001000) > 0 DO penS1(x+4, y)
    IF (w & #b00000100) > 0 DO penS1(x+5, y)
    IF (w & #b00000010) > 0 DO penS1(x+6, y)
    IF (w & #b00000001) > 0 DO penS1(x+7, y)

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
	               //  + + + + X X X + + + + +      10   mid line
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - -
	 #x_000_0_000, //  + - - - + - - - + - - -
	               //  + + + + + + + + + + + +
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + - - - X X X - + - - - = = = 4   base line
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
	 #x_E1C_0_FFF, //  X X X - + - - X X X - -
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
	 #x_E0F_0_FFE, //  X X X - + - - - X X X X
	               //  X X X X X X X X X X X +
	 #x_FFC_0_FF0, //  X X X X X X X X X X - -
	               //  X X X X X X X X + - - - = = =
	 #x_000_0_000, //  + - - - + - - - + - - -
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
	 #x_3FC_0_0F0, //  + - X X X X X X X X - -
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
	 #x_E06_0_F0E, //  X X X - + - - - + X X -
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
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
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
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
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
	 #x_F1E_0_E0E, //  X X X X + - - X X X X -
	               //  X X X + + + + + X X X +
	 #x_E0E_0_E0E, //  X X X - + - - - X X X -
	               //  X X X - + - - - X X X -
	 #x_E0E_0_F1E, //  X X X - + - - - X X X -
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
	 #x_3B8_0_3FC, //  + - X X X - X X X - - -
	               //  + - X X X X X X X X - -
	 #x_3CC_0_380, //  + - X X X X - - X X - -
	               //  + + X X X + + + + + + +
	 #x_380_0_380, //  + - X X X - - - + - - -
	               //  + - X X X - - - + - - -
	 #x_380_0_380, //  + - X X X - - - + - - -
	               //  + + X X X + + + + + + +
	 #x_380_0_380, //  + - X X X - - - + - - -
	               //  + - X X X - - - + - - - = = =
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
	 #x_0E0_0_0E0, //  + - - - X X X - + - - -
	               //  + + + + X X X + + + + +
	 #x_0E0_0_7FC, //  + - - - X X X - + - - -
	               //  + X X X X X X X X X - -
	 #x_7FC_0_0E0, //  + X X X X X X X X X - -
	               //  + + + + X X X + + + + +
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
	 #x_7FE_0_7FE, //  + X X X X X X X X X X -
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
	 
         #x_E38_0_E38, //  X X X - + - X X X - - -           127 rubout
	               //  X X X - + - X X X - - -
	 #x_E38_0_1C7, //  X X X - + - X X X - - -
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
  { LET col = currcol
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

    IF (w & #b100000000000) > 0 DO penS1(x+ 0, y)
    IF (w & #b010000000000) > 0 DO penS1(x+ 1, y)
    IF (w & #b001000000000) > 0 DO penS1(x+ 2, y)
    IF (w & #b000100000000) > 0 DO penS1(x+ 3, y)
    IF (w & #b000010000000) > 0 DO penS1(x+ 4, y)
    IF (w & #b000001000000) > 0 DO penS1(x+ 5, y)
    IF (w & #b000000100000) > 0 DO penS1(x+ 6, y)
    IF (w & #b000000010000) > 0 DO penS1(x+ 7, y)
    IF (w & #b000000001000) > 0 DO penS1(x+ 8, y)
    IF (w & #b000000000100) > 0 DO penS1(x+ 9, y)
    IF (w & #b000000000010) > 0 DO penS1(x+10, y)
    IF (w & #b000000000001) > 0 DO penS1(x+11, y)
  }

  currx, curry := cx, cy
}

AND write_ch_slice24(x, y, ch, line) BE
{ // Writes the horizontal slice of the given 24x16 character.
  // CURRENTLY UNDER DEVELOPMENT
  // This uses vertical slices rather than the horizontal
  // ones previously used. Each character is represented
  // by 16 words, each holding 24 bits. The values are in
  // binary in an order that is easy to proof read.
  
  // x is the x postion of the slice
  // y is the position of its lowest pixel
  // line is between 0 and fontW-1 indicating which vertical
  // slice to draw from left to right.
  
  LET cx, cy = currx, curry
  LET offset = 16 * ((ch&127) - 32)
  // offset is the subscript of the character in the following table.
  LET bitmap = offset + TABLE
         // Each character has 16 24-bit vertical slices packed
	 // in BCPL words
         // bitmap points to the 16 words of the bitmap for ch.
	 
      //           *        13 mid line    6 base line
      //           *         *             *               //     32 space
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *               //     33 !
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_1_1_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_1_1_1_1_0_0_0,  //==
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_1_1_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *               //     34 "
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    35 #
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    36 $
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_0_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0,  //==
      #b_0_0_1_1_1_0_0_0_0_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    37 %
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_0_0_1_1_0_0_0_0,
      #b_0_1_1_1_1_1_0_0_0_0_0_0_0_0_1_1_0_0_1_1_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_1_1_1_1_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_1_1_1_1_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_0_0_1_1_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0,
      #b_0_1_1_0_0_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0,
      #b_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *           x           *                    38 &
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_0_0_0_1_1_0_0_0_1_1_1_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_1_1_0_0_0_0_1_1_0_1_1_1_0_0_0_1_1_0_0_0_0_0_0,  //==
      #b_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_1_1_1_1_1_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_1_1_0_0_0_1_1_1_0_1_1_1_0_0_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                      39 '
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    40 (
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0,  //==
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0,  //==
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    41 )
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0,  //==
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0,  //==
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0,
      #b_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    42 *
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_0_0_0_1_1_1_0_0_0_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_1_1_1_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_1_1_1_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_0_0_0_1_1_1_0_0_0_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    43 +
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    44 ,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_1_1_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    45 -
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    46 .
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    47 /
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    48 0
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_1_1_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_1_1_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    49 1
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    50 2
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_1_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_1_1_1_1_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    51 3
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_1_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    52 4
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_1_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_1_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    53 5
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    54 6
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    55 7
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    56 8
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_1_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_1_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    57 9
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    58 :
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    59 ;
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_0_0_0_0_0_0_1_1_0_0_1_1_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    60 <
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    61 =
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    62 >
      #b_0_0_0_0_0_0_0_0_0_0_0_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    63 ?
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_1_1_1_1_0_0_1_1_1_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0_1_1_1_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    64 @
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_1_1_1_0_0_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_0_0_0_1_1_0_0_1_1_0_0_0_0_0_0,
      #b_0_1_1_0_0_0_0_1_1_1_1_1_1_1_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_1_1_1_1_1_1_1_1_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_1_1_0_0_0_0_1_1_0_0_1_1_0_0_0_0_0_0,  //==
      #b_1_1_0_0_0_0_1_1_0_0_0_0_1_1_0_0_1_1_0_0_0_0_0_0,  //==
      #b_1_1_0_0_0_0_1_1_0_0_0_0_1_1_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_1_1_1_1_1_1_1_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_1_1_0_0_0_0_1_1_1_1_1_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    65 A
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    66 B
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    67 C
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    68 D
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //                                   *	 
      //           *                       *                    69 E
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    70 F
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    71 G
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                     72 H
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //                                   *	 
      //           *                       *                    73 I
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    74 J
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    75 K
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_1_1_1_1_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    76 L
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    77 M
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //                                   *	 
      //           *                       *                    77 N
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    79 O
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    80 P
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    81 Q
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_1_1_1_1_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_1_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    82 R
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_1_1_1_1_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    83 S
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_1_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    84 T
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    85 U
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    86 V
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    87 W
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    88 X
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    89 Y
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    90 Z
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_1_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_1_1_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_1_1_1_1_0_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    91 [
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   92 \
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    93 ]
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    94 ^
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    95 _
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    96 `
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    97 a
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    98 b
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                    99 c
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   100 d
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   101 e
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_0_0_0_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_1_1_1_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //                                   *	 
      //           *                       *                   102 f
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   103 g
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_1_1_1_1_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   104 h
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   105 i
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_1_1_1_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   106 j
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_1_1_1_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   107 k
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   108 l
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,  //==
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   109 m
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   110 n
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //                                   *	 
      //           *                       *                   111 o
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   112 p
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   113 q 
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   114 r
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   115 s
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_0_0_0_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   116 t
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   117 u
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   118 v
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   119 w
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   120 x
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_0_0_0_0_0_0_0_0_0_0_0_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_0_0_0_0_0_0_0_0_0_0_0_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   121 y
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_1_1_0_0_0,  //==
      #b_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1_1_0_1_1_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_1_1_1_1_0,
      #b_0_0_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_1_1_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0_0_0_0_0_1_1_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0,
      #b_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   122 z
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_1_1_1_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_1_1_1_0_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_1_1_1_0_0_1_1_1_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_1_1_1_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_1_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_1_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   123 {
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_0_0_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   124 |
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,  //==
      #b_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   125 }
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_1_1_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0,
      #b_0_1_1_1_1_1_1_1_1_1_0_0_1_1_1_1_1_1_1_1_1_0_0_0,
      #b_1_1_1_1_1_1_1_1_1_0_0_0_0_1_1_1_1_1_1_1_1_1_0_0,  //==
      #b_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_0_0,  //==
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0,
      #b_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_0_0,
      #b_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_1_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                   126 ~
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,  //==
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      #b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
      //                                   *	 
      //           *                       *                 127 rubout
      #b_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1,  //==
      #b_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0,  //==
      #b_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0,
      #b_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1,
      #b_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1_0_0_0_0_1_1_1_1
      //                                   *	 


  // bitmap points to the 16 words giving the pixels of the character.
  
  { LET w = bitmap!(fontW-1-line)
    // w identifies the pixels of the vertical slice of character ch
    // The least significant bit corresponds to the lowest pixel of
    // the slice, is the one at (x,y).
    
  //IF ch=33 DO
  //{
  //  IF line=0 DO
  //  { writef("bitmap for character ch=%n*n", ch)
  //    FOR i = 0 TO 15 DO sawritef("%i2: %24b*n", i, bitmap!i)
  //  }
    
  //  sawritef("Bitmap %24b for ch=%n line=%i2 x=%n y=%n*n",
  //                   w,       ch,   line,    x,   y)
  //  //abort(6372)
    
  //}
    WHILE w DO
    { IF (w&1)>0 DO
      { penS1(x, y)
        //IF ch=33 DO
	//{ sawritef("w=%24b (%n,%n)*n", w, x, y)
        //  //abort(1357)
	//}
      }
      w, y := w>>1, y+1
    }
  }

  currx, curry := cx, cy
}

AND drawstr(x, y, s) BE
{ moveto(x, y)
  FOR i = 1 TO s%0 DO drawch(s%i)
}

AND drawstrcentred(x, y, s) BE drawstr(x-strpixels(s)/2, y, s)

AND strpixels(s) = VALOF
{ LET chlen = s%0
  LET pixellength = (fontW+charHsep) * chlen
  // Assuming no newline characters
  IF chlen>0 DO pixellength := pixellength - charHsep
  RESULTIS pixellength
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

  { currpen(currx, curry)
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
  { currpen(p, ymin)
    currpen(p, ymax)
  }
  FOR p = ymin+1 TO ymax-1 DO
  { currpen(xmin, p)
    currpen(xmax, p)
  }
}

AND fillrect(x, y, w, h) BE
{ LET xmin, xmax = x, x+w
  LET ymin, ymax = y, y+h
  IF xmin>xmax DO { xmin := xmax; xmax := x }
  IF ymin>ymax DO { ymin := ymax; ymax := y }

  FOR p = xmin TO xmax FOR q = ymin TO ymax DO
  { penS1(p, q)
  }
}

AND drawrndrect(x, y, w, h, r) BE
{ LET xmin, xmax = x, x+w
  LET ymin, ymax = y, y+h
  LET f, ddf_x, ddf_y, x, y = ?, ?, ?, ?, ?

  IF xmin>xmax DO { xmin := xmax; xmax := x }
  IF ymin>ymax DO { ymin := ymax; ymax := y }

  // Correct the radius if necessary
  IF r<0 DO r := 0
  IF r+r>xmax-xmin DO r := (xmax-xmin)/2
  IF r+r>ymax-ymin DO r := (ymax-ymin)/2

  // First draw everything other than the rounded corners
  FOR x = xmin+r TO xmax-r DO
  { currpen(x, ymin)
    currpen(x, ymax)
  }
  FOR y = ymin+r+1 TO ymax-r-1 DO
  { currpen(xmin, y)
    currpen(xmax, y)
  }
  
  // Now draw the rounded corners
  // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  f := 1 - r
  ddf_x := 1
  ddf_y := -2 * r
  x := 0
  y := r

  currpen(xmax, ymin+r)
  currpen(xmin, ymin+r)
  currpen(xmax, ymax-r)
  currpen(xmin, ymax-r)

  WHILE x<y DO
  { // ddf_x = 2*x + 1
    // ddf_y = -2 * y
    // f = x*x + y*y - r*r + 2*x - y + 1
    IF f>=0 DO
    { y := y-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    x := x+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    currpen(xmax-r+x, ymax-r+y) // octant 2
    currpen(xmin+r-x, ymax-r+y) // Octant 3
    currpen(xmax-r+x, ymin+r-y) // Octant 7
    currpen(xmin+r-x, ymin+r-y) // Octant 6
    currpen(xmax-r+y, ymax-r+x) // Octant 1
    currpen(xmin+r-y, ymax-r+x) // Octant 4
    currpen(xmax-r+y, ymin+r-x) // Octant 8
    currpen(xmin+r-y, ymin+r-x) // Octant 5
  }
}

AND fillrndrect(x, y, w, h, r) BE
{ LET xmin, xmax = x, x+w
  LET ymin, ymax = y, y+h
  LET f, ddf_x, ddf_y, x, y = ?, ?, ?, ?, ?
  LET lastx, lasty = 0, 0

  IF xmin>xmax DO { xmin := xmax; xmax := x }
  IF ymin>ymax DO { ymin := ymax; ymax := y }

  // Correct the radius is necessary
  IF r<0 DO r := 0
  IF r+r>xmax-xmin DO r := (xmax-xmin)/2
  IF r+r>ymax-ymin DO r := (ymax-ymin)/2

  FOR x = xmin TO xmax FOR y = ymin+r TO ymax-r DO
  { penS1(x, y)
    penS1(x, y)
  }

  // Now draw the rounded corners
  // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  f := 1 - r
  ddf_x := 1
  ddf_y := -2 * r
  x := 0
  y := r

  penS1(xmax, ymin+r)
  penS1(xmin, ymin+r)
  penS1(xmax, ymax-r)
  penS1(xmin, ymax-r)

  WHILE x<y DO
  { // ddf_x = 2*x + 1
    // ddf_y = -2 * y
    // f = x*x + y*y - r*r + 2*x - y + 1
    IF f>=0 DO
    { y := y-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    x := x+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    penS1(xmax-r+x, ymax-r+y) // octant 2
    penS1(xmin+r-x, ymax-r+y) // Octant 3
    penS1(xmax-r+x, ymin+r-y) // Octant 7
    penS1(xmin+r-x, ymin+r-y) // Octant 6
    penS1(xmax-r+y, ymax-r+x) // Octant 1
    penS1(xmin+r-y, ymax-r+x) // Octant 4
    penS1(xmax-r+y, ymin+r-x) // Octant 8
    penS1(xmin+r-y, ymin+r-x) // Octant 5

    UNLESS x=lastx DO
    { FOR fx = xmin+r-y+1 TO xmax-r+y-1 DO
      { penS1(fx, ymax-r+x)
        penS1(fx, ymin+r-x)
      }
      lastx := x
    }
    UNLESS y=lasty DO
    { FOR fx = xmin+r-x+1 TO xmax-r+x-1 DO
      { penS1(fx, ymax-r+y)
        penS1(fx, ymin+r-y)
      }
    }
  }
}
AND penS1p(x, y) BE
{ penS1(x, y)
  writef("penS1f: (%i4, %i4)*n", x, y)
}

AND drawarc90(n, x, y, r) BE
{ // This draws a 90 degree arc of a circle radius r in quadrant n
  // with centre ar (x,y).
  drawarc45(2*n,   x, y, r)
  drawarc45(2*n+1, x, y, r)
}

AND drawarc45(n, x, y, r) BE
{ // This draws a 45 degree arc of a circle radius r in octant n
  // with centre ar (x,y) using currpen.
  LET f = 1 - r
  LET ddf_x = 1
  LET ddf_y = -2 * r
  LET p = 0
  LET q = r
//writef("drawarc45: n=%n  (%n,%n) r=%n*n", n, x, y, r)
  SWITCHON n INTO
  { DEFAULT:                 ENDCASE
    CASE 7:
    CASE 0: currpen(x+r, y); ENDCASE
    CASE 1:
    CASE 2: currpen(x, y+r); ENDCASE
    CASE 3:
    CASE 4: currpen(x-r, y); ENDCASE
    CASE 5:
    CASE 6: currpen(x, y-r); ENDCASE
    }
    //abort(9917)
  WHILE p<q DO
  { // ddf_x = 2*p + 1
    // ddf_y = -2 * q
    // f = p*p + q*q - r*r + 2*p - q + 1
    IF f>=0 DO
    { q := q-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    p := p+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    SWITCHON n INTO
    { DEFAULT:                   ENDCASE
      CASE 0: currpen(x+q, y+p); ENDCASE
      CASE 1: currpen(x+p, y+q); ENDCASE
      CASE 2: currpen(x-p, y+q); ENDCASE
      CASE 3: currpen(x-q, y+p); ENDCASE
      CASE 4: currpen(x-q, y-p); ENDCASE
      CASE 5: currpen(x-p, y-q); ENDCASE
      CASE 6: currpen(x+p, y-q); ENDCASE
      CASE 7: currpen(x+q, y-p); ENDCASE
    }
  }
}

AND drawcircle(x, y, r) BE
{ // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  LET f = 1 - r
  LET ddf_x = 1
  LET ddf_y = -2 * r
  LET p = 0
  LET q = r
  currpen(x, y+r)
  currpen(x, y-r)
  currpen(x+r, y)
  currpen(x-r, y)

  WHILE p<q DO
  { // ddf_x = 2*p + 1
    // ddf_y = -2 * q
    // f = p*p + q*q - r*r + 2*p - q + 1
    IF f>=0 DO
    { q := q-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    p := p+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    currpen(x+p, y+q)
    currpen(x-p, y+q)
    currpen(x+p, y-q)
    currpen(x-p, y-q)
    currpen(x+q, y+p)
    currpen(x-q, y+p)
    currpen(x+q, y-p)
    currpen(x-q, y-p)
  }
}

AND drawellipse(x, y, rx, ry) BE
{ // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  LET r = x >= ry -> rx, ry
  LET f = 1 - r
  LET ddf_x = 1
  LET ddf_y = -2 * r
  LET p = 0
  LET q = r
  LET sp, sq = 0, 0
  LET f = 1 - r
  LET fx, fy  = rx, ry // The x and y factor are mx/r and my/r
  LET mx, my = ry, rx
  IF rx<ry DO mx, my := rx, ry
  
  //currpen(x, y+ry)
  //currpen(x, y-ry)
  //currpen(x+rx, y)
  //currpen(x-rx, y)

  WHILE p<=q DO
  { // ddf_x = 2*p + 1
    // ddf_y = -2 * q
    // f = p*p + q*q - r*r + 2*p - q + 1
    IF f>=0 DO
    { q := q-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    p := p+1
    ddf_x := ddf_x + 2
    f := f + ddf_x

    // (q, p) is the coord of a point in an octant of a circle radius r
    //        starting at (0, r)
    sp := rx - q
    sq := p
    // (sq, sp) is the coord of a point in an octant of a circle radius r
    //          starting at (0, 0)
    
    //fillcircle(x+r, y, 10)
    //fillcircle(x, y+r, 10) 
    //fillcircle(x,   y, 10)
    //setcolour(col_green)
    //currpen(x+sq, y+sp)       // Curve relative to (0,0)
    //setcolour(col_black)    
    currpen(x+(r-sp)*fx/r, y+(0+sq)*fy/r)    // Octant 0
    currpen(x+(0+sq)*fx/r, y+(r-sp)*fy/r)    // Octant 1
    currpen(x+(0-sq)*fx/r, y+(r-sp)*fy/r)    // Octant 2
    currpen(x-(r-sp)*fx/r, y+(0+sq)*fy/r)    // Octant 3
    currpen(x-(r-sp)*fx/r, y-(0+sq)*fy/r)    // Octant 4
    currpen(x+(0-sq)*fx/r, y-(r-sp)*fy/r)    // Octant 5
    currpen(x+(0+sq)*fx/r, y-(r-sp)*fy/r)    // Octant 6
    currpen(x+(r-sp)*fx/r, y-(0+sq)*fy/r)    // Octant 7

    setcolour(col_black)
  }
  //setcolour(col_green)
  currpen(x+rx, y)
  currpen(x-rx, y)
  currpen(x,    y+ry)
  currpen(x,    y-ry)
  //setcolour(col_black)
}

AND fillellipse(x, y, rx, ry) BE
{ // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  LET r = x >= ry -> rx, ry
  LET f = 1 - r
  LET ddf_x = 1
  LET ddf_y = -2 * r
  LET p = 0
  LET q = r
  LET sp, sq = 0, 0
  LET f = 1 - r
  LET fx, fy  = rx, ry // The x and y factor are mx/r and my/r
  LET mx, my = ry, rx
  IF rx<ry DO mx, my := rx, ry
  
  //currpen(x, y+ry)
  //currpen(x, y-ry)
  //currpen(x+rx, y)
  //currpen(x-rx, y)

  WHILE p<=q DO
  { // ddf_x = 2*p + 1
    // ddf_y = -2 * q
    // f = p*p + q*q - r*r + 2*p - q + 1
    IF f>=0 DO
    { q := q-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    p := p+1
    ddf_x := ddf_x + 2
    f := f + ddf_x

    // (q, p) is the coord of a point in an octant of a circle radius r
    //        starting at (0, r)
    sp := rx - q
    sq := p
    // (sq, sp) is the coord of a point in an octant of a circle radius r
    //          starting at (0, 0)

    { LET py = 0
      py := y+(0+sq)*fy/r
      FOR px = x-(r-sp)*fx/r TO x+(r-sp)*fx/r DO currpen(px, py)
      py := y-(0+sq)*fy/r
      FOR px = x-(r-sp)*fx/r TO x+(r-sp)*fx/r DO currpen(px, py)
      py := y+(r-sp)*fy/r
      FOR px = x+(0-sq)*fx/r TO x+(0+sq)*fx/r DO currpen(px, py)
      py := y-(r-sp)*fy/r
      FOR px = x+(0-sq)*fx/r TO x+(0+sq)*fx/r DO currpen(px, py)
    }
    
    
    setcolour(col_black)
  }
  //setcolour(col_green)
  //currpen(x+rx, y)
  //currpen(x-rx, y)
  //currpen(x,    y+ry)
  //currpen(x,    y-ry)
  //setcolour(col_black)
}

AND fillcircle(x, y, r) BE
{ // This is commonly called Bresenham's circle algorithm since it
  // is derived from Bresenham's line algorithm.
  LET f = 1 - r
  LET ddf_x = 1
  LET ddf_y = -2 * r
  LET p = 0
  LET q = r
  LET lastx, lasty = 0, 0
  penS1(x, y+r)
  penS1(x, y-r)
  FOR p = x-r TO x+r DO penS1(p, y)

  WHILE p<q DO
  { // ddf_x = 2*p + 1
    // ddf_y = -2 * q
    // f = p*p + q*q - r*r + 2*p - q + 1
    IF f>=0 DO
    { q := q-1
      ddf_y := ddf_y + 2
      f := f + ddf_y
    }
    p := p+1
    ddf_x := ddf_x + 2
    f := f + ddf_x
    penS1(x+p, y+q)
    penS1(x-p, y+q)
    penS1(x+p, y-q)
    penS1(x-p, y-q)
    penS1(x+q, y+p)
    penS1(x-q, y+p)
    penS1(x+q, y-p)
    penS1(x-q, y-p)
    UNLESS p=lastx DO
    { FOR fx = x-q+1 TO x+q-1 DO
      { penS1(fx, y+p)
        penS1(fx, y-p)
      }
      lastx := p
    }
    UNLESS q=lasty DO
    { FOR fx = x-p+1 TO x+p-1 DO
      { penS1(fx, y+q)
        penS1(fx, y-q)
      }
      lasty := q
    }
  }
}

AND drawcurve(v) BE
{ // v -> [2n, x1, y1, x2, y2,..., xn, yn]
  // n must be > 0
  moveto(v!1, v!2)
  FOR i = 3 TO v!0 BY 2 DO drawto(v!i, v!(i+1))
}

AND drawsmooth1(v) BE IF v!0>=8 DO // Nothing drawn unless at
                                   // least 4 vertices are given.
{ LET first, last = 1, v!0-7 // Subscripts of p0 and p(n+1)
  // The curve is only drawn from p1 to pn.
//abort(1234)
  FOR i = first TO last BY 2 DO // First and quartet of vertices
  { LET FLT xa,  FLT ya  = FLOAT v!i,     FLOAT v!(i+1)
    LET FLT xb,  FLT yb  = FLOAT v!(i+2), FLOAT v!(i+3)
    LET FLT xc,  FLT yc  = FLOAT v!(i+4), FLOAT v!(i+5)
    LET FLT xd,  FLT yd  = FLOAT v!(i+6), FLOAT v!(i+7)
    
    LET FLT dxb, FLT dyb = xc-xa, yc-ya
    LET FLT dxc, FLT dyc = xd-xb, yd-yb

//writef("i=%n*n", i)
//FOR i = 1 TO v!0 BY 2 DO writef("%i2: %i4 %i4*n", i, v!i, v!(i+1))
//writef("xa=   %6.2f   ya=  %6.2f*n", xa, ya)
//writef("xb=   %6.2f   yb=  %6.2f*n", xb, yb)
//writef("xc=   %6.2f   yc=  %6.2f*n", xc, yc)
//writef("xd=   %6.2f   yd=  %6.2f*n", xd, yd)

//writef("dxb= %6.2f   dyb= %6.2f*n", dxb, dyb)
//writef("dxc= %6.2f   dyc= %6.2f*n", dxc, dyc)

    // dxb and dyb give the tangent slope at b
    // dxc and dyc give the tangent slope at c

    // We need to find the coefficients of the quadratic curve
    // x = A + Bt + Ct^2
    // with the property that at t=0, x=xb and has the right slope at xb
    // and at t=1, x=xc and has the right slope at xc
    // This gives
    // xb = A
    // xc = A + B + C

    // If the quadratic for y is
    // y = P + Qt + Rt^2

    // we have
    // yb = P
    // yc = P + Q + R

    // The gradient is dy/dx = (dy/dt)/(dx/dt)

    // where
    //     dx/dt = B + 2Ct
    //     dy/dt = Q + 2Rt

    // At xb we have
    //     dx/dt = B
    //     dy/dt = Q
    // So 
    //     dyb/dxb = Q/B     ie     dyb * B = dxb * Q
    
    // At xc we have
    //     dx/dt = B + 2C
    //     dy/dt = Q + 2R
    // So
    //     dyc/dxc = (Q + 2R) / (B + 2C )
    // is dyc * (B + 2C) = dxc * (Q + 2R)

    // There are 6 equations we 6 unknowns as follows
    
    // xb = A
    // xc = A + B + C
    // yb = P
    // yc = P + Q + R
    // dyb * B = dxb * Q
    // dyc * (B + 2C ) = dxc * (Q + 2R)

    // So A = xb and P = yb and
    // There are 4 equations we 4 unknowns as follows

    // xb = xb + B + C      ie   C = xc-xb-B
    // yc = yb + Q + R      ie   R = yc-yb-Q
    // dyb * B = dxb * Q
    // dyc * (B + 2C ) = dxc * (Q + 2R)

    // This reduces to

    // C = xc-xb-B
    // R = yc-yb-Q
    // dyb * B = dxb * Q
    // dyc * (B + 2(xc-xb-B) ) = dxc * (Q + 2(yc-yb-Q))

    // The last equation simplifies to the following
    
    // dyc * (B + 2xc-2xb-2B) = dxc * (Q + 2yc-2yb-2Q)
    // dyc * (2xc-2xb-B) = dxc * (2yc-2yb-Q)
    // dyc * (2xc-2xb) - dyc * B = dxc * (2yc-2yb) - dxc * Q

    // Multiplying both sides by dya gives
     
    // dyc * dyb * (2xc-2xb) - dyc * dyb * B =
    //                             dxc * dyb * (2yc-2yb) - dxc * dyb * Q

    // Replacing dyb * B by dxb * Q give

    // dyc * dyb * (2xc-2xb) - dyc * dxb x Q =
    //                             dxc * dyb * (2yc-2yb) - dxc * dyb * Q

    // So
    // (dyc * dxb - dxc * dyb) x Q =
    //                       dyc * dyb * (2xc-2xb) - dxc * dyb * (2yc-2yb)

    // This give us the value of Q (which we do not expect to be infinite)
    // The othe variable can be solved using the following equations
    // in order.

    // dyb * B = dxb * Q           gives B
    // C = xc-xb-B                 gives C
    // R = yc-yb-Q                 gives R
    // A = xb                      gives A
    // P = yb                      gives P

    { LET FLT Qtop = dyc * dyb * 2*(xc-xb) - dxc * dyb * 2*(yc-yb)
      LET FLT Qbot   = dyc * dxb - dxc * dyb
      LET FLT Q = Qbot=0 -> Qtop,    Qtop / Qbot
      LET FLT B = dyb=0  -> dxb*Q,  dxb*Q / dyb
      LET FLT C = xc-xb-B
      LET FLT R = yc-yb-Q
      LET FLT A = xb
      LET FLT P = yb

      //writef("xb=%6.2f yb=%6.2f Qbot=%10.4f dyb=%6.2f*n", xb, yb, Qbot, dyb)
      //writef("Segment %i2->%i2 (%6.2f %6.2f) to (%6.2f %6.2f)*n",
      //       i, i+2, xb, yb, xc, yc)
      //writef("A=%10.4f B=%10.4f C=%10.4f  dxb=%10.4f dyb=%10.4f*n",
      //        A, B, C, dxb, dyb) 
      //writef("P=%10.4f Q=%10.4f R=%10.4f  dxc=%10.4f dyc=%10.4f*n",
      //        P, Q, R, dxc, dyc)
      
      currpen := penS3
      moveto(poly(0, 10, A, B, C), poly(0, 10, P, Q, R))
      FOR i = 0 TO 10 DO // For debugging
      //FOR i = 1 TO 10 DO
      { LET x = poly(i, 10, A, B, C)
        LET y = poly(i, 10, P, Q, R)
        //writef("%i2: %i6  %i6*n", i, x, y)
        drawto(x, y)
      }
    }
//    abort(1278)
  }
}

AND poly(i, upb, FLT A, FLT B, FLT C) = VALOF
{ LET FLT t = FLOAT i / FLOAT upb
  RESULTIS FIX((C*t + B)*t + A)
}

AND drawsmooth(v) BE IF v & v!0>=8 DO // At least 4 vertices are required.
{ // Draw a smooth curve through the verices in v.
  // v!0 is the upb and the x and y coords of the vertices
  // are pairs of elements of v.
  // The first and last edges edges are drawn as straight lines and
  // the others are drawn using drawsoothseg whose arguments are the
  // coordinates of two consecutive points with their gradients
  // given as direction cosines. The direction of the second
  // vertex is that of the first edge and the direction of the
  // penultimate vertex is that of the final edge.
  // If P, Q and R are three consective vertices of the curve
  // with P not being the first and R not being the last, the
  // gradient at Q is that of the line PR.
  // The position of the vertices on the curve should be chosen
  // with this in mind.
  
  LET upb = v!0  // Index of the y coordinate of the last vertex.
  LET i = 1      // Index of vertex P
  LET FLT xp,  FLT yp  = FLOAT v!1, FLOAT v!2   // Coords of P
  LET FLT xq,  FLT yq  = FLOAT v!3, FLOAT v!4   // Coords of Q
  LET FLT xr,  FLT yr  = FLOAT v!5, FLOAT v!6   // Coords of R
  LET FLT dxp, FLT dyp = 0, 0                   // For the gradient at P
  LET FLT dxq, FLT dyq = 0, 0                   // For the gradient at Q

  // Calculate the gradient at Q which is the gradient of the first edge.
  LET FLT len = radiusf(xq-xp, yq-yp) // The distance between P and Q
  IF len<1.0 RETURN // Error: the first edge is too short so draw nothing
  dxq, dyq := (xq-xp)/len, (yq-yp)/len
  
  //FOR i = 1 TO v!0 BY 2 DO writef("%i2: %i8 %i8*n", i, v!i, v!(i+1))

  moveto(FIX xq, FIX yq)                // Move to second point.

  // i is the index of P

  { // Move on to the next triplet PQR
    i := i+2
    xp, yp, dxp, dyp := xq, yq, dxq, dyq
    xq, yq := xr, yr
    xr, yr := FLOAT v!(i+4), FLOAT v!(i+5)
    IF i+5>=upb BREAK // R is the final vertex
    // PQR is not the final triplet
    // Calculate the gradient of Q based on P and R.
    len := radiusf(xr-xp, yr-yp) 
    dxq, dyq := (xr-xp)/len, (yr-yp)/len
    // Draw a smooth curve from P to Q
    drawsmoothseg(xp, yp, dxp, dyp,
                  xq, yq, dxq, dyq)
  } REPEAT

  // PQR is the final triplet, so the gradient at Q is the
  // gradient of QR.
  len := radiusf(xr-xq, yr-yq)
  dxq, dyq := (xr-xq)/len, (yr-yq)/len
  drawsmoothseg(xp, yp, dxp, dyp,
                xq, yq, dxq, dyq)
}

AND drawsmoothseg(FLT xa, FLT ya, FLT dxa, FLT dya,
                  FLT xb, FLT yb, FLT dxb, FLT dyb) BE
{ // Draw a smooth curve from (xa,ya) to (xb,ya) with
  // the gradient changing from direction cosines
  // (dxa,dya) to (dxb,dyb).
  // A is not the first vertex of the curve and B is not
  // the last since the first and last edges are always
  // drawn as straight lines.
  // If A and B are close enough a straight line is drawn,
  // otherwise a point M with a the gradient of AB is
  // chosen and the function called recursively to draw smooth
  // curves from A to M and M to B.

  //            P
  //          /    -
  //         /         -
  //        /              M
  //       A                   -
  //           -                  -
  //               -                  Q
  //                    -              \
  //                          -         \
  //                                -    \
  //                                      B
  // A->P is in direction (dxa,dya) and |AP| = |AB| * k
  // B->Q is in direction (dxb,dyb) and |BQ| = |AB| * k
  // where k is a carefully chosen constant typically between
  // greater then 0.25 but less than 0.5
  // M is the mid point of PQ
  
  // If PQR are the first three points of the curve
  // the gradient at Q is the gradient of PQ, and
  // the first edge is drawn as a straight line.
  // If PQR are the last three points of the curve
  // the gradient at Q is the gradient of QR, and
  // the last edge is drawn as a straight line.
  // otherwise the gradient at Q is the gradient of AB.
  
  // If |AB| < 8 a straight line is drawn from A to B,
  // otherwise recursive calls of drawsmoothseg are used
  // to draw the curve from A to M, and M to B.

  LET FLT len = radiusf(xb-xa, yb-ya) // |AB| in pixels
  //writef("drawsmoothseg arguments*n")
  //writef("xa=%8.2f ya=%8.2f dxa=%8.2f dya=%8.2f*n*
  //       *xb=%8.2f yb=%8.2f dxb=%8.2f dyb=%8.2f*n",
  //	 xa, ya, dxa, dya, xb, yb, dxb, dyb)
  //writef("length of AB is %8.2f*n*n", len)
  //abort(8888)
  IF len >= 8.0 DO
  { LET FLT lenk = len * 0.2
    LET FLT xp, FLT yp = xa+dxa*lenk, ya+dya*lenk // Coords of P
    LET FLT xq, FLT yq = xb-dxb*lenk, yb-dyb*lenk // Coords of Q
    LET FLT xm, FLT ym =   (xp+xq)/2,   (yp+yq)/2 // Coords of M
    //                         Q
    //                       * |       |AP| = |BQ| = |AB|/5
    //                     *   |       |PM| = |MQ| = |PQ|/2
    //                   *     B
    //                 M               Slope at A is AP
    //      A        *                 Slope at B is BQ
    //       \     *                   Slope at M is PQ
    //        \  *      
    //         P                       Draw curves AM and MB
    //
    LET FLT lenpq = radiusf(xq-xp, yq-yp) // Cannot be zero
    LET FLT dxm, FLT dym = (xq-xp)/lenpq, (yq-yp)/lenpq // Gradient at M
    //newline()
    //writef("xa=%8.2f ya-%8.2f, dxa=%8.2f, dya=%8.2f*n", xa, ya, dxa, dya)
    //writef("xp=%8.2f yp-%8.2f*n", xp, yp)
    //writef("xm=%8.2f ym-%8.2f, dxm=%8.2f, dym=%8.2f*n", xm, ym, dxm, dym)
    //writef("xq=%8.2f yq-%8.2f*n", xq, yq)
    //writef("xb=%8.2f yb-%8.2f, dxb=%8.2f, dyb=%8.2f*n", xb, yb, dxb, dyb)
//abort(3333)
    drawsmoothseg(xa, ya, dxa, dya,  xm, ym, dxm, dym)
    drawsmoothseg(xm, ym, dxm, dym,  xb, yb, dxb, dyb)
    RETURN
  }
  // Draw a straight line because vertices a and b are close enough.
  //writef("Draw a straight line A(%n %n) -> B(%n %n)*n",
  //       FIX xa, FIX ya, FIX xb, FIX yb)
  moveto(FIX xa, FIX ya)
  drawto(FIX xb, FIX yb)
  //abort(2222)
}

AND radiusf(FLT x, FLT y) = sys(Sys_flt, fl_sqrt, x*x+y*y)

AND drawcurlyH(x, y, xs, ys) BE
{ // Draw a horizontal curly bracket starting between
  // (x,y) and (x+xs, y) of height ys.
  MANIFEST { len=12 }
  LET prevpen = currpen
  LET shape = TABLE 12,
                0, -50,    //  1
                0,   0,    //  3  Start with gradiant zero
               20,  55,    //  5
               80,  45,    //  7
              100, 100,    //  9
              100, 200     // 11 Final gradiant

  LET v = VEC len
  v!0 := len
  currpen := penR24

  FOR i = 1 TO len BY 2 DO
    v!i, v!(i+1) := x+shape!i*xs/200, y+shape!(i+1)*ys/100
  drawsmooth(v)

  FOR i = 1 TO len BY 2 DO
    v!i, v!(i+1) := x+(2*100-shape!i)*xs/200, y+shape!(i+1)*ys/100
  drawsmooth(v)
  currpen := prevpen

}

AND drawcurlyV(x, y, xs, ys) BE
{ // Draw a vertical curly bracket starting between
  // (x,y) and (x+xs, ys) of width xs.
  MANIFEST { len=12 }
  LET prevpen = currpen 
  LET shape = TABLE 12,
                0, -50,    //  1
                0,   0,    //  3  Start with gradiant zero
               20,  55,    //  5
               80,  45,    //  7
              100, 100,    //  9
              100, 200     // 11 Final gradiant

  LET v = VEC len
  v!0 := len
  currpen := penR42

  FOR i = 1 TO len BY 2 DO
    v!i, v!(i+1) := x+shape!(i+1)*xs/100, y+shape!i*ys/200
  drawsmooth(v)

  FOR i = 1 TO len BY 2 DO
    v!i, v!(i+1) := x+shape!(i+1)*xs/100, y+(2*100-shape!i)*ys/200
  drawsmooth(v)

//  currcol := col_red
//  FOR i = 1 TO len BY 2 DO
//    penS5(x+shape!(i+1)*xs/100, y+shape!i*ys/100)

//  FOR i = 1 TO len BY 2 DO
//    penS5(x+shape!(i+1)*xs/100, y+(2*100-shape!i)*ys/100)
  currpen := prevpen
}

AND rndcorner(dir, x, y, r) BE SWITCHON dir INTO
{ DEFAULT: writef("Bad direction for rndcorner %n*n", dir)
           abort(999)
	   RETURN
	   
  CASE 0: drawarc90(0, x-r, y-r, r);  RETURN
  CASE 1: drawarc90(1, x+r, y-r, r);  RETURN
  CASE 2: drawarc90(2, x+r, y+r, r);  RETURN
  CASE 3: drawarc90(3, x-r, y+r, r);  RETURN
}

AND sqtextbox(x, y, text) = VALOF
{ LET pxlen = strpixels(text)
  LET ew = fontW*12/10
  LET w = pxlen+2*ew
  LET boxHby2 = fontH*8/10
  LET x1, x2 = x, x+w
  LET y1, y2 = y-boxHby2, y+boxHby2
  LET prevpen = currpen
  currpen := penV3
  moveto( x1,  y1);  drawto(x2, y1)    // Lower edge
  moveto( x1,  y2);  drawto(x2, y2)    // Upper edge
  currpen := penH3
  moveto( x1,  y1);  drawto(x1, y2)    // Left edge
  moveto( x2,  y1);  drawto(x2, y2)    // Right edge

  drawstr(x+ew, y-fontH*14/24, text)
  currpen := prevpen
  RESULTIS x2
}

AND rndtextbox(x, y, text) = VALOF
{ LET pxlen = strpixels(text)
  LET w = pxlen+3*fontW
  LET r = fontH * 8/10
  LET x1, x2 = x, x+w
  LET y1, y2 = y-fontH*12/10, y+fontH*12/10
  LET prevpen = currpen
  
  currpen := penS3
  drawarc90(0, x2-r, y2-r, r)
  drawarc90(1, x1+r, y2-r, r)
  drawarc90(2, x1+r, y1+r, r)
  drawarc90(3, x2-r, y1+r, r)

  currpen := penV3
  moveto( x1+r,  y1);  drawto(x2-r, y1)    // Lower edge
  moveto( x1+r,  y2);  drawto(x2-r, y2)    // Upper edge
  currpen := penH3
  moveto( x1,  y1+r);  drawto(x1, y2-r)    // Left edge
  moveto( x2,  y1+r);  drawto(x2, y2-r)    // Right edge

  drawstr(x+fontW*3/2, y-fontH*14/24, text)
  currpen := prevpen
  RESULTIS x2
}

LET strut(x, y) BE
{ LET prevcurrpen = currpen
  currpen := penS3
  moveto(x, y)
  drawby(0, cellH)
  currpen := prevcurrpen
}

AND zigzag(x, y) BE
{ LET prevcurrpen = currpen
  currpen := penS3
  moveto(x, y)
  drawby( cellHby4, cellHby4)
  drawby(-cellHby2, cellHby2)
  drawby( cellHby4, cellHby4)
  currpen := prevcurrpen
}

LET drawtestboxL(y, x1, x2, x3, str) BE
{ // Draw a test box with initial and final lines at level y
  // left justified at x2.
  LET boxwidth = (str%0+2) * (fontW+charHsep) - charHsep
  LET boxHby2 = fontH*8/10
  LET bx1, by1 = x2,          y-boxHby2
  LET bx2, by2 = x2+boxwidth, y+boxHby2
  // Draw the initial line
  moveto(x1, y); drawto(x2, y)
  // Draw the box
  moveto(bx1, by1)
  drawto(bx1, by2)
  drawto(bx2, by2)
  drawto(bx2, by1)
  drawto(bx1, by1)
  // Draw the string
  drawstr(x2+fontW+charHsep, y, str)
  // draw the final line
  IF x3 DO { moveto(x2+boxwidth, y); drawto(x3, y) }
}

AND drawtestboxC(y, x1, x2, x3, str) BE
{ // Draw a test box with initial and final lines at level y
  // centred at x2. The final line is onlt frawn if x~=0.
  LET boxwidth = (str%0+2) * (fontW+charHsep) - charHsep
  LET boxwidthby2 = boxwidth/2
  LET boxHby2 = fontH*8/10
  LET bx1, by1 = x2-boxwidthby2, y-boxHby2
  LET bx2, by2 = x2+boxwidthby2, y+boxHby2
  // Draw the initial line
  moveto(x1, y); drawto(bx1, y)
  // Draw the box
  moveto(bx1, by1)
  drawto(bx1, by2)
  drawto(bx2, by2)
  drawto(bx2, by1)
  drawto(bx1, by1)
  // Draw the string
  drawstrcentred(x2, y, str)
  // draw the final line
  IF x3 DO { moveto(bx2, y); drawto(x3, y) }
  
}

AND drawcatboxL(y, x1, x2, x3, str) BE
{ // Draw a category box with initial and final lines at level y
  // centres at x2. The final line is onlt frawn if x~=0.
  LET catwidth = (str%0+2) * (fontW+charHsep) - charHsep
  LET catwidthby2 = catwidth/2
  LET catHby2 = fontH*11/10
  LET bx1, by1 = x2, y-catHby2
  LET bx2, by2 = x2+catwidth, y+catHby2
  LET r = fontH * 8/10
  
  // Draw the initial line
  moveto(x1, y); drawto(bx1, y)
  
  // Draw the rounded box
  moveto(bx1, by1+r); drawto(bx1, by2-r); drawarc90(1, bx1+r, by2-r, r)
  moveto(bx1+r, by2); drawto(bx2-r, by2); drawarc90(0, bx2-r, by2-r, r)
  moveto(bx2, by2-r); drawto(bx2, by1+r); drawarc90(3, bx2-r, by1+r, r)
  moveto(bx2-r, by1); drawto(bx1+r, by1); drawarc90(2, bx1+r, by1+r, r)

  // Draw the string
  drawstrcentred(x2+catwidthby2, y, str)
  // draw the final line
  IF x3 DO { moveto(bx2, y); drawto(x3, y) }
}

AND drawcatboxC(y, x1, x2, x3, str) BE
{ // Draw a category box with initial and final lines at level y
  // centres at x2. The final line is onlt frawn if x~=0.
  LET catwidth = (str%0+2) * (fontW+charHsep) - charHsep
  LET catwidthby2 = catwidth/2
  LET catHby2 = fontH*11/10
  LET bx1, by1 = x2-catwidthby2, y-catHby2
  LET bx2, by2 = x2+catwidthby2, y+catHby2
  LET r = fontH * 8/10
  
  // Draw the initial line
  moveto(x1, y); drawto(bx1, y)
  
  // Draw the rounded box
  moveto(bx1, by1+r); drawto(bx1, by2-r); drawarc90(1, bx1+r, by2-r, r)
  moveto(bx1+r, by2); drawto(bx2-r, by2); drawarc90(0, bx2-r, by2-r, r)
  moveto(bx2, by2-r); drawto(bx2, by1+r); drawarc90(3, bx2-r, by1+r, r)
  moveto(bx2-r, by1); drawto(bx1+r, by1); drawarc90(2, bx1+r, by1+r, r)

  // Draw the string
  drawstrcentred(x2, y, str)
  // draw the final line
  IF x3 DO { moveto(x2+catwidthby2, y); drawto(x3, y) }
}

