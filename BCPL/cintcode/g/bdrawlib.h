/*
This is the header file for the BCPL drawing library b/bdrawlib.b
It is based in the old library g/graphics.b

Implemented by Martin Richards (c) 11 Sep 2021

The default setting of the manifest g_bdrawbase=400 is defined in
libhdr.h, but can be overridden by a later definition.

Unlike g/graphics.b this library will always generate BMP images using
24-bit pixels. Internally the image is formed in a rectangular array
(canvas) of BCPL words holding pixels of the form #x00rrggbb.  The
canvas has xsize pixels in the x direction and ysize pixels in the y
direction. The variables xupb and yupb are set to xsize-1 and ysize-1.
Pixels are addressed by integer coordinates (x,y) corresponding to the
bottom left corner of the pixel.

The pixel with coordinates (0,0) is a unit square with opposite
veritices (0,0) and (1,1) and is the bottom leftmost pixel in the
image. The pixels on the bottom line runs from (0,0) to (xupb,0), and
the vertical line on the left of the image runs from (0,0) to
(0,yupb).

The variable currcol holds the current colour as #x00rrggbb.

Drawing in done using a pen function held in currpen. Some of the
possible pen functions are:

drawpixel(x,y) Draw the pixel of colour currcol at position (x,y).
penS1(x,y)     Draw pixels a 1x1 square of at (x,  y).
penS2(x,y)     Draw pixels a 2x2 square of from (x-1,y-1) to (x,  y).
penS3(x,y)     Draw pixels a 3x3 square of from (x-1,y-1) to (x+1,y+1).
penS4(x,y)     Draw pixels a 4x4 square of from (x-2,y-2) to (x+1,y+1).
penS5(x,y)     Draw pixels a 5x5 square of from (x-2,y-2) to (x+2,y+2).

penH2(x,y)     Draw pixels from (x-1,y) to (x,  y)
penH3(x,y)     Draw pixels from (x-1,y) to (x+1,y)
penH4(x,y)     Draw pixels from (x-2,y) to (x+1,y)
penH5(x,y)     Draw pixels from (x-2,y) to (x+2,y)

penV2(x,y)     Draw pixels from (x,y-1) to (x,y)
penV3(x,y)     Draw pixels from (x,y-1) to (x,y+1)
penV4(x,y)     Draw pixels from (x,y-2) to (x,y+1)
penV5(x,y)     Draw pixels from (x,y-2) to (x,y+2)

penC5(x,y)     Draw a filled circle of diameter 5 centred at (x,y)

arrow(dir, x, y, len) Draw an arrow head of length len with the point
                      at (x,y). The direction is specified by dir.
                      It points to the right if dir=0, up if zero,
                      left if 2 and down if 3. It uses currpen which
                      shold be set appropriately.



History
25/11/2024
Added the functions:
drawtestboxL
drawtestboxC
drawcatboxL
drawcatboxC


History

02/12/2024
Added pixelspermetre
The default setting is for 12 16c24 chars per inch.
The possible fonts are 8x12, 12x18 and 16x24
These are selected by
selectfont(8)
selectfont(12) or
selectfont(16)
Previously the argument was a height not a width.

*/

// This is declarations for ploting flow graphs

MANIFEST {
  testH    = 24*3/2 // Assuming 24x16 font
  testHby2 = testH/2

  catH    = 24*2    // Assuming 24x16 font
  catHby2 = catH/2

  bendradius = 15      // Radius of bends
  catradius  = 15      // Radius of cat box round corners

  // Assume 16x24 font
  //     width   spaces
  wl1  =  3*16 +  2*3  // Length of a box containing 1 character 
  wl2  =  4*16 +  3*3  // Length of a box containing 2 characters 
  wl3  =  5*16 +  4*3  // Length of a box containing 3 characters
  wl4  =  6*16 +  5*3  // Length of a box containing 4 characters
  wl5  =  7*16 +  6*3  // Length of a box containing 5 characters
  wl6  =  8*16 +  7*3  // Length of a box containing 6 characters
  wl7  =  9*16 +  8*3  // Length of a box containing 7 characters
  wl8  = 10*16 +  9*3  // Length of a box containing 8 characters
  wl9  = 11*16 + 10*3  // Length of a box containing 9 characters
  wl10 = 12*16 + 11*3  // Length of a box containing 10 characters
  wl11 = 13*16 + 12*3  // Length of a box containing 11 characters
  wl12 = 14*16 + 13*3  // Length of a box containing 12 characters
  wl13 = 15*16 + 14*3  // Length of a box containing 13 characters
  wl14 = 16*16 + 15*3  // Length of a box containing 14 characters
  wl15 = 17*16 + 16*3  // Length of a box containing 15 characters
  wl16 = 18*16 + 17*3  // Length of a box containing 16 characters
  wl17 = 19*16 + 18*3  // Length of a box containing 17 characters
  wl18 = 20*16 + 19*3  // Length of a box containing 18 characters
  wl19 = 21*16 + 20*3  // Length of a box containing 19 characters
  wl20 = 22*16 + 21*3  // Length of a box containing 20 characters
  wl21 = 23*16 + 22*3  // Length of a box containing 21 characters
  wl22 = 24*16 + 23*3  // Length of a box containing 22 characters
  wl23 = 25*16 + 24*3  // Length of a box containing 23 characters
  wl24 = 26*16 + 25*3  // Length of a box containing 24 characters
  wl25 = 27*16 + 26*3  // Length of a box containing 25 characters
  wl26 = 28*16 + 27*3  // Length of a box containing 26 characters
  wl27 = 29*16 + 28*3  // Length of a box containing 27 characters
  wl28 = 30*16 + 29*3  // Length of a box containing 27 characters
  wl29 = 31*16 + 30*3  // Length of a box containing 27 characters
  wl30 = 32*16 + 31*3  // Length of a box containing 27 characters

  fac = 90

  hll  = testH*fac/130                   // H sep:  line  line
  hlt  = (hll+testHby2)*fac/100          // H sep:  line  test
  htl  = hlt
  hlc  = (hll+catHby2)*fac/100           // H sep:  line  cat
  hcl  = hlc
  
  htt  = (testHby2+hll+testHby2)*fac/100 // H sep:  test  test
  htc  = (testHby2 +hll+catHby2)*fac/100 // H sep:  test  cat
  hct  = htc
  
  hcc  = (catHby2 +hll+catHby2) *fac/100 // H sep:  cat   cat
  
  wll   = 30
  
  wtl   = 35
  wlt   = wtl
  
  wcl   = 40
  wlc   = wcl
  
  wtt   = 40
  
  wtc   = 45
  wct   = wtc
  
  wcc   = 50

  cellH   = 48
  cellW   = 48
  cellHby2   = cellH/2
  cellWby2   = cellW/2
  cellHby4   = cellH/4
  cellWby4   = cellW/4
  zigzagW    = cellWby2
}

GLOBAL {
openbdraw:   g_bdrawbase  // (xmax, ymax)
closebdraw                // ()

xsize       // Number of x pixel positions in the canvas
ysize       // Number of y pixel positions in the canvas

xupb        // =xsize-1
yupb        // =ysize-1
	    
canvas      // Rectangular array of 32-bit pixels 
            // Each element holds a 32-bit coulour #x00rrggbb

canvasupb   // UPB of canvas in words = xsize * ysize -1


col_white
col_majenta
col_blue
col_cyan
col_green
col_yellow
col_red
col_black

currx          // Current x position, 0<=currx<xsize
curry          // Cyrrent y position, 0<=currx<xsize
currcol        // Current colour of the form #X00rrggbb.

setcolour      // (col)             Set the current colour in currcol

currpen        // (x,y)             The current pen function
               //                   (0,0) position of the bottom left pixel

penS1          // (x,y)             Draw a point using currcol
penS2          // (x,y)             Draw a dot of size 2 using currcol
penS3          // (x,y)             Draw a dot of size 3 using currcol
penS4          // (x,y)             Draw a dot of size 4 using currcol
penS5          // (x,y)             Draw a dot of size 5 using currcol

penH2         // (x,y)             Draw a horizontal line length 2 of currcol
penH3         // (x,y)             Draw a horizontal line length 3 of currcol
penH4         // (x,y)             Draw a horizontal line length 4 of currcol
penH5         // (x,y)             Draw a horizontal line length 5 of currcol

penV2         // (x,y)             Draw a vertical line length 2 of currcol
penV3         // (x,y)             Draw a vertical line length 3 of currcol
penV4         // (x,y)             Draw a vertical line length 4 of currcol
penV5         // (x,y)             Draw a vertical line length 5 of currcol

penC5         // (x,y)             Draw a circle of radius 5 of currcol

drawarc45      // (n, x, y, r)     draw a 45 deg arg centre (x,y) radius r
               //                  n is the octant number from 0 to 7
drawarc90      // (n, x, y, r)     draw a 90 deg arg centre (x,y) radius r
               //                  n is the quadrant number from 0 to 3

drawarrow      // (dir, x, y, len)

rndcorner      // (dir, x, y, r)
drawgrid       // (sep, pen, colour)

fontW              // Typically  8 or 12
fontH              // Typically 12 or 18
charHsep           // Char horizontal separation in pixels, typically 2 or 3 
charVsep           // Char verticalal separation in pixels, typically 3 or 4
pixelspermetre     // The default value for 12  16x24 chars per inch

charLmargin        // Typically 10 pixels
charleveloffset    // The level offset used by drawch
charmidleveloffset // fontH/2 - baselevel, set by selectfont

write_ch_slice     // Used by drawch
fonttab            // The font table used by write_ch_slice

selectfont         // Sets fontW, fontH, drawch, write_ch_slice and fonttab

drawch         // (ch)              Draw a character
drawstr        // (x,y,s)           Draw a string
drawstrcentred // (x,y,s)           Draw a string centred
strpixels      // (s)               Length of a string in pixels
drawf          // (x,y,format,<args>)
drawwrch       // (ch)              The version of wrch used by drawf
drawfstr       //                   Used by drawwch
moveto         // (x,y)             Set the current position  
moveby         // (dx,dy)           Increment the current position
drawto         // (x,y)             Draw a line from the current position
drawby         // (dx,dy)           Draw a line given its position increment
drawrect       // (x,y,w,h)         Draw a rectangle of width w and height h
               //                   bottom left corner at (x,y) using currcol
drawfillrect   // (x,y,w,h)         Draw a filled rectangle, width w height h
               //                   bottom left corner at (x,y) using currcol
drawrndrect    // ((x,y,w,h,radius) Draw a rectangle with rounded corners
               //                   and height h with bottom left corner at (x,y)
	       //                   using currcol
fillrndrect    // ((x,y,radius)     Draw a filled rectangle with rounded corners
               //                   of width w and height h, bottom left
	       //                   at (x,y) using currcol
drawcircle     // (x,y,radius)      Draw a circle with centre (x,y) given radius
fillcircle     // (x,y,radius)      Draw a filled circle with centre (x,y) and
               //                   given radius
drawellipse    // (x,y,rx,ry)       Draw an ellipse with radii rx and ry
fillellipse    // (x,y,rx,ry)       Draw a filled ellipse with radii rx and ry

drawarc45      // (n, x, y, r)      Draw a 45 degree arc centred at (x,y)
               //                   with radius r in octant n

drawarc90      // (n, x, y, r)      Draw a 90 degree arc centred at (x,y)
               //                   with radius r in quadrant n

drawcurve      // (v)               Draw a piecewise linear curve. V!0 is
               //                   the upb of v, The point coordinates are
	       //                   pairs of words starting a v!1
drawsmooth     // (v)               Draw a smooth curve. V!0 is the upb of v.
               //                   The point coordinates are pairs of words
	       //                   starting a v!1. The first and last segments
	       //                   are linear, the others are smoothed. The
	       //                   first two and and last two point are
	       //                   normally close together.
drawcurlyH     // (x, y, xs, ys)    Draw a horizontal curly bracket, within
               //                   a rectangle of width xs and height ys
	       //                   whose bottom left corner is at (x,y)
drawcurlyV     // (x, y, xs, ys)    Draw a vertical curly bracket, within
               //                   a rectangle of width xs and height ys
	       //                   whose bottom left corner is at (x,y)

sqtextbox      // (x, y, text)      Draw text centred in a square box with
               //                   centre of the left edge at (x,y)
	       //                   returning the x coord of the right edge
rndtextbox     // (x, y, text)      Draw text centred in a rounded box with
               //                   centre of the left edge at (x,y)
	       //                   returning the x coord of the right edge

wrimage        // (filename)        Output the image as a .bpm file.

wrbmp

strut
zigzag

drawtestboxL
drawtestboxC
drawcatbocL
drawcatbocC
}

