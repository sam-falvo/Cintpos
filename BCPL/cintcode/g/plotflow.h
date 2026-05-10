// This is the header file for ploting flow graphs

// Insert the graphics library
//MANIFEST { g_bdrawbase=450 }

GET "bdrawlib.h"
GET "bdrawlib.b"

MANIFEST {
  testH    = 24*3/2 // Assuming 24x16 font
  testHby2 = testH/2

  catH    = 24*2    // Assuming 24x16 font
  catHby2 = catH/2

  bendradius = 15      // Radius of bends
  catradius  = 15      // Radius of cat box round corners

  // Assume 16x24 font
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

/*
LET drawtestboxL(y, x1, x2, x3, str) BE
{ // Draw a test box with initial and final lines at level y
  // left justified at x2.
  LET boxwidth = (str%0+2) * (fontW+charHsep) - charHsep
  LET bx1, by1 = x2,          y-testHby2
  LET bx2, by2 = x2+boxwidth, y+testHby2
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
  moveto(x2+boxwidth, y); drawto(x3, y)  
}

AND drawtestboxC(y, x1, x2, x3, str) BE
{ // Draw a test box with initial and final lines at level y
  // centred at x2.
  LET boxwidth = (str%0+2) * (fontW+charHsep) - charHsep
  LET boxwidthby2 = boxwidth/2
  LET bx1, by1 = x2-boxwidthby2, y-testHby2
  LET bx2, by2 = x2+boxwidthby2, y+testHby2
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
  moveto(bx2, y); drawto(x3, y)
  
}

AND drawcatboxL(y, x1, x2, x3, str) BE
{ // Draw a category box with initial and final lines at level y
  // The left edge of the cat box is at x position x2.
  LET catW = (str%0+2) * (fontW+charHsep) - charHsep
  LET catWby2 = catW/2
  LET bx1, by1 = x2, y-catHby2
  LET bx2, by2 = x2+catW, y+catHby2
  LET r = catradius
  
  // Draw the initial line
  moveto(x1, y); drawto(bx1, y)
  
  // Draw the rounded box
  moveto(bx1, by1+r); drawto(bx1, by2-r); drawarc90(1, bx1+r, by2-r, r)
  moveto(bx1+r, by2); drawto(bx2-r, by2); drawarc90(0, bx2-r, by2-r, r)
  moveto(bx2, by2-r); drawto(bx2, by1+r); drawarc90(3, bx2-r, by1+r, r)
  moveto(bx2-r, by1); drawto(bx1+r, by1); drawarc90(2, bx1+r, by1+r, r)

  // Draw the string
  drawstrcentred(x2+catWby2, y, str)
  // draw the final line
  moveto(bx2, y); drawto(x3, y)
  
}

AND drawcatboxC(y, x1, x2, x3, str) BE
{ // Draw a category box with initial and final lines at level y
  // centres at x2.
  LET catW = (str%0+2) * (fontW+charHsep) - charHsep
  LET catWby2 = catW/2
  LET bx1, by1 = x2-catWby2, y-catHby2
  LET bx2, by2 = x2+catWby2, y+catHby2
  LET r = catradius
  
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
  moveto(x2+catWby2, y); drawto(x3, y)
}

*/
