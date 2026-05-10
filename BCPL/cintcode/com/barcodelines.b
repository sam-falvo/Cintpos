/*
This is a program to generate a file containing some scan lines
taken from a image of a barcode. Each scan line consists of
a sequence of integers terminated by a dot. They represent
the gray scale colour of every point on the scan line.

Implemented by Marin Richards (c) January 2016
*/

GET "libhdr"

// Insert the graphics library
//MANIFEST { grgbase=450 }  // Defined in libhdr.h

GET "graphics.h"
GET "graphics.b"

MANIFEST {
  scanvupb=3000
  scanvmid = scanvupb/2
}

GLOBAL {
  x0:ug; y0
  x1;    y1
  xsize; ysize
  maxpixel // Max gray scale value, typically 255

  rowlen   // Length in bytes of a row including padding.
  picv     // Vector of pixel bytes
  scanv    // Temp vector of scan line pixels

  dira     // First scan line direction
  dirb     // Last scan line direction

  byteupb
  wordupb

  infilename
  instream
  outfilename
  outstream

  stdin
  stdout
  ch
  rdn // Read a number from the .pgm file.
  pos

  vin
  code
  digv
  error
  tracing

  plotbarch
}

LET start() = VALOF
{ LET argv = VEC 50
  LET dv = VEC 12
  digv := dv
  FOR i = 1 TO 12 DO digv!i := 0
  stdin  := input()
  stdout := output()

  UNLESS rdargs("from,to/k,-t/s,-a/n,-b/n", argv, 50) DO
  { writef("Bad arguments for barcodelines*n")
    RESULTIS 0
  }

  error := FALSE
  vin := 0
  dira, dirb := 0, 7

  infilename  := "barcode.pgm"        // Portable Gray Map file
  outfilename := "barcodelines.txt"

  IF argv!0 DO infilename := argv!0
  IF argv!1 DO outfilename := argv!1
  tracing := argv!2

  IF argv!3 DO dira := !(argv!3)
  IF argv!4 DO dirb := !(argv!4)

  xsize, ysize := 0, 0
  picv := 0

  UNLESS readpgm(infilename) DO
  { writef("Trouble with file %s*n", infilename)
    RESULTIS 0
  }

  writef("xsize=%n ysize=%n maxpixel=%n*n", xsize, ysize, maxpixel)

  IF error DO
  { writef("*nThere were error(s)*n*n")
    RESULTIS 0
  }

//writef("Calling wrcodelines*n")
//abort(1000)
  wrbarcodelines()

  IF picv DO freevec(picv)
  RESULTIS 0
}

AND readpgm(filename) = VALOF
{ xsize, ysize := 0, 0
  picv := 0

  instream := findinput(filename)
  UNLESS instream RESULTIS FALSE
  selectinput(instream)

  ch := rdch()
  UNLESS ch='P' RESULTIS FALSE
  ch := rdch()
  UNLESS ch='2' RESULTIS FALSE // Insist on Plain PGM format.
  ch := rdch()


  xsize := rdn()
  ysize := rdn()
  maxpixel := rdn()   // Maximum gray scale value
                      // 0 is black, maxpixel is white

  // If P5 the pixels are given as follows.
  // If maxpixel<256 what follows ia a sequence of binary bytes giving gray
  // scale values from left to right starting at the top of the image. If
  // maxpixel>=256 two bytes are used for each pixel.

  // If P2 the pixels are given as integers in ASCII.

  // Comments start with '#' and continue to the end of the line.
  
  rowlen := xsize
  UNTIL rowlen MOD 4 = 0 DO rowlen := rowlen+1
  // rowlen is xsize rounded up to a multiple of 4.

  byteupb := rowlen*ysize - 1
  wordupb := byteupb/4

  writef("xsize=%n ysize=%n rowlen=%n*n", xsize, ysize, rowlen)
  writef("byteupb=%n wordupb=%n*n", byteupb, wordupb)

  picv := getvec(wordupb)

  pos := 0
  FOR row = 0 TO ysize-1 DO readrow()
  UNLESS pos=byteupb+1 DO
  { writef("ERROR: pos=%n byteupb=%n*n", pos, byteupb)
    error := TRUE
  }

  IF FALSE DO
  { LET xc, yc = xsize/2, ysize/2
    LET w = TRUE  // =TRUE if in white area
    LET pd = 0
    LET pi = 3
    LET pc = TRUE

    FOR i = 0+3 TO ysize-1-3 DO
    { LET x, y = xc, i
      LET ch = ' '
      LET g = picv%(y*rowlen + x)
      LET d = picv%((y+2)*rowlen + x) - picv%((y-2)*rowlen + x)
      TEST w
      THEN { IF d<-30 TEST d>pd
             THEN ch, w, pd := 'B', FALSE, 0
             ELSE pd := d
           }
      ELSE { IF d>+30 TEST d<pd
             THEN ch, w, pd := 'W', TRUE, 0
             ELSE pd := d
           }
      UNLESS pc=w DO
      { writef("%c %i4*n", (pc->'W','B'), (i-pi+9)*10/18)
        pi, pc := i, w
      }
      //IF (i+200) MOD 20 = 0 DO newline()
      //writef(" %c%i3", ch, d)
    }
    newline()
  } 

  endstream(instream)
  selectinput(stdin)
writef("rdpgm returning TRUE*n")
  RESULTIS TRUE
}

AND readrow() BE
{ FOR i = 1 TO xsize DO
  { picv%pos := rdn()
//writef(" %i3:%i3", pos, picv%pos)
    pos := pos+1
  }
  UNTIL pos MOD 4 = 0 DO
  { picv%pos := 0
    pos := pos+1
  }
  //newline()
}

AND rdn() = VALOF
{ LET res = -1

  IF ch=endstreamch RESULTIS -1

  // Ignore pgm comments
  IF ch='#' DO
  { ch := rdch() REPEATUNTIL ch='*n' | ch=endstreamch
    LOOP
  }

  WHILE '0'<=ch<='9' DO
  { IF res<0 DO res := 0
    res := 10*res + ch  - '0'
    ch := rdch()
  }

  IF res >= 0 RESULTIS res

  ch := rdch()
} REPEAT

AND wrbarcodelines() BE
{ scanv := getvec(scanvupb)

  UNLESS scanv DO
  { writef("More space needed*n")
    error := TRUE
    RETURN
  }

  outstream := findoutput(outfilename)

  UNLESS outstream DO
  { writef("Trouble with file: %s*n", outfilename)
    error := TRUE
    RETURN
  }

  IF tracing DO prpicv()

  selectoutput(outstream)

  FOR dir = dira TO dirb DO outline(dir)

  endstream(outstream)
  selectoutput(stdout)
  writef("*nTransitions written to file %s*n", outfilename)
}

AND prpicv() BE
{ writef("*npicv*n")
  FOR y = ysize-1 TO 0 BY -1 DO
  { FOR x = 0 TO xsize-1 DO writef(" %z2", picv%(y*rowlen + x))
    newline()
  } 

}

AND pixel(x,y) = VALOF
{ LET col = picv%(y*rowlen + x )
  //writef("x=%i3  y=%i3  col=%i3*n", x, y, col)
  RESULTIS col
}

AND outline(dir) BE
{ // Output a line through the centre in direction
  // specified by dir.
  // dir=0     due north
  // dir=1     about 25 degrees east of north
  // dir=2     North-east
  // dir=3     about 25 degrees north of east
  // dir=4     Due east
  // dir=5     About 25 degrees south of east
  // dir=6     South-east
  // dir=7     About 25 degrees east of south

  LET cx, cy = xsize/2, ysize/2 // The centre
  // Each selected scan line will pass through the centre.
  LET x, y = 0, 0
  LET p, q = scanvmid, scanvmid
  LET layout = 0

  scanv!p := picv%(cy*rowlen+cx) // Mid point

  writef("*n# dir=%n*n*n", dir)

  SWITCHON dir INTO
  { DEFAULT:
      selectoutput(stdout)
      writef("Bad direction: %n*n", dir)
      RETURN

    CASE 0: // Direction due north
      p, q := 0, ysize-1
      FOR y = p TO q DO scanv!y := pixel(cx,y)
      ENDCASE

   CASE 1: // Direction about 25 degrees east of north

      // The scan line passes through the centre as follows
      //                   *
      //                   *
      //                 *
      //  cy ----------> *
      //                 *
      //               * ^
      //               * |
      //                cx

      x, y := cx, cy

      { y := y-1; IF y<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
        x := x-1; IF x<0 BREAK
        y := y-1; IF y<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
        y := y-1; IF y<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
      } REPEAT

      x, y := cx, cy

      { y := y+1; IF y>=ysize BREAK
        q := q+1; scanv!q := pixel(x,y)
        x := x+1; IF x>=xsize BREAK
        y := y+1; IF y>=ysize BREAK
        q := q+1; scanv!q := pixel(x,y)
        y := y+1; IF y>=ysize BREAK
        q := q+1; scanv!q := pixel(x,y)
      } REPEAT

      ENDCASE

   CASE 2: // Direction north-east
      x, y := cx, cy

      { y := y-1; IF y<0 BREAK
        x := x-1; IF x<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
      } REPEAT

      x, y := cx, cy

      { y := y+1; IF y>=ysize BREAK
        x := x+1; IF x>=xsize BREAK
        q := q+1; scanv!q := pixel(x,y)
      } REPEAT

      ENDCASE


   CASE 3: // Direction about 25 degrees north of east.

      // The scan line passes through the centre as follows

      //                  * * *
      //  cy -----> * * *
      //      * * *   ^
      //              |
      //             cx

      x, y := cx, cy

      { x := x-1; IF x<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
        y := y-1; IF y<0 BREAK
        x := x-1; IF x<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
        x := x-1; IF x<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
      } REPEAT

      x, y := cx, cy

      { x := x+1; IF x>=xsize BREAK
        q := q+1; scanv!q := pixel(x,y)
        y := y+1; IF y>=ysize BREAK
        x := x+1; IF x>=xsize BREAK
        q := q+1; scanv!q := pixel(x,y)
        x := x+1; IF x>=xsize BREAK
        q := q+1; scanv!q := pixel(x,y)
      } REPEAT

      ENDCASE

    CASE 4: // Direction due east
      p, q := 0, xsize-1
      FOR x = p TO q DO scanv!x := pixel(x,cy)
      ENDCASE

   CASE 5: // Direction about 25 degrees south of east.

      // The scan line passes through the centre as follows

      //      * * *
      //  cy -----> * * *
      //              ^   * * *
      //              |
      //             cx

      x, y := cx, cy

      { x := x-1; IF x<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
        y := y+1; IF y>=ysize BREAK
        x := x-1; IF x<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
        x := x-1; IF x<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
      } REPEAT

      x, y := cx, cy

      { x := x+1; IF x>=xsize BREAK
        q := q+1; scanv!q := pixel(x,y)
        y := y-1; IF y<0 BREAK
        x := x+1; IF x>=xsize BREAK
        q := q+1; scanv!q := pixel(x,y)
        x := x+1; IF x>=xsize BREAK
        q := q+1; scanv!q := pixel(x,y)
      } REPEAT

      ENDCASE

   CASE 6: // Direction south-east
      x, y := cx, cy

      { y := y-1; IF y<0 BREAK
        x := x+1; IF x>=xsize BREAK
        p := p-1; scanv!p := pixel(x,y)
      } REPEAT

      x, y := cx, cy

      { y := y+1; IF y>=ysize BREAK
        x := x-1; IF x<0 BREAK
        q := q+1; scanv!q := pixel(x,y)
      } REPEAT

      ENDCASE

   CASE 7: // Direction about 25 degrees east of south.

      // The scan line passes through the centre as follows
      //               *
      //               *
      //                 *
      //  cy ----------> *
      //                 *
      //                 ^ *
      //                 | *
      //                cx

      x, y := cx, cy

      { y := y-1; IF y<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
        x := x+1; IF x>=xsize BREAK
        y := y-1; IF y<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
        y := y-1; IF y<0 BREAK
        p := p-1; scanv!p := pixel(x,y)
      } REPEAT

      x, y := cx, cy

      { y := y+1; IF y>=ysize BREAK
        q := q+1; scanv!q := pixel(x,y)
        x := x-1; IF x<0 BREAK
        y := y+1; IF y>=ysize BREAK
        q := q+1; scanv!q := pixel(x,y)
        y := y+1; IF y>=ysize BREAK
        q := q+1; scanv!q := pixel(x,y)
      } REPEAT

      ENDCASE
  }

  FOR i = p TO q DO
  { writef(" %i3", scanv!i)
    layout := layout + 1
    IF layout MOD 16 = 0 DO newline()
  }
  writef("*n.*n")
}

AND outline1(dir) BE
{ // Output a line through the centre in direction
  // specified by dir.
  // dir=0     due north
  // dir=1     about 25 degrees east of north
  // dir=2     North-east
  // dir=3     about 25 degrees north of east
  // dir=4     Due east
  // dir=5     About 25 degrees south of east
  // dir=6     South-east
  // dir=7     About 25 degrees east of south

  LET y1 = rowlen  // One step north
  LET y2 = 2*y1    // Two steps north

  LET px, py = xsize/2, ysize/2  // Centre of the image
  LET layout = 0
  LET dx, dy = 0, 0

  SWITCHON dir INTO
  { DEFAULT:
      writef("Bad direction, dir=%n*n", dir)
      error := TRUE
      RETURN

    CASE 0: dx, dy :=  0,  1; ENDCASE
    CASE 1: dx, dy :=  1,  3; ENDCASE
    CASE 2: dx, dy :=  1,  1; ENDCASE
    CASE 3: dx, dy :=  3,  1; ENDCASE
    CASE 4: dx, dy :=  1,  0; ENDCASE
    CASE 5: dx, dy :=  3, -1; ENDCASE
    CASE 6: dx, dy :=  1, -1; ENDCASE
    CASE 7: dx, dy :=  1, -3; ENDCASE
  }

  // Find the end point of this line
  WHILE dx<=px<=xsize-dx & dy<=py<=ysize-dy DO
  { px, py := px-dx, py-dy
    //writef("x=%i4  y=%i4*n", px, py)
  }
  // (px,py) is the starting point of the line in direction (dx,dy}

  writef("*n*n# dir=%n dx=%n dy=%n*n*n", dir, dx, dy)
//abort(1000)

  IF dir=0 DO
  { // Vertical scan line
    scanv!0 := 0
    scanv!1 := 0
    pos := 1
    FOR y = 0 TO ysize-1 DO
    { // Calculate the pixel value at (px, y)
      LET p = y*rowlen + px
      // y -> a b c d e
      //          |
      //         px

      // Return (a +2b + 2c + 2d + e)/8
      LET res = ( picv%(p-2) + 
                  2 * (picv%(p-1) +picv%p+ picv%(p+1)) +
                  picv%(p+2)) / 8

      pos := pos + 1
      scanv!(pos-2) := (scanv!(pos-2) + res) / 3 // Average of three
      scanv!(pos-1) := scanv!(pos-1) + res
      scanv!pos := res
    }

    // The suitably smoothed data is now in scanv!2 to scanv!(pos-2)

/*
    FOR p = 2 TO pos-2 DO
    { IF layout MOD 20 = 0 DO newline()
      layout := layout+1
      //writef(" (%i4,%i4):", px, y)
      writef(" %i3", scanv!p)

    }
    writef("*n.*n")
*/

//abort(1001)

    // High to low transition from p1 to p2 is defined
    // as follows.

    //              * <-------------------- Start of transition
    //              |\  )  >= 4 units
    //              | *
    //              .......
    //              |     *
    //              |      \      )   >= 4 units
    //              |       * <------------ End of transition
    //              |       |\    )   <  4 units
    //              |       | *
    //              |       |  \  )   <  4 units
    //              p1      p2  *

    // Note that p2 could equal p1+1

    // The transition point is the value of p in [p1,p2]
    // that minimised ABS(scanv!p - (scanv!p1+scanv!p2)/2)


    // Loop to find the transition points

    { LET p, k = 2, 0
      LET p1, p2 = 0, 0
      LET a = scanv!2 // Previous pixel
      LET b = 0       // Next pixel
 
//writef("*nFind transitions*n")
//abort(1006)

      WHILE p < pos-2 DO
      { // Find next high to low transition
//writef("*nFind the next pair of transitions*n")
//abort(1007)

retry1:
        WHILE p < pos-2 DO
        { // It must start with a fall of at least 4 units
          p := p+1
          b := scanv!p // Next pixel
//writef("p=%i4  a=%i3  b=%i3*n", p-1, a, b)
          IF a > b+4 DO
          { p1 := p-1 // Possible start of a transition
            BREAK
          }
          // Transition not yet started
          a := b
        }
//writef("Possible start of high to low transition: p1=%i4  val=%i3*n", p1, scanv!p1)
 
        // Find end of transition
        a, k, p2 := scanv!p, 0, 0

        WHILE p < pos-2 DO
        { p := p+1
          b := scanv!p
//writef("p=%i4  a=%i3  b=%i3*n", p-1, a, b)
          IF b > a-4 DO
          { // Possible end of transition
            IF k>=2 DO
            { p2 := p-k
              BREAK // End of transition found
            }
            k, a := k+1, b
            LOOP
          }
          k, a := 0, b
        }

        UNLESS p2 DO
        { // End of scan line reached
          BREAK  // break out of the scan line loop
        }

        IF p2 & scanv!p1 - 40 < scanv!p2 DO
        { // Change in colour not sufficient so start the search again
//writef("Change from %n to %n not sufficient, so try again*n", scanv!p1, scanv!p2)
          p := p2   // Start search for a high to low transition from p2
          GOTO retry1
        }

//writef("High to low transition found: p1=%i4 p2=%i4  val1=%i3  val2=%i3*n",
//        p1, p2, scanv!p1, scanv!p2)

        // Find the transition point
        { LET hi = scanv!p1
          LET lo = scanv!p2
          LET mid = (hi+lo)/2
          LET midp = (p1+p2)/2
          LET midval = scanv!midp

          WHILE p1 < p2 DO
          { p1 := p1+1
            IF ABS(scanv!p1 - mid) < midval DO midp, midval := p1, ABS(scanv!p1 - mid) 
          }

//writef("Transition point: midp=%i4  val=%i3*n", midp, scanv!midp)
//abort(1001)

          IF layout MOD 16 = 0 DO newline()
          layout := layout+1
          writef(" %i5", midp) // High to low transition point
        }

//abort(1002)
        // Now find a low to high transition point
        p := p2

retry2:
        a := scanv!p

        WHILE p < pos-2 DO
        { // It must start with a rise of at least 4 units
          p := p+1
          b := scanv!p // Next pixel
//writef("p=%i4  a=%i3  b=%i3*n", p-1, a, b)
          IF a < b-4 DO
          { p1 := p-1 // Possible start of a transition
            BREAK
          }
          // Transition not yet started
          a := b
        }
//writef("Possible start of low to high transition: p1=%i4  val=%i3*n", p1, scanv!p1)
 
        // Find end of transition
        a, k, p2 := scanv!p, 0, 0

        WHILE p < pos-2 DO
        { p := p+1
          b := scanv!p
//writef("p=%i4  a=%i3  b=%i3*n", p-1, a, b)
          IF a > b-4 DO
          { // Possible end of transition
            IF k>=2 DO
            { p2 := p-k
              BREAK // End of transition found
            }
            k, a := k+1, b
            LOOP
          }
          k, a := 0, b
        }

        UNLESS p2 DO
        { // End of scan line reached
          BREAK  // break out of the scan line loop
        }

        IF scanv!p1 + 40 > scanv!p2 DO
        { // Change in colour not sufficient so start the search again
//writef("Change from %n to %n not sufficient, so try again*n", scanv!p1, scanv!p2)
          p := p2   // Start search for a low to high transition from p2
          GOTO retry2
        }

//writef("Low to high transition found: p1=%i4 p2=%i4  val1=%i3  val2=%i3*n",
//        p1, p2, scanv!p1, scanv!p2)

        // Find the transition point
        { LET lo = scanv!p1
          LET hi = scanv!p2
          LET mid = (hi+lo)/2
          LET midp = (p1+p2)/2
          LET midval = scanv!midp

          WHILE p1 < p2 DO
          { p1 := p1+1
            IF ABS(scanv!p1 - mid) < midval DO midp, midval := p1, ABS(scanv!p1 - mid) 
          }

//writef("Low to high transition point: midp=%i4  val=%i3*n", midp, scanv!midp)
//abort(1001)

          IF layout MOD 16 = 0 DO newline()
          layout := layout+1
          writef(" %i5", -midp) // low to high transition point
        }
//abort(1002)
      }

      writef("*n.*n")
//abort(1004)
      RETURN
    }
//abort(1005)
  }

  IF dy=0 DO
  { // horizontal line
    FOR x = 0 TO xsize-1 DO
    { IF layout MOD 5 = 0 DO newline()
      layout := layout+1
      writef(" (%i4,%i4):", x, py)
      writef(" %i3", picv%(py*rowlen + x))
    }
    writef("*n.*n")
    RETURN
  }

  IF -1 <= dx <= 1 DO
  { WHILE 0<=px<xsize DO
    { // eg dx=1 dy=3    X
      //                 X
      //                 X
      //               X
      //               X
      //    py ------> X
      //               ^
      //               |
      //              px
      LET n = ABS(dy)    // Non zero
      LET yinc = dy / n  // =+1 or =-1
      FOR i = 1 TO n DO
      { IF 0<=py<ysize DO
        { IF layout MOD 5 = 0 DO newline()
          layout := layout+1
          writef(" (%i4,%i4):", px, py)
          writef(" %i3", picv%(py*rowlen + px))
        }
        py := py + yinc
      }
      px := px+dx
    }
    writef("*n.*n")
abort(1003)
    RETURN
  }

  IF -1 <= dy <= 1 DO
  { WHILE 0<=py<ysize DO
    { // eg dx=3 dy=1
      //                     X X X
      //    py ------> X X X
      //               ^
      //               |
      //              px
      LET n = ABS(dx)    // Non zero
      LET xinc = dx / n  // =+1 or =-1
      FOR i = 1 TO n DO
      { IF  0<=px<xsize DO
        { IF layout MOD 5 = 0 DO newline()
          layout := layout+1
          writef(" (%i4,%i4):", px, py)
          writef(" %i3", picv%(py*rowlen + px))
        }
        px := px + xinc
      }
      py := py+dy
    }
    writef("*n.*n")
    RETURN
  }

  writef("ERROR: Bad dx=%n dy=%n*n", dx, dy)
  error := TRUE
}

AND plotbarcode() BE
{ LET ltab = TABLE #b0001101, #b0011001, #b0010011, #b0111101, #b0100011,
                   #b0110001, #b0101111, #b0111011, #b0110111, #b0001011

  LET rtab = TABLE #b1110010, #b1100110, #b1101100, #b1000010, #b1011100,
                   #b1001110, #b1010000, #b1000100, #b1001000, #b1110100


  x0 := xsize/2 - 56*3
  y0 := ysize/2 + 100

/*
  UNLESS opengraphics(xsize, ysize, mode8bit) DO
  { writef("Unable to open the graphics library*n")
    GOTO fin
  }

  // Draw all colours along the bottom of the canvas.
  //FOR x = 1 TO xsize-2 DO
  //{ currcolour := col_red
  //  wrpixel33(x, 1)
  //  wrpixel33(x, ysize-2)
  //  moveto(x, 4)
  //  currcolour := 255*x/xsize
  //  drawby(0, 20)    
  //}

  currcolour := col_black
  plotbarch(  5, digv! 1)
  plotbarch( 23, digv! 2)
  plotbarch( 30, digv! 3)
  plotbarch( 37, digv! 4)
  plotbarch( 44, digv! 5)
  plotbarch( 51, digv! 6)
  plotbarch( 63, digv! 7)
  plotbarch( 70, digv! 8)
  plotbarch( 77, digv! 9)
  plotbarch( 84, digv!10)
  plotbarch( 91, digv!11)
  plotbarch(108, digv!12)

  currcolour := col_black
  moveto(x0+135, y0+30)
  IF vin DO plotstr(vin)

  // Start symbol
  y1 := y0 - 216
  plotbar(9)
  plotbar(11)
  plotbardig(12, ltab!(digv!1))

  y1 := y0 - 202
  plotbardig(19, ltab!(digv!2))
  plotbardig(26, ltab!(digv!3))
  plotbardig(33, ltab!(digv!4))
  plotbardig(40, ltab!(digv!5))
  plotbardig(47, ltab!(digv!6))

  y1 := y0 - 216
  // Centre symbol
  plotbar(55)
  plotbar(57)

  y1 := y0 - 202
  plotbardig(59, rtab!(digv!7))
  plotbardig(66, rtab!(digv!8))
  plotbardig(73, rtab!(digv!9))
  plotbardig(80, rtab!(digv!10))
  plotbardig(87, rtab!(digv!11))

  y1 := y0 - 216
  plotbardig(94, rtab!(digv!12))

  // Right symbol
  plotbar(101)
  plotbar(103)

  wrgraph("pic.bmp")

fin:
  closegraphics()
*/
}

AND vin2digs(vin, dv) BE
{ LET x = 0
  TEST vin%0=7
  THEN { x :=        base36(vin%2)
         x := 36*x + base36(vin%3)
         x := 10*x + base10(vin%1)
         x := 10*x + base10(vin%4)
         x := 10*x + base10(vin%5)
         x := 10*x + base10(vin%6)
         x := 10*x + base10(vin%7)
       }
  ELSE { writef("*nvin = %s but must have length 7*n", vin)
         error := TRUE
       }

  dv!12 := 0
  FOR i = 11 TO 1 BY -1 DO
  { dv!i := x MOD 10
    x := x/10
  }
}

AND code2digs(code, dv) BE
{ LET len = code%0
  FOR i = 1 TO 12 DO dv!i := 0
  IF len>11 DO
  { writef("code string too long*n")
    error := TRUE
    RETURN
  }
  FOR i = 0 TO len-1 DO dv!(11-i) := base10(code%(len-i)) 
}

AND checkdig(dv) = VALOF
{ LET x = (dv!1+dv!3+dv!5+dv!7+dv!9+dv!11) * 3 +
          (dv!2+dv!4+dv!6+dv!8+dv!10)
  x := x MOD 10
  IF x DO x := 10-x
  RESULTIS x
}

AND base10(ch) = VALOF
{ IF '0'<=ch<='9' RESULTIS ch-'0'
  error := TRUE
  writef("Bad base10 digit %c*n", ch)
  RESULTIS 0
}

AND base36(ch) = VALOF
{ IF '0'<=ch<='9' RESULTIS ch-'0'
  IF 'A'<=ch<='Z' RESULTIS ch-'A'+10
  IF 'a'<=ch<='z' RESULTIS ch-'a'+10
  error := TRUE
  writef("Bad base36 digit %c*n", ch)
  RESULTIS 0
}

AND plotbarch(p, dig) BE
{ LET x = x0 + 3*p - 4
  LET y = y0 - 229
  moveto(x, y)
  //drawch('X')
  drawch(dig+'0')
}

AND plotbardig(p, bits) BE
{ 
//writef("plotbardig: p=%i3 %b7*n", p, bits)
  IF (bits & #b1000000) > 0 DO plotbar(p)
  IF (bits & #b0100000) > 0 DO plotbar(p+1)
  IF (bits & #b0010000) > 0 DO plotbar(p+2)
  IF (bits & #b0001000) > 0 DO plotbar(p+3)
  IF (bits & #b0000100) > 0 DO plotbar(p+4)
  IF (bits & #b0000010) > 0 DO plotbar(p+5)
  IF (bits & #b0000001) > 0 DO plotbar(p+6)
}

AND plotbar(p) BE
{ LET x = x0 + 3*p
  FOR i = 0 TO 2 DO
  { moveto(x+i, y1)
    drawto(x+i, y0)
  }
}
