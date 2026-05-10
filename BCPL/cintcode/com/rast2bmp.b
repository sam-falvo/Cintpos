/*
This program reads a raster file (typically RASTER) converting it
into an image (typically junk.bmp) in bmp format which can then be
using gimp or converted to png format using ffmpeg.

Implemented by Martin Richards (c) 26 Feb 2025

History

26/02/2025
Made minor modifications to the command arguments.
Unless specified by the user it selects the layout and labelling
of the grid lines.
*/

GET "libhdr"
GET "plotflow.h"

/*
y4
y3  x1  x2---------------------------------------x3  x4
        |      |      |                          |
        |      |      |                          |
   100K |----------------  etc   Raster image    |
        |      |      |                          |
        |      |      |                          |
y2   0K x2---------------------------------------x3

y1     0M     1M     2M ....
y0
*/

MANIFEST {
  y0 = 10            // Top edge
  y1 = 50            // X coords
  y2 = 80            // Base of raster image
  y3 = y2+1400       // Top of raster image
  y4 = y3+100        // Top edge
    
  x0 = 10            // Left edge
  x1 = 100           // Y coords
  x2 = x1+40         // Left edge of raster image
  x3 = x2+2000       // Right edge of raster image
  x4 = x3+100        // Right edge

  width  = x4+3 & -4 // BMP image width and height
  height = y4

  imagewidth  = x3-x2 // width and height of the raster data
  imageheight = y3-y2 // used to scale the image
}

GLOBAL {
  stdin:ug
  stdout
  
  ch
  charv               // Used by charvwritef
  charvwritef
  charvwrch
  
  lineno              // Current input line number.
  rastlineno          // Current raster line number.
  addr

  fromfilename
  fromstream
  
  tofilename
  tostream
  
  countmax            // From the F parameter
  addrmax             // From the M parameter
  countsperline       // From the K parameter
  scale               // From the S parameter

  gridxsep
  gridysep
  
  count               // Instruction count from the start
                      // of the current raster line.
  memaddr             // A raster point assumes byte addresses
                      // memaddr to memaddr+bytesperpoint-1 have
		      // been accessed.

  // These are set from the RASTER data, typically m1000 S8
  // The rest of the RASTER data specifies raster line, typically like
  // W23 B132 W34 B92 N
  // giving runs of white and black points.
  // Each raster line is terminated by N.
  // count starts at zero and is is incremented by countsperline on every N.
  // Bn in the raster data fills a black rectangle corresponding to
  // counts from count to count+countsperline-1 and memory locations from
  // addr to addr+n*bytesperpoint-1.
  // This black rectangle is scaled to fit in the image rectangle
  // (x2,y2,x3,y3) representing count values from 0 to countmax and
  // 0 to addrmax.

  scalex      // Scale Cintcode instruction counts to image x coords
  scaley      // Scale Cintcode memory addresses to image y coords
}

LET scalex(count) = x2 + muldiv(count, x3-x2, countmax)

LET scaley(addr)  = y2 + muldiv(addr, y3-y2,  addrmax)

LET drawlabel(dir, count, addr, len, str) BE
{ LET x = scalex(count)
  LET y = scaley(addr)
  LET slen = strpixels(str)
  LET dx, dy, sx, sy = 0, 0, 0, 0
  SWITCHON dir INTO
  { DEFAULT:
      writef("System error in drawlabel, dir=^n*n", dir)
      abort(999)
      RETURN
    CASE 00: dx, dy, sx, sy := -len,    0, -slen/2-20,   0;  ENDCASE
    CASE 05: dx, dy, sx, sy := -len, -len,        -20, -20;  ENDCASE
    CASE 01: dx, dy, sx, sy :=    0, -len,          0, -20;  ENDCASE
    CASE 15: dx, dy, sx, sy :=  len, -len,         20, -20;  ENDCASE
    CASE 02: dx, dy, sx, sy :=  len,    0,  slen/2+20,   0;  ENDCASE
    CASE 25: dx, dy, sx, sy :=  len,  len,         20,  20;  ENDCASE
    CASE 03: dx, dy, sx, sy :=    0,  len,          0,  20;  ENDCASE
    CASE 35: dx, dy, sx, sy := -len,  len,        -20,  20;  ENDCASE
  }
  //writef("drawlabel: dir=%n str=%s*n", dir, str)
  //writef("drawlabel: countmax=%n addrmax=%n, count=%n addr=%n*n",
  //        countmax, addrmax, count, addr)
  //writef("drawlabel: x=%n y=%n, dx=%n dy=%n sx=%n sy=%n slen=%n str=%s*n",
  //        x, y, dx, dy, sx, sy, slen, str)
  //abort(2345)

  setcolour(col_black)

  drawarrow(dir, x, y, 15)
  moveto(x, y); drawby(dx, dy)
  drawstrcentred(x+dx+sx, y+dy+sy, str)
  setcolour(col_black)
  RETURN
}

LET drawrasterdata(b) = VALOF
{ // The raster data stream has already been opened.
  // Return TRUE if the ratster data is read successfully
  // If b=TRUE this function returns after just reading
  // the F and M parameters, having set addrmax and countmax.
  // The second time drawrasterdata is called b-FALSE causing it to
  // process the rest of the raster data.
  
  lineno := 1
  rastlineno := 1
  addr, count := 0, 0
  ch := rdch()
  
  UNTIL ch=endstreamch DO
  { LET black = FALSE
    LET val = 0
    //writef("ch='%c'*n", ch)
    //abort(5566)
    SWITCHON capitalch(ch) INTO
    { DEFAUT:
        writef("Bad character '%c' on line %n of the raster data file*n", ch, lineno)
        RESULTIS FALSE

      CASE '*n':
        lineno := lineno+1
      CASE '*s':
      CASE '*t':
        ch := rdch()
	LOOP

      CASE 'F':
       ch := rdch()
       countmax := rdnum()
       writef("countmax = %n*n", countmax)
       LOOP
      
      CASE 'M':
       ch := rdch()
       addrmax := rdnum()
       writef("addrmax = %n*n", addrmax)
       IF b RESULTIS TRUE
       LOOP
       
      
      CASE 'K':
       ch := rdch()
       countsperline := rdnum()
       writef("countsperline = %n*n", countsperline)
       LOOP
      
      CASE 'S':
       ch := rdch()
       scale := rdnum()
       writef("scale = %n*n", scale)
       LOOP
      
      CASE 'N': 
        count := count+countsperline // Value of count at the
	                             // start of the next raster line
        IF count MOD 2_000_000 = 0 DO
          writef("count=%n*n", count)
	addr := 0
	ch := rdch()
	rastlineno := rastlineno+1
	//writef("rastlineno=%i5*n", rastlineno)
	LOOP

     CASE 'B':
       black := TRUE
     CASE 'W':
       ch := rdch()
       val := rdnum()
       IF black DO
         dopoint(addr, count, val) // Black from addr to addr+val
       addr := addr+val
       LOOP
    }
  }
  RESULTIS TRUE
}

AND rdnum() = VALOF
{ LET res = 0
  LET ok = FALSE
  WHILE '0' <= ch <= '9' DO
  { ok := TRUE
    res := 10*res + ch - '0'
    ch := rdch()
  }
  UNLESS ok DO
    writef("Bad number on line %n", lineno)
  RESULTIS res
}

AND dopoint(addr, count, val) BE
{ // Draw a scaled black rectangle for byte addresses
  // addr to addr+val
  // count is the Cintcode instruction counter are the start
  // of the current raster line.
  LET yl = y2 + muldiv(imageheight, addr*scale, addrmax) 
  LET yh = y2 + muldiv(imageheight, (addr+val)*scale, addrmax)
  LET xl = x2 + muldiv(imagewidth, count, countmax)
  LET xh = x2 + muldiv(imagewidth, count+1, countmax)
  //writef("Black rect ( %i4, %i4 %i4, %i4)*n", xl, yl, xh, yh)
  FOR y = yl TO yh DO penS1(xl, y)
  //fillrect(xl, yl, xh, yh)
//  abort(1000)
}

LET drawdiagram() = VALOF
{ // The raster data stream has already been opened.
  // Return TRUE if the rastster data is read successfully
  LET r = bendradius
  LET v = VEC 10    // For charv needed by charvwritef
  charv := v
  
  currpen := penS3

  addrmax  := -1
  countmax := -1

  drawrasterdata(TRUE) // Just read the F and M parameters

  setcolour(#xE0D0FF)
  fillrect(x2, y2, x3-100, y3-5)
  setcolour(col_black)

  drawgridlines()
  //drawlabel(35,   1_800_000,  75_000, 80, "SYN")
  //drawlabel(25,   2_300_000,  95_000, 50, "Stack")

  drawrasterdata(FALSE)
}

AND drawgridlines() BE
{
  UNLESS gridxsep>0 DO
  { gridxsep := VALOF
    { IF countmax/     1_000 < 20 RESULTIS      1_000
      IF countmax/     2_000 < 20 RESULTIS      2_000
      IF countmax/     5_000 < 20 RESULTIS      5_000
      IF countmax/    10_000 < 20 RESULTIS     10_000
      IF countmax/    20_000 < 20 RESULTIS     20_000
      IF countmax/    50_000 < 20 RESULTIS     50_000
      IF countmax/   100_000 < 20 RESULTIS    100_000
      IF countmax/   200_000 < 20 RESULTIS    200_000
      IF countmax/   500_000 < 20 RESULTIS    500_000
      IF countmax/ 1_000_000 < 20 RESULTIS  1_000_000
      IF countmax/ 2_000_000 < 20 RESULTIS  2_000_000
      IF countmax/ 5_000_000 < 20 RESULTIS  5_000_000
      IF countmax/10_000_000 < 20 RESULTIS 10_000_000
      RESULTIS 20_000_000
    }
  }

  UNLESS gridysep>0 DO
  { gridysep := VALOF
    { IF addrmax/     1_000 < 10 RESULTIS      1_000
      IF addrmax/     2_000 < 10 RESULTIS      2_000
      IF addrmax/     5_000 < 10 RESULTIS      5_000
      IF addrmax/    10_000 < 10 RESULTIS     10_000
      IF addrmax/    20_000 < 10 RESULTIS     20_000
      IF addrmax/    50_000 < 10 RESULTIS     50_000
      IF addrmax/   100_000 < 10 RESULTIS    100_000
      IF addrmax/   200_000 < 10 RESULTIS    200_000
      IF addrmax/   500_000 < 10 RESULTIS    500_000
      IF addrmax/ 1_000_000 < 10 RESULTIS  1_000_000
      IF addrmax/ 2_000_000 < 10 RESULTIS  2_000_000
      IF addrmax/ 5_000_000 < 10 RESULTIS  5_000_000
      IF addrmax/10_000_000 < 10 RESULTIS 10_000_000
      RESULTIS 20_000_000
    }
  }

  writef("drawgridlines: countmax=%n addrmax=%n*n", countmax, addrmax)
  writef("drawgridlines: gridxsep=%n gridysep=%n*n", gridxsep, gridysep)

  UNLESS gridysep>0 DO
  { gridysep := 50_000
  }

  // Horizontal grid lines
  { LET y = 0
    UNTIL y > addrmax DO
    { LET sy = scaley(y)
      // Choose the y coordinate format
      TEST addrmax >= 1_000_000
      THEN charvwritef("%4iM", y/1_000_000)
      ELSE charvwritef("%4iK", y/1_000)
      setcolour(col_black)
      drawstr(scalex(0)-strpixels(charv)-20, sy, charv)
      setcolour(col_red)
      currpen := penS2
      moveto(x2, sy); drawto(x3, sy)
      setcolour(col_black)
      y := y + gridysep // Step up to the next y grid line.
    }
  }

  // Vertical grid line
  { LET x = 0
    UNTIL x > countmax DO
    { LET sx = scalex(x)
      // Choose the x coordinate format
      TEST countmax >= 1_000_000
      THEN charvwritef("%nM", x/1_000_000)
      ELSE charvwritef("%nK", x/1_000)
      setcolour(col_black)
      drawstrcentred(sx, y2-30, charv)
      setcolour(col_red)
      currpen := penS2
      moveto(sx, y2); drawto(sx, y3)
      setcolour(col_black)
      x := x + gridxsep // Step up to the next y grid line.
    }
  }
}

LET charvwritef(format, a, b, c, d, e) = VALOF
{ LET prevwrch = wrch
  wrch := charvwrch
  charv!0 := 0
  writef(format, a, b, c, d, e)
  wrch := prevwrch
  RESULTIS charv
}

AND charvwrch(ch) BE
{ LET len = charv%0+1
  charv%0, charv%len := len, ch
}

LET start() = VALOF
{ LET stdout = output()
  LET widthonly = FALSE
  LET argv = VEC 50

  gridxsep, gridysep := 0, 0
  
  UNLESS rdargs("from,to/k,xsep=-xs/N,ysep-ys/N,-d/S,-w/S", argv, 50) DO
  { writef("Bad arguments for tst, format: -s/s*n")
    RESULTIS 0
  }

  fromfilename := "RASTER"
  IF argv!0 DO fromfilename := argv!0 // from
  
  tofilename := "junk.bmp"
  IF argv!1 DO tofilename := argv!1   // to/K

  IF argv!2 DO gridxsep := !argv!2    // xsep-xs/N
  IF argv!3 DO gridysep := !argv!3    // ysep-ys/N

  widthonly := argv!4                 // -w/S

  UNLESS openbdraw(width, height) DO
  { writef("Unable to open the graphics library*n")
    GOTO fin
  }

  fromstream := findinput(fromfilename)
  UNLESS fromstream DO
  { writef("Trouble with file: %s*n", fromfilename)
    GOTO fin
  }
  selectinput(fromstream)
  UNLESS read_raster_params() GOTO fin
  // maxbyteaddr
  // countmax
  // kval
  // sval
  
  
  currcol := col_black
  selectfont(16)
  charleveloffset := charmidleveloffset
  currpen := penS3
  
  { // Assume 12 chars per inch ie 12*(fontW+charHsep) pixels per inch
    // or 12*(charW+charHsep) pixels per 25.4 mm
    // so one pixel has a width of 25.4 / (12*(fontW+charHsep)) mm
    // So the width in mm of the image is width * 25.4 / (12*(fontW+charHsep))
    LET mmwidth = width * 254 / (12*(fontW+charHsep))
    // The is a scaled with 1 digit after the decimal point. 
    writef("*n%s: width = %n pixels  height = %n pixels       *
            *width in mms = %5.1d*n*n",
            tofilename, width, height, mmwidth)
    delay(10_000)
  }

  IF widthonly GOTO fin   // Only output the image width not generating
                          // the .bmp file.
  
draw:
  drawdiagram(fromstream)
  wrbmp(tofilename)

fin:
  IF fromstream DO endstream(fromstream)
  closebdraw()

  RESULTIS 0
}

AND read_raster_params() = VALOF
{ // This function reads the raster data file to set the
  // raster parameters, namely:
  
  // instrcountmax  The number of Cintcode instructions represented
  //                in the raster data.
  // countmax       =instrcountmax/kval, the number of raster lines.
  // addrmax        is the highest byte address accessed
  // memvupb        = addrmax/kval, the upb of memv
  // kval           is the number of Cintcode instructions obeyed
  //                per raster line.
  // sval           The number of address bytes represented by
  //                each point in the raster data.
  // The first line of a Raster file is of the form:

  //       F<instrcountmax> M<addrmax> K<kval> S<sval>
  
  instrcountmax  := 0
  countmax  := 0      // Number of the last raster line
  addrmax := 0        // Highest byte address referenced
  kval := 0           // Number of Cintcode instruction per line
  sval := 0           // Number of byte addresses per raster unit
  memvupb := 0

  IF FALSE DO
  { instrcountmax  := 1_000_000
    countmax  := 1_000_000
    addrmax := 100_000 // max byte address
    memvupb := 0
    kval := 1000
    sval := 8
    fcountmax := FLOAT countmax
    fkval := FLOAT kval

    countmax := instrcountmax/kval
    memvupb := addrmax/ sval
  
    RESULTIS TRUE
  }
  
  ch := rdch() REPEATWHILE ch='*s'

  IF ch='F' DO
  { ch := rdch()
    instrcountmax := rdn()
  }
  
  WHILE ch=' ' DO ch := rdch()
  
  IF ch='M' DO
  { ch := rdch()
    addrmax := rdn()
  }
  
  WHILE ch=' ' DO ch := rdch()
  
  IF ch='K' DO
  { ch := rdch()
    kval := rdn()
  }
  WHILE ch=' ' DO ch := rdch()

  IF ch='S' DO
  { ch := rdch()
    sval := rdn()
  }

  UNLESS instrcountmax & addrmax DO
  { writef("Bad RASTER file*n")
    RESULTIS FALSE
  }
  
  fcountmax := FLOAT countmax
  fkval := FLOAT kval

  countmax := instrcountmax/kval
  memvupb := addrmax/ sval
  
  RESULTIS TRUE
}


