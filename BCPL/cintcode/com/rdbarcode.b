/*
############ THIS is in the early staged of development ##########

This is a program to decode barcode.

Implemented by Marin Richards (c) December 2015
*/

GET "libhdr"

// Insert the graphics library
//MANIFEST { g_grbase=450 }

GET "graphics"
GET "graphics.b"

GLOBAL {
  x0:ug; y0
  x1;    y1
  xsize; ysize
  maxpixel // Max gray scale value, typically 255

  rowlen   // Length of a row including padding in bytes.
  picv     // Vector of pixel bytes

  byteupb
  wordupb

  infilename
  instream

  stdin
  stdout
  ch
  rdn // Read a number from the .pgm file.
  pos

  vin
  code
  digv
  error

  plotbarch
}

LET start() = VALOF
{ 
  LET argv = VEC 50
  LET dv = VEC 12
  digv := dv
  FOR i = 1 TO 12 DO digv!i := 0
  stdin  := input()
  stdout := output()

  UNLESS rdargs("from,-t/s", argv, 50) DO
  { writef("Bad arguments for rdbarcode*n")
    RESULTIS 0
  }

  error := FALSE
  vin := 0

  infilename := "barcode.pgm"

  IF argv!0 DO infilename := argv!0

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

  FOR i = 1 TO 12 DO writef(" %i2", digv!i)
  newline()
  IF vin DO writef("vin=%s*n", vin)

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
  UNLESS ch='2' RESULTIS FALSE
  ch := rdch()

  writef("File starts with P2*n")

  xsize := rdn()
  ysize := rdn()
  maxpixel := rdn()

  rowlen := xsize
  UNTIL rowlen MOD 4 = 0 DO rowlen := rowlen+1

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
  RESULTIS TRUE
}

AND readrow() BE
{ FOR i = 1 TO xsize DO
  { picv%pos := rdn()
    pos := pos+1
  }
  UNTIL pos MOD 4 = 0 DO
  { picv!pos := 0
    pos := pos+1
  }
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

AND plotbarcode() BE
{ LET ltab = TABLE #b0001101, #b0011001, #b0010011, #b0111101, #b0100011,
                   #b0110001, #b0101111, #b0111011, #b0110111, #b0001011

  LET rtab = TABLE #b1110010, #b1100110, #b1101100, #b1000010, #b1011100,
                   #b1001110, #b1010000, #b1000100, #b1001000, #b1110100


  x0 := xsize/2 - 56*3
  y0 := ysize/2 + 100

///*
  UNLESS opengraphics(xsize, ysize, mode8bit) DO
  { writef("Unable to open the graphics library*n")
    GOTO fin
  }

  // Draw all colours along the bottom of the canvas.
  FOR x = 1 TO xsize-2 DO
  { currcolour := col_red
    drawpoint33(x, 1)
    drawpoint33(x, ysize-2)
    moveto(x, 4)
    currcolour := 255*x/xsize
    drawby(0, 20)    
  }

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
  IF vin DO drawstr(vin)

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
//*/
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
