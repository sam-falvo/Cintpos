/*
This is a program to draw EAN-13 and GS1-128 barcodes. 
It will also draw UPC-A barcodes since they are a subset
of EAN-13 codes.

Usage: text,to/k,ean/s,gs/s,-t/s

text      Text to encode
to/k      File to hold the bmp image of the barcode
ean/s     Draw a EAN-13 barcode -- 13 decimal digits
gs/s      Draw a GS1-128 barcode -- ASCII text        The default
-t/s      Turn on debugging trace

Implemented by Marin Richards (c) January 2015
*/

GET "libhdr"

// Insert the graphics library
//MANIFEST { g_grbase=450 }

GET "graphics"
GET "graphics.b"

GLOBAL {
  stdin:ug
  stdout
  infilename
  tofilename
  instream
  outstream

  x0;    y0
  x1;    y1
  xsize; ysize

  text
  barcodetype // =1 for EAN-13  =2 for GS1-128
  tracing
  code
  valv        // Values to be encoded from valv!0 to valv!(valvp-1)
  valvp
  error

  plotEANch
  setslct


  plotEANch
  currset    // ='A' for SetA  ='B' for SetB  ='C' for SetC
  ch2val     // Convert a character to a value based on the current set
  gscheckval
  gsbits     // A table giving GS value to stripe patterns
}

LET start() = VALOF
{ LET argv = VEC 50
  LET v = VEC 100
  valv := v            // Vector of values to encode
  stdout := output()
  stdin  := input()

  UNLESS rdargs("text,to/k,ean/s,gs/s,-t/s", argv, 50) DO
  { writef("Bad arguments for barcode*n")
    RESULTIS 0
  }

  xsize, ysize := 1000, 700      // Size of the image

  error := FALSE

  text := 0
  tofilename := "tstpic.bmp" // Default bmp file name
  barcodetype := 2           // Default barcode type GS1-128

  IF argv!0 DO text := argv!0             // text
  IF argv!1 DO tofilename := argv!1       // to/k
  IF argv!2 DO barcodetype := 1           // ean/s
  IF argv!3 DO barcodetype := 2           // gs/s
  tracing := argv!4                       // -t/s

// Table of value patterns for values 0 TO 106
  gsbits := TABLE
    #b11011001100,#b11001101100,#b11001100110,#b10010011000, //   0
    #b10010001100,#b10001001100,#b10011001000,#b10011000100, //   4
    #b10001100100,#b11001001000,#b11001000100,#b11000100100, //   8
    #b10110011100,#b10011011100,#b10011001110,#b10111001100, //  12
    #b10011101100,#b10011100110,#b11001110010,#b11001011100, //  16
    #b11001001110,#b11011100100,#b11001110100,#b11101101110, //  20
    #b11101001100,#b11100101100,#b11100100110,#b11101100100, //  24
    #b11100110100,#b11100110010,#b11011011000,#b11011000110, //  28
    #b11000110110,#b10100011000,#b10001011000,#b10001000110, //  32
    #b10110001000,#b10001101000,#b10001100010,#b11010001000, //  36
    #b11000101000,#b11000100010,#b10110111000,#b10110001110, //  40
    #b10001101110,#b10111011000,#b10111000110,#b10001110110, //  44
    #b11101110110,#b11010001110,#b11000101110,#b11011101000, //  48
    #b11011100010,#b11011101110,#b11101011000,#b11101000110, //  52
    #b11100010110,#b11101101000,#b11101100010,#b11100011010, //  56
    #b11101111010,#b11001000010,#b11110001010,#b10100110000, //  60
    #b10100001100,#b10010110000,#b10010000110,#b10000101100, //  64
    #b10000100110,#b10110010000,#b10110000100,#b10011010000, //  68
    #b10011000010,#b10000110100,#b10000110010,#b11000010010, //  72
    #b11001010000,#b11110111010,#b11000010100,#b10001111010, //  76
    #b10100111100,#b10010111100,#b10010011110,#b10111100100, //  80
    #b10011110100,#b10011110010,#b11110100100,#b11110010100, //  84
    #b11110010010,#b11011011110,#b11011110110,#b11110110110, //  88
    #b10101111000,#b10100011110,#b10001011110,#b10111101000, //  92
    #b10111100010,#b11110101000,#b11110100010,#b10111011110, //  96
    #b10111101110,#b11110101110,#b11110101110,#b11010000100, // 100
    #b11010010000,#b11010011100,#b11000111010                // 104

  UNLESS text DO text := "9780471976974"  // This is an ISBN number

  IF error DO
  { writef("*nThere were error(s)*n*n")
    RESULTIS 0
  }

  //IF tracing DO
  //{ FOR i = 0 TO valvp-1 DO writef(" %i2", valv!i)
  //  newline()
  //}


  IF barcodetype=1 DO plotEANcode(text)

  IF barcodetype=2 DO plotGScode(text)

  newline()
  RESULTIS 0
}

AND plotEANcode() BE
{ LET atab = TABLE #b0001101, #b0011001, #b0010011, #b0111101, #b0100011,
                   #b0110001, #b0101111, #b0111011, #b0110111, #b0001011

  LET btab = TABLE #b0100111, #b0110011, #b0011011, #b0100001, #b0011101,
                   #b0111001, #b0000101, #b0010001, #b0001001, #b0010111

  LET ctab = TABLE #b1110010, #b1100110, #b1101100, #b1000010, #b1011100,
                   #b1001110, #b1010000, #b1000100, #b1001000, #b1110100

  //                    234567   Digit positions for left digits
  LET setslct = TABLE #b000000,  // 0   0 = use set A, 1 = use set B
                      #b001011,  // 1
                      #b001101,  // 2
                      #b001110,  // 3
                      #b010011,  // 4
                      #b011001,  // 5
                      #b011100,  // 6
                      #b010101,  // 7
                      #b010110,  // 8
                      #b011010   // 9

  LET set = 0 // The parity settings to encode the thirteenth digit.

  // Each of the six left hand digits can be encoded with either an
  // even or odd parity pattern of stripes. The selection of which
  // parity is used for these six digits encodes the thirteenth
  // digit of an EAN-13 barcode using setslct which is a table giving
  // the parity pattern for each digit. 

  text2digs(text, valv)

  valv!12 := checkdig(valv) // Check digit for UPC-A barcodes

  set := setslct!(valv!0) // The thirteenth digit is held in valv!0 and
                          // the six left digits are in valv!1 to valv!6

  x0 := xsize/2 - 56*3
  y0 := ysize/2 + 100

  UNLESS opengraphics(xsize, ysize, mode8bit) DO
  { writef("Unable to open the graphics library*n")
    GOTO fin
  }

  currcolour := col_black
  plotEANch(  5, valv! 0)

  plotEANch( 16, valv! 1)
  plotEANch( 23, valv! 2)
  plotEANch( 30, valv! 3)
  plotEANch( 37, valv! 4)
  plotEANch( 44, valv! 5)
  plotEANch( 51, valv! 6)

  plotEANch( 61, valv! 7)
  plotEANch( 68, valv! 8)
  plotEANch( 75, valv! 9)
  plotEANch( 82, valv!10)
  plotEANch( 89, valv!11)
  plotEANch( 96, valv!12)

  plotEANch(108, '>')

  currcolour := col_black

  // Start symbol
  y1 := y0 - 216
  plotslice(9)
  plotslice(11)

  y1 := y0 - 202
  // Draw the left hand digits using table atab or btab
  plotEANdig(12, ((set & #b100000) = 0->atab,btab)!(valv!1))
  plotEANdig(19, ((set & #b010000) = 0->atab,btab)!(valv!2))
  plotEANdig(26, ((set & #b001000) = 0->atab,btab)!(valv!3))
  plotEANdig(33, ((set & #b000100) = 0->atab,btab)!(valv!4))
  plotEANdig(40, ((set & #b000010) = 0->atab,btab)!(valv!5))
  plotEANdig(47, ((set & #b000001) = 0->atab,btab)!(valv!6))

  y1 := y0 - 216
  // Centre symbol
  plotslice(55)
  plotslice(57)

  y1 := y0 - 202
  // Draw the right hand digits using table ctab
  plotEANdig(59, ctab!(valv!7))
  plotEANdig(66, ctab!(valv!8))
  plotEANdig(73, ctab!(valv!9))
  plotEANdig(80, ctab!(valv!10))
  plotEANdig(87, ctab!(valv!11))
  plotEANdig(94, ctab!(valv!12))

  // Right symbol
  y1 := y0 - 216
  plotslice(101)
  plotslice(103)

  wrgraph(tofilename)

fin:
  closegraphics()
}

AND num2ch(ch) = 32<=ch<=127 -> ch, '?'

AND GStext2valv(str) BE
{ // Convert str to GS1-128 values
  LET p = 1  // Position in str
  LET len = str%0
  LET startch = 104 //StartB
  currset := 'B'
  valvp := 0        // No date in valv yet

  // Determine the start character
  IF is2digs(str,p) |
     digcount(str,p)>=4     DO currset, startch := 'C', 105 // StartC
  IF ctrlbeforelower(str,p) DO currset, startch := 'A', 103 // StartA

  valv!valvp := startch   // The Start character
  IF tracing DO
  { writef("*n*ntext2val: %i3: %c %i3 %11b   Start%c*n",
            valvp, ' ', startch, gsbits!startch, currset)
//abort(1000)
  } 
  valvp := valvp+1

  // The GS1-128 specification requires the next value in the
  // barcode to be 102 (FNC1), but this seems to be optional.
  IF FALSE DO // So we omit it for the time being
  { valv!valvp := 102       // FNC1
    IF tracing DO
    { writef("text2val: %i3: %c %i3 %11b   FNC1*n",
              valvp, currset, valv!valvp, gsbits!(valv!valvp))
//abort(1000)
    } 
    valvp := valvp+1
  }

  IF currset='C' DO
  { // Encode digit pairs using SetC
    WHILE p<len & isdig(str%p) & isdig(str%(p+1)) DO
    { valv!valvp := (str%p-'0')*10 + (str%(p+1)-'0') // In range 00 to 99
      p := p+2
      IF tracing DO
      { writef("text2val: %i3: %c %i3 %11b   Digit pair*n",
               valvp, currset, valv!valvp, gsbits!(valv!valvp))
      }
      valvp := valvp+1
    }

    // Select either code set A or B if there is more text
    IF p <= len TEST ctrlbeforelower(str, p)
    THEN { valv!valvp := 101 // CodeA
           IF tracing DO
           { writef("text2val: %i3: %c %i3 %11b   CodeA*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
           } 
           currset := 'A'      // SetA
           valvp := valvp+1
         }
    ELSE { valv!valvp := 100 // CodeB
           IF tracing DO
           { writef("text2val: %i3: %c %i3 %11b   CodeB*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
           } 
           currset := 'B'      // SetB
           valvp := valvp+1
         }
  }

  // Convert the remaining characters

  UNTIL p > len DO
  { // We are currently using code set A or B

    // Check whether it is worth changing to SetC
    LET n = digcount(str, p)

    IF n>=4 DO
    { // It is worth changing to SetC
      IF n MOD 2 = 1 DO
      { // There are an odd number of digits so convert one
        // using the current code set before changing to SetC
        LET ch = str%p
        //IF tracing DO
        //{ writef("text2val: n =%n is odd*n", n)
        //} 
        valv!valvp := ch2val(ch)
        IF tracing DO
        { writef("text2val: %i3: %c %i3 %11b   '%c'*n",
                  valvp, currset, valv!valvp, gsbits!(valv!valvp), num2ch(ch))
        } 
        valvp := valvp+1
        p := p+1
      }

      // There is now an even number of digits at p
      valv!valvp := 99 // Code C
      IF tracing DO
      { writef("text2val: %i3: %c %i3 %11b   CodeC*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
      } 
      valvp := valvp+1
      currset := 'C' 

      // Encode these digit pairs using SetC
      WHILE p<len & isdig(str%p) & isdig(str%(p+1)) DO
      { valv!valvp := (str%p-'0')*10 + (str%(p+1)-'0') // In range 00 to 99
        IF tracing DO
        { writef("text2val: %i3: %c %i3 %11b   Digit pair*n",
               valvp, currset, valv!valvp, gsbits!(valv!valvp))
        } 
        valvp := valvp+1
        p := p+2
      }

      IF p>len BREAK // No more text

      // Next character is not a digit so select code set A or B

      TEST ctrlbeforelower(str, p)
      THEN { valv!valvp := 101 // CodeA
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   CodeA*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
             } 
             valvp := valvp+1
             currset := 'A'      // SetA
           }
      ELSE { valv!valvp := 100 // CodeB
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   CodeB*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
             } 
             valvp := valvp+1
             currset := 'B'      // SetB
           }
    }

    // We have at least one character to convert and
    // we are currently using code set A or B

    IF currset='B' DO
    { LET ch = str%p

      IF 0 <= ch <= 31 TEST ctrlbeforelower(str, p+1)
      THEN { valv!valvp := 98  // Code A
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   CodeA*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
             } 
             currset := 'A'      // SetA
             valvp := valvp+1

             valv!valvp := ch2val(ch)
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   '%c'*n",
                      valvp, currset, valv!valvp, gsbits!(valv!valvp), num2ch(ch))
             } 
             valvp := valvp+1
             p := p+1
             LOOP
           }
      ELSE { valv!valvp := 98  // Shift -- B to A
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   Shift*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
             } 
             currset := 'A'      // SetA
             valvp := valvp+1

             valv!valvp := ch2val(ch)
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   '%c'*n",
                      valvp, currset, valv!valvp, gsbits!(valv!valvp), num2ch(ch))
             } 
             currset := 'B'      // Automatically return to SetB
             valvp := valvp+1
             p := p+1
             LOOP
           }
      }

    IF currset='A' DO
    { LET ch = str%p

      IF 96 <= ch <= 127 TEST ctrlbeforelower(str, p+1)
      THEN { valv!valvp := 98  // Shift -- A to B
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   Shift*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
             } 
             currset := 'B'      // SetB
             valvp := valvp+1

             valv!valvp := ch2val(ch)
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   '%c'*n",
                      valvp, currset, valv!valvp, gsbits!(valv!valvp), num2ch(ch))
             } 
             currset := 'B'      // Automatically return to SetA
             valvp := valvp+1
             p := p+1
             LOOP
           }
      ELSE { valv!valvp := 100 // Code B
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   CodeB*n",
                    valvp, currset, valv!valvp, gsbits!(valv!valvp))
             } 
             currset := 'B'      // SetB
             valvp := valvp+1

             valv!valvp := ch2val(ch)
             IF tracing DO
             { writef("text2val: %i3: %c %i3 %11b   '%c'*n",
                      valvp, currset, valv!valvp, gsbits!(valv!valvp), num2ch(ch))
             } 
             p := p+1
             LOOP
           }
    }

    { LET ch = str%p
      valv!valvp := ch2val(ch)
      IF tracing DO
      { writef("text2val: %i3: %c %i3 %11b   '%c'*n",
               valvp, currset, valv!valvp, gsbits!(valv!valvp), num2ch(ch))
      } 
      valvp := valvp+1
      p := p+1
      LOOP
    }
  }

  valv!valvp := checkval()
  IF tracing DO
  { writef("text2val: %i3: %c %i3 %11b   Check value*n",
    valvp, currset, valv!valvp, gsbits!(valv!valvp))
  } 
  valvp := valvp+1

  valv!valvp := 106
  IF tracing DO
  { writef("text2val: %i3: %c %i3 %11b11 Stop*n",
    valvp, currset, valv!valvp, gsbits!(valv!valvp))
  } 
  valvp := valvp+1
}

AND checkval() = VALOF
{ LET sum = valv!0
  FOR i = 1 TO valvp-1 DO sum := sum + i*valv!i
  RESULTIS sum MOD 103
}

AND isdig(ch) = '0'<=ch<='9' -> TRUE, FALSE

AND is2digs(str,p) = VALOF
{ // Return TRUE if p points to the final 2 characters of str and they
  // are both digits.
  UNLESS str%0=p+1 & isdig(str%p) & isdig(str%(p+1)) RESULTIS FALSE
  RESULTIS TRUE
}

AND digcount(str, p) = VALOF
{ // Returns the number of digits at p in str.
  LET res = 0
  FOR i = p TO str%0 DO
  { LET ch = str%i
    UNLESS '0'<=ch<='9' BREAK
    res := res+1
  }
  RESULTIS res
}

AND ctrlbeforelower(str, p) = VALOF
{ // Returns TRUE if there is a character in the range 0 to 31
  // before any charcter in the range 96 to 127.
  LET ctrlchfound = FALSE
  FOR i = p TO str%0 DO
  { LET ch = str%i
    IF 0 <= ch <= 31 DO ctrlchfound := TRUE
    IF 96 <=ch <= 127 RESULTIS ctrlchfound
  }
  RESULTIS FALSE
}

AND ch2val(ch) = VALOF
{ IF currset='C' RESULTIS 0 <= ch <= 99 -> ch, -1

  // currset must be A or B
  IF currset='A' DO
  { IF  0 <= ch <= 31 RESULTIS 64+ch
    IF 32 <= ch <= 95 RESULTIS ch-32
    RESULTIS -1
  }

  IF currset='B' & 32 <= ch <= 127 RESULTIS ch-32

  RESULTIS -1

}

AND text2digs(s, v) BE
{ // Encode EAN-13 string s as values in v!0 ..  v!12
  LET len = s%0
  FOR i = 0 TO 12 DO v!i := 0
  IF len>13 DO len := 13
  FOR i = 0 TO len-1 DO v!(12-i) := ch2num(s%(len-i)) 
}

AND checkdig(v) = VALOF
{ // Used in UPC-A barcodes and also apparently in EAN-13 code.
  // v!0 to v!11 holds decimal digits in range 0 to 9
  // This function returns the check digit to be placed
  // in v!12 for a UPC-A or EAN-13 barcode.
  LET x = (v!1+v!3+v!5+v!7+v!9+v!11) * 3 +
          (v!0+v!2+v!4+v!6+v!8+v!10)
  x := x MOD 10
  IF x DO x := 10-x
  RESULTIS x
}

AND ch2num(ch) = VALOF
{ IF '0'<=ch<='9' RESULTIS ch-'0'
  error := TRUE
  writef("Bad ch2num digit '%c'*n", ch)
  RESULTIS 0
}

AND plotEANch(p, dig) BE
{ LET x = x0 + 3*p - 4
  LET y = y0 - 229
  moveto(x, y)
  TEST dig>10
  THEN drawch(dig)
  ELSE drawch(dig+'0')
}

AND plotGSch(p, ch) BE
{ LET x = x0 + 3*p - 4
  LET y = y0 - 150
  moveto(x, y)
  drawch(ch)
}

AND plotEANdig(p, bits) BE
{ 
//writef("plotEANdig: p=%i3 %b7*n", p, bits)
  IF (bits & #b1000000) > 0 DO plotslice(p)
  IF (bits & #b0100000) > 0 DO plotslice(p+1)
  IF (bits & #b0010000) > 0 DO plotslice(p+2)
  IF (bits & #b0001000) > 0 DO plotslice(p+3)
  IF (bits & #b0000100) > 0 DO plotslice(p+4)
  IF (bits & #b0000010) > 0 DO plotslice(p+5)
  IF (bits & #b0000001) > 0 DO plotslice(p+6)
}

AND plotGSval(p, bits) BE
{ LET s = ""
  IF (bits & #b10000000000) > 0 DO plotslice(p)
  IF (bits & #b01000000000) > 0 DO plotslice(p+1)
  IF (bits & #b00100000000) > 0 DO plotslice(p+2)
  IF (bits & #b00010000000) > 0 DO plotslice(p+3)
  IF (bits & #b00001000000) > 0 DO plotslice(p+4)
  IF (bits & #b00000100000) > 0 DO plotslice(p+5)
  IF (bits & #b00000010000) > 0 DO plotslice(p+6)
  IF (bits & #b00000001000) > 0 DO plotslice(p+7)
  IF (bits & #b00000000100) > 0 DO plotslice(p+8)
  IF (bits & #b00000000010) > 0 DO plotslice(p+9)
  IF (bits & #b00000000001) > 0 DO plotslice(p+10)

  IF bits = #b11000111010 DO // The first 11 stripes of stop code
  { plotslice(p+11) // Add the last two stripes of the stop code
    plotslice(p+12)
    s := "11"
  }
  //IF tracing DO
  //  writef("plotGSval: %i3: %11b%s*n", p, bits, s)
}

AND plotslice(p) BE
{ LET x = x0 + 3*p
  FOR i = 0 TO 2 DO
  { moveto(x+i, y1)
    drawto(x+i, y0)
  }
}

AND plotGScode(str) BE
{ x0 := 20 //xsize/2 - (11*valvp)*3/2
  y0 := ysize/2 + 50

  UNLESS opengraphics(xsize, ysize, mode8bit) DO
  { writef("Unable to open the graphics library*n")
    GOTO fin
  }

  GStext2valv(str, valv)

  currcolour := col_black

  FOR i = 1 TO str%0 DO plotGSch(3*i, str%i)

  currcolour := col_black
  moveto(x0+135, y0+30)

  y1 := y0 - 100

  // Draw the value barcodes
  FOR i = 0 TO valvp-1 DO plotGSval(11*i, gsbits!(valv!i))

  wrgraph(tofilename)

fin:
  closegraphics()
}

