/*
A barcode reader program.

Implemented by Martin Richards (c) January 2016

Usage: from,to/k,ean/s,gs/s,-t/s

from     This specifies an input file of grey scale scan lines
         in ASCII seperated by dots.

to/k     This specifies the destination of the decoded barcode.

ean/s    Specifies the the barcode is in EAN-13 format representing
         13 decimal digits, but it will also decode UPC-A barcodes
         since these are a subset of EAN-13 barcodes.

gs/s     Specifies the the barcode is in GS1-128 format representing
         a variable length sequence of ASCII characters.

I will use the term stripe to mean the thinnest line of black or white
in a barcode. The term black bar to denote a region of one to four
consecutive black stripes with white stripes on either side.  The term
white bar to denote a region of one to four consecutive white stripes
with black stripes on either side.  Barcode documentation sometimes
uses the term module in place of stripe.

EAN-13 Encoding

With 0 and 1 representing white and black stripes, the three tables
given below show the encoding of decimal digits in the barcode.  A
barcode starts with the left hand guard pattern 000000000101. This is
followed by a sequence of 6 left hand digits each occupying 7 stripes,
encoded using either table A or B. This is followed by centre guard
pattern 01010, followed by six right hand digits encoded using table
C. The barcode is terminated by the right hand guard 101000000.

      Table A                 Table B                 Table C
             x y   z                 x y   z                 x y   z
0 0001101    3 2   3    0 0100111    3 5   4    0 1110010    5 3   4
1 0011001    4 3   3    1 0110011    4 4   4    1 1100110    4 4   4
2 0010011    3 4   3    2 0011011    3 3   4    2 1101100    3 3   4
3 0111101    5 2   5    3 0100001    5 5   2    3 1000010    5 5   2
4 0100011    4 5   3    4 0011101    4 2   4    4 1011100    2 4   4
5 0110001    5 4   3    5 0111001    5 3   4    5 1001110    3 5   4
6 0101111    2 5   5    6 0000101    2 2   2    6 1010000    2 2   2
7 0111011    4 3   5    7 0010001    4 4   2    7 1000100    4 4   2
8 0110111    3 4   5    8 0001001    3 3   2    8 1001000    3 3   2
9 0001011    2 3   3    9 0010111    2 4   4    9 1110100    4 2   4

Note that every digit pattern consists of 7 stripes containing two
black bars and two white bars each with a width of between 1 and
4. Every pattern in tables A and B starts with a white stripe and ends
with a black one, and every pattern in table C starts with a block
stripe and ends with a white one.  x is the distance between the
starts of the two black bars and y is the distance between their
ends. z is the total number of black stripes in the pattern. These
three values are sufficient to decode the pattern.

For UPC-A (and not EAN-13) barcodes there is a check digit calculated
as follows. If the digits from left to right are d1, d2,..., d12, then
d12 is chosen to cause (3*(d1+d3+d5+d7+d9+d11)+(d2+d4+d6+d8+d10+d12))
to be an exact multiple of 10.

For EAN-13 barcodes each of the left hand six digits is encoded using
either table A or B. The selection of which tables are used encode a
13th decimal digit, as specified by the following table.

                13th digit code
                   0   AAAAAA
                   1   AABABB
                   2   AABBAB
                   3   AABBBA
                   4   ABAABB
                   5   ABBABB
                   6   ABBBAA
                   7   ABABAB
                   8   ABABBA
                   9   ABBABA

For UPC-A barcode, the left hand six digits always use table A.

GS1-128 Encoding

GS1-128 barcode represent a variable length string of ASCII
characters. These are encoded using patterns of 11 consecutive
stripes. These patterns correspond to numbers in the range 0 to 105
using the following table.

  0 11011001100    1 11001101100    2 11001100110    3 10010011000
  4 10010001100    5 10001001100    6 10011001000    7 10011000100
  8 10001100100    9 11001001000   10 11001000100   11 11000100100
 12 10110011100   13 10011011100   14 10011001110   15 10111001100
 16 10011101100   17 10011100110   18 11001110010   19 11001011100
 20 11001001110   21 11011100100   22 11001110100   23 11101101110
 24 11101001100   25 11100101100   26 11100100110   27 11101100100
 28 11100110100   29 11100110010   30 11011011000   31 11011000110
 32 11000110110   33 10100011000   34 10001011000   35 10001000110
 36 10110001000   37 10001101000   38 10001100010   39 11010001000
 40 11000101000   41 11000100010   42 10110111000   43 10110001110
 44 10001101110   45 10111011000   46 10111000110   47 10001110110
 48 11101110110   49 11010001110   50 11000101110   51 11011101000
 52 11011100010   53 11011101110   54 11101011000   55 11101000110
 56 11100010110   57 11101101000   58 11101100010   59 11100011010
 60 11101111010   61 11001000010   62 11110001010   63 10100110000
 64 10100001100   65 10010110000   66 10010000110   67 10000101100
 68 10000100110   69 10110010000   70 10110000100   71 10011010000
 72 10011000010   73 10000110100   74 10000110010   75 11000010010
 76 11001010000   77 11110111010   78 11000010100   79 10001111010
 80 10100111100   81 10010111100   82 10010011110   83 10111100100
 84 10011110100   85 10011110010   86 11110100100   87 11110010100
 88 11110010010   89 11011011110   90 11011110110   91 11110110110
 92 10101111000   93 10100011110   94 10001011110   95 10111101000
 96 10111100010   97 11110101000   98 11110100010   99 10111011110
100 10111101110  101 11110101110  102 11110101110  103 11010000100
104 11010010000  105 11010011100

These values are interpreted in of three code sets A, B and C. The
barcode start with a pattern for value 103, 104 or 105 which selects
respectively A, B or C as the initial code set to be used. The
interpretation of the values from 0 to 102 depend on the code set
currently selected, as follows.

Val  A   B   C   Val  A   B   C   Val  A   B   C   Val  A   B   C
  0 SP  SP  00     1  !   !  01     2  "   "  02     3  #   #  03
  4  $   $  04     5  %   %  05     6  &   &  06     7  '   '  07
  8  (   (  08     9  )   )  09    10  *   *  10    11  +   +  11
 12  ,   ,  12    13  -   -  13    14  .   .  14    15  /   /  15
 16  0   0  16    17  1   1  17    18  2   2  18    19  3   3  19
 20  4   4  20    21  5   5  21    22  6   6  22    23  7   7  23
 24  8   8  24    25  9   9  25    26  :   :  26    27  ;   ;  27
 28  <   <  28    29  =   =  29    30  >   >  30    31  ?   ?  31
 32  @   @  32    33  A   A  33    34  B   B  34    35  C   C  35
 36  D   D  36    37  E   E  37    38  F   F  38    39  G   G  39
 40  H   H  40    41  I   I  41    42  J   J  42    43  K   K  43
 44  L   L  44    45  M   M  45    46  N   N  46    47  O   O  47
 48  P   P  48    49  Q   Q  49    50  R   R  50    51  S   S  51
 52  T   T  52    53  U   U  53    54  V   V  54    55  W   W  55
 56  X   X  56    57  Y   Y  57    58  Z   Z  58    59  [   [  59
 60  \   \  60    61  ]   ]  61    62  ^   ^  62    63  _   _  63
 64 NUL  `  64    65 SOH  a  65    66 STX  b  66    67 ETX  c  67
 68 EOT  d  68    69 ENQ  e  69    70 ACK  f  70    71 BEL  g  71
 72  BS  h  72    73  HT  i  73    74  LF  j  74    75  VT  k  75
 76  FF  l  76    77  CR  m  77    78  SO  n  78    79  SI  o  79
 80 DLE  p  80    81 DC1  q  81    82 DC2  r  82    83 DC3  s  83
 84 DC4  t  84    85 NAK  u  85    86 SYN  v  86    87 ETB  w  87
 88 CAN  x  88    89  EM  y  89    90 SUB  z  90    91 ESC  {  91
 92  FS  |  92    93  GS  }  93    94  RS  ~  94    95  US DEL 95
 96 fn3 fn3 96    97 fn2 fn2 97    98 sft sft 98    99 cdC cdC 99
100 cdB fn4 cdB  101 fn4 cdA cdA  102 fn1 fn1 fn1

When code set C is selected each value corresponds to two decimal
digits except value 100 selects set B, 101 selects set A and 102
represents the special value FNC1. Code set A cannot encode lower case
letter but can generate the ASCII control characters. Code set B
cannot generate the control characters but can generate the lower case
letters.

When code set A is selected, value 98 selects set B for the next
character before reverting back to A. When code set B is selected,
value 98 selects set A for the next character before reverting back to
B.

Barcodes start with the pattern for value 103, 104 or 105 to select
code set A, B or C. This is followed by the pattern for 102
(FNC1). Then follows values representing ASCII characters possibly
shift or code selection characters. After the final ASCII character
there is a pattern check value followed by the stop pattern
1100011101011.

The check value is calculated as follows. Every value in the barcode
is given a weight which is it position numbered from left to right
starting at zero, except for the start character which is given a
weight of 1. Every value up to the check character is multiplied by
its weight and summed. The remainder after division by 103 of this sum
of products is the value of the check character.




History

20/01/2016
Started to add code to read GS1-128 barcodes.

11/01/2016
The program is being modified to read EAN-13 barcodes as well as UPC-A
barcodes.

04/01/2016
Initial implementation.

*/

GET "libhdr.h"

GLOBAL {
  stdin:ug
  stdout
  infilename
  outfilename
  instream
  outstream
  barcodetype // =1 for EAN-12   =2 for Gs1-128
  scanv       // Scan line gray scale values
  scanvp      // Pointer into scanv
  trv         // Vector of transitions
  trvp        // Current position
  trvq        // Used by decodeline

  ch          // Latest character from instream
  token
  numval
  tracing
  digits        // Decoded EAN-13 digits

  valv          // Decoded GS1-128 values
  valp

  readscanline
  findbarlengths
  decodeline
  lex
  totalwidth   // Used to estimate the stripe width
  stripes      // Used with toalwidth.
}

MANIFEST {
  scanvupb=3000 // Upper bound of scanv
  trvupb=200 // Upper bound of trv -- we only expect about 60 transitions
  s_eof=1
  s_dot
  s_num
}

LET start() = VALOF
{ LET stdin = input()
  LET stdout = output()
  LET argv = VEC 50
  LET tv = VEC trvupb
  LET dv = VEC 13/bytesperword // For the digits byte vector
                               // Room for string of length 13

  instream, outstream := 0, 0

  trv := tv
  trvp, trvq := 0, 0

  scanv := getvec(scanvupb)
  UNLESS scanv DO
  { writef("Error: More memory needed*n")
    RESULTIS 0
  }

  digits := dv

  UNLESS rdargs("from,to/k,ean/s,gs/s,-t/s", argv, 50) DO
  { writef("Bad arguments for bardecode*n")
    RESULTIS 0
  }

  outfilename := 0
  infilename := "barcodelines.txt"
  barcodetype := 1

  IF argv!0 DO infilename := argv!0    // from
  IF argv!1 DO outfilename := argv!1   // to/k
  IF argv!2 DO barcodetype := 1        // ean/s
  IF argv!3 DO barcodetype := 2        // gr/s
  tracing := argv!4                    // -t/s

  instream := findinput(infilename)
  UNLESS instream DO
  { writef("Trouble with file %s*n", infilename)
    RESULTIS 0
  }  

  IF outfilename DO
  { outstream := findoutput(outfilename)
    UNLESS outstream DO
    { writef("Trouble with file %s*n", outfilename)
      RESULTIS 0
    }
  }

  IF instream DO selectinput(instream)

//sawritef("Calling readscanline*n")

  token := s_dot

  WHILE readscanline() DO
  { IF token=s_eof BREAK

    // We start by smoothing the data
    smoothdata(scanv, scanvp)

    IF tracing DO
    { LET layout = 0
      writef("*nScan line*n")
      FOR i = 1 TO scanvp DO
      { IF layout MOD 10 = 0 DO writef("*n%i4: ", layout)
        layout := layout+1
        writef(" %i3", scanv!i)
      }
      newline()
    }

//abort(1000)

    IF findbarlengths() DO
    {
      IF tracing DO
      { LET layout = 0
        writef("*n%i2 Bar widths*n", trvp)
        FOR i = 1 TO trvp DO
        { IF layout MOD 10 = 0 DO newline()
          layout := layout+1
          writef(" %i6", trv!i)
        }
        newline()
      }

//abort(6100)

      IF barcodetype=1 & decode_ean() DO
      { writef("*n*nEAN-13 barcode successfully decoded: %s*n*n", digits)
        GOTO fin
      }

      IF barcodetype=2 & decode_gs() DO
      { writef("*n*nGS1-128 barcode successfully decoded: %s*n*n", digits)
        GOTO fin
      }

abort(6104)

      reversetransitions()

      IF tracing DO
      { LET layout = 0
        writef("*n%i2 Reverses bar widths*n", trvp)
        FOR i = 1 TO trvp DO
        { IF layout MOD 10 = 0 DO newline()
          layout := layout+1
          writef(" %i6", trv!i)
        }
        newline()
      }

      IF barcodetype=1 & decode_ean() DO
      { writef("*n*nEAN-13 barcode successfully decoded: %s*n*n", digits)
        GOTO fin
      }

      IF barcodetype=2 & decode_gs() DO
      { writef("*n*nGS1-128 barcode successfully decoded: %s*n*n", digits)
        GOTO fin
      }

abort(6105)

    }
  }

fin:
  IF outstream DO
  { endstream(outstream)
    selectoutput(stdout)
  }

  IF instream DO
  { endstream(instream)
    selectinput(stdin)
  }

  RESULTIS 0
}

AND readscanline() = VALOF
{ // This sets scanvp to the number of pixels in the scan line,
  // placing grey scale values in scanv!1 to scanv!scanvp.

  ch := rdch()
  scanvp := 0         // No data yet

  { lex()             // Get next lexical token
    SWITCHON token INTO
    { DEFAULT:
        writef("System error: token=%n*n", token)
        RESULTIS FALSE

      CASE s_eof: // End of scan line file
        RESULTIS TRUE

      CASE s_dot: // End of scan line
        RESULTIS TRUE

      CASE s_num: // A number
        scanvp := scanvp+1
        IF scanvp > scanvupb DO
        { writef("System error: Too much scan data*n")
          RESULTIS FALSE
        }
//writef("scanv=%n scanvp=%n numval=%n*n", scanv, scanvp, numbval)
        scanv!scanvp := numval
        LOOP
    }
  } REPEAT
}

AND lex() BE
{ LET neg = FALSE

//writef("ch = %i3 ", ch)
//IF ch>=32 DO writef(" '%c'", ch)
//newline()

  SWITCHON ch INTO
  { DEFAULT:
      writef("Bad ch in lex %c %n*n", ch, ch)
      ch := rdch()
      LOOP

    CASE endstreamch:
      token := s_eof
      RETURN

    CASE '#':       // A comment
      ch := rdch() REPEATUNTIL ch='*n' | ch=endstreamch
      LOOP

    CASE '*s':
    CASE '*n':
      ch := rdch()  // Ignore white space
      LOOP

    CASE '.':
      ch := rdch()
      token := s_dot
      RETURN

    CASE '-':
      neg := TRUE
      ch := rdch()

    CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
    CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
      numval := 0
      WHILE '0'<=ch<='9' DO
      { numval := 10*numval + ch - '0'
        ch := rdch()
      }
      IF neg DO numval := -numval
      token := s_num
      RETURN
  }
} REPEAT

AND smoothdata(v, upb) BE
{ // The data is in v!1 to v!upb
  // This function will replace
  // v!p by (v!(p-2)+4*v!(p-1)+6*v!p+4*v(p+1)+v!(p+2))/16
  // for all p between 3 and upb-2.
  LET a, b, c, d, e = ?, v!1, v!2, v!3, v!4
  FOR p = @v!3 TO @v!(upb-2) DO
  { a := b
    b := c
    c := d
    d := e
    e := p!2
    p!0 := (a+e + 4*(b+d) + 6*c) / 16
  }
}

AND findbarlengths() = VALOF
{ LET p = 1
  LET p1, p2 = 0, 0
  LET prevpt = 1
  LET a, b = scanv!p, 0
  LET k, max, min = 0, 0, 0

  trvp := 1
  trv!trvp := -1 // Dummy dark to light transition

  // Find the next light to dark transition

  WHILE p < scanvp-5 DO
  { //xx

    //writef("*nSearch for the next light to dark transition starting at %i4*n*n", p)

    WHILE p < scanvp-5 DO
    { // Find the next position where scanv!p drops by more
      // than 40 units in 5 steps.
      k, max, min := 0, 0, 0

      IF scanv!p < scanv!(p+5) + 40 DO
      { //writef("p=%i4/%i3, p+5=%i4/%i3 diff=%i3*n",
        //           p,scanv!p, p+5,scanv!(p+5), scanv!(p+5)-scanv!p)
        p := p+1
        LOOP
      }

//writef("p=%i4/%i3, p+5=%i4/%i3 diff=%i3*n",
//                   p,scanv!p, p+5,scanv!(p+5), scanv!(p+5)-scanv!p)

      // A drop of more than 40 has been found

//writef("*nA drop of more than 40 found at %n*n*n", p+5)

//writef("*nSearch backward for the high point at the start of the transition*n")

      // Search backwards to find the start of the possible transition
      p1 := p+5       // This is a lowish point
      max := scanv!p1
      k := 0

      FOR i = p1-1 TO 3 BY -1 DO // Step backwards
      { 
//writef("i=%i4/%i3  p1=%i4/%i3*n", i, scanv!i, p1, max)
        IF scanv!i > max DO { p1, max, k := i, scanv!i, 0; LOOP }
        k := k+1
        IF k>=3 BREAK // A suitable high point has been reached
      }

//writef("*nHigh point found at p1=%i4/%i3*n", p1, max)
//abort(1010)

      // max is the high value at position p1 at the start
      // of the transition



//writef("*nSearch forward for the low point at the end of the transition*n")

      // Search forwards to find the low point of the possible transition
      p2 := p+5       // This is a lowish point
      min := scanv!p2 // Starting value
      k := 0

      FOR i = p2+1 TO scanvp-2 DO // Search forwards
      { 
//writef("i=%i4/%i3  p2=%i4/%i3*n", i, scanv!i, p2, min)
        IF scanv!i < min DO { p2, min, k := i, scanv!i, 0; LOOP }
        k := k+1
        IF k>=3 BREAK // A suitable low point has been reached
      }

//writef("*nLow point found at p2=%i4/%i3*n", p2, min)
//abort(1030)

      // min is the low value at position p2 at the end
      // of the transition
      BREAK
    }

    IF p >= scanvp-5 BREAK // End of scan line so stop

    // Light to dark transition found between p1 and p2

//IF tracing DO
//  writef("Light to dark transition found between p1=%i4/%i3 and  p2=%i4/%i3*n",
//          p1,scanv!p1,  p2,scanv!p2)
//abort(1012)

    // Find the transition point

    { LET midval = (scanv!p1 + scanv!p2)/2
      LET pt = (p1+p2)/2
      LET diff = ABS(scanv!pt-midval)

      // Find the point that has a the value closest to midval
      FOR i = p1 TO p2 DO
      { LET newdiff = ABS(scanv!i - midval)  
        IF diff > newdiff DO pt, diff := i, newdiff

//IF tracing DO
//  writef("midval=%i3 diff=%i4 scanv!%i4=%i3 scanv!%i4=%i3 scanv!%i4=%i3 scanv!%i4=%i3*n",
//          midval,    diff,    i, scanv!i,   p1,scanv!p1,  pt,scanv!pt, p2,scanv!p2)
      }

      trvp := trvp+1
      trv!trvp := 10*pt // Store the position of the light to dark transition

IF tracing DO
{
  //writef("Light to dark transition point at pt=%i5/%i3   width %i3*n", pt, scanv!pt, pt-prevpt)
//abort(1301)
}
      prevpt := pt
    }
  

    IF p >= scanvp-5 BREAK // End of scan line so stop

  // Find the next dark to light transition

    p := p2

    //writef("*nSearch for the next dark to light transition starting at %i4*n*n", p)

    WHILE p < scanvp-5 DO
    { // Find the next position where scanv!p rises by more
      // than 40 units in 5 steps.
      k, max, min := 0, 0, 0

      IF scanv!p >= scanv!(p+5) - 40 DO
      { //writef("p=%i4/%i3, p+5=%i4/%i3 diff=%i3*n",
        //           p,scanv!p, p+5,scanv!(p+5), scanv!(p+5)-scanv!p)
        p := p+1
        LOOP
      }

//writef("p=%i4/%i3, p+5=%i4/%i3 diff=%i3*n",
//        p,scanv!p, p+5,scanv!(p+5), scanv!(p+5)-scanv!p)

      // A drop of more than 40 has been found

//writef("*nA rise of more than 40 found at %n*n*n", p+5)

//writef("*nSearch backward for the low point at the start of the transition*n")
//abort(1500)

      // Search backwards to find the start of the possible transition
      p1 := p+5       // This is a highish point
      min := scanv!p1
      k := 0

      FOR i = p1-1 TO 3 BY -1 DO // Search backwards
      { 
//writef("i=%i4/%i3  p1=%i4/%i3*n", i, scanv!i, p1, min)
        IF scanv!i < min DO { p1, min, k := i, scanv!i, 0; LOOP }
        k := k+1
        IF k>=3 BREAK // A suitable high point has been reached
      }

//writef("*nLow point found at p1=%i4/%i3*n", p1, min)
//abort(1010)

      // max is the high value at position p1 at the start
      // of the transition



//writef("*nSearch forward for the high point at the end of the transition*n")

      // Search forwards to find the high point at the end of the possible transition
      p2 := p+5       // This is a lowish point
      max := scanv!p2 // Starting value
      k := 0

      FOR i = p2+1 TO scanvp-2 DO // Step forwards
      { 
//writef("i=%i4/%i3  p1=%i4/%i3*n", i, scanv!i, p1, max)
//        IF scanv!i > max DO { p2, max, k := i, scanv!i, 0; LOOP }
        k := k+1
        IF k>=3 BREAK // A suitable high point has been reached
      }

//writef("*nHigh point found at p2=%i4/%i3*n", p2, min)
//abort(1030)

      // max is the high value at position p2 at the end
      // of the transition
      BREAK
    }

    // Dark to light transition found between p1 and p2

//IF tracing DO
//  writef("Dark to light transition found between p1=%i4/%i3 and  p2=%i4/%i3*n",
//          p1,scanv!p1,  p2,scanv!p2)
//abort(1012)

    // Find the transition point

    { LET midval = (scanv!p1 + scanv!p2)/2
      LET pt = (p1+p2)/2
      LET diff = ABS(scanv!pt-midval)

      // Find the point that has a the value closest to midval
      FOR i = p1 TO p2 DO
      { LET newdiff = ABS(scanv!i - midval)  
        IF diff > newdiff DO pt, diff := i, newdiff

//IF tracing DO
//  writef("midval=%i3 diff=%i4 scanv!%i4=%i3 scanv!%i4=%i3 scanv!%i4=%i3 scanv!%i4=%i3*n",
//          midval,    diff,    i, scanv!i,   p1,scanv!p1,  pt,scanv!pt, p2,scanv!p2)
      }

      trvp := trvp+1
      trv!trvp := -10*pt // Store the position of the light to dark transition

//IF tracing DO
//{
//  writef("Dark to light transition at pt=%i5/%i3   width %i3*n*n", -pt, scanv!pt, pt-prevpt)
//abort(1002)
//}
      prevpt := pt
      p := p2

    IF p >= scanvp-5 BREAK // End of scan line so stop

    }
    IF p >= scanvp-5 BREAK // End of scan line so stop
  }
  //IF tracing DO
  //  writef("findbarlengths: returning, scanvp=%i4 trvp=%n*n", scanvp, trvp)

ret:
  trvp := trvp+1
  trv!trvp := 10*scanvp // Dummy light to dark transition

  // Replace transition values with widths.
  // White bars have negative widths.
  // Black bars have positive widths.
  FOR i = 1 TO trvp-2 BY 2 DO
  { trv!i := -(trv!(i+1)+trv!i)
    trv!(i+1) := -(trv!(i+2)+trv!(i+1))
  }
  trvp := trvp-1
 
//abort(8000)

  //IF tracing DO
  //{ FOR i = 1 TO trvp DO
  //  { IF i MOD 10 = 1 DO newline()
  //    writef(" %i7", trv!i)
  //  }
  //  newline()
  //}

//abort(8001)

  // For debugging convert to nearest number of stripes,
  // assuming one stripe is 17 units wide.
  //setstripewidth(2, 3+4+4, 3+7+7)

//abort(8002)

//writef("Approximate bar widths in stripes*n")
//  FOR i = 1 TO trvp DO
//  { LET a = trv!i
    //LET b = (((ABS a)*10+85)/170 + 5) / 10
    //LET b = (((ABS a)*stripes*10) / totalwidth + 5) / 10
//    LET b = width2stripes(a)
//    trv!i := a<0 -> -b, b
//  }
  RESULTIS TRUE
}

AND width2stripes(w) =
  (((ABS w)*stripes*10) / totalwidth + 5) / 10

AND reversetransitions() BE
{ // Reverse the elements of trv.
  LET p, q = 1, trvp
  UNTIL p >= q DO
  { LET a, b = trv!p, trv!q
    trv!p, trv!q := b, a
    p, q := p+1, q-1
  }
}

AND digbits2dig(bits) = VALOF SWITCHON bits INTO
{ DEFAULT:       RESULTIS 0

  CASE #b000000: RESULTIS '0'
  CASE #b001011: RESULTIS '1'
  CASE #b001101: RESULTIS '2'
  CASE #b001110: RESULTIS '3'
  CASE #b010011: RESULTIS '4'
  CASE #b011001: RESULTIS '5'
  CASE #b011100: RESULTIS '6'
  CASE #b010101: RESULTIS '7'
  CASE #b010110: RESULTIS '8'
  CASE #b011010: RESULTIS '9'
}

AND setstripewidth(pos, n, s) BE
{ // This sets totalwidth and stripes specifying
  // a stripe width of totalwidth/stripes.
  // If an error is detected, totalwidth is set to zero.
  // pos is the position in trv of the first
  // white or black bar to consider,
  // n is the number of bars to consider, and
  // s if the number of stripes that these bars occupy.

//writef("setstripewidth: entered*n")

  totalwidth, stripes := 0, s

  IF pos+n > trvp RETURN  // Return with totalwidth=0

  // Within trv
  // white bars have negative widths and
  // black bars have positive widths.
  FOR i = 0 TO n-1 DO totalwidth := totalwidth + ABS(trv!(pos+i))

  IF tracing DO
    writef("pos=%i2 n=%n totalwidth=%n, stripes=%n, stripe width = %5.2d*n",
           pos, n, totalwidth, stripes, (totalwidth*100)/stripes)
  // Successful return
}

AND decode_ean() = VALOF
{ // Return TRUE if successful
  // The transitions are in trv!1 to trv!trvp
  LET dig1bits = 0

//writef("decodeline: entered*n")

  trvq := 1

  // Search for the left guard 0000001010

  { IF trvq+3+7+7 > trvp DO
    { IF tracing DO
        writef("No left guard found in this scan line*n")

      RESULTIS FALSE // No left guard found
    }
    setstripewidth(trvq+1, 3+4+4, 3+7+7)
    IF rdleftguard() BREAK
    trvq := trvq+1
  } REPEAT

  // trvq now points to the white stripe at the end of the guard
  // which should be the first stripe of the first digit.

  IF tracing DO
  { writef("Left guard found, trvq now=%n*n", trvq)
    abort(5000)
  }
  digits%1 := '?'

  // Read the left digits
  FOR i = 2 TO 7 DO
  { LET rc = ?
    LET n = 4+4+4
    LET s = 7+7+7
    LET p = trvq-4 // Normally 4 bars earlier
    IF i=2 DO p, n, s := trvq-3, 3+4+4, 3+7+7
    IF i=7 DO n, s := 4+4+5, 7+7+5

//writef("*nCalling rdleftdigit i=%n  trvq=%n*n", i, trvq)

    setstripewidth(p, n, s)
    rc := rdleftdigit()
    IF rc<0 RESULTIS FALSE
    dig1bits := dig1bits<<1
    IF rc>0 DO dig1bits := dig1bits | 1
    digits%i := result2
    digits%0 := i
    IF tracing DO
    { writef("digits: %i2  %s  bits=%b6*n", i, digits, dig1bits)
      abort(6004)
    }
  }
  digits%1 := digbits2dig(dig1bits)
  IF tracing DO
  { writef("digits after decoding digit1: %i2  %s*n", 7, digits)
    abort(5006)
  }

  // Read centre guard 010101
  setstripewidth(trvq+-4, 4+5+4, 7+5+7)
  UNLESS rdcentreguard() RESULTIS FALSE
  // trvq now points to the black stripe just after the guard
  IF tracing DO
  { writef("Centre guard found at trvq=%n, trvq now=%n*n", trvq, trvq+4)
    abort(5006)
  }

  FOR i = 8 TO 13 DO
  { LET rc = ?
    LET n = 4+4+4
    LET s = 7+7+7
    LET p = trvq-4 // Normally 4 bars earlier
    IF i=8 DO p, n, s := trvq-5, 5+4+4, 5+7+7
    IF i=13 DO n, s := 4+4+3, 7+7+3

//writef("*nCalling rdrightdigit i=%n  trvq=%n*n", i, trvq)

    setstripewidth(p, n, s)
    UNLESS rdrightdigit() RESULTIS FALSE
    digits%i := result2
    digits%0 := i
    IF tracing DO
    { writef("digits: %i2  %s*n", i, digits)
      abort(6004)
    }
  }

  // Read end guard 1010000
  IF tracing DO writef("*nCalling rdrightguard at %i4*n", trvq)
  setstripewidth(trvq-8, 4+4+3, 7+7+3)
  UNLESS rdrightguard() RESULTIS FALSE
  IF tracing DO writef("Right guard found, trvq=%n*n", trvq)

  RESULTIS TRUE
}

AND rdleftguard() = VALOF
{ // Pattern 00000010101
  // Leave trvq pointing to the white stripe at the end of the guard.
  // This is the first stripe of the first left digit.

  // On entry trvq points to a white bar representing the
  // quiet region before the 3 strip guard.

  IF trvq + (3+7+7) < trvp DO
  { // There is room in trv for the guard and two digits
    LET a = - trv!trvq      // Should be quiet zone
    LET b =   trv!(trvq+1)  // Should be a black bar
    LET c = - trv!(trvq+2)  // Should be a white bar
    LET d =   trv!(trvq+3)  // Should be a black bar

    // a,b,c and d should all be positive widths.

    IF tracing DO
    { writef("a to d = %i5 %i5 %i5 %i5*n", a, b, c, d)
      //abort(7000)
    }

    IF a>=0 DO
    { // Scale to nearest multiple of stripes.
      LET w0 = width2stripes(a)
      LET w1 = width2stripes(b)
      LET w2 = width2stripes(c)
      LET w3 = width2stripes(d)

      IF tracing DO
      { writef("w0 to w3 = %i5 %i5 %i5 %i5*n", w0, w1, w2, w3)
        //abort(7001)
      }

      IF w0>=6 & w1=1 & w2=1 & w3=1 DO
      { // Left guard found
        //IF tracing DO
        //{ writef("Left guard from %n to %n*n*n", trvq, trvq+4)
        //}
        trvq := trvq+4   // Position of white bar after the guard
        RESULTIS TRUE
      }
    }
  }

  IF tracing DO
  { writef("No left guard found at trvq=%n*n*n", trvq)
    abort(1002)
  }
  // No left guard found
  RESULTIS FALSE
}

AND rdcentreguard() = VALOF
{ // Centre guard pattern 01010
  // Leave trvq pinting to the black stripe at the end of the guard.
  // This is the first stripe of the first right digit.

  IF trvq <= trvp-4
  { LET a = - trv!trvq      // Should be a white bar
    LET b =   trv!(trvq+1)  // Should be a black bar
    LET c = - trv!(trvq+2)  // Should be a white bar
    LET d =   trv!(trvq+3)  // Should be a black bar
    LET e = - trv!(trvq+4)  // Should be a white bar

    IF tracing DO
    { writef("rdcentreguard: a to e = %i5 %i5 %i5 %i5 %i5*n", a, b, c, d, e)
    }

    IF a>=0 DO
    { LET w0 = width2stripes(a)
      LET w1 = width2stripes(b)
      LET w2 = width2stripes(c)
      LET w3 = width2stripes(d)
      LET w4 = width2stripes(e)

      IF tracing DO
      { writef("w0 to w4 = %i5 %i5 %i5 %i5 %i5*n", w0, w1, w2, w3, w4)
      }

      IF w0=1 & w1=1 & w2=1 & w3=1 & w4=1 DO
      { // Centre guard found
        //IF tracing DO
        //{ writef("Centre guard found at trvq=%n, trvq now=%n*n", trvq, trvq+5)
        //}
        trvq := trvq+5   // Position of the dark bar after the guard
        RESULTIS TRUE
      }
    }
  }

  // Centre guard not found
    IF tracing DO
    { writef("Centre guard not found at trvq=%n*n", trvq)
      abort(6004)
    }
  RESULTIS FALSE
}

AND rdrightguard() = VALOF
{ // Right guard pattern 101000000

  IF trvq <= trvp-3
  { LET a =   trv!trvq      // Should be a black stripe
    LET b = - trv!(trvq+1)  // Should be a white stripe
    LET c =   trv!(trvq+2)  // Should be a black stripe
    LET d = - trv!(trvq+3)  // Should be a white bar

    IF tracing DO
    { writef("rdrightguard: a to e = %i5 %i5 %i5 %i5*n", a, b, c, d)
    }

    IF a>0 DO
    { LET w0 = width2stripes(a)
      LET w1 = width2stripes(b)
      LET w2 = width2stripes(c)
      LET w3 = width2stripes(d)

      IF tracing DO
      { writef("w0 to w3 = %i5 %i5 %i5 %i5*n", w0, w1, w2, w3)
      }

      IF w0=1 & w1=1 & w2=1 & w3>6 DO
      { // Right guard found
        IF tracing DO
        { writef("Right guard found at trvq=%n, trvq now=%n*n", trvq, trvq+4)
        }
        trvq := trvq+4   // Position of the dark bar after the guard
        RESULTIS TRUE
      }
    }
  }

  // No left guard found
  IF tracing DO
  { writef("Right guard not found at trvq=%n*n", trvq)
  }
  RESULTIS FALSE
}

AND rdleftdigit() = VALOF
{ // trvq points to the first stripe which should be white of
  // the first digit. Digits occupy 7 stripes consisting of
  // two black regions.

  // Leave trvq pointing to the stripe after the last stripe
  // of the digit. This should be the first stripe of the next
  // left digit or the first stripe of the centre guard.

  // The decoding of the left digits is based on the following tables.

  //       Table A                    Table B
  //              x y   z                    x y   z
  // 0 0001101    3 2   3       0 0100111    3 5   4
  // 1 0011001    4 3   3       1 0110011    4 4   4
  // 2 0010011    3 4   3       2 0011011    3 3   4
  // 3 0111101    5 2   5       3 0100001    5 5   2
  // 4 0100011    4 5   3       4 0011101    4 2   4
  // 5 0110001    5 4   3       5 0111001    5 3   4
  // 6 0101111    2 5   5       6 0000101    2 2   2
  // 7 0111011    4 3   5       7 0010001    4 4   2
  // 8 0110111    3 4   5       8 0001001    3 3   2
  // 9 0001011    2 3   3       9 0010111    2 4   4

  LET dig = 0

//writef("rdleftdigit: trvq=%n  trvp=%n*n", trvq, trvp)

//abort(5001)
  IF totalwidth DO
  { LET a = - trv!trvq      // The 4 bars of a digit
    LET b =   trv!(trvq+1)
    LET c = - trv!(trvq+2)
    LET d =   trv!(trvq+3)

    IF tracing DO
    { writef("trvq=%i2 a=%i3 b=%i3 c=%i3 d=%i3*n", trvq,a,b,c,d)
      //abort(5002)
    }

    // Estimate the stripe half width based on two dark regions
    // assumed to be of unit width.

    IF a > 0 DO
    { 

      //                  a      b       c     d
      //               |     left digit stripes    |
      //                    |<--b1-->|      |<-b2->|
      //            ###......#########.......#######..
      //                    |<--e2---|----->|      |
      //                             |<-----e1---->|

      // x, y and z scaled 10 units per stripe
      LET x = b+c        // Distance between black starts
      LET y = c+d        // Distance between black end
      LET z = b+d        // = b1 + b2

      LET res = -1

      IF tracing DO
      { writef("x=%n y=%n  z=%n  ie %5.2d %5.2d %5.2d*n",
               x, y, z,
               width2stripes(100*x), width2stripes(100*y), width2stripes(100*z))
      }

      // Round to the nearest integer
      x := width2stripes(x)
      y := width2stripes(y)
      z := width2stripes(z*100)

      IF tracing DO
        writef("x=%n y=%n z=%5.2d*n", x, y, z)
//abort(5003)

      res := -1     // =-1 failure    =0 table A    =1 table B
      dig := '?'

      SWITCHON x<<4 | y INTO
      { DEFAULT:                                        ENDCASE

        // Table A digits

        CASE #x32: dig := '0';                 res := 0; ENDCASE
        CASE #x43: dig := z < 400 -> '1', '7'; res := 0; ENDCASE
        CASE #x34: dig := z < 400 -> '2', '8'; res := 0; ENDCASE
        CASE #x52: dig := '3';                 res := 0; ENDCASE
        CASE #x45: dig := '4';                 res := 0; ENDCASE
        CASE #x54: dig := '5';                 res := 0; ENDCASE
        CASE #x25: dig := '6';                 res := 0; ENDCASE
        CASE #x23: dig := '9';                 res := 0; ENDCASE

        // Table B digits

        CASE #x35: dig := '0';                 res := 1; ENDCASE
        CASE #x44: dig := z > 300 -> '1', '7'; res := 1; ENDCASE
        CASE #x33: dig := z > 300 -> '2', '8'; res := 1; ENDCASE
        CASE #x55: dig := '3';                 res := 1; ENDCASE
        CASE #x42: dig := '4';                 res := 1; ENDCASE
        CASE #x53: dig := '5';                 res := 1; ENDCASE
        CASE #x22: dig := '6';                 res := 1; ENDCASE
        CASE #x24: dig := '9';                 res := 1; ENDCASE
      }

      // A left digit has been found
      IF tracing DO
      { writef("Found left digit %c at trvq=%n  res = %n, trvq now = %n*n",
                dig, trvq, res, trvq+4)
      }
      trvq := trvq+4 // 4 white or black bars
      result2 := dig
//abort(5004)
      RESULTIS res  // =-1 failure  =0 for table A    =1 for table B
    }
  }

  IF tracing DO
  { writef("Left digit not found*n")
    abort(1003)
  }
  // Left digit not found
  RESULTIS FALSE
}

AND rdrightdigit() = VALOF
{ // trvq points to the first stripe which should be black of
  // the first digit. Digits occupy 7 stripes consisting of
  // two black regions.

  // Leave trvq pointing to the stripe after the last stripe
  // of the digit. This should be the first stripe of the next
  // right digit or the first stripe of the right guard.

  // The decoding of the right digits is based on the following table.

//         Table C
//                x y   z
//   0 1110010    5 3   4
//   1 1100110    4 4   4
//   2 1101100    3 3   4
//   3 1000010    5 5   2
//   4 1011100    2 4   4
//   5 1001110    3 5   4
//   6 1010000    2 2   2
//   7 1000100    4 4   2
//   8 1001000    3 3   2
//   9 1110100    4 2   4

  LET dig = 0

  IF tracing DO
  { writef("rdrightdigit: trvq=%n  trvp=%n*n", trvq, trvp)
    //abort(5001)
  }

  IF totalwidth DO
  { LET a =   trv!trvq      // The 4 bars of a digit
    LET b = - trv!(trvq+1)
    LET c =   trv!(trvq+2)
    LET d = - trv!(trvq+3)

    IF tracing DO
    { writef("trvq=%i2 a=%i3 b=%i3 c=%i3 d=%i3*n", trvq,a,b,c,d)
      //abort(5002)
    }

    // Estimate the stripe half width based on two dark regions
    // assumed to be of unit width.

    IF a > 0 DO
    { 

      //                    a      b      c      d
      //               |     right digit stripes    |
      //               |<--b1-->|      |<-b2->|
      //            ...#########.......#######......
      //               |<--e2---|----->|      |
      //                        |<-----e1---->|

      // x, y and z scaled 10 units per stripe
      LET x = a+b        // Distance between black starts
      LET y = b+c        // Distance between black end
      LET z = a+c        // = b1 + b2

      IF tracing DO
      { writef("x=%n y=%n  z=%n  ie %5.2d %5.2d %5.2d*n",
               x, y, z,
               width2stripes(100*x), width2stripes(100*y), width2stripes(100*z))
      }

      // Round to the nearest integer
      x := width2stripes(x)
      y := width2stripes(y)
      z := width2stripes(z*100)

      IF tracing DO
        writef("x=%n y=%n z=%5.2d*n", x, y, z)

      dig := '?'

      SWITCHON x<<4 | y INTO
      { DEFAULT:                             ENDCASE

        // Table C digits

//   0 1110010    5 3   4
//   1 1100110    4 4   4
//   2 1101100    3 3   4
//   3 1000010    5 5   2
//   4 1011100    2 4   4
//   5 1001110    3 5   4
//   6 1010000    2 2   2
//   7 1000100    4 4   2
//   8 1001000    3 3   2
//   9 1110100    4 2   4
        CASE #x53: dig := '0';                 ENDCASE
        CASE #x44: dig := z > 300 -> '1', '7'; ENDCASE
        CASE #x33: dig := z > 300 -> '2', '8'; ENDCASE
        CASE #x55: dig := '3';                 ENDCASE
        CASE #x24: dig := '4';                 ENDCASE
        CASE #x35: dig := '5';                 ENDCASE
        CASE #x22: dig := '6';                 ENDCASE
        CASE #x42: dig := '9';                 ENDCASE
      }

      IF dig='?' DO
      { // Right digit not found
        IF tracing DO
        { writef("Right digit not found*n")
          abort(5003)
        }
        RESULTIS FALSE
      }

      // A right digit has been found
      IF tracing DO
      { writef("Found right digit %c at trvq=%n  trvq now = %n*n",
                dig, trvq, trvq+4)
      }
      trvq := trvq+4 // 2 black and 2 white bars
      result2 := dig
//abort(5004)
      RESULTIS TRUE  // =TRUE is decode successful
    }
  }
}

AND decode_gs() = VALOF
{ // Return TRUE if successful
  // The transitions are in trv!1 to trv!trvp
  //writef("decode_gs: entered*n")

  LET val = 0
  LET v = VEC 100
  valv := v

writef("decode_gs: entered*n")

  FOR i = 1 TO trvp-7-6 DO
  { // Try to decode the barcode starting at position i.
    trvq := i
    valp := 0 // No values found yet.

    // Search for the start value

    setstripewidth(trvq+1, 6+6, 11+11)
    // Test that the quiet zone is wide enought
    IF width2stripes(trv!trvq)<=6 LOOP

    // The quiet zone was wide enough.
writef("trvq=%i3 trv!trvq=%i3*n", trvq, trv!trvq)

    trvq := trvq+1 // Step over the quient zone

writef("trvq=%i3 trv!trvq=%i3  expecting start code*n", trvq, trv!trvq)
    val := rdvalue()
writef("val=%i4 trvq=%i3*n", val, trvq)
    IF tracing DO
    { writef("val=%n*n", val)
      abort(1001)
    }
    UNLESS 103 <= val <= 105 LOOP
    // A start value has been found
    valp := valp + 1
    valv!valp := val  // Store the start value

    // Start value found

    IF tracing DO
    { writef("Start value %n found, trvq now=%n*n", valv!valp, trvq)
      abort(5000)
    }

    // Look for possible FNC1
    setstripewidth(trvq, 6+6, 11+11)
writef("trvq=%i3 trv!trvq=%i3*n", trvq, trv!trvq)
    val := rdvalue()
writef("val=%i4*n", val)
    //UNLESS val=102 RESULTIS FALSE // FNC1 not found
    valp := valp+1
    valv!valp := val  // Store the FNC1 value

    IF val=102 DO
    { // FNC1 found
      IF tracing DO
      { writef("FNC1 found, trvq now=%n*n", trvq)
        abort(5000)
      }
    }

    // Now try to read all the values until stop is reached.
    WHILE trvq+7 <= trvp DO
    { // Read values
      setstripewidth(trvq-6, 6+6, 11+11) // Previous and current values
writef("trvq=%i3 trv!trvq=%i3*n", trvq, trv!trvq)
      val := rdvalue()
writef("val=%i4*n", val)
      IF 0 <= val <= 101 DO
      { valp := valp+1
        valv!valp := val
        LOOP
      }
      IF val=106 DO
      { // Probable stop value
        LET w1 = width2stripes((trv!(trvq))*100)
        LET w2 = width2stripes(trv!(trvq+1))  // Quiet zone
        IF 0_50 <= w1 <= 3_00 & w2>6 RESULTIS TRUE
      }
      BREAK // No value found
    }

    //Try the next value for i.
  }

  // No barcode found in this scan line.
  RESULTIS FALSE
}

AND near8(v) = 6_25 <= v <= 9_75 -> TRUE, FALSE
AND near6(v) = 4_25 <= v <= 7_75 -> TRUE, FALSE
AND near4(v) = 2_25 <= v <= 5_75 -> TRUE, FALSE

AND rdvalue1() = VALOF
{ LET res = rdvalue1()
writef("res=%n*n", res)
  RESULTIS res
}

AND rdvalue() = VALOF
{ // Read the next 11 stripe value or 13 stripe stop code
  // trvq points to th the first bar of the values.


  LET w1 =   trv!(trvq)    // A black bar
  AND w2 = - trv!(trvq+1)  // A white bar
  AND w3 =   trv!(trvq+2)  // A black bar
  AND w4 = - trv!(trvq+3)  // A white bar
  AND w5 =   trv!(trvq+4)  // A black bar

  IF tracing DO
  { writef("rdvalue: w1=%n w2=%n w3=%n w4=%n w5=%n*n", w1,w2,w3,w4,w5)
    //abort(2001)
  }

  IF w1>0 DO // Check that the first bar is black
  { LET e1 = width2stripes(w1+w2)
    LET e2 = width2stripes(w2+w3)
    LET e3 = width2stripes(w3+w4)
    LET e4 = width2stripes(w4+w5)
    LET v  = width2stripes((w1+w3+w5)*100) // Total width of black bars

    trvq := trvq+6 // Point to first stripe of next value

    IF tracing DO
    { writef("rdvalue: e1=%n e2=%n e3=%n e4=%n v=%5.2d*n", e1,e2,e3,e4,v)
      //abort(2002)
    }

    IF tracing DO
    { writef("rdvalue: SWITCHON %o6*n", e1<<9 | e2<<6 | e3<<3 | e4)
      //abort(2002)
    }

    SWITCHON e1<<9 | e2<<6 | e3<<3 | e4 INTO
    { DEFAULT:                RESULTIS -1   // No value found

      CASE #3344: IF near6(v) RESULTIS   0; ENDCASE // 11011001100
      CASE #4433: IF near6(v) RESULTIS   1; ENDCASE // 11001101100
      CASE #4444: IF near6(v) RESULTIS   2; ENDCASE // 11001100110
      CASE #3334: IF near4(v) RESULTIS   3; ENDCASE // 10010011000
      CASE #3345: IF near4(v) RESULTIS   4; ENDCASE // 10010001100
      CASE #4434: IF near4(v) RESULTIS   5; ENDCASE // 10001001100
      CASE #3443: IF near4(v) RESULTIS   6; ENDCASE // 10011001000
      CASE #3454: IF near4(v) RESULTIS   7; ENDCASE // 10011000100
      CASE #4543: IF near4(v) RESULTIS   8; ENDCASE // 10001100100
      CASE #4333: IF near4(v) RESULTIS   9; ENDCASE // 11001001000
      CASE #4344: IF near4(v) RESULTIS  10; ENDCASE // 11001000100
      CASE #5433: IF near4(v) RESULTIS  11; ENDCASE // 11000100100
      CASE #2345: IF near6(v) RESULTIS  12; ENDCASE // 10110011100
      CASE #3434: IF near6(v) RESULTIS  13; ENDCASE // 10011011100
      CASE #3445: IF near6(v) RESULTIS  14; ENDCASE // 10011001110
      CASE #2454: IF near6(v) RESULTIS  15; ENDCASE // 10111001100
      CASE #3543: IF near6(v) RESULTIS  16; ENDCASE // 10011101100
      CASE #3554: IF near6(v) RESULTIS  17; ENDCASE // 10011100110
      CASE #4553: IF near6(v) RESULTIS  18; ENDCASE // 11001110010
      CASE #4324: IF near6(v) RESULTIS  19; ENDCASE // 11001011100
      CASE #4335: IF near6(v) RESULTIS  20; ENDCASE // 11001001110
      CASE #3453: IF near6(v) RESULTIS  21; ENDCASE // 11011100100
      CASE #4542: IF near6(v) RESULTIS  22; ENDCASE // 11001110100
      CASE #4334: IF near8(v) RESULTIS  23; ENDCASE // 11101101110
      CASE #4234: IF near6(v) RESULTIS  24; ENDCASE // 11101001100
      CASE #5323: IF near6(v) RESULTIS  25; ENDCASE // 11100101100
      CASE #5334: IF near6(v) RESULTIS  26; ENDCASE // 11100100110
      CASE #4343: IF near6(v) RESULTIS  27; ENDCASE // 11101100100
      CASE #5432: IF near6(v) RESULTIS  28; ENDCASE // 11100110100
      CASE #5443: IF near6(v) RESULTIS  29; ENDCASE // 11100110010
      CASE #3333: IF near6(v) RESULTIS  30; ENDCASE // 11011011000
      CASE #3355: IF near6(v) RESULTIS  31; ENDCASE // 11011000110
      CASE #5533: IF near6(v) RESULTIS  32; ENDCASE // 11000110110
      CASE #2245: IF near4(v) RESULTIS  33; ENDCASE // 10100011000
      CASE #4423: IF near4(v) RESULTIS  34; ENDCASE // 10001011000
      CASE #4445: IF near4(v) RESULTIS  35; ENDCASE // 10001000110
      CASE #2354: IF near4(v) RESULTIS  36; ENDCASE // 10110001000
      CASE #4532: IF near4(v) RESULTIS  37; ENDCASE // 10001101000
      CASE #4554: IF near4(v) RESULTIS  38; ENDCASE // 10001100010
      CASE #3244: IF near4(v) RESULTIS  39; ENDCASE // 11010001000
      CASE #5422: IF near4(v) RESULTIS  40; ENDCASE // 11000101000
      CASE #5444: IF near4(v) RESULTIS  41; ENDCASE // 11000100010
      CASE #2334: IF near6(v) RESULTIS  42; ENDCASE // 10110111000
      CASE #2356: IF near6(v) RESULTIS  43; ENDCASE // 10110001110
      CASE #4534: IF near6(v) RESULTIS  44; ENDCASE // 10001101110
      CASE #2443: IF near6(v) RESULTIS  45; ENDCASE // 10111011000
      CASE #2465: IF near6(v) RESULTIS  46; ENDCASE // 10111000110
      CASE #4643: IF near6(v) RESULTIS  47; ENDCASE // 10001110110
      CASE #4443: IF near8(v) RESULTIS  48; ENDCASE // 11101110110
      CASE #3246: IF near6(v) RESULTIS  49; ENDCASE // 11010001110
      CASE #5424: IF near6(v) RESULTIS  50; ENDCASE // 11000101110
      CASE #3442: IF near6(v) RESULTIS  51; ENDCASE // 11011101000
      CASE #3464: IF near6(v) RESULTIS  52; ENDCASE // 11011100010
      CASE #3444: IF near8(v) RESULTIS  53; ENDCASE // 11011101110
      CASE #4223: IF near6(v) RESULTIS  54; ENDCASE // 11101011000
      CASE #4245: IF near6(v) RESULTIS  55; ENDCASE // 11101000110
      CASE #6423: IF near6(v) RESULTIS  56; ENDCASE // 11100010110
      CASE #4332: IF near6(v) RESULTIS  57; ENDCASE // 11101101000
      CASE #4354: IF near6(v) RESULTIS  58; ENDCASE // 11101100010
      CASE #5532: IF near6(v) RESULTIS  59; ENDCASE // 11100011010
      CASE #4552: IF near8(v) RESULTIS  60; ENDCASE // 11101111010
      CASE #4355: IF near4(v) RESULTIS  61; ENDCASE // 11001000010
      CASE #7422: IF near6(v) RESULTIS  62; ENDCASE // 11110001010
      CASE #2234: IF near4(v) RESULTIS  63; ENDCASE // 10100110000
      CASE #2256: IF near4(v) RESULTIS  64; ENDCASE // 10100001100
      CASE #3323: IF near4(v) RESULTIS  65; ENDCASE // 10010110000
      CASE #3356: IF near4(v) RESULTIS  66; ENDCASE // 10010000110
      CASE #5523: IF near4(v) RESULTIS  67; ENDCASE // 10000101100
      CASE #5534: IF near4(v) RESULTIS  68; ENDCASE // 10000100110
      CASE #2343: IF near4(v) RESULTIS  69; ENDCASE // 10110010000
      CASE #2365: IF near4(v) RESULTIS  70; ENDCASE // 10110000100
      CASE #3432: IF near4(v) RESULTIS  71; ENDCASE // 10011010000
      CASE #3465: IF near4(v) RESULTIS  72; ENDCASE // 10011000010
      CASE #5632: IF near4(v) RESULTIS  73; ENDCASE // 10000110100
      CASE #5643: IF near4(v) RESULTIS  74; ENDCASE // 10000110010
      CASE #6533: IF near4(v) RESULTIS  75; ENDCASE // 11001010000
      CASE #5442: IF near8(v) RESULTIS  77; ENDCASE // 11110111010
      CASE #6522: IF near4(v) RESULTIS  78; ENDCASE // 11000010100
      CASE #4752: IF near6(v) RESULTIS  79; ENDCASE // 10001111010
      CASE #2236: IF near6(v) RESULTIS  80; ENDCASE // 10100111100
      CASE #3325: IF near6(v) RESULTIS  81; ENDCASE // 10010111100
      CASE #3336: IF near6(v) RESULTIS  82; ENDCASE // 10010011110
      CASE #2563: IF near6(v) RESULTIS  83; ENDCASE // 10111100100
      CASE #3653: IF near6(v) RESULTIS  84; ENDCASE // 10011110100
      CASE #3663: IF near6(v) RESULTIS  85; ENDCASE // 10011110010
      CASE #5233: IF near6(v) RESULTIS  86; ENDCASE // 11110100100
      CASE #6322: IF near6(v) RESULTIS  87; ENDCASE // 11110010100
      CASE #6333: IF near6(v) RESULTIS  88; ENDCASE // 11110010010
      CASE #3335: IF near8(v) RESULTIS  89; ENDCASE // 11011011110
      CASE #3553: IF near8(v) RESULTIS  90; ENDCASE // 11011110110
      CASE #5333: IF near8(v) RESULTIS  91; ENDCASE // 11110110110
      CASE #2225: IF near6(v) RESULTIS  92; ENDCASE // 10101111000
      CASE #2247: IF near6(v) RESULTIS  93; ENDCASE // 10100011110
      CASE #4425: IF near6(v) RESULTIS  94; ENDCASE // 10001011110
      CASE #2552: IF near6(v) RESULTIS  95; ENDCASE // 10111101000
      CASE #2574: IF near6(v) RESULTIS  96; ENDCASE // 10111100010
      CASE #5222: IF near6(v) RESULTIS  97; ENDCASE // 11110101000
      CASE #5244: IF near6(v) RESULTIS  98; ENDCASE // 11110100010
      CASE #2445: IF near8(v) RESULTIS  99; ENDCASE // 10111011110
      CASE #2554: IF near8(v) RESULTIS 100; ENDCASE // 10111101110
      CASE #4225: IF near8(v) RESULTIS 101; ENDCASE // 11110101110
      CASE #5224: IF near8(v) RESULTIS 102; ENDCASE // 11110101110
      CASE #3255: IF near4(v) RESULTIS 103; ENDCASE // 11010000100
      CASE #3233: 
//writef("rdvalue: CASE #3233: reached v=%5.2d*n", v)
IF near4(v) RESULTIS 104; ENDCASE // 11010010000
      CASE #3235: IF near6(v) RESULTIS 105; ENDCASE // 11010011100

      CASE #5642: UNLESS near6(v) ENDCASE// 1100011101011   stop code
                  // Check the stop code is followed by 2 black stripes
                  // and a quiet zone of more than 6 stripes.
                  UNLESS width2stripes(trv!trvq) ENDCASE
                  UNLESS width2stripes(trv!(trvq+1))>6 ENDCASE
                  RESULTIS 106
    }
  }

  // Value not found
  RESULTIS -1
}
