/*
This contains various functions to help process the Reed-Soloman error
correcting codes used in QR 2D barcodes.

Implemented in BCPL by Martin Richards (c) February 2016 based on
Reed-Solomon codes for coders from Wikiversity and strongly influenced
by the BBC White Paper WHP 031 entitled: Reed-Solomon error correction
by C.K.P. Clarke.

*/

GET "libhdr"

MANIFEST {
  // Parameters for the GF arithmetic, either GF(2^8) or GF(2^4)

  // Parameter for GF(2^8)

  GFvalmax=255   // GF(2^8) elements are in the range 0 to 255
  GFlog2upb=255  
  //GFexp2upb=509  // 509 = 254+255
  GFexp2upb=510  // 510 = 255+255

  GFpoly = #b_1_0001_1101
  GFtbit = #b_1_0000_0000  // =GFvakmax+1



  // Parameter for GF(2^4)    --- Comment out if using GF(2^8)
/*
  GFvalmax=15   // GF(2^4) elements are in the range 0 to 15
  GFlog2upb=15 
  GFexp2upb=30  // 30 = 15+15

  GFpoly = #b_1_0011
  GFtbit = #b_1_0000  // =GFvakmax+1
*/
}

GLOBAL {
  gf_log2:ug
  gf_exp2
}

LET start() = VALOF
{ newline()
  gf_log2 := 0
  gf_exp2 := 0
  initlogs()

  // Test hamming_dist
  //test_hamming_dist()

  // Test qr-check_format
  //test_qr_check_format()

  // Test encode_format
  //test_encode_format(#b00011<<10)

  // Test qr_decode_format
  //test_qr_decode_format()

  // Test gf_mul and gf_div
  //test_mul_div()

  // Test test_gf_poly_eval
  //test_gf_poly_eval()

  // Test gf_add
  //test_gf_poly_add()

  // Test gf_generate_poly
  //test_gf_generator_poly()

  // Test gf_poly_divmod
  //test_gf_poly_divmod()

  // Test rs_encode_msg
  //test_rs_encode_msg()

  // Test rs_calc_syndromes
  test_rs_calc_syndromes()

  // Test find_errata_locator
  test_find_errata_locator()

  // Test rs_find_error_locator()
  //test_rs_find_error_locator()

  // test rs_find_error_evaluator
  test_rs_find_error_evaluator()

  IF gf_log2 DO freevec(gf_log2)
  IF gf_exp2 DO freevec(gf_exp2)

  RESULTIS 0
}

/*
2-D QR barcode are composed of black and white cells normally called
modules typically arranged in an nxn square. The following represents
a typical 21x21 QR barcode.

     * * * * * * * . * * . . * . * * * * * * *
     * . . . . . * . * . * . * . * . . . . . *
     * . * * * . * . . . . * * . * . * * * . *
     * ' * * * . * . * * * . . . * . * * * . *
     * . * * * . * . . . * * . . * . * * * . *
     * . . . . . * . . * * * * . * . . . . . *
     * * * * * * * . * . * . * . * * * * * * *
     . . . . . . . . * . . * * . . . . . . . .
     * . * * . * * * . . * * * . * . . * . * *
     . * * . * * . * * * . . * . . . * * * * .
     . * * * * . * * . * . * . . . * . * . * *
     . * . . . * . * * . * * * . . . . * . * .
     * . * * * . * * * . * * * . * * . * . * .
     . . . . . . . . * * . * * . . . * . . . .
     * * * * * * * . * . . * . . . * * * * * .
     * . . . . . * . * * * * * * . * . * * * .
     * . * * * . * . . * * * * . . . . * * * .
     * . * * * . * . * . . . * . * . . . . * .
     * . * * * . * . * * * * * . . * . * * . .
     * . . . . . * . . * * * . * * * * . . . *
     * * * * * * * . * * . * * * * . * * * . .

In this diagram * represents a black module and . a white one. One can
easily see small squares at the top left, top right and bottom
left. These are there to allow a scanner to locate the position and
orientation of the barcode. The following diagram how the modules of a
QR barcode are used.

     * * * * * * * . o x x x x . * * * * * * *
     * . . . . . * . n x x x x . * . . . . . *
     * . * * * . * . m x x x x . * . * * * . *
     * ' * * * . * . l x x x x . * . * * * . *
     * . * * * . * . k x x x x . * . * * * . *
     * . . . . . * . j x x x x . * . . . . . *
     * * * * * * * . * . * . * . * * * * * * *
     . . . . . . . . i x x x x . . . . . . . .
     a b c d e f * g h x x x x h i j k l m n o
     x x x x x x . x x x x x x x x x x x x x x
     x x x x x x * x x x x x x x x x x x x x x
     x x x x x x . x x x x x x x x x x x x x x
     x x x x x x * x x x x x x x x x x x x x x
     . . . . . . . . ? x x x x x x x x x x x x
     * * * * * * * . g x x x x x x x x x x x x
     * . . . . . * . f x x x x x x x x x x x x
     * . * * * . * . e x x x x x x x x x x x x
     * . * * * . * . d x x x x x x x x x x x x
     * . * * * . * . c x x x x x x x x x x x x
     * . . . . . * . b x x x x x x x x x x x x
     * * * * * * * . a x x x x x x x x x x x x

The modules labelled * are always black and those marked with dots ,
are always white. The modules labelled abcdefghijklmn appear twice in
the barcode and contain a format information about how the message
data modules, all labelled x, are to be interpreted. The module
labelled ? contains no useful data. Using the convention that a black
module represents the digit 1 and a white one represents 0, the string
abcdefghijklmn represents a 15 bit binary number. This is modified by
performing the exclusive or operation on it with the mask pattern
101010000010010. For the example given above the calculation is as
follows.

abcdefghijklmn    101101101001011
                  101010000010010
                  ---------------
result            000111101011001

The leftmost 5 bits contains the format specification consisting of
two bits specifying the error correction level for the message data
followed by three bits specifying a mask bit pattern. In our example
there are 8x26 binary digits of message data corresponding to 26
bytes. If the level indicator is 00 there are 16 bytes of message data
and 10 error correction bytes. The indicator 01 specified 19 and 7, 10
specifies 13 and 13, and 11 specifies 9 and 17. Barcodes may be
damaged causing some of its modules to be misread by the barcode
scanner.  Increasing the number of error correction bytes increases
the number of misread modules that can be corrected. Of course, having
more error correction bytes reduces the number of message bytes.  The
mask bits specifies the pattern of zeroes and ones associated with
every module in the barcode. If we identify the position of a module
by the pair (i,j) where i is the column number counting from left to
right starting at zero, and j is the row number counting from top to
bottom stating at zero. So for an nxn barcode, the top left module has
coordinates (0,0) and the bottom right one has (n-1,n-1). If the mask
bits in the format information are 000, then the mask bit for module
(i,j) is a one if (i+j MOD 2 = 0. The rules for all eight masks are
given by the following table.

                  Mask bits        Module (i,j) is 1 if

                    000                (i+j) MOD 2 = 0
                    001                  i MOD 2 = 0
                    010                  j MOD 3 = 0
                    011              (i+j) MOD 3 = 0
                    100              (i/2+j/3) MOD 2 = 0
                    101          (i*j) MOD 2 + (i*j) MOD 3 = 0
                    110         ((i*j) MOD 3 + (i*j)) MOD 2 = 0
                    111          ((i*j) MOD 3 + i+j) MOD 2 = 0

The mask is applied to the data and correction bits using the
exclusive or operator before the error correction mechanism is
applied. Note that both occurrences of the the format information
abcdefghijklmno use the same mask pattern 101010000010010 which does
not correspond to any of the patterns given above. The mask is chosen
so that the orientation of the scanned barcode can be deduced without
ambiguity. For instance, if the data produced a pattern at the bottom
right corner that was identical to the marker squares at the top left,
there would be a problem. But this can be avaioded by choosing a
different mask pattern.

Of the 15 bits abcdefghijklmno only the first five abcde hold the
format information. The remaining 10 bits fghijklmno are there to
correct errors due to damaged or misread barcodes. The method used is
rather subtle and cunning. For each of the 32 patterns abcde, a
carefully chosen 10 bit pattern is added, giving 32 code words as
follows.

      abcde fghijklmno

C00   00000 0000000000
C01   00001 0100110111
C02   00010 1001101110
C03   00011 1101011001
C04   00100 0111101011
C05   00101 0011011100
C06   00110 1110000101
C07   00111 1010110010
C08   01000 1111010110
C09   01001 1011100001
C10   01010 0110111000
C11   01011 0010001111
C12   01100 1000111101
C13   01101 1100001010
C14   01110 0001010011
C15   01111 0101100100
C16   10000 1010011011
C17   10001 1110101100
C18   10010 0011110101
C19   10011 0111000010
C20   10100 1101110000
C21   10101 1001000111
C22   10110 0100011110
C23   10111 0000101001
C24   11000 0101001101
C25   11001 0001111010
C26   11010 1100100011
C27   11011 1000010100
C28   11100 0010100110
C29   11101 0110010001
C30   11110 1011001000
C31   11111 1111111111

These code words have been chosen so the every one is different from
every other one in at least 7 bit positions, as is shown by the
following table.

     C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C  C
     0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3  3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0  1

C00  0 7 7 8 8 7 7 8 8 7 7 8 8 7 7 8 7 8 8 7 7 8 8 7 7 8 8 7 7 8 8 15
C01  7 0 8 7 7 8 8 7 7 8 8 7 7 8 8 7 8 7 7 8 8 7 7 8 8 7 7 8 8 7 15 8
C02  7 8 0 7 7 8 8 7 7 8 8 7 7 8 8 7 8 7 7 8 8 7 7 8 8 7 7 8 8 15 7 8
C03  8 7 7 0 8 7 7 8 8 7 7 8 8 7 7 8 7 8 8 7 7 8 8 7 7 8 8 7 15 8 8 7
C04  8 7 7 8 0 7 7 8 8 7 7 8 8 7 7 8 7 8 8 7 7 8 8 7 7 8 8 15 7 8 8 7
C05  7 8 8 7 7 0 8 7 7 8 8 7 7 8 8 7 8 7 7 8 8 7 7 8 8 7 15 8 8 7 7 8
C06  7 8 8 7 7 8 0 7 7 8 8 7 7 8 8 7 8 7 7 8 8 7 7 8 8 15 7 8 8 7 7 8
C07  8 7 7 8 8 7 7 0 8 7 7 8 8 7 7 8 7 8 8 7 7 8 8 7 15 8 8 7 7 8 8 7
C08  8 7 7 8 8 7 7 8 0 7 7 8 8 7 7 8 7 8 8 7 7 8 8 15 7 8 8 7 7 8 8 7
C09  7 8 8 7 7 8 8 7 7 0 8 7 7 8 8 7 8 7 7 8 8 7 15 8 8 7 7 8 8 7 7 8
C10  7 8 8 7 7 8 8 7 7 8 0 7 7 8 8 7 8 7 7 8 8 15 7 8 8 7 7 8 8 7 7 8
C11  8 7 7 8 8 7 7 8 8 7 7 0 8 7 7 8 7 8 8 7 15 8 8 7 7 8 8 7 7 8 8 7
C12  8 7 7 8 8 7 7 8 8 7 7 8 0 7 7 8 7 8 8 15 7 8 8 7 7 8 8 7 7 8 8 7
C13  7 8 8 7 7 8 8 7 7 8 8 7 7 0 8 7 8 7 15 8 8 7 7 8 8 7 7 8 8 7 7 8
C14  7 8 8 7 7 8 8 7 7 8 8 7 7 8 0 7 8 15 7 8 8 7 7 8 8 7 7 8 8 7 7 8
C15  8 7 7 8 8 7 7 8 8 7 7 8 8 7 7 0 15 8 8 7 7 8 8 7 7 8 8 7 7 8 8 7
C16  7 8 8 7 7 8 8 7 7 8 8 7 7 8 8 15 0 7 7 8 8 7 7 8 8 7 7 8 8 7 7 8
C17  8 7 7 8 8 7 7 8 8 7 7 8 8 7 15 8 7 0 8 7 7 8 8 7 7 8 8 7 7 8 8 7
C18  8 7 7 8 8 7 7 8 8 7 7 8 8 15 7 8 7 8 0 7 7 8 8 7 7 8 8 7 7 8 8 7
C19  7 8 8 7 7 8 8 7 7 8 8 7 15 8 8 7 8 7 7 0 8 7 7 8 8 7 7 8 8 7 7 8
C20  7 8 8 7 7 8 8 7 7 8 8 15 7 8 8 7 8 7 7 8 0 7 7 8 8 7 7 8 8 7 7 8
C21  8 7 7 8 8 7 7 8 8 7 15 8 8 7 7 8 7 8 8 7 7 0 8 7 7 8 8 7 7 8 8 7
C22  8 7 7 8 8 7 7 8 8 15 7 8 8 7 7 8 7 8 8 7 7 8 0 7 7 8 8 7 7 8 8 7
C23  7 8 8 7 7 8 8 7 15 8 8 7 7 8 8 7 8 7 7 8 8 7 7 0 8 7 7 8 8 7 7 8
C24  7 8 8 7 7 8 8 15 7 8 8 7 7 8 8 7 8 7 7 8 8 7 7 8 0 7 7 8 8 7 7 8
C25  8 7 7 8 8 7 15 8 8 7 7 8 8 7 7 8 7 8 8 7 7 8 8 7 7 0 8 7 7 8 8 7
C26  8 7 7 8 8 15 7 8 8 7 7 8 8 7 7 8 7 8 8 7 7 8 8 7 7 8 0 7 7 8 8 7
C27  7 8 8 7 15 8 8 7 7 8 8 7 7 8 8 7 8 7 7 8 8 7 7 8 8 7 7 0 8 7 7 8
C28  7 8 8 15 7 8 8 7 7 8 8 7 7 8 8 7 8 7 7 8 8 7 7 8 8 7 7 8 0 7 7 8
C29  8 7 15 8 8 7 7 8 8 7 7 8 8 7 7 8 7 8 8 7 7 8 8 7 7 8 8 7 7 0 8 7
C30  8 15 7 8 8 7 7 8 8 7 7 8 8 7 7 8 7 8 8 7 7 8 8 7 7 8 8 7 7 8 0 7
C31  15 8 8 7 7 8 8 7 7 8 8 7 7 8 8 7 8 7 7 8 8 7 7 8 8 7 7 8 8 7 7 0

This means that, if we have a 15 bit pattern that matched one of the
code word with fewer than 3 bits in error, then it is the unique
closest match. If 4 or more bits are in error the best match may not
be unique and indeed may just be wrong. This scheme thus allows us to
detect and correct up to three bits in error. Since the code word
occurs twice in the barcode, there is an increased chance of finding a
successful match. This only fails if both codewords in the barcode
have 4 or more bits in error.  Since the table contains only 32
codewords the closest match can be found by exhaustive search,
by checking the scanned codeword with each of the 32 entries in the
table.

Essentially the same mechanism is used to correct errors in the
message data bytes, but since the number of possible messages is huge,
exhaustive search is not feasible, and another much more complicated
method is needed.

The tables above were calculated using the following functions.
*/

// The function hamming_dist(x,y) returns the number positions in
// the bit patterns x and y where the corresponding bits differ.
// Notice the diff & (diff-1) is a bit pattern identical to diff but
// with one of its ones removed. So the WHILE loop removes the ones
// from diff one by one incrementing count each time.

AND hamming_dist(x, y) = VALOF
{ LET diff = x XOR y
  LET count = 0
  WHILE diff DO diff, count := diff & (diff-1), count+1
  RESULTIS count
}

AND test_hamming_dist() BE
{ writef("Testing hamming_dist*n")
  FOR a = 0 TO 7 DO
  { writef("*n%3b: ", a)
    FOR b = 0 TO 7 DO writef(" %n", hamming_dist(a,b))
  }
  newline()
  newline()
}

AND qr_check_format(fmt) = VALOF
{ // This calculated the remainder after the 15-bit fmt is divided
  // by polynomial #b10100110111.
  LET g = #b10100110111 // The generator polynomial
  // We perform long division on bit patterns using
  // XOR in place of subtraction.
  //writef("%15b*n", fmt)
  FOR i = 4 TO 0 BY -1 DO
  { //writef("%15b   i=%n  bit=%15b*n", fmt, i, 1<<(i+10))
    TEST (fmt & (1<<(i+10))) > 0
    THEN { //writef("%15b  1*n", g<<i)
           fmt := fmt XOR (g<<i)
           //writef("---------------*n")
           //writef("%15b*n", fmt)
         }
    ELSE { //writef("%15b  0*n", 0)
           //writef("---------------*n")
           //writef("%15b*n", fmt)
         }
  }
  RESULTIS fmt // This is the 10 bit remainder
}

AND test_qr_check_format() BE
{ LET res = qr_check_format(#b000111101011001)
  writef("*nTesting gr_check_format*n")
  writef("*nThe remainder after dividing polynomial #b000111101011001*n")
  writef("by #b10100110111 is %15b.*n*n", res)
  UNLESS res=0 DO writef("ERROR: It should have been zero*n")
}

AND encode_format(fmt) = fmt<<10 XOR qr_check_format(fmt<<10)
// Note that
// qr_check_format(encode_format(fmt)) is zero for all fmt in 0 to 31.

AND test_encode_format() BE
{ writef("*nTesting encode_format*n*n")
  FOR fmt = #b00000 TO #b11111 DO
    writef("C%z2   %5b %10b*n", fmt, fmt, encode_format(fmt))

  FOR j = 0 TO 31 DO
  { writef("C%z2 ", j)
    FOR i = 0 TO 31 DO
    { LET Ci = encode_format(i)
      LET Cj = encode_format(j)

      writef(" %n", hamming_dist(Ci, Cj))
    }
    newline()
  }
}

AND qr_decode_format(fmt) = VALOF
{ LET best_fmt = -1
  LET best_dist = 16
  LET best_dist_count = 0
  FOR test_fmt = 0 TO 31 DO
  { LET test_code = encode_format(test_fmt)
    LET test_dist = hamming_dist(fmt, test_code)
    TEST test_dist < best_dist
    THEN { best_dist := test_dist
           best_fmt := test_fmt
           best_dist_count := 1
         }
    ELSE IF test_dist = best_dist DO best_dist_count := best_dist_count + 1

    writef("%5b: best_fmt %5i best_dist=%n best_dist_count=%n*n",
            test_fmt, best_fmt, best_dist, best_dist_count)
  }
  result2 := best_dist + best_dist_count*100
  // best_fmt is the best match if best_dist_count=1
  RESULTIS best_fmt
}

AND test_qr_decode_format() BE
{ LET code = 0

  writef("*nTesting qr_decode_format*n*n")

  code := #b000111101011001   // No errors
  writef("%15b => %5b dist=%n count=%n*n",
         code, qr_decode_format(code), result2 MOD 100, result2/100)

  code := #b111111101011001   // 3 bit errors
  writef("%15b => %5b dist=%n count=%n*n",
         code, qr_decode_format(code), result2 MOD 100, result2/100)

  code := #b111011101011001   // 4 bit errors
  writef("%15b => %5b dist=%n count=%n*n",
         code, qr_decode_format(code), result2 MOD 100, result2/100)

  code := #b111011001011011   // several bit errors
  writef("%15b => %5b dist=%n count=%n*n",
         code, qr_decode_format(code), result2 MOD 100, result2/100)
}

/*

In our example QR barcode the message and error correction bytes are
held as a stream of 26 bytes labelled A to Z. The digit following the
label gives the bit position within the byte. The placement of these
bytes is shown in the following diagram.

  *  *  *  *  *  *  *  .  o R1 R0 Q7 Q6  .  *  *  *  *  *  *  *
  *  .  .  .  .  .  *  .  n R3 R2 Q5 Q4  .  *  .  .  .  .  .  *
  *  .  *  *  *  .  *  .  m R5 R4 Q3 Q2  .  *  .  *  *  *  .  *
  *  '  *  *  *  .  *  .  l R7 R6 Q1 Q0  .  *  .  *  *  *  .  *
  *  .  *  *  *  .  *  .  k S1 S0 P7 P6  .  *  .  *  *  *  .  *
  *  .  .  .  .  .  *  .  j S3 S2 P5 P4  .  *  .  .  .  .  .  *
  *  *  *  *  *  *  *  .  *  .  *  .  *  .  *  *  *  *  *  *  *
  .  .  .  .  .  .  .  .  i S5 S4 P3 P2  .  .  .  .  .  .  .  .
  a  b  c  d  e  f  *  g  h S7 S6 P1 P0  h  i  j  k  l  m  n  o
 Z1 Z0 Y7 Y6 X1 X0  . W7 W6 T1 T0 O6 O6 J1 J0 I7 I6 D1 D0 C7 C6
 Z3 Z2 Y5 Y4 X3 X2  * W5 W4 T3 T2 O5 O4 J3 J2 I5 I4 D3 D2 C5 C4
 Z5 Z4 Y3 Y2 X5 X4  . W3 W2 T5 T4 O3 O2 J5 J4 I3 I2 D5 D4 C3 C2
 Z7 Z6 Y1 Y0 X7 X6  * W1 W0 T7 T6 O1 O0 J7 J6 I1 I0 D7 D6 C1 C0
  .  .  .  .  .  .  .  .  ? U1 U0 N7 N6 K1 K0 H7 H6 E1 E0 B7 B6
  *  *  *  *  *  *  *  .  g U3 U2 N5 N4 K3 K2 H4 H4 E3 E2 B5 B4
  *  .  .  .  .  .  *  .  f U5 U4 N3 N2 K5 K4 H3 H2 E5 E4 B3 B2
  *  .  *  *  *  .  *  .  e U7 U6 N1 N0 K7 K6 H1 H0 E7 E6 B1 B0
  *  .  *  *  *  .  *  .  d V1 V0 M7 M6 L1 L0 G7 G6 F1 F0 A7 A6
  *  .  *  *  *  .  *  .  c V3 V2 M5 M4 L3 L2 G5 G4 F3 F2 A5 A4
  *  .  .  .  .  .  *  .  b V5 V4 M3 M2 L5 L4 G3 G2 F5 F4 A3 A2
  *  *  *  *  *  *  *  .  a V7 V6 M1 M0 L7 l6 G1 G0 F7 F6 A1 A0

In our example barcode the format bit were 00_011 indicating that
there are 16 message data bytes A to P followed by 10 error correction
bytes Q to Z, and the mask pattern 011 is to be used.  After XORing
with the mask pattern, we find that the message bytes are: 40 d2 75 47
76 17 32 06 27 26 96 c6 c6 96 70 ec and the error correction bytes are
bc 2a 90 13 6b af ef fd 4b e0.

The message data is then corrected, if necessary using the error
correction bytes and the result is then decoded.

The resulting message bit stream is decoded as follows. The first 4
bit specify an encoding mode which in our example is 0100 indicating
that the data consists of 8-bit bytes. The next 8-bits specifies the
number of bytes in the message. This is followed by another mode
indicator or the end of message code 0000. The end of message code is
omitted if there is no room for it.

All that remains is to see how the error correction bytes are used to
correct the message.

The error correction technique makes use of arithmetic using the
normal operators of multiplication, division addition and subtraction
but with the restriction that all values fit in 8-bit bytes. This sort
of arithmetic is called galois Field 2^8 (typically written as
GF(2^8)).

Throughout this document, k will be the number of bytes in the
message, and n will be the length of the code word consisting of the
message followed by e=n-k error correcting bytes. Our example can be
described as a (26,16) code, indicating than n is 26, k is 16 and
e is 10.

*/

// Addition and subtraction are the same.
AND gf_add(x, y) = x XOR y

AND gf_sub(x, y) = x XOR y

// Multiplication in GF(2^8) can be done by regular multiplication
// producing a result typically too large to fit in 8 bits, followed
// by taking the remainder after division my 100011101. We will
// implement multiplication using table lookup of discrete logarithm
// and anti-logorithm tables. These are both quite small and can be
// precomputed. We can create these two tables using the function
// initlogs.
AND initlogs() BE
{ LET x = 1
  gf_log2 := getvec(GFlog2upb)
  gf_exp2 := getvec(GFexp2upb) // 509 = 254+255
  UNLESS gf_log2 & gf_exp2 DO
  { writef("initlogs: More space needed*n")
    abort(999)
  }

  // Initialise gf_exp2 with powers of 2 in GF(2^8) or GF(2^4).
  // While doing so place the inverse entries in gf_log2. Using
  // a double sized exp2 table improves the efficiency of both
  // functions gf_mul and gf_div.

  gf_log2!0 := -1

  //FOR i = 0 TO 254 DO // All possible values of form x MOD 255
  FOR i = 0 TO GFvalmax DO // All possible element values
  { gf_exp2!i := x
    gf_exp2!(i+GFvalmax) := x // Note 2^255=1 in GF(2^8)
    gf_log2!x := i
    x := x<<1
    UNLESS (x & GFtbit)=0 DO x := x XOR GFpoly
  }
  RETURN

  FOR i = 0 TO GFlog2upb DO
  { IF i MOD 20 = 0 DO writef("*nlog2 %i3: ", i)
    writef(" %i3", gf_log2!i)
  }
  newline()

  FOR i = 0 TO GFexp2upb DO
  { IF i MOD 20 = 0 DO writef("*nexp2 %i3: ", i)
    writef(" %i3", gf_exp2!i)
  }
  newline()
}

AND gf_mul(x, y) = VALOF
{ // Perform GF multiplication using logarithms base 2.
  // Since log 0 is undefined x=0 and y=0 are special cases.
  IF x=0 | y=0 RESULTIS 0
  //writef("gf_mul: x=%i3 y=%i3 log!x=%i3 log!y=%i3 => %i3*n",
  //       x,y,
  //       gf_log2!x, gf_log2!y, gf_exp2!(gf_log2!x+gf_log2!y))
  RESULTIS gf_exp2!(gf_log2!x + gf_log2!y)
}

AND gf_div(x, y) = VALOF
{ // Perform GF division using logarithms base 2.
  // Since log 0 is undefined x=0 and y=0 are special cases.
  IF y=0 DO
  { writef("gf_div: Division by zero*n")
    abort(999)
  }
  IF x=0 RESULTIS 0
  RESULTIS gf_exp2!(GFvalmax + gf_log2!x - gf_log2!y)
}

AND test_mul_div() BE
{ LET errors = 0
  writef("*nTesting gf_mul and gf_div*n")
  writef("*nTesting gf_div(gf_mul(x,y),y)=x*
         * for all x in [0,%n] and y in [1,%n]*n", GFvalmax, GFvalmax)
  FOR x = 0 TO GFvalmax FOR y = 1 TO GFvalmax DO
  { UNLESS gf_div(gf_mul(x,y),y)=x DO
    { LET xy = gf_mul(x,y)
      writef("test_mul_div: Error x=%n y=%n xy=%i3 xy/y=%i3*n", x, y, xy, gf_div(xy,y))
      errors := errors+1
      abort(1000)
    }
  }

  writef("*nTesting gf_inverse*n")
  FOR x = 1 TO GFvalmax UNLESS gf_inverse(x) = gf_div(1,x) DO
  { writef("Inverse: x=%n inverse=%n div(1,x)=%n*n", x, gf_inverse(x), gf_div(1,x))
    abort(1002)
  }

  writef("*nThere %p\was\were\ %n error%ps*n", errors, errors, errors)
}

AND gf_pow(x,y) = gf_exp2!((gf_log2!x * y) MOD GFvalmax)

AND gf_inverse(x) = gf_exp2!(GFvalmax - gf_log2!x)

/*

We now implement some some functions that work on polynomials with GF
coefficients. We will use a vectors to represent a polynomials. Its
zeroth element will be the degree of the polynomial (n, say) and v!1
will be the coefficient x^n, v!2 will be the coefficient x^(n-1), and
so on. So v!(n+1) will be the coefficient of x^0, which is the
constant term.

*/

AND gf_poly_scale(p, x, q) BE
{ // Multiply, using gf_mul, every coefficient of polynomial p by
  // scalar x leaving the result in q, assumed to be large enough.
  LET deg = p!0  // The degree of polynomial p
  q!0 := deg      // The degree of the result
  FOR i = 1 TO deg+1 DO q!i := gf_mul(p!i, x)
} 

AND gf_poly_add(p, q, r) BE
{ // Add polynomials p and q leaving the result in r
  LET degp = p!0 // The number of coefficients is one larger
  LET degq = q!0 // than the degree of the polynomial.
  LET degr = degp
  IF degq>degr DO degr := degq
  // degr is the larger of the degrees of p and q.
  r!0 := degr    // The degree of the result
  FOR i = 1 TO degp+1    DO r!(i+degr-degp) := p!i
  FOR i = 1 TO degr-degp DO r!i := 0 // Pad higher coeffs with 0s
  FOR i = 1 TO degq+1 DO r!(i+degr-degq) := r!(i+degr-degq) XOR q!i
}

AND test_gf_poly_add() BE
{ LET d1, a,b,c     = 2, 2, 3, 4       // 2x^2 + 3x + 4
  LET d2, u,v,w,x,y = 4, 5, 6, 7, 8, 9 // 5x^4 + 6x^3 + 7x^2 + 8x + 9
  LET r = VEC 10

  r!0, r!1, r!2, r!3 := 2, 10, 11, 12

  gf_poly_add(@d1, @d2, r)
  newline()
  writef("Poly  "); pr_poly(@d1)
  writef("+     "); pr_poly(@d2)
  writef("gives "); pr_poly(r)

  gf_poly_add(@d2, @d1, r)
  newline()
  writef("Poly  "); pr_poly(@d2)
  writef("+     "); pr_poly(@d1)
  writef("gives "); pr_poly(r)

  gf_poly_add(@d2, @d2, r)
  newline()
  writef("Poly  "); pr_poly(@d2)
  writef("+     "); pr_poly(@d2)
  writef("gives "); pr_poly(r)

  newline()
}

// GF addition and subtraction are the same.
AND gf_poly_sub(p, q, r) BE gf_poly_add(p, q, r)

AND gf_poly_mul(p, q, r) BE
{ // Multiply polynomials p and q leaving the result in r
  LET degp = p!0
  LET degq = q!0
  LET degr = degp+degq
//newline()
//writef("poly_mul p= "); pr_poly(p)
//writef("poly_mul q= "); pr_poly(q)

  r!0 := degr    // Degree of the result
  FOR i = 1 TO degr+1 DO r!i := 0
  FOR j = 1 TO degq+1 DO
    FOR i = 1 TO degp+1 DO
      r!(i+j-1) := r!(i+j-1) XOR gf_mul(p!i, q!j)
//writef("gives    r= "); pr_poly(r)
//abort(1000)
}

AND gf_poly_mulbyxn(p, e, r) BE
{ // Multiply polynomials p by x^e leaving the result in r
  LET degp = p!0
  LET degr = degp + e
  r!0 := degr
  FOR i = 1 TO degp+1 DO r!i := p!i
  FOR i = degp+2 TO degr+1 DO r!i := 0
}

AND gf_poly_eval(p, x) = VALOF
{ // Evaluate polynomial p for a given x using Horner's method.
  // Eg use:  ax^3 + bx^2 + cx^1 + d  =  ((ax + b)x + c)x + d
  LET res = p!1
  FOR i = 2 TO p!0+1 DO
    res := gf_mul(res,x) XOR p!i // mul by x and add next coeff
  RESULTIS res
}

AND test_gf_poly_eval() BE
{ LET p = ?
  writef("*nTesting gf_poly_eval*n")

  p := TABLE 0, #x11
  writef("poly:       "); pr_poly(p)
  FOR x = 1 TO 4 DO
    writef("x=%x2 => %x2*n", x, gf_poly_eval(p, x))
  newline()

  p := TABLE 1, #x11, #x22
  writef("poly:       "); pr_poly(p)
  FOR x = 1 TO 4 DO
    writef("x=%x2 => %x2*n", x, gf_poly_eval(p, x))
  newline()

  p := TABLE 3, #x2, #x5, #xA, #xF
  writef("poly:       "); pr_poly(p)
  FOR x = 1 TO 4 DO
    writef("x=%x2 => %x2*n", x, gf_poly_eval(p, x))
  newline()
}

AND gf_generator_poly(e, g) BE
{ // Set in g the polynomial resulting from the expansion of
  // (x-2^0)(x-2^1)(x-2^2) ... (x-2^(e-1))
  // Note that the polynomial has leading term x^(n-1).
  LET t = VEC GFvalmax
  g!0, g!1 := 0, 1 // The polynomial: 1.
  FOR i = 0 TO e-1 DO
  { LET d, a, b = 1, 1, gf_pow(2,i) // (x + 2^i)
    // @d points to polynomial:        (x - 2^i)
    // which in GF arithmetic is also: (x + 2^i)
    //writef("Poly g "); pr_poly(g)
    //writef("mul by "); pr_poly(@d)
    FOR i = 0 TO g!0+1 DO t!i := g!i // Copy g into t
    gf_poly_mul(t, @d, g) // Multiply t by (x-2^i) into g
    //writef("gives  "); pr_poly(g)
    //newline()
  }
}

AND test_gf_generator_poly() BE
{ LET g = VEC 50
  writef("*nTesting gf_generator_poly*n*n")

  FOR i = 0 TO 10 DO
  { gf_generator_poly(i, g)
    writef("gf_generator_poly(%i2,g) => g=", i); pr_poly(g)
  }
  newline()
}

AND pr_poly(p) BE
{ FOR i = 1 TO p!0+1 DO writef(" %x2", p!i)
  newline()
}

AND pr_poly_decimal(p) BE
{ FOR i = 1 TO p!0+1 DO writef(" %i3", p!i)
  newline()
}

AND gf_poly_divmod(p, q, r) BE
{ // This divides polynomial p by ploynomial q placing
  // the quotient and remainder in r, assumed to be large
  // enough.
  LET degp = p!0   // The degree of polynomial p.
  LET degq = q!0   // The degree of polynomial q.
  LET degr = degp

  LET t = VEC GFvalmax  // Vector to hold the next product of the generator

  UNLESS q!1 > 0 DO
  { writef("The divisor must have a non zero leading coefficient*n")
    abort(999)
    RETURN
  }

  // Copy polynomial p into r.
  r!0 := degr
  FOR i = 1 TO degr+1 DO r!i := p!i

  //writef("q:         "); pr_poly(q)
  //writef("r:         "); pr_poly(r)

  FOR i = 1 TO degp-degq+1 DO
  { LET dig = gf_div(r!i, q!1)
    IF dig DO
    { gf_poly_scale(q, dig, t)
      //writef("scaled  q: ")
      //FOR j = 2 TO i DO writef("   ")
      //pr_poly(t)
      r!i := dig
      FOR j = 2 TO t!0+1 DO r!(i+j-1) := r!(i+j-1) XOR t!j
    }
    //writef("new     r: "); pr_poly(r)
  }
}

AND gf_poly_div(p, q, r) BE
{ gf_poly_divmod(p, q, r)
  r!0 := p!0 - q!0  // Select just the quotient
}

AND gf_poly_mod(p, q, r) BE
{ LET degp = p!0
  LET degq = q!0
  LET degr = degq - 1
//newline()
//writef("p:          "); pr_poly(p)
//writef("q:          "); pr_poly(p)
  gf_poly_divmod(p, q, r)
//writef("p divmod q: "); pr_poly(r)
  r!0 := degr  // Over write the quotient with the remainder.
  FOR i = 1 TO degr+1 DO r!i := r!(i+degp-degr)
//writef("p mod q:    "); pr_poly(r)
//newline()

//gf_poly_div(p,q,r)
//writef("p  / q:    "); pr_poly(r)
//newline()
//abort(1000)
}

AND test_gf_poly_divmod() BE
{ LET p = VEC 20
  LET q = TABLE 5, 5, 1, 1, 1, 1, 1 // 5x^6 +x^4+x^3 +x^2+x+1
  LET r = VEC 20
  LET quotient  = VEC 20
  LET remainder = VEC 20

  writef("*nTesting gf_poly_divmod*n*n")
  gf_generator_poly(10, p)

  writef("p:         "); pr_poly(p)
  writef("q:         "); pr_poly(q)
  gf_poly_divmod(p, q, r)
  writef("p divmod q:"); pr_poly(r)
  newline()

  writef("p:         "); pr_poly(p)
  writef("q:         "); pr_poly(q)
  gf_poly_div(p, q, quotient)
  writef("p / q:     "); pr_poly(quotient)
  newline()

  writef("p:         "); pr_poly(p)
  writef("q:         "); pr_poly(q)
  gf_poly_mod(p, q, remainder)
  writef("p mod q:   "); pr_poly(remainder)
  newline()

  writef("*nnumerator:                  "); pr_poly(p)

  gf_poly_mul(q, quotient, r)
  writef("q ** quotient:               "); pr_poly(r)


  gf_poly_add(r, remainder, p)
  writef("q ** quotient + remainder:   "); pr_poly(p)
  newline()
}

/*

In most of the worked example we assume we are using GF(2^8).

The function rs_encode_msg returns in r the polynomial msg
concatenated with the e Reed-Solomon check bytes which represent
remainder after the msg polynomial is divided by the generator
polynomial created by rs_generator_poly with an argument of e. The msg
bytes are left shifted by e positions corresponding to multiplying the
msg polynomial by x^e. The following is an example of the calculation
with message polynomial 12 34 56 78 and e=6. This value of e gives us
the generator polynomial 01 3F 01 DA 20 E3 26.

                                            12 9D 43 57
                           ----------------------------
   01 3F 01 DA 20 E3 26 ) 12 34 56 78 00 00 00 00 00 00
                          12 A9 12 88 7A 4D 16
                             -----------------
                             9D 44 F0 7A 4D 16 00
                             9D 07 9D 3F 4A 51 23
                                -----------------
                                43 6D 45 07 47 23 00
                                43 3A 43 F7 88 5A 1F
                                   -----------------
                                   57 06 F0 CF 79 1F 00
                                   57 11 57 99 32 67 DD
                                   --------------------
                                      17 A7 56 4B 78 DD

It thus computes 12 9D 43 57 as the quotient and 17 A7 56 4B 7B DD as
the remainder.  The process is basically long division using gf_mul
for multiplication and XOR for subtraction. If at each stage the
senior byte is not subtracted, the senior 4 bytes of the accumulator
would hold the quotient and the junior 6 bytes would hold the remainder.
This is because the senior coefficient of the generator polynomial is
always a one. If, at the end, we replace the senior 4 bytes of the
accumulator by the original 4 byte message, we create the Reed-Solomon
codeword for the 4 byte message and 6 check bytes.

The definition of rs_encode_msg is as follows.
*/

AND rs_encode_msg(msg, e, r) BE
{ // This appends e Reed-Solomon check bytes on the end of the
  // message bytes placing the result in r which is assumed to be
  // large enough.
  LET degm = msg!0  // The degree of the message polynomial.
  LET degr = degm+e // The degree of the RS codeword polynomial
  LET g = VEC GFvalmax   // Vector to hold the generator.
  LET t = VEC GFvalmax   // Vector to hold the next product of the generator

  // Place the message polynomial multiplied by x^n in r.
  r!0 := degr 
  FOR i = 1 TO degm+1       DO r!i := msg!i
  FOR i =  degm+2 TO degr+1 DO r!i := 0

  gf_generator_poly(e, g)
  // g!1 is always one.
  //writef("generator: "); pr_poly(g)
  //writef("initial r: "); pr_poly(r)

  FOR i = 1 TO degm+1 DO
  { LET dig = r!i
    IF dig DO
    { gf_poly_scale(g, dig, t)
      //writef("scaled  g: ")
      //FOR j = 2 TO i DO writef("   ")
      //pr_poly(t)
      FOR j = 2 TO g!0+1 DO r!(i+j-1) := r!(i+j-1) XOR t!j
    }
    //writef("new     r: "); pr_poly(r)
  }
  // Copy msg in the senior end of r replacing the bytes that
  // hold the quotient
  FOR i = 1 TO degm+1 DO r!i := msg!i
}

AND test_rs_encode_msg() BE
{ LET msg = ?
  LET r = VEC 26

  writef("*nTesting rs_encode_msg*n*n")

  msg := TABLE 3, #x12, #x34, #x56, #x78
  rs_encode_msg(msg, 6, r) // Compute the RS codeword
  writef("message:   "); pr_poly(msg)
  writef("codeword:  "); pr_poly(r)
  newline()

  //msg := TABLE 15, #x40, #xD2, #x75, #x47, #x76, #x17, #x32, #x06,
  //                 #x27, #x26, #x96, #xC6, #xC6, #x96, #x70, #xEC
  //rs_encode_msg(msg, 10, r) // Compute the RS codeword
  //writef("message:   "); pr_poly(msg)
  //writef("codeword:  "); pr_poly(r)
  //newline()
}

/*

We have seen the a Reed-Solomon codeword consists of k bytes of
message followed by e error correction bytes which represent the
remainder after deviding the message polynomial multiplied by x^e by
the generating polynomial. Since addition and subtraction are both the
same in GF arithmetic, the effect is that the codeword is exactly
divisible by the generating polynomial and, sSince the generator
polynomial is the product of many factors of the form (1 - x*2^i),
each of these factors exactly divides into the codeword exactly.
However if some bytes of the codeword are corrupted, none of these
factors will exactly divide the corrupted codeword. We can easily
create a polynomial of degree e-1 whose coefficients are the e
remainders obtained when attempting to divide the corrupted codeword
by each factor of the generator polynomial.

To demonstrate the error correction of a corrupted Reed Solomon
codeword, I will use an example of a 4 byte message 12 34 56 78 and 6
error correcting bytes. We thus have k=4, e=6 and so n=10.  The
generator polynomial g(x) is therefore

g(x) = (x-2^0)(x-2^1)(x-2^2)(x-2^3)(x-2^4)(x-2^5)
     = (x-01)(x-02)(x-04)(x-08)(x-16)(x-32)
     = 01*x^6 + 3F*x^5 + 01*x^4 + DA*x^3 + 20*x^2 + E3*x + 26

generator:  01 3F 01 DA 20 E3 26
codeword:   12 34 56 78 17 A7 56 4B 78 DD

Note the using + rather than - makes no difference in GF
arithmetic. It turns out that using this generator of this form
maximises the Hamming distance between codewords.

For simplicity we will write g, T the codeword and R the corrupted
codeword as:

g = 01 3F 01 DA 20 E3 26
T = 12 34 56 78 17 A7 56 4B 78 DD
R = 12 34 00 00 17 00 56 4B 78 DD

You will notice that bytes 3, 4 and 6 of the codeword have been
zeroed.

In general, when we attempt to read a codeword some of its bytes may
be corrupted resulting in a different polynomial R(x) which can be
written as the sum of T(x), the original codeword, and E(x) an errors
polynomial giving a correction value for each coefficient of R. This
is stated in the following equation:

R(x) = T(x) + E(x)

Suppose the polynomial for a corrupted codeword is

R = 12 34 00 00 17 00 56 4B 78 DD

This will mean that

E = 00 00 56 78 00 A7 00 00 00 00

which when added to R gives the corrected codeword. Our problem is can
we deduce the errors polynomial E knowing only R and the generator
polynomial . It turns out that we can, provided not too many bytes have
been corrupted. With 6 check bytes we can correct the 6/2=3 corrupted
bytes in R.

To do this we first construct a polynomial S (called the syndromes
polynomial) whose coefficients are the remainders after dividing R by
each of the factors of the generator polynomial. In our example e=6 so
the generator has 6 factors (x-2^0), (x-2^1), (x-4^2), (x-2^3),
(x-2^4) and (x-2^5). S can be written as

S(x) = S5*x^5 + S4*x^4 + S3*x^3 + S2*x^2 + S1*x + S0

When we divide R by (x-2^i) we obtain a quotient polynomial Qi and a
remander Si. These, of course, satisfy the following equation

   R(x) = (x-2^i)*Qi(x) + Si

and if we set x = 2^i this reduces to

   R(2^i) = Si

So Si can be calculated just by evaluating the polynomial R(x) at
x=2^i. For our example the syndromes polynomial is:

S = 2E B8 0E CB 50 35

If we happen to know in advance the positions in the codeword that
have been corrupted, in this case 3, 4 and 6, then we could write the
errors polynomial as

E(x) = Y1*x^7 + Y2*x^6 + Y3*x^4         All the other terms are zero

Hopefully there is sufficient information to deduce Y1=56, Y2=78 and
Y3=A7.

Since we have just shown E(2^i) = Si, we can say

Si = E(2^i)
   = Y1*2^(7*i) + Y2*2^(6*i) + Y3*2^(4*i)
   == Y1*X1^i + Y2*X2^i + Y3*X3^i
where X1 = 2^7, X2 = 2^6 and X3 = 2^4

These 6 equations can be written as a matrix product as follow

( S0 ) =  ( X1^0  X2^0  X3^0 ) x ( Y1 )
( S1 )    ( X1^1  X2^1  X3^1 )   ( Y2 )
( S2 )    ( X1^2  X2^2  X3^2 )   ( Y3 )
( S3 )    ( X1^3  X2^3  X3^3 )
( S4 )    ( X1^4  X2^4  X3^4 )
( S5 )    ( X1^5  X2^5  X3^5 )

We know that S = 2E B8 0E CB 50 35 and assuming we know that X1=2^7, X2=2^6 and
X3=2^4, this product simplifies to

( 2E ) =  ( 2^ 0  2^ 0  2^ 0 ) x ( Y1 )
( B8 )    ( 2^ 7  2^ 6  2^ 4 )   ( Y2 )
( 0E )    ( 2^14  2^12  2^ 8 )   ( Y2 )
( CB )    ( 2^21  2^18  2^12 )
( 50 )    ( 2^28  2^24  2^16 )
( 35 )    ( 2^35  2^30  2^20 )

or

2^(0*7) 2^(0*6) 2^(0*4) = 01 01 01
2^(1*7) 2^(1*6) 2^(1*4) = 80 40 10
2^(2*7) 2^(2*6) 2^(2*4) = 13 CD 1D
2^(3*7) 2^(3*6) 2^(3*4) = 75 2D CD
2^(4*7) 2^(4*6) 2^(4*4) = 18 8F 4C
2^(5*7) 2^(5*6) 2^(5*4) = 9C 60 B4

( 2E ) =  ( 01  01  01 ) x ( Y1 )
( B8 )    ( 80  40  10 )   ( Y2 )
( 0E )    ( 13  CD  1D )   ( Y2 )
( CB )    ( 75  2D  CD )
( 50 )    ( 18  8F  4C )
( 35 )    ( 9C  60  B4 )

If these equations are consistent and non singular they can be solved.
The solution in this case turns out to be Y1=56, Y2=78 and
Y3=A7, as expected.

These values for Y1, Y2 and Y3 tells us that E(x)=56*x^7+78*x^6+A7*x^4
giving us the required result

E = 00 00 56 78 00 A7 00 00 00 00

which when added to

R = 12 34 00 00 17 00 56 4B 78 DD

give use the corrected codeword

T = 12 34 56 78 17 A7 56 4B 78 DD

If we know the locations of 6 error we could correct all 6. But, as is
probably the case, we do not know the location of any of them we have
more work to do.

The following functions calculate the syndromes polynomial and use it
to confirm the accuracy the description just given.

*/

AND rs_calc_syndromes(codeword, e, r) BE
{ // e = the number of error correction bytes
  //writef("*nrs_calc_syndromes:*n")
  //writef("codeword:  "); pr_poly(codeword)
  r!0 := e-1  // The degree of the syndromes polynomial.
  FOR i = 0 TO e-1 DO
  { LET p2i = gf_pow(2,i)
    LET res = gf_poly_eval(codeword, p2i)
    //writef("%i2 2^i = %x2 => %x2 %i3*n", i, p2i, res, res)
    r!(i+1) := res // r!(i+1) = codeword(2^i)
  }
}

AND test_rs_calc_syndromes() BE
{ LET msg = ?
  LET g = VEC 26 // For the codeword
  LET r = VEC 26 // For the codeword
  LET s = VEC 26 // For the syndromes
  LET c = VEC 26 // Corrupted codeword
  LET t = VEC 26 // Temp polynomial

  writef("*nTesting rs_calc_syndromes*n")

  newline()
  msg := TABLE 3, #x12, #x34, #x56, #x78
  gf_generator_poly(6, g)    // Compute the generator polinomial
  rs_encode_msg(msg, 6, r)   // Compute the RS codeword leaving it in r

  newline()
  writef("2^i:       ")               // 01 02 04 08 10 20 40
  FOR i = 0 TO 6 DO writef(" %x2", gf_pow(2,i))
  newline()
  newline()
  writef("generator: "); pr_poly(g)    // 01 3F 01 DA 20 E3 26
  writef("message:   "); pr_poly(msg)  // 12 34 56 78
  writef("codeword:  "); pr_poly(r)    // 12 34 56 78 17 A7 56 4B 78 DD
  FOR i = 0 TO r!0+1 DO c!i := r!i
  c!3 := 0
  c!4 := 0
  c!6 := 0
  writef("corrupted: "); pr_poly(c)    // 12 34 00 00 17 00 56 4B 78 DD
  rs_calc_syndromes(c, 6, s) // Compute the syndromes of polynomial c
  writef("syndromes: "); pr_poly(s)    // 89 81 DE 6E 82 7A
  writef("*nNote that*n")
  gf_poly_mod(c, (TABLE 1, 1, 1), t)
  writef("Remainder after dividing c by (x+2^0) is %x2*n", t!1)
  writef("gf_poly_eval(c, 2^0) is                  %x2 = S0*n", gf_poly_eval(c, 1))
  gf_poly_mod(c, (TABLE 1, 1, 2), t)
  writef("Remainder after dividing c by (x+2^1) is %x2*n", t!1)
  writef("gf_poly_eval(c, 2^1) is                  %x2 = S1*n", gf_poly_eval(c, 2))
  gf_poly_mod(c, (TABLE 1, 1, 4), t)
  writef("Remainder after dividing c by (x+2^2) is %x2*n", t!1)
  writef("gf_poly_eval(c, 2^2) is                  %x2 = S2*n", gf_poly_eval(c, 4))
  gf_poly_mod(c, (TABLE 1, 1, 8), t)
  writef("Remainder after dividing c by (x+2^3) is %x2*n", t!1)
  writef("gf_poly_eval(c, 2^3) is                  %x2 = S3*n", gf_poly_eval(c, 8))
  gf_poly_mod(c, (TABLE 1, 1, 16), t)
  writef("Remainder after dividing c by (x+2^4) is %x2*n", t!1)
  writef("gf_poly_eval(c, 2^4) is                  %x2 = S4*n", gf_poly_eval(c, 16))
  gf_poly_mod(c, (TABLE 1, 1, 32), t)
  writef("Remainder after dividing c by (x+2^5) is %x2*n", t!1)
  writef("gf_poly_eval(c, 2^5) is                  %x2 = S5*n", gf_poly_eval(c, 32))

newline()
  writef("position 3 corresponds to term x^7*n")
  writef("position 4 corresponds to term x^6*n")
  writef("position 6 corresponds to term x^4*n")

  newline()
  writef("X1=x^7 X2=x^6 X3=x^4*n")

  newline()
  writef("X1^0 X2^0 X3^0 = 2^(0x7) 2^(0x6) 2^(0x4) = %x2 %x2 %x2*n",
          gf_exp2! 0 , gf_exp2! 0, gf_exp2! 0)
  writef("X1^1 X2^1 X3^1 = 2^(1x7) 2^(1x6) 2^(1x4) = %x2 %x2 %x2*n",
          gf_exp2! 7 , gf_exp2! 6, gf_exp2! 4)
  writef("X1^2 X2^2 X3^2 = 2^(2x7) 2^(2x6) 2^(2x4) = %x2 %x2 %x2*n",
          gf_exp2!14 , gf_exp2!12, gf_exp2! 8)
  writef("X1^3 X2^3 X3^3 = 2^(3x7) 2^(3x6) 2^(3x4) = %x2 %x2 %x2*n",
          gf_exp2!21 , gf_exp2!18, gf_exp2!12)
  writef("X1^4 X2^4 X3^4 = 2^(4x7) 2^(4x6) 2^(4x4) = %x2 %x2 %x2*n",
          gf_exp2!28 , gf_exp2!24, gf_exp2!16)
  writef("X1^5 X2^5 X3^5 = 2^(5x7) 2^(5x6) 2^(5x4) = %x2 %x2 %x2*n",
          gf_exp2!35 , gf_exp2!30, gf_exp2!20)

  newline()

  writef("01x56 + 01x78 + 01xA7 = %x2   -- 89*n",
          gf_mul(#x01,#x56) XOR gf_mul(#x01,#x78) XOR gf_mul(#x01,#xA7))
  writef("80x56 + 40x78 + 10xA7 = %x2   -- 81*n",
          gf_mul(#x80,#x56) XOR gf_mul(#x40,#x78) XOR gf_mul(#x10,#xA7))
  writef("13x56 + CDx78 + 1DxA7 = %x2   -- DE*n",
          gf_mul(#x13,#x56) XOR gf_mul(#xCD,#x78) XOR gf_mul(#x1D,#xA7))
  writef("75x56 + 2Dx78 + CDxA7 = %x2   -- 6E*n",
          gf_mul(#x75,#x56) XOR gf_mul(#x2D,#x78) XOR gf_mul(#xCD,#xA7))
  writef("18x56 + 8Fx78 + 4CxA7 = %x2   -- 82*n",
          gf_mul(#x18,#x56) XOR gf_mul(#x8F,#x78) XOR gf_mul(#x4C,#xA7))
  writef("9Cx56 + 60x78 + B4xA7 = %x2   -- 7A*n",
          gf_mul(#x9C,#x56) XOR gf_mul(#x60,#x78) XOR gf_mul(#xB4,#xA7))

  writef("*nie Y1=56  Y2=78  Y3=A7  assuming e1=7  e2=6  e3=4*n")
  writef("So if we know e1, e2 and e3 we can solve the equations*n")
  writef("to give Y1, Y2 and Y3*n")
  //newline()
  //msg := TABLE 15, #x40, #xD2, #x75, #x47, #x76, #x17, #x32, #x06,
  //                 #x27, #x26, #x96, #xC6, #xC6, #x96, #x70, #xEC
  //writef("message:   "); pr_poly(msg)
  //rs_encode_msg(msg, 10, r)   // Compute the RS codeword leaving it in r
  //r!1 := 0 // Corrupt the code word
  //rs_calc_syndromes(r, 10, s) // Compute the syndromes poly leaving it in s

  //newline()
  //writef("codeword:  "); pr_poly(r)
  //writef("syndromes: "); pr_poly(s)
}

/*

Our problem is now to try and find the locations of errors in the
corrupted codeword using only its syndomes polynomial and the
generator polynomial.

It is common in mathematcs and computing to pick out a seemingly
unrelated construct, as if by magic, and after a little elementary
manipulation suddenly realise it is just what we want.

Let us assume there are three locations e1, e2 and e3 containing
corrupted bytes in the codeword.  Let us now consider the following
polynomial.

L(x) = (1+x*2^e1)(1+x*2^e2)(1+x*2^e3)
     = 1 + L1*x + L2*x^2 + L3*x^3

This polynomial is zero when x=2^-e1, or x=2^-e2 or x=2^-e3. This
allows us the write the following equation.

    1 + L1*2^-ej + L2*2^-2ej + l3*2^-3ej = 0

If we multiply this equation by Yj*2^(i+3)ej, we get

    Yj*2^(i+3)ej + L1*Yj*2^(i+2)ej + L2*Yj*2^(i+1)ej + L3*Yj*2^iej = 0

If we write these for each value of j, we get

    Y1*2^(i+3)e1 + L1*Y1*2^(i+2)e1 + L2*Y1*2^(i+1)e1 + L3*Y1*2^ie1 = 0
    Y2*2^(i+3)e2 + L1*Y2*2^(i+2)e2 + L2*Y2*2^(i+1)e2 + L3*Y2*2^ie2 = 0
    Y3*2^(i+3)e3 + L1*Y3*2^(i+2)e2 + L2*Y3*2^(i+1)e3 + L3*Y3*2^ie3 = 0

Remembering that

E(x) = Y1*x^e1 + Y2*x^e2 + Y3*x^e3

We can add these three equations together giving

    E(2^(i+3)) + L1*E(2^(i+2)) + L2*E(2^(i+1)) + L3*E(2^i) = 0

We thus have 3 equations of this for by setting i to each value
between 0 and 2, and we can replace E(2^k) by R(2^k). The equations
thus become

    R(2^3) + L1*R(2^2) + L2*R(2^1) + L3*R(2^0) = 0
    R(2^4) + L1*R(2^3) + L2*R(2^2) + L3*R(2^1) = 0
    R(2^5) + L1*R(2^4) + L2*R(2^3) + L3*R(2^2) = 0

These equations can be written in matrix form as follows.

    ( R(2^3) ) = ( R(2^2) R(2^1) R(2^0) ) x ( L1 )
    ( R(2^4) )   ( R(2^3) R(2^2) R(2^1) )   ( L2 )
    ( R(2^5) )   ( R(2^4) R(2^3) R(2^2) )   ( L3 )

Provided the 3x3 matrix is not singular, the equations can be solved
giving us the values of L1, L2 and L3. We now have the equation

L(x) = 1 + L1*x + L2*x^2 + L3*x^3

completely defined and we can therefor find its roots X1, X2 and X3,
and since X1=2^-e1, or X2=2^-e2andr X3=2^-e3, we can deduce the error
positions e1, e2 and e3.

For our example, the equations matrix equation is

    ( 6E ) = ( DE 81 89 ) x ( L1 )
    ( 82 )   ( 6E DE 81 )   ( L2 )
    ( 7A )   ( 82 6E DE )   ( L3 )

giving L1=D0, L2=1B and L3=98.

In general, we do not know how many errors there are. if there are
fewer than 3 the 3x3 matrix will have a zero determinant and we will
have to try for 2 errors, but if the top left 2x2 determinant is zero,
we will have to try the top left 1x1 matrix.

The solution, if any, of this matrix equation is normally solved by
Berlekamp's method described later.

*/

AND find_errata_locator(e_pos, r) BE
{ // This function is only used when we know the locations of
  // the error bytes.
  // Compute the erasures/error/errata locator polynomial from
  // given positions. e_pos is a list or error positions
  // numbered from the right hand end of the code word.
  // The resulting polynomial is placed in r and is the product
  // of terms of the form (1 - x*2^i) for each i in e_pos.
  r!0, r!1 := 0, 1 // The initial polynomial
  writef("find_errata_locator: e_pos: "); pr_poly_decimal(e_pos)
//abort(2000)
  FOR i = 1 TO e_pos!0+1 DO
  { LET e = e_pos!i // e is an error position taken from e_pos
                    // counting from the right hand end of
                    // the code word.
    LET d, a, b = 1, gf_pow(2, e), 1 // term = (1 - x*2^e)
    LET t = VEC GFvalmax
    FOR i = 0 TO r!0+1 DO t!i := r!i // Copy r
    //writef("*nfind_errata_locator: e=%n*n", e)
    //writef("Multiplying:  "); pr_poly(t)
    //writef("by term:      "); pr_poly(@d)
    gf_poly_mul(t, @d, r) // r := r * term
    //writef("Giving:       "); pr_poly(r)
  }
}

AND test_find_errata_locator() BE
{ LET e_pos = VEC 20
  LET e_loc = VEC 20
  LET g = VEC 20
  LET r = VEC 20
  LET c = VEC 20
  LET msg = TABLE 3, #x12, #x34, #x56, #x78

  writef("*nTesting find_errata_locator*n")

  newline()
  gf_generator_poly(6, g)    // Compute the generator polinomial
  rs_encode_msg(msg, 6, r)   // Compute the RS codeword leaving it in r

  newline()
  writef("generator: "); pr_poly(g)    // 01 3F 01 DA 20 E3 26
  writef("message:   "); pr_poly(msg)  // 12 34 56 78
  writef("codeword:  "); pr_poly(r)    // 12 34 56 78 17 A7 56 4B 78 DD
  FOR i = 0 TO r!0+1 DO c!i := r!i
  c!3 := 0
  c!4 := 0
  c!6 := 0
  writef("corrupted: "); pr_poly(c)    // 12 34 00 00 17 00 56 4B 78 DD
  rs_calc_syndromes(c, 6, r) // Compute the syndromes of polynomial c
  writef("syndromes: "); pr_poly(r)    // 89 81 DE 6E 82 7A

  newline()

  writef("%x2    %x2 %x2 %x2*n",
          gf_poly_eval(c, gf_exp2!(3)),
          gf_poly_eval(c, gf_exp2!(2)),
          gf_poly_eval(c, gf_exp2!(1)),
          gf_poly_eval(c, gf_exp2!(0)))

  writef("%x2    %x2 %x2 %x2*n",
          gf_poly_eval(c, gf_exp2!(4)),
          gf_poly_eval(c, gf_exp2!(3)),
          gf_poly_eval(c, gf_exp2!(2)),
          gf_poly_eval(c, gf_exp2!(1)))

  writef("%x2    %x2 %x2 %x2*n",
          gf_poly_eval(c, gf_exp2!(5)),
          gf_poly_eval(c, gf_exp2!(4)),
          gf_poly_eval(c, gf_exp2!(3)),
          gf_poly_eval(c, gf_exp2!(2)))

  newline()

/*
Lambda(x) = (1+x*2^e1)(1+x*2^e2)(1+x*2^e3)
          = 1 + L1*x + L2*x^2 + L3*x^3

where e1, e2 and e3 are the powers of 2 corresponding to where the
errors are in the corrupted codeword.
*/
  { LET Lambda = VEC 40
    LET d1, a1, b1 = 1, gf_exp2!7, 1 // (1 - x*2^e1)
    LET d2, a2, b2 = 1, gf_exp2!6, 1 // (1 - x*2^e2)
    LET d3, a3, b3 = 1, gf_exp2!4, 1 // (1 - x*2^e3)
    writef("t1= "); pr_poly(@d1)     // 80 01
    writef("t2= "); pr_poly(@d2)     // 40 01
    writef("t3= "); pr_poly(@d3)     // 10 01
    newline()
    gf_poly_mul(@d1, @d2, r)
    //writef("t1xt2= "); pr_poly(r)
    gf_poly_mul(r, @d3, Lambda)
    writef("Lambda= "); pr_poly(Lambda) // 98 1B D0 01
    newline()

    writef("80^-1 = %x2*n", gf_inverse(#x80)) // 1B
    writef("40^-1 = %x2*n", gf_inverse(#x40)) // 36
    writef("10^-1 = %x2*n", gf_inverse(#x10)) // D8
    newline()

    e_pos!0 := -1
    e_pos!1 := 0

    FOR x = 0 TO GFvalmax DO
    { LET a = gf_poly_eval(@d1, x)
      LET b = gf_poly_eval(@d2, x)
      LET c = gf_poly_eval(@d3, x)
      LET prod = gf_mul(gf_mul(a, b), c)
      UNLESS prod=0 LOOP
      //UNLESS x<6 | x=#x1B | x=#x36 | x=#xD8 LOOP
      writef("t1(%x2)=%x2 t2(%x2)=%x2 t3(%x2)=%x2 prod=%x2 L(%x2)=%x2*n",
              x,a,        x,b,         x,c,     prod, x,gf_poly_eval(Lambda,x))
      e_pos!0 := e_pos!0 + 1
      e_pos!(e_pos!0+1) := x
    }
    newline()
    { LET e = TABLE 9, #x00,#x00,#x56,#x78,#x00,#xA7,#x00,#x00,#x00,#x00
      FOR i = 0 TO 5 DO
      { LET x = gf_poly_eval(e, gf_exp2!i)
        writef("E(2^%n) = %x2 = S%n*n", i, x, i)
      }
      newline()
    }
    writef("2^-7= %x2*n", gf_inverse(gf_exp2!7))
    writef("2^-6= %x2*n", gf_inverse(gf_exp2!6))
    writef("2^-4= %x2*n", gf_inverse(gf_exp2!4))

    writef("Lambda(2^-7)= %x2*n", gf_poly_eval(Lambda, gf_inverse(gf_exp2!7)))
    writef("Lambda(2^-6)= %x2*n", gf_poly_eval(Lambda, gf_inverse(gf_exp2!6)))
    writef("Lambda(2^-4)= %x2*n", gf_poly_eval(Lambda, gf_inverse(gf_exp2!4)))
    newline()
/*
    ( 6E ) = ( DE 81 89 ) x ( L1 )
    ( 82 )   ( 6E DE 81 )   ( L2 )
    ( 7A )   ( 82 6E DE )   ( L3 )

    giving L1=D0, L2=1B and L3=98.
*/
    writef("DExD0+81x1B+89x98 = %X2    -- 6E*n",
           gf_mul(#xDE,#xD0) XOR gf_mul(#x81,#x1B) XOR gf_mul(#x89,#x98))
    writef("6ExD0+DEx1B+81x98 = %X2    -- 82*n",
           gf_mul(#x6E,#xD0) XOR gf_mul(#xDE,#x1B) XOR gf_mul(#x81,#x98))
    writef("82xD0+6Ex1B+DEx98 = %X2    -- 7A*n",
           gf_mul(#x82,#xD0) XOR gf_mul(#x6E,#x1B) XOR gf_mul(#xDE,#x98))
  }

  newline()
  find_errata_locator(e_pos, e_loc)  
  writef("e_pos=   "); pr_poly(e_e_pos)
  writef("e_loc=   "); pr_poly(e_loc)
  newline()
//abort(5555)
}

AND rs_find_error_locator(synd, err_loc) BE
{ // This sets err_loc to the error locator polynomial
  // given the syndromes polynomial. It is only used
  // when we do not know the locations of any of the
  // error bytes, so the maximum number of error that
  // can be found is the (synd!0+1)/2. It uses the
  // Berlekamp-Massey algorithm.
  LET old_loc = VEC 50
  LET degs = synd!0
  LET k, l = 1, 0
  LET Lambda = VEC 50  // To hold the error locator polynomial
  LET newL   = VEC 50  // To hold the error locator polynomial
  LET C      = VEC 50  // To hold a correction polynomial
  LET P1     = VEC 50
  LET P2     = VEC 50

  Lambda!0, Lambda!1 := 0, 1  // Polynomial: 1
  C!0, C!1, C!2 := 1, 1, 0    // Polynomial: x+0

  UNTIL k > degs+1 DO // degs+1 = number of correction bytes
  { LET delta = 0 //synd!1 // codeword(2^0)
    LET degL = Lambda!0
    //newline()
    //writef("Lambda: "); pr_poly(Lambda)
    //writef("C:      "); pr_poly(C)
    //writef("synd:   "); pr_poly(synd)
    //writef("k=%n l=%n*n", k, l)

    FOR i = 0 TO l DO
    { LET Li = Lambda!(degL+1-i) // Coeff of x^i in current Lambda
      LET f = synd!(k-i)         // S(2^(k-i))
      LET Lif = gf_mul(Li, f)
      //writef("i=%n delta: %x2*n", i, delta)
      delta := delta XOR Lif
      //writef("i=%n Li=%x2 f=%x2 Lif=%x2 => delta=%x2*n",
      //        i,   Li,    f,    Lif,       delta)
    }
    //writef("delta: %x2*n", delta)

    IF delta DO
    { gf_poly_scale(C, delta, P1)
      //writef("Multiply C by delta=%x2 giving: ", delta); pr_poly(P1)
      gf_poly_add(P1, Lambda, newL)
      //writef("Add L giving newL              "); pr_poly(newL)
      IF 2*l < k DO
      { l := k-l
        gf_poly_scale(Lambda, gf_inverse(delta), C)
        //writef("Since 2xl < k set C = Lambda/delta: "); pr_poly(C)
      }
    }

    // Multiply C by x
    C!0 := C!0 + 1
    C!(C!0+1) := 0
    //writef("Multiply C by x giving: "); pr_poly(C)

    FOR i = 0 TO newL!0+1 DO Lambda!i := newL!i
    //writef("Set new version of Lambda:   "); pr_poly(Lambda)
    k := k+1
  }
  FOR i = 0 TO Lambda!0+1 DO err_loc!i := Lambda!i

  newline()
  writef("*nLambda:          "); pr_poly(Lambda)
  newline()
  FOR i = 15 TO 20 DO
  {// writef("Lambda(2^%i2): %x2*n", i, gf_poly_eval(Lambda, gf_exp2(i)))
  }
}

AND test_rs_find_error_locator() BE
{ LET g = VEC 20
  LET r = VEC 20
  LET c = VEC 20
  LET Lambda = VEC 20
  LET msg = ?

  writef("*nTesting rs_find_error_locator*n")


  //newline()

  // This test case uses GF(2^8)

  msg := TABLE 3, #x12, #x34, #x56, #x78
  gf_generator_poly(6, g)    // Compute the generator polinomial
  rs_encode_msg(msg, 6, r)   // Compute the RS codeword leaving it in r

  newline()
  writef("message:   "); pr_poly(msg)  // 12 34 56
  writef("generator: "); pr_poly(g)    // 01 3F 01 DA 20 E3 26
  writef("codeword:  "); pr_poly(r)    // 12 34 56 78 17 A7 56 4B 78 DD
  FOR i = 0 TO r!0+1 DO c!i := r!i
  c!3 := 0
  c!4 := 0
  c!6 := 0
  writef("corrupted: "); pr_poly(c)    // 12 34 00 00 17 00 56 4B 78 DD
  rs_calc_syndromes(c, 6, r) // Compute the syndromes of polynomial c

  // Follow the worked example in CKP Clarke's BBC paper.
  // To agree with the paper we must use GF(2^4)
  //r := TABLE 3, 15, 3, 4, 12
  writef("syndromes: "); pr_poly(r)

  rs_find_error_locator(r, Lambda)
  writef("Lambda:   "); pr_poly(Lambda)
}

AND rs_find_error_evaluator(synd, Lambda, Omega) BE
{ // Compute the error evaluator polynomial omega.
  // 
  // omega(x) = (synd(x) * Lambda(x)) MOD x^(e+1) // e=n-k
  LET degs = synd!0

  gf_poly_mul(synd, Lambda, Omega)
  writef("synd:            ");  pr_poly(synd)
  writef("lambda:          ");  pr_poly(Lambda)
  writef("synd x Lambda:   "); pr_poly(Omega)
  FOR i = 0 TO degs DO Omega!(i+1) := Omega!(i+1+Omega!0-degs)
  Omega!0 := degs
  writef("Omega:           "); pr_poly(Omega)
}

AND test_rs_find_error_evaluator() BE
{ LET msg  = ?
  LET n    = ?      // Code word length
  LET k    = ?      // Message length
  LET e    = ?      // The number of check bytes  

  LET S       = VEC 100 // For the syndromes polynomial
  AND R       = VEC 100 // For the codeword for msg
  AND C       = VEC 100 // For the corrupted codeword
  AND G       = VEC 100 // For the generator polynomial
  AND Lambda  = VEC 100 // For the erasures polynomial
  AND Ldash   = VEC 100 // For d/dx of Lambda
  AND Omega   = VEC 100 // For the evaluator polynomial
  AND err_pos = VEC 100 // For the error positions

  writef("*nTesting rs_find_error_evaluator*n*n")

  msg := TABLE 15, #x40, #xD2, #x75, #x47, #x76, #x17, #x32, #x06,
                   #x27, #x26, #x96, #xC6, #xC6, #x96, #x70, #xEC

  //msg := TABLE 3, #x12, #x34, #x56, #x78

  k := 16   // Message bytes
  //k := 4   // Message bytes
  e := 6    // correction bytes
  n := k+e  // codeword bytes

  gf_generator_poly(6, G)  // Compute the generator polinomial
  rs_encode_msg(msg, 6, R) // Compute the RS codeword leaving it in R

  newline()
  writef("message:   "); pr_poly(msg) // 12 34 56
  writef("generator: "); pr_poly(G)   // 01 3F 01 DA 20 E3 26
  writef("codeword:  "); pr_poly(R)   // 12 34 56 78 17 A7 56 4B 78 DD
  FOR i = 0 TO R!0+1 DO C!i := R!i
  C!3 := 0
  C!4 := 0
  C!6 := 0
  writef("corrupted: "); pr_poly(C)   // 12 34 00 00 17 00 56 4B 78 DD
  rs_calc_syndromes(C, 6, S)          // syndromes of polynomial c

  writef("syndromes: "); pr_poly(S)

  rs_find_error_locator(S, Lambda)
  writef("Lambda:   "); pr_poly(Lambda)
  rs_find_error_evaluator(S, Lambda, Omega)
  writef("Omega:    "); pr_poly(Omega)
  newline()

writef("Checking Omega*n")
writef("O0 = S0: %x2=%x2*n", Omega!(Omega!0+1), S!(S!0+1))
writef("O1 = S1+S0L1: %x2=%x2*n", Omega!(Omega!0),
       gf_add(S!(S!0), gf_mul(S!(S!0+1),Lambda!(Lambda!0))))
abort(7777)
  writef("Lambda:     "); pr_poly(Lambda)


  { LET p, q = 1, 0
    writef("Lambda:     "); pr_poly(Lambda)

    FOR i = Lambda!0 TO 0 BY -1 DO
    { // Just copy the coefficients of the odd powers of lambda
//writef("p=%i2: ", p)
      IF (i & 1) = 1 DO
      { Ldash!0 := q
        q := q+1
        Ldash!q := Lambda!p
//writef("Ldash!%n = %x2", q+1, Lambda!p)
      }
//newline()
      p := p+1
    }
    writef("Ldash:      "); pr_poly(Ldash)
  }

  err_pos!0 := -1
  FOR i = 0 TO n-1 DO
  { LET a = gf_poly_eval(Lambda, gf_inverse(gf_exp2!i))
    writef("Lambda(2^%i2) = %x2*n", i, a)
    IF a=0 DO
    { LET p = gf_poly_eval(Omega,  gf_inverse(gf_exp2!(i)))
      LET q = gf_poly_eval(Ldash,  gf_inverse(gf_exp2!(2*i)))
      LET j = err_pos!0
      j := j+1
      err_pos!0 := j
      err_pos!(j+1) := i
//writef("setting err_pos  j=%n i=%n*n", j, i)
      writef("Correction: %x2 from %x2/%x2*n",
              gf_mul(gf_div(p, q),gf_exp2!i), p, q)
    }
  }
  //writef("err_pos:     "); pr_poly_decimal(err_pos)
}
