/*
This a demonstration of how to write a simple .ppm file file
representing a coloured image with 3 bytes per RGB pixel.

Implemented by Martin Richards (c) 25 Apr 2021

PPM format used is as follows:

P6           The magic number.
<xsize>      A decimal number in ASCII giving the image width in pixels.
<ysize>      A decimal number in ASCII giving the image height in pixels.
255          The red, green and blue colour each have a range 0 to 255.

This is followed by <xsize>*<ysize> pixels each represented by three
bytes in binary for the intensity of the red, green and blue components,
in that order.
*/

GET "libhdr"

LET start() = VALOF
{ LET filename = "junk.ppm"
  LET stdout = output()
  LET tostream = findoutput(filename)
  LET xsize, ysize = 1500, 500
  selectoutput(tostream)

  writef("P6*n")
  writef("%n*n%n*n255*n", xsize, ysize)
  FOR y = 1 TO ysize DO
    FOR x = 1 TO xsize DO
    { LET r = 255 * x / xsize
      LET g = 255 * (xsize-x) / xsize
      LET b = 255 * y / ysize
      binwrch(r)
      binwrch(g)
      binwrch(b)
    }
  newline()
  endstream(tostream)
  RESULTIS 0
}
