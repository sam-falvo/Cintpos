SECTION "tstwritef"

GET "libhdr"

GLOBAL { g:ug }

MANIFEST { }

STATIC {}

GLOBAL {}

LET start() = VALOF
{ //LET a = ""
  //LET b = "A"
  //LET c = "AB"
  //LET d = "ABC"
  //LET e = "ABCD"
  //LET f = "ABCDE"
  //LET g = "ABCDEF"
  //LET h = "ABCDEFG"
  //LET i = "ABCDEFGH"
  //LET j = "ABCDEFGHI"
  LET plat = sys(Sys_platform)
  writef("Platform number = %n*n", plat)

  writef("Test new writef formats*n")

  writef("%10i*n", 1234)
  writef("%iA*n", 1234)
  writef("%10.2d*n", 1234)
  writef("%10.2d*n", -1234)
  writef("%10.0d*n", 1234)
  writef("%10.0d*n", -1234)

  writef("%n %n (%f) %n %n*n", 1, 2, "%n %n %n", 3, 4, 5, 6, 7)

  FOR d = 1000 TO 3000 BY 250 DO // Scaled decimal with 3 digit after
                                 // the decimal point
  { writef("Delay for %6.3d seconds*n", d)
    delay(d)
  }

  RESULTIS 0
}

