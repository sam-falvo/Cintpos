// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "STACK"

GET "libhdr"

LET start() BE
{ LET argv = VEC 5
  LET size = 0

  IF rdargs("size", argv, 5)=0 DO
  { writes("bad argument for STACK*n")
    stop(20)
  }

  IF argv!0=0 DO
  { writef("current stack size is %n*n", cli_defaultstack)
    stop(0)
  }

  size := strtonumb(argv!0)

  IF size<100 DO
  { writes("suggested stack size too small*n")
    stop(20)
  }

  cli_defaultstack := size
}

AND strtonumb(s) = VALOF
{ LET a = 0
  FOR i = 1 TO s%0 DO
  { LET ch = s%i
    UNLESS '0'<=ch<='9' RESULTIS 0
    a := 10*a + ch - '0'
  }
  RESULTIS a
}
