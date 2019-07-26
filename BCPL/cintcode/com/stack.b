// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "STACK"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 5
  LET size = 0

  IF rdargs("size", argv, 5)=0 DO
  { writes("bad argument for STACK*n")
    RESULTIS 20
  }

  UNLESS argv!0 DO
  { LET freewords = stackfree()
    LET used = result2
    
    writef("Current stack size is %n/%n  free words=%n*n",
           used, cli_defaultstack, freewords)
    RESULTIS 0
  }

  IF string_to_number(argv!0) DO size := result2

  IF size<100 DO
  { writes("suggested stack size too small*n")
    RESULTIS 20
  }

  cli_defaultstack := size
  RESULTIS 0
}
