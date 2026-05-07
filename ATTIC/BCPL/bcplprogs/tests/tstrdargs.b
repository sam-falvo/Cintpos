// Test the rdargs function

GET "libhdr"

LET start() = VALOF
{ LET format = "from/a/p,to/k,ver/p,flag/s/p,num/n/p"
  LET x = 1111
  LET argv = VEC 10
  LET y = 2222
  LET z = 3333
  LET rc = ?
  FOR i = 0 TO 10 DO argv!i := #xAAAA_AAAAA

  rc := rdargs(format, argv, 10)


  writef("rdargs format=*"%s*"  argv=%n rc=%n*n*n", format, argv, rc)

  FOR i = -2 TO 12 DO
  { LET x = argv!i
    writef("%i2 %i6: %x8 %n*n", i, argv+i, x, x) 
  }
  newline()
 
  UNLESS rc DO
  { writef("Bad arguments for tstrdargs %s*n", format)
    RESULTIS 0
  }

  TEST argv!0 THEN writef("from/a/p: %s*n", argv!0)
              ELSE writef("from/a/p: %s*n", "<unset>")
  TEST argv!1 THEN writef("to/k:     %s*n", argv!1)
              ELSE writef("to/k:     %s*n", "<unset>")
  TEST argv!2 THEN writef("ver/p:    %s*n", argv!2)
              ELSE writef("ver/p:    %s*n", "<unset>")
  TEST argv!3 THEN writef("flag/s/p: true*n")
              ELSE writef("flag/s/p: false*n")
  TEST argv!4 THEN writef("num/n/p:  %n*n", !argv!4)
              ELSE writef("num/n/p:  %s*n", "<unset>")

  RESULTIS 0
}
