GET "libhdr"

LET start() = VALOF
{ FOR i = 2 TO 1000 DO
  { LET sum, a, r = 0, i, 0
    UNTIL a=0 DO
    { sum := sum + a MOD 10
      a := a/10
    }
    r := sum MOD 3
    writef("%4i:   ", i)
    TEST isprime(i)
    THEN writef("    prime")
    ELSE writef("not prime")
    writef("   sum = %3i  r = %n  ", sum, r)
    IF r DO writef("prime")
    newline()
abort(1000)

  }

  RESULTIS 0
}

AND isprime(n) = VALOF
{ FOR f = 2 TO n-1 IF n MOD f = 0 RESULTIS FALSE
  RESULTIS TRUE
}
