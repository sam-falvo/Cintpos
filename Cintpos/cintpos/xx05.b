GET "libhdr"

LET start() = VALOF
{ LET FLT fx = 0

  TEST fx = 0.0
  THEN writef("OK*n")
  ELSE writef("Bad %x8*n", fx=0.0)

  RESULTIS 0
}
