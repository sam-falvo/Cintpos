GET "libhdr"

LET start() = VALOF
{ LET filename = "fltnumbers"
  LET instream = findinput(filename)
  LET stdin = input()
  LET upb = 4
  LET v = getvec(upb)
  writef("v=%n*n", v)

  FOR i = 0 TO upb DO v!i := #x1000+i

  FOR i = -1 TO (v!-1 & -2) - 2 DO
    writef("%i7: i=%i2 %x8 %12i*n", v+i, i, v!i, v!i) 

  abort(1000)
  freevec(v)
  abort(1001)

  UNLESS instream DO
  { writef("Trouble with file %s*n", filename)
    RESULTIS 0
  }
  selectinput(instream)

  FOR i = 1 TO 20 DO
  { LET n = readn()
    LET x = readflt()
    
    IF n=0 RESULTIS 0

    TEST result2
    THEN { writef("%i2: Bad floating point number*n", n)
           { LET ch = rdch()
             //writef("Skipping ch='%c' %n*n", (ch>=32->ch,'?'), ch)
             IF ch='*n' | ch=endstreamch BREAK
           } REPEAT
         }
    ELSE writef("%i2: Number is %23.6e    %23.6f*n", n, x, x)
  }

  RESULTIS 0
}

