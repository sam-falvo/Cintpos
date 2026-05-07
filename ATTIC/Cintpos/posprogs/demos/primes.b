GET "libhdr"
 
GLOBAL {  count: ug  }
 
//MANIFEST {  upb = 999  }
MANIFEST {  upb = 1000000  }
 
LET start() = VALOF
{  LET isprime = getvec(upb)
   LET bigp = 0
   count := 0

FOR i = 1 TO 10 DO
{   FOR i = 2 TO upb DO isprime!i := TRUE  // Until proved otherwise.
 
   FOR p = 2 TO upb IF isprime!p DO
   {  LET i = p//*p
      UNTIL i>upb DO {  isprime!i := FALSE; i := i + p }
      //out(p)
      bigp := p
   }
}

writef("Largest prime %n*n", bigp)
   writes("*nend of output*n")
   freevec(isprime)
   RESULTIS 0
}
 
AND out(n) BE
{  IF count REM 10 = 0 DO newline()
   writef(" %i3", n)
   count := count + 1
}
