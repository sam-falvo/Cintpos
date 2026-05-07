GET "libhdr"
 
GLOBAL $( tab:200; Xtab:201 $)
 
MANIFEST $( expectedval=200; digits=expectedval*10; Xtabupb=200 $)

LET start() BE
$( LET v1 = VEC 9
   AND v2 = VEC Xtabupb
 
   tab, Xtab := v1, v2
   
   FOR i = 0 TO Xtabupb DO Xtab!i := 0

   FOR i = 1 TO 1000 DO
   $( LET sum, Xsqx10 = 0, ?
      FOR i = 0 TO 9 DO tab!i := 0
      FOR i = 1 TO digits DO $( LET dig = randno(10) - 1
                                tab!dig := tab!dig + 1
                             $)
      FOR i = 0 TO 9 DO $( LET diff = tab!i - expectedval
                           writef("%i3 ", tab!i)
                           sum := sum + diff*diff
                        $)
      Xsqx10 := sum*10 / expectedval
      IF Xsqx10<=Xtabupb DO Xtab!Xsqx10 := Xtab!Xsqx10 + 1
      writef("*n%i3  Sum = %i3 Xsqx10 = %i3*n", i, sum, Xsqx10)
   $)
    
   writes("*nXi Squared Distribution*n")
   FOR i = 0 TO Xtabupb DO
   $( IF i REM 20 = 0 DO writef("*n%i2.0:  ", i/10)
      writef("%i2 ", Xtab!i)
   $)
   newline()
$)
