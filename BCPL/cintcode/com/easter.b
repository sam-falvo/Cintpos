GET "libhdr"

LET isleap(year) = year MOD 400 = 0 -> TRUE,
                   year MOD 100 = 0 -> FALSE,
                   year MOD   4 = 0 -> TRUE,
                                       FALSE

LET start() = VALOF
{ LET args = VEC 50
  LET year = 0
  
  IF rdargs("YEAR/N,CYCLE/S", args, 50)=0 DO
  { writef("Bad arguments for EASTER*n")
    RESULTIS 20
  }

  IF args!1 DO                     // CYCLE/S
  { writef("Calculating the Easter cycle*n")
    writef("The Easter cycle is %n*n", cycle())
    RESULTIS 0
  }

  TEST args!0               // YEAR/N
  THEN { year := !args!0
        }
  ELSE { LET days, msecs, dummy = 0, 0, 0
         datstamp(@days)
         year := 1970

         { // Loop to get year
           LET yearlen = isleap(year) -> 366, 365
           IF days <= yearlen BREAK
           days, year := days - yearlen, year + 1
         } REPEAT
       }
         
  IF 0<=year<=99 DO year := year+2000
  FOR y = year TO year+9 DO
    writef("The date of Easter in %n is %n/%n*n", 
           y, easter(y)/10, easter(y) MOD 10)

  RESULTIS 0
}

AND easter(year) = VALOF
{ LET a    = year MOD 19
  LET b, c = year/100, year MOD 100
  LET d, e = b/4, b MOD 4
  LET f    = (b+8)/25
  LET g    = (b-f+1)/3
  LET h    = (19*a+b-d-g+15) MOD 30
  LET i, k = c/4, c MOD 4
  LET l    = (32+2*e+2*i-h-k) MOD 7
  LET m    = (a+11*h+22*l)/451
  LET x    = h+l-7*m+114
  LET n, p = x/31, x MOD 31
  RESULTIS 10*(p+1)+n
}

// The following is a debugging version of easter with an obvious cycle
AND easter1(year) = year MOD 2_718_281

AND cycle() = VALOF
{ MANIFEST { year=1996; K=7654321 }

  LET hashdiff = K*easter(year+1) XOR K*easter(year)
  
  FOR cycle = 1 TO 6_000_000 DO
  { LET y = year + cycle + cycle

    hashdiff := hashdiff XOR K*easter(y) XOR K*easter(y+1)

    IF cycle MOD 1_000_000 = 0 DO writef("trying cycle = %i9*n", cycle)
    IF hashdiff=0 DO
    { writef("hashdiff=0 when cycle is %n*n", cycle+1)
      IF iscycle(cycle+1) RESULTIS cycle+1
    }
    
  }

  RESULTIS 0
}

AND iscycle(cycle) = VALOF
{ writef("testing cycle = %n*n", cycle)
  FOR i = 0 TO cycle DO
  { UNLESS easter(1996+i)=easter(1996+cycle+i) RESULTIS FALSE
    IF i MOD 1000000 = 0 DO writef("%n matched so far*n", i+1)
  }
  RESULTIS TRUE
}
