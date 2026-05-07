GET "mcpl.h"
 
FUN start : =>
  LET args = VEC 50

  IF rdargs("YEAR", args, 50)=0 DO
  { writef "Bad arguments for EASTER\n"
    RETURN 20
  }

  TEST args!0=0

  THEN { writef("Calculating the Easter cycle\n")
         writef("The Easter cycle is %d\n", cycle())
       }

  ELSE { LET year = str2numb(args!0)
         IF 0<=year<=99 DO year +:= 1900
         writef("The date of Easter in %d is %d/%d\n", 
                 year, easter(year)/10, easter(year) MOD 10)
       }

  RETURN 0
.
FUN easter : year =>
  LET a = year MOD 19
  LET b=year/100, c=year MOD 100
  LET d=b/4, e=b MOD 4
  LET f = (b+8)/25
  LET g = (b-f+1)/3
  LET h = (19*a+b-d-g+15) MOD 30
  LET i=c/4, k=c MOD 4
  LET l = (32+2*e+2*i-h-k) MOD 7
  LET m = (a+11*h+22*l)/451
  LET x = h+l-7*m+114
  LET n=x/31, p=x MOD 31
  RETURN 10*(p+1)+n
.
// The following is a debugging version of easter with an obvious cycle
FUN easter1 : year => year MOD 2_718_281
.
MANIFEST Year=1996, K=7654321

FUN cycle : =>
  LET hashdiff = K*easter(Year+1) XOR K*easter(Year)
  
  FOR cycle = 1 TO 6_000_000 DO
  { LET y = Year + cycle + cycle

    hashdiff := hashdiff XOR K*easter(y) XOR K*easter(y+1)

    IF cycle MOD 1_000_000 = 0 DO writef("trying cycle = %9d\n", cycle)

    IF hashdiff=0 DO
    { writef("hashdiff=0 when cycle is %d\n", cycle+1)
      IF iscycle(cycle+1) RETURN cycle+1
    }
  }

  RETURN 0
.
FUN iscycle : cycle =>
  writef("testing cycle = %d\n", cycle)
  FOR i = 0 TO cycle DO
  { UNLESS easter(1996+i)=easter(1996+cycle+i) RETURN FALSE
    IF i MOD 1000000 = 0 DO writef("%d matched so far\n", i+1)
  }
  RETURN TRUE
.
