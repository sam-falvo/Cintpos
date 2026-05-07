GET "mcpl.h"

FUN buf :  =>                 // Body of buf1, buf2 and buf3
  LET val = cowait()
  LET p=0, q=0
  LET v = VEC 200

  { // val = 0  means extract value from the buffer
    // val~= 0  means push value into the buffer
    TEST val THEN { IF p=q+201 DO writef "Buffer full\n"
                    v!(p++ MOD 201) := val
                  }
             ELSE { IF p=q DO writef "Buffer empty\n"
                    val := v!(q++ MOD 201)
                  }
    val := cowait val
  } REPEAT
.
FUN tee : [a1, a2] =>         // Body of tee1 and tee2
  LET in=a1, out=a2
  cowait()                    // End of initialisation.

  { LET val = callco(in, 0)
    callco(out, val)
    cowait val
  } REPEAT
.
FUN mul : [a1, a2] =>         // Body of x2, x3 and x5
  LET k=a1, in=a2
  cowait()                    // End of initialisation.
   
  cowait(k * callco(in, 0)) REPEAT
.
FUN merge : [a1, a2] =>       // Body of mer1 and mer2
  LET inx=a1, iny=a2
  cowait()                    // End of initialisation

  LET x=0, y=0, min=0

  { IF x=min DO x := callco(inx, 0)
    IF y=min DO y := callco(iny, 0)
    min := x<y -> x, y
    cowait min
  } REPEAT
.
FUN start : =>
  LET buf1 = initco(buf,   500)
  LET buf2 = initco(buf,   500)
  LET buf3 = initco(buf,   500)
  LET tee1 = initco(tee,   100, buf1, buf2)
  LET tee2 = initco(tee,   100, buf2, buf3)
  LET x2   = initco(mul,   100, 2,    tee1)
  LET x3   = initco(mul,   100, 3,    tee2)
  LET x5   = initco(mul,   100, 5,    buf3)
  LET mer1 = initco(merge, 100, x2,   x3)
  LET mer2 = initco(merge, 100, mer1, x5)

  LET val = 1   
  FOR i = 1 TO 100 DO { writef(" %6d", val)
                        UNLESS i MOD 10 DO newline()
                        callco(buf1, val)  // Push val into buf1
                        val := callco mer2
                      }

  deleteco buf1; deleteco buf2; deleteco buf3
  deleteco tee1; deleteco tee2
  deleteco x2;   deleteco x3;   deleteco x5
  deleteco mer1; deleteco mer2
.
