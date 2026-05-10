GET "libhdr"

LET start() = VALOF
{ 
    LET t1 = VEC 2  // t1!0 = secs since Linux epoch
    LET t2 = VEC 2  // t1!1 = microsecs since midnight
    LET t = ?
    LET a = 12345
    LET b = 54321
    LET c = 34567
    
    sys(8, t1)  // get timeofday
    FOR i=1 TO 1_000_000 DO
        muldiv(a, b, c)
    sys(8, t2)  // get timeofday
    t := (t2!0-t1!0) * 1_000_000 + t2!1 - t1!1  // microsecs
    writef("time: %i7 usecs*n", t)
    RESULTIS 0
}
