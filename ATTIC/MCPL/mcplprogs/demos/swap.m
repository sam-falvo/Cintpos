MODULE swap
 
GET "mcpl.h"


FUN swap
: [x],  [y] => x, y := y, x
.

FUN start : =>
 LET a=123, b=456
 writef("a=%d  b=%d\n", a, b)
 swap(@a, @b)
 writef("a=%d  b=%d\n", a, b)
.
