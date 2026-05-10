GET "libhdr"

GLOBAL {
FLT f:200
FLT x
}

LET start() = VALOF
{ LET FLT x : 1 => 5
            : 2 => 6
  LET y = f() + 3
L: 
  RESULTIS 0
  M:
}


AND FLT f() = 2.0
