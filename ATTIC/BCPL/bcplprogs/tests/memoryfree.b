/*
This is a program to test the stackfree and memoryfree functions
*/

GET "libhdr"

LET start() = VALOF
{ LET spacev = getvec(50000)

  LET free = stackfree(FALSE)
  LET size = result2
  writef("*nStack usage %n/%n   Free word = %n*n*n", size-free, size, free)

  f(101)

  free := stackfree(TRUE)
  size := result2

  writef("*nStack usage %n/%n   Free word = %n above HWM*n*n", size-free, size, free)


  free := memoryfree()
  size := result2
  writef("*nMemory usage %n/%n   Free word = %n*n", size-free, size, free)

  // Now corrupt the store chain.
  spacev!-1 := -1
  free := memoryfree()
  size := result2
  writef("*nMemory usage %n/%n   Free word = %n*n", size-free, size, free)

  freevec(spacev)
  RESULTIS 0
}

AND f(n) = n=0 -> 1, n+f(n-1)
