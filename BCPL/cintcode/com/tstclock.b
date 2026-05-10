GET "libhdr"

LET start() = VALOF
{ LET msecs1 = rtn_msecs!rootnode
  LET msecs2 = 0
  LET count1m = instrcount(f, -2, 1_000_000)
  LET p = 0
  LET v = VEC 100

writef("count1m=%n*n",count1m)
  FOR i = 0 TO 15 DO
  { LET j = 1
    msecs2, j := rtn_msecs!rootnode, j+1 REPEATWHILE msecs1=msecs2 & j<10_000_000
    IF i>5 DO
    { p := p+1
      v!p := muldiv(count1m,j,1_000_000)
      p := p+1
      v!p := rtn_icountmax!rootnode
      p := p+1
      v!p := msecs2-msecs1
    }
    msecs1 := msecs2
  }
  
  FOR i = 1 TO p BY 3 DO
    writef("%i9: icountmax=%i9  increment = %i4 msecs*n", v!i, v!(i+1), v!(i+2))
    
   RESULTIS 0
}

AND f1(msecs1, lim) = VALOF WHILE lim DO lim :=lim-1

AND f(msecs1, lim) = VALOF
{ LET msecs2, j = 0, 0
  msecs2, j := rtn_msecs!rootnode, j+1 REPEATWHILE msecs1=msecs1 & j<lim
writef("f: msecs1=%n lim=%n => j=%n*n", msecs1, lim, j)
  RESULTIS j
}
