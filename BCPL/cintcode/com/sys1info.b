h/*
This program output system information about the current
BCPL system.

Implemented by Martin Richards (c) July 2019
*/

GET "libhdr"

LET start() = VALOF
{ LET hostaddrsize = rtn_hostaddsize!rootnode
  LET bitsperword, msb1, allones = 1, 1, 1
  LET ww = 65
  

  UNTIL (msb<<1)=0 DO
    bitsperword, msb, allones := bitsperword+1, msb<<1, allones<<1 | 1

  writef("*nThis version of BCPL is running on a %s ender machine*n",
          (@ww)%0=65 -> "little", "big")

  writef("The BCPL word is %n bits long*n*n", bitsperword)

  writef("Host address size = %n bits*n", hostaddrsize)
  
  RESULTIS 0
}