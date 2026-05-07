GET "libhdr"

//MANIFEST {
//  FLT x = 1
//  y = 2 + x
//}

LET start(FLT z) = VALOF
{ //LET k = z < 10/3
  //LET val = sys(Sys_flt, fl_unmk, 1.0)
  //LET e = result2
  //LET a = k + 2 * ( 3 + 4)
  //writef("%13i  e=%n*n", val, e)
  writef("%13.2e*n", 123.456)
  writef("%13.2e*n", 1.9999)
  writef("%13.2e*n", 0.9999)
  RESULTIS 0
}
