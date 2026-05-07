















GET "libhdr"

LET start() = VALOF
{ writef("*nTesting the FMOD and MODF operations*n")
  FOR x = -20 TO +20 BY 2 DO
    FOR y= -20 TO +20 BY 2 UNLESS y=0 DO
    { LET FLT fx = (FLOAT x) / 10.0
      LET FLT fy = (FLOAT y) / 10.0
      //LET FLT res = sys(Sys_flt, fl_mod, fx, fy)
      LET FLT res = fx #MOD fy
      //LET FLT rem = result2
      //writef("%6.3f #MOD %6.3f => %6.3f remainder %6.3f*n",
      //        fx, fy, res, rem)
      writef("%6.3f #MOD %6.3f => %6.3f*n",
              fx, fy, res)
      abort(1000)
    }
  RESULTIS 0
}
