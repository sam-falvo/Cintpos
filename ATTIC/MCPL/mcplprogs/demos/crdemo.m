GET "mcpl.h"
 
GLOBAL
  locked:Ug, buf, ptr_in, ptr_out, curr_id, ticks,
  tick, server_fn, client_fn, slicer_fn, monitor, wrbuf
 
MANIFEST Bufupb=3, F_put=101, F_get


FUN start : =>
  writes "Monitor simulator entered\n"

  buf := getvec Bufupb
  ptr_in, ptr_out := 0, 0
  locked := FALSE
  ticks := 0
 
  LET v = VEC 4
  v!0 := createco(server_fn, 500)
 
  FOR id = 1 TO 4 DO v!id := createco(client_fn, 500)
 
  LET slicer = createco(slicer_fn, 500)

  callco(slicer, v)
 
  deleteco slicer
  FOR id = 0 TO 4 DO deleteco(v!id)
  freevec buf

  writes "end of test\n"
  RETURN 0
. 
FUN tick : => cowait()
 .
FUN server_fn : =>
{ LET val = monitor F_get
  FOR i = 1 TO 3 DO tick()
  writef("T= %3d %d: Server >>>>>>>%3d\n", ticks, curr_id, val)
  tick()
} REPEAT
.
FUN client_fn : =>
  LET val = curr_id
 
  { FOR i = 1 TO 20 + 10*curr_id DO tick()  // delay depending on the id
    val := val + 100
    writef("T= %3d %d: Client putting  %3d\n", ticks, curr_id, val)
    monitor(F_put, val)
    tick()
  } REPEAT
.
 
FUN slicer_fn : cptrv =>
  UNTIL ticks>200 DO
  { FOR id = 0 TO 4 DO { curr_id := id
                         callco(cptrv!id)
                       }
    ticks++
  }
.
 
 
 
 
 
 
 
FUN monitor : fno, arg =>
{ LET res
 
  tick() REPEATWHILE locked
  // tick()  NOT allowed here
  locked := TRUE
//  writef("T= %3d %d: locked\n", ticks, curr_id)
 
  tick()
  { MATCH fno
    : F_put =>  // put arg into the buffer if there is room
                  tick()
                  IF ptr_in>=ptr_out+Bufupb DO
                  { tick()
                    locked := FALSE
//                    writef("T= %3d %d: unlocked\n", ticks, curr_id)
                    LOOP
                  }
                  tick()
                  buf ! (ptr_in MOD Bufupb) := arg
                  tick()
                  ptr_in := ptr_in + 1
                  writef("T= %3d %d: ++++++ put %3d  buf now: ",
                          ticks, curr_id, arg)
                  wrbuf()
                  EXIT
 
    : F_get =>  // get two values from the buffer if possible
                 tick()
                 IF ptr_out>=ptr_in DO
                 { tick()
                   locked := FALSE
//                 writef("T= %3d %d: unlocked\n", ticks, curr_id)
                   LOOP
                 }
                 tick()
                 res := buf ! (ptr_out MOD Bufupb)
                 tick()
                 ptr_out := ptr_out + 1
                 writef("T= %3d %d: ------ get %3d  buf now: ",
                         ticks, curr_id, res)
                 wrbuf()
                 EXIT

    :        =>  writef("Unknown monitor function %d\n", fno)
                 EXIT
    .
  }
  locked := FALSE
//  writef("T= %3d %d: unlocked\n", ticks, curr_id)
  tick()
  RETURN res
} REPEAT
.
FUN wrbuf : =>
{ FOR i = ptr_out TO ptr_in-1 DO writef(" %3d", buf!(i MOD Bufupb))
  newline()
}
.