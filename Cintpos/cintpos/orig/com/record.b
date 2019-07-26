SECTION "RECORD"


GET "libhdr"
//GET "g/clihdr.h"
//GET "g/iohdr.h"
 
MANIFEST $( record_stop = -1 $)

LET start() BE // RECORD [TO] file [NOTIME] | OFF
  $( LET argv = VEC 50
     LET ttab = rtn.tasktab ! rootnode
     LET ctask = - (scb.type ! cli.standardoutput)
     LET ctcb  = ttab ! ctask
     LET cgv = tcb.gbase ! ctcb

     IF rdargs("TO,OFF/S,NOTIME/S",argv,50) = 0 |
       (argv ! 0) ~= 0 ~= (argv ! 1)   |
       (argv ! 2) ~= 0 ~= (argv ! 1)   |
       (argv ! 0) = (argv ! 1) THEN
       $( writes("Bad parameters for RECORD*N")
          stop(return.hard)
       $)

     TEST argv ! 1 ~= 0 THEN // Recording off
       $( LET rtask = cgv ! 204  // COHAND's output_devtaskid
          LET rtcb  = ?

          IF rtask < 0 THEN
            $( writes("Recording wasn't on!*N")
               stop(return.soft)
            $)

          rtcb := ttab ! rtask

          sendpkt(notinuse,rtask,record_stop)

          deletetask(rtask)
          unloadseg((tcb.seglist ! rtcb) ! 3)
       $)

      ELSE

       $( LET rtask, code = ?,?
          LET segl        = VEC 3

          IF cgv ! 204 > 0 THEN
            $( writes("Recording already on!*N")
               stop(return.soft)
            $)


          code := loadseg ( "bin/recordtask" )

          segl ! 0 := 3
          segl ! 1 := (tcb ! tcb.seglist) ! 1
          segl ! 2 := (tcb ! tcb.seglist) ! 2
          segl ! 3 := code

          IF code = 0 THEN
            $( writes("Failed to load task code*N")
               stop(return.hard)
            $)

          rtask := createtask(segl,400,tcb.pri ! ctcb - 100)

          IF rtask = 0 THEN
            $( writes("Failed to make task*N")
               unloadseg(code)
               stop(return.hard)
            $)

          IF sendpkt(notinuse,rtask,?,?,?,argv!0,cgv,
             ctask,currentdir,argv ! 2 = 0) = 0 THEN
            $( writef("Can't open %S for RECORD*N",argv!0)
               deletetask(rtask)
               unloadseg(code)
               stop(return.hard)
            $)
       $)
  $)


