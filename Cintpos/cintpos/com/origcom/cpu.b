|| (C) Copyright 1979 Tripos Research Group
||     University of Cambridge
||     Computer Laboratory

SECTION "cpu"

|| NEEDS "display"

GET "libhdr"

GET "clihdr"

MANIFEST
$( binary.display       = FALSE // TRUE if binary display
                                // FALSE if hex display
   ticks.interval       = 14
$)

GLOBAL
$( display              : ug+0  // rtn(display.value)
   rc                   : ug+1  // reference count
   t                    : ug+2  // time mod ticks.interval
   refc                 : ug+3  // -> incremented word
   q                    : ug+4  // quotient
   r                    : ug+5  // remainder
   av                   : ug+6  // average usage
   cycle                : ug+7  // cycle value
   idleid               : ug+8  // idling task
   mainid               : ug+9  // main task
   timeout              : ug+10 // idle time out
$)

LET start(pkt) BE
// PKT is zero if run under a cli, non-zero if run as a task
$( LET first.time = TRUE
   // if run as a command (under the cli) ...
   IF pkt=0 THEN
   $( LET seglist = VEC 3
      seglist!0, seglist!1, seglist!2, seglist!3 :=
           3, tcb!tcb.seglist!1, 0, cli.module

      // create the idling task
      idleid := createtask(seglist, 100, 1)
      IF idleid=0 THEN error("CPU already running ?")

      seglist!2 := tcb!tcb.seglist!2 // BLIB library
      // create the high priority task
      mainid := createtask(seglist, 100, maxint-1)
      IF mainid=0 THEN
      $( deletetask(idleid)
         error("Createtask failure")
      $)

      // start up the low priority task
      sendpkt(-1, idleid, 0)
      // start up the high priority task
      sendpkt(-1, mainid, idleid)

      // make sure the cli does NOT unload this code segment
      cli.module := 0
      FINISH
   $)

   // return the start up packet
   idleid := pkt!pkt.type
   qpkt(pkt)

   // if this is the low priority cpu task ...
   IF idleid=0 THEN
   $( returncode := returncode+1 REPEATUNTIL testflags(4)
      deletetask(taskid)
      abort(100)
   $)

   // otherwise, this is the high priority task

   // find the address of the word that the idle task
   // is incrementing
   refc := rootnode!rtn.tasktab!idleid!tcb.gbase+[@returncode-@globsize]
   rc := 0
   t  := ticks.interval-2
   av := 0
   cycle := 0
   timeout := -1

   changepri(taskid, maxint)
   changepri(idleid, maxint-1)

   $( !refc := 0
      delay(1)
      update()
      t := [t+1] REM ticks.interval
      IF first.time & t=0 THEN
      $( changepri(idleid, 1)
         changepri(taskid, maxint-1)
         first.time := FALSE
      $)
   $) REPEAT
$)

AND update() BE
$( LET c = !refc
   IF testflags(1) THEN exit()

   TEST t=0
   THEN
   $( IF c>rc THEN rc := c
      IF rc=0 THEN rc := 1
      q := muldiv(rc-av, [binary.display -> bitsperword, 100], rc)
      IF result2>[rc/2] THEN q := q+1
      UNLESS binary.display IF q=1 DO q := 0
      TEST binary.display
      THEN r := [NOT #0]>>[bitsperword-q]
      ELSE
      $( r := 4096*cycle + 256*[q/100] + 16*[q/10 REM 10] + q REM 10
         cycle := [cycle+1] REM 16
      $)
      av := c
      TEST q<=2
      THEN $( display(r)
              IF timeout=-1 THEN timeout := cycle
           $)
      ELSE
      $( display(r)
         timeout := -1
      $)
   $)
   ELSE
   $( av := av*t + c
      av := av/(t+1)
   $)
$)

AND error(message) BE
$( writef("%s failed - %s*n", cli.commandname, message)
   stop(return.severe)
$)

AND exit() BE
$( // clear the display
   display(0)

   // make the idling task self-destruct when next active
   setflags(idleid, 4)

   // make sure it runs
   // note that the highest priority should always
   // be available, as (by convention) it is only
   // used by tasks which never suspend (and that are
   // of short duration!)
   WHILE changepri(idleid, maxint)=0 LOOP

   // delete this task, freeing this code segment
   endtask(tcb!tcb.seglist!3)
$)
