/*
The Cintpos Task and Coroutine Benchmark

############### Still undergoing minor modification #################

This is one of the benchmark programs freely available via my Tcobench
distribution (tcobench.tgz or tcobench.zip) from my home page
(www.cl.cam.ac.uk/users/mr10).

Re-implemented by Martin Richards (c) September 2015.

This benchmark is designed to demonstrate that Cintpos tasks using
coroutines can be used effectively to implement process control style
applications. It is a complete rewrite of an earlier version written
in 2004.

It is designed to be a language independent benchmark and will be
translated into other languages, such as C or Java, for
comparison. One such translation is in java-colib/Tcobench.java of
this Tcobench distribution, but this is in the early stages of
development.

The program creates several Cintpos tasks (equivalent to threads) that
run under the control of the Cintpos scheduler. The task share the
same address space but have their own run time stacks and global
vectors. The program performs a long sequence of operations involving
task to task and coroutine to coroutine communication and occasional
real time delays.

There are several write client tasks that send data to write server
tasks.  The server tasks have several worker coroutines to process
these these requests. The worker coroutines pass the data on to
multiplexor tasks that each control of a number of channels used to
pass the data to the reading half of the benchmark program. Each
channel has a buffer holding data waiting to be transferred. When the
buffer is full, a write request to that channel fails and the
multiplexor reports this to its server which, in turn, reports it to
the write client. After a short delay, the write client tries sending
data via a different server-multiplexor-channel route.

The reading half of the benchmark contains several read client tasks
that send requests to read server tasks. Each read server has several
worker coroutines to process these requests. These worker coroutines
send read requests to specified multiplexor tasks to obtain data from
specified channels.  If the channel buffer is empty, the read request
fails and the multiplexor reports this to its server which, in turn,
reports it to the read client. After a short delay, the read client
tries sending another read request via a different
server-multiplexor-channel route.

Each read and write client repeatedly creates and performs schedules
of work items invoking each possible server-multiplex-channel path.
If there are s servers, m multiplexors and c channels, there will be
s*m*c items in each schedule. One of the items is marked to cause a
real time delay after the client has successfully processed the
request, another item is marked to cause a similar delay in a worker
coroutine of its server after successfully processing the request, and
a third item causes a delay after its specified channel has been
successfully read from or written to. The items in the schedules are
processed by clients in random order.

The clients are synchronised so that none can create and process its
next schedule until all the other read and write clients have
completed their previous schedules.

Each server has worker coroutines to process requests and one logger
coroutine. When a server receives a request it passes it to a worker
for processing, but there is the constraint that the busiest worker
must not process more than 5 requests more than the least busy
worker. This constraint is included to demonstatrate the use of
condition variables and, in particular, the functions condwait, notify
and notifyAll. After a worker coroutine has successfully processed a
request it sends two random numbers to the logger coroutine.  These
numbers are sent separately and so mutex style locking and unlocking
is necessary, but since the locking occurs between coroutines its
implementation is efficient. To check that the locking mechanism
works, the logger coroutines returns the sum of two numbers to the
originating worker for checking. The communication between worker
coroutines and the logger is by means of coroutine based Occum style
channels using the functions cowrite and coread.

The program creates one low priority printer task, and each logger
occasionally sends a message to the printer task. It also occasionally
delays for a short period. This allows other tasks within the
benchmark to gain control, helping to demonstate that the locking
mechanism works properly.

Condition variables are also used by worker coroutines when waiting
for request packets to process.

Each multiplexor task has one read and one write coroutine per channel
to process multiplexor requests. If a write request is received when
its channel buffer is full, it is immediately returned to the server
with an indication of failure. Otherwise the data is copied into the
channel's buffer and after a possible delay the request packet is
returned to the server.  If a read request find that the specified
channel buffer is empty, the packet is immediately returned to its
server with an indication of failure. The server then returns the
packet to is client which after a short delay sends another request
normally involving a different server-multiplexor-channel route.

If a read or write request specifies a multiplexor delay, only the
appropriate channel coroutine is held up. Requests involving other
channels are not affected.

The stats task performs various jobs. One is to receive and accumulate
data about the frequency of various operations such as calls of qpkt,
taskwait, callco and cowait. It also performs synchronisation
operations involving sync, rddone and wrdone packets. Thirdly it
continually measures the CPU utilisation by counting how often it can
bounce a packet off the bounce task in each 100 msec period. Since the
bounce task has the lowest priority, it can only return a packet when
all the other tcobench tasks are suspended.

At the end of the run, the stats task outputs a summary of the
statistics it has gathered.  Typical output is the following.

0.000 1> tcobench

Thread and Coroutine Benchmark

loopmax    =    2 (k)
climax     =   20 (n)
srvmax     =   15 (s)
workmax    =   14 (w)
mpxmax     =   10 (m)
chnmax     =   10 (c)
chnbufsize =   35 (b)
delaymsecs =  500 (d)

Requests per schedule = 1500
maxcountdiff          =    5



Start  time:  05-Oct-2015 09:36:00

Finish time:  05-Oct-2015 09:36:35

All clients have finished their work

Number of calls of qpkt:            7077796
Number of calls of taskwait:        7073234
Number of calls of callco:         34771561
Number of calls of cowait:         34771561
Number of calls of resumeco:         172311
Number of calls of condwait(..):     268106
Number of calls of notify(..):       124024
Number of calls of notifyAll(..):      8524
Number of increments:                120000
   increment had to wait:             27268
Number of calls of lock(..):         120000
   lock had to wait:                  33111
Number of  500 msec delays:             240
Print task counter:                    2400
Calls to logger:                     120000
Send fail count:                       3185
Read fail count:                        839
Bounce task counter:                3276007
Read checksum:                       811018
Write checksum:                      811018
Read count:                           60000
Write count:                          60000

     Approximate CPU utilisation over 339 periods of 100 msecs

  0-10% 10-20% 20-30% 30-40% 40-50% 50-60% 60-70% 70-80% 80-90% 90-100%
   82    204     51      0      1      1      0      0      0      0  

Tcobench completed
35.440 1> 
*/

SECTION "TCOBENCH"

GET "libhdr"

MANIFEST {
  // Packet types
  act_startstats=100      // Controller to stats task     
  act_startrdclient       // Controller to client to start it reading
  act_startwrclient       // Controller to client to start it writing
  act_startrdserver       // Controller to server
  act_startwrserver       // Controller to server
  act_startmpx            // Controller to mpx
  act_startbounce         // Controller to bounce task
  act_startprinter        // Controller to printer task
  act_clock               // Stats task to clock device
  act_bounce              // Stats task to bounce task
  act_print               // Server to printer task
  act_read                // A read request from a client or server
  act_write               // A write request from a client or server
  act_sync                // Client to stats task to synchronise with other clients
  act_rddone              // Read client to stats task to synchronise end of run.
  act_wrdone              // Write client to stats task to synchronise end of run.
  act_die                 // Controller to most tasks to cause them
                          // to return to DEAD state ready for deletion
  act_calibrate           // Controller to the stats task
  act_run                 // Controller to stats task to release
                          // the clients. It is returned when all
                          // clients have completed all their schedules.
  act_addstats            // Send statistics counts to the stats task
  act_prstats             // Output the accumulated statistics

  // Positions of the counters in the statistics vector.
  // These are in the same order as those in the global vector.
  p_qpkt=0
  p_taskwait
  p_callco
  p_resumeco
  p_cowait
  p_condwait
  p_notify
  p_notifyAll
  p_inc
  p_incwait
  p_lock
  p_lockw
  p_delaylong
  p_bounce
  p_print
  p_logger
  p_readfail
  p_sendfail

  p_rdchecksum
  p_wrchecksum
  p_rdcount
  p_wrcount

  statvupb = p_wrcount-1
}

STATIC {
  // These static quantites are shared by all tasks in the benchmark,
  // once initialised these variables remain constant.

  // The use of STATIC variables is bad practice. It would be better
  // to give these values to each tcobench task that needs them when
  // they start up.          

  // Each task has its own set of global variables. One task cannot
  // access the global variables of another task.

  rdclientv        = 0   // For read client task ids
  wrclientv        = 0   // For write client task ids
  rdserverv        = 0   // For read server task ids
  wrserverv        = 0   // For write server task ids
  mpxv             = 0   // For multiplexor task ids
  controllertaskid = 0   // To hold the controller task id
  statstaskid      = 0   // To hold the stats task id
  bouncetaskid     = 0   // To hold the bounce task id
  printertaskid    = 0   // To hold the printer task id

  // Default settings
  loopmax      =   4
  climax       =  20 // Upb of rdclintv and wrclientv
  srvmax       =  15 // Upb of rdserverv and wrserverv
  workmax      =  14 // Number of work coroutines per server
  mpxmax       =  10 // Number of mpx tasks
  chnmax       =  10 // Number of buffers per mpx task
  chnbufsize   =  35 // The size of each channel buffer
  delaymsecs   = 500 // Delay time for read and write delay requests
  maxcountdiff =   5 // maximum allowable value of wrkcount-minwrkcount

  requestvupb  = 15*10*10   // Number of requests per schedule

  tracing      = FALSE
}

GLOBAL {  // Globals belonging to each task

  // Each task has its own set of global variables that other tasks
  // cannot access. Note that the counter c_qpkt of how many times
  // this task call qpkt can be incremented efficiently. Such counters
  // are sent to the stats task at the end of the run where they are
  // accumulated and output.

  c_qpkt:ug     // These must be in the same order as the p_ declarations
  c_taskwait
  c_callco
  c_resumeco
  c_cowait
  c_condwait
  c_notify
  c_notifyAll
  c_inc
  c_incwait
  c_lock
  c_lockw
  c_delaylong
  c_bounce
  c_print
  c_logger
  c_readfail
  c_sendfail
  c_rdchecksum
  c_wrchecksum
  c_rdcount
  c_wrcount

  modech        // Mode of a client or server, 'R' or 'W'.

  pktlist
  gomultievent
  multi_done    // Used to cause gomultievent to return
                // to single event mode
  mainco_ready  // If this is FALSE gomultievent puts new request packets
                // in a queue rather than giving them to the main coroutine.

  startuppkt    // The startup packet received by servers and mpx tasks
                // from the controller task. Not returned until the server
                // or mpx task is fully initialised and ready to process
                // requests.
  diepkt        // Sent by the controller to cause tasks to return to DEAD
                // state ready for deletion.

  condwait      // Function to wait on a condition variable
                // eg condwait(@countcondvar)
  notify        // Wakeup first coroutine waiting on a condition variable
                // eg notify(@pktcondvar)
  notifyAll     // Wakeup all coroutines waiting on a condition variable
                // eg notifyAll(@countwaitlist)

  lock          // eg lock(@loggerlock)
  unlock        // eg unlock(@loggerlock)

  startstats
  startbounce
  startprinter
  startclient
  startserver
  startmpx

  // Client task globals
  clino         // Number of this client

  // Server task globals
  serno         // Number of this server
  serverq       // Queue of packet waiting to be processed by
                // worker coroutines. Servers place packets in this
                // list when all worker coroutines are busy.
  busycount     // Number of server worker coroutines currently
                // processing requests.
  wrkcov        // Vector of this server's worker coroutines
  countv        // Vector of this server's worker workcounts
                // ie the number of requests successfully processed
                // by each worker.

  minwrkcount   // The work count of the this server's least busy worker

  countcondvar  // The condition variable for condition:
                //   wrkcount-minwrkcount <= maxcountdiff | diepkt

  pktcondvar    // condition variable used by work coroutines waiting
                // for a request packet.

  loggerco      // The logger coroutine for this server task
  loggerlock    // List of coroutines waiting for the logger

  loggerin      // Occam style channel for logger input
  loggerout     // Occam style channel for logger output

  // Mpx task globals
  mpxno         // Number of this mpx task
  mpxrdcov      // Vector of multiplexor channel read coroutines
  mpxwrcov      // Vector of multiplexor channel write coroutines
  rdbusyv       // rdbusyv!chnno is TRUE if the read coroutine
                // for channel chnno is busy.
  wrbusyv       // wrbusyv!chnno is TRUE if the write coroutine
                // for channel chnno is busy.
  rdwkqv        // rdwkqv!chnno is a list of pending mpx read packets
                // for channel chnno.
  wrwkqv        // wrwkqv!chnno is a list of pending mpx write packets
                // for channel chnno.
  bufv          // bufv!chnno is the mpx circular buffer for its channel.
                // These buffers each have climax*srvmax+1 elements.
  bufpv         // bufpv!chnno is the subscript into bufv!chnno where
                // the next write data will be written.
  bufqv         // bufqv!i is the subscript into bufv!i where the next
                // data value will be read from.
                // If bufpv!i=bufqv!i, there is no pending write data
                // within the mpx task for client i.
  mpxbusycount  // Count of the number of channel read and write
                // coroutines that are busy. 

  // Language independent random number generator

  seed          // Seed for the machine/language independent
  nextrnd       // random number generator.
  setseed
}

LET start(pkt) = VALOF
{ // If pkt=0, this code is being run as the Cintpos command that
  // creates, runs and finally deletes all the tasks of the benchmark.
  // If pkt is non zero it is the startup packet for one of the
  // benchmark tasks. The tasks are:

  //     the  controller task is the main program of the benchmark
  //     the  stats task
  //     the  bounce task
  //     the  printer task
  //     n    read client tasks
  //     n    write client tasks
  //     s    read server tasks
  //     s    write server tasks
  //     m    multiplexor tasks
  //     c    channels per multiplexor task

  // The main program runs as the controller task. It creates
  // all the other tasks and lets them run. On completion the
  // controller deletes all its tasks and workspace it created
  // before exiting to the CLI.

  // Comment out the next line to test the nextrnd function.
  IF FALSE DO
  { LET i = 0
    // Find the period of the random number feedback shift register.
    // It should be 2^32-1 = #xFFFFFFFF.
    seed := 1
    { i := i+1
      nextrnd()
      IF seed=1 DO
      { sawritef("Random number cycle length = %n %x8*n", i, i)
        RESULTIS 0
      }
      IF (i & #xFFFFFF) = 0 DO
        sawritef("i = %x8*n", i) 
    } REPEAT
  }

  initstats()  // Initialise this task's statistics counters.
               // Bear in mind this might be any one of the
               // tcobench tasks.

  UNLESS pkt DO
  { // Start is being called from the CLI.
    tcobenchcom()  // Execute the tcobench CLI command.
                   // This will be the controller task.
    RESULTIS 0     // Return to the CLI.
  }

  // Start is also the main function of every tcobench task,
  // and pkt is the startup packet.
  SWITCHON pkt_type!pkt INTO
  { DEFAULT:
      sawritef("T%z2 Controller:*
               *   System Error: Unexpected packet from %n type %n*n",
                taskid, pkt!pkt_id, pkt!pkt_type)
      abort(999)
      c_qpkt := c_qpkt+1
      qpkt(pkt)
      RESULTIS 0

    CASE act_startstats:    startstats(pkt);   ENDCASE
    CASE act_startbounce:   startbounce(pkt);  ENDCASE
    CASE act_startprinter:  startprinter(pkt); ENDCASE
    CASE act_startrdclient: startclient(pkt);  ENDCASE
    CASE act_startwrclient: startclient(pkt);  ENDCASE
    CASE act_startrdserver: startserver(pkt);  ENDCASE
    CASE act_startwrserver: startserver(pkt);  ENDCASE
    CASE act_startmpx:      startmpx(pkt);     ENDCASE
  }

  RESULTIS 0
}

AND initstats() BE
{ // Initialize this tasks statistics counters
  c_qpkt        := 0
  c_taskwait    := 0
  c_callco      := 0
  c_resumeco    := 0
  c_cowait      := 0
  c_condwait    := 0
  c_notify      := 0
  c_notifyAll   := 0
  c_inc         := 0
  c_incwait     := 0
  c_lock        := 0
  c_lockw       := 0
  c_delaylong   := 0
  c_bounce      := 0
  c_print       := 0
  c_logger      := 0
  c_readfail    := 0
  c_sendfail    := 0
  c_rdchecksum  := 0
  c_wrchecksum  := 0
  c_rdcount     := 0
  c_wrcount     := 0
}

AND sendstats() BE
{ // Send this tasks statistics to the stats task
  LET sv = @c_qpkt
  sendpkt(notinuse, statstaskid, act_addstats, 0,0, sv)
}

AND prstats() BE
{ // Cause the stats task to print the accumulated statistics.
  sendpkt(notinuse, statstaskid, act_prstats)
}

AND tcobenchcom() BE
{ // This performs the tcobench CLI command.
  // It is the body of the controller task

  // Build a segment list containing KLIB, BLIB and this module.
  LET upb,              klib,              blib,     module =
        3, rootnode!rtn_klib, rootnode!rtn_blib, cli_module
  LET segv = @upb
  // segv is used in all the calls of createtask below.

  LET argv = VEC 100

  UNLESS rdargs("-k/N,-n/N,-s/N,-w/N,-m/N,-c/N,-b/N,-d/N,*
                *-t/S,-x/S,-y/S,-z/S",
                argv, 100) DO
  { sawritef("Bad arguments for TCOBENCH*n")
    RETURN
  }

  // The default parameter
  loopmax    :=   2
  climax     :=  20
  srvmax     :=  15
  workmax    :=  14
  mpxmax     :=  10
  chnmax     :=  10
  chnbufsize :=   0  // Will be set later

  delaymsecs := 500

  tracing := argv!8                  // -t/S

  IF argv!9 DO                       // -x/S
  { // Alternate setting of the parameters
    loopmax    :=   1
    climax     :=   2
    srvmax     :=   2
    workmax    :=   3
    mpxmax     :=   2
    chnmax     :=   3

    loopmax    :=   1
    climax     :=   1
    srvmax     :=   1
    workmax    :=   1
    mpxmax     :=   1
    chnmax     :=   1
  }

  IF argv!10 DO                      // -y/S
  { // Alternate setting of the parameters
    loopmax    :=   2
    climax     :=   5
    srvmax     :=   3
    workmax    :=   3
    mpxmax     :=   2
    chnmax     :=   3
  }

  IF argv!11 DO                      // -z/S
  { // Alternate setting of the parameters
    loopmax    :=   3
    climax     :=  10
    srvmax     :=   4
    workmax    :=   7
    mpxmax     :=   3
    chnmax     :=   4
  }

  // Conditionally override the default or alternate settings
  IF argv!0 DO loopmax    := !argv!0 // -k/N
  IF argv!1 DO climax     := !argv!1 // -n/N
  IF argv!2 DO srvmax     := !argv!2 // -s/N
  IF argv!3 DO workmax    := !argv!3 // -w/N
  IF argv!4 DO mpxmax     := !argv!4 // -m/N
  IF argv!5 DO chnmax     := !argv!5 // -c/N
  IF argv!6 DO chnbufsize := !argv!6 // -b/N
  IF argv!7 DO delaymsecs := !argv!7 // -d/N

  requestvupb := srvmax*mpxmax*chnmax
  UNLESS chnbufsize DO
  { // Set the channel buffer size to about one tenth of the
    // number of values sent to each channel on each iteration.
    chnbufsize := (climax*srvmax)/10 + 5
  }

  sawritef("*nThread and Coroutine Benchmark*n*n")
  sawritef("loopmax    = %i4 (k)*n", loopmax)
  sawritef("climax     = %i4 (n)*n", climax)
  sawritef("srvmax     = %i4 (s)*n", srvmax)
  sawritef("workmax    = %i4 (w)*n", workmax)
  sawritef("mpxmax     = %i4 (m)*n", mpxmax)
  sawritef("chnmax     = %i4 (c)*n", chnmax)
  sawritef("chnbufsize = %i4 (b)*n", chnbufsize)
  sawritef("delaymsecs = %i4 (d)*n", delaymsecs)
  sawrch('*n')
  sawritef("Requests per schedule = %i4*n",   requestvupb)

  sawritef("maxcountdiff          = %i4*n",   maxcountdiff)

  sawrch('*n')

  rdclientv := getvec(climax)
  wrclientv := getvec(climax)
  rdserverv := getvec(srvmax)
  wrserverv := getvec(srvmax)
  mpxv      := getvec(mpxmax)

  IF rdclientv FOR i = 0 TO climax DO rdclientv!i := 0
  IF wrclientv FOR i = 0 TO climax DO wrclientv!i := 0
  IF rdserverv FOR i = 0 TO srvmax DO rdserverv!i := 0
  IF wrserverv FOR i = 0 TO srvmax DO wrserverv!i := 0
  IF mpxv FOR i = 0 TO mpxmax DO mpxv!i := 0

  bouncetaskid     := 0
  printertaskid    := 0
  statstaskid      := 0

  // This test is delayed until it is safe to jump to fin.
  UNLESS rdclientv & wrclientv &
         rdserverv & wrserverv &
         mpxv DO
  { sawritef("More memory needed*n")
    GOTO fin
  }

  // The main cli task is the controller task for the benchmark
  controllertaskid := taskid

  // We now create all the tcobench tasks.
  c_taskwait := c_taskwait+1
  statstaskid := createtask(segv, 1000, 9000)   // The highest priority
  UNLESS statstaskid DO
  { sawritef("Unable to create the stats task*n")
    abort(999)
  }
  IF tracing DO sawritef("T%z2 Stats:   Task created*n", taskid)

  c_taskwait := c_taskwait+1
  bouncetaskid := createtask(segv, 1000, 10)   // Very low priority
  UNLESS bouncetaskid DO
  { sawritef("Unable to create the bounce task*n")
    abort(999)
  }
  IF tracing DO sawritef("T%z2 Bounce:  Task created*n", taskid)

  c_taskwait := c_taskwait+1
  printertaskid := createtask(segv, 1000, 11) // Priority just greater than
  UNLESS printertaskid DO                     // that of the bounce task
  { sawritef("Unable to create the printer task*n")
    abort(999)
  }
  IF tracing DO sawritef("T%z2 Printer: Task created*n", taskid)

  FOR clino = 1 TO climax DO
  { LET id = createtask(segv, 1000, 4000+clino)
    c_taskwait := c_taskwait+1
    UNLESS id DO
    { sawritef("Unable to create RC%z2*n", clino)
      abort(999)
    }
    rdclientv!clino := id
    IF tracing DO sawritef("T%z2 RC%z2:    Task created*n", taskid, clino)

    id := createtask(segv, 1000, 5000+clino)
    c_taskwait := c_taskwait+1
    UNLESS id DO
    { sawritef("Unable to create WC%z2*n", clino)
      abort(999)
    }
    wrclientv!clino := id
    IF tracing DO sawritef("T%z2 WC%z2:    Task created*n", taskid, clino)
  }

  FOR serno = 1 TO srvmax DO
  { LET id = createtask(segv, 1000, 6000+serno) // Priorities higher than the
                                                // read and write clients
    c_taskwait := c_taskwait+1
    UNLESS id DO
    { sawritef("Unable to create RS%z2*n", serno)
      abort(999)
    }
    rdserverv!serno := id
    IF tracing DO sawritef("T%z2 RS%z2:    Task created*n", taskid, serno)
  }

  FOR serno = 1 TO srvmax DO
  { LET id = createtask(segv, 1000, 7000+serno) // Priorities higher than
                                                // read servers
    c_taskwait := c_taskwait+1
    UNLESS id DO
    { sawritef("Unable to create WS%z2*n", serno)
      abort(999)
    }
    wrserverv!serno := id
    IF tracing DO sawritef("T%z2 WS%x2:    Task created*n", taskid, serno)
  }

  FOR mpxno = 1 TO mpxmax DO
  { LET id = createtask(segv, 1000, 8000+mpxno) // Even higher priority
    c_taskwait := c_taskwait+1
    UNLESS id DO
    { sawritef("Unable to create M%z2*n", mpxno)
      abort(999)
    }
    mpxv!mpxno := id
    IF tracing DO
      sawritef("T%z2 M%z2:     Task created*n", taskid, mpxno)
  }

  // All tcobench tasks have been created but left in DEAD state

  sawrch('*n')

  // Start up all the tcobench tasks.

  sendpkt(notinuse, statstaskid,   act_startstats)
  sendpkt(notinuse, bouncetaskid,  act_startbounce)
  sendpkt(notinuse, printertaskid, act_startprinter)

  // Send startup packets to all the multiplexor tasks
  FOR mpxno = 1 TO mpxmax DO
    sendpkt(notinuse, mpxv!mpxno, act_startmpx, 0, 0, mpxno)

  // Send startup packets to all the read server tasks
  FOR serno = 1 TO srvmax DO
    sendpkt(notinuse, rdserverv!serno, act_startrdserver, 0, 0, serno)

  // Send startup packets to all the write server tasks
  FOR serno = 1 TO srvmax DO
    sendpkt(notinuse, wrserverv!serno, act_startwrserver, 0, 0, serno)

  // Send startup packets to all the read client tasks
  FOR clino = 1 TO climax DO
    sendpkt(notinuse, rdclientv!clino, act_startrdclient, 0, 0, clino)

  // Send startup packets to all the write client tasks
  FOR clino = 1 TO climax DO
    sendpkt(notinuse, wrclientv!clino, act_startwrclient, 0, 0, clino)

  // All the tcobench tasks have been created and started, but the
  // client tasks are now waiting to run their first schedules of work.
  // The servers and multiplexors will be waiting for their first
  // request packets.

  // Perform calibration before the clients start their work.
  IF tracing DO
    sawritef("T%z2 Controller:*
             * Sending a calibrate packet to the stats task*n",
             taskid)
  sendpkt(notinuse, statstaskid, act_calibrate)

  // Finally the controller sends a run packet to the stats task to
  // release all the clients. This packet is only returned to the
  // controller when all clients have finished all their schedules.
  // It is important that calibration is done before sending the run
  // packet to the stats task.

  IF tracing DO
    sawritef("T%z2 Controller:*
             * Sending run packet to the stats task*n",
             taskid)

  wrtime("*nStart  time: ")
  sendpkt(notinuse, statstaskid, act_run)
  wrtime("*nFinish time: ")

  sawritef("*nAll clients have finished their work*n")

  // Send die packets to all clients, servers and multiplexor tasks,
  // and the bounce and printer tasks.

  // Send die packets to all the write client tasks
  FOR clino = 1 TO climax DO
    sendpkt(notinuse, wrclientv!clino, act_die)

  // Send die packets to all the read client tasks
  FOR clino = 1 TO climax DO
    sendpkt(notinuse, rdclientv!clino, act_die)

  // Send die packets to all the read server tasks
  FOR serno = 1 TO srvmax DO
    sendpkt(notinuse, rdserverv!serno, act_die)

  // Send die packets to all the write server tasks
  FOR serno = 1 TO srvmax DO
    sendpkt(notinuse, wrserverv!serno, act_die)

  // Send startup packets to all the mpx tasks
  FOR mpxno = 1 TO mpxmax DO
    sendpkt(notinuse, mpxv!mpxno, act_die)

  sendpkt(notinuse, printertaskid, act_die)
  sendpkt(notinuse, bouncetaskid,  act_die)

  // All tcobench tasks except the stats task should be returning to
  // DEAD state.

  IF tracing DO
    sawritef("T%z2 Controller: *
             *All tcobench tasks except the stats task *
             *should be returning to DEAD state*n",
             taskid)

  // Arrive here when the benchmark has completed all its work.

  // Send the controller's statistics counters to the stats task.
  sendstats()

  // Cause the accumulated statistics counters to be output/
  prstats()

  // Cause the stats task to return to DEAD state.
  sendpkt(notinuse, statstaskid, act_die)

fin:
  // Delete all the tasks created by the controller.

  IF tracing DO
    sawritef("T%z2: Attempting to delete all tcobench tasks*n", taskid)

  IF mpxv FOR mpxno = 1 TO mpxmax WHILE mpxv!mpxno DO
  { TEST deletetask(mpxv!mpxno)
    THEN { IF tracing DO
             sawritef("T%z2 Controller: M%z2 deleted*n", taskid, mpxno)
           mpxv!mpxno := 0
         }
    ELSE { IF tracing DO
             sawritef("T%z2 Controller: M%z2 NOT deleted*n", taskid, mpxno)
           delay(100)
         }
  }

  IF rdserverv FOR serno = 1 TO srvmax WHILE rdserverv!serno DO
  { TEST deletetask(rdserverv!serno)
    THEN { IF tracing DO
             sawritef("T%z2 Controller: RS%z2 deleted*n", taskid, serno)
           rdserverv!serno := 0
         }
    ELSE { IF tracing DO
             sawritef("T%z2 Controller: RS%z2 NOT deleted*n", taskid, serno)
           delay(100)
         }
  }

  IF wrserverv FOR serno = 1 TO srvmax WHILE wrserverv!serno DO
  { TEST deletetask(wrserverv!serno)
    THEN { IF tracing DO
             sawritef("T%z2 Controller: WS%z2 deleted*n", taskid, serno)
           wrserverv!serno := 0
         }
    ELSE { IF tracing DO
             sawritef("T%z2 Controller: WS%z2 NOT deleted*n", taskid, serno)
           delay(100)
         }
  }

  IF rdclientv FOR clino = 1 TO climax WHILE rdclientv!clino DO
  { TEST deletetask(rdclientv!clino)
    THEN { IF tracing DO
             sawritef("T%z2 Controller: RC%z2 deleted*n", taskid, clino)
           rdclientv!clino := 0
         }
    ELSE { IF tracing DO
             sawritef("T%z2 Controller: RC%z2 NOT deleted*n", taskid, clino)
           delay(100)
         }
  }

  IF wrclientv FOR clino = 1 TO climax WHILE wrclientv!clino DO
  { TEST deletetask(wrclientv!clino)
    THEN { IF tracing DO
             sawritef("T%z2 Controller: WC%z2 deleted*n", taskid, clino)
           wrclientv!clino := 0
         }
    ELSE { IF tracing DO
             sawritef("T%z2 Controller: WC%z2 NOT deleted*n", taskid, clino)
           delay(100)
         }
  }

  WHILE bouncetaskid DO
  { TEST deletetask(bouncetaskid)
    THEN { IF tracing DO
             sawritef("T%z2 Controller: bounce deleted*n", taskid)
           bouncetaskid := 0
         }
    ELSE { IF tracing DO
             sawritef("T%z2 Controller: bounce NOT deleted*n", taskid)
           delay(100)
         }
  }

  WHILE printertaskid DO
  { TEST deletetask(printertaskid)
    THEN { IF tracing DO
             sawritef("T%z2 Controller: printer deleted*n", taskid)
           printertaskid := 0
         }
    ELSE { IF tracing DO
             sawritef("T%z2 Controller: printer NOT deleted*n", taskid)
           delay(100)
         }
  }

  WHILE statstaskid DO
  { TEST deletetask(statstaskid)
    THEN { IF tracing DO
             sawritef("T%z2 Controller: stats task deleted*n", taskid)
           statstaskid := 0
         }
    ELSE { IF tracing DO
             sawritef("T%z2 Controller: stats task NOT deleted*n", taskid)
           delay(100)
         }
  }

  // All tcobench tasks have now been deleted.
  
  IF rdclientv DO freevec(rdclientv)
  IF wrclientv DO freevec(wrclientv)
  IF rdserverv DO freevec(rdserverv)
  IF wrserverv DO freevec(wrserverv)
  IF mpxv      DO freevec(mpxv)

  // The test is now complete
  sawritef("*nTcobench completed*n")
} // End of tcobenchcom

AND wrtime(mess) BE
{ LET v = VEC 14 // Write mess followed by the current date and time.
  datstring(v)
  sawritef("%s %s %s*n", mess, v, v+5)
}

//******************** Stats Task Body *******************************

AND startstats(pkt) BE
{ LET synclist,   synclistlen = 0, 0
  LET donelist,   donelistlen = 0, 0
  LET calibratepkt = 0
  LET runpkt, alldone = 0, FALSE
  LET bounces    = 0   // Number of bounces, zeroed at the start of each period
  LET bouncesmax = 0   // The maximum number of bounces in one clock period
  LET bouncing = FALSE // TRUE if the bounce pkt has been sent to the bounce task.
  LET clocking = FALSE // TRUE if the clock pkt has been sent to the clock
  LET clockpkt  = TABLE notinuse,
                        -1,              // The clock device number
                        act_clock, 0, 0,
                        100              // Return after 100 msec

  LET bouncepkt = TABLE notinuse,  ?, act_bounce, 0, 0
  LET utilisationv = VEC 9
  LET cycle = 1

  diepkt := 0

  bouncepkt!pkt_id := bouncetaskid
  FOR i = 0 TO 9 DO utilisationv!i := 0

  set_process_name("Stats")

  IF tracing DO
    sawritef("T%z2 Stats:   Task ready*n", taskid)

  c_qpkt := c_qpkt+1
  qpkt(pkt) // Return the startup packet

  { // Start of stats task event loop

    // It expects packets of the following types:

    //    act_calibrate  Sent by the controller
    //    act_run        Sent by the controller
    //    act_sync       from client tasks asking permission
    //                   to perform the next schedule. Returned
    //                   when all sync packets have been received
    //                   provided the run packet has been received
    //                   from the controller.
    //    act_rddone     Sent by a read client
    //    act_wrdone     Sent by a write client
    //    act_addstats   receive stats from other tasks
    //    act_prstats    output the stats
    //    act_die        Sent by the controller

    c_taskwait := c_taskwait+1
    pkt := taskwait()

    SWITCHON pkt!pkt_type INTO
    { DEFAULT:
        sawritef("T%z2 Stats: Unexpected packet from %n type %n*n",
                  taskid, pkt!pkt_id, pkt!pkt_type)
        c_qpkt := c_qpkt + 1
        qpkt(pkt) // Return this unexpected packet
        ENDCASE

      CASE act_calibrate:
        // Calibrate the maximum bounce rate.
        calibratepkt := pkt
        UNLESS synclistlen = climax+climax LOOP
        // Only perform calibration when all read and write clients
        // are ready to run their first schedules.

      calibrate:
        // We may arrive here from CASE act_sync:
        IF tracing DO
          sawritef("*nT%z2 Stats: Calibrating the bounce counter*n", taskid)

        { LET cycles = 10 // The number of 100msec time periods to use
                          // when calibrating tha bounce rate.
          bounces := 0

          clocking := TRUE      // Start clocking
          c_qpkt := c_qpkt+1
          qpkt(clockpkt)

          bouncing := TRUE      // Start bouncing
          c_qpkt := c_qpkt+1
          qpkt(bouncepkt) // This returns as soon as the bounce task can return it.

          WHILE clocking | bouncing DO
          { LET pkt = ?
            c_taskwait := c_taskwait+1
            //Wait for a packet from either the clock or bounce task.
            pkt := taskwait()

            SWITCHON pkt!pkt_type INTO
              { DEFAULT:
               sawritef("T%z2 Stats:*
                        * System Error: Unexpected packet from %n type %n*n",
                        taskid, pkt!pkt_id, pkt!pkt_type)
               abort(999)

              CASE act_clock:
                clocking := FALSE
                IF bouncesmax=0 DO bounces := 1 // Ignore first time period
                IF bouncesmax < bounces DO bouncesmax := bounces
                bounces := 0
                cycles := cycles - 1
                IF cycles DO
                { // Start another 100 msecs time period.
                  c_qpkt := c_qpkt+1
                  qpkt(clockpkt)
                  clocking := TRUE 
                }
                LOOP

             CASE act_bounce:
                bounces := bounces+1
                bouncing := FALSE
                UNLESS cycles BREAK // Both clk and bounce pkts are back
                c_qpkt := c_qpkt+1
                qpkt(bouncepkt)
                bouncing := TRUE
                LOOP
            }
          }
        }

        // Calibration is complete and both the clock and bounce
        // packets have been returned.

        IF tracing DO
          sawritef("*nT%z2 Stats: bouncesmax = %n*n*n", taskid, bouncesmax)

        // Give the calibrate packet back to the controller.
        pkt := calibratepkt
        calibratepkt := 0
        c_qpkt := c_qpkt+1
        qpkt(pkt)
        LOOP

      CASE act_run:
        runpkt := pkt

        // Start the clock
        c_qpkt := c_qpkt+1
        qpkt(clockpkt)
        clocking := TRUE

        // Start bouncing
        c_qpkt := c_qpkt+1
        qpkt(bouncepkt)
        bouncing := TRUE
        // Fall through in CASE act_sync:

      CASE act_sync:
        IF pkt!pkt_type=act_sync DO
        { // Request from a client to start the next schedule.
          // It is returned when all clients have sent sync packets.
          pkt!0 := synclist
          synclist := pkt
          synclistlen := synclistlen+1
          IF tracing DO
            sawritef("T%z2 Stats:*
                     *  Sync received from T%z2*n",
                     taskid, pkt!pkt_id, synclistlen)
        }

        UNLESS synclistlen = climax+climax LOOP

        IF calibratepkt GOTO calibrate

        // Note that controller only sends the run packet after
        // calibration has been completed.

        IF runpkt DO
        { // Run packet and all sync packets received
          // so return the sync packets to their clients.
          LET p = synclist
          IF tracing DO
            sawritef("T%z2 Stats:  %i2 Releasing all clients*n",
                     taskid, cycle)
          cycle := cycle + 1
          synclist, synclistlen := 0, 0
          WHILE p DO
          { pkt := p
            p := p!0
            pkt!0 := notinuse
            c_qpkt := c_qpkt+1
            qpkt(pkt)
          }
        }
        LOOP

      CASE act_clock:
        // Keep clocking until alldone=TRUE
        UNLESS alldone DO
        { LET u = ?

          c_qpkt := c_qpkt+1
          qpkt(pkt)  // Send the packet back to the clock

          // Even after calibration bouncesmax might still increase.
          IF bouncesmax < bounces DO bouncesmax := bounces

          u := 1000 * (bouncesmax-bounces) / bouncesmax / 101  // 0 ... 9
          IF tracing DO
            sawritef("T%z2 Stats: clock packet received, bounces=%n u=%n*n",
                     taskid, bounces, u)

          UNLESS 0<=u<=9 DO
          { sawritef("T%z2 Controller:*
                     * System error -- Utilisation u=%n out of range*n",
                     taskid, u)
            abort(999)
          }
          utilisationv!u := utilisationv!u + 1
          //IF tracing DO
          //  sawritef("T%z2 Controller:*
          //           * Utilisation = %i3  bounces = %i5 bouncesmax = %i5*n",
          //            taskid, u, bounces, bouncesmax)
          bounces := 0
          LOOP
        }

        // alldone=TRUE so we have reached the end of the final
        // time period.
        clocking := FALSE
        // Do not send the packet to the clock device.
        IF tracing DO
          sawritef("T%z2 Stats:*
                   *  clocking set to FALSE*n",
                    taskid)
        LOOP

      CASE act_bounce:
        bounces := bounces+1

        IF clocking DO
        { // Still in a time period so bounce again
          c_qpkt := c_qpkt+1
          qpkt(pkt)
          LOOP
        }

        // clocking=FALSE so the end of the final time period
        // has been reached. This only happens when a clock packet
        // is received when alldone=TRUE.

        // We have thus just received the first bounce packet
        // after the end of the final time period. It is therefore
        // time to return the run packet to the controller.

        bouncing := FALSE
        IF tracing DO
        { sawritef("T%z2 Stats:*
                   *   bouncing set to FALSE*n",
                   taskid)
        }

        c_qpkt := c_qpkt+1
        qpkt(runpkt)

        // We still need to process addstats packets from dying tasks,
        // the prstats packet from the controller and the final die
        // packet which will cause the stats task to return to DEAD
        // state.
        LOOP

      CASE act_rddone:
      CASE act_wrdone:
      { // Client has finished all its schedules.

        TEST pkt!pkt_type=act_rddone
        THEN { IF tracing DO
                 sawritef("T%z2 Stats:*
                          *   rddone packet received from T%z2*n",
                          taskid, pkt!pkt_id)
             }
        ELSE { IF tracing DO
                 sawritef("T%z2 Stats:*
                          *   wrdone packet received from T%z2*n",
                          taskid, pkt!pkt_id)
             }

        pkt!0 := donelist
        donelist := pkt
        donelistlen := donelistlen+1
        IF donelistlen = climax+climax DO
        { // All the rddone and wrdone packets have been received
          // so return them to their clients.
          LET p = donelist
          donelist, donelistlen := 0, 0
          WHILE p DO
          { pkt := p
            p := p!0
            pkt!0 := notinuse
            c_qpkt := c_qpkt+1
            qpkt(pkt)
          }

          alldone := TRUE
          IF tracing DO
            sawritef("T%z2 Stats:*
                     *   alldone set to TRUE*n",
                     taskid)
        }
        LOOP
      }

      CASE act_addstats:
      { // Add the stats from another task
        LET sv = pkt!pkt_a1
        c_qpkt := c_qpkt + 1 // Because of qpkt below

        // Add a task's statistics counters to the stats task's
        // own counters
        c_qpkt      := sv!p_qpkt      + c_qpkt
        c_taskwait  := sv!p_taskwait  + c_taskwait
        c_callco    := sv!p_callco    + c_callco
        c_resumeco  := sv!p_resumeco  + c_resumeco
        c_cowait    := sv!p_cowait    + c_cowait
        c_condwait  := sv!p_condwait  + c_condwait
        c_notify    := sv!p_notify    + c_notify
        c_notifyAll := sv!p_notifyAll + c_notifyAll
        c_inc       := sv!p_inc       + c_inc
        c_incwait   := sv!p_incwait   + c_incwait
        c_lock      := sv!p_lock      + c_lock
        c_lockw     := sv!p_lockw     + c_lockw
        c_delaylong := sv!p_delaylong + c_delaylong
        c_bounce    := sv!p_bounce    + c_bounce
        c_print     := sv!p_print     + c_print
        c_logger    := sv!p_logger    + c_logger
        c_readfail  := sv!p_readfail  + c_readfail
        c_sendfail  := sv!p_sendfail  + c_sendfail

        c_rdchecksum := (sv!p_rdchecksum + c_rdchecksum) MOD 1_000_000
        c_wrchecksum := (sv!p_wrchecksum + c_wrchecksum) MOD 1_000_000
        c_rdcount    := sv!p_rdcount     + c_rdcount
        c_wrcount    := sv!p_wrcount     + c_wrcount

        c_qpkt := c_qpkt+1
        qpkt(pkt)    // Return the addstats pkt to its task
        ENDCASE
      }

      CASE act_prstats:
        // Output the statistics counters that have been accumulated in
        // the stats task.

        c_qpkt := c_qpkt + 1 // Because of qpkt below

        sawritef("*n")
        sawritef("Number of calls of qpkt:          %i9*n", c_qpkt)
        sawritef("Number of calls of taskwait:      %i9*n", c_taskwait)
        sawritef("Number of calls of callco:        %i9*n", c_callco)
        sawritef("Number of calls of cowait:        %i9*n", c_cowait)
        sawritef("Number of calls of resumeco:      %i9*n", c_resumeco)
        sawritef("Number of calls of condwait(..):  %i9*n", c_condwait)
        sawritef("Number of calls of notify(..):    %i9*n", c_notify)
        sawritef("Number of calls of notifyAll(..): %i9*n", c_notifyAll)
        sawritef("Number of increments:             %i9*n", c_inc)
        sawritef("   increment had to wait:         %i9*n", c_incwait)
        sawritef("Number of calls of lock(..):      %i9*n", c_lock)
        sawritef("   lock had to wait:              %i9*n", c_lockw)
        sawritef("Number of %i4 msec delays:       %i9*n", delaymsecs, c_delaylong)
        sawritef("Print task counter:               %i9*n", c_print)
        sawritef("Calls to logger:                  %i9*n", c_logger)
        sawritef("Send fail count:                  %i9*n", c_sendfail)
        sawritef("Read fail count:                  %i9*n", c_readfail)
        sawritef("Bounce task counter:              %i9*n", c_bounce)

        sawritef("Read checksum:                    %i9*n", c_rdchecksum)
        sawritef("Write checksum:                   %i9*n", c_wrchecksum)
        sawritef("Read count:                       %i9*n", c_rdcount)
        sawritef("Write count:                      %i9*n", c_wrcount)

        sawritef("*n")

      { LET total = 0
        FOR i = 0 TO 9 DO total := total + utilisationv!i

        UNLESS total DO total := 1

        sawritef("     Approximate CPU utilisation over %n periods of 100 msecs*n*n",
                 total)
        sawritef("  0-10%% 10-20%% 20-30%% 30-40%% 40-50%% 50-60%%*
                 * 60-70%% 70-80%% 80-90%% 90-100%%*n")
        FOR i = 0 TO 9 DO sawritef("%i5  ", utilisationv!i)
        sawritef("*n")
        c_qpkt := c_qpkt+1
        qpkt(pkt)
        ENDCASE
      }

      CASE act_die:
        IF tracing DO
          sawritef("T%z2 Stats:*
                   * Die packet received*n",
                   taskid)
        diepkt := pkt
        BREAK
    }
  } REPEAT

  // This point is only reached when when the stats task should
  // return to DEAD state, ready for deletion.

  IF tracing DO
    sawritef("T%z2 Stats:   Returning the die packet to controller*n", taskid)

  c_qpkt := c_qpkt+1
  qpkt(diepkt)

  IF tracing DO
    sawritef("T%z2 Stats:   Returning to DEAD state*n", taskid)

  RETURN // Return the stats task to DEAD state
         // ready for deletion by the controller.
}

//******************** Bounce Task Body ******************************

AND startbounce(pkt) BE
{ LET echoco = createco(echofn, 300)
  set_process_name("Bounce")

  IF tracing DO
    sawritef("T%z2 Bounce:  Task ready*n", taskid)

  c_qpkt := c_qpkt+1
  qpkt(pkt) // Return the startup packet

  { c_taskwait := c_taskwait+1
    pkt := taskwait()
    SWITCHON pkt!pkt_type INTO
    { DEFAULT:
        sawritef("T%z2 Bounce: Unexpected packet from %n type %n*n",
                  taskid, pkt!pkt_id, pkt!pkt_type)
        abort(999)

      CASE act_bounce:
        c_bounce := c_bounce+1

        // The following loop is to keep the ratio of
        // callcos to qpkts high even when the bounce
        // count is high.
        FOR i = 1 TO 10 DO
        { c_callco := c_callco+1
          callco(echoco, i)
        }

        c_qpkt := c_qpkt+1
        qpkt(pkt) // Return the packet
        LOOP

      CASE act_die:
        IF tracing DO
          sawritef("T%z2 Bounce: die packet received*n", taskid)

        // Delete the echo coroutine
        IF echoco DO { deleteco(echoco); echoco := 0 }

        // Send the bounce statistics to the stats task
        sendstats()

        IF tracing DO
          sawritef("T%z2 Bounce: Returning to DEAD state*n", taskid)

        c_qpkt := c_qpkt+1
        qpkt(pkt) // Return the die packet to the controller.

        RETURN // Return to DEAD state ready for deletion.
    }
  } REPEAT
}

AND echofn(x) BE // The body of echoco
{ c_cowait := c_cowait+1
  x := cowait(x)
} REPEAT

//******************** Printer Task Body *****************************

// This simulates a printer. It it essentially the same as the bounce
// task but runs at slightly higher priority. Server logger coroutines
// occasionally send packets to this task. This typically causes another
// task to gain control.
AND startprinter(pkt) BE
{ LET delaypkt = TABLE notinuse,
                       -1,       // The clock device
                       act_clock,
                       0,0,      // Result fields
                       10        // Delay for 10 msecs
  LET q = 0         // List of pending print packets while delaying.
  LET printpkt = 0  // Current print packet, if any.

  set_process_name("Printer")

  IF tracing DO
    sawritef("T%z2 Printer: Task ready*n", taskid)

  c_qpkt := c_qpkt+1
  qpkt(pkt) // Return the startup packet

  { // Start of the printer event loop.
    TEST q
    THEN { // Dequeue a pending print packet.
           pkt  := q
           q    := !q
           !pkt := notinuse
         
           IF tracing DO
             sawritef("T%z2 Printer: Pending packet %n type %n dequeued*n",
                      taskid, pkt, pkt!pkt_type)

         }
    ELSE { // Wait for a print or die packet
           c_taskwait := c_taskwait+1
           pkt := taskwait()
           IF tracing DO
             sawritef("T%z2 Printer: Packet %n type %n received*n",
                      taskid, pkt, pkt!pkt_type)
         }

    SWITCHON pkt!pkt_type INTO
    { DEFAULT:
        sawritef("T%z2 Printer: Unexpected packet from task %n*n",
                  taskid, pkt!pkt_id)
        abort(999)

      CASE act_print:  // arg1: modech  arg2: serno
      { LET modech = pkt!pkt_arg1
        LET serno  = pkt!pkt_arg2
        c_print := c_print+1
        IF tracing DO
          sawritef("T%z2 Printer: act_print received from %cS%z2*n",
                    taskid, modech, serno)
        printpkt := pkt // Save the print packet to be returned after
                        // 10 msecs.
        IF tracing DO
          sawritef("T%z2 Printer: sending a delay packet to the clock*n",
                    taskid)

        c_qpkt := c_qpkt+1
        qpkt(delaypkt)

        { // Wait for the delay packet to return, queueing any
          // other packets that arrive in the mean time.
          pkt := taskwait()
          IF pkt=delaypkt BREAK

          // Insert the packet in q.
          !pkt := q
          q := pkt
        } REPEAT

        // The 10 msecs delay has now ended, so return the print
        // packet and process another (possibly pending) request.

        c_qpkt := c_qpkt+1
        qpkt(printpkt) // Return the print packet
        printpkt := 0
        LOOP
      }

      CASE act_die:
        IF tracing DO
          sawritef("T%z2 Printer: die packet received*n", taskid)

        // Send the printer statistics to the stats task
        sendstats()

        c_qpkt := c_qpkt+1
        qpkt(pkt) // Send die packet back to the controller

        IF tracing DO
          sawritef("T%z2 Printer: Returning to DEAD state*n", taskid)

        RETURN  // Return printer task to DEAD state.
    }
  } REPEAT
}

//******************* Read or Write Client Body ****************************

AND startclient(pkt) BE
{ // Body of a client task which runs in single-event mode.
  // pkt is the startup packet or one of the following

  //      act_startrdclient  a1:clino
  // or   act_startwrclient  a1:clino
  // or   act_die

  LET serverv = 0 // Will hold the vector of read or write servers
                  // for this client.
  LET requestv = getvec(requestvupb)
  LET datav    = getvec(requestvupb)

  // Distinquish between read and write clients.
  TEST pkt!pkt_type=act_startrdclient
  THEN modech, serverv := 'R', rdserverv
  ELSE modech, serverv := 'W', wrserverv

  clino := pkt!pkt_arg1 // From the startup packet

  UNLESS requestv & datav DO
  { sawritef("T%z2 %cC%z2: Unable to allocate requestv and datav*n",
              taskid, modech, clino)
    GOTO fin
  }

  set_process_name("%cC%z2", modech, clino)

  setseed(clino+modech*100)

  //FOR i = 1 TO 5 DO
  //{ LET x = nextrnd(9999)
  //  sawritef("%i5*n", x)
  //}
  //abort(1001)

  //IF tracing DO
  //  sawritef("T%z2 %cC%z2: Started*n", taskid, modech, clino)

  c_qpkt := c_qpkt+1
  qpkt(pkt)  // Return the start up packet to the controller

  FOR count = 1 TO loopmax DO
  { // Create the schedule of requests for this client in requestv
    // and datav (for write client data).
    // Each request a specifies different server-multiplexor-channel
    // combination and so requestv will contain servmax*mpxmax*chnmax
    // items. One will be randomly chosen to have a delay in this
    // client, a different one will be randomly chosen to cause
    // a delay in server, and a third different one will delay in
    // a multiplexor.
    LET pos = 0
    LET rqstvupb = requestvupb
    LET clidelayitem = 0  // This will be the subscript of requestv of
                          // the request to delay in this client.
    LET srvdelayitem = 0  // This will be the subscript of requestv of
                          // the request to delay in its server.
    LET mpxdelayitem = 0  // This will be the subscript of requestv of
                          // the request to delay in its multiplexor.

    // This client will not create its next schedule until all other
    // read and write clients are ready to do the same. This is achieved
    // by sending a sync packet to the stats task which is only returned
    // it when all the client sync packets have been received.

    sendpkt(notinuse, statstaskid, act_sync, 0,0)

    // This point is reached when all read and write clients have sent
    // sync packets to the stats task.

    // This client can now start, create and run its next schedule.

    IF tracing DO
    { sawritef("T%z2 %cC%z2:    Starting iteration %n out of %n*n",
                taskid, modech, clino, count, loopmax)
      //abort(1000)
    }

    // Create a list of requests to be processed.

    // Choose three distinct requests to cause real time delays,
    // if possible.

    IF requestvupb >= 3 DO
    { clidelayitem := nextrnd(rqstvupb) // In range 1..requestvupb

      srvdelayitem := nextrnd(rqstvupb) // In range 1..requestvupb
      REPEATWHILE srvdelayitem=clidelayitem

      mpxdelayitem := nextrnd(rqstvupb) // In range 1..requestvupb
      REPEATWHILE mpxdelayitem=clidelayitem |
                  mpxdelayitem=srvdelayitem
    }

    FOR serno = 1 TO srvmax DO
      FOR mpxno = 1 TO mpxmax DO
        FOR chnno = 1 TO chnmax DO
        { LET flag = 'n'
          pos := pos+1
          IF pos=clidelayitem DO flag := 'c' // Client delay
          IF pos=srvdelayitem DO flag := 's' // Server delay
          IF pos=mpxdelayitem DO flag := 'm' // Multiplexor delay
          requestv!pos := flag<<24 | serno<<16 | mpxno<<8 | chnno
          datav!pos    := modech='R' -> 0,
                          nextrnd(9999) // Range 1 to 9999
        }

    IF tracing DO
    { sawritef("*nT%z2 %cC%z2:    Schedule of work*n",
               taskid, modech, clino)
      FOR i = 1 TO requestvupb DO
      { // Output the schedule as a debugging aid.
        LET req   = requestv!i
        LET flag  = req>>24 & 255
        LET serno = req>>16 & 255
        LET mpxno = req>>8  & 255
        LET chnno = req     & 255
        LET data  = datav!i
      
        sawritef(" %c%cS%z2M%z2C%z2:%z4",
                 flag, modech, serno, mpxno, chnno, data)
        IF i MOD 5 = 0 DO sawritef("*n")
      }
      UNLESS requestvupb MOD 5 = 0 DO sawritef("*n")
      sawritef("*n")
      //abort(1000)
    }

    // Now process the requests in the schedule in random order.
    WHILE rqstvupb DO
    { LET i = nextrnd(rqstvupb) // Choose a random request from
      LET req   = requestv!i    // the schedule.
      LET data  = datav!i       // Data for a write client 
      LET flag  = req>>24 & 255 // ='c' delay in this client,
                                // ='s' delay in a server
                                // ='m' delay in a multiplexor
                                // ='n' no delay
      LET serno = req>>16 & 255
      LET mpxno = req>>8  & 255
      LET chnno = req     & 255

      TEST modech='R'
      THEN { // Code for a read client
             IF tracing DO
               sawritef("T%z2 RC%z2:*
                         *    %c%cC%z2S%z2M%z2C%z2:%z4*
                         * Sending read request to server*n",
                         taskid, clino, flag,
                         modech, clino, serno, mpxno, chnno, data)

             data := sendpkt(notinuse, serverv!serno, act_read,
                             0, 0, flag, clino, serno, mpxno, chnno, data)

             IF tracing DO
               sawritef("T%z2 RC%z2:*
                         *    %c%cC%z2S%z2M%z2C%z2:%z4*
                         * Packet returned from server*n",
                         taskid, clino, flag,
                         modech, clino, serno, mpxno, chnno, data)
             IF data=0 DO
             { // Read was not successful since its channel buffer
               // was empty.
               IF tracing DO
                 sawritef("T%z2 RC%z2:    %c%cC%z2S%z2M%z2C%z2:%z4 read failed*n*n",
                          taskid, clino, flag,
                          modech, clino, serno, mpxno, chnno, data)

               c_readfail := c_readfail+1

               // Delay for 200 msecs to give other tasks a chance to run,
               // hopefully allowing write clients to put more data in
               // the channels buffers.
               delay(200)

               LOOP // Try sending another random request from the schedule.
             }

             // The read request was successful.

             IF tracing DO
               sawritef("T%z2 RC%z2:*
                        *    %c%cC%z2S%z2M%z2C%z2:%z4 read successful*n",
                        taskid, clino, flag,
                        modech, clino, serno, mpxno, chnno, data)
             c_rdchecksum := (c_rdchecksum + data) MOD 1_000_000
             c_rdcount    := c_rdcount+1
           }
      ELSE { // Code for a write client.
             LET rc = ?                          // Return code
             IF tracing DO
               sawritef("T%z2 WC%z2:    %c%cC%z2S%z2M%z2C%z2:%z4*
                        * Sending write request to server*n",
                        taskid, clino, flag,
                        modech, clino, serno, mpxno, chnno, data)
             rc := sendpkt(notinuse, serverv!serno, act_write,
                           0, 0, flag, clino, serno, mpxno, chnno, data)
             IF rc=0 DO
             { // Write was not successful since its channel buffer
               // was full.
               IF tracing DO
                 sawritef("T%z2 WC%z2:*
                          *    %c%cC%z2S%z2M%z2C%z2:%z4 send failed*n*n",
                          taskid, clino, flag,
                          modech, clino, serno, mpxno, chnno, data)
               c_sendfail := c_sendfail+1

               // Delay for 20 msecs to give other tasks a chance to run.
               delay(20)

               LOOP // Try sending another random request from the schedule.
             }

             // The write request was successful in that the data was
             // written by the specified multiplexor in the specified
             // channel buffer. The data may not have been read yet by
             // a read client.

             IF tracing DO
               sawritef("T%z2 WC%z2:*
                        *    %c%cC%z2S%z2M%z2C%z2:%z4 Data sent*n",
                        taskid, clino, flag,
                        modech, clino, serno, mpxno, chnno, data)

             c_wrchecksum := (c_wrchecksum + data) MOD 1_000_000
             c_wrcount := c_wrcount+1
           }

      // The read or write request has been successfully processed.

      // Delay for delaymsecs if a client delay is specified.
      IF flag='c' DO
      { IF tracing DO
          sawritef("T%z2 WC%z2:*
                   *    %c%cC%z2S%z2M%z2C%z2:%z4 Client delay*n",
                    taskid, clino, flag,
                    modech, clino, serno, mpxno, chnno, data)
        c_delaylong := c_delaylong+1
        delay(delaymsecs)
        IF tracing DO
          sawritef("T%z2 WC%z2:*
                   *    %c%cC%z2S%z2M%z2C%z2:%z4 Client delay ended*n",
                    taskid, clino, flag,
                    modech, clino, serno, mpxno, chnno, data)
      }

      // Remove the read or write request from the schedule.
      requestv!i := requestv!rqstvupb
      datav!i    := datav!rqstvupb
      rqstvupb   := rqstvupb-1

      // Process another request of the schedule, if any.
    }
    // Create and perform the next schedule.
  }

  IF tracing DO
    sawritef("T%z2 %cC%z2:*
             *    All iterations completed*n",
             taskid, modech, clino)

fin:
  IF requestv DO freevec(requestv)
  IF datav    DO freevec(datav)

  // Tell the stats task that this client has finished its
  // final schedule.
  TEST modech='R'
  THEN { IF tracing DO
           sawritef("T%z2 %cC%z2:*
                    *    Sending act_rddone to the stats task*n",
                    taskid, modech, clino)
         sendpkt(notinuse, statstaskid, act_rddone)
       }
  ELSE { IF tracing DO
           sawritef("T%z2 %cC%z2:*
                    *    Sending act_wrdone to the stats task*n",
                    taskid, modech, clino)
         sendpkt(notinuse, statstaskid, act_wrdone)
       }

  // Wait for the act_die packet from the controller
  IF tracing DO
    sawritef("T%z2 %cC%z2:*
             *    waiting for die packet from the controller*n",
             taskid, modech, clino)

  c_taskwait := c_taskwait+1
  pkt := taskwait()

  UNLESS pkt!pkt_type=act_die DO
  { sawritef("T%z2 %cC%z2:*
             * System error -- act_die packet expected*n", taskid)
    abort(999)
  }

  IF tracing DO
    sawritef("T%z2 %cC%z2:*
             * die packet received*n",
              taskid, modech, clino)

  // Send this client's statistics to the stats task
  sendstats()

  IF tracing DO
    sawritef("T%z2 %cC%z2:*
             *    Returning the die packet to the controller*n",
             taskid, modech, clino)

  c_qpkt := c_qpkt+1 // For qpkt below
  qpkt(pkt)

  IF tracing DO
    sawritef("T%z2 %cC%z2:   Returning to DEAD state*n",
              taskid, modech, clino)

  RETURN  // Return this Client to DEAD state
}

//******************* Read or Write Server Task Body ********************

AND startserver(pkt) BE
{ // This is the body of a read or write server task most of which runs
  // in multi-event mode using gomultievent.

  // pkt is the startup packet of the form:
  //      act_startrdserver   a1:serno
  // or   act_startwrserver   a1:serno

  // This initialises the task as either a read or write server and
  // sets its own identity in id.

  // Later packets are of the form:

  //      act_read   a1:flag  a2:clino a3:serno a4:mpxno a5:chano
  // or   act_write  a1;flag  a2:clino a3:serno a4:mpxno a5:chano a6:data
  // or   act_sync   a1:clino a2:serno
  // or   act_die

  startuppkt := pkt      // Used by servermaincofn

  serno  := pkt!pkt_arg1 // The read or write server number
  modech := pkt!pkt_type=act_startrdserver -> 'R', 'W'

  set_process_name("%cS%z2", modech, serno)

  // Set random number seed for random values to send
  // by worker coroutines to the logger.
  setseed(serno+modech*100)

  gomultievent(servermaincofn, 1000)

fin:
  IF tracing DO
    sawritef("T%z2 %cS%z2:    dying*n", taskid, modech, serno)

  // Send this server's statistics to the stats task.
  IF tracing DO
    sawritef("T%z2 %cS%z2:*
             *    Sending server statistics to stats task*n",
             taskid, modech, serno)

  sendstats()

  // Return the die packet to the controller.
  c_qpkt := c_qpkt+1
  qpkt(diepkt)
  IF tracing DO
    sawritef("T%z2 %cS%z2:   returning to DEAD state*n",
              taskid, modech, serno)

  RETURN  // Return server to DEAD state
}

AND loggercofn(arg) BE
{ LET i = 0
  IF tracing DO
    sawritef("T%z2 %cS%z2Log: Ready*n", taskid, modech, serno)

  { // Start of logger loop.
    LET a, b = ?, ?
    a := coread(@loggerin)
    //IF tracing DO
    //  sawritef("T%z2 %cS%z2log: Received a=%n*n",
    //            taskid, modech, serno, a)
    c_logger := c_logger + 1
    i := i+1

    // Occasionally the logger sends a message to the printer task
    IF i MOD 50 = 0 DO
    { //IF tracing DO
      //  sawritef("T%z2 %cS%z2log:*
      //           * logger sending pkt to printer*n",
      //            taskid, modech, serno)
      sendpkt(notinuse, printertaskid, act_print, 0, 0, modech, serno)
      //IF tracing DO
      //  sawritef("T%z2 %cS%z2log:*
      //           * pkt returned from printer*n",
      //           taskid, modech, serno)
    }

    // Occasionally the logger delays briefly
    IF i MOD 7 = 0 DO delay(2)

    b := coread(@loggerin)
    //IF tracing DO
    // sawritef("T%z2 %cS%z2log: Received b=%n*n",
    //           taskid, modech, serno, b)

    { LET x = a+b
      //IF tracing DO
      //  sawritef("T%z2 %cS%z2log: Replying %n*n",
      //            taskid, modech, serno, x, x)
      wrpn(x)
      cowrite(@loggerout, -1)
    }
  } REPEAT
}

AND wrpn(x) BE IF x DO
{ wrpn(x>>1)
  cowrite(@loggerout, x&1)
}

AND getloggerval() = VALOF
{ LET res = 0
  //IF tracing DO
  //  sawritef("T%z2 %cS%z2: getloggerval called*n",
  //            taskid, modech, serno)

  { LET dig = coread(@loggerout)
    //IF tracing DO
    //  sawritef("T%z2 %cS%z2:*
    //           * getloggerval: dig = %i2 res = %i2*n",
    //           taskid, modech, serno, dig, res)
    IF dig<0 RESULTIS res
    res := res+res+dig
  } REPEAT
}

AND servermaincofn() BE
{ // This is a server's main coroutine running in multi-event mode under
  // the control of gomultievent.
  LET pkt, type = ?, ?

  // mainco_ready is initially set to FALSE by gomultievent.

  diepkt := 0       // Only non zero when the die packet has been received.

  // Condition variables are held in this server's globals and shared
  // by this server's worker coroutines.

  // When a work coroutine wishes to process a request it must increment
  // its count but may have to wait, using condwait(@countwaitlist),
  // if wrkcount-minwrkcount has grown too large. When it increments its
  // count and it is possible that minwrkcount must also be incremented.
  // If this happens all coroutine waiting on condcondvar should be
  // released. This is done by the call: notifyAll(@countwaitlist).

  countcondvar := 0 // Used by workers wishing to increment wrkcount.
  pktcondvar   := 0 // Used by workers waiting for packets to service.

  serverq := 0      // List of packets waiting to be served by worker
                    // coroutines.

  wrkcov := getvec(workmax)    // Vector of worker coroutines.
  countv := getvec(workmax)    // Vector of workcounts.

  UNLESS wrkcov & countv DO
  { sawritef("T%z2 %cS%z2: Unable to allocate vectors*n",
              taskid, modech, serno)
    GOTO fin
  }

  // There are currently no worker coroutines and all counts are zero.
  FOR i = 0 TO workmax DO wrkcov!i, countv!i := 0, 0

  minwrkcount := 0

  busycount  := 0    // Count of worker coroutines that are busy
                     // A worker coroutine is busy when it is processing
                     // a request packet. It is not busy when waiting on
                     // countcondvar or pktcondvar.

  loggerlock := 0    // The logger mutex

  loggerin  := 0     // Occam style channel for logger input
  loggerout := 0     // Occam style channel for logger output

  // Create the logger coroutine for this server.
  loggerco := initco(loggercofn, 1000)
  UNLESS loggerco DO
  { sawritef("T%z2 %cS%z2:*
             * Unable to create logger coroutine*n",
              taskid, modech, serno)
    GOTO fin
  }

  c_callco := c_callco+1 // Because of callco in initco above

  // Create the server work coroutines that will process read or
  // write requests
  FOR wkno = 1 TO workmax DO
  { LET co = initco(workcofn, 1000, wkno)
    UNLESS co DO
    { sawritef("T%z2 %cS%z2W%z2: Unable to create worker coroutine*n",
                taskid, modech, serno, wkno)
      GOTO fin
    }
    c_callco := c_callco+1 // Because of callco in initco above
    wrkcov!wkno := co
    IF tracing DO sawritef("T%z2 %cS%z2W%z2: Worker coroutine ready*n",
                            taskid, modech, serno, wkno)
  }

  //IF tracing DO
  //  sawritef("T%z2 %cS%z2: Ready*n", taskid, modech, serno)

  c_qpkt := c_qpkt+1
  qpkt(startuppkt)

  { // Start of this server's event loop
    LET chnno, data = ?, ?

    // Get a new request from gomultievent.
    // It should be a read, write or die packet.
    // Bounce or clock packets belonging to worker
    // coroutines will be delivered automatically
    // to their coroutines by gomultievent.

    IF tracing DO
      sawritef("T%z2 %cS%z2:*
               *    Waiting for a request packet from a client*n*n",
                taskid, modech, serno)
    mainco_ready := TRUE
    c_cowait := c_cowait+1
    pkt := cowait()
    mainco_ready := FALSE

    SWITCHON pkt!pkt_type INTO
    { DEFAULT:
        sawritef("T%z2 %cS%z2:*
                 * Unexpected packet received from %cC%z2*n",
                  taskid, modech, serno, modech, clino)
        abort(999)

      CASE act_read:
      CASE act_write:
        type   := pkt!pkt_type
        clino  := pkt!pkt_arg1
        //This work coroutine already knows its serno (and modech).
        mpxno  := pkt!pkt_arg3
        chnno  := pkt!pkt_arg4
        data   := pkt!pkt_arg5  // If it is a write request

      { // Append the packet onto the end of the server's serverq
        // and release the first coroutine, if any, waiting on
        // pktcondvar
        LET p = @serverq
        WHILE !p DO p := !p
        !p, !pkt := pkt, 0
        //IF tracing DO
        //  sawritef("T%z2 %cS%z2:*
        //           * Calling notify(%n) for condition pktcondvar*n",
        //            taskid, modech, serno, @pktcondvar)

        // Wake up a worker, if any, waiting for a request.
        notify(@pktcondvar)
        LOOP
      }

       CASE act_die:  // From the controller task.
         // Cause this server to return to DEAD state.
         IF tracing DO
           sawritef("T%z2 %cS%z2: Die packet received*n",
                    taskid, modech, serno)
         diepkt := pkt
         // Release all workers on countcondvar.
         // They will all notice that diepkt is non zero.
         notifyAll(@countcondvar)
         BREAK  // Ie go to fin
    }
  } REPEAT

fin:
  // Delete all worker coroutines and the logger.
  IF tracing DO
    sawritef("T%z2 %cS%z2: Deleting worker coroutines*n",
              taskid, modech, serno)

  FOR wkno = 1 TO workmax IF wrkcov!wkno DO deleteco(wrkcov!wkno)
  IF tracing DO
    sawritef("T%z2 %cS%z2: Deleting logger coroutine*n",
              taskid, modech, serno)
  IF loggerco DO deleteco(loggerco)
  
  IF tracing DO
    sawritef("T%z2 %cS%z2: Freeing the work space*n",
              taskid, modech, serno)
  IF wrkcov   DO freevec(wrkcov)
  IF countv   DO freevec(countv)

  IF tracing DO
    sawritef("T%z2 %cS%z2:*
             * Cause gomultievent to return to single event mode*n",
             taskid, modech, serno)

  multi_done := TRUE
  c_cowait := c_cowait+1
  cowait()  // Return to the controller.

  // This point should not be reached.
  sawritef("T%z2 %cS%z2: servermaincofn: System error*n",
            taskid, modech, serno)
  abort(999)
}

AND workcofn(args) BE
{ // This is the body of a server work coroutine.

  LET wkno = args!0 // As supplied by initco in servermaincofn
  LET wrkcount = 0
  // wrkcount is also held in countv so that other workers can see it
  // when deciding whether to increment minwrkcount.
  countv!wkno := wrkcount

  // The server number is already set in serno and
  // modech is 'R' if the work coroutine belongs to a read server.
  // If modech is 'W' the work coroutine belongs to a write server.
  // Once initialised, the work coroutine is only given read, write
  // or die packets or replies from previous calls it made using
  // sendpkt

  { // Start of this worker's main loop.

    // First check that this worker's work count is small enough to
    // allow a request to be service.

    // workcount is the number of read or write packets this worker
    // has successfully processed.  It may never exceed the work count
    // of the least busy worker coroutine plus maxcountdiff
    // (typically=5).

    UNLESS wrkcount <= minwrkcount+maxcountdiff DO
      c_incwait := c_incwait+1 


    // Suspend this worker if necessary.
    UNTIL wrkcount <= minwrkcount+maxcountdiff | diepkt DO
      condwait(@countcondvar)

    // If the server receives a die packet, it is placed in diepkt
    // and all workers waiting on countcondvar are woken up
    // including this one.

    // This worker may now proceed.

    // The controller only sends a die packet when all read and write
    // clients have completed their work, and all workers are no
    // longer busy.  serverq and countcondvar will be both be zero.

    IF diepkt DO
    { c_cowait := c_cowait + 1
      cowait()                 // Return control to servermaincofn
      abort(9999)              // We should never reach this point
    } 

    // Wait for a read or write request

    UNTIL serverq DO
    { IF tracing DO
      { sawritef("*nT%z2 %cS%z2W%z2:*
                 * Not busy, busycount=%n:*n",
                  taskid, modech, serno, wkno, busycount)
      }

      //IF tracing DO
      //  sawritef("T%z2 %cS%z2W%z2:*
      //           * Calling condwait(%n) for condition pktcondvar*n",
      //            taskid, modech, serno, wkno, @pktcondvar)
      //IF tracing DO
      //  sawritef("T%z2 %cS%z2W%z2:*
      //           * busycount=%n*n",
      //            taskid, modech, serno, wkno, busycount)

      c_condwait := c_condwait+1
      condwait(@pktcondvar)
      // Indicate that this worker is busy processing a request
      busycount := busycount + 1

      IF tracing DO
      { sawritef("*nT%z2 %cS%z2W%z2:*
                 * Busy, busycount=%n:*n",
                  taskid, modech, serno, wkno, busycount)
      }
    }

    // There is a request packet in serverq

    { // Extract the first packet from serverq
      LET pkt = VALOF
      { LET p = serverq
        serverq := !p
        !p:= notinuse
        //IF tracing DO
        //  sawritef("T%z2 %cS%z2W%z2:*
        //           * packet de-queued from serverq*n",
        //            taskid, modech, serno, wkno)
        RESULTIS p
      }
      LET type   = pkt!pkt_type
      LET flag   = pkt!pkt_arg1
      LET clino  = pkt!pkt_arg2
      //This work coroutine already knows its serno (and modech).
      LET mpxno  = pkt!pkt_arg4
      LET chnno  = pkt!pkt_arg5
      LET data   = pkt!pkt_arg6  // Non zero, if it is a write request

      IF tracing DO
        sawritef("T%z2 %cS%z2W%z2:*
                 * Processing %c%cC%z2S%z2M%z2C%z2:%z4*n",
                  taskid, modech, serno, wkno,
                  flag, modech, clino, serno, mpxno, chnno, data)

      // The packet must be a read, write request.

      TEST modech='R'
      THEN { // Read request.

             // Send the request to a multiplexor
             IF tracing DO
               sawritef("T%z2 RS%z2W%z2: %cRC%z2S%z2M%z2C%z2:%z4*
                        *  Sending read request to multiplexor*n",
                        taskid, serno, wkno,
                        flag, clino, serno, mpxno, clino, 0)
             data := sendpkt(notinuse, mpxv!mpxno, type,
                             0, 0,     flag, clino, serno, mpxno, chnno, 0)

             // data=0 if the specified channel buffer was empty
             ///delay(10)

             UNLESS data DO
             { // The read failed so immediately return the read
               // request to the client.
               IF tracing DO
                 sawritef("T%z2 RS%z2W%z2: %cRC%z2S%z2M%z2C%z2:%z4*
                          * Returning failed read request to client*n",
                           taskid, serno, wkno,
                           flag, clino, serno, mpxno, clino, data)

               pkt!pkt_r1 := 0  // Indicate failure.
               c_qpkt := c_qpkt+1
               qpkt(pkt)        // Return the read packet to the read client.
               busycount := busycount - 1 // This worker is no longer busy.

               LOOP
             }

             IF tracing DO
               sawritef("T%z2 RS%z2W%z2: %cRC%z2S%z2M%z2C%z2:%z4*
                        *  Read request returned from multiplexor with data*n",
                        taskid, serno, wkno,
                        flag, clino, serno, mpxno, clino, data)
           }
      ELSE { // Write request
             LET rc = ?

             // Then send the request to the specified multiplexor
             IF tracing DO
               sawritef("T%z2 WS%z2W%z2: %cWC%z2S%z2M%z2C%z2:%z4*
                        * Sending write request to multiplexor %z2*n",
                         taskid, serno, wkno,
                         flag, clino, serno, mpxno, clino, data, mpxno)
             rc := sendpkt(notinuse, mpxv!mpxno, type,
                           0, 0,
                           flag, clino, serno, mpxno, chnno, data)
             //IF tracing DO
             //  sawritef("T%z2 WS%z2W%z2: %cWC%z2S%z2M%z2C%z2:%z4*
             //           * Write request returned from multiplexor rc=%n*n",
             //            taskid, serno, wkno,
             //            flag, clino, serno, mpxno, clino, data, rc)
             // rc = TRUE  if successful write
             // rc = FALSE if the channel was full

             UNLESS rc DO
             { // The write failed so immediately return the write
               // request to the client.
               IF tracing DO
                 sawritef("T%z2 WS%z2W%z2: %cWC%z2S%z2M%z2C%z2:%z4*
                          * Returning failed write request to client*n",
                           taskid, serno, wkno,
                           flag, clino, serno, mpxno, clino, data)

               pkt!pkt_r1 := FALSE // Indicate failure.
               c_qpkt := c_qpkt+1
               qpkt(pkt)           // Return the write packet.
               busycount := busycount - 1 // This worker is no longer busy.

               LOOP
             }
           }

      // The request was successful.

      IF flag='s' DO
      { // This request specified a server delay, so do it.

        IF tracing DO
          sawritef("T%z2 %cS%z2W%z2: %c%cC%z2S%z2M%z2C%z2:%z4*
                   * Server delay*n",
                    taskid, modech, serno, wkno,
                    flag, modech, clino, serno, mpxno, chnno, data)
        c_delaylong := c_delaylong+1
        delay(delaymsecs)
        IF tracing DO
          sawritef("T%z2 %cS%z2W%z2: %c%cC%z2S%z2M%z2C%z2:%z4*
                   * Server delay done*n",
                    taskid, modech, serno, wkno,
                    flag, modech, clino, serno, mpxno, chnno, data)
      }

      // A request has been successfully processed, so send a message
      // to the server's logger coroutine and check its reply.

      //IF tracing DO
      //  sawritef("*nT%z2 %cS%z2W%z2: Request loggerlock %n*n",
      //            taskid, modech, serno, wkno, @loggerlock)
      lock(@loggerlock)
      //IF tracing DO
      //  sawritef("T%z2 %cS%z2W%z2: Got  loggerlock %n*n",
      //            taskid, modech, serno, wkno, @loggerlock)

      // Send two random numbers in succession to the logger.
      { LET x = nextrnd(99)
        LET y = nextrnd(99)

        IF tracing DO
          sawritef("T%z2 %cS%z2W%z2: Sending %z2 and %z2 to logger*n",
                    taskid, modech, serno, wkno, x, y)
        cowrite(@loggerin, x)

        // Send a pkt to the bounce task to give other tasks a chance
        // to run.
        sendpkt(notinuse, bouncetaskid, act_bounce, 0,0)

        //IF tracing DO
        //  sawritef("T%z2 %cS%z2W%z2: Sending %z2 to logger, waiting for reply*n",
        //            taskid, modech, serno, wkno, y)
        cowrite(@loggerin, y)

        // Check the logger's reply is x+y.
        { LET reply = getloggerval()

          TEST reply=x+y
          THEN { IF tracing DO
                   sawritef("T%z2 %cS%z2W%z2:*
                            * Reply from logger = %n -- Correct*n",
                             taskid, modech, serno, wkno, reply)
               }
          ELSE { sawritef("T%z2 %cS%z2W%z2:*
                          * Reply from logger = %n -- Bad, should be %n*n",
                          taskid, modech, serno, wkno, reply, x+y)
                 abort(999)
               }
        }
      }

      //IF tracing DO
      //  sawritef("T%z2 %cS%z2W%z2: Freeing loggerlock %n*n*n",
      //            taskid, modech, serno, wkno, @loggerlock)
      unlock(@loggerlock)

      // Now increment wrkcount, incrementing minwrkcount if necessary.

      countv!wkno := wrkcount + 1
      c_inc := c_inc + 1

      IF wrkcount=minwrkcount DO
      { // minwrkcount must be incremented if this worker is the
        // only worker with wrkcount=mincount
        minwrkcount := minwrkcount+1
        FOR wrkn = 1 TO workmax IF countv!wrkn < minwrkcount DO      
        { // Restore previous minwrkcount
          minwrkcount := minwrkcount-1
          BREAK
        }
        wrkcount := wrkcount+1

        IF tracing DO
        { sawritef("T%z2 %cS%z2W%z2: Wrkcount=%n minwrkcount=%n*n",
                    taskid, modech, serno, wkno, wrkcount, minwrkcount)
          prcounts()
        }

        IF wrkcount = minwrkcount DO
        { // minwrkcount has just been incremented so
          // wake up all coroutines waiting on countcondvar
          IF tracing DO
            sawritef("T%z2 %cS%z2W%z2:*
                     * minwrkcount incremented so calling notifyAll %n*n*n",
                      taskid, modech, serno, wkno, @loggerlock)
          notifyAll(@countcondvar)
        }
      }

      // The request has been successfully processed, so
      // return the request packet to the client.
      IF tracing DO
        sawritef("T%z2 WS%z2W%z2: %cWC%z2S%z2M%z2C%z2:%z4*
                 * Returning successful request to client*n",
                  taskid, serno, wkno,
                  flag, clino, serno, mpxno, clino, data)

      TEST modech='R'
      THEN pkt!pkt_res1 := data  // Return the data value, or
      ELSE pkt!pkt_res1 := TRUE  // indicate a successful write.
      c_qpkt := c_qpkt+1
      qpkt(pkt)                  // Return the request packet to the client.

      busycount := busycount - 1 // This worker is no longer busy.
    }
  } REPEAT
}

AND prcounts() BE IF tracing DO
{ sawritef("T%z2 %cS%z2:    Wrkcounts  ",
            taskid, modech, serno)
  FOR wkno = 1 TO workmax DO
    sawritef(" W%z2/%n", wkno, countv!wkno)
  sawritef("   minwrkcount=%n*n", minwrkcount)
}

//******************* Multiplexor Task Body ****************************

AND startmpx(pkt) BE
{ // This is the body of a multiplexor task most of which runs
  // in multi-event mode using gomultievent.

  // pkt is the startup packet giving the mpx number.

  // The vectors mpxrdcov, mpxwrcov, rdbusyv, wrbusyv, rdwkqv, wrwkqv,
  // bufv, bufpv and bufqv each have one element per channel, and are
  // shared by the read and write coroutines.

  // bufv!chnno is the circular buffer (of chnbufsize elements) of
  // data waiting to be transferred via channel chnno of this mpx task.
  // bufpv!chnno is the next available write position in this buffer.
  // bufqv!chnno is the position in this buffer of the next value to
  // be read from the buffer. If bufpv!chnno=bufqv!chnno the circular
  // buffer is empty for channel chnno.

  // rdbusyv!chnno is TRUE if the read coroutine for channel chnno
  // is currently busy processing a read request.

  // wrbusyv!chnno is TRUE if the read coroutine for channel chnno
  // is currently busy processing a write request.

  // rdwkqv!chnno is the list of read packets waiting to be processed
  // by the read coroutine for channel chnno.

  // wrwkqv!chnno is the list of write packets waiting to be processed
  // by the write coroutine for channel chnno.

  // The mpx task processes the following packets:

  // act_startmpx  a1:mpxno

  //   This is the startup packet specifying the mpx number.
  //   It allocates and initialises all the vectors and channel
  //   buffers, and creates and starts all of the read and write
  //   coroutines for each of the channels for this multiplexor.

  // act_read a1:flag a2:clino a3:servno a4:mpxno a5:chnno.

  //   If the read coroutine for this channel is currently busy
  //   this request is inserted in its rdwkq, otherwise it is
  //   given to the read coroutine for this channel for processing.
  //   A read coroutine is busy when it is processing a read packet
  //   for its channel. If no data is currently available it may be
  //   busy for some time. It is only not busy when it is waiting
  //   for the next request.

  // act_write  a1:flag a2:clino a3:serno a4:mpxno a5:chnno a6:data.

  //   If the write coroutine for this channel is currently busy
  //   this request is inserted in its wrwkq, otherwise it is
  //   passed to the write coroutine for processing.
  //   If the buffer for the specified channel is not full, the write
  //   coroutine transfers the data into the buffer. If the buffer was
  //   empty, it wakes up a waiting read coroutine, if any. If a delay
  //   is specified it performs the delay. Finally, it returns the
  //   request packet to its server and waits for the next request.
  //   If the buffer is full, the write packet is returned, after a
  //   short delay, to its write server with an indication of failure
  //   in its res1 field.
  //   A write coroutine is only busy while it is processing a write
  //   request up to the time it returns the write packet to its server.
  //   It is not busy when it is waiting for the next request.

  // act.die    no arguments

  //   This is sent by the controller when all client tasks have
  //   completed all iterations of their schedules. It causes
  //   this mpx task to send its accumulated statistics to the
  //   stats task, deletes all the multiplexor coroutines and work
  //   space and then returns to DEAD state, just after returning
  //   the die packet.

  UNLESS pkt!pkt_type=act_startmpx DO
  { sawritef("T%z2: ERROR: startmpx packet expected*n")
    abort(999)
  }

  mpxno := pkt!pkt_arg1  // The number of this mpx task.

  mpxbusycount := 0      // The number of channel corroutines
                         // that are currently busy (ie not
                         // waiting for requests).

  startuppkt := pkt      // This global variable allows
                         // mpxmaincofn to return the startup
                         // packet to the controller when
                         // initialisation is complete.

  diepkt := 0            // This will contain the die packet
                         // when it is received.

  set_process_name("M%z2", mpxno)

  //IF tracing DO
  //  sawritef("T%z2 M%z2: Calling gomultievent*n", taskid, mpxno)

  gomultievent(mpxmaincofn, 1000)

fin:
  IF tracing DO
    sawritef("T%z2 M%z2:    Sending stats data to the stats task*n",
              taskid, mpxno)

  // Send this multiplexor's statistics to the stats task
  sendstats()

  IF tracing DO
    sawritef("T%z2 M%z2:*
             * Returning the die packet to the controller*n",
             taskid, mpxno)
  c_qpkt := c_qpkt+1
  qpkt(diepkt)

  IF tracing DO
    sawritef("T%z2 M%z2:   Returning to DEAD state*n",
              taskid, mpxno)

  RETURN  // Return this multiplexor to DEAD state
}

AND mpxmaincofn() BE
{ // This is the gomultievent main coroutine body for a multiplexor.
  // It creates and starts the channel's read and write coroutines
  // and at the end it deletes them. This coroutine runs in parallel
  // with these read and write coroutines.

  // Allocate all the multiplexor work space.

  mpxrdcov   := getvec(chnmax)
  mpxwrcov   := getvec(chnmax)
  rdbusyv    := getvec(chnmax)
  wrbusyv    := getvec(chnmax)
  rdwkqv     := getvec(chnmax)
  wrwkqv     := getvec(chnmax)
  bufv       := getvec(chnmax)
  bufpv      := getvec(chnmax)
  bufqv      := getvec(chnmax)
                               // waiting fo data.

  UNLESS mpxrdcov & mpxwrcov &
         rdbusyv  & wrbusyv &
         rdwkqv   & wrwkqv &
         bufpv    & bufqv &
         bufv     DO
  { sawritef("T%z2 M%z2: getvec failure*n", taskid, mpxno)
    GOTO fin
  }

  FOR chnno = 1 TO chnmax DO
  { mpxrdcov!chnno, mpxwrcov!chnno := 0, 0 
    rdbusyv!chnno, wrbusyv!chnno := FALSE, FALSE
    rdwkqv!chnno, wrwkqv!chnno := 0, 0
    bufv!chnno := 0
    bufpv!chnno, bufqv!chnno, bufv!chnno := 0, 0, 0
  }

  // Allocate all the channel buffers
  FOR chnno = 1 TO chnmax DO
  { LET buf = getvec(chnbufsize)
    UNLESS buf DO
    { sawritef("T%z2 M%z2C%z2: Unable to allocate a buffer*n",
                taskid, mpxno, chnno)
      GOTO fin
    }
    bufv!chnno := buf
    IF tracing DO
      sawritef("T%z2 M%z2C%z2:  Allocated buffer size=%n*n",
                taskid, mpxno, chnno, chnbufsize)
  }

  // Create and initialise all the channel read coroutines.

  FOR chnno = 1 TO chnmax DO
  { // The following call will create a channel read coroutine
    // leaving waiting for a read request.
    LET co = initco(mpxrdcofn, 1000, mpxno, chnno)
    UNLESS co DO
    { sawritef("T%z2 M%z2RC%z2: Unable to create read coroutine*n",
                taskid, mpxno, chnno)
      GOTO fin
    }
    c_callco := c_callco+1 // Because of callco in initco above
    mpxrdcov!chnno := co
    IF tracing DO
      sawritef("T%z2 M%z2RC%z2:*
               * Created and waiting for a read packet*n",
               taskid, mpxno, chnno)
//abort(6000)
  }

  // Create and initialise all the channel write coroutines.

  FOR chnno = 1 TO chnmax DO
  { // This creates a channel write coroutine leaving it
    // waiting in cowait for a multiplexor write packet delivered
    // by gomultievent.
    LET co = initco(mpxwrcofn, 1000, mpxno, chnno)
    UNLESS co DO
    { sawritef("T%z2 M%z2WC%z2: Unable to create write coroutine*n",
                taskid, mpxno, chnno)
      GOTO fin
    }
    c_callco := c_callco+1 // Because of callco in initco above
    mpxwrcov!chnno := co
    IF tracing DO
      sawritef("T%z2 M%z2WC%z2:*
               * Created and waiting for a write packet*n",
               taskid, mpxno, chnno)
//abort(6001)
  }

  IF tracing DO
    sawritef("T%z2 M%z2:*
             * All read and write coroutines created and ready*n*n",
             taskid, mpxno)

  c_qpkt := c_qpkt+1
  qpkt(startuppkt)


//sawritef("T%z2 M%z2: Entering its event loop*n")

  { // Start of this multiplexor's event loop

    // Get next packet from a server via gomultievent
    LET pkt  = ?
    LET type = ?

    // Get a new request from gomultievent
    mainco_ready := TRUE

    // mainco_ready is TRUE when this multiplexor is ready to
    // accept a read, write or die packets.

    c_cowait := c_cowait+1
    pkt  := cowait() // Wait for a read or write from a server
                     // via gomultievent(...),
                     // or a die packet from the controller.
    type := pkt!pkt_type

    mainco_ready := FALSE

    SWITCHON type INTO
    { DEFAULT:
        sawritef("T%z2 %cS%z2:*
                 * Unexpected packet received from %cC%z2*n",
                  taskid, modech, serno, modech, clino)
        abort(999)

      CASE act_read:
      { LET flag   = pkt!pkt_arg1
        LET clino  = pkt!pkt_arg2
        LET serno  = pkt!pkt_arg3
        LET mpxno  = pkt!pkt_arg4
        LET chnno  = pkt!pkt_arg5
        LET data   = pkt!pkt_arg6  // =0 for read requests

        IF tracing DO
          sawritef("T%z2 M%z2:*
                   *     %cRC%z2S%z2M%z2C%z2:%z4 Read request received*n",
                    taskid, mpxno,
                    flag, clino, serno, mpxno, chnno, data)

        // Insert this packet at head of the queue of
        // read work queue for this channel.

        pkt!pkt_link := rdwkqv!chnno
        rdwkqv!chnno := pkt

        IF tracing DO
          sawritef("T%z2 M%z2RC%z2:*
                   * Inserting read request at head of its rdwkq*n",
                   taskid, mpxno, chnno)

        UNLESS rdbusyv!chnno DO
        { c_callco := c_callco+1
          callco(mpxrdcov!chnno) // Wake up the channel read coroutine
                                 // if it was not busy.
        }
        // Process next read, write or die request.
        LOOP
      }

      CASE act_write:
      { LET flag   = pkt!pkt_arg1
        LET clino  = pkt!pkt_arg2
        LET serno  = pkt!pkt_arg3
        LET mpxno  = pkt!pkt_arg4
        LET chnno  = pkt!pkt_arg5
        LET data   = pkt!pkt_arg6  // =0 for read requests

        IF tracing DO
          sawritef("T%z2 M%z2:*
                   *     %cWC%z2S%z2M%z2C%z2:%z4 Write request received*n",
                    taskid, mpxno,
                    flag, clino, serno, mpxno, chnno, data)

        // Insert this packet at head of the queue of
        // write work queue for this channel.

        pkt!pkt_link := wrwkqv!chnno
        wrwkqv!chnno := pkt

        IF tracing DO
          sawritef("T%z2 M%z2WC%z2:*
                   * Inserting a write request at head of its wrwkq*n",
                   taskid, mpxno, chnno)

        UNLESS wrbusyv!chnno DO
        { c_callco := c_callco+1
          callco(mpxwrcov!chnno) // Wake up the channel write coroutine
                                 // if it was not busy.
        }
        // Process next read, write or die request.
        LOOP
      }

      CASE act_die:  // From the controller task
        IF tracing DO
          sawritef("T%z2 M%z2:*
                   * Die packet received*n",
                    taskid, mpxno)
        diepkt := pkt
        BREAK
    }
  } REPEAT

fin:

  // For safety check that none of the read or write
  // coroutines are busy and that their work queues are
  // empty, and that all channel buffers are empty.

  FOR chnno = 1 TO chnmax DO
  { IF bufpv & bufqv UNLESS bufpv!chnno=bufqv!chnno DO
    { sawritef("T%z2 M%z2C%z2: Channel buffer is not empty*n",
                taskid, mpxno, chnno)
      prbufs()
      abort(999)
    }
    IF rdwkqv & rdwkqv!chnno DO
    { sawritef("T%z2 M%z2C%z2: Read work queue is not empty*n",
                taskid, mpxno, chnno)
      abort(999)
    }
    IF wrwkqv & wrwkqv!chnno DO
    { sawritef("T%z2 M%z2C%z2: Write work queue is not empty*n",
                taskid, mpxno, chnno)
      abort(999)
    }
    IF rdbusyv & rdbusyv!chnno DO
    { sawritef("T%z2 M%z2C%z2: A read coroutine is still busy*n",
                taskid, mpxno, chnno)
      abort(999)
    }
    IF wrbusyv & wrbusyv!chnno DO
    { sawritef("T%z2 M%z2C%z2: A write coroutine is still busy*n",
                taskid, mpxno, chnno)
      abort(999)
    }
  }
//abort(1002)  

  // Delete all the read and write coroutines

  FOR chnno = 1 TO chnmax DO
  { IF mpxrdcov & mpxrdcov!chnno DO deleteco(mpxrdcov!chnno)
    IF mpxwrcov & mpxwrcov!chnno DO deleteco(mpxwrcov!chnno)
  }

  IF tracing DO
   sawritef("T%z2 M%z2:*
             * Returning all multiplexor work space to free store*n",
             taskid, mpxno)

  // Free all the channel buffers
  IF bufv FOR chnno = 1 TO chnmax IF bufv!chnno DO
    freevec(bufv!chnno)

  // Free all the vectors
  IF mpxrdcov   DO freevec(mpxrdcov)
  IF mpxwrcov   DO freevec(mpxwrcov)
  IF rdbusyv    DO freevec(rdbusyv)
  IF wrbusyv    DO freevec(wrbusyv)
  IF rdwkqv     DO freevec(rdwkqv)
  IF wrwkqv     DO freevec(wrwkqv)
  IF bufv       DO freevec(bufv)
  IF bufpv      DO freevec(bufpv)
  IF bufqv      DO freevec(bufqv)

  IF tracing DO
    sawritef("T%z2 M%z2:*
             * Cause gomultievent to return to single event mode*n",
             taskid, mpxno)

  multi_done := TRUE // Leave multi event mode
  c_cowait := c_cowait+1
  cowait()  // Return to mpxserver in single event mode.

  // This point should not be reached.
  sawritef("T%z2 M%z2: mpxmaincofn: System error*n", taskid, mpxno)
  abort(999)
}

AND mpxrdcofn(arg) BE
{ // This is the main function of a read coroutine for channel chnno.
  // It runs in multievent mode and is woken up by mpxmaincofn when
  // there is a request to process and it is not busy.

  LET mpxno, chnno = arg!0, arg!1
  LET buf   = bufv!chnno
  LET p, q  = ?, ?
  LET flag  = ?
  LET clino = ?
  LET serno = ?
  LET data  = ?
  LET pkt   = ?

  rdbusyv!chnno := TRUE // Only =FALSE when waiting to be woken up.
  mpxbusycount := mpxbusycount+1

  { // Start of the main loop for the read coroutine for this channel.

    // Get the next read packet, waiting if necessary.

    UNTIL rdwkqv!chnno DO
    { rdbusyv!chnno := FALSE
      mpxbusycount := mpxbusycount-1
      IF tracing DO
      { sawritef("T%z2 M%z2RC%z2:*
                 * Waiting for a read request*n",
                 taskid, mpxno, chnno)
      }
      c_cowait := c_cowait+1
      cowait()
      rdbusyv!chnno := TRUE
      mpxbusycount := mpxbusycount+1
    }

    // Dequeue the next read packet
    pkt := rdwkqv!chnno
    rdwkqv!chnno := pkt!pkt_link
    pkt!pkt_link := notinuse

    // Extract its parameters
    flag  := pkt!pkt_a1
    clino := pkt!pkt_a2
    serno := pkt!pkt_a3
    data  := pkt!pkt_a6

    p, q := bufpv!chnno, bufqv!chnno

    IF p=q DO
    { // The channel buffer is empty, so return the packet
      // to the read server with and indication of failure.
      IF tracing DO
      { prbufs()
        sawritef("T%z2 M%z2RC%z2: %cRC%z2S%z2M%z2C%z2:%z4*
                 * Returning failed read packet to its server*n",
                  taskid, mpxno, chnno, flag,
                  clino, serno, mpxno, chnno, data)
      }

      pkt!pkt_res1 := 0      // Indicate failure
      c_qpkt := c_qpkt+1
      qpkt(pkt)
      LOOP
    }

    pkt!pkt_res1 := buf!q
    bufqv!chnno := (q+1) MOD chnbufsize

    IF tracing DO
    { sawritef("T%z2 M%z2RC%z2:*
               * Data %z4 extracted from buffer position %n*n",
               taskid, mpxno, chnno, buf!q, q)
      prbufs()
    }

    // Conditionally perform a multiplexor delay
    IF flag='m' DO
    { IF tracing DO
        sawritef("T%z2 M%z2RC%z2: %cRC%z2S%z2M%z2C%z2:%z4*
                 * Channel delay*n",
                  taskid, mpxno, chnno, flag,
                  clino, serno, mpxno, chnno, data)
      c_delaylong := c_delaylong+1
      delay(delaymsecs)
      IF tracing DO
        sawritef("T%z2 M%z2RC%z2: %cRC%z2S%z2M%z2C%z2:%z4*
                 * Channel delay done*n",
                  taskid, mpxno, chnno, flag,
                  clino, serno, mpxno, chnno, data)
    }

    // Return the successful read request packet to its server.
    IF tracing DO
      sawritef("T%z2 M%z2RC%z2:*
               * %cRC%z2S%z2M%z2C%z2:%z4*
               * Returning successful read packet to its server*n",
                taskid, mpxno, chnno, flag,
                clino, serno, mpxno, chnno, data)

    c_qpkt := c_qpkt+1
    qpkt(pkt)

    // Process the next read packet, if any.
  } REPEAT
}

AND mpxwrcofn(arg) BE
{ // This is the main function of a write coroutine for channel chnno.
  // It runs in multievent mode and is woken up by mpxmaincofn when
  // there is a request to process and it is not busy.

  LET mpxno, chnno = arg!0, arg!1
  LET buf   = bufv!chnno
  LET p, q  = ?, ?
  LET r     = ?
  LET flag  = ?
  LET clino = ?
  LET serno = ?
  LET data  = ?
  LET pkt   = ?

  wrbusyv!chnno := TRUE // Only =FALSE when waiting to be woken up.
  mpxbusycount := mpxbusycount+1

  { // Start of the main loop for the write coroutine for this channel.

    // Get the next write packet, waiting if necessary.

    UNTIL wrwkqv!chnno DO
    { wrbusyv!chnno := FALSE
      mpxbusycount := mpxbusycount-1
      IF tracing DO
      { sawritef("T%z2 M%z2WC%z2:*
                 * Waiting for a write request*n",
                 taskid, mpxno, chnno)
      }
      c_cowait := c_cowait+1
      cowait()
      wrbusyv!chnno := TRUE
      mpxbusycount := mpxbusycount+1
    }

    // Dequeue the next write packet
    pkt := wrwkqv!chnno
    wrwkqv!chnno := pkt!pkt_link
    pkt!pkt_link := notinuse

    // Extract its parameters
    flag  := pkt!pkt_a1
    clino := pkt!pkt_a2
    serno := pkt!pkt_a3
    data  := pkt!pkt_a6

    IF tracing DO
      sawritef("T%z2 M%z2WC%z2: %cWC%z2S%z2M%z2C%z2:%z4*
               * Processing a write request*n",
                taskid, mpxno, chnno, flag,
                clino, serno, mpxno, chnno, data)

    p, q := bufpv!chnno, bufqv!chnno
    r := (p + 1) MOD chnbufsize

    IF r=q DO
    { // The channel buffer was full, so return the packet
      // to the write server with and indication of failure.
      IF tracing DO
      { prbufs()
        sawritef("T%z2 M%z2WC%z2: %cWC%z2S%z2M%z2C%z2:%z4*
                 * Returning failed write packet to its server*n",
                  taskid, mpxno, chnno, flag,
                  clino, serno, mpxno, chnno, data)
      }

      pkt!pkt_res1 := 0      // Indicate failure
      c_qpkt := c_qpkt+1
      qpkt(pkt)
      LOOP
    }

    pkt!pkt_res1 := TRUE  // Indicate successful write
    buf!p := data
    bufpv!chnno := r

    IF tracing DO
    { sawritef("T%z2 M%z2WC%z2: %cWC%z2S%z2M%z2C%z2:%z4*
               * Data pushed into channel buffer*n",
                taskid, mpxno, chnno, flag,
                clino, serno, mpxno, chnno, data)
      prbufs()
    }

    // Conditionally perform a multiplexor delay
    IF flag='m' DO
    { IF tracing DO
        sawritef("T%z2 M%z2WC%z2: %cWC%z2S%z2M%z2C%z2:%z4*
                 * Multiplexor delay*n",
                  taskid, mpxno, chnno, flag,
                  clino, serno, mpxno, chnno, data)
      c_delaylong := c_delaylong+1
      delay(delaymsecs)
      IF tracing DO
        sawritef("T%z2 M%z2WC%z2: %cWC%z2S%z2M%z2C%z2:%z4*
                 * Multiplexor delay done*n",
                  taskid, mpxno, chnno, flag,
                  clino, serno, mpxno, chnno, data)
    }

    // Return the successful write request packet to its server.
    IF tracing DO
      sawritef("T%z2 M%z2WC%z2:*
               * %cWC%z2S%z2M%z2C%z2:%z4*
               * Returning successful write packet to its server*n",
                taskid, mpxno, chnno, flag,
                clino, serno, mpxno, chnno, data)

    c_qpkt := c_qpkt+1
    qpkt(pkt)

    // Process the next write packet, if any.
  } REPEAT
}

AND prbufs() BE //IF tracing DO
{ FOR chnno = 1 TO chnmax DO
  { LET p = bufpv!chnno
    LET q = bufqv!chnno
    LET empty = p=q
    LET buf = bufv!chnno
    sawritef("    M%z2C%z2:  Buffer: ", mpxno, chnno)
    UNTIL p=q DO
    { LET val = buf!q
      sawritef("%z4 ", val) // data
      q := (q+1) MOD chnbufsize
    }
    sawritef("*n")

    IF rdwkqv!chnno DO
    { LET p = rdwkqv!chnno
      sawritef("   M%z2C%z2: %s:", mpxno, chnno, empty->"rdrq", "wrrq")
      WHILE p DO
      { LET type = p!pkt_type
        LET ch = type=act_read  -> 'R',
                 type=act_write -> 'W',
                 '?'
        sawritef(" %cS%z2", ch, p!pkt_arg3) // serno
        UNLESS empty DO sawritef("/%z4", p!pkt_arg6)
        p := !p
      }
      sawritef("*n")
    }
  }
//abort(1000)
}

/* cptr := findpkt(pkt)

This routine searches pktlist for the specified packet. If found,
the list item is dequeued and the corresponding coroutine pointer is
returned. It returns zero if the packet is not found in pktlist.
*/

AND findpkt(pkt) = VALOF
{ LET a = @pktlist

  // Search pktlist for the relevant item
  { LET p = !a
    UNLESS p RESULTIS 0   // Not found

    IF p!1 = pkt DO
    { !a := !p            // Remove from pktlist
      RESULTIS p!2        // The coroutine
    }
    a := p
  } REPEAT
}

/* res := sndpkt(link, id, act, r1, r2, a1, a2, a3, a4, a5, a6)

This routine is a substitute sendpkt for multi-event tasks.  It can
only be called when running as a non root coroutine of a task.  A
packet-coroutine pair is placed in pktlist before dispatching the
packet (using qpkt) so that, when the packet returns to this task,
this coroutine can be reactivated.

Multi-event mode is entered by executing

     pktlist, sendpkt := 0, sndpkt

Re-implemented by MR 7/6/02, futher modified MR 7/5/03
*/

AND sndpkt(link, id, act, res1, res2, a1, a2, a3, a4, a5, a6) = VALOF
{ // The following variables form the pktlist node.
  // Functions other than sndpkt may
  // manipulate pktlist so its format must not change.
  LET next, pkt, co = pktlist, @link, currco

  // Safety check -- sndpkt cannot be called from the root coroutine
  IF currco!co_parent<=0 DO
  { sawritef(
     "DLIB co=%n T%n: can't sndpkt %n(id=%n,type=%n) from root coroutine*n",
       currco, taskid, pkt, id, act)
    abort(999)
  }

  pktlist := @next       // Insert it at the head of pktlist

  c_qpkt := c_qpkt+1
  UNLESS qpkt(pkt) DO
  { sawritef("co=%n: sndpkt -- qpkt failure*n", currco)
    abort(181)
  }

  { LET p = ?
    c_cowait := c_cowait+1
    p := cowait() // Safety check -- we must resume with the

    IF p=pkt BREAK   // expected packet.
    sawritef("co=%n T=%n: sndpkt: received wrong pkt=%n should be %n*n",
              currco, taskid,  p, pkt)
    abort(182)
  } REPEAT

  result2 := res2
  RESULTIS res1
}

// This is the single event mode version of sendpkt. It overrides
// the definition in DLIB so that it can gather usage statistics.

AND sendpkt(link,id,type,res1,res2,a1,a2,a3,a4,a5,a6) = VALOF
{ c_qpkt := c_qpkt+1
  UNLESS qpkt(@link) DO
  { sawritef("T%z2: System error in sendpkt, id=%n type=%n*n", taskid, id, type)
    abort(181)
    result2 := 0
    RESULTIS 0
  }

  c_taskwait := c_taskwait+1
  IF taskwait() = @link DO
  { result2 := res2
    RESULTIS res1
  }

  sawritef("T%z2: System error in sendpkt: Bad packet received*n", taskid)
  abort(182)
  result2 := 0
  RESULTIS 0
}

AND gomultievent(maincofn, stacksize) BE
{ LET mainco = createco(maincofn, stacksize)
  LET oldsendpkt = sendpkt
  LET queue  = 0   // Queue of packets waiting to be processed by mainco.

  UNLESS mainco RETURN

  sendpkt, pktlist := sndpkt, 0 // Enter multi-event mode

  multi_done   := FALSE // TRUE when it is time to return to single
                        // event mode.
  mainco_ready := FALSE // =TRUE when mainco is ready to process
                        // packets. If FALSE, packets are appended
                        // to queue by gomultievent and given to
                        // mainco later.

  // Start up mainco.

  c_callco := c_callco+1
  callco(mainco, 0)     // Tell mainco to initialise itself
                        // by allocating workspace and creating
                        // coroutines as necessary.

  //IF tracing DO
  //  sawritef("*nT%n: gomultievent entering its event loop*n", taskid)

  UNTIL multi_done DO      // Multi-event loop
  { LET pkt  = taskwait()
    LET cptr = findpkt(pkt)
    c_taskwait := c_taskwait+1

    TEST cptr
    THEN { // cptr is the multi-event coroutine waiting for this packet.
           //IF tracing DO
           //  sawritef("T%z2 gomultievent:*
           //           * giving control to the coroutine waiting for pkt=%n*n",
           //            taskid, pkt)
           c_callco := c_callco+1
           callco(cptr, pkt) // Give pkt to this coroutine
         }
    ELSE { UNLESS mainco_ready DO
           { // This packet does not belong to a multi-event
             // coroutine and mainco is busy, so append it
             // onto the end of queue and wait for another
             // packet.
             // Note that this code is only obeyed if mainco
             // is suspended waiting for a particular packet,
             // as in a call of delay. In tcobench this never
             // happens.
             LET p = @queue
             WHILE !p DO p := !p
             !pkt, !p := 0, pkt
             LOOP
            }
            // This packet does not belong to a multi-event
            // coroutine and mainco is not busy, so give it
            // to mainco.
            c_callco := c_callco+1
            callco(mainco, pkt) // Give the packet to mainco.
          }

    // Try to deal with the packets in queue.
    WHILE queue & mainco_ready DO
    { pkt := queue       // De-queue a pending packet
      queue := !pkt
      !pkt := notinuse
      c_callco := c_callco+1
      callco(mainco, pkt) // Give it to mainco
    }
  } // End of UNTIL multi_done loop.

  // Return to single-event mode

  // All coroutines and work space created by mainco must have
  // been deleted.

  IF tracing DO
    sawritef("*nT%n:*
             * gomultievent task %n returning to single event mode*n",
              taskid, taskid)

  sendpkt := oldsendpkt

  // Delete the coroutines created by gomultievent.
  deleteco(mainco)
}

AND cowrite(ptr, data) BE
{ LET cptr = !ptr
  TEST cptr
  THEN { !ptr := 0       // Clear the channel word
       }
  ELSE { !ptr := currco  // Set the channel word to this coroutine
         // Wait for the pointer to the reading coroutine
         c_cowait := c_cowait+1
         cptr := cowait()
        }
  // cptr points to the reading coroutine which is
  // guaranteed to be inactive.
  c_callco := c_callco+1
  callco(cptr, data) // Send the data to coread
}

// coread and cowrite provide Occum style channel communication
// used by servers communicating with their logger coroutines.
AND coread(ptr) = VALOF
{ LET cptr = !ptr

  IF cptr DO
  { !ptr := 0         // Clear the channel word
    c_resumeco := c_resumeco+1
    RESULTIS resumeco(cptr, currco)
  }

  !ptr := currco      // Set channel word to this coroutine
  c_cowait := c_cowait+1
  RESULTIS cowait()   // Wait for value from cowrite
}

// Locks are represented by simple variables such as loggerlock.
// The possible lock values are:
//   0            The lock is currently unlocked.
//  -1            The lock is locked by just one coroutine
//                which is not necessarily the current coroutine.
//                Note that the coroutine that owns the lock
//                may have passed control to another coroutine.
//                For example, a worker coroutine may have given
//                control to the logger coroutine using cowrite.
// ~=0 & ~=-1     The value points to the first node of a list
//                of coroutines waiting on the lock. A typical
//                node in the list is [next, co] where next
//                points to the next node in the list, or is zero,
//                and co is a coroutine waiting on the lock.

// The arguments to both lock and unlock must be pointers to
// lock variables, eg lock(&loggerlock) and unlock(@loggerlock).

AND lock(ptr) BE
{ c_lock := c_lock+1   // Gather locking statistics

  TEST !ptr
  THEN { // The lock was locked
         LET link, cptr = 0, currco // Create a lock node [link, cptr]
         TEST !ptr=-1
         THEN { !ptr := @link       // It was locked by just one
                                    // coroutine, so make a unit list.
              }
         ELSE { LET p = !ptr        // Othewise append the lock node
                WHILE !p DO p := !p // to the end of the list.
                !p := @link
              }
         c_lockw := c_lockw+1       // Gather locking statistics
         //IF tracing DO
         //  sawritef("T%z2 co %n lock   %n:*
         //           * Waiting for the lock*n",
         //           taskid, currco, ptr)
         c_cowait := c_cowait+1
         cowait() // Suspend until unlock(..) called
         //IF tracing DO
         //  sawritef("T%z2 co %n lock   %n:*
         //           * Just given the lock*n",
         //           taskid, currco, ptr)
         // We now own the lock and !ptr will be non zero
       }
  ELSE { !ptr := -1 // Mark as locked by just one coroutine
         //IF tracing DO
         //  sawritef("T%z2 co %n lock   %n:*
         //           * Obtained the lock*n",
         //           taskid, currco, ptr, currco)
       }
}

AND unlock(ptr) BE
{ LET node = !ptr
  UNLESS node DO
  { sawritef("%z2 co %n:*
             * Attempt to free a lock that is not locked*n",
             taskid, currco)
    abort(999)
    RETURN
  }
  TEST node=-1
  THEN { !ptr := 0 // The lock was owned by the current coroutine
                   // so free it.
         //IF tracing DO
         //  sawritef("T%z2 co %n unlock %n:*
         //           * Freeing lock*n", taskid, currco, ptr)
       }
  ELSE { // Release a coroutine waiting on this lock
         LET next, cptr = node!0, node!1
         !ptr := next -> next, -1
         //IF tracing DO
         //  sawritef("T%z2 co %n unlock %n:*
         //           * Releasing coroutine %n*n",
         //           taskid, currco, ptr, cptr)
         c_callco := c_callco+1
         callco(cptr)
         //IF tracing DO
         //  sawritef("T%z2 co %n unlock %n:*
         //           * Resuming execution*n",
         //           taskid, currco, ptr)
       }
}

// Typical usage of wait, notify and notifyAll is:

// UNTIL <complicated condition> DO condwait(@countcondvar)

// When something happens that may change the condition
// notify(@countcondvar) or notifyAll(@countcondvar) is called.

// A variant of condwait, notify and notifyAll could save and
// restore the current input and output stream selections, but
// this is not necessary in this benchmark.

// Several coroutines can wait on the same condition.

AND condwait(condvarptr) BE
{ LET link, cptr = !condvarptr, currco // Form a node [link, cptr]
  c_condwait := c_condwait+1  // Gather statistics
  !condvarptr := @link
  c_cowait := c_cowait+1
  cowait()                    // Suspend until the waiting condition
}

AND notifyAll(condvarptr) BE
{ // Wakeup all coroutines waiting on the specified condition.
  LET p = !condvarptr

  c_notifyAll := c_notifyAll+1

  !condvarptr := 0 // Clear the wait list since some of released
                   // coroutines may immediately wait on the
                   // same condition.

  WHILE p DO { // Give control to every coroutine, in turn, that
               // is waiting on this condition.
               LET cptr = p!1
               p := !p
               c_callco := c_callco+1
               callco(cptr)
             }
}

AND notify(condvarptr) BE
{ // Wakeup one coroutine, if any, waiting on the specified condition.
  LET p = !condvarptr

  c_notify := c_notify+1

  UNLESS p RETURN // No coroutines waiting on this condition

  // There is at least one coroutine waiting on this condition.
  !condvarptr := !p  // Dequeue the first item

  // Release the first waiting coroutine.
  c_callco := c_callco+1
  callco(p!1)
}

// Return a machine independent random number between 1 and max
AND nextrnd(max) = VALOF
{ MANIFEST {
    //feedback = #xD008    // 16 15 13 4  => period 2^16-1
    feedback = #x80200003  // 32 22  2 1  => period 2^32-1
    // #x80200003 = 1000 0000 0010 0000 0000 0000 0000 0011
    //              |           |                        ||
    //              |           |                        |1
    //              32          22                       2
  }
  TEST (seed&1)=0
  THEN seed := seed>>1
  ELSE seed := seed>>1 XOR feedback
  RESULTIS (seed>>1) MOD max + 1
}

AND setseed(a) BE
{ seed := a | 1
  FOR i = 1 TO nextrnd(50) + 10 DO nextrnd(1000)
}




