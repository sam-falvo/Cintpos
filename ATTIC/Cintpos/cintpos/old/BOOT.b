SECTION "BOOT"

GET "libhdr"

GLOBAL {
sadebug1:ug
checkaddr
cont
debug
error
gb
gh
gsb
gsh
gw
instrtype
isfun
nextpc
praddr
prinstr
print
rdval
rdvaraddr
rch
wrcortn
wrframe
writearg

bpt
bpt_addr
bpt_instr
brkstep
ch
cptr
fsize
gptr
lch
membase
memlim
oldcount
pptr
recp
recl
regs       // The current register set
style
snglstep
val
vars
crntcb
standalone
rdflag
wrflag
wwrch
}

STATIC { debugstarted = FALSE }

MANIFEST
{
g_globsize=0; g_sys=3; g_currco=7; g_colist=8 

globword  = #xEFEF0000
entryword = #x0000DFDF

f_brk   = 2

r_a     = 0
r_b     = 1
r_c     = 2
r_p     = 3
r_g     = 4
r_st    = 5
r_pc    = 6
r_count = 7
r_upb   = 7

tasktabupb = 100
devtabupb  = 1000
}

// Most the code in this file is obeyed using the boot stack and
// boot global vector. On entry to start the boot global vector
// is already initialised with all globals defined in:
// BOOT
// BLIB
// SYSLIB, KLIB, FLIB, IOLIB,
// The stack is already cleared.

LET start(fn, size, c) BE
// This is the very first BCPL code to be entered (from cintpos)
// Its stack and globals are already setup
// and sys is in rootnode!rtn.sys
{ LET p = sagetvec(1006)  // The KLIB stack
  LET g = sagetvec(1000)  // The KLIB global vector
  g!0 := 1000             // Set KLIB's globsize

  sawritef("*nCintpos System (12 Sept 2003)*n")

  // Make the boot stack into a coroutine
  currco := @fn-3
  colist := currco
  currco!co.pptr     :=  0
  currco!co.parent   := -1      // Mark as root coroutine
  currco!co.list     :=  0
  currco!co.fn       :=  start  // These are the same locations as fn
  currco!co.size     :=  1000   //                                 size
  currco!co.c        :=  0      //                             and c


//sawritef("boot1*n")

  // Initialise the standalone debugger
  membase      := rootnode!rtn.membase
  memlim       := membase + rootnode!rtn.memsize
  vars         := TABLE 0,0,0,0,0,0,0,0,0,0
  bpt_addr     := TABLE 0,0,0,0,0,0,0,0,0,0
  bpt_instr    := TABLE 0,0,0,0,0,0,0,0,0,0
  crntcb       := 0

  // Breakpoints may be set and cleared by the DEBUG task so
  // bpt_addr and bpt_instr must be accessible to it
  rootnode!rtn.bptaddr  := bpt_addr
  rootnode!rtn.bptinstr := bpt_instr

  style := 'F'                   // Default printing style
  val := 0
  brkstep, snglstep := FALSE, FALSE

  // Get ready to enter KLIB
  FOR i = 0 TO 1006 DO p!i := 0

//sawritef("boot2*n")
  klibregs!r_a     := 0          // A
  klibregs!r_b     := 0          // B
  klibregs!r_c     := 0          // C
  klibregs!r_p     := p<<2       // P
  klibregs!r_g     := g<<2       // G
  klibregs!r_st    := 1          // Interrupts disabled (in KLIB)
  klibregs!r_pc    := startklib  // PC
  klibregs!r_count := -1         // Count  (-1 = infinity)

{ LET sw = 2  // 1=cintasm  2=interpret
  klibregs!r_count := sw=1 -> -1, 1_000_000_000
}


//sawritef("boot3*n")
//sys(Sys_tracing, TRUE)

  // The following assigment gives BOOT a private copy of sys (SYS RTN)
  // that will work on BIG and LITTLE ender machines!!
  // It allows the calls of sys in the following loop to work even if
  // a breakpoint has been set in the normal version of sys.

  sys := 4*TABLE #x917B7B91  // 91=SYS and 7B=RTN

  { LET res = sys(Sys_interpret, klibregs)
sys(Sys_tracing, FALSE)
//sawritef("*nboot4 res=%n*n", res)
//FOR i = 0 TO 7 DO sawritef("R%n = %n*n", i, klibregs!i)
    IF res=0 DO sys(Sys_quit, 0)
    IF res=-1 LOOP  // Re-enter the interpreter immediately
    val := klibregs!r_pc
//sawritef("boot5 val=%n*n", val)
//sys(Sys_quit,res)
    UNLESS sadebug(res, klibregs) DO
    { sawritef("Standalone debug could not cope*n")
      sys(Sys_quit, res)
    }
  } REPEAT
}

AND startklib(fn, size, c) BE
{ // Make the kernel stack into a coroutine
  // Entered with ST=1  ie interrupts disabled (in KLIB)

  LET p = @fn - 3
  LET g = @globsize

  // g is the base of KLIB's own global vector
  // g!0 (=globsize) is the size of KLIB's global vector
  // g!0 is the only element containing information
  // Initialise all the others to their respective unset values
  FOR i = 1 TO g!0 DO g!i := globword + i

  // Now begin to initialise the kernel globals 
  sys := rootnode!rtn.sys     // Get the sys function from the rootnode
//sys(Sys_tracing, TRUE)
  // Setup the coroutine environment
  currco := p 
  colist := currco
  currco!co.pptr     :=  0
  currco!co.parent   := -1      // Mark as root coroutine
  currco!co.list     :=  0
  currco!co.fn       :=  start  // These are the same locations as fn
  currco!co.size     :=  1000   //                                 size
  currco!co.c        :=  0      //                             and c
//sys(Sys_tracing, FALSE)

  // There is no current task yet
  crntcb := 0

  // Set all resident library globals entry points
  sys(Sys_globin, rootnode!rtn.klib)  // globin(klib)
  sys(Sys_globin, rootnode!rtn.blib)  // globin(blib)

  // The KLIB's global vector and coroutine environment are now setup

//sawritef("startklib*n") // Cannot call sawritef earlier!!

  rdch, wrch := sardch, sawrch              // Use standalone versions
  getvec, freevec := sagetvec, safreevec    // for now.

  // Setup tasktab, devtab and the initial tasks

//sawritef("startklib: initialising the kernel data structure*n")
  initkernel()

// Now start Tripos running by entering the scheduler, giving
// it the highest priority tcb -- It will actually run IDLE which
// will send a packet to task 1 (CLI).

//  sawritef("startklib: calling srchwk(%n)*n", rtn.tcblist!rootnode)
  srchwk(rtn.tcblist!rootnode)

  // This should code should never be reached.
  sawritef("Unexpected return from srchwk*n")
  sys(Sys_quit, 0)
}

AND initkernel() BE
{ LET g = @ globsize            // Get KLIB's global vector
  LET tasktab, devtab = 0, 0
  LET klib = rtn.klib!rootnode  //  holds SYSLIB, KLIB etc
  LET blib = rtn.blib!rootnode  //  holds BLIB, etc
  LET tcb, dcb = 0, 0

  FOR r = r_a TO r_upb DO
  { saveregs!r  := 0 // Registers at time of last interrupt
    regsint!r   := 0 // Interrupt service routine registers
  }

  // Now create the TRIPOS Kernel Data Structure

  // Create the task table
  tasktab := getvec(tasktabupb)
  tasktab!0 := tasktabupb
  FOR i = 1 TO tasktabupb DO tasktab!i := 0
  rtn.tasktab!rootnode := tasktab

  // Create the device table
  devtab := getvec(devtabupb)
  devtab!0 := devtabupb
  FOR i = 1 TO devtabupb DO devtab!i := 0
  rtn.devtab!rootnode := devtab

  // Create the clock device (-1)
  dcb := getvec(Dcb_upb)
  FOR i = 0 TO Dcb_upb DO dcb!i := 0
  Dcb_type!dcb := Devt_clk
  Dcb_intson!dcb := TRUE
  rootnode!rtn.clkintson := FALSE // It will be turned on later
  createdev(dcb)

  // Create of the keyboard device (-2)
  dcb := getvec(Dcb_upb)
  FOR i = 0 TO Dcb_upb DO dcb!i := 0
  Dcb_type!dcb := Devt_ttyin
  Dcb_intson!dcb := TRUE
  createdev(dcb)

  // Create of the display device (-3)
  dcb := getvec(Dcb_upb)
  FOR i = 0 TO Dcb_upb DO dcb!i := 0
  Dcb_type!dcb := Devt_ttyout
  Dcb_intson!dcb := TRUE
  createdev(dcb)


// Set interrupt service routine registers
  regsint!r_a     := 0
  regsint!r_b     := 0
  regsint!r_c     := 0
  regsint!r_p     := getvec(500) << 2
  regsint!r_g     := g << 2            // Share the kernel Gvec
  regsint!r_st    := 3                 // Interrupts disabled (in ISR)
  regsint!r_pc    := irqrtn            // Interrupt service routine
  regsint!r_count := -1

// Initialise the other rootnode fields
  rtn.tcblist!rootnode   := 0
  rtn.crntask!rootnode   := 0
  rtn.blklist!rootnode   := 0
  rootnode!rtn.clkintson := FALSE // It will be turned on later
  rtn.clwkq!rootnode     := 0
  rtn.info!rootnode      := 0
  rtn.info!rootnode      := 0

// Make the Idle task
  createtask(mksegl(3, klib, blib, saloadseg("syscin/IDLE")), #x39EC+1, 0) 
//  createtask(mksegl(3, klib, blib, saloadseg("syscin/IDLE")), 200, 0) 
  tcb := tasktab!1          // It must have created task 1
  tasktab!1 := 0            // remove its entry from tasktab
  tcb.taskid!tcb := 0       // set the IDLE task id to zero
  tcb.wkq!tcb := TABLE 0,0  // pkt to start the IDLE task
  tcb.state!tcb := #b1101   // set state to DEAD with PKT

// Make the other resident tasks

// Task 1 -- The CLI
  createtask(mksegl(4, klib, blib, saloadseg("syscin/CLI_INIT"),
                                   saloadseg("syscin/CLI")),      1000, 1000) 

// Task 2 -- The Debug Task
  createtask(mksegl(3, klib, blib, saloadseg("syscin/DEBUG")),    1000, 9800) 

// Task 3 -- The Console Handler
  createtask(mksegl(3, klib, blib, saloadseg("syscin/COHAND")),   1000, 9900) 

// Task 4 -- The File Handler
  createtask(mksegl(3, klib, blib, saloadseg("syscin/FH0")),      1000, 9400) 

// Task 5 -- The MBX Handler
  createtask(mksegl(3, klib, blib, saloadseg("syscin/MBXHAND")),  1000, 9300) 

// Task 6 -- The TCP Handler
  createtask(mksegl(3, klib, blib, saloadseg("syscin/TCPHAND")),  1000, 9600) 

  rtn.clkintson!rootnode := TRUE // At last we can turn on the clk

//prrootnode()
//prtasks()
}

// All interrupts are handled by this interrupt routine.
// It runs using the kernel global vector but has its own
// stack of 500 words. Its argument is the id of the
// interrupting device.

AND irqrtn(devid) BE TEST devid=-1 // Treat the clock specially
THEN 
{ // Decrement res1 field of leading clk pkt if any and send to
  // its task if res1=0. If further clk pkts have res1<=0 they should
  // also be released.
  LET pkt = rtn.clwkq!rootnode
//sawritef("irqrtn: clk interrupt, clkwkq = %n*n", pkt)
  IF pkt DO
  { LET ticks = pkt.res1!pkt - 1
    pkt.res1!pkt := ticks
//sawritef("irqrtn: first pkt res1 = %n*n", pkt.res1!pkt)
    IF ticks<=0 DO
    { // At least one clk pkt is to be released
      LET ctcb = rtn.crntask!rootnode
      LET htcb = ctcb
      WHILE pkt & pkt.res1!pkt<=0 DO
      { LET npkt = pkt.link!pkt         // Release all
        htcb := movepkt(pkt, -1, htcb)  // expired pkts
        pkt := npkt
      }
      rtn.clwkq!rootnode := pkt
      UNLESS ctcb=htcb DO { interrupttask(ctcb)
                            srchwk(htcb)
                          }
    }
  }

  // No need to reschedule so continue running the current task
  sys(Sys_rti, saveregs)
}
ELSE
{ LET n = -devid  // Deal with interrupt from non-clock device
  LET dcb = 0
  LET devtab = rtn.devtab!rootnode
  
  IF 0<n<=devtab!0 DO dcb := devtab!n
  //IF n=4 DO sawritef("irqrtn: processing interrupt for device %n*n", devid)

  IF dcb DO
  { LET pkt = Dcb_wkq!dcb
    IF pkt DO
    { LET ctcb = rtn.crntask!rootnode
      LET htcb = ctcb
      LET npkt = pkt.link!pkt

      Dcb_wkq!dcb := npkt    // Dequeue the pkt
      IF npkt DO
      { //sawritef("irqrtn: starting device for next pkt on wkq*n")
        sys(Sys_devcom, dcb, Devc_start)  // Start processing the next pkt
      }
      // send packet back to sender
      htcb := movepkt(pkt, devid, htcb)
      UNLESS ctcb=htcb DO { interrupttask(ctcb)
                            srchwk(htcb)
                          }
    }
  }

  // No need to reschedule so continue running the current task
  sys(Sys_rti, saveregs)
}


// All interrupt routines use the kernel global vector but
// each have their own stack of 500 words.

// Movepkt(pkt, senderid, htcb) may only be called from an
// interrupt routines.  On entry, the interrupt status must be 3.
// It appends the pkt to the wkq of its destination task.
// It updates the id field of the pkt with senderid.
// If the destination task had no packets and is of higher
// priority than that of htcb, then movepkt returns the
// the destination tcb, otherwise it returns htcb.
// If the destination task does not exist it returns htcb.

AND movepkt(pkt, senderid, htcb) = VALOF
{ LET tasktab = rtn.tasktab!rootnode
  LET destid = pkt.id!pkt
  LET tcb = ?
  UNLESS 0<destid<=tasktab!0 RESULTIS htcb
//IF senderid=-2 DO sawritef("movepkt from %n to %n*n", senderid, destid)
  tcb := tasktab!destid
  UNLESS tcb RESULTIS htcb
  pkt.link!pkt := 0
  pkt.id!pkt := senderid

  UNLESS tcb.wkq!tcb DO
  { tcb.wkq!tcb := pkt // the wkq was empty
    tcb.state!tcb := tcb.state!tcb + #b0001
    IF tcb.pri!tcb > tcb.pri!htcb RESULTIS tcb
    RESULTIS htcb
  }

  // the wkq was not empty, so append the pkt
  { LET p = tcb.wkq!tcb
    WHILE pkt.link!p DO p := pkt.link!p
    pkt.link!p := pkt
  }

  RESULTIS htcb
}

// Interrupttask may only called from interrupt routines.
// It puts the current task into interrupted state (100X)
// giving it the registers that are in rootregs.

AND interrupttask(tcb) BE
{ tcb.state!tcb := tcb.state!tcb | #b1000

  tcb.a !tcb    := r_a !saveregs
  tcb.b !tcb    := r_b !saveregs
  tcb.c !tcb    := r_c !saveregs
  tcb.p !tcb    := r_p !saveregs
  tcb.g !tcb    := r_g !saveregs
  tcb.st!tcb    := r_st!saveregs
  tcb.pc!tcb    := r_pc!saveregs
  tcb.count!tcb := r_count!saveregs
}

AND prrootnode() BE
{ writef("Rootnode at %n:*n", rootnode)
  writef("  tasktab    %i8*n", rtn.tasktab!rootnode)
  writef("  devtab     %i8*n", rtn.devtab!rootnode)
  writef("  tcblist    %i8*n", rtn.tcblist!rootnode)
  writef("  crntask    %i8*n", rtn.crntask!rootnode)
  writef("  blklist    %i8*n", rtn.blklist!rootnode)
  writef("  clkintson  %i8*n", rtn.clkintson!rootnode)
  writef("  clwkq      %i8*n", rtn.clwkq!rootnode)
  writef("  memsize    %i8*n", rtn.memsize!rootnode)
  writef("  info       %i8*n", rtn.info!rootnode)
  writef("  sys        %i8*n", rtn.sys!rootnode)
  writef("  blib       %i8*n", rtn.blib!rootnode)
  writef("  boot       %i8*n", rtn.boot!rootnode)
  writef("  klib       %i8*n", rtn.klib!rootnode)
}


AND prtasks() BE
{ LET t = rtn.tcblist!rootnode
  UNTIL t=0 DO
  { LET seglist = tcb.seglist!t
    LET pkt = tcb.wkq!t
    writef("tcb at %i4:*n", t)
    writef(" taskid %i4", tcb.taskid!t)
    writef(" wkq %i4", tcb.wkq!t)
    writef(" pri %i4", tcb.pri!t)
    writef(" state %i4", tcb.state!t)
    writef(" stsiz %i4", tcb.stsiz!t)
    writef(" seglist %i4", tcb.seglist!t)
    writes("*n   Seglist:  ")
    FOR i = 0 TO seglist!0 DO writef("%i8 ", seglist!i)
    writes("*n   Packets:  ")
    UNTIL pkt<=0 DO
    { writef("pkt %n id  %n   ", pkt, pkt.id!pkt)
      pkt := pkt.link!pkt
    }
    newline()
    t := tcb.link!t
  }
}

AND mksegl(n, a, b, c, d) = VALOF
{ LET segl = getvec(n)
  LET t = @n
  FOR i = 0 TO n DO segl!i := t!i
  RESULTIS segl
}

AND sadebug(code, trapregs) = VALOF
// Only entered as a result of the interpreter encountering a fault.
// trapregs represent the state at that moment.

{ IF sawrch<0 DO     // Test if the system is initialised
  { FOR i = 1 TO 49 DO
      sys(Sys_sawrch, "Fault occurred before the system was initialised*n"%i)
    sys(Sys_quit, code)
  }

  wrch := sawrch

  FOR i = 0 TO 9 DO            // Remove all BRK instructions (if any)
  { LET ba = bpt_addr!i
    IF ba DO 0%ba := bpt_instr!i
  }

  rtn.lastch!rootnode := -1
  rtn.insadebug!rootnode := TRUE
  recp, recl := level(), recover // recovery point for error()
  regs := trapregs

//sawritef("sadebug: code=%n*n", code)
  bpt   := -1
  selectask(-1, trapregs)

  IF code=3 DO          // Zero count.
  { IF brkstep DO
    { brkstep := FALSE  // Breakpoint continuation.
      IF oldcount>0 DO oldcount := oldcount-1
      trapregs!r_count := oldcount
      GOTO ret          // Restore BRK instructions and continue.
    }
    IF snglstep DO
    { snglstep := FALSE // Single step.
      IF oldcount>0 DO oldcount := oldcount-1
      trapregs!r_count := oldcount
      writes("A="); print(regs!r_a)
      writes("B="); print(regs!r_b)
      prinstr(val)
      newline()
      GOTO recover
    }
  }

//writef("val=%n  ", val)

  IF code=2 DO  // BRK instruction
  { //newline()
    //FOR i = 0 TO 9 DO sawritef("%n: %i9*n", i, bpt_addr!i)
    FOR i = 0 TO 9 IF bpt_addr!i=val DO
    { bpt := i
      writef("*n!! BPT %n:  ", bpt)
      writearg(val)
      newline()
      prprompt(trapregs)
      wrch(' ')
      prstate()
      GOTO recover
    }
  }

  IF code=10 DO // Cintasm single step
  {  prprompt(trapregs)
     wrch(' ')
     prstate()
     GOTO recover
  }

  { LET gn = regs!r_pc - globword
    LET mess =  VALOF SWITCHON code INTO
                { CASE   1: RESULTIS "Illegal instruction"
                  CASE   2: RESULTIS "BRK instruction"
                  CASE   3: RESULTIS "Zero count"
                  CASE   4: TEST 0<=gn<=gptr!0
                            THEN RESULTIS "G%n unassigned"
                            ELSE RESULTIS "Negative pc"
                  CASE   5: RESULTIS "Division by zero"
                  CASE  10: RESULTIS "Cintasm single step"
                  CASE  11: RESULTIS "Watch addr: %$%i7 value: %i8"
                  CASE  12: RESULTIS "Indirect address out of range: %$%$%$%n"
                  CASE  99: RESULTIS "User requested"
                  CASE 110: RESULTIS "Callco fault"
                  CASE 111: RESULTIS "Resumeco fault"
                  CASE 112: RESULTIS "Deleteco fault"
                  CASE 180: RESULTIS "Unable to delete a task"
                  CASE 181: RESULTIS "Unable to send a packet"
                  CASE 182: RESULTIS "Unexpected pkt received"
                  CASE 186: RESULTIS "Bad input stream"
                  CASE 187: RESULTIS "Bad output stream"
                  CASE 188: RESULTIS "Unable to replenish input"
                  CASE 189: RESULTIS "Wrch fault"
                  CASE 190: RESULTIS "Endread fault"
                  CASE 191: RESULTIS "Endwrite fault"
                  CASE 197: RESULTIS "Store chain fault"
                  DEFAULT:  RESULTIS "Unknown fault"
                }
    sawritef("*n!! ABORT %n: ", code)
    sawritef(mess, gn, !1, !2, !3)
    sawrch('*n')
    GOTO recover
  }

  rch()
  GOTO sw

recover:
  ch := '*n'

nxt:                          // Main loop for debug commands
  IF ch='*n' DO prprompt(trapregs)

  rch() REPEATWHILE ch='*s'
sw:
  rdflag := FALSE
  SWITCHON ch INTO

  { DEFAULT: error()
      
    CASE '?':
       writes("*n?          Print list of debug commands*n")
       writes("Gn Pn Rn Vn Wn An          Variables*n")
       writes("G  P  R  V  W  A           Pointers*n")
       writes("123 #o377 #FF03 'c         Constants*n")
       writes("**e /e %e +e -e |e &e       Dyadic operators*n")
       writes("!e                         Subscription*n")
       writes("< >                        Left/Right shift one place*n")
       writes("$c $d $f $b $o $s $u $x    Set the print style*n")
       writes("SGn SPn SRn SVn SWn SAn    Store current value*n")
       writes("Sn                         Select task n*n")
       writes("S.                         Select current task*n")
       writes("H          Hold/Release selected task*n")
       writes("K          Disable/Enable clock interrupts*n")
       writes("=          Print current value*n")
       writes("Tn         Print n consecutive locations*n")
       writes("I          Print current instruction*n")
       writes("N          Print next instruction*n")
       writes("Q          Quit -- leave the cintpos system*n")
       writes("M          Set/Reset memory watch address*n")
       writes("B 0Bn eBn  List, Unset or Set breakpoints*n")
       writes("X  (G4B9C) Set breakpoint 9 at start of clihook*n")
       writes("Z  (P1B9C) Set breakpoint 9 at return of current fn*n")
       writes("C          Continue normal execution*n")
       writes("\          Execute one instruction*n")
       writes(".          Move to current coroutine*n")
       writes(",          Move down one stack frame*n")
       writes(";          Move to parent coroutine*n")
       writes("[          Move to first coroutine*n")
       writes("]          Move to next coroutine*n")
       GOTO recover

    CASE '0': CASE '1': CASE '2':
    CASE '3': CASE '4': CASE '5':
    CASE '6': CASE '7': CASE '8':
    CASE '9': CASE '#': CASE '*'':
    CASE 'G': CASE 'P': CASE 'R':
    CASE 'V': CASE 'W': CASE 'A':
              val := rdval();                 GOTO sw

    CASE '!': rch(); val := cont(val  +  rdval());  GOTO sw
    CASE '+': rch(); val := val  +  rdval();        GOTO sw
    CASE '-': rch(); val := val  -  rdval();        GOTO sw
    CASE '**':rch(); val := val  *  rdval();        GOTO sw
    CASE '/': rch(); { LET a = rdval()
                       UNLESS a DO error()
                       val := val / a
                       GOTO sw
                     }
    CASE '%': rch(); { LET a = rdval()
                       UNLESS a DO error()
                       val := val REM a
                       GOTO sw
                     }
    CASE '|': rch(); val := val  |  rdval();        GOTO sw
    CASE '&': rch(); val := val  &  rdval();        GOTO sw

    CASE '<': val := val << 1;                GOTO nxt
    CASE '>': val := val >> 1;                GOTO nxt

    CASE '=': print(val); newline();          GOTO recover

    CASE 'S': { LET type = ?
                rch()
                // Is it a task selection?
                IF ch='.' DO       { selectask(-1, trapregs); rch(); GOTO sw }
                IF '0'<=ch<='9' DO { selectask(rdval(), trapregs);   GOTO sw }
                // No -- it must be a store instruction
                type := ch
                rch()
                !rdvaraddr(type) := val
                GOTO sw
              }

    CASE 'H': // Hold/Release currently selected task
              UNLESS crntcb DO { writes("No task selected*n"); GOTO recover }
              { LET state = tcb.state!crntcb
                LET held = state & #b0010
                tcb.state!crntcb := state NEQV #b0010
                writef("*nTask %n %s*n",
                        tcb.taskid!crntcb, held -> "released", "held")
                GOTO recover
              }

    CASE 'K': // Disable/enable clock interrupts
              TEST rtn.clkintson!rootnode
              THEN { rtn.clkintson!rootnode := FALSE
                     writef("*nClock interrupts disabled*n")
                   }
              ELSE { rtn.clkintson!rootnode := TRUE
                     writef("*nClock interrupts enabled*n")
                   }
              GOTO recover
    CASE 'T': rch()
            { LET n = rdn()
              IF n<=0 DO n := 1
              FOR i=0 TO n-1 DO
              { IF i REM 5 = 0 DO praddr(val+i)
                print(cont(val+i))
              }
              newline()
              GOTO sw
            }

    CASE '$': rch()
              UNLESS ch='B' | ch='C' | ch='D' | ch='F' |
                     ch='O' | ch='S' | ch='U' | ch='X' DO
              { writef("Valid style letters are: BCDFOSUX*n")
                GOTO nxt
              }
              style := ch
              GOTO nxt

    CASE 'Q': sawritef("*n"); sys(Sys_quit, 0)   // Quit
         
    CASE 'N': val := nextpc(val)
    CASE 'I': prinstr(val); newline(); GOTO recover

    CASE 'X':  // Equivalent to G4B9C
         val := gptr!4  // set breakpoint 9 at clihook and resume
         GOTO caseb

    CASE 'Z':  // Equivalent to P1B9C
         val := pptr!1  // set breakpoint 9 to current return address and resume

    caseb:
    CASE 'B':  // Set, clear or display breakpoints.
     { LET comch = ch
       TEST comch='B' THEN rch()       // For B
                      ELSE ch := '9'   // For X or Z
       IF '0'<=ch<='9' DO
       { LET n = ch - '0'  // Set or Clear a break point.
         bpt_addr!n := 0
         IF val=0 GOTO nxt
         checkaddr(val>>2)
         FOR i = 0 TO 9 IF bpt_addr!i=val DO bpt_addr!i := 0
         bpt_addr!n, bpt_instr!n := val, 0%val
         GOTO comch='B' -> nxt, resume
       }
       UNLESS ch='*n' DO newline()
       FOR i = 0 TO 9 DO  // List all breakpoints.
       { LET ba=bpt_addr!i
         UNLESS ba=0 DO
         { writef("%n:  ", i)
           writearg(ba)
           newline()
         }
       }
       GOTO recover
     }

    CASE 'M':  // Set or clear memory watch address
               checkaddr(val)
               TEST val THEN writef("*nWatch address: %n*n", val)
                        ELSE writef("*nWatch unset*n")
               sys(Sys_watch, val)
               GOTO recover

resume:
    CASE 'C': // Continue Cintcode execution.
             { LET pc = trapregs!r_pc
               newline()
               FOR i = 0 TO 9 IF pc=bpt_addr!i DO
               { // We are resuming at a break point
                 oldcount := trapregs!r_count
                 trapregs!r_count := 1 // Set it up to execute just
                 brkstep := TRUE   // one instruction
                 GOTO retstep
               }
               GOTO ret  // Resume execution.
             }

    CASE '\': oldcount := trapregs!r_count // Single step execution.
              trapregs!r_count := 1
              snglstep := TRUE
              GOTO retstep

    CASE ',':  // Move down one stack frame and output it.
             { LET a = pptr!0>>2
               IF a=cptr | a=0 DO { writef(" Base of stack*n")
                                    GOTO recover
                                  }
               fsize := pptr-a
               pptr := a
               wrframe()
               GOTO recover
             }

    CASE ';': IF cptr DO
              { LET c = cont(cptr+co.parent)
                IF c<=0 DO
                { writef(" There is no parent coroutine*n")
                  GOTO recover
                }
                cptr := c
              }
              GOTO newc

    CASE '.': cptr := cont(gptr+g_currco)
              GOTO newc

    CASE ']': cptr := cont(cptr+co.list)
              IF cptr=0 DO { writef(" End of coroutine list*n")
                             GOTO recover
                           }
              GOTO newc

    CASE '[': cptr := cont(gptr+g_colist)

newc:         UNLESS cptr DO
              { writef("No such coroutine*n")
                GOTO recover
              }
              TEST cptr=cont(gptr+g_currco)
              THEN pptr := regs!r_p>>2
              ELSE pptr := cont(cptr+co.pptr)>>2
              fsize := cptr + 6 + cptr!co.size - pptr
              wrcortn(trapregs)
              GOTO recover

    CASE endstreamch: newline(); GOTO ret 

    CASE '*s':
    CASE '*n':GOTO nxt
  }

ret:FOR i = 0 TO 9 DO { LET ba=bpt_addr!i // Set all breakpoints.
                        IF ba DO 0%ba := f_brk
                      }
retstep:
//sawritef("sadebug: retstep reached*n")
//FOR i = 0 TO 7 DO sawritef("R%n = %i7*n", i, trapregs!i)
  rtn.insadebug!rootnode := FALSE
  RESULTIS TRUE  // Successful return from sadebug
}

AND prprompt(trapregs) BE
{ LET id = -1
  LET st = trapregs!r_st
  LET letter = '?'
//sys(Sys_tracing, FALSE)
//sawritef("crntcb=%n*n", crntcb)
  IF crntcb DO id, letter := crntcb!tcb.taskid, crntcb!tcb.active -> 'a', 'd'
  IF st=1 DO letter := 'k'
  IF st=2 DO letter := 'b'
  IF st=3 DO letter := 'i'
  writef("%c%n# ", letter, id)  // Standalone prompt
}

AND prstate() BE
{ writes("A="); print(regs!r_a)
  writes("B="); print(regs!r_b)
  prinstr(val)
  newline()
}

AND wrcortn(trapregs) BE
{ LET size = cont(cptr+co.size)
  LET hwm = size+6
  writef(" %i7: ", cptr)
  TEST cptr!co.parent=-1
  THEN      writes("Root   ")
  ELSE TEST cptr!co.parent=0
       THEN writes("Dormant")
       ELSE writes("Active ")
  writes(" coroutine ")
  writearg(cont(cptr+co.fn))
  WHILE cont(cptr+hwm)=stackword DO hwm:=hwm-1
  writef("  Size %i5  Hwm %i5*n", size, hwm-6)
  prprompt(trapregs)
  wrch(' ')
  wrframe()
}

AND wrframe() BE
{ LET n = fsize
//writef("fsize=%n*n", fsize)
  IF n>6 DO n := 6
  writef(" %i7:", pptr)
  IF pptr=cptr DO { writes("   Base of stack*n"); RETURN }
  writearg(pptr!2)
  FOR i=3 TO n DO print(cont(pptr+i))
  newline()
}

AND writearg(n) BE TEST isfun(n)
                   THEN writef("    %s ", (n>>2)-2)
                   ELSE TEST globword<=n<=globword+gptr!0
                        THEN writef("     #G%z3# ", n-globword)
                        ELSE writef(" %iA ", n)

AND isfun(f) = VALOF
{ LET a = f>>2
  UNLESS (f&3)=0 & membase+3<a<=memlim RESULTIS FALSE // +3 added 28/6/02
  IF a!-3=entryword & a%-8=7 RESULTIS TRUE 
  RESULTIS FALSE
}

AND rdn() = VALOF
{ LET res = 0
  WHILE '0'<=ch<='9' DO { res := res*10 + ch - '0'; rch() }
  RESULTIS res
}

AND rdvaraddr(type) = VALOF
{ LET base, lim, n = ?, ?, ?
  UNLESS '0'<=ch<='9' DO error()
  n := rdn()
  SWITCHON type INTO
  { DEFAULT:   error()
    CASE 'P': base, lim := pptr, fsize;           ENDCASE
    CASE 'G': base, lim := gptr, gptr!g_globsize; ENDCASE
    CASE 'R': base, lim := regs, r_upb;           ENDCASE
    CASE 'V': base, lim := vars, 9;               ENDCASE
    CASE 'W': base, lim := crntcb, tcb.upb;         ENDCASE
    CASE 'A': base, lim :=    0, memlim;          ENDCASE
  }
  UNLESS 0<=n<=lim DO error()
  RESULTIS base + n
}

AND rdval() = VALOF
{ LET res, radix = 0, 10

  SWITCHON ch INTO
  { DEFAULT:   error()

    CASE 'G':  rch()
               IF '0'<=ch<='9' RESULTIS !rdvaraddr('G')
               RESULTIS gptr

    CASE 'P':  rch()
               IF '0'<=ch<='9' RESULTIS !rdvaraddr('P')
               RESULTIS pptr

    CASE 'R':  rch()
               IF '0'<=ch<='9' RESULTIS !rdvaraddr('R')
               RESULTIS regs

    CASE 'V':  rch()
               IF '0'<=ch<='9' RESULTIS !rdvaraddr('V')
               RESULTIS vars

    CASE 'W':  rch()
               IF '0'<=ch<='9' RESULTIS !rdvaraddr('W')
               RESULTIS crntcb

    CASE 'A':  rch()
               IF '0'<=ch<='9' RESULTIS !rdvaraddr('A')
               RESULTIS 0

    CASE '*'': rch(); res := lch; rch();  RESULTIS res

    CASE '#':  radix := 16
               rch()
               IF ch='O' DO { radix := 8; rch() }

    CASE '0': CASE '1': CASE '2': CASE '3': CASE '4': 
    CASE '5': CASE '6': CASE '7': CASE '8': CASE '9': 
               { LET d = 100
                 IF '0'<=ch<='9' DO d := ch-'0'
                 IF 'A'<=ch<='F' DO d := ch-'A'+10
                 IF d>=radix RESULTIS res
                 res := res*radix+d
                 rch()
               } REPEAT
  }
}

AND praddr(a) BE
{ LET type, base = 'A', 0
  IF pptr <= a <= pptr+fsize           DO type, base := 'P', pptr
  IF gptr <= a <= gptr+gptr!g_globsize DO type, base := 'G', gptr
  IF vars <= a <= vars+9               DO type, base := 'V', vars
  IF crntcb <= a <= crntcb+tcb.upb         DO type, base := 'W', crntcb
  IF regs <= a <= regs+r_upb           DO type, base := 'R', regs
  writef("*n%c%i5:", type, a-base)
}

AND print(n) BE SWITCHON style INTO
{ DEFAULT:   error();                 RETURN
  CASE 'C':  { LET p = @n
               writes(" ")
               FOR i = 0 TO 3 DO
               { LET ch = p%i
                 wrch(32<=ch<=127 -> ch, '.')
               }
               RETURN
             }
  CASE 'B':  writef( " %bW ", n);     RETURN
  CASE 'D':  writef( " %IA ", n);     RETURN
  CASE 'F':  writearg(n);             RETURN
  CASE 'O':  writef( " %OC ", n);     RETURN
  CASE 'S':  checkaddr(n)
             writef( " %S ",  n);     RETURN
  CASE 'U':  writef( " %UA ", n);     RETURN
  CASE 'X':  writef( " %X8 ", n);     RETURN
}

AND checkaddr(a) = VALOF
{ UNLESS membase<=a<=memlim DO error()
  RESULTIS a
}

AND selectask(id, trapregs) BE
{ // IF id<0 select the current tcb and the trap registers
  // otherwise select the tcb for task id and the registers
  // corresponding to that task.
  LET tasktab = rtn.tasktab!rootnode
  LET st = trapregs!r_st
  LET t = 0

  IF tasktab & 0<id<=tasktab!0 DO t := tasktab!id
  IF id<0 DO t := rtn.crntask!rootnode

  UNLESS t DO
  { sawritef("Task %n does not exist*n", id)
    crntcb, cptr, regs := 0, 0, 0
    RETURN
  }

  crntcb, cptr := t, 0

  TEST t=rtn.crntask!rootnode
  THEN TEST id>0 & st=3
       THEN regs := saveregs     // regs of crntask at time of fault
       ELSE regs := trapregs     // regs at time of fault
  ELSE regs := @tcb.regs!t       // regs belonging to non current task


//sawritef("regs=%n*n", regs)
  gptr  := r_g !regs >> 2
  pptr  := r_p !regs >> 2

  // Set current coroutine if in user or kernel mode and the task
  // is active
  IF st<2 & t!tcb.active DO cptr := gptr!g_currco

  UNLESS cptr <= pptr <= cptr + cptr!co.size + 6 DO cptr := 0

  fsize := 100
//sawritef("cptr=%n*n", cptr)
  IF cptr DO fsize := cptr + 6 + cptr!co.size - pptr
//sawritef("fsize=%n*n", fsize)
}

AND cont(a) = !checkaddr(a)

AND error() BE { writes("  ??*n"); longjump(recp, recl) }

AND wrfcode(f) BE
{ LET s = VALOF SWITCHON f&31 INTO
  { DEFAULT:
    CASE  0: RESULTIS "     -     K   LLP     L    LP    SP    AP     A"
    CASE  1: RESULTIS "     -    KH  LLPH    LH   LPH   SPH   APH    AH"
    CASE  2: RESULTIS "   BRK    KW  LLPW    LW   LPW   SPW   APW    AW"
    CASE  3: RESULTIS "    K3   K3G  K3G1  K3GH   LP3   SP3   AP3  L0P3"
    CASE  4: RESULTIS "    K4   K4G  K4G1  K4GH   LP4   SP4   AP4  L0P4"
    CASE  5: RESULTIS "    K5   K5G  K5G1  K5GH   LP5   SP5   AP5  L0P5"
    CASE  6: RESULTIS "    K6   K6G  K6G1  K6GH   LP6   SP6   AP6  L0P6"
    CASE  7: RESULTIS "    K7   K7G  K7G1  K7GH   LP7   SP7   AP7  L0P7"
    CASE  8: RESULTIS "    K8   K8G  K8G1  K8GH   LP8   SP8   AP8  L0P8"
    CASE  9: RESULTIS "    K9   K9G  K9G1  K9GH   LP9   SP9   AP9  L0P9"
    CASE 10: RESULTIS "   K10  K10G K10G1 K10GH  LP10  SP10  AP10 L0P10"
    CASE 11: RESULTIS "   K11  K11G K11G1 K11GH  LP11  SP11  AP11 L0P11"
    CASE 12: RESULTIS "    LF   S0G  S0G1  S0GH  LP12  SP12  AP12 L0P12"
    CASE 13: RESULTIS "   LF$   L0G  L0G1  L0GH  LP13  SP13 XPBYT     S"
    CASE 14: RESULTIS "    LM   L1G  L1G1  L1GH  LP14  SP14   LMH    SH"
    CASE 15: RESULTIS "   LM1   L2G  L2G1  L2GH  LP15  SP15   BTC  MDIV"
    CASE 16: RESULTIS "    L0    LG   LG1   LGH  LP16  SP16   NOP CHGCO"
    CASE 17: RESULTIS "    L1    SG   SG1   SGH   SYS    S1    A1   NEG"
    CASE 18: RESULTIS "    L2   LLG  LLG1  LLGH   SWB    S2    A2   NOT"
    CASE 19: RESULTIS "    L3    AG   AG1   AGH   SWL    S3    A3  L1P3"
    CASE 20: RESULTIS "    L4   MUL   ADD    RV    ST    S4    A4  L1P4"
    CASE 21: RESULTIS "    L5   DIV   SUB   RV1   ST1   XCH    A5  L1P5"
    CASE 22: RESULTIS "    L6   REM   LSH   RV2   ST2  GBYT  RVP3  L1P6"
    CASE 23: RESULTIS "    L7   XOR   RSH   RV3   ST3  PBYT  RVP4  L2P3"
    CASE 24: RESULTIS "    L8    SL   AND   RV4  STP3   ATC  RVP5  L2P4"
    CASE 25: RESULTIS "    L9   SL$    OR   RV5  STP4   ATB  RVP6  L2P5"
    CASE 26: RESULTIS "   L10    LL   LLL   RV6  STP5     J  RVP7  L3P3"
    CASE 27: RESULTIS "  FHOP   LL$  LLL$   RTN  GOTO    J$ ST0P3  L3P4"
    CASE 28: RESULTIS "   JEQ   JNE   JLS   JGR   JLE   JGE ST0P4  L4P3"
    CASE 29: RESULTIS "  JEQ$  JNE$  JLS$  JGR$  JLE$  JGE$ ST1P3  L4P4"
    CASE 30: RESULTIS "  JEQ0  JNE0  JLS0  JGR0  JLE0  JGE0 ST1P4     -"
    CASE 31: RESULTIS " JEQ0$ JNE0$ JLS0$ JGR0$ JLE0$ JGE0$     -     -"
  }
  LET n = f>>5 & 7
  FOR i = 6*n+1 TO 6*(n+1) DO wrch(s%i)
}

AND prinstr(pc) BE
{ LET a = 0
  writef(" %i7: ", pc)
  checkaddr(pc>>2)
  wrfcode(0%pc)
  SWITCHON instrtype(0%pc) INTO
  { DEFAULT:
    CASE '0':                                      RETURN
    CASE '1': a  := gb(pc+1);                      ENDCASE
    CASE '2': a  := gh(pc+1);                      ENDCASE
    CASE '4': a  := gw(pc+1);                      ENDCASE
    CASE 'R': a  := pc+1 + gsb(pc+1);              ENDCASE
    CASE 'I': pc := pc+1 + 2*gb(pc+1) & #xFFFFFFFE
              a  := pc + gsh(pc);                  ENDCASE
  }
  writef("  %n", a)
  vars!9 := a
}

AND gb(pc) = 0%pc

AND gsb(pc) = 0%pc<=127 -> 0%pc, 0%pc-256

AND gsh(pc) = VALOF
{ LET h = gh(pc)
  RESULTIS h<=#x7FFF -> h, h - #x10000
}

AND gh(pc) = VALOF
{ LET w = ?
  LET p = @w  // Designed to work on both Big and Little Ender M/Cs.
  p%0, p%1, p%2, p%3 := 0%pc, 0%(pc+1), 0%pc, 0%(pc+1)
  RESULTIS w & #xFFFF
}

AND gw(pc) = VALOF
{ LET w = ?
  LET p = @w  // Designed to work on both Big and Little Ender M/Cs.
  p%0, p%1, p%2, p%3 := 0%pc, 0%(pc+1), 0%(pc+2), 0%(pc+3)
  RESULTIS w
}

AND instrtype(f) = "?0000000000RI10000000000000RIRI*
                  *124111111111111111110000RIRIRIRI*
                  *12411111111111111111000000RIRIRI*
                  *1242222222222222222200000000RIRI*
                  *124000000000000000BL00000000RIRI*
                  *12400000000000000000000000RIRIRI*
                  *1240000000000?2?000000000000000?*
                  *124000000000012?00000000000000??"%f

AND nextpc(pc) = VALOF SWITCHON instrtype(0%pc) INTO
                       { DEFAULT:
                         CASE '0': RESULTIS pc+1
                         CASE '1':
                         CASE 'R':
                         CASE 'I': RESULTIS pc+2
                         CASE '2': RESULTIS pc+3
                         CASE '4': RESULTIS pc+5
                         CASE 'B': pc := pc+2 & #xFFFFFFFE
                                   RESULTIS pc + 4*gh(pc) + 6
                         CASE 'L': pc := pc+2 & #xFFFFFFFE
                                   RESULTIS pc + 2*gh(pc) + 6
                       }


AND rch() BE
{ { lch := rtn.lastch!rootnode
    rtn.lastch!rootnode := -1
//sawritef("lch = %x2*n", lch)
    IF lch>=0 BREAK
    sys(Sys_usleep, 20000)
    //sys(Sys_usleep, 1000000)
  } REPEAT
//sawritef("lch=%n '%c'*n", lch, lch)
  sawrch(lch)
  IF lch=#177 DO error()
  ch := capitalch(lch)
  rdflag := TRUE
}







