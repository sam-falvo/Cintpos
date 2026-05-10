/*
This program output system information about the current
BCPL system.

Implemented by Martin Richards (c) 23 March 2021

It calculates the Cintcode instruction rate for a reasonable balance
of instructions using the function bench which is closely related to
the bench100 program.
*/

GET "libhdr"

GLOBAL {
  stdout:ug
  stdin
  tostream
  bench:300

  spacev
  spacep
  spacet
  newvec

  debug
}

LET newvec(upb) = VALOF
{ LET p = spacep-upb-1
//writef("newvec: upb=%n spacev=%n spacep=%n spacet=%n*n",
//        upb, spacev, spacep, spacet)
  IF p<spacev DO
  { writef("More space needed*n")
    abort(999)
  }
  spacep := p
  RESULTIS p
}

LET start() = VALOF
{ LET argv = VEC 50
  LET buildno = sys(Sys_getbuildno)
  LET flags = result2

  LET hostaddrsize = rtn_hostaddsize!rootnode
  LET bitsperword, msb, allones = 1, 1, 1
  LET ww = 65
  LET datestream = findinput("../TGZDATE")
  stdout, stdin := output(), input()

  spacev := getvec(10000)
  spacet := spacev+10000
  spacep := spacet

  UNLESS rdargs("TO/K,-d/S", argv, 50) DO
  { writef("Bad argument for sysinfo*n")
    RESULTIS 0
  }

  tostream := 0
  IF argv!0 DO                // TO/k
  { tostream := findoutput(argv!0)
    UNLESS tostream DO
    { writef("Unable to open file %s for output*n", argv!0)
      RESULTIS 0
    }
    selectoutput(tostream)
  }
  
  debug := argv!1             // -d

  writef("*nTGZDATE: ")
  TEST datestream
  THEN { LET oldin = input()
         selectinput(datestream)
         { LET ch = rdch()
	   IF ch=endstreamch BREAK
	   wrch(ch)
	 } REPEAT
	 endread()
	 selectinput(oldin)
       }
  ELSE { writef("Unknown*n")
       }

  writef("Build: ")
  prbuild(buildno)
  writef("*nFlags:")
  prflags(flags)

  UNTIL (msb<<1)=0 DO
    bitsperword, msb, allones := bitsperword+1, msb<<1, allones<<1 | 1

  writef("The host is a %s ender machine*n", (@ww)%0=65 -> "little", "big")
  writef("Host address size: %n     ", hostaddrsize)

  writef("BCPLWORD: %n*n", bitsperword)
  writef("Interpreter: %s*n", rootnode!rtn_fast -> "fast", "slow")
  
  TEST sys(Sys_setraster, 3, 0)
  THEN writef("Rastering is available*n")
  ELSE writef("Rastering is not available*n")
  
  prinstructionrate()
  newline()
  freevec(spacev)
  IF tostream DO endstream(tostream)
  RESULTIS 0
}


AND prbuild(n) BE
{ LET str = VALOF SWITCHON n INTO
  { DEFAULT:  RESULTIS 0

    CASE bld_linux:             RESULTIS "Linux"
    CASE bld_linuxSDL:          RESULTIS "LinuxSDL"
    CASE bld_linuxSDL2:         RESULTIS "LinuxSDL2"
    CASE bld_linuxGL:           RESULTIS "LinuxGL"
    CASE bld_linuxSDLGL:        RESULTIS "LinuxSDLGL"
    CASE bld_linuxSDL2GL:       RESULTIS "LinuxSDL2GL"
    CASE bld_linuxiSH:          RESULTIS "LinuxiSH"

    CASE bld_Raspi:             RESULTIS "Raspi"
    CASE bld_RaspiSDL:          RESULTIS "RaspiSDL"
    CASE bld_RaspiSDL2:         RESULTIS "RaspiSDL2"
    CASE bld_RaspiSDLGL:        RESULTIS "RaspiSDLGL"
    CASE bld_RaspiSDL2GL:       RESULTIS "RaspiSDL2GL"

    CASE bld_MacOSX:            RESULTIS "MacOSX"
    CASE bld_MacOSXSDL:         RESULTIS "MacOSXSDL"
    CASE bld_MacOSXSDL2:        RESULTIS "MacOSXSDL2"
    CASE bld_MacOSXSDLGL:       RESULTIS "MacOSXSDLGL"
    CASE bld_MacOSXSDL2GL:      RESULTIS "MacOSXSDL2GL"

    CASE bld_VmsVax:            RESULTIS "VmsVax"
    CASE bld_Win32:             RESULTIS "Win32"
    CASE bld_CYGWIN:            RESULTIS "CYGWIN"
  }
  
  TEST str=0
  THEN writef("Unknown, buildno=%n", n)
  ELSE writef(str)
}

AND prflags(flags) BE
{ UNLESS (flags&bldf_sound)=0     DO writef(" SOUND")
  UNLESS (flags&bldf_callc)=0     DO writef(" CALLC")
  UNLESS (flags&bldf_joystick)=0  DO writef(" joystick")
  UNLESS (flags&bldf_ALSAavail)=0 DO writef(" ALSAavail")
  UNLESS (flags&bldf_SDLavail)=0  DO writef(" SDLavail")
  UNLESS (flags&bldf_GLavail)=0   DO writef(" GLavail")
  newline()
}

AND prinstructionrate() BE
{ LET days, msecs, dummy = 0, 0, 0
  LET startmsecs = 0
  LET endmsecs = 0
  LET count = instrcount(bench)
  LET rate = 0
  datstamp(@days)
  startmsecs := msecs
  bench()
  datstamp(@days)
  endmsecs := msecs
  IF debug DO
  { writef("Instruction count = %n minint=%16x maxint=%16x*n",
            count, minint, maxint)
    writef("startmsecs=%n  endmsecs=%n execution msecs=%n*n",
            startmsecs, endmsecs, endmsecs-startmsecs)
  }
  rate := muldiv(count, 1000, (endmsecs - startmsecs | 1))
  //writef("rate=%16x*n", rate)

  writef("Execution rate: %n,%z3,%z3 Cintcode instructions per second*n",
             rate/1_000_000,
	     rate/1_000 MOD 1000,
	     rate MOD 1000)
          
}

.

GET "libhdr"

MANIFEST { // Unncomment out one of the following lines
//Count=10000;      Qpktcountval=23246;   Holdcountval=9297
  Count=10000*10;   Qpktcountval=232625;  Holdcountval=93050
//Count=10000*100;  Qpktcountval=2326410; Holdcountval=930563

i_idle = 1
i_work
i_handlera
i_handlerb
i_deva
i_devb
tasktab_upb  = i_devb
 
p_link = 0
p_id
p_kind
p_a1
p_a2
pkt_upb= p_a2
 
t_link = 0
t_id
t_pri
t_wkq
t_state
t_fn
t_v1
t_v2
tcb_upb= t_v2
 
bufsize = 3
 
pktbit         = 1
waitbit        = 2
holdbit        = 4
 
notpktbit      = NOT pktbit
notwaitbit     = NOT waitbit
notholdbit     = NOT holdbit
 
s_run          = 0
s_runpkt       = pktbit
s_wait         = waitbit
s_waitpkt      = waitbit + pktbit
s_hold         = holdbit
s_holdpkt      = holdbit + pktbit
s_holdwait     = holdbit + waitbit
s_holdwaitpkt  = holdbit + waitbit + pktbit
 
k_dev   = 1000
k_work  = 1001
}
 
GLOBAL {
  bench:300

  spacev
  spacep
  spacet
  newvec
  
  debug

  tasktab
  tasklist
  tcb
  taskid
  v1
  v2
 
  trace
  schedule
  qpkt
  wait
  holdself
  release
 
  idlefn
  workfn
  handlerfn
  devfn
 
  qpktcount
  holdcount
  tracing
  layout
}
 
LET bench() = VALOF
{ // This function hopefully executes a realistic balance of different
  // Cintcode instructions.
  LET wkq = 0
  LET quiet = TRUE

  UNLESS quiet DO writef("*Nbench mark starting, Count=%n*N", Count)
 
  tasktab := newvec(tasktab_upb)
  tasktab!0 := tasktab_upb
  FOR taskno = 1 TO tasktab_upb DO tasktab!taskno := 0
 
  tasklist := 0
 
  createtask(i_idle, 0, wkq, s_run, idlefn, 1, Count)
 
  wkq := pkt(0,   0, k_work)
  wkq := pkt(wkq, 0, k_work)
  createtask(i_work, 1000, wkq, s_waitpkt, workfn, i_handlera, 0)
 
  wkq := pkt(0,   i_deva, k_dev)
  wkq := pkt(wkq, i_deva, k_dev)
  wkq := pkt(wkq, i_deva, k_dev)
  createtask(i_handlera, 2000, wkq, s_waitpkt, handlerfn, 0, 0)
 
  wkq := pkt(0,   i_devb, k_dev)
  wkq := pkt(wkq, i_devb, k_dev)
  wkq := pkt(wkq, i_devb, k_dev)
  createtask(i_handlerb, 3000, wkq, s_waitpkt, handlerfn, 0, 0)
 
  wkq := 0
  createtask(i_deva, 4000, wkq, s_wait, devfn, 0, 0)
  createtask(i_devb, 5000, wkq, s_wait, devfn, 0, 0)
 
  tcb := tasklist
 
  qpktcount, holdcount := 0, 0

  UNLESS quiet DO
  { writes("*Nstarting*N")
    // writef("starting time = %n*N", time())
  }
  tracing, layout := FALSE, 0
  schedule()
  UNLESS quiet DO
  { writes("*Nfinished*N")
    // writef("*Nfinishing time = %n*N", time())
 
    writef("qpkt count = %n  holdcount = %n*N",
            qpktcount,       holdcount)
 
    writes("these results are ")
    TEST qpktcount=Qpktcountval & holdcount=Holdcountval
    THEN writes("correct")
    ELSE writes("incorrect")
 
    writes("*Nend of run*N")
  }
  RESULTIS 0
}
 
AND createtask(id, pri, wkq, state, fn, v1, v2) BE
{ LET t = newvec(tcb_upb)
  tasktab!id := t  // insert in the task table
  t_link!t := tasklist
  t_id!t   := id
  t_pri!t  := pri
  t_wkq!t  := wkq
  t_state!t:= state
  t_fn!t   := fn
  t_v1!t   := v1
  t_v2!t   := v2
  tasklist := t
}
 
AND pkt(link, id, kind) = VALOF
{ LET p = newvec(pkt_upb)
  FOR i = 0 TO pkt_upb DO p!i := 0
  p_link!p    := link
  p_id!p      := id
  p_kind!p    := kind
  RESULTIS p
}
 
 
AND trace(ch) BE
{ layout := layout - 1
  IF layout<=0 DO
  { newline()
    layout := 50
  }
  wrch(ch)
  ch := 7
}
 
LET schedule() BE UNTIL tcb=0 DO
{ LET pkt, newtcb = 0, ?

  SWITCHON t_state!tcb INTO
  { CASE s_waitpkt:    pkt := t_wkq!tcb
                       t_wkq!tcb := p_link!pkt
                       t_state!tcb := t_wkq!tcb=0 -> s_run, s_runpkt
    CASE s_run:
    CASE s_runpkt:     taskid, v1, v2 := t_id!tcb, t_v1!tcb, t_v2!tcb
                       IF tracing DO trace(taskid+'0')
                       newtcb := (t_fn!tcb)(pkt)
                       t_v1!tcb, t_v2!tcb := v1, v2
                       tcb := newtcb
                       LOOP
 
    CASE s_wait:
    CASE s_hold:
    CASE s_holdpkt:
    CASE s_holdwait:
    CASE s_holdwaitpkt:tcb := t_link!tcb
                       LOOP
 
    DEFAULT:           RETURN
  }
}
 
AND qpkt(pkt) = VALOF
{ LET t = findtcb(p_id!pkt)
  IF t=0 RESULTIS 0
  qpktcount := qpktcount + 1
 
  p_link!pkt, p_id!pkt := 0, taskid
  TEST t_wkq!t=0
  THEN { t_wkq!t := pkt
         t_state!t := t_state!t | pktbit
         IF t_pri!t > t_pri!tcb RESULTIS t
       }
  ELSE append(pkt, @ t_wkq!t)
  RESULTIS tcb
}
 
AND wait() = VALOF
{ t_state!tcb := t_state!tcb | waitbit
  RESULTIS tcb
}
 
AND holdself() = VALOF
{ holdcount := holdcount + 1
  t_state!tcb := t_state!tcb | holdbit
  RESULTIS t_link!tcb
}
 
AND release(id) = VALOF
{ LET t = findtcb(id)
  IF t=0 RESULTIS 0
 
  t_state!t := t_state!t & notholdbit
  IF t_pri!t > t_pri!tcb RESULTIS t
  RESULTIS tcb
}
 
AND findtcb(id) = VALOF
{ LET t = 0
  IF 1 <= id <= tasktab!0 DO t := tasktab!id
  IF t=0 DO writes("*Nbad task id*N")
  RESULTIS t
}
 
AND idlefn(pkt) = VALOF
{ v2 := v2 - 1
  IF v2=0 RESULTIS holdself()
  TEST (v1&1)=0
  THEN { v1 := v1>>1
         RESULTIS release(i_deva)
       }
  ELSE { v1 := v1>>1 NEQV #XD008
         RESULTIS release(i_devb)
       }
}
 
AND workfn(pkt) = VALOF TEST pkt=0
  THEN RESULTIS wait()
  ELSE { LET buf = @ p_a2!pkt
         v1 := i_handlera + i_handlerb - v1
         // v1 is alternately i>handlera AND i_handlerb
 
         p_id!pkt := v1   // set the destination task id
         p_a1!pkt := 0    // set the buffer subscript
         FOR i = 0 TO bufsize DO
         { v2 := v2 + 1
           IF v2>26 DO v2 := 1
           buf%i := "ABCDEFGHIJKLMNOPQRSTUVWXYZ"%v2
         }
         RESULTIS qpkt(pkt)
       }
 
AND handlerfn(pkt) = VALOF
{ UNLESS pkt=0 DO append(pkt, p_kind!pkt=k_work -> @v1,  @v2)
  UNLESS v1=0 DO
  { LET workpkt = v1
    LET count, buf = p_a1!workpkt, @ p_a2!workpkt
    IF count>bufsize DO
    { v1 := p_link!v1
      RESULTIS qpkt(workpkt)  // send back the exhausted a work packet
    }
    UNLESS v2=0 DO
    { LET devpkt = v2
      v2 := p_link!v2
      p_a1!devpkt := buf%count  // copy character into it
      p_a1!workpkt := count+1   // incrementing the character count
      RESULTIS qpkt(devpkt)     // send the packet to the device task
    }
  }
 
  // cannot proceed for lack of a packet so wait for one
  RESULTIS wait()
}
 
AND devfn(pkt) = VALOF TEST pkt=0
  THEN { IF v1=0 RESULTIS wait()
         pkt := v1
         v1 := 0
         RESULTIS qpkt(pkt)
       }
  ELSE { v1 := pkt
         IF tracing DO trace(p_a1!pkt)
         RESULTIS holdself()
       }
 
AND append(pkt, ptr) BE
{ p_link!pkt := 0
  UNTIL !ptr=0 DO ptr := !ptr
  !ptr := pkt
}
