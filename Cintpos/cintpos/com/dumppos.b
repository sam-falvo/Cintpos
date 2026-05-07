// This is a program to dump information about the Cintpos system
// Implemented by Martin Richards (c) November 2003

// Usage:

// dumpsys [FROM <image>] [TO <file>]

// The FROM argument specifies the dump image file,
// the default DUMP.mem
// The TO argument specifies where to send the output.

// The program dumps the rootnode and all memory blocks then for
// each task it dumps its TCB, workqueue, global vector and
// coroutine stacks.

SECTION " dumpsys"

GET "libhdr"

GLOBAL
{ eof: ug
  pptr
  gptr
  cptr
  ctcb
  fsize

  regs
  tasktab
  devtab
  membase
  memlim
  memupb
  context

  imagefilename  // 0 or name of the image file

  // Variables used by getimage, nextword, and mem.
  datastream       // scb of the image file
  blkcount         // repetition count if neg, otherwise size of block
  currword         // current word from image file
  pagetab          // The page table
  pagenoupb        // The page table upb

  rec_p; rec_l     // Recovery label for longjump
}

MANIFEST {
  maxpri    = maxint

  sectnamesize = (11 + bytesperword)/bytesperword
  routnamesize = (11 + bytesperword)/bytesperword
  nameoffset   = -routnamesize
  vecupb       = 5000

  r_a=0
  r_b
  r_c
  r_p
  r_g
  r_st
  r_pc
  r_count
  r_mw
  r_upb = r_mw

  // Contants used by getimage and mem.
  pageshift = 12   // 12 is 15 seconds faster than 10
  pageupb = (1<<pageshift)-1
  pagemask = pageupb
}

LET start() BE
{ LET argv       = VEC 50
  AND datv       = VEC 2
  AND datstrings = VEC 12
  AND sysin      = input()
  AND sysout     = output()
  AND toname     = 0
  AND tostream   = 0
  AND imagefilename = "DUMP.mem"
  AND taskcount  = 0

  UNLESS rdargs("FROM,TO/K", argv, 50) DO
  { writes("bad arguments for DUMPSYS*n")
    stop(20)
  }

  pagetab := 0

  IF argv!0 DO imagefilename := argv!0
  IF imagefilename UNLESS getimage(imagefilename) DO
  { writef("Unable to load image file %s*n", imagefilename)
    RETURN
  }

  rec_p, rec_l := level(), fin

  membase, memlim := 0, 1_000_000  // Temp setting to make mem work.

  tasktab := mem(rootnode+rtn_tasktab)
  devtab  := mem(rootnode+rtn_devtab)
  membase := mem(rootnode+rtn_membase)
  memlim  := mem(rootnode+rtn_memsize)
  context := mem(rootnode+rtn_context)

  UNLESS tasktab & devtab & memlim & context DO
  { writef("The DUMP file is corrupt*n")
    stop(20)
  }
  // context = 1   SIGINT received
  // context = 2   SIGSEGV received
  // context = 3   dump caused by fault in BOOT of standalone debug
  // context = 4   dump requested by user calling sys(Sys_quit, -2)
  // context = 5   non zero user fault code
  // context = 6   dump requested in standalone debug

  toname := argv!1
 
  IF toname DO
  { tostream := findoutput(toname)
    UNLESS tostream DO
    { writef("can't open %s*n", toname)
      GOTO fin
    }
    selectoutput(tostream)
  }

  writef("*nImage File: %s", imagefilename)
  IF memdatstamp(datv) DO
  { dat_to_strings(datv, datstrings)

    writef("           Dated:  %s %s*n", @datstrings!0, @datstrings!5)
  }
  newline()

  SWITCHON context INTO
  { DEFAULT:  writef("Unknown reason (%n) for dumping memory", context)
              ENDCASE
    CASE 1:   writef("Dump caused by signal SIGINT")
              ENDCASE
    CASE 2:   writef("Dump caused by signal SIGSEGV")
              ENDCASE
    CASE 3:   writef("Dump caused by fault in BOOT or standalone debug")
              ENDCASE
    CASE 4:   writef("Dump by user probably calling: dumpmem")
              ENDCASE
    CASE 5:   writef("Dump caused by non zero user fault code")
              ENDCASE
    CASE 6:   writef("Dump requested in standalone debug")
              ENDCASE
  }
  newline()
  
  writef("*nLast abort code: %n*n", mem(rootnode+rtn_abortcode))

  writef("*nCintcode memory from %n to %n*n", membase, memlim)

  wrregs("BOOT Registers", bootregs)
  wrregs("KLIB Registers", klibregs)
  wrregs("SAVE Registers", saveregs)
  wrregs("ISR  Registers", isrregs)

  dumprootnode()

  IF mem(klibregs+r_st)=3 DO
  { writef("*nThe Interrupt Service Routine is currently running*n*n")
  }

  dumptask(mem(rootnode+rtn_idletcb))
  FOR id = 1 TO mem(tasktab) DO dumptask(mem(tasktab+id))

  dumpmemory()

fin:
  IF tostream UNLESS sysout=tostream DO endstream(tostream)
  deleteimage()
}

AND memdatstamp(v) = VALOF
{ LET tv = rootnode+rtn_days
 
  v!0 := mem(tv+0)
  v!1 := mem(tv+1)
  v!2 := mem(tv+2) // For compatibility with old dat format

  RESULTIS TRUE
}

AND prpkt(pkt) BE TEST pkt & validaddr(pkt)
THEN { writef("%6i: PKT:   ", pkt)
       FOR i = pkt_link TO pkt_r2 DO writearg(mem(pkt+i))
       writef("*n")
       FOR i = pkt_a1   TO pkt_a6 DO writearg(mem(pkt+i))
       newline()
     }
ELSE { writearg(pkt)
       writes("   Bad packet address*n")
     }

AND dumprootnode() BE
{ writef("*nRootnode at %n*n*n", rootnode)

  writef("  tasktab    %10i*n", mem(rtn_tasktab+rootnode))
  writef("  devtab     %10i*n", mem(rtn_devtab+rootnode))
  writef("  tcblist    %10i*n", mem(rtn_tcblist+rootnode))
  writef("  crntask    %10i  ", mem(rtn_crntask+rootnode))
  writef(" task %n*n", mem(tcb_taskid+mem(rtn_crntask+rootnode)))
  writef("  blklist    %10i*n", mem(rtn_blklist+rootnode))
  writef("  clkintson  %10i*n", mem(rtn_clkintson+rootnode))
  writef("  clwkq      %10i*n", mem(rtn_clwkq+rootnode))
  writef("  memsize    %10i*n", mem(rtn_memsize+rootnode))
  writef("  info       %10i*n", mem(rtn_info+rootnode))
  writef("  sys        %10i*n", mem(rtn_sys+rootnode))
  writef("  blib       %10i*n", mem(rtn_blib+rootnode))
  writef("  boot       %10i*n", mem(rtn_boot+rootnode))
  writef("  klib       %10i*n", mem(rtn_klib+rootnode))
  writef("  abortcode  %10i*n", mem(rtn_abortcode+rootnode))
  writef("  context    %10i*n", mem(rtn_context+rootnode))
  writef("  sysp       %10i*n", mem(rtn_sysp+rootnode))
  writef("  sysg       %10i*n", mem(rtn_sysg+rootnode))
  writef("  sysst      %10i*n", mem(rtn_sysst+rootnode))
  writef("  days       %10i*n", mem(rtn_days+rootnode))
  writef("  msecs      %10i*n", mem(rtn_msecs+rootnode))
  writef("  idletcb    %10i*n", mem(rtn_idletcb+rootnode))

  writef("*nTasktab at %n upb=%n*n", tasktab, mem(tasktab))
  FOR i = 1 TO mem(tasktab) IF mem(tasktab+i) DO
  { LET tcb = mem(tasktab+i)
    LET id, pri, wkq = mem(tcb+tcb_taskid), mem(tcb+tcb_pri), mem(tcb+tcb_wkq)
    LET taskname = tcb+tcb_namebase // MR 17/01/05
    writef("%6i: TCB for task %3i,    pri %5i  ", tcb, id, pri)
    FOR i = 1 TO memb(taskname, 0) DO wrch(memb(taskname, i))
    newline()
    WHILE wkq DO
    { LET pkt = wkq
      prpkt(pkt)
      UNLESS pkt & validaddr(pkt) BREAK
      wkq := mem(wkq)
    }
  }
  writef("*nDevtab at %n upb=%n*n", devtab, mem(devtab))
  IF validaddr(devtab) FOR i = 1 TO mem(devtab) IF mem(devtab+i) DO
  { LET dcb = mem(devtab+i)
    LET id, type, wkq = mem(dcb+Dcb_devid), mem(dcb+Dcb_type), mem(dcb+Dcb_wkq)
    IF dcb=0 LOOP
    writef("%6i: DCB for device %3i type %n ", dcb, id, type)
    SWITCHON type INTO
    { DEFAULT:          writef("(unknown)"); ENDCASE

      CASE Devt_clk:    writef("(clock)");  ENDCASE
      CASE Devt_ttyin:  writef("(ttyin)");  ENDCASE
      CASE Devt_ttyout: writef("(ttyout)"); ENDCASE
      CASE Devt_fileop: writef("(fileop)"); ENDCASE
      CASE Devt_tcpdev: writef("(tcpdev)"); ENDCASE
    }
    newline()
    IF id=-1 DO // The clock wkq is in the root node
      wkq := mem(rtn_clwkq+rootnode)
    WHILE wkq>0 & validaddr(wkq) DO
    { LET pkt = wkq
      prpkt(pkt)
      wkq := mem(wkq)
    }
  }  
}

AND dumpmemory() BE
{ LET blocklist  = mem(rootnode+rtn_membase)
  LET blocklistok = result2
  LET topofstore = mem(rootnode+rtn_memsize)
  LET topofstoreok = result2
  LET a = blocklist
  LET free, used, n = 0, 0, 0
  LET largest_free = 0
  LET joinfree = 0
  LET sectnames, routnames, globinit = 0, 0, 0
  LET constr = output()
  LET outstr = 0

  UNLESS blocklistok DO
  { writef("The blocklist pointer is corrunpt*n")
    RETURN
  }
  UNLESS topofstoreok DO
  { writef("The topofstore value is corrunpt*n")
    RETURN
  }

  writef("*nMap of free and allocated blocks in %n..%n*n*n", membase, memlim)

  WHILE mem(a) & result2 DO
  { LET size = mem(a)
    LET freeblk = (size&1)=1

    UNLESS a=0 | validaddr(a) DO
    { writef("*nStore chain corrupt*n")
      BREAK
    }

    //IF testflags(flag_b) GOTO exit

    TEST freeblk
    THEN { // Free block
           size := size-1
           free := free + size
           joinfree := joinfree + size
           IF joinfree > largest_free DO largest_free := joinfree
         }
    ELSE { // Used block
           used := used + size
           joinfree := 0
         }

    UNLESS size>=0 & a+size<=topofstore DO
    { writef("******Store chain corrupt!!*n*
              *Noticed at %n*n", a)
      BREAK
    }

    writef("%8i:%7i ", a, size)
    IF freeblk DO { writes("free "); GOTO nxt }

    { LET creator = a+size-5
      LET len = memb(creator, 0)
      writef(" allocated by: ")
      
      UNLESS 1 <= len <= 16 DO len := 0 // Invalid creator name

      FOR i = 1 TO len DO wrch(memb(creator, i))
      FOR i = len+1 TO 16 DO wrch(' ')
    }

    IF a = (rootnode-1) DO { writes("Rootnode"); GOTO nxt }
    IF mem(a+3) = sectword & memb(a+4, 0) = 11 DO
    { LET name = a+4
      writef("Section ")
      FOR i = 1 TO memb(name, 0) DO wrch(memb(name, i))
      GOTO nxt
    }

    FOR id = 1 TO mem(tasktab) DO
    { LET t = mem(tasktab+id)
      IF t DO
      { LET p, g = mem(t+tcb_sbase), mem(t+tcb_gbase)
        IF a+1=p DO { writef("Task %n stack", id); GOTO nxt }
        IF a+1=g DO { writef("Task %n global vector", id); GOTO nxt }
        IF a+1=t DO { writef("Task %n TCB", id); GOTO nxt }
        IF g & a>500 & 1<mem(g)<=1000 DO
          FOR gn = 1 TO mem(g) IF a+1=mem(g+gn) DO
          { writef("Task %n G%n => ", id, gn)
            GOTO dump
          }
      }
    }
dump:
    writes("*n         ")
    FOR i = 1 TO 5 DO { LET n = mem(a+i)
                        TEST -10_000_000<=n<=10_000_000
                        THEN writef("%10i ", n)
                        ELSE writef("#x%8x ", n)
                      }
nxt: 
    newline()
    a := a + size
  }

  writef("End of block list = %n*n", a)
  topofstore := a

  writef("*nLargest contiguous free area: %n words*n", largest_free)
  writef("Totals: %n words available, %n used, %n free*n*n",
          used+free, used, free)

exit:
}

AND dumptask(tcb) BE IF tcb DO
{ LET id = mem(tcb+tcb_taskid)
  LET seglist, pkt = 0, 0
  LET state, flags, cliseg = 0, 0, 0
  LET dead = FALSE             // Assume activated unless proved otherwise
  LET pkt = mem(tcb_wkq+tcb)
  regs, pptr, gptr := 0, 0, 0

  UNLESS validaddr(tcb) RETURN

  seglist := mem(tcb_seglist+tcb)
  pkt     := mem(tcb + tcb_wkq)
  state   := mem(tcb + tcb_state)
  flags   := mem(tcb + tcb_flags)
  dead    := (state & State_dead) = State_dead
  cliseg  := mem(seglist+4)    // The CLI segment

  writef("*n################### Task %2i:", id)
  wrch(' ')
  FOR i = 1 TO memb(tcb+tcb_namebase, 0) DO wrch(memb(tcb+tcb_namebase, i))
  wrch(' ')
  FOR i = memb(tcb+tcb_namebase, 0)+1 TO 15+16 DO wrch('#')
  writef("*n*n")
  writef("tcb=%n:", tcb)
  writef(" priority %n,",   mem(tcb + tcb_pri))
  writef(" stack size %n,", mem(tcb+tcb_stsiz))
  writef(" flags=#b%b6*n",  mem(tcb+tcb_flags))

  { LET state = mem(tcb+tcb_state)
    writef("*nState #b%b4: ", mem(tcb+tcb_state))
    SWITCHON state & #b1100 INTO
    { CASE #b0000: writef(" running");     ENDCASE
      CASE #b0100: writef(" waiting");     ENDCASE
      CASE #b1000: writef(" interrupted"); ENDCASE
      CASE #b1100: dead := TRUE
                   writef(" dead");        ENDCASE
    }
    UNLESS (state & #b0010)=0 DO writef(" held")
    UNLESS (state & #b0001)=0 DO writef(" with packet")
    newline()
  }

  writef("*nPackets: wkq=%n*n", pkt)
  WHILE validaddr(pkt) DO
  { prpkt(pkt)
    UNLESS validaddr(pkt) BREAK
    pkt := mem(pkt_link+pkt)
  }

  IF dead DO
  { writef("*nNo stack or global vector since the task is dead*n")
    RETURN
  }

  regs, ctcb, cptr := 0, tcb, 0

  TEST mem(rootnode+rtn_crntask)=ctcb
  THEN { // klibregs is the register set passed to the interpreter by BOOT
         // these are normally the registers to use.
         regs := klibregs // The default register set
         // If st=3 we are in the interrupt service routine
         // so we will use the registers that were saved when the
         // interrupt occurred.
         IF mem(regs+r_st)=3 DO regs := saveregs
       }
  ELSE regs := @ctcb!tcb_a

  gptr  := mem(regs+r_g) >> 2
  pptr  := mem(regs+r_p) >> 2

  // Set current coroutine if in user or kernel mode and the task
  // is active
  IF mem(tcb+tcb_active) DO cptr := mem(gptr+g_currco)

  UNLESS cptr <= pptr <= cptr + mem(cptr+co_size + 6) DO cptr := 0

  fsize := 100
//sawritef("cptr=%n*n", cptr)
  IF cptr DO fsize := cptr + 6 + mem(cptr+co_size) - pptr

  wrregs("Registers", regs)

  writef("*nSeglist %n:  length %n", mem(tcb_seglist+tcb), mem(seglist))
  //FOR i = 1 TO mem(seglist) DO writef("  %n", mem(seglist+i))

  { LET segl = mem(tcb + tcb_seglist)

    IF segl & validaddr(segl) FOR j = 1 TO mem(segl) DO
    { LET layout = 0
      LET seg = mem(segl+j)
      
      IF result2 LOOP

      writef("*nSeg%n %6i: ", j, seg)
      WHILE seg & validaddr(seg) DO
      { IF layout & (layout MOD 5)=0 DO writef("*n             ")
        wrch(' ')
        layout := layout + 1
        write_sectname(seg)
        seg := mem(seg)
      }
    }
    newline()

    IF gptr & validaddr(gptr) DO
    { LET gupb = mem(gptr)
      writef("*nGlobal variables at G = %n:*n", gptr)
      IF 0<gupb<=1000 FOR gn = 0 TO gupb DO
      { LET w = mem(gptr+gn)
        IF gn REM 5 = 0 DO
        { LET v, gw = gptr+gn, globword+gn
          IF              mem(v+0)=gw+0 &
             (gn+1>gupb | mem(v+1)=gw+1) &
             (gn+2>gupb | mem(v+2)=gw+2) &
             (gn+3>gupb | mem(v+3)=gw+3) &
             (gn+4>gupb | mem(v+4)=gw+4) DO { gn := gn+4; LOOP }
          writef("*nG%z3:", gn)
        }
        writearg(w)
      }
      newline()
    }

    writef("*nCoroutine stacks for task %n:*n", id)  
    wrcortns(tcb)
  }
}

AND wrregs(str, regs) BE
{ writef("*n%s:*n", str)
  UNLESS validaddr(regs) RETURN
  writef("a=%n b=%n c=%n ",    mem(regs+r_a), mem(regs+r_b), mem(regs+r_c))
  writef("p=%n(%n) g=%n(%n) ", mem(regs+r_p), mem(regs+r_p)>>2,
                               mem(regs+r_g), mem(regs+r_g)>>2)
  writef("st=%n pc=%n count=%n*n", mem(regs+r_st), mem(regs+r_pc),
                                   mem(regs+r_count))
}

AND write_sectname(s) BE
{ LET name = s+3
  TEST validaddr(s) & (mem(s+2) = sectword) & (memb(name, 0) = 11)
  THEN FOR i = 1 TO 11 DO wrch(memb(name, i))
  ELSE { writes("Bad Section Name ")
writef("*ns=%n %x8 %x8 %x8*n", s, mem(s+2), mem(s+3), mem(s+4), mem(s+5))
         //abort(1278)
       }
}

AND validaddr(a) = a>=0 & membase<=a<=memlim -> TRUE, FALSE

AND checkaddr(a) = VALOF
{ UNLESS membase<=a<=memlim DO
  { writef("*nBad address %n not in range %n--%n*n", a, membase, memlim)
    RESULTIS 0
  }
  RESULTIS a
}

AND cont(a) = mem(checkaddr(a))

AND wrcortns(tcb) BE
{ cptr := cont(gptr+g_colist)

  UNLESS validaddr(cptr) DO
  { writef("*nInvalid coroutine list*n")
    RETURN
  }

//sawritef("cptr=%n pptr=%n gptr=%n regs=%n*n", cptr, pptr, gptr, regs)

  WHILE 0<cptr<=memlim DO
  { TEST cptr=cont(gptr+g_currco)
    THEN TEST 1<=mem(rootnode+rtn_context)<=2 & // SIGINT or SIGSEGV
              mem(rootnode+rtn_crntask)=tcb
         THEN pptr := mem(rootnode+rtn_sysp)
         ELSE pptr := mem(regs+r_p)>>2
    ELSE pptr := cont(cptr+co_pptr)>>2

    UNLESS validaddr(pptr) DO
    { writef("*nInvalid coroutine resumption point*n")
      RETURN
    }

    fsize := cptr + 6 + mem(cptr+co_size) - pptr
//sawritef("cptr=%n pptr=%n gptr=%n fsize=%n*n", cptr, pptr, gptr, fsize)

    { LET size = cont(cptr+co_size)
      LET hwm = size+6
      LET parent = mem(cptr+co_parent)

      UNLESS validaddr(size) DO
      { writef("*nInvalid coroutine size=%n*n", size)
        RETURN
      }

      UNLESS parent=0 | parent=-1 | validaddr(parent) DO
      { writef("*nInvalid coroutine parent=%n*n", parent)
        RETURN
      }

      writef("*n%7i: ", cptr)
      IF cptr=mem(gptr+g_currco) DO writes("Current ")
      writes("Coroutine ")
      writearg(cont(cptr+co_fn))
      writef("  Parent %n", mem(cptr+co_parent))
      WHILE cont(cptr+hwm)=stackword DO hwm:=hwm-1
      writef("  Stack %n/%n*n", hwm-6, size)
      wrframe()

      // Move down one stack frame and output it.
      { LET a = mem(pptr)>>2
        IF a<cptr | a>pptr DO { writes(" Corrupt stack chain*n")
                                BREAK
                              }
        IF a=cptr | a=0    DO { writef(" Base of stack*n")
                                BREAK
                              }
        fsize := pptr-a
        pptr := a
        wrframe()
      } REPEAT
    }

    // Find next coroutine in the list
    cptr := cont(cptr+co_list)
  }
  TEST cptr THEN writef("*nCorrupt coroutine list*n")
            ELSE writef("*nEnd of coroutine list*n")
}

AND wrframe() BE
{ writef("%7i:", pptr)
  IF pptr=cptr DO { writes("   Base of stack*n"); RETURN }
  writearg(mem(pptr+2))
  FOR i=3 TO 6 UNLESS i>=fsize DO writearg(cont(pptr+i))
  newline()
  IF fsize>7 DO
  { writef("        ")
    FOR i = 7 TO 11 UNLESS i>=fsize DO writearg(cont(pptr+i))
    newline()
  }
  IF fsize>12 DO
  { writef("        ")
    FOR i = 12 TO 16 UNLESS i>=fsize DO writearg(cont(pptr+i))
    newline()
  }
}

AND writearg(n) BE TEST isfun(n)
                   THEN { // Write the function name right justified
                          LET s = (n>>2)-3  // MR 1/11/03
                          LET len = 0
                          FOR i = 1 TO 11 DO
                          { IF memb(s, i)='*s' BREAK
                            len := len+1
                          }
                          FOR i = len+1 TO 15 DO wrch('*s')
                          FOR i = 1 TO len DO wrch(memb(s, i))
                        }
                   ELSE TEST globword<=n<=globword+1000  // MR 1/11/03
                        THEN writef("         #G%z3#", n-globword)
                        ELSE TEST -10_000_000<=n<=10_000_000
                             THEN writef("    %11i", n)
                             ELSE writef("     #x%8x", n)

AND isfun(f) = VALOF
{ LET a = f>>2
  UNLESS (f&3)=0 & membase+4<a<=memlim RESULTIS FALSE // MR 25/9/03
  IF mem(a-4)=entryword & memb(a-3, 0)=11 RESULTIS TRUE 
  RESULTIS FALSE
}

AND nextword() = VALOF
{ UNTIL blkcount DO
  { UNLESS readwords(@blkcount, 1) DO
    { //sawritef("Bad count in dump file data*n")
      //abort(1000)
      blkcount, currword := 0, #xBAD00BAD
      RESULTIS currword
    }
    IF blkcount<0 DO
    { UNLESS readwords(@currword, 1) DO
      { sawritef("Bad block dump file*n")
        currword := #xBAD00BAD
      }
    }
    //TEST blkcount<0
    //THEN sawritef("%7i x %8x*n", -blkcount, currword)
    //ELSE sawritef("%7i BLOCK*n", blkcount)
//abort(1000)
  }

  TEST blkcount>0
  THEN { UNLESS readwords(@currword, 1) DO
         { sawritef("Bad block dump file*n")
           currword := #xBAD00BAD
         }
         blkcount := blkcount - 1
         RESULTIS currword
       }
  ELSE { blkcount := blkcount+1
         RESULTIS currword
       }

}

AND eqpages(p1, p2) = VALOF
{ FOR i = 0 TO pageupb UNLESS p1!i=p2!i RESULTIS FALSE
  RESULTIS TRUE
}

AND getimage(filename) = VALOF
{ // Return TRUE is successful, FALSE otherwise.
  LET res = FALSE                 // Assume the worst
  LET memupb = 0

  datastream := findinput(filename)

  UNLESS datastream RESULTIS FALSE
  selectinput(datastream)

  blkcount, currword := 0, #xBAD00BAD
  pagetab := 0
  pagenoupb := 0

  // The first word of the dump file is the upper bound of
  // the Cintcode memory in words.

  // Get the memory upb
  UNLESS readwords(@memupb, 1) DO
  { sawritef("Bad memupb in dump file*n")
    GOTO ret
  }

  IF memupb<0 GOTO ret  // and return FALSE

  pagenoupb := memupb>>pageshift    // pageshift typically 10
  pagetab := getvec(pagenoupb)
  UNLESS pagetab DO
  { writef("Unable to to allocate space for the page table*n")
    RESULTIS FALSE
  }
  // Initialise the page table
  FOR pageno = 0 TO pagenoupb DO pagetab!pageno := 0

  // Read pages from the dump sharing them whenever possible.
  FOR pageno = 0 TO pagenoupb DO
  { LET page = VEC pageupb          // pageupb is typically 1023 = 2**10-1
    AND newpage = 0
    // Read the next page
    FOR i = 0 TO pageupb DO page!i := nextword()

    // Is it the same as a previous page
    FOR p = 0 TO pageno-1 IF eqpages(page, pagetab!p) DO
    { // A matching page has been found
      newpage := pagetab!p
//sawritef("page %5i same as page %5i*n", pageno, p)
      BREAK
    }
    UNLESS newpage DO
    { // A matching page was not found so make a new one
      newpage := getvec(pageupb)
      FOR i = 0 TO pageupb DO newpage!i := page!i
//sawritef("page %5i is new*n", pageno)
    }
    // Put the page in the page table
    pagetab!pageno := newpage
//abort(1234)
  }

  res := TRUE   // Image successful read

ret:
  IF datastream DO endstream(datastream)
//abort(2222)
  RESULTIS res
}

AND deleteimage() BE IF pagetab DO
{ FOR pageno = 0 TO pagenoupb DO
  { LET page = pagetab!pageno
    UNLESS page LOOP
    freevec(page)
    // Delete all entries pointing to this page.
    FOR p = pageno+1 TO pagenoupb IF page=pagetab!p DO pagetab!p := 0
  }
  freevec(pagetab)
}

AND mem(p) = VALOF
{ LET pageno = p>>pageshift  // Typically 10
  AND offset = p & pagemask  // Typically 1023
  
  IF validaddr(p) & pageno <= pagenoupb DO
  { result2 := 0
    RESULTIS pagetab!pageno!offset
  }

  result2 := TRUE // To indicate success.
  RESULTIS pagetab!pageno!offset
}

AND memb(p, n) = VALOF
{ LET word = mem(p+(n>>2))
  UNLESS result2 RESULTIS 0
  RESULTIS (@word)%(n&3)
}
