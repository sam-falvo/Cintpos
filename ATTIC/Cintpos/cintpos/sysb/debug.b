// (c) Copyright: Martin Richards   September 2003

// This is code for the debug task (typically task 2)
// Very similar code can be found in BOOT.b (sadebug)

/*
20/9/02 MR
Made compatible with sadebug in BOOT.b
*/

SECTION "DEBUG"

GET "libhdr"

GLOBAL {
sadebug:ug
debug
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

bpt_addr
bpt_instr

ch
cptr
fsize
gptr
lch
membase
memlim
pptr
recp
recl
regs
style
val
vars
ctcb
rdflag
wrflag
wwrch
salev
}

MANIFEST
{
f_brk   = 2

r_a      = 0
r_b      = 1
r_c      = 2
r_p      = 3
r_g      = 4
r_st     = 5
r_pc     = 6
r_count  = 7
r_upb    = 7

}

// The DEBUG Task is given its startup pkt by CLI_INIT as the
// CLI starts up.

LET start(pkt) = VALOF
{ 
//sawritef("DEBUG: entered  pkt=%n*n", pkt)
  set_process_name("Debug_Task") // MR 3/2/03

  qpkt(pkt)  // Return the startup packet immediately

  // The DEBUG task has its own 10 variables
  vars         := TABLE 0,0,0,0,0,0,0,0,0,0

  // Get sadebug's breakpoint vectors from the rootnode
  // so that breakpoints can be set and cleared by the DEBUG task 
  bpt_addr     := rootnode!rtn_bptaddr
  bpt_instr    := rootnode!rtn_bptinstr

  recp  := level()
  recl  := recover

  membase      := rootnode!rtn_membase
  memlim       := membase + rootnode!rtn_memsize
  ctcb, pptr, gptr, regs := 0, 0, 0, 0

  style        := 'F'
  val          := 0

  initio()
  selectinput(findinput("**"))
  selectoutput(findoutput("**"))
  selectask(Task_cli)

//sawritef("DEBUG: 4*n")

  rch()
  GOTO sw

recover:
  ch := '*n'
nxt:                       // Main loop for debug commands
  IF ch='*n' DO prprompt()
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
       writes("SGn SPn SRn SVn SWn SAn    Store in variable*n")
       writes("Sn                         Select task n*n")
       writes("S.                         Select current task*n")
       writes("H          Hold/Release selected task*n")
       writes("K          Disable/Enable clock interrupts*n")
       writes("=          Print current value*n")
       writes("Tn         Print n consecutive locations*n")
       writes("I N        Print current/next Cintcode instruction*n")
       writes("D          Dump Cintcode memory to DUMP.mem*n")
       writes("Q          Quit -- leave the cintpos system*n")
       writes("M          Set/Reset memory watch address*n")
       writes("B 0Bn eBn  List, Unset or Set breakpoints*n")
       writes("X  (G4B9)  Set breakpoint 9 at start of clihook*n")
       writes("Z  (P1B9)  Set breakpoint 9 at return of current function*n")
       writes("C          Continue normal execution           -- disabled*n")
       writes("\          Single step execute one instruction -- disabled*n")
       writes(". ; [ ]    Move to current/parent/first/next coroutine*n")
       writes(",          Move down one stack frame*n")
       GOTO nxt

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
    CASE '|': rch(); val := val  |  rdval();  GOTO sw
    CASE '&': rch(); val := val  &  rdval();  GOTO sw

    CASE '<': val := val << 1;                GOTO nxt
    CASE '>': val := val >> 1;                GOTO nxt

    CASE '=': print(val); newline();          GOTO nxt

    CASE 'S': { LET type = ?
                rch()
                // Is it a task selection?
                IF ch='.' DO       { selectask(-1); rch(); GOTO sw }
                IF '0'<=ch<='9' DO { selectask(rdval());   GOTO sw }
                // No -- it must be a store instruction
                type := ch
                rch()
                !rdvaraddr(type) := val
                GOTO sw
              }

    CASE 'H': // Hold/Release currently selected task
              UNLESS ctcb DO { writes("No task selected*n"); GOTO nxt }
              { LET state = tcb_state!ctcb
                LET held = state & #b0010
                tcb_state!ctcb := state NEQV #b0010
                writef("*nTask %n %s*n",
                        tcb_taskid!ctcb, held -> "released", "held")
                GOTO nxt
              }

    CASE 'K': // Disable/enable clock interrupts
              TEST rtn_clkintson!rootnode
              THEN { rtn_clkintson!rootnode := FALSE
                     writef("*nClock interrupts disabled*n")
                   }
              ELSE { rtn_clkintson!rootnode := TRUE
                     writef("*nClock interrupts enabled*n")
                   }
              GOTO nxt

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

    CASE 'D': sys(Sys_dumpmem, 6) // Dump the memory (context=6)
              GOTO nxt
              GOTO recover

    CASE 'Q': sawritef("*n"); sys(Sys_quit, 0)   // Quit
         
    CASE 'N': val := nextpc(val)
    CASE 'I': prinstr(val); newline(); GOTO nxt

    CASE 'X':  // Equivalent to G4B9
         val := gptr!4  // set breakpoint 9 at clihook
         GOTO caseb

    CASE 'Z':  // Equivalent to P1B9
         val := pptr!1  // set breakpoint 9 to current return address

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
         GOTO nxt
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
               GOTO nxt

    CASE 'C':  // Continue execution.
               writef("Not available from DEBUG task*n")
               GOTO nxt

    CASE '\':  // Single step one instruction
               writef("Not available from DEBUG task*n")
               GOTO nxt


    CASE ',':  // Move down one stack frame and output it.
             { LET a = pptr!0>>2
               IF a=cptr | a=0 DO { writef(" Base of stack*n")
                                    GOTO nxt
                                  }
               fsize := pptr-a
               pptr := a
               wrframe()
               GOTO nxt
             }

    CASE ';': IF cptr DO
              { LET c = cont(cptr+co_parent)
                IF c<=0 DO
                { writef(" There is no parent coroutine*n")
                  GOTO recover
                }
                cptr := c
              }
              GOTO newc

    CASE '.': cptr := cont(gptr+g_currco)
              GOTO newc

    CASE ']': cptr := cont(cptr+co_list)
              IF cptr=0 DO { writef(" End of coroutine list*n")
                             GOTO recover
                           }
              GOTO newc

    CASE '[': cptr := cont(gptr+g_colist)

newc:         UNLESS cptr DO
              { writef("No such coroutine*n")
                GOTO nxt
              }
              TEST cptr=cont(gptr+g_currco)
              THEN pptr := regs!r_p>>2
              ELSE pptr := cont(cptr+co_pptr)>>2
              fsize := cptr + 6 + cptr!co_size - pptr
              wrcortn()
              GOTO nxt

      CASE endstreamch:
              writef("EOF hit*n")
              GOTO ret

      CASE '*s':
      CASE '*n':GOTO nxt
    }

ret:
}

AND prprompt() BE
{ LET id = -1
  LET st = regs!r_st
  LET letter = '?'
  IF ctcb DO id, letter := ctcb!tcb_taskid, ctcb!tcb_active -> 'a', 'd'
  IF st=1 DO letter := 'k'
  IF st=2 DO letter := 'b'
  IF st=3 DO letter := 'i'
  writef("%c%n** ", letter, id)  // DEBUG task prompt
  deplete(cos)
}

AND wrcortn() BE
{ LET size = cont(cptr+co_size)
  LET hwm = size+6
  writef(" %i7: ", cptr)
  TEST cptr!co_parent=-1
  THEN      writes("Root   ")
  ELSE TEST cptr!co_parent=0
       THEN writes("Dormant")
       ELSE writes("Active ")
  writes(" coroutine ")
  writearg(cont(cptr+co_fn))
  WHILE cont(cptr+hwm)=stackword DO hwm:=hwm-1
  writef("  Size %i5  Hwm %i5*n", size, hwm-6)
  wrframe()
}

AND wrframe() BE
{ writef(" %i7:", pptr)
  IF pptr=cptr DO { writes("   Base of stack*n"); RETURN }
  writearg(pptr!2)
  FOR i = 3 TO 6 UNLESS i>=fsize DO print(cont(pptr+i))
  newline()
  IF fsize>7 DO
  { writef("         ")
    FOR i = 7 TO 11 UNLESS i>=fsize DO print(cont(pptr+i))
    newline()
  }
}

AND writearg(n) BE TEST isfun(n)
                   THEN writef(" %s ", (n>>2)-3) // MR 25/9/03
                   ELSE TEST globword<=n<=globword+gptr!0
                        THEN writef("   #G%z3#    ", n-globword)
                        ELSE TEST -10000000<=n<=10000000
                             THEN writef(" %iB ", n)
                             ELSE writef("  #x%x8 ", n)

AND isfun(f) = VALOF
{ LET a = f>>2
  UNLESS (f&3)=0 & membase+4<a<=memlim RESULTIS FALSE // MR 25/9/03
  IF a!-4=entryword & a%-12=11 RESULTIS TRUE 
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
      CASE 'W': base, lim := ctcb, tcb_upb;         ENDCASE
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
                 RESULTIS ctcb

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
   IF ctcb <= a <= ctcb+tcb_upb         DO type, base := 'W', ctcb
   IF regs <= a <= regs+r_upb           DO type, base := 'R', regs
   writef("*n%c%i3:", type, a-base)
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

AND selectask(id) BE
{ // IF id<0 select the current tcb and the trap registers
  // otherwise select the tcb for task id and the registers
  // corresponding to that task.
  LET tasktab = rtn_tasktab!rootnode
  LET st = regs -> regs!r_st, 0
  LET t = 0

  IF tasktab & 0<id<=tasktab!0 DO t := tasktab!id
  IF id<0 DO t := rtn_crntask!rootnode

  UNLESS t DO
  { sawritef("Task %n does not exist*n", id)
    RETURN
  }

  ctcb, cptr := t, 0

  regs := @tcb_regs!t       // regs belonging to non current task


//sawritef("regs=%n*n", regs)
  gptr  := r_g !regs >> 2
  pptr  := r_p !regs >> 2

  // Set current coroutine if in user or kernel mode and the task
  // is active
  IF st<2 & t!tcb_active DO cptr := gptr!g_currco

  UNLESS cptr <= pptr <= cptr + cptr!co_size + 6 DO cptr := 0

  fsize := 100
//sawritef("cptr=%n*n", cptr)
  IF cptr DO fsize := cptr + 6 + cptr!co_size - pptr
//sawritef("fsize=%n*n", fsize)
}

AND cont(a) = !checkaddr(a)

AND error() BE { writes("  ??*n"); longjump(recp, recl) }

//AND rch() BE { lch := rdch(); ch := capitalch(lch)
//sawritef(" DEBUG: rch read %n*n", lch) }

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

AND testbreak() BE IF testflags(flag_b) DO error()

AND rch() BE
{ 
  IF wrflag DO deplete(cos)
  wrflag := FALSE
  testbreak()
  lch := rdch()
  testflags(flag_b)

  ch := capitalch(lch)
  rdflag := TRUE
}


AND wch(c) BE
{ c := c&#377
  wrflag := TRUE
  wwrch(c)
}
