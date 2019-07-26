// (c) Copyright: Martin Richards   February 1997

MODULE mboot

GET "mcpl.h"

GLOBAL
  checkaddr:Ug,
  cont,
  debug,
  error,
  gb,
  gh,
  gs,
  gsh,
  gw,
  instrtype,
  isfun,
  nextp,
  praddr,
  prinstr,
  print,
  rdval,
  rdvaraddr,
  rch,
  wrcortn,
  wrframe,
  writearg,

  bpt,
  bpt_addr,
  bpt_instr,
  brkstep,
  ch,
  cptr,
  fsize,
  gptr,
  hilim,
  lch,
  lolim,
  membase,
  memlim,
  oldcount,
  pptr,
  recp,
  recl,
  regs,
  style,
  snglstep,
  val,
  vars


MANIFEST
  G_globsize=0, G_sys=3, G_currco=7, G_colist=8, G_rootnode=9,
  G_upb=1000,

  Globword  = #xEFEF0000,
  Entryword = #x0000DFDF,

  F_brk   = 2,

  R_a = 0, R_b, R_c, R_p, R_g, R_st, R_pc, R_count, R_h,
  R_upb   = R_h

FUN start : => 
   LET regv = VEC R_upb
   LET g = getvec G_upb  // The CLI global vector
   LET p = getvec 207    // The CLI root coroutine stack
   
   rdch, wrch := sardch, sawrch
   writes("MCPL Mintcode System\n")

   FOR i = 0 TO 207   DO p!i := 0
   FOR i = 0 TO G_upb DO g!i := Globword + i;
   g!G_globsize := G_upb
   g!G_sys      := sys
   g!G_rootnode := rootnode
   g!G_currco   := 0          // These are initialised to simplify
   g!G_colist   := 0          // the initial debugging of mintasm.
   
   regs         := regv   
   regs!R_a     := 0          // A
   regs!R_b     := 0          // B
   regs!R_c     := 0          // C
   regs!R_p     := p          // P
   regs!R_g     := g          // G
   regs!R_st    := 0          // ST
   regs!R_pc    := startcli   // PC
   regs!R_count := -1         // Count  (-1 = infinity)
   regs!R_h     :=  0         // H

   membase      := rootnode!Rtn_membase
   memlim       := membase + rootnode!Rtn_memsize
   lolim, hilim := membase, memlim
   vars         := TABLE [0,0,0,0,0,0,0,0,0,0]
   bpt_addr     := TABLE [0,0,0,0,0,0,0,0,0,0]
   bpt_instr    := TABLE [0,0,0,0,0,0,0,0,0,0]
   style        := 'F'
   val          := 0
   brkstep, snglstep := FALSE, FALSE

   sys := [sys!0] // Copy of sys body -- to make breakpoints in sys work!

   { LET res = sys(1, regs)  // Call the interpreter
     IF res=0 DO sys(0, 0)

     IF res=-1 DO // select the fast interpreter (mintasm)
     { regs!R_count := -1
       LOOP
     }

     IF res=-2 DO // select the slow interpreter (minterp)
     { regs!R_count := #x7FFFFFFF
       LOOP
     }

     debug res 
   } REPEAT
.
FUN startcli : =>
  LET f=startcli, size200, c=0, h=0
  currco := @f-12          // Initialise the coroutine environment
  colist := currco
  currco!Co_pptr   := 0
  currco!Co_parent := -1  // Mark as root coroutine.
  currco!Co_list   := 0

  sys(24, rootnode!Rtn_msyslib)  // globin(msyslib)
  sys(24, rootnode!Rtn_mlib)     // globin(mlib)
   
  rootnode!Rtn_keyboard := 0
  rootnode!Rtn_screen   := 0

  selectoutput(findoutput "*")
  selectinput(findinput "*")

  rootnode!Rtn_mcli := globin(loadseg("sysmin/MCLI"))
   
  start()
  sys(0, 0)
.
MANIFEST
  Recover=100, Nxt, Ret

FUN debug : code =>
  bpt   := -1
  gptr  := regs!R_g
  cptr  := gptr!G_currco
  pptr  := regs!R_p
  fsize := 7 + cptr!Co_size
  val   := regs!R_pc

  FOR i = 0 TO 9 DO       // Remove all BRK instructions, if any.
  { LET ba = bpt_addr!i
    IF ba~=0 AND %ba=F_brk DO %ba := bpt_instr!i
  }

  { IF code=3 DO  // Zero count.
    { IF brkstep DO
      { brkstep := FALSE  // Breakpoint continuation.
        IF oldcount>=0 DO oldcount := oldcount-1
        regs!R_count := oldcount
        // Restore BRK instructions and continue.
        FOR i = 0 TO 9 DO { LET ba=bpt_addr!i // Set all breakpoints.
                            UNLESS ba=0 DO %ba := F_brk
                          }
        RETURN
      }
      IF snglstep DO
      { snglstep := FALSE // Single step.
        IF oldcount>=0 DO oldcount := oldcount-1
        regs!R_count := oldcount
        writes("A="); print(regs!R_a)
        writes("B="); print(regs!R_b)
        prinstr(val)
        newline()
        RAISE Recover
      }
    }

    IF code=2 DO  // BRK instruction
      FOR i = 0 TO 9 IF bpt_addr!i=val DO
      { bpt := i
        writef("\n!! BPT %d:  ", bpt)
        writearg(val)
        writes("\n   ")
        writes("A="); print(regs!R_a)
        writes("B="); print(regs!R_b)
        prinstr(val)
        newline()
        RAISE Recover
      }

    IF code=10 DO // Cintasm single step
    { writes("A="); print(regs!R_a)
      writes("B="); print(regs!R_b)
      prinstr(val)
      newline()
      RAISE Recover
    }

    LET gn = regs!R_pc - Globword
    LET mess =  MATCH code
                :   1 => "Illegal instruction"
                :   2 => "BRK instruction"
                :   3 => "Zero count"
                :   4 => 0<=gn<=gptr!0 -> "G%d unassigned",
                                          "Negative pc"
                :   5 => "Division by zero"
                :   6 => "Unhandled exception"
                :  10 => "Cintasm single step"
                :  99 => "User requested"
                : 110 => "Callco fault"
                : 111 => "Resumeco fault"
                : 112 => "Deleteco fault"
                : 186 => "Selectinput fault"
                : 187 => "Selectoutput fault"
                : 189 => "Wrch fault"
                : 190 => "Endread fault"
                : 191 => "Endwrite fault"
                : 197 => "Store chain fault"
                :     => "Unknown fault"
                .
     writef("\n!! ABORT %d: ", code)
     writef(mess, gn)
     newline()
  } HANDLE : Recover => .


  ch := '\n'

{ // Main loop

 { MATCH ch
   : '?' =>
         writes("\n\
                \?     Print list of debug commands\n\
                \Gn Pn Rn Vn            Variables\n\
                \G  P  R  V             Pointers\n\
                \n #b101 #o377 #x7FF 'c Constants\n\
                \*e /e %e +e -e |e &e   Dyadic operators\n\
                \< > !                  Postfixed operators\n\
                \SGn SPn SRn SVn        Store in variable\n\
                \=          Print current value\n\
                \Tn         Print n consecutive locations\n\
                \$c         Set print style C, D, F, B, O, S, U or X\n\
                \LL LH      Set Low and High store limits\n\
                \I          Print current instruction\n\
                \N          Print next instruction\n\
                \Q          Quit\n\
                \B 0Bn eBn  List, Unset or Set breakpoints\n\
                \C          Continue execution\n\
                \X          Equivalent to G4B9C\n\
                \Z          Equivalent to P1B9C\n\
                \\\          Execute one instruction\n\
                \,          Move down one stack frame\n\
                \.          Move to current coroutine\n\
                \;          Move to parent coroutine\n\
                \[          Move to first coroutine\n\
                \]          Move to next coroutine\n")
         RAISE Recover

   : '0'..'9' | '#' | '\'' | 'G' | 'P' | 'R' | 'V' =>
              val := rdval();          LOOP

   : '+' => rch(); val   +:= rdval();  LOOP
   : '-' => rch(); val   -:= rdval();  LOOP
   : '*' => rch(); val   *:= rdval();  LOOP
   : '/' => rch(); val   /:= rdval();  LOOP
   : '%' => rch(); val MOD:= rdval();  LOOP
   : '|' => rch(); val   |:= rdval();  LOOP
   : '&' => rch(); val   &:= rdval();  LOOP

   : '<' => val <<:= 1;                RAISE Nxt
   : '>' => val >>:= 1;                RAISE Nxt
   : '!' => val := !val;               RAISE Nxt

   : '=' => print val; newline();      RAISE Recover

   : 'S' => LET type = rch()
            rch()
            ! rdvaraddr type := val
            LOOP

   : 'T' => rch()
            FOR i=0 TO rdn()-1 DO
            {  IF i MOD 5 = 0 DO praddr val
               print(cont val)
               val +:= Bpw
            }
            newline()
            LOOP

   : '$' => rch(); style := ch;           RAISE Nxt

   : 'Q' => sys(0, 0)   // Quit
         
   : 'L' => rch()
            IF ch='L' DO {  lolim := val; RAISE Nxt  }
            IF ch='H' DO {  hilim := val; RAISE Nxt  }
            error()
         
   : 'N' => val := nextpc(val)
            prinstr val; newline(); RAISE Recover

   : 'I' => prinstr val; newline(); RAISE Recover

   : 'X' =>  // Equivalent to G4B9C
       val := gptr!4  // set break point 9 to entry point of clihook
       GOTO 'B'

   : 'Z' =>  // Equivalent to P1B9C
       val := pptr!1  // set break point 9 to return address
       GOTO 'B'

   : 'B' =>  // Set, clear or display breakpoints.
      LET comch = ch
      TEST comch='B' THEN rch() ELSE ch := '9'
      IF '0'<=ch<='9' DO
      { LET n = ch - '0'  // Set or Clear a break point.
        bpt_addr!n := 0
        IF val=0 RAISE Nxt
        checkaddr val
        FOR i = 0 TO 9 DO
           IF bpt_addr!i=val DO bpt_addr!i := 0
        bpt_addr!n  := val
        bpt_instr!n := %val
        IF comch='B' RAISE Nxt
        GOTO 'C'
      }
      UNLESS ch='\n' DO newline()
      FOR i = 0 TO 9 DO  // List break points.
      { LET ba=bpt_addr!i
        UNLESS ba=0 DO
        { writef("%d:  ", i)
          writearg ba
          newline()
        }
      }
      RAISE Recover

   : 'C' => // Continue execution.
            LET pc = regs!R_pc
            newline()
            FOR i = 0 TO 9 IF pc=bpt_addr!i DO
            {  oldcount := regs!R_count
               regs!R_count := 1
               brkstep := TRUE
               RETURN  // Execute the instruction at the break point
            }
            RAISE Ret  // Resume normal execution.

   : '\\'=> oldcount := regs!R_count // Single step execution.
            regs!R_count := 1
            snglstep := TRUE
            RETURN

   : ',' => // Move down one stack frame and output it.
            LET a = pptr!0
            IF a=cptr DO {  writef(" Base of stack\n")
                            RAISE Recover
                         }
            fsize := (pptr-a)/Bpw
            pptr := a
            wrframe()
            RAISE Recover

   : '.' => TEST cptr=cont(@ gptr!G_currco)
            THEN pptr := regs!R_p
            ELSE pptr := cont(@ cptr!Co_pptr)
            fsize := 7 + cptr!Co_size
            wrcortn()
            RAISE Recover

   : ';' => IF cont(@ cptr!Co_parent)<0 DO
            {  writef(" End of parent chain\n")
               RAISE Recover
            }
            cptr := cont(@ cptr!Co_parent)
            GOTO '.'

   : ']' => cptr := cont(@ cptr!Co_list)
            IF cptr=0 DO {  writef(" End of coroutine list\n")
                            RAISE Recover
                         }
            GOTO '.'

   : '[' => cptr := cont(@ gptr!G_colist)
            GOTO '.'

   : Endstreamch => newline(); RAISE Ret 

   : '\n' => RAISE Recover

   : ' '  => RAISE Nxt

   :      => error()
   .
 } HANDLE
   : Recover => ch := '\n'
                IF ch='\n' DO writes("* ")
                rch()
                LOOP

   : Nxt     => IF ch='\n' DO writes("* ")
                rch()
                LOOP

   : Ret     => FOR i = 0 TO 9 DO
                { LET ba=bpt_addr!i // Set all breakpoints.
                  UNLESS ba=0 DO %ba := F_brk
                }
                RETURN
   .
} REPEAT
.

FUN wrcortn : =>
  LET size = cont(@ cptr!Co_size)
  LET hwm = size+7
  writef(" %7d: ", cptr)
  TEST cptr!Co_parent=-1
  THEN      writes("   Root")
  ELSE TEST cptr!Co_parent=0
       THEN writes("Dormant")
       ELSE writes(" Active")
  writes(" coroutine ")
  writearg(cont(@ cptr!Co_fn))
  WHILE cont(@ cptr!hwm)=0 DO hwm:=hwm-1
  writef("  Size %5d  Hwm %5d\n   ", size, hwm-7)
  wrframe()
.
FUN wrframe : =>
  LET n = fsize - 1
  IF n>6 DO n := 6
  writef(" %7d:", pptr)
  IF pptr=cptr DO {  writes("   Base of stack\n"); RETURN }
  writearg(pptr!2)
  FOR i=3 TO n DO print(cont(@ pptr!i))
  newline()
.
FUN writearg : n => TEST isfun n 
                    THEN writef("    %s ", @ n!-2)
                    ELSE TEST Globword<=n<=Globword+999
                         THEN writef("    GLOB%3d ", n-Globword)
                         ELSE writef(" %10d ", n)
.
FUN isfun : f =>
  UNLESS (f&3)=0 AND membase<f<=memlim RETURN FALSE
  RETURN f!-3=Entryword
.
FUN rdn : =>
  LET res = 0
  WHILE '0'<=ch<='9' DO {  res := res*10 + ch - '0'; rch() }
  RETURN res
.
FUN rdvaraddr : type =>
  LET base, lim, n
  UNLESS '0'<=ch<='9' DO error()
  n := rdn()
  { MATCH type
    : 'G' => base, lim := gptr, gptr!G_globsize
    : 'P' => base, lim := pptr, fsize-1
    : 'R' => base, lim := regs, 7
    : 'V' => base, lim := vars, 9
    :     => error()
    .
  }
  UNLESS 0<=n<=lim DO error()
  RETURN @ base!n
.
FUN rdval : =>
  MATCH ch
  : 'G'     =>  rch()
                IF '0'<=ch<='9' RETURN !rdvaraddr('G')
                RETURN gptr

  : 'P'     =>  rch()
                IF '0'<=ch<='9' RETURN !rdvaraddr('P')
                RETURN pptr

  : 'R'     =>  rch()
                IF '0'<=ch<='9' RETURN !rdvaraddr('R')
                RETURN regs

  : 'V'     =>  rch()
                IF '0'<=ch<='9' RETURN !rdvaraddr('V')
                RETURN vars

  : '\''    =>  rch(); LET res = lch; rch();  RETURN res

  : '0'..'9' |'#' =>
                LET radix = 10  
                IF ch='#' DO
                { radix := 16
                  rch()
                  MATCH ch
                  : 'X'  => radix := 16; rch()
                  : 'O'  => radix :=  8; rch()
                  : 'B'  => radix :=  2; rch()
                  :      =>
                  .
                }

                LET res = 0
                { LET d = 100
                  IF '0'<=ch<='9' DO d := ch-'0'
                  IF 'A'<=ch<='F' DO d := ch-'A'+10
                  IF d>=radix RETURN res
                  res := res*radix+d
                  rch()
                } REPEAT

  :          => error()
  .
.
FUN praddr : a =>
  LET type = 'A'
  LET ad   = a
  IF gptr<=a<=@ gptr!(gptr!G_globsize) DO type, ad := 'G', (a-gptr)/Bpw
  IF pptr<=a<=@ pptr!(fsize-1)         DO type, ad := 'P', (a-pptr)/Bpw
  IF regs<=a<=@ regs!7                 DO type, ad := 'R', (a-regs)/Bpw
  IF vars<=a<=@ vars!9                 DO type, ad := 'V', (a-vars)/Bpw
  writef("\n%c%3d:", type, ad)
.
FUN print : n =>
  MATCH style
  : 'C' =>  LET p = @n
            writef(" %C%C%C%C ", p%0, p%1, p%2, p%3)
  : 'D' =>  writef( " %10d ", n)
  : 'F' =>  writearg(n)
  : 'B' =>  writef( " %32b ", n)
  : 'O' =>  writef( " %12o ", n)
  : 'S' =>  writef( " %s ",  n)
  : 'U' =>  writef( " %10d ", n)
  : 'X' =>  writef( " %8x ", n)
  :     =>  error()
  .
.

FUN checkaddr : a =>
  UNLESS lolim<=a<=hilim DO error()
  RETURN a
.
FUN cont : a => !checkaddr(a)
.
FUN error : => writes("  ??\n")
               RAISE Recover
.
FUN rch : => lch := rdch(); ch := capitalch lch
.
FUN instrtype : f => "??0000000000RI10000000000000RIRI\
                     \124111111111111111110000RIRIRIRI\
                     \12411111111111111111000000RIRIRI\
                     \1242222222222222222200000000RIRI\
                     \124000000000000000000000000?RIRI\
                     \12400000000000000000000000RIRIRI\
                     \12400000000000200000000000000000\
                     \124000000000002000000000000?RI00" % f
.
FUN wrfcode : f =>
  LET s =  "     -     K   LLP     L    LP    SP    AP     A\
           \     -    KH  LLPH    LH   LPH   SPH   APH    AH\
           \   BRK    KW  LLPW    LW   LPW   SPW   APW    AW\
           \    K3   K3G  K3G1  K3GH   LP3   SP3   AP3  L0P3\
           \    K4   K4G  K4G1  K4GH   LP4   SP4   AP4  L0P4\
           \    K5   K5G  K5G1  K5GH   LP5   SP5   AP5  L0P5\
           \    K6   K6G  K6G1  K6GH   LP6   SP6   AP6  L0P6\
           \    K7   K7G  K7G1  K7GH   LP7   SP7   AP7  L0P7\
           \    K8   K8G  K8G1  K8GH   LP8   SP8   AP8  L0P8\
           \    K9   K9G  K9G1  K9GH   LP9   SP9   AP9  L0P9\
           \   K10  K10G K10G1 K10GH  LP10  SP10  AP10 L0P10\
           \   K11  K11G K11G1 K11GH  LP11  SP11  AP11 L0P11\
           \    LF   S0G  S0G1  S0GH  LP12  SP12  AP12 L0P12\
           \   LF$   L0G  L0G1  L0GH  LP13  SP13  INDW     S\
           \    LM   L1G  L1G1  L1GH  LP14  SP14   LMH    SH\
           \   LM1   L2G  L2G1  L2GH  LP15  SP15   BTC  MDIV\
           \    L0    LG   LG1   LGH  LP16  SP16   NOP CHGCO\
           \    L1    SG   SG1   SGH   SYS    S1    A1   NEG\
           \    L2   LLG  LLG1  LLGH LVIND    S2    A2   NOT\
           \    L3    AG   AG1   AGH   STB    S3    A3 INC1B\
           \    L4   MUL   ADD    RV    ST    S4    A4 INC4B\
           \    L5   DIV   SUB   RV1   ST1   XCH    A5 DEC1B\
           \    L6   MOD   LSH   RV2   ST2  INDB  RVP3 DEC4B\
           \    L7   XOR   RSH   RV3   ST3 INDB0  RVP4 INC1A\
           \    L8    SL   AND   RV4  STP3   ATC  RVP5 INC4A\
           \    L9   SL$    OR   RV5  STP4   ATB  RVP6 DEC1A\
           \   L10    LL   LLL   RV6  STP5     J  RVP7 DEC4A\
           \  FHOP   LL$  LLL$   RTN     -    J$ ST0P3     -\
           \   JEQ   JNE   JLS   JGR   JLE   JGE ST0P4  HAND\
           \  JEQ$  JNE$  JLS$  JGR$  JLE$  JGE$ ST1P3 HAND$\
           \  JEQ0  JNE0  JLS0  JGR0  JLE0  JGE0 ST1P4   UNH\
           \ JEQ0$ JNE0$ JLS0$ JGR0$ JLE0$ JGE0$   CTA RAISE"

  LET p = 6 *((f&31)<<3 + f>>5)
  FOR i = p TO p+5 DO wrch(s%i)
.
FUN prinstr : pc =>
  LET a = 0
  writef(" %7d: ", pc)
  checkaddr(pc)
  wrfcode(%pc)
  MATCH instrtype(%pc)
  : '0' => RETURN
  : '1' => a  := gb(pc+1)
  : '2' => a  := gh(pc+1)
  : '4' => a  := gw(pc+1)
  : 'R' => a  := pc+1 + gsb(pc+1)
  : 'I' => pc := (pc+1 + 2*gb(pc+1)) & #xFFFFFFFE
           a  := pc + gsh(pc)
  :     => RETURN
  .
  writef("  %d", a)
  vars!9 := a
.
FUN gb : pc => %pc
.
FUN gsb : pc => %pc<=127 -> %pc, %pc-256
.
FUN gsh : pc =>
  LET h = gh(pc)
  RETURN h<=#x7FFF -> h, h - #x10000
.
FUN gh : pc =>
  LET w, p = @w  // Designed to work on both Big and Little Ender M/Cs.
  p%0, p%1, p%2, p%3 := pc%0, pc%1, pc%0, pc%1
  RETURN w & #xFFFF
.
FUN gw : pc =>
  LET w, p = @w  // Designed to work on both Big and Little Ender M/Cs.
  p%0, p%1, p%2, p%3 := pc%0, pc%1, pc%2, pc%3
  RETURN w
.
FUN nextpc : pc => MATCH instrtype(%pc)
                   : '0'             => RETURN pc+1
                   : '1' | 'R' | 'I' => RETURN pc+2
                   : '2'             => RETURN pc+3
                   : '4'             => RETURN pc+5
                   : 'B'             => pc := pc+2 & #xFFFFFFFE
                                        RETURN pc + 4*gh(pc) + 6
                   : 'L'             => pc := pc+2 & #xFFFFFFFE
                                        RETURN pc + 2*gh(pc) + 6
                   :                 => RETURN pc+1
                   .
.
