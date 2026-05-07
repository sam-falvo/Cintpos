// (c) M. Richards  Copyright 21 July  1997

/*
** Usage:
**
** preload bcpl                Preload the program to study
** stats                       Enable stats gathering on next command
** bcpl com/bcpl.b to junk     Execute the command to study
** 
** interpreter                 Select the fast interpreter (cintasm)
**                             stats automatically selects the slow one
**
** stats to STATS              Send instruction frequencies to file
**                             or
** stats profile to PROFILE    Send detailed profile info to file
**                             or
** stats analysis to ANALYSIS  Generate statistical analysis to file
*/

MODULE stats

GET "mcpl.h"
GET "mcli.h"

MANIFEST 
  Modword        = #xFDDF,   // MODULE and Entry marker words.
  Entryword      = #xDFDF,

  Modnamesize    = 8/Bpw,
  Routnamesize   = 8/Bpw,
  Nameoffset     = -Routnamesize,

  Disupb=26, Typeupb=8

GLOBAL
  s_lpdis:Ug, d_lpdis,
  s_spdis,    d_spdis,
  s_grds,     d_grds,
  s_gwrs,     d_gwrs,
  s_kdis,     d_kdis,
  s_rfdis,    d_rfdis,
  s_rbdis,    d_rbdis,
  s_idis,     d_idis,
  s_lndis,    d_lndis,
  s_lmdis,    d_lmdis,

  s_gvecap,   d_gvecap,
  s_pvecap,   d_pvecap,
  s_incdec,   d_incdec,
  s_gbyt,     d_gbyt,
  s_pbyt,     d_pbyt,
  s_adds,     d_adds,
  s_subs,     d_subs,
  s_eops,     d_eops,
  s_fv,       d_fv,
  s_fcount,   d_fcount,
  s_cj,       d_cj,
  s_cj0,      d_cj0,
  s_ftype,    d_ftype,
  s_rtn,      d_rtn,

  tostream,
  fcode,
  freq,
  curmodule,
  pc,

  profile,
  analysis

FUN start : =>
  LET argv = VEC 20
  LET outstream = 0
  LET tallyv = rootnode!Rtn_tallyv
  LET oldout = output()

  UNLESS tallyv DO
  { writes "Statistic gathering not available\n"
    RETURN 20
  }
   
  UNLESS rdargs("TO/K,PROFILE/S,ANALYSIS/S",argv, 20) DO
  { writes "Bad arguments for STATS\n"
    RETURN 20
  }
   
  UNLESS argv!0 OR argv!1 OR argv!2 DO
  { cli_tallyflag := TRUE
    sys(0, -2) // Select slow statistics gathering interpreter
    writef("Statistics gathering enabled, tally vector size %d words\n",
           tallyv!0)
    RETURN 0
  }

  profile  := argv!1
  analysis := argv!2

  UNLESS argv!0 DO argv!0 := "*"

  outstream := findoutput(argv!0)
  IF outstream=0 DO { writes("Trouble with file %s\n", argv!0)
                      RETURN 20
                    }
  selectoutput outstream
   
  init_analysis()
  statsout(rootnode!Rtn_blklist, tallyv, tallyv!0)
  free_storage()

  UNLESS outstream=oldout DO endwrite()
  selectoutput oldout
  RETURN 0
.
FUN good_name : string =>
   UNLESS string%7 = 0 RETURN FALSE

   FOR i = 0 TO 6 DO
   { LET ch = string%i

     UNLESS 'A'<=ch<='Z' OR 'a'<=ch<='z' OR '0'<=ch<='9' OR
            ch='_' OR ch=' ' RETURN FALSE
   }

   RETURN TRUE
.
FUN statsout : base, tallyv, upb =>
  curmodule := 0

  // Scan memory
  FOR i = 1 TO upb DO
  { pc := base + i
    fcode := %pc
    freq  := tallyv!i

    IF profile DO
    { IF i MOD Bpw = 0 DO              // On a word boundary
      { IF pc!1=Modword AND good_name(@pc!2) DO
        { curmodule := i
          writef("\n%6d: Module:    %s   Size: %d\n",
                    curmodule,      @pc!2,      pc!0)
        }
        IF pc!0=Entryword AND good_name(@pc!1) DO
           writef("+%5d: Function:  %s\n", i-curmodule, @pc!1)
      }

      IF freq DO
      { writef("+%5d:%6d ", i-curmodule, freq)
        prinstr(pc, base+curmodule)
        newline()
      }
    }

    IF freq DO analyse_instr()

    IF intflag() DO { writef "\n++++ BREAK\n"
                      BREAK
                    }
  }
   
  IF analysis DO { pr_analysis(); RETURN  }

  // Otherwise print a simple frequency table

  writef("\nInstruction frequencies (total executed %d)\n", d_fcount)
   
  FOR i = 0 TO 128 BY 128 FOR j = 0 TO 31 DO
  { newline()
    FOR k = 0 TO 96 BY 32 DO
    { LET f = i + j + k
      IF f=128 DO newline()
      wrfcode f
      writef(" %7d   ", d_fv!f)
    }
  }

  newline()
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
  wrfcode(%pc)
  MATCH instrtype(%pc)
  : '0' => RETURN
  : '1' => a  := gb(pc+1)
  : '2' => a  := gh(pc+1)
  : '4' => a  := gw(pc+1)
  : 'R' => a  := pc+1 + gsb(pc+1) - curmodule
  : 'I' => pc := (pc+1 + 2*gb(pc+1)) & #xFFFFFFFE
           a  := pc + gsh(pc) - curmodule
  :     => RETURN
  .
  writef("  %d", a)
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

FUN init_analysis : =>
  s_lpdis, d_lpdis := getvec Disupb, getvec Disupb
  s_spdis, d_spdis := getvec Disupb, getvec Disupb
  s_grds, d_grds := 0, 0
  s_gwrs, d_gwrs := 0, 0
  s_kdis,  d_kdis  := getvec Disupb, getvec Disupb
  s_rfdis, d_rfdis := getvec Disupb, getvec Disupb
  s_rbdis, d_rbdis := getvec Disupb, getvec Disupb
  s_idis,  d_idis  := getvec Disupb, getvec Disupb
  s_lndis, d_lndis := getvec Disupb, getvec Disupb
  s_lmdis, d_lmdis := getvec Disupb, getvec Disupb

  FOR i = 0 TO Disupb DO
  { s_lpdis!i, d_lpdis!i := 0, 0
    s_spdis!i, d_spdis!i := 0, 0
    s_kdis!i,  d_kdis!i  := 0, 0
    s_rfdis!i, d_rfdis!i := 0, 0
    s_rbdis!i, d_rbdis!i := 0, 0
    s_idis!i,  d_idis!i  := 0, 0
    s_lndis!i, d_lndis!i := 0, 0
    s_lmdis!i, d_lmdis!i := 0, 0
  }

  s_gvecap, d_gvecap := 0, 0
  s_pvecap, d_pvecap := 0, 0
  s_incdec, d_incdec := 0, 0
  s_gbyt,   d_gbyt   := 0, 0
  s_pbyt,   d_pbyt   := 0, 0
  s_adds,   d_adds   := 0, 0
  s_subs,   d_subs   := 0, 0
  s_eops,   d_eops   := 0, 0
   
  s_fv, d_fv := getvec 255, getvec 255
  FOR i = 0 TO 255 DO s_fv!i, d_fv!i := 0, 0

  s_fcount, d_fcount := 0, 0
  s_cj,  d_cj  := 0, 0
  s_cj0, d_cj0 := 0, 0

  s_ftype, d_ftype := getvec Typeupb, getvec Typeupb
  FOR i = 0 TO Typeupb DO s_ftype!i, d_ftype!i := 0, 0
   
  s_rtn, d_rtn := 0, 0
.
FUN free_storage : =>
  freevec s_lpdis;   freevec d_lpdis
  freevec s_spdis;   freevec d_spdis
  freevec s_kdis;    freevec d_kdis
  freevec s_rfdis;   freevec d_rfdis
  freevec s_rbdis;   freevec d_rbdis
  freevec s_idis;    freevec d_idis
  freevec s_lndis;   freevec d_lndis
  freevec s_lmdis;   freevec d_lmdis

  freevec s_fv;      freevec d_fv
  freevec s_ftype;   freevec d_ftype
.
FUN pr_analysis : =>
  writef("\n\nInstructions  %7d(%5d)\n", d_fcount, s_fcount)
   
  FOR i = 0 TO 128 BY 128 FOR j = 0 TO 31 DO
  { newline()
    FOR k = 0 TO 96 BY 32 DO
    { LET f = i + j + k
      IF f=128 DO newline()
      writef("    ")
      wrfcode(f)
      writef("    ")
    }
    newline()
    FOR k = 0 TO 96 BY 32 DO
    { LET f = i + j + k
      writef(" %7d(%4d)", d_fv!f, s_fv!f)
    }
  }
  writes("\n\n     GVECAP        PVECAP         GBYT          PBYT\n")
  writef("%7d(%4d) ", d_gvecap, s_gvecap)
  writef("%7d(%4d) ", d_pvecap, s_pvecap)
  writef("%7d(%4d) ", d_gbyt,  s_gbyt)
  writef("%7d(%4d) ", d_pbyt,  s_pbyt)
   
  writes("\n\n     INC/DEC       ADDS          SUBS           EOPS\n")
  writef("%7d(%4d) ", d_incdec, s_incdec)
  writef("%7d(%4d) ", d_adds, s_adds)
  writef("%7d(%4d) ", d_subs, s_subs)
  writef("%7d(%4d) ", d_eops, s_eops)

  writes("\n\n      CJ            CJ0\n")
  writef("%7d(%4d) ", d_cj,  s_cj)
  writef("%7d(%4d) ", d_cj - curmodule, s_cj0)
   
  writes("\n\nOperand distributions")
   
  writes("\n\n       LP            SP            K\n")
  FOR i = 0 TO Disupb DO
  { writef("%7d(%4d) ", d_lpdis!i, s_lpdis!i)
    writef("%7d(%4d) ", d_spdis!i, s_spdis!i)
    writef("%7d(%4d) ", d_kdis!i,  s_kdis!i)
    wrdisrange(i)
    newline()
  }

  writes("\n\n       LG            SG            RTN\n")
  writef("%7d(%4d) ", d_grds, s_grds)
  writef("%7d(%4d) ", d_gwrs, s_gwrs)
  writef("%7d(%4d) ", d_rtn,  s_rtn)

  writes(
 "\n\n       LN            LM            RF            RB            I\n")
  FOR i = 0 TO Disupb DO
  { writef("%7d(%4d) ", d_lndis!i, s_lndis!i)
    writef("%7d(%4d) ", d_lmdis!i, s_lmdis!i)
    writef("%7d(%4d) ", d_rfdis!i, s_rfdis!i)
    writef("%7d(%4d) ", d_rbdis!i, s_rbdis!i)
    writef("%7d(%4d) ", d_idis!i,  s_idis!i)
    wrdisrange(i)
    newline()
  }
  writes "\nInstruction types\n\n"
  FOR i = 0 TO 8 DO
    writef(" %c  %8d(%4d)  %4d\n",
           "?0124RIBL"%i, d_ftype!i, s_ftype!i,
            ("012352200"%i-'0')*s_ftype!i)
.

FUN analyse_instr : =>
  LET a = 0
  s_fv!fcode, d_fv!fcode := s_fv!fcode+1, d_fv!fcode+freq
  s_fcount, d_fcount := s_fcount+1, d_fcount+freq

  MATCH instrtype(fcode)
  : '0' => s_ftype!1, d_ftype!1 := s_ftype!1+1, d_ftype!1+freq
  : '1' => s_ftype!2, d_ftype!2 := s_ftype!2+1, d_ftype!2+freq
  : '2' => s_ftype!3, d_ftype!3 := s_ftype!3+1, d_ftype!3+freq
  : '4' => s_ftype!4, d_ftype!4 := s_ftype!4+1, d_ftype!4+freq
  : 'R' => s_ftype!5, d_ftype!5 := s_ftype!5+1, d_ftype!5+freq
           a  := pc+1 + gsb(pc+1);
           TEST a>=pc THEN updis(s_rfdis, d_rfdis, a-pc)
                      ELSE updis(s_rbdis, d_rbdis, pc-a)
  : 'I' => s_ftype!6, d_ftype!6 := s_ftype!6+1, d_ftype!6+freq
           a := gb(pc+1)
           updis(s_idis, d_idis, 255-a)
           a := pc+1 + 2*a & #xFFFFFFFE
           a  := a + gsh(a); 
           TEST a>=pc THEN updis(s_rfdis, d_rfdis, a-pc)
                      ELSE updis(s_rbdis, d_rbdis, pc-a)
  : 'B' => s_ftype!7, d_ftype!7 := s_ftype!7+1, d_ftype!7+freq
  : 'L' => s_ftype!8, d_ftype!8 := s_ftype!8+1, d_ftype!8+freq
  :  ?  => s_ftype!0, d_ftype!0 := s_ftype!0+1, d_ftype!0+freq
  .

 MATCH fcode 
 : 0..1|
   2        => //   BRK
               RETURN
 : 3..11    => //    K3 .. K11
               call fcode; RETURN
 : 12..13   => //    LF .. LF$
               RETURN
 : 14       => //    LM
               numb(-gb(pc+1)); RETURN
 : 15..26   => //   LM1, L0 .. L10
               numb(fcode-16); RETURN
 : 27       => //  FHOP
               RETURN
 : 28..29   => //   JEQ, JEQ$
               cjump(); RETURN
 : 30..31   => //  JEQ0, JEQ0$
               cjump0(); RETURN
               RETURN
 : 32       => //     K
               call(gb(pc+1)); RETURN
 : 33       => //    KH
               call(gh(pc+1)); RETURN
 : 34       => //    KW
               call(gw(pc+1)); RETURN
 : 35..43   => //   K3G .. K11G
               grd(); call(fcode-32); RETURN
 : 44       => //   S0G
               grd(); vecwr(); RETURN
 : 45       => //   L0G
               grd(); vecrd(); RETURN
 : 46..47   => //   L1G, L2G
               grd(); add(); numb(fcode-45); vecrd(); RETURN
 : 48       => //    LG
               grd(); RETURN
 : 49       => //    SG
               gwr(); RETURN
 : 50       => //   LLG
               RETURN
 : 51       => //    AG
               grd(); add(); RETURN
 : 52..55   => //   MUL, DIV, MOD, XOR
               eop(); RETURN
 : 56..59   => //    SL, SL$, LL, LL$
               RETURN
 : 60..61   => //   JNE, JNE$
               cjump(); RETURN
 : 62..63   => //  JNE0, JNE0$
               cjump0(); RETURN
 : 64..66   => //   LLP, LLPH, LLPW
               RETURN
 : 67..75   => //  K3G1 .. K11G1
               grd(); call(fcode-64); RETURN
 : 76       => //  S0G1
               grd(); vecwr(); RETURN
 : 77       => //  L0G1
               grd(); vecrd(); RETURN
 : 78..79   => //  L1G1, L2G1
               grd(); vecrd(); add(); numb(fcode-77); RETURN
 : 80       => //   LG1
               grd(); RETURN
 : 81       => //   SG1
               gwr(); RETURN
 : 82       => //  LLG1
               RETURN
 : 83       => //   AG1
               grd(); add(); RETURN
 : 84       => //   ADD
               add(); RETURN
 : 85       => //   SUB
               sub(); RETURN
 : 86..89   => //   LSH, RSH, AND, OR
               eop(); RETURN
 : 90..91   => //   LLL, LLL$
               RETURN
 : 92..93   => //   JLS, JLS$
               cjump(); RETURN
 : 94..95   => //  JLS0, JLS0$
               cjump0(); RETURN
 : 96       => //     L
               numb(gb(pc+1)); RETURN
 : 97       => //    LH
               numb(gh(pc+1)); RETURN
 : 98       => //    LW
               numb(gw(pc+1)); RETURN
 : 99..107  => //  K3GH .. K11GH
               grd(); call(fcode-96); RETURN
 : 108      => //  S0GH
               grd(); vecwr(); RETURN
 : 109      => //  L0GH
               grd(); vecrd(); RETURN
 : 110..111 => //  L1GH, L2GH
               grd(); numb(fcode-109); add(); vecrd(); RETURN
 : 112      => //   LGH
               grd(); RETURN
 : 113      => //   SGH
               gwr(); RETURN
 : 114      => //  LLGH
               RETURN
 : 115      => //   AGH
               grd(); add(); RETURN
 : 116      => //    RV
               vecrd(); RETURN
 : 117..122 => //   RV1 .. RV6
               numb(fcode-116); add(); vecrd(); RETURN
 : 123      => //   RTN
               s_rtn, d_rtn := s_rtn+1, d_rtn+freq
               RETURN
 : 124..125 => //   JGR, JGR$
               cjump(); RETURN
 : 126..127 => //  JGR0, JGR0$
               cjump0(); RETURN
 : 128      => //    LP
               locrd(gb(pc+1)); RETURN
 : 129      => //   LPH
               locrd(gh(pc+1)); RETURN
 : 130      => //   LPW
               locrd(gw(pc+1)); RETURN
 : 131..144 => //   LP3 .. LP16
               locrd(fcode-128); RETURN
 : 145      => //   SYS
               RETURN
 : 146      => //   LVIND
               add(); RETURN
 : 147      => //   STB
               s_pbyt, d_pbyt := s_pbyt+1, d_pbyt+freq
               RETURN
 : 148      => //    ST
               vecwr(); RETURN
 : 149..151 => //   ST1 .. ST3
               numb(fcode-148); add(); vecwr(); RETURN
 : 152..154 => //  STP3 .. STP5
               locrd(fcode-149); add(); vecwr(); RETURN
 : 155      => //     -
               RETURN
 : 156..157 => //   JLE, JLE$
               cjump(); RETURN
 : 158..159 => //  JLE0, JLE0$
               cjump0(); RETURN
 : 160      => //    SP
               locwr(gb(pc+1)); RETURN
 : 161      => //   SPH
               locwr(gh(pc+1)); RETURN
 : 162      => //   SPW
               locwr(gw(pc+1)); RETURN
 : 163..176 => //   SP3 .. SP16
               locwr(fcode-160); RETURN
 : 177..180 => //    S1 .. S4
               numb(fcode-176); sub(); RETURN
 : 181      => //   XCH
               RETURN
 : 182      => //  INDB
               s_gbyt, d_gbyt := s_gbyt+1, d_gbyt+freq
               RETURN
 : 183      => //  INDB0
               s_gbyt, d_gbyt := s_gbyt+1, d_gbyt+freq
               RETURN
 : 184..185 => //   ATC, ATB
 : 186..187 => //     J, J$
               RETURN
 : 188..189 => //   JGE, JGE$
               cjump(); RETURN
 : 190..191 => //  JGE0, JGE0$
               cjump0(); RETURN
 : 192      => //    AP
               locrd(gb(pc+1)); add(); RETURN
 : 193      => //   APH
               locrd(gh(pc+1)); add(); RETURN
 : 194      => //   APW
               locrd(gw(pc+1)); add(); RETURN
 : 195..204 => //   AP3 .. AP12
               locrd(fcode-192); add(); RETURN
 : 205      => //  INDW
               vecrd(); RETURN
 : 206      => //   LMH
               numb(-gh(pc+1)); RETURN
 : 207      => //   BTC
               RETURN
 : 208      => //   NOP
               RETURN
 : 209..213 => //    A1 .. A5
               numb(fcode-208); add(); RETURN
 : 214..218 => //  RVP3 .. RVP7
               locrd(fcode-211); add(); vecrd(); RETURN
 : 219..220 => // ST0P3, ST0P4
               locrd(fcode-216); vecwr(); RETURN
 : 221..222 => // ST1P3, ST1P4
               locrd(fcode-218); numb(1); add(); vecwr(); RETURN
 : 223      => //   CTA
               RETURN
 : 224      => //     A
               numb(gb(pc+1)); add(); RETURN
 : 225      => //    AH
               numb(gh(pc+1)); add(); RETURN
 : 226      => //    AW
               numb(gw(pc+1)); add(); RETURN
 : 227..236 => //  L0P3 .. L0P12
               locrd(fcode-224); vecrd(); RETURN
 : 237      => //     S
               numb(gb(pc+1)); sub(); RETURN
 : 238      => //    SH
               numb(gh(pc+1)); sub(); RETURN
 : 239..240 => //  MDIV, CHGCO
               RETURN
 : 241..242 => //   NEG, NOT
               eop(); RETURN
 : 243      |  //  INC1B
   244      |  //  INC4B
   245      |  //  DEC1B
   246      |  //  DEC4B
   247      |  //  INC1A
   248      |  //  INC4A
   249      | //  DEC1A
   250      => //  DEC4A
               s_incdec, d_incdec := s_incdec+1, d_incdec+freq
               RETURN
 : 251      => //      -
               RETURN
 : 252..253 => //  HAND, HAND$
               RETURN
 : 254      => //   UNH
               RETURN
 : 255      => // RAISE
               RETURN
 : code     => // DEFAULT:
               writef("Unknown function op code %d\n", code)
               RETURN
 .
.
// Number
FUN numb : n => TEST n<0 THEN updis(s_lmdis, d_lmdis, -n)
                         ELSE updis(s_lndis, d_lndis, n)
.
// Local read
FUN locrd : n => updis(s_lpdis, d_lpdis, n)
.
// Local write
FUN locwr : n => updis(s_spdis, d_spdis, n)
.
// Global read
FUN grd : => s_grds, d_grds := s_grds+1, d_grds+freq
.
//Global write
FUN gwr : => s_gwrs, d_gwrs := s_gwrs+1, d_gwrs+freq
.
// Indirect read
FUN vecrd : => s_gvecap, d_gvecap := s_gvecap+1, d_gvecap+freq
.
// Indirect write
FUN vecwr : => s_pvecap, d_pvecap := s_pvecap+1, d_pvecap+freq
.
// Add
FUN add : => s_adds, d_adds := s_adds+1, d_adds+freq
.
// Subtract
FUN sub : => s_subs, d_subs := s_subs+1, d_subs+freq
.
// Other expression operators
FUN eop : => s_eops, d_eops := s_eops+1, d_eops+freq
.
// Conditional jump (not on zero)
FUN cjump : => s_cj, d_cj := s_cj+1, d_cj+freq
.
// Conditional jump on zero
FUN cjump0 : => s_cj0, d_cj0 := s_cj0+1, d_cj0+freq
.
// Function call
FUN call : k => updis(s_kdis, d_kdis, k)
.
// Accumulate offset (or value) size statistics
FUN updis : s_v, d_v, val =>
{ s_v!Disupb, d_v!Disupb := s_v!Disupb+1, d_v!Disupb+freq
   IF val<0 RETURN
   IF val<=16 DO  { s_v!val, d_v!val := s_v!val+1, d_v!val+freq
                     RETURN
                  }
   IF val<32 DO   { s_v!17, d_v!17 := s_v!17+1, d_v!17+freq
                   RETURN
                  }
   IF val<64 DO   { s_v!18, d_v!18 := s_v!18+1, d_v!18+freq
                     RETURN
                  }
   IF val<128 DO  { s_v!19, d_v!19 := s_v!19+1, d_v!19+freq
                     RETURN
                  }
   IF val<256 DO  { s_v!20, d_v!20 := s_v!20+1, d_v!20+freq
                     RETURN
                  }
   IF val<512 DO  { s_v!21, d_v!21 := s_v!21+1, d_v!21+freq
                     RETURN
                  }
   IF val<1024 DO { s_v!22, d_v!22 := s_v!22+1, d_v!22+freq
                     RETURN
                  }
   IF val<2048 DO { s_v!23, d_v!23 := s_v!23+1, d_v!23+freq
                     RETURN
                  }
   IF val<4096 DO { s_v!24, d_v!24 := s_v!24+1, d_v!24+freq
                     RETURN
                  }
   s_v!25, d_v!25 := s_v!25+1, d_v!25+freq
}
.
FUN wrdisrange : i =>
  IF i<=16 DO { writef("%d", i);    RETURN }
  IF i=17  DO { writes "17-31";     RETURN }
  IF i=18  DO { writes "32-63";     RETURN }
  IF i=19  DO { writes "64-127";    RETURN }
  IF i=20  DO { writes "128-255";   RETURN }
  IF i=21  DO { writes "256-511";   RETURN }
  IF i=22  DO { writes "512-1023";  RETURN }
  IF i=23  DO { writes "1024-2047"; RETURN }
  IF i=24  DO { writes "2048-4095"; RETURN }
  IF i=25  DO { writes "4096-";     RETURN }
  writes "Total"
.
