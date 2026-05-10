// This is a reconstruction of bcplccg.b for the BBC BCPL System.
// When compiled by bbcbcpl using the 32-bit BCPL system it
// should generate an object file identical to bbccin/BCPLCCG.

// Reconstructed by Martin Richards (c) Mar 2017

// This reconstruction was made with the aid of oldcom/oldcincg.b
// and map/BCPLCCG.map created by: c df BCPLCCG.

// Comments such as // 1187: give byte addresses of positions
// in the object code. They refer to BCPL statements immediately
// below the comment.



SECTION "CCG1"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"
GET "com16/bcpl.h"

LET START() BE
{ // 16.
  LET workspace1 = MAXVEC()   // p3   -- NOT USED
  // 19.
  oldoutput := OUTPUT()
  // 23.
  WRITES("RCP CINTCODE generation*n")
  // 27.
  debug := 0  //+1 //+1 //+1
  // 30.
  ocodeinstream := FINDINPUT("OCODE$$")
  // 36.
  UNLESS ocodeinstream DO
  { //38.
    rc := RESULT2
    // 42.
    cgerror("CANNOT OPEN OCODE")
    //46.
    STOP(rc)
  }
  // 50.
  SELECTINPUT(ocodeinstream)
  // 54.
  progsize := 0
  //57.
  op := rdn()
  // 61.
//sawritef("  61: START: calling cgsect(%n, %n)*n", cgworkspace, cgworksize)
//abort(61)
  cgsects(cgworkspace, cgworksize)
  // 68.
  WRITEF("CINTCODE size = %N words*n", progsize/2)
  // 77.
//sawritef("  77: START: returning*n")
}

AND cgsects(workvec, vecsize) BE UNTIL // 156.
                                       op=0 DO
{ // 158.
  LET p = workvec        // p5
//sawritef("cgsect: workvec=%n vecsize=%n*n", workvec, vecsize)
  // 160.
  tempv := p
  // 163.
  p := p+90
  // 166.
  tempt := p
  // 168.
  dp := workvec+vecsize
  // 172.
  labv := p
  // 175.
  labnumber := 300
  // 180.
  p := p+labnumber
  // 184.
  labv!0 := -1
  // 187.
  MOVE(labv, labv+1, labnumber-1) // Set all element of labv to -1
  // 199:
  stv := p
  // 202:
  stvp := 0
  // 205.
  dpblklist := 0
  // 207.
  initdatalists() // This initialises reflist, rlist, nlist glist
                  // and sets freelist, dpblk and dp to zero

  // 209.
  incode := FALSE
  // 212.
  maxgn := 0
  // 214.
  maxlab := 0
  // 216.
  maxssp := 0
  // 218.
  procdepth := 0
  // 220.
  initstack(3)
  // 223.
  forgetall()
  // 225.

  UNLESS op=s.section | op=s.needs DO   // -> 238  NOT commented out for BBC Compiler
  { //235.
//abort(235)
    // Fudge for sections CCG6A and CCG7
    //UNLESS progsize=5620 | progsize=6222 DO
      codew(0)  // For size of module.
  }

  // Cause CCG8 to be compiled with naming=TRUE
  //naming := progsize=6612 -> TRUE, FALSE

  // 238.     -> 357
  WHILE op=s.section | op=s.needs DO  // Condition tested at 357
  { // 240.
    LET n = rdn()         // p6
    // 243.
    LET v = VEC 3         // p7
    // 246.
    v%0 := 7
    // 252.
    FOR i = 1 TO n DO  { // 258.
                         LET c = rdn()     // p14
                         // 263.
                         IF i<=7 DO v%i := c
                         // 273.
                       }

    // 280.
    FOR i = n+1 TO 7 DO v%i := 32  //ASCII space.
//sawritef(" 280: OCODE op=%i3 %s*n", op, op2str(op))
    // 299.
    TEST op=s.section
    THEN { // 305.
           codew(sectword)  // FDDF
           // 310,
           //J 317
         }
    ELSE { // 312.
           codew(needsword)  // FEED
         }

//FOR i = 0 TO 7 DO sawritef("section name: %c*n", v%i)
//abort(317)
    // 317.
    FOR i = 0 TO 6 BY 2 DO codew(CAPCH(v%i) | CAPCH(v%(i+1))<<8)
    // 353.
    op := rdn()

    // 357.     if op=s.needs   J 240 condition test for while loop as 238
    // 363.     if op=s.section J 240
  }

//sawritef(" 367: cgsect: calling scan*n")
  // 367.
  scan()
  // 369.
  op := rdn()
  // 373.
  outputsection(op=0)
  // 379.
  progsize := progsize + stvp
  { // 385.
    LET p = dpblklist           // p6
    // 388.
    WHILE p DO  // -> 397
    { // 390.
      LET t = p
      // 392.
      p := !p
      // 395.
      FREEVEC(t)
      // 397.  if p~=0 J 390
    }
  }
  // 400.         repeat while op ~= 0
  // 404.    if op ~= 0 J 158
}

.

SECTION "CCG1A"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

// Read an OCODE operator or argument.

LET rdn() = VALOF
{ // 438:
  LET n, res = RDBIN(), ?
  // 441:
  IF (n & 128) = 0 RESULTIS n 
  // 449:
  res := n & 127

  { // 454:
    n := RDBIN()
    // 457;
    res := (res << 7) + (n & 127)
    // 467:
    IF n+1=0 RESULTIS 0  // Test for endstreamch
    // 474:
  } REPEATWHILE (n & 128) ~= 0
  // 480:
  RESULTIS res
}

// Read in an OCODE label.
AND rdl() = VALOF
{ // 482:
  LET l = rdn()
  // 485:
  IF maxlab<l DO
  { // 489:
    maxlab := l
    // 492:
    checklab()
  }
  // 494:
  RESULTIS l
}

// Read in a global number.
AND rdgn() = VALOF
{ // 496:
  LET g = rdn()
  // 499:
  IF maxgn<g DO maxgn := g
  // 506:
  RESULTIS g
}

// Generate next label number.
AND newlab() = VALOF
{ // 508:
  labnumber := labnumber-1
  // 513:
  checklab()
  // 517:
  RESULTIS labnumber
}

AND checklab() BE IF // 518.
                     maxlab>=labnumber DO
{ // 524:
  cgerror("TOO MANY LABELS")
  // 528
  STOP(40)
}


AND cgerror(mes, a) BE
{ // 550.
  WRITES("*nERROR: ")
  // 552.
  WRITEF(mes, a)
  // 559.
  NEWLINE()
  // 561.
  RETURN
}

// Initialize the simulated stack (SS).
LET initstack(n) BE
{ // 572.
  arg2, arg1, ssp := tempv, tempv+3, n
  // 582.
  pendingop := s.none
  // 586.
  h1!arg2, h2!arg2, h3!arg2 := k.loc, ssp-2, ssp-2
  // 601.
  h1!arg1, h2!arg1, h3!arg1 := k.loc, ssp-1, ssp-1
  // 616.
  IF maxssp<ssp DO maxssp := ssp
  // 624.
}

// Move simulated stack (SS) pointer to N.
AND stack(n) BE
{ // 626.
  IF maxssp<n DO maxssp := n
  // 633.
  IF n>=ssp+4 DO { // 639.
                   store(0,ssp-1)
                   // 646.
                   initstack(n)
                   // 649.
                   RETURN
                 }

  WHILE // 650.
        n>ssp DO // 652.
                 loadt(k.loc, ssp)

  UNTIL // 658.
        n=ssp DO
  { IF // 665.
       arg2=tempv DO  // -> 715
    { TEST // 671.
           n=ssp-1    // -> 711
           // 
      THEN { // 677.
             ssp := n
             // 679.
             h1!arg1, h2!arg1, h3!arg1 := h1!arg2, h2!arg2, ssp-1
             // 694.
             h1!arg2, h2!arg2, h3!arg2 := k.loc, ssp-2, ssp-2
             // 709.
           }
      ELSE // 711.
           initstack(n)
      // 714.
      RETURN
    }
    // 715.
    arg1, arg2, ssp := arg1-3, arg2-3, ssp-1
    // 730.  if ssp~=n  j 665
  }
}

// store all ss items from s1 to s2 in their true
// locations on the stack.
// it may corrupt both registers a and b.
AND store(s1, s2) BE // 736:
                     FOR p = tempv TO arg1 BY 3 DO
                     { // 744:
                       LET s = h3!p
                       // 746:
                       IF s>s2 RETURN
                       // 751:
                       IF s>=s1 DO storet(p)
                       // 758.    p := p+3
                       // 761.    if p<=limit j 744
                     }

.

SECTION "CCG2"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

LET scan() BE
{ // 822:
  IF debug>1 DO { // 827.
                  dboutput()
                }
//IF 100<=stvp<=700 DO
//{ 
//  dboutput()
//  sawritef("%i5: scan: OCODE op %i3 %s*n", stvp, op, op2str(op))
//  abort(829)
//}
  // 829.
  SWITCHON op INTO  // -> 1326

  { DEFAULT:      // 831.
                  cgerror("BAD OP %N", op)
                  // 838.
                  ENDCASE

    CASE 0:       // 840.
                  RETURN
      
    CASE s.debug: // 841:
                  debug := (debug+1) REM 3
                  ENDCASE

      CASE s.lp:   // 850:
                   loadt(k.loc,   rdn());   ENDCASE
      CASE s.lg:   // 858:
                   loadt(k.glob,  rdgn());  ENDCASE
      CASE s.ll:   // 866:
                   loadt(k.lab,   rdl());   ENDCASE
      CASE s.ln:   // 874:
                   loadt(k.numb,  rdn());   ENDCASE

      CASE s.lstr: // 882:
                   cgstring(rdn());         ENDCASE

      CASE s.true: // 888:
                   loadt(k.numb, -1);       ENDCASE
      CASE s.false:// 895:
                   loadt(k.numb,  0);       ENDCASE

      CASE s.llp:  // 902:
                   loadt(k.lvloc,  rdn());  ENDCASE
      CASE s.llg:  // 910:
                   loadt(k.lvglob, rdgn()); ENDCASE
      CASE s.lf:
      CASE s.lll:  // 918:
                   loadt(k.lvlab,  rdl());  ENDCASE

      CASE s.sp:   // 927:
                   storein(k.loc,  rdn());  ENDCASE
      CASE s.sg:   // 935:
                   storein(k.glob, rdgn()); ENDCASE
      CASE s.sl:   // 943:
                   storein(k.lab,  rdl());  ENDCASE

      CASE s.stind:// 951:
                   cgstind(); ENDCASE

      CASE s.rv:   // 955:
                   cgrv(); ENDCASE

      CASE s.mult:CASE s.div:CASE s.rem:
      CASE s.plus:CASE s.minus:
      CASE s.eq:CASE s.ne:
      CASE s.ls:CASE s.gr:CASE s.le:CASE s.ge:
      CASE s.lshift:CASE s.rshift:
      CASE s.logand:CASE s.logor:CASE s.eqv:CASE s.neqv:
      CASE s.not:CASE s.neg:CASE s.abs:
                   // 959:
                   cgpendingop()
                   // 961.
                   pendingop := op
                   // 965.
                   ENDCASE

      CASE s.endfor:
                   // 967:
                   cgpendingop()
                   // 969.
                   pendingop := s.le
                   // 973.

      CASE s.jt:   // 973:
                   cgjump(TRUE, rdl());  ENDCASE

      CASE s.jf:   // 981:
                   cgjump(FALSE, rdl()); ENDCASE

      CASE s.goto: // 989:
                   cgpendingop()
                   // 991:
                   store(0, ssp-2)
                   //998:
                   loada(arg1)
                   // 1002:
                   gen(f.goto)
                   // 1006:
                   stack(ssp-1)
                   // 1011:
                   incode := FALSE
                   // this is a good place to deal with
                   // some outstanding forward refs.
                   // 1014.
                   chkrefs(50)
                   // 1018.
                   ENDCASE

      CASE s.xlab:                 // set label on even address
      CASE s.lab:  // 1020:
                   cgpendingop()
                   // 1022:
                   UNLESS incode DO chkrefs(30)
                   // 1030:
                   store(0, ssp-1)
                   // 1037:
                   IF op=s.xlab DO aligneven()
                   // 1045:
                   setlab(rdl())
                   // 1049:
                   forgetall()
                   // 1051.
                   incode := procdepth>0
                   // 1059.
                   ENDCASE

      CASE s.query:// 1061.
                   loadt(k.loc, ssp)
                   // 1067.
                   ENDCASE

      CASE s.stack:// 1069.
                   cgpendingop()
                   // 1071.
                   stack(rdn())
                   // 1075.
                   ENDCASE

      CASE s.store:// 1077.
                   cgpendingop()
                   // 1079.
                   store(0, ssp-1)
                   // 1086.
                   ENDCASE

      CASE s.entry:
                { // 1088.
                  LET n = rdn()    // p3
                  // 1091.
                  LET lab = rdl()  // p4
                  // 1094.
                  cgentry(n, lab)
                  // 1098.
                  procdepth := procdepth + 1
                  // 1103.
                  ENDCASE
                }

      CASE s.save:
                { // 1105.
                  cgsave(rdn())
                  // 1109.
                  ENDCASE
                }

      CASE s.fnap:
      CASE s.rtap: // 1111.
                   cgapply(op, rdn())
                   // 1118.
                   ENDCASE

      CASE s.rtrn:                   
      CASE s.fnrn: // 1120.
                   cgreturn(op)
                   // 1124.
                   ENDCASE

      CASE s.endproc:
                 { // 1126.
                   LET n = rdn()
                   // 1129.
                   cgstatics(n)   // The argument is ignored
                   // 1131.
                   procdepth := procdepth - 1
                   // 1136.
                   ENDCASE
                 }

      CASE s.res:
      CASE s.jump:
                { // 1138.
                  LET lab = rdl()   // p3
                  // 1141.
                  cgpendingop()
                  // 1143.
                  store(0, ssp-2)
                  // 1150.
                  TEST op=s.jump
                  THEN { // 1156.    -> 1162
                         storet(arg1)
                         // 1160.   -> 1171
                       }
                  ELSE { // 1162.
                         loada(arg1)
                         // 1166.
                         stack(ssp-1)
                       }

                  { // 1171.
                    op := rdn()
                    // 1175.
                    UNLESS op=s.stack BREAK  // -> 1187
                    // 1181.
                    stack(rdn())
                    // 1185.     -> 1171
                  } REPEAT

                  // 1187.
                  TEST op=s.lab       // -> 1223
                  THEN { // 1193.
                         LET m = rdl()     // p4
                         // 1196.
                         UNLESS lab=m DO genr(f.j, lab)
                         // 1204
                         setlab(m)
                         // 1207.
                         forgetall()
                         // 1209.
                         incode := procdepth>0
                         // 1217.   
                         op := rdn()
                          // 1221.      -> 1236
                       }
                  ELSE { // 1223.
                         genr(f.j, lab)
                         // 1229.
                         incode := FALSE
                         // deal with some refs.
                         // 1232.
                         chkrefs(50)
                       }
                  // 1236.
                  LOOP         // -> 822
                }

      // rstack always occurs immediately after a lab statement
      // at a time when cgpendingop() and store(0, ssp-2) have
      // been called.

      CASE s.rstack: // 1238:
                     initstack(rdn())
                     // 1242:
                     loadt(k.a, 0)
                     // 1248.
                     ENDCASE

      CASE s.finish:  // compile code for:  stop(0).
         { // 1250:
           LET k = ssp    // p3
           // 1253:
           stack(ssp+3)
           // 1256:
           loadt(k.numb, 0)
           // 1261:
           loadt(k.glob, gn.stop)
           // 1266:
           cgapply(s.rtap, k)    // simulate the call: stop(0, 0)
           // 1272.
           ENDCASE
         }

      CASE s.switchon:
                     { // 1274.
                       LET upb = 2*rdn() + 1   // p3
                       // 1280.
                       LET v = getvec(upb)
                       // 1283.
                       UNLESS v DO    //  -> 1293
                       { //1285.
                         cgerror("NO ROOM FOR SWITCH")
                         // 1289.
                         stop(40)
                       }
                       // 1293.
                       cgswitch(v, upb)
                       // 1298.
                       freevec(v)
                       // 1301.
                       ENDCASE
                     }

      CASE s.getbyte:  
      CASE s.putbyte:  // 1303.
                       cgbyteop(op)
                       // 1307.
                       ENDCASE

      CASE s.global:   // 1309.
                       cgglobal(rdn())
                       // 1313.
                       RETURN

      CASE s.datalab:  // 1318.
                       cgdatalab(rdl())
                       // 1322.
                       LOOP  //    -> 822
   }
   // 1586.
   op := rdn()
   // 1590.     -> 822
} REPEAT

.

SECTION "CCG3"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

// compiles code to deal with any pending op.
LET cgpendingop() BE
{ // 1650.
  LET f = 0   // p3
  // 1652.
  LET sym = TRUE             // p4
  // 1654.
  LET pndop = pendingop      // p5
  // 1657.
  pendingop := s.none

  IF // 1661.
     isnum(arg1) &   // -> 1674
    // 1167.
    cgconstexp(pndop) DO   // -> 1674
      // 1673.
      RETURN

  // 1674.

  SWITCHON pndop INTO   //  -> 1831
  { DEFAULT:      // 1676.
                  cgerror("BAD PENDINGOP %N", pndop)

    CASE s.none:  // 1682.
                  RETURN

    CASE s.abs:   // 1683.
                  loada(arg1)
                  // 1687.
                  chkrefs(3)
                  // 1690.
                  genb(jfn0(f.jge), 127+2) // conditionally skip
                  // 1700.
                  gen(f.neg)           // over this neg instruction.
                  // 1704.
                  forget.a()
                  // 1706.
                  RETURN

    CASE s.neg:   // 1707.
                  loada(arg1)
                  // 1711.
                  gen(f.neg)
                  // 1715.
                  forget.a()
                  // 1717.
                  RETURN

    CASE s.not:   // 1718.
                  loada(arg1)
                  // 1722.
                  gen(f.not)
                  // 1726.
                  forget.a()
                  // 1728.
                  RETURN

    CASE s.eq: CASE s.ne:
    CASE s.ls: CASE s.gr:
    CASE s.le: CASE s.ge:
                  // 1729.
                  f := prepj(jmpfn(pndop))
                  // 1735
                  chkrefs(4)
                  // 1738.
                  genb(f, 127+2)    // jump to    ---
                  // 1744.          //               |
                  gen(f.fhop)       //               |
                  // 1748.          //               |
                  gen(f.lm1)        // this point  <-
                  // 1752.
                  lose1(k.a, 0)
                  // 1758.
                  forgetall()
                  // 1760.
                  RETURN

    CASE s.minus: // 1761.
                  UNLESS isnum(arg1) DO  //  -> 1774
                  { // 1767
                    f, sym := f.sub, FALSE
                    // 1772
                    ENDCASE            // -> 1922
                  }
                  // 1774.
                  h2!arg1 := -h2!arg1

    CASE s.plus:  // 1780.
                  cgplus(); RETURN

    CASE s.mult:  // 1783.
                  f      := f.mul;        ENDCASE
    CASE s.div:   // 1788.
                  f, sym := f.div, FALSE; ENDCASE
    CASE s.rem:   // 1795.
                  f, sym := f.rem, FALSE; ENDCASE
    CASE s.lshift:// 1802.
                  f, sym := f.lsh, FALSE; ENDCASE
    CASE s.rshift:// 1809.
                  f, sym := f.rsh, FALSE; ENDCASE
    CASE s.logand:// 1816
                  f      := f.and;        ENDCASE
    CASE s.logor: // 1821.
                  f      := f.or;         ENDCASE
    CASE s.eqv:
    CASE s.neqv:  // 1826.
                  f      := f.xor;        ENDCASE
    // 1831. swb and table
  }

  // 1922.
  TEST sym THEN { // 1925.
//sawritef("1922: cgpendingop: calling loadboth()*n")
//abort(1922)
                  loadboth()
                  // 1927.   -> 1931
                }
           ELSE { // 1929.
//sawritef("1929: cgpendingop: calling loadba()*n")
//abort(1929)
                  loadba()
                }
  // 1931.
  gen(f)
  // 1934.
  forget.a()
  // 1936.
  IF pndop=s.eqv DO gen(f.not)
  // 1945.
  lose1(k.a, 0)
  // 1951.
//sawritef("1951: cgpendingop: returning*n")
//abort(1951)
  RETURN
}

AND cgconstexp(op) = VALOF
{ // 1970.
  SWITCHON op INTO   // -> 2019
  { CASE s.none:     RESULTIS TRUE

    CASE s.abs:      // 1975.
                     h2!arg1 := ABS h2!arg1
                     // 1983.
                     RESULTIS TRUE    // -> 2042

    CASE s.neg:      //1986.
                     h2!arg1 := -h2!arg1
                     // 1993.
                     RESULTIS TRUE

    CASE s.not:      //1995.
                     h2!arg1 := ~h2!arg1
                     // 2002.
                     RESULTIS TRUE

    DEFAULT:         // 2004.
                     UNLESS isnum(arg2)   //  -> 2013
                       // 2010.
                       RESULTIS FALSE
                     // Both arg2 and arg1 are numbers.
                     // 2013.
                     RESULTIS cgconstdyadic(op)
    // 2019. SWB and table
  }
  // 2042.
  //RETURN
}

AND cgconstdyadic(op) = VALOF
{ // Apply op to two numbers in arg2 and arg1
  // If no applicable return FALSE
  // otherwise replace arg2 and arg1 by the value of the
  // expression and return TRUE.

  // 2044.
  LET n1 = h2!arg2   // p4
  // 2047.
  LET n2 = h2!arg1   // p5
  // 2050.
  LET res = TRUE     // p6
  // 2052.
  LET val = VALOF SWITCHON op INTO   // -> 2113   val=p7
  { DEFAULT:         // 2054.
                     res := FALSE
                     // 2056.
                     RESULTIS 0
    CASE s.minus:    // 2058.
                     RESULTIS n1 - n2
    CASE s.plus:     // 2063.
                     RESULTIS n1 + n2
    CASE s.mult:     // 2067.
                     RESULTIS n1 * n2
    CASE s.div:      // 2072.
                     RESULTIS n1 / n2
    CASE s.rem:      // 2077.
                     RESULTIS n1 REM n2
    CASE s.lshift:   // 2082.
                     RESULTIS n1 << n2
    CASE s.rshift:   // 2087.
                     RESULTIS n1 >> n2
    CASE s.logand:   // 2092.
                     RESULTIS n1 & n2
    CASE s.logor:    // 2097.
                     RESULTIS n1 | n2
    CASE s.eqv:      // 2102.
                     RESULTIS n1 EQV n2
    CASE s.neqv:     // 2108.
                     RESULTIS n1 NEQV n2
    // 2113.   SWB and table
  }
  // 2164.
  IF res DO
  { // 2168.
    lose1(k.numb, val)
  }
  // 2173.
  RESULTIS res
}

.

SECTION "CCG4"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

LET loadval(x, pushing) BE  // ONLY called from loada and push.
// Load compiles code to have the following effect:
// If pushing=TRUE    B := A; A := <x>.
// If pushing=FALSE   B := ?; A := <x>.
{ // 2208
  LET k, n = h1!x, h2!x   //  p5  p6
//sawritef("2208: loadval: entered, x=%n pushing=%n k=%n n=%n*n", x, pushing, k, n)
//IF k=k.numb & n=0 DO abort(2212)
  UNLESS // 2212:
         pushing |    //  -> 2246
         // 2215.
         h1!x=k.a DO  // -> 2246  Dump A register if necessary.
  { // 2222:
//sawritef("2222: loadval: x=%n pushing=%n k=%n n=%n*n", x, pushing, k, n)
//abort(2222)
    FOR t = arg1 TO tempv BY -3 DO   //  -> 2242
    { // 2228.
//sawritef("2228: loadval: t=%n h1!t=%n h2!t=%n, h3!t=%n*n", t, h1!t, h2!t, h3!t)
//abort(2228)
      IF h1!t=k.a DO  //  -> 2238
      { // 2233.
//sawritef("2233: loadval: calling storet(%n)*n", t)
//abort(2233)
        storet(t)
        BREAK         //  -> 2246
      }
      // 2238.   p7 := p7 - 3
      // 2242.   if lim <= p7 J 2228
    }
  }

//sawritef("2246: loadval: infok.a=%n infon.a=%n infok.b=%n infon.b=%n*n",
//                   infok.a, infon.a, infok.b, infon.b)
//abort(2246)
  IF // 2246.
     infok.a=k &     //  -> 2261
     // 2251.
     infon.a=n DO    //  -> 2261
  { // 2256.
//sawritef("2256: loadval: k=infok.a n=infon.a*n")
//abort(2256)
    h1!x := k.a
    // 2259.
    h2!x := 0
//dboutput()
  }

//sawritef("2261: loadval: switching on %n*n", h1!x)
//abort(2261)
  // 2261
  SWITCHON h1!x INTO     //  -> 2492
  { CASE k.c:
    CASE k.b:
    CASE 8:
    CASE 7:
    CASE 6:
    DEFAULT:  // 2263:
              cgerror("IN LOADA %N", k)
              // 2271.
              STOP(40)

    CASE k.a: // 2273:
//sawritef("2273: loadval: CASE k.a:*n")
//abort(2273)
              IF pushing DO    //  -> 2287
              {
//sawritef("2276: loadval: CASE k.a: pushing=TRUE*n")
//abort(2276)
                // 2276:
                UNLESS inreg.b(infok.a, infon.a) DO  //  -> 2287 THIS CODE IS WRONG
                                                     // This call of inreg.b returns FALSE
                                                     // so genatb is always called.
                //UNLESS infok.a=infok.b & infon.a=infon.b DO  // Correction
                {
//sawritef("2285: loadval: calling genatb()*n")
//abort(2285)
                  // 2285.
                  genatb()
//dboutput()
                }
              }
              // 2287.
              RETURN

     CASE k.numb:
     {
//sawritef("2288: loadval: CASE k.numb: n=%n*n", n)
//abort(2288)
       TEST // 2288.
            -1<=n<=10     //  -> 2302
       THEN // 2295.
            gen(f.l0+n)
            // 2300.     -> 2338
       ELSE // 2302.
            TEST 0<=n<=255   // ->2317
            THEN // 2309.
                 genb(f.l, n)
                 // 2315.    -> 2338
            ELSE TEST // 2317.
                      -255<=n<=0     //  -> 2332
                 THEN // 2324.
                      genb(f.lm, -n)
                      // 2330.     -> 2338
                 ELSE // 2332.
                      genw(f.lw, n)
       // 2338.
       ENDCASE      //  -> 2542
     }

     CASE k.loc:
//sawritef("2340: loadval: CASE k.loc: n=%n*n", n)
//abort(2340)
                  TEST // 2340.
                       3<=n<=16         //  -> 2355
                  THEN { // 2348.
                         gen(f.lp0+n)
                         // 2353.       -> 2376
                       }
                  ELSE TEST // 2355.
                            0<=n<=255   // -> 2370
                       THEN { // 2362.
                              genb(f.lp, n)
                              // 2368.     -> 2376
                            }
                       ELSE { // 2370.
                              genw(f.lpw, n)
                            }
                  // 2376.
                  ENDCASE     //  -> 2542

     CASE k.glob: // 2378.
//sawritef("2378: loadval: CASE k.glob: n=%n*n", n)
                  geng(f.lg, n)
                  // 2384.
                  ENDCASE

     CASE k.lab:  // 2386.
//sawritef("2386: loadval: CASE k.lab: n=%n*n", n)
                  genr(f.ll, n)
                  // 2392.
                  ENDCASE

     CASE k.lvloc:
//sawritef("2394: loadval: CASE k.lvloc: n=%n*n", n)
                  TEST // 2394.
                       0<=n<=255         // -> 2409
                  THEN { // 2401.
                         genb(f.llp, n)
                         // 2407.        -> 2415
                       }
                  ELSE { // 2409.
                         genw(f.llpw, n)
                       }
                  // 2415
                  ENDCASE   //   -> 2542

     CASE k.lvglob:
//sawritef("2394: loadval: CASE k.lvglob: n+%n*n", n)
                   // 2417.
                   geng(f.llg, n)
                   // 2423.
                   ENDCASE

     CASE k.lvlab:
//sawritef("2425: loadval: CASE k.lvlab: n+%n*n", n)
                   // 2425.
                   genr(f.lll, n)
                   // 2431.
                   ENDCASE

     CASE k.loc0:
//sawritef("2433: loadval: CASE k.loc0: n+%n*n", n)
                   // 2433.
                   gen(f.l0p0+n)
                   // 2438.
                   ENDCASE

     CASE k.loc1:
//sawritef("2440: loadval: CASE k.loc1: n+%n*n", n)
                   // 2440.
                   gen(f.l1p0+n)
                   // 2445.
                   ENDCASE

     CASE k.loc2:
//sawritef("2447: loadval: CASE k.loc2: n+%n*n", n)
                   // 2447.
                   gen(f.l2p0+n)
                   // 2452.
                   ENDCASE

     CASE k.loc3:
//sawritef("2454: loadval: CASE k.loc3: n+%n*n", n)
                   // 2454.
                   gen(f.l3p0+n)
                   // 2459.
                   ENDCASE

     CASE k.loc4:
//sawritef("2461: loadval: CASE k.loc4: n+%n*n", n)
                   // 2461.
                   gen(f.l4p0+n)
                   // 2466.
                   ENDCASE

     CASE k.glob0:
//sawritef("2468: loadval: CASE k.glob0: n+%n*n", n)
                   // 2468.
                   geng(f.l0g, n)
                   // 2474.
                   ENDCASE

     CASE k.glob1:
//sawritef("2476: loadval: CASE k.glob1: n+%n*n", n)
                   // 2476.
                   geng(f.l1g, n)
                   // 2482.
                   ENDCASE

     CASE k.glob2:
//sawritef("2484: loadval: CASE k.glob2: n+%n*n", n)
                   // 2484.
                   geng(f.l2g, n)
                   // 2490.
                   ENDCASE
  }

  // 2492.   SWL and table

  // A loading instruction has just been compiled.
  // 2542.
  setinfo.b(infok.a, infon.a)
  //2549.
  setinfo.a(h1!x, h2!x)
  // 2554.
  h1!x, h2!x := k.a, 0
//sawritef("2259: loadval: h1!x=%n h2!x=%n infok.a=%n infon.a=%n infok.b=%n infon.b=%n*n",
//                   h1!x, h2!x, infok.a, infon.a, infok.b, infon.b)
//abort(2259)
  // 2559.
  RETURN
}

AND loadba() BE
{
//sawritef("2572: loadba:*n")
  IF // 2572.
     loadboth()=swapped DO // 2577.
                           genxch()
  // 2579.
  RETURN
}

AND genxch() BE
{ // 2580
  LET k, n = infok.a, infon.a    //  p3  p4
//sawritef("2586: genxch:*n")
  // 2586.
  setinfo.a(infok.b, infon.b)
  //2593.
  setinfo.b(k, n)
  // 2598.
  gen(f.xch)
  // 2602.
  RETURN
}

AND genatb() BE
{
//sawritef("2604: genatb:*n")
  // 2604.
  gen(f.atb)
  // 2608.
  setinfo.b(infok.a, infon.a)
  // 2615.
  RETURN
}

LET loada(x)   BE
{
//sawritef("2616: loadba: x=%n->[%n,%n,%n]*n", x, h1!x, h2!x, h3!x)
 // 2616.
  loadval(x, FALSE)
  // 2621.
  RETURN
}

AND push(x) BE
{
//sawritef("2622: push: x=%n->[%n,%n,%n]*n", x, h1!x, h2!x, h3!x)
 // 2622.
  loadval(x, TRUE)
  // 2627.
  RETURN
}

AND loadboth() = VALOF
// Compiles code to cause
//   either    arg2 -> B  and  arg1 -> A
//             giving result notswapped
//   or        arg2 -> A  and  arg2 -> B
//             giving result swapped.
// loadboth only swaps if this saves code.
{ // 2628.
  LET x = arg2     // p3
  // 2631.
  LET y = arg1     // p4
//dboutput()
//sawritef("2634: loadboth: x=%n y=%n*n", x, y)
//abort(2634)

  // First ensure that no other stack item uses reg A.
  // 2634.
  FOR t = tempv TO arg2-3 BY 3 DO   // -> 2654  t=p5   limit=p6
  { // 2643.
    IF h1!t=k.a DO     //  -> 2651
      // 2648.
      storet(t)
    // 2651.     p5 := p5+3
    // 2654.     if p5 <= limit J 2643
  }
  // 2658.
  
  { // 2658.
    LET xa, ya = inreg.a(x), inreg.a(y)  // xa=p5 ya=p6
    // 2665.
    AND xb, yb = inreg.b(x), inreg.b(y)  // xb=p7 yb=p8
//sawritef("2674: loadboth: xa=%n ya=%n xb=%n yb=%n*n", xa, ya, xa, yb)
//abort(2674)

    // 2674.
    IF h1!x=k.a DO       // -> 2699
    { // 2679.
      IF yb RESULTIS swapped    // p8
      // 2685.
      IF ya DO                  // p6      -> 2693
      { // 2688.
        genatb()
        // 2690.
//sawritef("2690: loadboth: returning notswapped*n")
//abort(2690)
        RESULTIS notswapped
      }
      // 2693.
      push(y)
      // 2697.
//sawritef("697: loadboth: returning notswapped*n")
//abort(2697)
      RESULTIS notswapped
    }

    // 2699
    IF xa DO                  // p5    -> 2727
    { // 2702.
      IF yb DO
      { 
//sawritef("2702: loadboth: xa and yb both TRUE, returning swapped*n")
//abort(2702)
        RESULTIS swapped
      }
      // 2708.
      IF ya |                 // p6
         // 2711.
         h1!y=k.a DO            // -> 2721
      { // 2716.
        genatb() // x and y are both in A so copy A to B
//dboutput()
//sawritef("2716: loadboth: xb=TRUE and y in A returning notswapped*n")
//abort(2716)
        RESULTIS notswapped
      }
      // 2721.
//sawritef("2721: loadboth: calling push(y) y=%n->[%n,%n,%n]*n",
//          y, h1!y, h2!y, h3!y)
//abort(2721)
      push(y)  // x is in A so Compile A->B; y->A
      // 2724.
//sawritef("2724: loadboth: returning notswapped*n")
//abort(2724)
      RESULTIS notswapped         
    }

    // xa=p5 ya=p6 xb=p7 yb=p8
    // 2727.    
    IF xb DO                   // p7     -> 2757
    { // 2730.
      IF ya |                  // p6
         // 2733.
         h1!y=k.a DO           // -> 2741
      { // 2738
//sawritef("2738: loadboth: xb=TRUE and (ya=TRUE or y is in A) returning notswapped*n")
//abort(2738)
        RESULTIS notswapped
      }

      // 2741.
      genxch()        // Copy B into A
      // 2743.
      IF yb DO        // p8      -> 2751
      { // 2746.
        genatb()  // x and y are both in A so compile A->B
        // 2748.
//sawritef("2748: loadboth: returning notswapped*n")
//abort(2748)
        RESULTIS notswapped
      }
      // 2751.
      push(y)   // x is in A so compile A->B; y->A
      // 2754.
//sawritef("2754: loadboth: returning notswapped*n")
//abort(2754)
      RESULTIS notswapped
    }

    // 2757.
    IF ya |       // p6
      // 2760.
      h1!y=k.a DO
    { // 2765.
      push(x)  // y is in A so compile A->B; x->A
      // 2768.
//sawritef("2768: loadboth: y was in A returning swapped*n")
//abort(2768)
      RESULTIS swapped
    }
    // 2771.      J 2778
    // Two resolving words

    // 2778.
    IF yb DO                   // p8    -> 2816
    { // 2781.
      LET yk = h1!y     // p9
      // 2783.
      LET yn = h2!y     // p10
      // 2785.
//sawritef("2785: loadboth: yk=%n yn=%n*n", yk, yn)
//abort(2785)
      UNLESS yk=k.loc &
             // 2789.
             3<=yn<=16 DO           // -> 2816
      { // 2797.
        UNLESS yk=k.numb &
               // 2801.
               -1<=yn<=10 DO
        { // 2808.
          genxch()  //
          // 2810.
//sawritef("2810: loadboth: yk=%n yn=%n calling push()*n", yk, yn)
//abort(2810)
          push(x)
          // 2813.
//sawritef("2813: loadboth: returning swapped*n")
//abort(2813)
          RESULTIS swapped
        }
      }
    }
    // 2816.
//sawritef("2816: loadboth: calling loada(%n)*n", x)
//abort(2816)
    loada(x)
    // 2819.
//sawritef("3819: loadboth: calling push(%n)*n", y)
//abort(2819)
    push(y)
    // 2822.
//sawritef("2822: loadboth: returning notswapped*n")
//abort(2822)
    RESULTIS notswapped
  }
}

AND inreg.a(x) = VALOF
{ // 2824:
  RESULTIS h1!x=infok.a & h2!x=infon.a -> TRUE, FALSE
//sawritef("2824: inreg.a(%n): h1!x=%n h2!x=%n infok.a=%n infon.a=%n res=%n*n",
//          x, h1!x, h2!x, infok.a, infon.a, res)
//  RESULTIS res
}

AND inreg.b(x) = VALOF
{ // 2842:
  RESULTIS h1!x=infok.b & h2!x=infon.b -> TRUE, FALSE
//sawritef("2842: inreg.b(%n): h1!x=%n h2!x=%n infok.b=%n infon.b=%n res=%n*n",
//          x, h1!x, h2!x, infok.b, infon.b, res)
//  RESULTIS res
}

AND setinfo.a(k, n) BE
{ // 2858:
  infok.a, infon.a := k, n
//sawritef("2858: setinfo.a: infok.a=%n infon.a=%n*n", infok.a, infon.a)
//dboutput()
}

AND setinfo.b(k, n) BE
{ // 2864:
  infok.b, infon.b := k, n
//sawritef("2964: setinfo.b: infok.b=%n infon.b=%n*n", infok.b, infon.b)
//dboutput()
}

AND addinfo.a(k, n) BE
{ // 2870.
  IF infok.a=k.none DO                  // -> 2880
  { // 2873.
    setinfo.a(k, n)
  }
 // 2878.
}

AND forget.a() BE
{ // 2882.
//sawritef("2882: forget.a:*n")
  setinfo.a(k.none, 0)
  // 2887.
}

AND forget.b() BE
{ // 2888.
//sawritef("2888: forget.b:*n")
  setinfo.b(k.none, 0)
  // 2893.
}

AND forgetall() BE
{ // 2894.
//sawritef("2894: forgetall:*n")
  forget.a()
  // 2896
  forget.b()
  // 2898.
}

// Forgetvar is called just after a simple variable (k, n) has been
// updated.  k is k.loc, k.glob or k.lab.  Note that register
// information about indirect local and global values
// must also be thrown away.
AND forgetvar(k, n) BE
{ // 2900.
//sawritef("2900: forgetvar: k=%n n=%n*n", k,n)

  IF mustforget(k, n, infok.a, infon.a) DO  // -> 2915
  { // 2913.
    forget.a()
  }
  // 2915.
  IF mustforget(k, n, infok.b, infon.b) DO
  { // 2928.
    forget.b()
  }
  // 2930.
}

AND forgetallvars() BE  // Called after STIND or PUTBYTE.
{ // If A is known to hold the value of a local, global or static
  // or indirectly refers to a location addressed via a local or
  // global, the information is infoa.k and infon.a must be cleared.
  // IE if infok.a is any of k.loc, k.glob, k.lab, k.loc0 to k.loc4,
  // or k.glob0 to k.glob2, call forget.a.
  // Similarly conditionally call forget.b.
//dboutput() 
//sawritef("%i5: forgetallvars: infok.a=%n infon.a=%n infok.b=%n infon.b=%n*n", stvp,
//          infok.a, infon.a, infok.b, infon.b)
//abort(2932)
  IF // 2932.
     infok.a=k.loc |  // -> 2949
     // 2937.
     infok.a=k.glob | // -> 2949
     // 2940.
     infok.a=k.lab |  // -> 2949
     // 2944.
     infok.a>=k.loc0 DO  // -> 2951   loc0 - loc4  glob0 - glob2
  { // 2949.
    forget.a()
//dboutput() 
//sawritef("%i5: forgetallvars: after forget.a infok.a=%n infon.a=%n infok.b=%n infon.b=%n*n", stvp,
//          infok.a, infon.a, infok.b, infon.b)
    // 2951.
  }

  IF // 2951.
     infok.b=k.loc |    // -> 2968
     // 2956.
     infok.b=k.glob |    // -> 2968
     // 2959.
     infok.b=k.lab |    // -> 2968
     // 2963.
     infok.b>=k.loc0 DO    // -> 2970
  { // 2968.
    forget.b()
//dboutput() 
//sawritef("%i5: forgetallvars: after forget.b infok.a=%n infon.a=%n infok.b=%n infon.b=%n*n", stvp,
//          infok.a, infon.a, infok.b, infon.b)
    // 2970.
  }
//dboutput() 
//sawritef("%i5: forgetallvars: returning*n", stvp)
//abort(2970)
}

AND mustforget(k,n, infok,infon) = VALOF
{ // This is only called from forgetvar indicating that the
  // local or global variable specified by (k,n) has just been
  // updated. This means that information about the current
  // value held in A or B may now be invalid. (infok,infon)
  // holds the current information about either A or B.
  // The resultis TRUE if forget.a or forget.b must be called.

  // 2972.
  IF n=infon DO            // -> 3008     res false if n ~= infon
  { // 2976.
    IF k=infok |           // -> 3005     res true if n=infon and k=infok
       ( // 2980.
         k=k.loc &         // -> 2992
         // 2983.
         k.loc0<=infok<=k.loc4 ) |      // -> 2992
       ( // 2992.
         k=k.glob &     // -> 3008
         // 2996.
         k.glob0<=infok<=k.glob2) DO  // -> 3008   res false
    { // 3005.
      RESULTIS TRUE  // resultis is true if n=infon and
                     // either k=infok
                     // or     k=k.loc  and infok id k.loc0  to k.loc4
                     // or     k=k.glob and infok id k.glob0 to k.glob2
    }
  }
  // 3008.
  RESULTIS FALSE
}

.

SECTION "CCG4A"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

LET isnum(x) =
  // 3104:
  h1!x = k.numb

AND iszero(a) = 
  // 3112:
  h1!a=k.numb & h2!a=0 -> TRUE, FALSE

// Store the value of a SS item in its true stack location.
AND storet(x) BE
{ // 3124: 
  LET s = h3!x
  IF h1!x=k.loc & h2!x=s RETURN
  // 3135:
  loada(x)
  // 3138:
  gensp(s)
  //3141:
  forgetvar(k.loc, s)
  // 3146:
  addinfo.a(k.loc, s)
  // 3151:
  h1!x, h2!x := k.loc, s
}

AND gensp(s) BE
{ TEST // 3156.
       3<=s<=16   //  -> 3171
  THEN { // 3164.
         gen(f.sp0+s)
         // 3169.     -> 3192
       }
  ELSE TEST // 3171.
            0<=s<=255   //   -> 3186
       THEN { // 3178.
              genb(f.sp, s)
              //3184.    -> 3192
            }
       ELSE { // 3186.
              genw(f.spw, s)
            }
  // 3192.
  RETURN
}

// Load an item (K,N) onto the SS. It may move SS items.
AND loadt(k, n) BE
{ // 3194.
  cgpendingop()

  TEST // 3196.
       arg1+3=tempt   // -> 3225
  THEN { // 3203.
         storet(tempv)  // SS stack overflow.
         // 3207.
         MOVE(tempv+3, tempv, arg1-tempv) 
         // 3223.   -> 3235
       }
  ELSE { // 3225.
         arg2, arg1 := arg2+3, arg1+3
       }
  // 3235.
  h1!arg1,h2!arg1,h3!arg1 := k,n,ssp
  // 3252.
  ssp := ssp + 1
  // 3252.
  IF maxssp<ssp DO maxssp := ssp
  // 3260.
  RETURN
}


// Replace the top two SS items by (K,N) and set PENDINGOP=S.NONE.
AND lose1(k, n) BE
{ // 3262.
  ssp := ssp - 1

  TEST // 3267.
       arg2=tempv    // -> 3290
  THEN { // 3273.
         h1!arg2,h2!arg2 := k.loc,ssp-2
         // 3282.
         h3!arg2 := ssp-2
         // 3288.   -> 3297
       }
  ELSE { // 3290. 
         arg1 := arg2
         // 3294.
         arg2 := arg2-3
       }
  // 3297.
  h1!arg1, h2!arg1, h3!arg1 := k,n,ssp-1
  // 3310.
  pendingop := s.none
  // 3314.
  RETURN
}

AND swapargs() BE
{ // 3316: 
  LET k, n = h1!arg1, h2!arg1
  // 3322:
  h1!arg1, h2!arg1 := h1!arg2, h2!arg2
  // 3331:
  h1!arg2, h2!arg2 := k, n
  // 3338.
  RETURN
}

AND cgbyteop(op) BE
{
//debug := 2
//dboutput()
//sawritef("cgbyteop(%n)*n", op)
//abort(3340)
  // 3340.
  cgpendingop()
  // 3342.
  TEST // 3342.
       op=s.getbyte     //  -> 3363
  THEN { // 3347.
         loadba()
         // 3349.
         gen(f.gbyt)
         // 3353.
         forget.a()
         // 3355.
         lose1(k.a, 0)
         // 3361.        -> 3409
       }
  ELSE { // 3363.             // op=s.putbyte
         LET arg3 = arg2-3
         // 3367.
         TEST arg3 - tempv < 0            // -> 3390
         THEN { // 3372.
                loadt(k.loc, ssp-3)
                // 3379.
                loada(arg1)
                // 3383.
                stack(ssp-1)
                // 3388.      -> 3393 
              }
         ELSE { // 3390.
                loada(arg3)
              }
         // 3393.
         gen(f.atc)
         // 3397.
         loadba()
         // 3399.
         gen(f.pbyt)
         // 3403.
         forgetallvars()
         // 3405.
         stack(ssp-3)
         // 3410.
       }
//dboutput()
//sawritef("cgbyteop(%n): returning*n", op)
//debug := 1
//abort(3340)
  // 3410.
  RETURN
}

AND cgstind() BE
{ // This compiles indirect assignments.
  // Without optimisation the strategy is
  // (1) call cgpendingop to clear the pendingop
  // (2) compile code to place arg2 and arg1 in B and A
  // (3) generate instuction ST
  // (4) call stack(ssp-2)
  // (5) call forgetallvars
  // However, it may be possible to use:
  // ST ST1 ST2 ST3             eg A!2 := B
  // STP3 STP4 STP5             eg (P!4)!A := B
  // ST0P3 ST0P4 ST1P3 ST1P4    eg (P!4)!1 := A
  // S0G0 S0G1 S0G2             eg (G!n)!0 := A

  LET t = VALOF    // t=p3
  { 
//dboutput()
//sawritef("cgstind: entered*n")
//abort(3412)
    IF // 3412.
       pendingop=s.plus DO   //  -> 3554
    { // Compile arg1!arg2 := arg3
//sawritef("cgstind: pendingop was plus*n")
      IF // 3418.
         isnum(arg2) DO swapargs()
//dboutput()
//sawritef("cgstind: after possibly calling swapargs*n")
//abort(3426)
      IF // 3426
         isnum(arg1) DO   // -> 3453
      { // Compile n!arg2 := arg3
        // 3432.
        LET n = h2!arg1                     // p3
//sawritef("cgstind: pendingop is plus and arg1 is number %n*n", n)
//abort(3435)

        IF // 3435.
           0<=n<=3 DO  //  -> 3453 
        { // 3441.
          stack(ssp-1)
          // 3446.
          pendingop := s.none
          // Compile n!arg1 := arg2  0<=n<=3 using
          // ST ST1 ST3 or ST3 or possibly
          // ST0P3 ST0P4 ST0P5 ST1P3 ST1P2 ST1P5 S0G S0G1 S0G2

          // 3450
//sawritef("cgstind: Compile %n!arg1 := arg2  t=%n*n", n, n)
//abort(3450)
          RESULTIS n     // 0<=n<=3     -> 3557
        }
        // 3453.
      }
      // pendingop=s.plus and
      // arg1 is either a number not in range 0 to 3 or
      // it is not a number.

      // 3453.

      // Check whether arg1 or arg2 is local 3, 4 or 5
      IF // 3453.
         h1!arg2=k.loc &
         // 3458.
         3<=h2!arg2<=5 DO swapargs()
//dboutput()
//sawritef("cgstind: if arg2 was local 3, 4 or 5 the arguments have been swapped*n")
//abort(3450)

      IF // 3467.
         h1!arg1=k.loc &    // -> 3495
         // 3472.
         3<=h2!arg1<=5 DO   // -> 3495
      { // pendingop is s.plus and arg1 is local 3, 4 or 5 so
        // compile: P3!arg2:=arg3  P4!arg2:=arg3  P5!arg2:=arg3  
        // 3479.
        LET n = h2!arg1                   // p3

        // 3482.
        stack(ssp-1)
        // 3487.
        pendingop := s.none
        // Compile: P3!arg1:=arg2  P4!arg1:=arg2  P5!arg1:=arg2
        // Set t to 4, 5 or 6 and compile using
        // STP3 to STP5   eg  (P!4)!A := B
//dboutput()
//sawritef("cgstind: Compile  P%n!arg1 := arg2   t=%n*n", n, n+1)
//abort(3450)
        // 3491.
        RESULTIS n+1  // The codes for SP3, SP4 and SP5.   -> 3557
      }

      UNLESS // 3495.
             arg2=tempv DO    //  -> 3554
      { // 3501.
        LET arg3 = arg2 - 3           // p3
        // arg3 exists
        IF // 3503.
           h1!arg3=k.a DO   //  -> 3554
        { // arg3 is in A
          IF // 3508.
             h1!arg2=k.loc |  // -> 3521
             // 3513.
             h1!arg2=k.glob | // -> 3521
             // 3517.
             h1!arg2=k.numb DO // 3521.          -> 3523
                               swapargs()

          IF // 3523.
             h1!arg1=k.loc |     //  -> 3536
             // 3528.
             h1!arg1=k.glob |    //  -> 3536
             // 3532.
             h1!arg1=k.numb DO    // -> 3554
          // Optimize the case  <arg2>!<arg1> := <arg3>
          // where <arg3> is already in A
          // and <arg1> is a local, a global or a number.
          { // Compile  Pn!arg2 := A  Gn!arg2 := A  or n!arg2 := A
            // These all push arg2 then add Pn Gn or n followed by ST

            // 3536.
//dboutput()
//sawritef("cgstind: optimizing the case <arg2>!<arg1> := A where arg1 is a loc, glob or numb*n")
//abort(3536)
            push(arg2)  // Compile: A, B := arg2, arg3
            // 3540.
            cgplus()    // Compile using A, AP or AG
            // 3542.
            gen(f.st)
            // 3546.
            stack(ssp-2)
            // 3551.
            forgetallvars()
//dboutput()
//sawritef("cgstind: returning*n")
//abort(3553)
            // 3553.
            RETURN
          }
          // 3554.
        }
        // 3554.
      }
      // 3554.
    }

    // pendingop optimisations were not possible
    // so compile the pendingop and return 0 to
    // cause !arg1 := arg2 to be compiled.

    // 3554.
    cgpendingop()
//dboutput()
//sawritef("cgstind: compile !arg1 := arg2*n")
//abort(3553)
    // Compile !arg1 := arg2
    // 3556.
    RESULTIS 0
  }
  // 3557.    
  // SP3    to initialise t  p3
  // 3558.

  // 0<=t<=6

  // if t = 0 1 2 3  compile t!arg1 := arg2
  // if t = 4 5 6    compile P3!arg1:=arg2 P4!arg1:=arg2 P5!arg1:=arg2 

//dboutput()
//TEST t<=3
//THEN sawritef("cgstind: compiling %n!<arg1> := <arg2>*n", t)
//ELSE sawritef("cgstind: compiling P%n!<arg1> := <arg2>*n", t-1)
//abort(3558)

  { // 3558.
    LET k, n = h1!arg1, h2!arg1    // t=p3  k=p4  n=p5
    // Choose a Cintcode function code.
    LET cinop = VALOF
    { IF // 3564.
         k=k.glob &
         // 3568.
         t=0 DO       //  -> 3575
      { // Compile 0!Gn := arg2
        // 3573.
        RESULTIS f.s0g    // J 3604
      }
      // 3575.
      IF k = k.loc &    //  -> 3603
         // 3579.
         3<=n<=4 DO     //  -> 3603
      { IF // 3586.
           t=0 DO       //  -> 3594
        { // Compile P3!0:=arg2  P4!0:=arg2 for ST0P3 or ST0P4
          // 2589.
          RESULTIS f.st0p0+n    // -> 3604
        }
        // 3594.
        IF t=1 DO       //  -> 3603    
        { // Compile P3!1:=arg2  P4!1:=arg2 for ST1P3 or ST1P4
          // 3598.
          RESULTIS f.st1p0+n        // -> 3604
        }
        // 3603.
      }
      // 3603.
      RESULTIS 0  // No Cintcode op code chosen.
    }

    // 3604.
    // SP6    to store result in cinop
    TEST // 3605.
         cinop=0     // -> 3616
    THEN { // 3607.
//debug := 2
//dboutput()
//sawritef("cgstind: calling loadba*n")
           loadba()
           // 3609.
//dboutput()
//sawritef("cgstind: calling gen(f.st0+%n)  f.st0+t=%n*n", t, f.st0+t)
           gen(f.st0+t)
//abort(3614)
           // 3614.                // -> 3635 
         }
    ELSE { // 3616.
//debug := 2
//dboutput()
//sawritef("cgstind: calling loada(arg2)*n")
           loada(arg2)
//dboutput()
//sawritef("cgstind: after loada(arg2) cinop=%n*n", cinop)
//abort(3620)
           // 3620.
           TEST cinop=f.s0g   // -> 3632    
           THEN { // 3625.
                  geng(cinop, n)
                  // 3630.        -> 3635
                }
           ELSE { // 3632.
                  gen(cinop)
                }
         }
    // 3635.
    stack(ssp-2)
    // 3640
    forgetallvars()
//dboutput()
//sawritef("cgstind: returning*n")
//abort(3642)
    // 3642.
    RETURN 
  }
}

// Store the top item of the SS in (K,N).
AND storein(k, n) BE
// K is K.LOC, K.GLOB or K.LAB.
{ // 3644:
  cgpendingop()
  // 3646:
  loada(arg1)

  // 3650:
  SWITCHON k INTO     // -> 3683
  { DEFAULT:     // 3652:
                 cgerror("IN STOREIN %N", k)
                 // 3658:
                 STOP(40)

    CASE k.loc:  // 3662:
                 gensp(n);       ENDCASE
    CASE k.glob: // 3667:
                 geng(f.sg, n);  ENDCASE
    CASE k.lab:  // 3675;
                 genr(f.sl, n);  ENDCASE

    // 3683.  SWB and table
  }
//dboutput()
//sawritef("*n%i5: Calling forgetvar(%n, %n)*n", stvp, k, n)
  // 3702.
  forgetvar(k, n)
//dboutput()
//sawritef("*n%i5: Calling addinfo.a(%n, %n)*n", stvp, k, n)
  // 3707.
  addinfo.a(k, n)
//dboutput()
//sawritef("*n%i5: returned from addinfo.a(%n, %n)*n", stvp, k, n)
//abort(3712)
  // 3712.
  stack(ssp-1)
  // 3717.
  RETURN
}

.

SECTION "CCG5"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

LET cgrv() BE
{ // Without optimisation the strategy is
  // (1) call cgpendingop to clear prendingop
  // (2) call loada(arg1) to place arg1 in A
  // (3) compile instuction RV
  // but optimisations may be possible using
  // RV1 to RV6           eg A := A!4
  // L0P3 to L0P12        eg A := (P!8)!0
  // L1P3 to L1P6         eg A := (P!5)!1
  // L2P3 to L2P5         eg A := (P!4)!2
  // L3P3 to L3P4         eg A := (P!4)!3
  // L4P3 to L4P4         eg A := (P!4)!4
  // L0G L0G1 L0G2        eg A := (G!n)!0
  // L1G L1G1 L1G2        eg A := (G!n)!1
  // L2G L2G1 L2G2        eg A := (G!n)!2

  LET t = VALOF         // t=p3
  { IF // 3794.
       pendingop=s.plus DO
    { // 3800.
      IF isnum(arg2) DO swapargs()
      // 3808.
      IF isnum(arg1) DO
      { // 3814.
        LET n = h2!arg1    // n=p3
        // 3817.
        IF 0<=n<=6 DO  // -> 3835
        { //3823.
          stack(ssp-1)
          // 3828.
          pendingop := s.none
          // Use RV RV1 to RV6
          RESULTIS n
        }
      }

      IF // 3835.
         h1!arg2=k.loc &
         // 3840.
         3<=h2!arg2<=7 DO
      { // 3847.
        swapargs()
      }

      IF // 3849.
         h1!arg1=k.loc &
         // 3854.
         3<=h2!arg1<=7 DO  //  -> 3877
      { // 3861.
        LET n = h2!arg1   // p3
        // 3864.
        stack(ssp-1)
        // 3869.
        pendingop := s.none
        // Use RVP3 to RVP7
        // 3873.
        RESULTIS 10 + n   //  3880
      }
    }
    // 3877.
    cgpendingop()
    RESULTIS 0
  }

  // Now compile code for A := S!<arg1>
  // where          S is 0,..., 6, P!3 ,..., P!7
  // depending on   t =  0,..., 6,  13 ,...,  17

  // 3880.
  LET k, n = h1!arg1, h2!arg1   //  p3  p4

  IF // 3887.
     k=k.glob &
     // 3891.
     0<=t<=2 DO   //  ->  3903
     { // 3897.
       h1!arg1 := k.glob0 + t
       // 3902.
       RETURN
     }
  // 3903.
  IF k=k.loc & n>=3 DO   //  -> 3957
    IF t=0 & n<=12 |
       t=1 & n<=6  |
       t=2 & n<=5  |
       t=3 & n<=4  |
       t=4 & n<=4  DO
    { // 3951.
      h1!arg1 := k.loc0 + t
      // 3956.
      RETURN
    }
  // 3957.
  loada(arg1)
  TEST // 3961.
       t<=6       //  ->  3972
  THEN { // 3965.
         gen(f.rv+t)
         // 3970.   -> 3980
       }
  ELSE { // 3972.
         gen(f.rvp0 + t - 10)
       }
  // 3980.
  forget.a()
  // 3982.
  h1!arg1, h2!arg1 := k.a, 0
  // 3990.
  RETURN
}

AND cgplus() BE
// Compiles code to compute <arg2> + <arg1>.
// It does not look at PENDINGOP.

{ // 3992:
  IF iszero(arg1) DO
  { // 3998:
    stack(ssp-1)
    // 4003:
    RETURN
  }
  // 4004:
  IF iszero(arg2) DO
  { // 4010:
    IF h2!arg1=ssp-1 &
       ( // 4017:
         h1!arg1=k.loc |
         // 4022:
         k.loc0<=h1!arg1<=k.loc4) DO
      // 4032:
      loada(arg1)
    // 4036:
    lose1(h1!arg1, h2!arg1)
    // 4043.
    RETURN
  }

  TEST // 4044:
       inreg.a(arg1)   // J 4056
  THEN // 4050:
       loada(arg1)
       // 4054.        // J 4066
  ELSE IF // 4056:
          inreg.a(arg2) DO
         // 4062:
         loada(arg2)

  // 4066.
  IF h1!arg1=k.a DO swapargs()
  // 4074:
  IF h1!arg2=k.loc &
     //4076.
     3<=h2!arg2<=12 DO
    //
    swapargs()

  IF // 4089:
     h1!arg1=k.loc &
     // 4091.
     3<=h2!arg1<=12 DO
     { // 4102.
       loada(arg2)
       // 4106.
       gen(f.ap0 + h2!arg1)
       // 4110.
       forget.a()
       // 4114.
       lose1(k.a, 0)
       // 4120.
       RETURN
     }

   IF // 4121.
      h1!arg2=k.numb &
      // 4126.
      -4<=h2!arg2<=5 DO // 4135.
                        swapargs()
   IF // 4137.
      h1!arg1=k.numb &
      // 4142.
      -4<=h2!arg1<=5 DO { // 4151.
                          LET k = h2!arg1
                          //4154.
                          loada(arg2)
                          // 4158.
                          TEST k < 0    // J 4169
                          THEN { // 4161.
                                 gen(f.s0 - k)
                                 // 4167        // J4174
                               }
                          ELSE { // 4169.
                                 gen(f.a0 + k)
                                 // 4174.
                               }
                          // 4174.
                          forget.a()
                          // 4176.
                          lose1(k.a, 0)
                          // 4182.
                          RETURN
                        }

   // 4183
   IF h1!arg2=k.loc DO swapargs()
   // 4190.
   IF h1!arg1=k.loc DO   // J 4247
   { // 4195.
     LET n = h2!arg1    // p3
     // 4198.
     loada(arg2)
     // 4202.
     TEST 3<=n<=12 THEN // 4310.
                        gen(f.ap0 + n)
                   ELSE TEST // 4217.
                             0<=n<=255
                        THEN // 4224.
                             genb(f.ap, n)
                        ELSE // 4232.
                             genw(f.apw, n)
     // 4238.
     forget.a()
     // 4240.
     lose1(k.a, 0)
     // 4246.
     RETURN
   }

   // 4247.
   IF h1!arg2=k.glob DO swapargs()
   // 4254.
   IF h1!arg1=k.glob DO { // 4259.
                          loada(arg2)
                          // 4263.
                          geng(f.ag, h2!arg1)
                          // 4270.
                          forget.a()
                          // 4272.
                          lose1(k.a, 0)
                          // 4278.
                          RETURN
                        }

   // 4279.
   IF h1!arg2=k.numb DO swapargs()
   // 4286.
   IF h1!arg1=k.numb DO { // 4291.
                          LET n = h2!arg1
                          // 4294.
                          loada(arg2)
                          // 4298.
                          TEST 0<=n<=255 // -> 4313
                          THEN { // 4305.
                                 genb(f.a, n)
                                 // 4311.          -> 4319
                               }
                          ELSE { // 4313.
                                 genw(f.aw, n)
                               }
                          // 4319.
                          forget.a()
                          // 4321.
                          lose1(k.a, 0)
                          // 4327.
                          RETURN
                        }
   // 4328.
   loadboth()
   // 4330.
   gen(f.add)
   // 4334.
   forget.a()
   //4336.
   lose1(k.a, 0)
   // 4342.
}

AND cgglobal(k) BE
{ // 4344.
  LET len = 0
  // 4346.
  LET p = glist
  // 4349.
  WHILE p DO
  { // 4351.
    len := len+1
    p := !p
  }
  // len is now the length of glist.

//sawritef("4359: cgglobal: k=%n glist=%n len=%n*n", k, glist, len)
  // 4359.
  incode := FALSE
  // 4362.
  aligneven()
  // 4364.
//sawritef("4364: cgglobal: calling cgstatics*n")
//abort(4364)
  cgstatics()
  // 4366.           J 4368

//sawritef("4366: cgglobal: calling dealwithrefs*n")
//abort(4366)
  WHILE dealwithrefs(2*len + stvp) LOOP

  // 4377.
  p := glist
  // 4380.                J 4393

  WHILE p DO
  { // 4382.
    setlab(h2!p)
    // 4385.
//sawritef("4385: cgglobal: calling codew*n")
//abort(4385)
    codew(labv!(h3!p))
    // 4391.
    p := h1!p
    // 4393.
  }

  // 4396.
  codew(-len)
  // 4400.
  FOR i = 1 TO k DO     // p6
  { 
//sawritef("4406: cgglobal: i = %n stvp=%n*n", i, stvp)
    // 4406.
    codew(rdgn())
//sawritef("4410: cgglobal: stvp=%n*n", stvp)
    // 4410.
    codew(labv!rdl())
  }
//sawritef("4424: cgglobal: stvp=%n*n", stvp)
  // 4424.
  codew(maxgn)
  // 4428.
}

.

SECTION "CCG5A"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"
GET "com16/bcpl.h"

LET cgentry(n, lab) BE
{ // 4462.
  LET v = VEC 3    // v = p5
  // 4465.
  v%0 := 7
  // 4471.
  FOR i = 1 TO n DO { // 4477.        i=p10
                      LET c = rdn()   // p12
                      // 4480.
                      IF i<=7 DO v%i := c
                      // 4490.
                    }
  // 4497.
  FOR i = n+1 TO 7 DO v%i := 32  // Ascii SPACE.
  // 5516.
  chkrefs(100)  // Deal with some forward refs.
  // 4520.
  aligneven()
  // 4522.
  IF naming DO { // 4526.
//sawritef("cgentry: calling codew(entryword)*n")
                 codew(entryword)
                 // 4531.
                 FOR i = 0 TO 6 BY 2 DO
                 { // 4535.
                   codew(CAPCH(v%i) | CAPCH(v%(i+1))<<8)
                   // 3558.
                 }
               }
   // 4565.
   setlab(lab)
   // 4568.
   incode := TRUE
   // 4571.
   forgetall()
   // 4573.
}

AND cgsave(n) BE
{ // 4574.
  IF n>3 DO setinfo.a(k.loc, 3)
  // 4580.
  initstack(n)
  // 4583.
//dboutput()
//sawritef("cgsave: n=%n*n", n)
//abort(4583)
}

// Function or routine call.
AND cgapply(op, k) BE
{ // 4584.
  LET sa = k+3  // Stack address of first arg (if any).    sa=p5
//sawritef("4584: cgapply: op=%n k=%n*n", op, k)
  // 4587.
  cgpendingop()


//sawritef("4589: cgapply: dealing with no args*n")
// Deal with non args.
  // 4589.
  FOR t = tempv TO arg2 BY 3 DO
  { // 4597.
    IF h3!t>=k BREAK
    // 4604.
    IF h1!t=k.a DO storet(t)
    // 4612.
  }

//sawritef("4619: cgapply: dealing with args 2, 3,...*n")
// Deal with args 2, 3 ...
  // 4619.
  FOR t = tempv TO arg2 BY 3 DO    // t=p7
  { // 4627.
    LET s = h3!t               // p8
    // 4630.
    IF s=sa DO
    { // We have found the SS item for the first arg.
      IF // 4633.
         h1!t=k.a &
         // 4638.
         t+3=arg2 DO
      { // Two argument call with the first arg already in A.
        // 4644.
        push(arg2)
        // 4646.
        storet(arg2)    // Store second arg.
        // 4650.
        genxch()        // Restore first arg back to A.
        // 4652.
        BREAK
      }
    }
    // Ensure that all arguments other than arg1 are stored in the stack
    // 4654.
    IF s>sa DO storet(t)
    // 4661.
  }

//sawritef("4668: cgapply: move first arg (if any) into A*n")
  // Move first arg (if any) into A.
  // 4668.
  FOR t = arg2 TO tempv BY -3 DO
  { // 4676.
    LET s = h3!t   // in p8
    // 4679.
    IF s<sa BREAK
    // 4684.
    IF s=sa DO
    { // 4688.
      loada(t)
    }
    // 4691.
  }
//sawritef("4699: cgapply: sa=%n h3!tempv=%n*n", sa, h3!tempv)
  // 4699.
  IF sa<h3!tempv DO    //  p5      -> 4718
  { // 4704.
    loadt(k.loc, sa)
    // 4709.
    loada(arg1)
    // 4713.
    stack(ssp-1)
  }
    // First arg (if any) is now in A.
//sawritef("4718: cgapply: first arg (if any) is now in A*n")
  // 4718.
  TEST h1!arg1=k.glob &
       // 4723.
       3 <= k <= 12
  THEN {
//sawritef("4731: cgapply: calling geng(f.k0g+k, h2!arg1)*n")
         // 4731.
         geng(f.k0g+k, h2!arg1)
       }
  ELSE { push(arg1)
         // First arg (if any) is now in B
         // and the procedure address is in A.
         TEST // 4746.
              3<=k<=12
         THEN // 4757.
              gen(f.k0+k)
         ELSE TEST // 4759.
                   0<=k<=255
              THEN // 4766.
                   genb(f.k, k)
              ELSE // 4774.
                   genw(f.kw, k)
       }

  // 4780.
  forgetall()
  // 4782.
  stack(k)
  // 4785.
  IF op=s.fnap DO loadt(k.a, 0)
  // 4795.
//sawritef("4795: cgapply: returning*n")
//abort(4795)
}

AND cgreturn(op) BE  // used by OCODE operators FNRN and RTRN
{ // 4796.
//sawritef("csreturn: op=%n*n", op)
  cgpendingop()
  // 4798.
  IF op=s.fnrn DO loada(arg1)
  // 4807.
  gen(f.rtn)
  //stack(ssp-1)                     // BUG
  //IF op=s.fnrn DO stack(ssp-1)   // Correction by MR
  // 4811.
  incode := FALSE
}

// Used for OCODE operators JT and JF.
AND cgjump(b,l) BE
{ // 4816.
  LET f = jmpfn(pendingop)    // p5
  // 4821.
  IF f=0 DO { loadt(k.numb,0); f := f.jne }
  // 4831.
  pendingop := s.none
  // 4835.
  UNLESS b DO f := compjfn(f)
  // 4842.
  store(0,ssp-3)
  // 4849.
  genr(prepj(f),l)
  // 4857.
  stack(ssp-2)
  // 4862.
}

AND jmpfn(op) = VALOF // 4864.
                      SWITCHON op INTO
{ DEFAULT:   // 4866.
             RESULTIS 0
  CASE s.eq: // 4869.
             RESULTIS f.jeq
  CASE s.ne: // 4873.
             RESULTIS f.jne
  CASE s.ls: // 4877.
             RESULTIS f.jls
  CASE s.gr: // 4881
             RESULTIS f.jgr
  CASE s.le: // 4885.
             RESULTIS f.jle
  CASE s.ge: // 4889.
             RESULTIS f.jge
}

AND jfn0(f) = // 4916.
              f+2 // Change F.JEQ into F.JEQ0  etc...

AND revjfn(f) = // 4918.
                f=f.jls -> f.jgr,
                // 4926.
                f=f.jgr -> f.jls,
                // 4935
                f=f.jle -> f.jge,
                // 4944.
                f=f.jge -> f.jle,
                // 4953.
                f

AND compjfn(f) = // 4956.
                 f=f.jeq -> f.jne,
                 // 4964.
                 f=f.jne -> f.jeq,
                 // 4973.
                 f=f.jls -> f.jge,
                 // 4982.
                 f=f.jge -> f.jls,
                 // 4991.
                 f=f.jgr -> f.jle,
                 // 5000.
                 f=f.jle -> f.jgr,
                 // 5009.
                 f

AND prepj(f) = VALOF  // Returns the appropriate m/c fn.
{ // 5012.
  IF iszero(arg2) DO
  { // 5018.
    swapargs()
    // 5020.
    f := revjfn(f)
  }
  // 5024.
  IF iszero(arg1) DO
  { // 5030.
    loada(arg2)
    // 5034.
    RESULTIS jfn0(f)
  }
  // 5039.
  IF loadboth()=swapped RESULTIS revjfn(f)
  // 5049.
  RESULTIS f
}

.

SECTION "CCG6"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

// Compiles code for SWITCHON.
LET cgswitch(v, upb) BE
{ // 5112.
  LET n = (upb-1)/2     // Number of cases.     p5
  // 5117.
  LET dlab = rdl()      // Default label.       p6
//sawritef("cgswitchl: v=%n upb=%n n=%n dlab=L%n*n", v, upb, n, dlab)

  // 5120.
  casek, casel := v, v+n

  // Read and sort (K,L) pairs.
  // 5126.
  FOR i = 1 TO n DO
  { // 5132.
    LET k = rdn()
    // 5135.
    LET l = rdl()
    // 5138.
    LET j = i-1
    // 5141.
    WHILE j DO
    { // 5143.
      IF k > casek!j BREAK
      // 5152.
      casek!(j+1), casel!(j+1) := casek!j, casel!j
      // 5172.
      j := j - 1
      // 5175.
    }
    // 5178.
    casek!(j+1), casel!(j+1) := k, l
    // 5192.
  }
  // The case constants are in casek!1 to casek!n in ascending oerder.
  // The corresponding labels are in casel!1 to casel!n.
  // 5199.
  cgpendingop()
  // 5201.
  store(0, ssp-2)
  // 5208.
  loada(arg1)        // The argument og SWITCHON
  // 5212
  stack(ssp-1)
  // 5217.
  { TEST // 5217.
         n=0 |                             // -> 5237
         // 5220.
         n < casek!n/2 - casek!1/2 + 4     // -> 5244
    THEN { // Compile SWB switch
           // 5237.
           cgswitchb(n, dlab)  // Compile a binary chop switch
           // 5242.   -> 5249
         }
    ELSE { // Compile a SWL switch
           // 5244.
           cgswitchl(n, dlab)  // Compile a label vector switch
           // 5249.
         }
    // 5249.
//sawritef("cgswitch: returning*n")
//abort(5249)
  }
}

// Code has already been compiled to set A to the 
// value of the switch expression.
AND cgswitchb(n, dlab) BE  // Binary chop switch
{ // 5250.
//sawritef("cgswitchb: n=%n dlab=L%n*n", n, dlab)
//abort(5250)
  chkrefs(4*n+6)
//abort(5256)
  // 5256.
  gen(f.swb)
//abort(5260)
  // 5260.
  aligneven()
  // 5262.
  codew(n)
  // 5266.
  coder(dlab)
  // 5268.
  FOR i = 1 TO n DO
  { // 5274.
    codew(casek!i)
    // 5279.
    coder(casel!i)
    //5284.
  }
  // 5291.  
}


AND cgswitchl(n, dlab) BE  // Label vector switch
{ // 5292.
  LET p = 1                // p5 
//sawritef("cgswitchl: n=%n dlab=L%n*n", n, dlab)
  // 5294.
  chkrefs((casek!n - casek!1) * 2 + 10)
  // 5306.
  gen(f.swl)
  // 5310.
  aligneven()
  // 5312.
  codew(casek!n - casek!1 + 1)
  // 5321.
  coder(dlab)
  // 5324.
  codew(casek!1) // Minimum case constant
  FOR k = casek!1 TO casek!n DO    // p6
  { // 5337.
    TEST k = casek!p
    THEN { // 5337.
           coder(casel!p)
           //5348.
           p := p+1
           // 5351.
         }
    ELSE { // 5353.
           coder(dlab)
         }
    // 5356.
  }
  // 5363.
}

AND cgstring(n) BE
{ // This ensures that the string constant is padded with
  // a zero if n is even.

  // 5364.
  LET lab, a = newlab(), n    // p4  p5
  // 5369.
  loadt(k.lvlab,lab)
//sawritef("%i5: cgstring: LSTR L%n*n", stvp, n)
  // 5375.
  { IF n DO
    { // 5378.
      a := a | rdn()<<8
    }
    // 5385.
    nliste := appendblk(nliste, lab, a)
    // 5395.
    lab := 0
    // 5397.
    IF n<=1 BREAK
    // 5403.
    n := n-2
    // 5407.
    a := rdn()
    // 5410.
  } REPEAT
  // 5412.
//dboutput()
//sawritef("%i5: returning from cgstring*n", stvp)
//abort(5412)
}

AND setlab(lab) BE
{ // 5414.
  LET p = @rlist   // p4

  // rlist is a list of positions of instructions such as
  // JNE or LL that refer to labels that have not yet been set.
  // This functions sets label lab in labv and deals with all
  // instructions that refer to this label. If the such an
  // instruction is in direct relative addressing range, its
  // operand is updated and the rlist item removed from rlist.
  // If it is out of direct relative addressing range, it will
  // be resolved later using an indirect resolving word
  // generated by a call of chkrefs or dealwithrefs. These
  // resolving words are delayed to maximise the the number of
  // references that can share them.

  // 5417.
  IF debug>0 DO
    // 5421.
    WRITEF("     L%N:*n", lab)
  // 5427. 
  labv!lab := stvp  // Set the label in labv.
                    // Unset labels have the special value -1.

  // Fill in all the refs that are in range.
  { // 5432.
    LET r = !p              // p5
    // 5434.
    IF r=0 BREAK

    TEST // 5439.
         h3!r=lab &
         // 5443.
         inrange.d(h2!r, stvp)
    THEN { // 5451.
           fillref.d(h2!r, stvp)
           // 5457.
           !p := !r   // Remove item from RLIST.
           // 5459
           freeblk(r)
         }
    ELSE { // 5464.
           p := r  // Keep the item.
         }
    // 5466.
  } REPEAT
  // 5468.
  rliste := p     // Ensure that rliste is sensible.

  // 5471.
  p := @reflist

  // reflist is a list of words that need to be set to the value
  // of labels. reflist items are of the form [next, addr, lab].
  // refliste points to the last reflist item, if any, or is zero.

  { // 5474.
    LET r = !p // p points to global variable reflist or to
               // the link field of a reflist item.
    // 5476.
    IF r=0 BREAK  // We have reached the end of reflist.
     
    TEST // 5481.
         h3!r=lab
    THEN { // r points to a reflist item that refers to
           // the label that has just been set.
           // 5485.
           LET a = h2!r              // p6
           // 5487.
           putw(a,stvp-a) // Fill in the relative address.
           // 5495.
           !p := !r       // Remove item from reflist.
           // 5497.
           freeblk(r)
           // 5500.          -> 5504
         }
    ELSE // 5502.
         p := r  // Keep the reflist item.
    // 5502.
  } REPEAT     //    -> 5474 
  // 5506.
  refliste := p   // Ensure refliste is sensible.
                  // It will point to a link word containing zero,
                  // being either reflist or the link word of the
                  // last item in reflist.
  // 5509.
  RETURN
}

AND cgdatalab(lab) BE
{ // This compiles an OCODE sequence such as
  // DATALAB Llab  ITEMN 10 ITEMN 11
  // This will append [link, lab, 10] and [link, 0, 11] onto
  // then end of nlist. These static variables will be inserted
  // into the compiled code by the next call of cgstatics.
  // This compiler does not use any ITEML statements.
  // 5522.
  op := rdn()
//debug := 3
//dboutput()
//sawritef("cgdatalab: lab=L%n op=%n*n", lab, op)
  // 5526.
  IF op=s.itemn DO     // s.itemn      -> 5545
  { // 5530.
    nliste := appendblk(nliste, lab, rdn())
//sawritef("cgdatalab: setting nliste = %n -> [%n, %n, %n]*n",
//           nliste, h1!nliste, h2!nliste, h3!nliste)
//dboutput()
//abort(5541)
    // 5541.
    lab := 0
    // 5543.
    LOOP       // -> 5522
  }
  // 5545.
  IF op=s.iteml DO      // s.iteml      -> 5566
  { // 5551.
    gliste := appendblk(gliste, lab, rdl())
    // 5562.
    lab := 0
    // 5562.
    LOOP                //  -> 5522
  }
//dboutput()
//sawritef("cgdatalab: Returning with op=%n*n", op)
//abort(5566)
  // 5566.
  RETURN
} REPEAT

// 5567.
// NOP

AND cgstatics() BE // 5568.             note: no argument
                   UNTIL nlist=0 DO    // -> 5619
{ // 5570.
  LET nl, len = nlist, 0    // p3  p4

  // 5575.
  nliste := @nlist  // All NLIST items will be freed.

  // 5579.
  len, nl := len+2, !nl REPEATUNTIL nl=0 | h2!nl ~= 0

  // 5589.
  aligneven()
  // 5591.
  chkrefs(len)

  // 5594.
  setlab(h2!nlist)  // NLIST always starts labelled.

  { // 5598,
    LET blk = nlist   // p5
    // 5601.
    nlist := !nlist
    // 5605.
    freeblk(blk)
    // 5608.
    codew(h3!blk)
    // 5611.
  } REPEATUNTIL nlist=0 | h2!nlist ~= 0
  // 5619.    if nlist~=0 -> 5570
  
}
// 5623.
// RTN

.

SECTION "CCG6A"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

LET newblk(a, b, c) = VALOF
{ // Allocate and initialise a block [a,b,c] from
  // either the freelist of blocks or from space
  // obtained using GETVEC. The GETVEC blocks are
  // held in the dpblklist chain.

  // 5670.
  LET p = freelist          // p6
  TEST // 5673.
       p=0
  THEN { // The freelist is empty so allocate a block
         // from the current GETVEC block pointed to
         // by dpblk. If there is no more room in dpblk
         // allocate another using GETVEC.

         // 5676.
         dq := dq-3    //dq=g476

         // 5681.
         IF dq < dpblk DO
         { // 5685.
           dpblk := GETVEC(129)
           // 5691.
           dq := dpblk + 126 + 1
           // 4696.
           !dpblk := dpblklist
           // 5700.
           dpblklist := dpblk
         }
         // 5704.
         p := dq
         // 5707.      J 5712
       }
  ELSE // 5709.
       freelist := !p
//sawritef("newblk: a=%n b=%b c=%x4 p=%n dq=%n dpblk=%n*n", a, b, c, p, dq, dpblk)
//abort(5712)
  // 5712.
  h1!p, h2!p, h3!p := a, b, c
  // 5721.
  RESULTIS p
}

AND freeblk(p) BE
{ // 5724.
  !p := freelist
  // 5727.
  freelist := p
}

AND appendblk(a, b, c) = VALOF
{ // Create a block [0, b, c] and
  // place a pointer to it in location a.

  // 5732.
  LET p = newblk(0, b, c)      // p6
//sawritef("appendblk: created blk %n -> [%n,%n,%x4]*n", p, h1!p,h2!p,h3!p)
  // 5740.
  !a := p
  // 5741.
  RESULTIS p
}

AND initdatalists() BE
{ // 5744.
  reflist, refliste := 0, @reflist
  // 5751.
  rlist,   rliste   := 0, @rlist
  // 5758.
  nlist,   nliste   := 0, @nlist
  // 5765.
  glist,   gliste   := 0, @glist
  // 5773.
  freelist, dpblk, dq := 0, 0, 0  // MR correction
  // 5779.
}

LET geng(f, n) BE
{ //5780.
  genb(f+32*(n/256), n REM 256)
  // 5799.
}

LET gen(f) BE IF // 5800.
                 incode DO
{ // 5804. 
 chkrefs(1)
  // 5807.
  IF debug DO wrcode("", f)
  // 5817.
  codeb(f)
  // 5820.
}

LET genb(f, a) BE
{ // 5824.
  IF incode DO
  { // 5828.
    chkrefs(2)
    // 5831.
    IF debug>0 DO
      // 5835.
      wrcode("%I3", f, a)
    // 5843
    codeb(f)
    codeb(a)
  }
  // 5849.
}

LET genr(f, n) BE
{
//sawritef("genr(%n, L%n)*n", f, n)
//dboutput()
//sawritef("%i5: genr(%x2, L%n)*n", stvp, f, n)
  IF // 5854.
     incode DO
  { // 5858.
    chkrefs(2)
    IF // 5861.
       debug>0 DO wrcode("L%N", f, n)
    // 5873.
    codeb(f)
    // 5876.
    codeb(0)
    // 5881.
    relref(stvp-2, n)
//dboutput()
//abort(5881)
  }
}

LET genw(f, w) BE
{ // 5892.
  chkrefs(3)
  // 5895.
  IF debug>0 DO wrcode("W%N", f, w)
  //  5907.
  codeb(f)
  // 5910.
  codeb(w & 255)
  // 5916.
  codeb((w>>8) & 255)
}

AND checkspace() BE // 5930.
                    IF stvp/2>dp-stv DO
                    //IF stvp>32000 DO
{ // 5943.
  cgerror("PROGRAM TOO LARGE %N BYTES COMPILED", stvp)
  // 5950.
  STOP(40)
  // 5954.
}


AND codeb(byte) BE
{ // 5992. 
  stv%stvp := byte
  // 5999.
  stvp := stvp + 1
  // 6004.
  checkspace()
  // 6006.
}

AND codew(w) BE
{ IF // 6008.
     debug>0 DO
  { // 6012.
//sawritef("codew: w=%i5 =#x%x4*n", w, w)
    WRITEF("%I3:   DATA %I3 %I3*n", stvp, w>>8 & 255, w & 255)
  }
  // 6031.
  codeb(w & 255)
  // 6037.
  codeb(w>>8 & 255)
  // 6045.
}

AND coder(lab) BE
{ // Compile a word containing the relative address to label lab.

  // 6068.
  LET labval = labv!lab   // labval=p4
//sawritef("coder: lab=L%n  labval=%n*n", lab, labval)
  // 6072.
  IF debug>0 DO WRITEF("%I3:   DATA L%N-$*n", stvp, lab)
  // 6085.
  codeb(0)
  // 6088.
  codeb(0)
  // 6091.
  TEST labval=-1 THEN { // lab is unset, so append a reflist item.
                        // 6095. 
                        refliste := appendblk(refliste, stvp-2, lab)
                        // 6107.   J 6122
                      }
                 ELSE { // 6109.
                        putw(stvp-2, labval-stvp+2)
                      }
  // 6122.
//dboutput()
//abort(6122)
}

AND getw(a) = 
  // 6144.
  stv%a | stv%(a+1)<<8

AND putw(a, w) BE
{ // 6160.
  stv%a, stv%(a+1) := w, w>>8
//sawritef("putw: a=%n w=%n*n", a, w)
}

AND aligneven() BE
  IF // 6182.
     (stvp & 1) ~= 0 DO codeb(f.nop)

.

SECTION "CCG7"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

LET chkrefs(n) BE  // Resolve references until it is possible
                   // to compile n bytes without a reference
                   // going out of range.
{ // 6276:
  LET p = @rlist         // p4
//sawritef("chkrefs: n=%n p=%n*n", n, p)
//abort(6279)
  // 6279:
  skiplab := 0

  { // 6282:
    LET r, a = !p, ?           //  p5  p6
    // 6284:
    IF r=0 DO // 6287.     -> 6289
              BREAK     // -> 6339
    //6289:
    a := h2!r // RLIST is ordered in increasing A.

    IF // 6289.
       (stv%a & 1) = 0 DO     // -> 6314
    { // An unresolved reference at address A
      IF // 6299.
         inrange.i(a, stvp+n+3) DO //        -> 6311
                                   BREAK  // -> 6339
      // This point is reached if there is an unresolved
      // ref at A which would be able to access an
      // indirection word at stvp+n+3 and so an indirect
      // data word must be compiled now. The +3 is to
      // allow for a possible skip jump instruction and
      // a possible filler byte.

      // 6311.
      genindword(h3!r)
      // 6314.
    }

    // At this point the reference at A is in range of
    // a resolving indirect data word and should be
    // removed from rlist if there is no chance that
    // it can be resolved by a direct relative address.
    TEST // 6314.
         inrange.d(a, stvp)   // -> 6326
    THEN { // 6322.
           p := r        // Keep the item.
           // 6324.    -> 6337
         }
    ELSE { // 6326.
           !p := !r   // Free item if already resolved
           // 6328.
           freeblk(r) // and no longer in direct range.
           // 6331
           IF !p=0 DO rliste := p  // Correct RLISTE.
         }
    // 6337.     -> 6282
  } REPEAT

  // At this point all necessary indirect data words have
  // been compiled.

  // 6339:
  UNLESS skiplab=0 DO { // 6343:
                        setlab(skiplab)
                        // 6345.
                        skiplab, incode := 0, TRUE
                      }
  // 6351:
}

AND dealwithrefs(n) = VALOF
{ // Ensure that n bytes of code can be compiled before
  // any forward references go out of range. This may
  // require the compilation of some indirect resolving words.
  // It return TRUE is it generated at least one resolving word.
  // 6352.
  LET p = @rlist
//sawritef("*n%i5: 6355 dealwithrefs: entered n=%n*n", stvp, n)

// Temp fudge                  ####################################
//  chkrefs(512) // Deal with all outstanding references
//  RESULTIS FALSE
// Temp fudge                  ####################################

//abort(6355)
  // 6355.
  WHILE h1!p DO      // -> 6401
  { // 6357.
    LET np = h1!p     // np=p5
    // 6359.
    LET addr = h2!np  // addr=p6  byte address
    // 6361.
    LET lab = h3!np   // lab=p7
//sawritef("%i5: 6363 dealwithrefs: p=%n np=%n addr=%n lab=%n labv!lab=%n stv%%addr=%n*n",
//          stvp, p, np, addr, lab, labv!lab, stv%addr)
//sawritef("%i5: 6363 dealwithrefs: addr=%n lab=%n ((stv%%addr) & 1)=%n (labv!lab)=-1)=%n*n",
//          stvp, addr, lab, (stv%addr & 1), labv!lab=-1)
//abort(6363)
    IF // 6363.
       (stv%addr & 1) = 0     // -> 6399
    { TEST // 6371.
           labv!lab=-1             // -> 6393
      THEN { 
//sawritef("%i5: 6377 dealwithrefs: addr=%n lab=L%n unresolved and label unset*n", stvp, addr, lab)
//abort(6377)
             // 6377.
             UNLESS inrange.d(addr, n+2) DO  //  -> 6391
             {
//sawritef("%i5: 6385 dealwithrefs: addr=%n lab=L%n not in direct relative range so gen resolving word*n",
//          stvp, addr, lab)
//abort(6385)
               // 6385.
               genindword(lab)
//sawritef("%i5: 6388 dealwithrefs: Returning TRUE*n", stvp)
//abort(6388)
               // 6388.
               RESULTIS TRUE   // -> 6405
             }
//sawritef("%i5: 6391 dealwithrefs: addr=%n lab=L%n in direct relative range so ignore this item*n",
//          stvp, addr, lab)
//abort(6391)
             // 6391.       J 6399
           }
      ELSE {
//sawritef("%i5: 6393 dealwithrefs: addr=%n lab=L%n instruction not yet resolved by indirect ref*n",
//          stvp, addr, lab)
//sawritef("%i5: 6393 dealwithrefs: addr=%n lab=L%n or the label is already set, so gen resolve word*n",
//          stvp, addr, lab)
//abort(6393)
           // 6393.
           genindword(lab)
           // 6396.
//sawritef("%i5: 6396 dealwithrefs: Returning TRUE*n", stvp)
//abort(6396)
             // 6396.
             RESULTIS TRUE  //  -> 6405
           }
    }

    // 6399.
    p := np
    // 6401.
    // WHILE loop test: if h1!p~=0   -> 6357 
  }
//sawritef("%i5: 6404 dealwithrefs: Returning FALSE*n", stvp)
//abort(6404)
  // 6404.
  RESULTIS FALSE
}

AND genindword(lab) BE  // Called only from chkrefs and dealwithrefs.
{ // This function generates an indirect reference resolving word
  // only used by relative addressing instructions that cannot use
  // direct relative addresses. Resolving words are used if the
  // relative address is too far back or too far forward, ie not
  // within direct relative addressing range.

  // 6406.
  LET r = rlist // Assume RLIST ~= 0         p4
                // This is because genindword is only called
                // when there is at least one outstanding
                // forward reference to label lab.
//dboutput()
//sawritef("%i5: genindword: lab=L%n*n", stvp, lab)
  // 6409.
  IF incode DO  // -> 6450
  { // Compile a skip jump around the resolving word
    // 6413. 
    skiplab := newlab()
    // 6417.
    IF debug>0 DO
      // 6421.
      wrcode("L%N", f.j, skiplab)
    // 6431.
    codeb(f.j)
    // 6435.
    codeb(0)
    // 6438.
    relref(stvp-2, skiplab)
    // 6447.
    incode := FALSE
  }
  // 6450.
  aligneven()
//dboutput()
//sawritef("%i5: genindword: lab=L%n r=%n*n", stvp, lab, r)
  // 6452.
  UNTIL r=0 DO
  { // r points to an rlist item of the form [next, addr, lab]
//sawritef("%i5: genindword: r=%n -> [%n,%n,L%n]*n", stvp, r, h1!r, h2!r, h3!r)
//UNLESS (stv%(h2!r) & 1)=0 DO
//{ //sawritef("%i5: 6454 genindword: instruction %x2 at %n is already indirect*n",
//  //         stvp, stv%(h2!r), h2!r)
//  //abort(999)
//}
    IF // 6454.
       h3!r=lab &              // -> 6472
       // 6458.
       (stv%(h2!r) & 1)=0 DO   // -> 6472
    { 
//sawritef("genindword: Filling in indirect forward ref*n")
//sawritef("genindword: instruction:  %i5: %x2 %x2   L%n resolving word at %n*n",
//          h2!r, stv%(h2!r), stv%(h2!r+1), lab, stvp)
      // 6466.
      fillref.i(h2!r, stvp)
//sawritef("genindword: updated to:   %i5: %x2 %x2   L%n resolving word at %n*n",
//          h2!r, stv%(h2!r), stv%(h2!r+1), lab, stvp)
//abort(6446)
      //{ // We should remove item r from rlist because it has been resolved.
//sawritef("%i5: genindword: Not removing item r=%n from rlist*n", stvp, r)
      //}
    }
    // 6472.
    r := !r
    // 6474.
  }
//sawritef("%i5: genindword: calling coder(%n)*n", stvp, lab)
  // 6477.
  coder(lab) // Generate an indirect resolving word for label lab.
//dboutput()
//abort(6477)
}

AND inrange.d(a, p) =
  // 6486.
  a-126 <= p <= a+129
// The result is TRUE if direct relative instr (eg J) at
// A can address location P directly.

AND inrange.i(a, p) = VALOF
// The result is TRUE if indirect relative instr (eg J)
// at A can address a resolving word at p.
{ // 6506.
  LET rel = (p-a-2)/2          // p5
//sawritef("inrange.i: a=%n p=%n p-a-2=%n rel=%n res=%n*n", a, p, p-a-2, rel, 0<=rel<=255)
//abort(6513)
  // 6513.
  RESULTIS 0 <= rel <= 255
}

AND fillref.d(a, p) BE
{ // 6528.
  stv%a := stv%a & 254  // Back to direct form if neccessary.
  // 6541.
  stv%(a+1) := p-a+126
//sawritef("fillref.d: a=%n p=%n*n", a, p)
//sawritef("fillref.d: updated instruction at %n is %n %n*n", a, stv%a, stv%(a+1))
//abort(6541)
}

AND fillref.i(a, p) BE  // p is the (even) address of the resolving word.
                        // a is the address of a relative addressing instruction.
                        // This will be made indirect and the operand byte
                        // at a+1 will be set to the appropriate relative
                        // address.
{ // 6558.
  stv%a := stv%a | 1    // Force indirect form.
  // 6570.
  stv%(a+1) := (p-a-2)/2
//sawritef("%i5: fillref.i: a=%n p=%n p-a-2=%n (p-a-2)/2=%n*n", stvp, a, p, (p-a-2), (p-a-2)/2)
//sawritef("%i5: fillref.i: updated instruction at %n is %x2 %x2*n", stvp, a, stv%a, stv%(a+1))
///IF (p-a-2)/2 > 255 DO abort(6570)
}

AND relref(a, l) BE
// RELREF is only called just after compiling
// a relative reference instruction at
// address A (=stvp-2).
{ // 6588.
  LET labval = labv!l              // p5
//sawritef("relref: a=%n l=L%n*n", a,l)
   IF // 6592.
      labval>=0 &
      // 6594.
      inrange.d(a, labval) DO
   { // 6600.
     fillref.d(a, labval)
     // 6605.
     RETURN
   }

   // All other references in RLIST have
   // addresses smaller than A and so RLIST will
   // remain properly ordered if this item
   // is added to the end.

   // 6606.
   !rliste := newblk(0, a, l)
   // 6615.
   rliste := !rliste
//sawritef("relref: L%n not yet set*n", l)
//dboutput()
//abort(6615)
}

.

SECTION "CCG8"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"
GET "com16/bcpl.h"

LET outputsection(lastsection) BE   // g420
{ // 6682.
  WHILE reflist DO
  { // 6684.
    cgerror("LABEL L%N UNSET", h3!reflist)
    //6691.
    reflist := !reflist
    // 6695.   if reflis ~= 0 J 6684
  }
  // 6699.
  IF codestream DO  //g254
  { // 6703.
    SELECTOUTPUT(codestream)
    // 6705.
    OBJWORD(t.hunk)
    // 6710.
    OBJWORD(stvp/2)
    // Fudge for sections CCG6A and CCG7
    //IF getw(0)=0 DO // Only fill in the length word if present
    ///  putw(0, stvp/2) // Fill in the hunk length -- add by MR
//sawritef("outputsection: stvp=%n*n", stvp)
//FOR i = 0 TO stvp-1 DO
//{ IF i MOD 10 = 0 DO sawritef("*n%i3: ", i)
//  sawritef(" %x2", stv%i)
//}
//sawritef("*n*n")
//sawritef("Calling WRITEWORDS(stv, %n) stvp=%n*n", stvp>>1, stvp)
    // 6716.
    WRITEWORDS(stv, stvp>>1) // stvp>>1 is the number of 16-bit words to write
                             // stvp is assumed to be even.
    // 6725.
    IF lastsection DO
    { // 6728.
      OBJWORD(t.end)
    }
    // 6733.
    SELECTOUTPUT(oldoutput)
  }
  // 6737.
}

AND OBJWORD(w) BE    // g421
{ WRBIN(w)
  WRBIN(w>>8)
}

/*############################################
.

//       Extra debugging code not in the standard codegenerator.

SECTION "CCG9"

GET "com16/LIBHDR.h"
GET "com16/ccghdr.h"

LET dboutput() BE
{ WRITEF("%i5:", stvp)
  WRITES(" A="); wrkn(infok.a, infon.a)
  WRITES(" B="); wrkn(infok.b, infon.b)
  WRITEF(" ssp=%i3", ssp)
  WRITEF(" pndop=%s", op2str(pendingop))
  WRITEF(" incode=%c", incode -> 'T', 'F')
  
  IF debug>=2 DO { WRITES("  STK: ")
                   FOR p=tempv TO arg1 BY 3  DO
                   { IF (p-tempv) REM 30 = 10 DO NEWLINE()
                     wrkn(h1!p,h2!p)
                     WRCH('*s')
                   }
                 }
   
  IF debug=3 DO {  LET l = rlist
                   WRITES("*nrlist: ")
                   UNTIL l=0 DO { WRITEF("%n L%n  ", l!1, l!2)
                                  l := !l
                                }
                   l := nlist
                   WRITES("*nnlist: ")
                   UNTIL l=0 DO { WRITEF("L%n %x4  ", l!1, l!2)
                                  l := !l
                                }
                }
  NEWLINE()
  //IF glist DO
  //{ sawritef("%i5: dboutput: glist is non empty*n", stvp)
  //  abort(999)
  //}
}


AND wrkn(k,n) BE
{ LET s = VALOF SWITCHON k INTO
  { DEFAULT:       //sawritef(" wrkn: k=%n n=%n*n", k, n)
                   k := n
                   RESULTIS "?"
    CASE k.none:   RESULTIS "-"
    CASE k.numb:   RESULTIS "N%n"
    //CASE k.fnlab:  RESULTIS "F"
    CASE k.lvloc:  RESULTIS "@P%n"
    CASE k.lvglob: RESULTIS "@G%n"
    CASE k.lvlab:  RESULTIS "@L%n"
    CASE k.a:      RESULTIS "A"
    CASE k.b:      RESULTIS "B"
    CASE k.c:      RESULTIS "C"
    CASE k.loc:    RESULTIS "P%n"
    CASE k.glob:   RESULTIS "G%n"
    CASE k.lab:    RESULTIS "L%n"
    CASE k.loc0:   RESULTIS "0P%n"
    CASE k.loc1:   RESULTIS "1P%n"
    CASE k.loc2:   RESULTIS "2P%n"
    CASE k.loc3:   RESULTIS "3P%n"
    CASE k.loc4:   RESULTIS "4P%n"
    CASE k.glob0:  RESULTIS "0G%n"
    CASE k.glob1:  RESULTIS "1G%n"
    CASE k.glob2:  RESULTIS "2G%n"
  }
  WRITEF(s, n)
}

AND wrcode(form, f, a, b) BE
{ IF debug=2 DO dboutput()
//sawritef("Calling wrcode(form=*"%s*", f=%i3, a=%i5, b=%i5*n", form, f, a, b)
  WRITEF("%i4: ", stvp)
  wrfcode(f)
  WRITES("  ")
  WRITEF(form, a, b)
  NEWLINE()
//abort(1234)
}

AND wrfcode(f) BE
{ LET s = VALOF SWITCHON f&31 INTO
  { DEFAULT:
    CASE  0: RESULTIS "     -     K   LLP     L    LP    SP    AP     A"
    CASE  1: RESULTIS "     -    KW  LLPW    LW   LPW   SPW   APW    AW"
    CASE  2: RESULTIS "   BRK   S0G  S0G1  S0G2     -     -     -     -"
    CASE  3: RESULTIS "    K3   K3G  K3G1  K3G2   LP3   SP3   AP3  L0P3"
    CASE  4: RESULTIS "    K4   K4G  K4G1  K4G2   LP4   SP4   AP4  L0P4"
    CASE  5: RESULTIS "    K5   K5G  K5G1  K5G2   LP5   SP5   AP5  L0P5"
    CASE  6: RESULTIS "    K6   K6G  K6G1  K6G2   LP6   SP6   AP6  L0P6"
    CASE  7: RESULTIS "    K7   K7G  K7G1  K7G2   LP7   SP7   AP7  L0P7"
    CASE  8: RESULTIS "    K8   K8G  K8G1  K8G2   LP8   SP8   AP8  L0P8"
    CASE  9: RESULTIS "    K9   K9G  K9G1  K9G2   LP9   SP9   AP9  L0P9"
    CASE 10: RESULTIS "   K10  K10G K10G1 K10G2  LP10  SP10  AP10 L0P10"
    CASE 11: RESULTIS "   K11  K11G K11G1 K11G2  LP11  SP11  AP11 L0P11"
    CASE 12: RESULTIS "   K12  K12G K12G1 K12G2  LP12  SP12  AP12 L0P12"
    CASE 13: RESULTIS " CODE1   L0G  L0G1  L0G2  LP13  SP13     -     -"
    CASE 14: RESULTIS "    LM   L1G  L1G1  L1G2  LP14  SP14     -     -"
    CASE 15: RESULTIS "   LM1   L2G  L2G1  L2G2  LP15  SP15 CODE2     -"
    CASE 16: RESULTIS "    L0    LG   LG1   LG2  LP16  SP16   NOP     -"
    CASE 17: RESULTIS "    L1    SG   SG1   SG2   SYS    S1    A1   NEG"
    CASE 18: RESULTIS "    L2   LLG  LLG1  LLG2   SWB    S2    A2   NOT"
    CASE 19: RESULTIS "    L3    AG   AG1   AG2   SWL    S3    A3  L1P3"
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
  FOR i = 6*n+1 TO 6*(n+1) DO WRCH(s%i)
}

AND op2str(op) = VALOF SWITCHON op INTO
  { DEFAULT:        RESULTIS "Unknown op"

    CASE s.true:    RESULTIS "true"  
    CASE s.false:   RESULTIS "false"  
    CASE s.rv:      RESULTIS "rv"  
    CASE s.fnap:    RESULTIS "fnap"  
    CASE s.mult:    RESULTIS "mult"  
    CASE s.div:     RESULTIS "div"  
    CASE s.rem:     RESULTIS "rem"  
    CASE s.plus:    RESULTIS "plus"  
    CASE s.minus:   RESULTIS "minus"  
    CASE s.query:   RESULTIS "query"  
    CASE s.neg:     RESULTIS "neg"  
    CASE s.abs:     RESULTIS "abs"  
    CASE s.eq:      RESULTIS "eq"  
    CASE s.ne:      RESULTIS "ne"  
    CASE s.ls:      RESULTIS "ls"  
    CASE s.gr:      RESULTIS "gr"  
    CASE s.le:      RESULTIS "le"  
    CASE s.ge:      RESULTIS "ge"  
    CASE s.not:     RESULTIS "not"  
    CASE s.lshift:  RESULTIS "lshift"  
    CASE s.rshift:  RESULTIS "rshift"  
    CASE s.logand:  RESULTIS "logand"  
    CASE s.logor:   RESULTIS "logor"  
    CASE s.eqv:     RESULTIS "eqv"  
    CASE s.neqv:    RESULTIS "neqv"  
    CASE s.lf:      RESULTIS "lf"  
    CASE s.lp:      RESULTIS "lp"  
    CASE s.lg:      RESULTIS "lg"  
    CASE s.ln:      RESULTIS "ln"  
    CASE s.lstr:    RESULTIS "lstr"  
    CASE s.ll:      RESULTIS "ll"  
    CASE s.llp:     RESULTIS "llp"  
    CASE s.llg:     RESULTIS "llg"  
    CASE s.lll:     RESULTIS "lll"  
    CASE s.needs:   RESULTIS "needs"  
    CASE s.section: RESULTIS "section"  
    CASE s.rtap:    RESULTIS "rtap"  
    CASE s.goto:    RESULTIS "goto"  
    CASE s.finish:  RESULTIS "finish"  
    CASE s.switchon:RESULTIS "switchon"  
    CASE s.global:  RESULTIS "global"  
    CASE s.sp:      RESULTIS "sp"  
    CASE s.sg:      RESULTIS "sg"  
    CASE s.sl:      RESULTIS "sl"  
    CASE s.stind:   RESULTIS "stind"  
    CASE s.jump:    RESULTIS "jump"  
    CASE s.jt:      RESULTIS "jt"  
    CASE s.jf:      RESULTIS "jf"  
    CASE s.endfor:  RESULTIS "endfor"  
    CASE s.xlab:    RESULTIS "xlab"  
    CASE s.lab:     RESULTIS "lab"  
    CASE s.stack:   RESULTIS "stack"  
    CASE s.store:   RESULTIS "store"  
    CASE s.rstack:  RESULTIS "rstack"  
    CASE s.entry:   RESULTIS "entry"  
    CASE s.save:    RESULTIS "save"  
    CASE s.fnrn:    RESULTIS "fnrn"  
    CASE s.rtrn:    RESULTIS "rtrn"  
    CASE s.res:     RESULTIS "res"  
    CASE s.datalab: RESULTIS "datalab"  
    CASE s.iteml:   RESULTIS "iteml"  
    CASE s.itemn:   RESULTIS "itemn"  
    CASE s.endproc: RESULTIS "endproc"  
    CASE s.debug:   RESULTIS "debug"  
    CASE s.none:    RESULTIS "none"  
    CASE s.getbyte: RESULTIS "getbyte"  
    CASE s.putbyte: RESULTIS "putbyte"  
  }
#####################################*/



