// This is a reconstruction of bcpltrn.b for the BBC BCPL System.
// When compiled by bbcbcpl using the 32-bit BCPL system it
// should generate an object file identical to bbccin/BCPLTRN.

// Reconstructed by Martin Richards (c) Mar 2017

// This reconstruction was made with the aid of oldcom/bcpl-trn.b
// and map/BCPLTRN.map created by: c df BCPLTRN.

// Comments such as // 1187: give byte addresses of positions
// in the object code. They refer to BCPL statements immediately
// below the comment.

SECTION "TRN1"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/trnhdr.h"

LET START(x) BE
{ // 16.
  err.p, err.l := LEVEL(), fail
  // 24.
  errcount := 0
  // 27.
  dvece, dvecp := 3, 3
  // 32.
  dvec!0, dvec!1, dvec!2 := 0, 0, 0

  // 42.
  globdecls := 0

  // 45.
  casep, caseb := 0, -1

  // 50.
  endcaselabel, defaultlabel := 0, 0
  // 55.
  resultlabel, breaklabel, looplabel := -1, -1, -1
  // 62.
  comcount, currentbranch := 0, x
  // 68.
  ocount, paramnumber := 0, 0
//sawritef("bcpltrn entered*n")
  // 73.
  SELECTOUTPUT(ocodeoutstream)
  // 77.
  WHILE x~=0 &
       (h1!x=s.section | h1!x=s.needs) DO
  { // 79.
     out1(h1!x)
     // 82.
     outstring(h2!x+1)
     // 86.
     x:=h3!x
  }
  // 101.
  ssp := savespacesize
  // 105.
  out2(s.stack, ssp)
//sawritef("bcpltrn: calling decllabels(x)*n")
  // 112.
  decllabels(x)
//sawritef("bcpltrn: calling trans(x)*n")
  // 115.
  trans(x)
//sawritef("bcpltrn: calling out2(s.global, globdecls/2)*n")
  // 118.
  out2(s.global, globdecls/2)

  // 127.
  FOR i = 0 TO globdecls-2 BY 2 DO  // i=p4
    // 135.
    out2(globdecl!i, globdecl!(i+1))

fail:
//sawritef("bcpltrn: START calling SELECTOUTPUT(verstream) verstream=%n*n", verstream)
  // 156.
  SELECTOUTPUT(verstream)
  // 160.
}


AND nextparam() = VALOF
{ // 162.
  paramnumber := paramnumber + 1
  // 167.
  RESULTIS paramnumber
}

AND transreport(n, x) BE
{ // 168.
   LET oldout = OUTPUT()
   // 171.
   SELECTOUTPUT(verstream)
   // 175.
   wrtransmess(n, x, comcount)
   // 184.
   errcount := errcount+1
   // 189.
   reportcount := reportcount+1
   // 194.
   rc := 40
   // 198.
   IF reportcount>=10 DO
   { // 202.
     WRITES("*nTOO MANY ERRORS*n")
     // 206.
     LONGJUMP(err.p, err.l)
   }
   // 213.
   IF n=141 |        // -> 227
      // 218.
      n=143 |        // -> 227
      // 222.
      n=144 DO                   // -> 238
   { // 227.
     WRITES("*nFATAL ERROR*n")
     // 231.
     LONGJUMP(err.p, err.l)
   }
   // 238.
   SELECTOUTPUT(oldout)
   // 241.
   RETURN
}

AND wrtransmess(n, x, count) BE
{ // 274.
  LET mess = VALOF SWITCHON n INTO    // mess=p6
  { // 276.
    DEFAULT: // 276.
             RESULTIS "Bad Expression"
    CASE 101: // 280.
             RESULTIS "DEFAULT ??"
    CASE 104: // 284.
             RESULTIS "BREAK, LOOP or RESULTIS ??"
    CASE 105: // 288.
             RESULTIS "CASE ??"
    CASE 106: // 292.
             RESULTIS "Same CASE twice"
    CASE 113:
    CASE 109: // 296.
             RESULTIS "Bad LHS expr."
    CASE 112:
    CASE 110: // 300.
             RESULTIS "LHS & RHS mismatch"
    CASE 115: // 304.
             RESULTIS "*"%S*" not declared"
    CASE 116: // 308.
             RESULTIS "*"%S*" out of scope"
    CASE 119:
    CASE 118:
    CASE 117: // 312.
             RESULTIS "Bad constant"
    CASE 141: // 316.
             RESULTIS "Too many cases"
    CASE 142: // 320.
             RESULTIS "*"%S*" declared twice"
    CASE 143: // 324.
             RESULTIS "Too many names"
    CASE 144: // 328.
             RESULTIS "Too many globals"
  }
  // 407.
  WRITEF("Error after %N commands*n", count)
  // 413.
  WRITEF(mess, @h3!x)
  // 419.
  NEWLINE()
  // 421.
  RETURN
}

.

SECTION "TRN2"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/trnhdr.h"

LET trans(x) BE
{ again:
  // 746.
  IF x=0 RETURN

  { // 750.
    LET sw = FALSE
    // 752.
    comcount := comcount+1
    // 757.
    currentbranch := x
//sawritef("trans: x=%n switching on %n*n", x, h1!x)
//abort(2000)
    // 760.
    SWITCHON h1!x INTO

    { CASE 96:
      CASE 95:
      CASE 94:
      CASE 93:
      CASE 92:
      CASE 91:
      CASE 90:
      CASE 89:
      CASE 88:
      CASE 87:
      CASE 86:
      CASE 85:
      CASE 84:
      CASE 83:
      CASE 82:
      CASE 81:
      CASE 80:
      CASE 78:
      CASE 77:
      CASE 64:
      DEFAULT:
        // 762.
        transreport(100, x)
        // 768.
        ENDCASE

      CASE s.let:
      { // 770.
        LET a, s, s1 = dvece, ssp, 0
        // 778.
        LET v = vecssp
//sawritef("trans: case let: calling declnames(%n)*n", h2!x)
        // 761.
        declnames(h2!x)
//sawritef("trans: case let: calling checkdistinct(%n, %n)*n", a, dvece)
        // 784.
        checkdistinct(a, dvece)
        // 790.
        vecssp, s1 := ssp, ssp
        // 795.
        ssp := s
//sawritef("trans: case let: calling transdef(%n)*n", h2!x)
        // 798.
        transdef(h2!x)
        // 801.
        UNLESS ssp=s1 DO transreport(110, x)
        // 812.
        UNLESS ssp=vecssp DO { // 818.
                               ssp := vecssp
                               // 820.
                               out2(s.stack, ssp)
                             }
        // 829.
//sawritef("trans: case let: calling out1(s.store)*n")
        out1(s.store)
//sawritef("trans: case let: calling decllabels(%n)*n", h3!x)
        // 833.
        decllabels(h3!x)
//sawritef("trans: case let: calling trans(%n)*n", h3!x)
        // 836.
        trans(h3!x)
        // 839.
        vecssp := v
        // 842.
        UNLESS ssp=s DO out2(s.stack, s)
        // 853.
        dvece, ssp := a, s
//sawritef("trans: case let: done*n")
        // 859,
        ENDCASE    // -> 1642
      }

      CASE s.static:
      CASE s.global:
      CASE s.manifest:
      { // 861.
        LET a, s = dvece, ssp
        // 867.
        AND op = h1!x     // p7
        // 869.
        LET list = h2!x
        // 871.
        LET p = list + 2
        // 874.
        IF op=s.manifest DO op := s.number
        // 881.
        FOR i = 0 TO h2!list-1 BY 2 DO    // -> 958
        { // 889.
          LET name = p!i
          // 893.
          LET k = evalconst(p!(i+1))
          // 902.
          TEST op=s.static                // -> 944
          THEN { // 907.
                 LET m = nextparam()     // p14
                 // 912.
                 addname(name, s.label, m)
                 // 924.
                 out2(s.datalab, m)
                 // 933.
                 out2(s.itemn, k)
                 // 942.             -> 955
               }

          ELSE { // 944.
                 addname(name, op, k)
                 // 955.
               }

        }
        // 955.   Code at end of FOR loop

        // 962.
        decllabels(h3!x)
        // 965.
        trans(h3!x)
        // 968.
        dvece, ssp := a, s
        // 972.
        ENDCASE            // -> 1642
      }


      CASE s.ass:
        // 976.
        assign(h2!x, h3!x)
        // 981.
        ENDCASE   // -> 1642

      CASE s.rtap:
      { // 983.
        LET s = ssp
        // 986.
        ssp := ssp+savespacesize
        // 990.
        out2(s.stack, ssp)
        // 995.
        loadlist(h3!x)
        // 998.
        load(h2!x)
        // 1001.
        out2(s.rtap, s)
        // 1007.
        ssp := s
        // 1007.
        ENDCASE   // -> 1642
      }

      CASE s.goto:
        // 1012.
        load(h2!x)
        // 1015.
        out1(s.goto)
        // 1019.
        ssp := ssp-1
        // 1024.        -> 1642
        ENDCASE

      CASE s.colon:
        // 1026.
        out2(s.xlab, h4!x)
        // 1032.
        comcount := comcount-1
        // 1037.
        x := h3!x
        // 1039.
        GOTO again

      CASE s.unless:
        // 1042.
        sw := TRUE
      CASE s.if:
      { // 1044.
        LET l = nextparam()
        // 1047.
        jumpcond(h2!x, sw, l)
        // 1052.
        trans(h3!x)
        // 1057.
        out2(s.lab, l)
        // 1063.
        ENDCASE    // -> 1642
      }

      CASE s.test:
      { // 1065
        LET l, m = nextparam(), nextparam()
        // 1071.
        jumpcond(h2!x, FALSE, l)
        // 1078.
        trans(h3!x)
        // 1081.
        out2(s.jump, m)
        // 1087.
        out2(s.lab, l)
        // 1093.
        trans(h4!x)
        // 1096.
        out2(s.lab, m)
        // 1102.
        ENDCASE     // -> 1642
      }

      CASE s.loop:
        // 1104.
        IF looplabel<0 DO transreport(104, x)
        // 1114.
        IF looplabel=0 DO looplabel := nextparam()
        // 1122.
        out2(s.jump, looplabel)
        // 1129..
        ENDCASE     // -> 1642

      CASE s.break:
        // 1131.
        IF breaklabel<0 DO transreport(104, x)
        // 1141.
        IF breaklabel=0 DO breaklabel := nextparam()
        // 1149.
        out2(s.jump, breaklabel)
        // 1156.
        ENDCASE    // -> 1642

      CASE s.return:
        // 1158.
        out1(s.rtrn)
        // 1162.
        ENDCASE    // -> 1642

      CASE s.finish:
        // 1164.
        out1(s.finish)
        // 1168.
        ENDCASE    // -> 1642

      CASE s.resultis:
        // 1177.
        IF resultlabel<0 DO transreport(104, x)
        // 1180.
        load(h2!x)
        // 1183.
        out2(s.res, resultlabel)
        // 1190.
        ssp := ssp - 1
        // 1195.
        ENDCASE    // -> 1642

      CASE s.while:
        // 1197.
        sw := TRUE
      CASE s.until:
      { // 1199.
        LET l, m = nextparam(), nextparam()
        // 1205
        LET bl, ll = breaklabel, looplabel
        // 1211.
        breaklabel, looplabel := 0, m
        // 1217.
        out2(s.jump, m)
        // 1222.
        out2(s.lab, l)
        // 1228.
        trans(h3!x)
        // 1231.
        out2(s.lab, m)
        // 1237.
        jumpcond(h2!x, sw, l)
        // 1244.
        UNLESS breaklabel=0 DO out2(s.lab, breaklabel)
        // 1253.
        breaklabel, looplabel := bl, ll
        // 1259.
        ENDCASE    // -> 1642
      }

      CASE s.repeatwhile:
        // 1261.
        sw := TRUE
      CASE s.repeatuntil:
      CASE s.repeat:
      { // 1263.
        LET l,bl,ll = nextparam(),breaklabel,looplabel
        // 1277.
        breaklabel, looplabel := 0, 0
        // 1286.
        out2(s.lab, l)
        // 1293.
        TEST h1!x=s.repeat
        THEN { // 1297.
               looplabel := l
               // 1300.
               trans(h2!x)
               // 1303.
               out2(s.jump, l)
             }
        ELSE { // 1311.
               trans(h2!x)
               // 1314.
               UNLESS looplabel=0 DO
                 // 1318. 
                 out2(s.lab, looplabel)
               // 1323.
               jumpcond(h3!x, sw, l)
             }
        // 1330.
        UNLESS breaklabel=0 DO out2(s.lab, breaklabel)
        // 1339.
        breaklabel, looplabel := bl, ll
        // 1345.
        ENDCASE   // -> 1642
      }

      CASE s.case:
      { // 1347.
        LET l, k = nextparam(), evalconst(h2!x)
//sawritef("s.case: l=L%n k=%n casep=%n caset=%n*n", l, k, casep, caset)
        // 1354.
        IF casep>=caset DO
        { // 1360.
//abort(1360)
          transreport(141, x)
        }
        // 1366. 
        IF caseb<0 DO transreport(105, x)
        // 1376.
        FOR i = caseb TO casep-1 DO
          // 1385.
          IF casek!i=k DO transreport(106, x)
        // 1400.
        casek!casep := k
        // 1411.
        casel!casep := l
        // 1418.
        casep := casep + 1
        // 1423.
        out2(s.lab, l)
        // 1429.
        x := h3!x
        // 1431.
        GOTO again    // -> 746
      }

      CASE s.default:
        // 1434.
        IF caseb<0 DO transreport(105, x)
        // 1444.
        UNLESS defaultlabel=0 DO
          // 1448.
          transreport(101, x)
        // 1454.
        defaultlabel := nextparam()
        // 1458.
        out2(s.lab, defaultlabel)
        // 1463.
        x := h2!x
        // 1465.
        GOTO again   // -> 746

      CASE s.endcase:
        // 1468.
        IF caseb<0 DO transreport(105, x)
        // 1478.
        out2(s.jump, endcaselabel)
        // 1485.
        ENDCASE   // -> 1642

      CASE s.switchon:
        // 1487.
        transswitch(x)
        // 1490.
        ENDCASE  // -> 1642

      CASE s.for:
        // 1492.
        transfor(x)
        // 1495.
        ENDCASE  // -> 1642

      CASE s.semicolon:
        // 1497.
        comcount := comcount-1
        // 1502.
        trans(h2!x)
        // 1505.
        x := h3!x
        // 1507.
        GOTO again     // -> 746

      CASE s.semicolonlist:
        // 1512.
        comcount := comcount - 1
        // 1517.
        FOR h = 2 TO h2!x+1 DO trans(h!x)
        // 1535.
        ENDCASE   // -> 1642
    }
  }
}

.

SECTION "TRN3"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/trnhdr.h"



LET declnames(x) BE
    // 1670.
    UNTIL x=0 SWITCHON h1!x INTO

     {  DEFAULT:
               // 1674.
               transreport(102, currentbranch)
               // 1681.
               BREAK   // ->1735

         CASE s.vecdef: CASE s.valdef:
               // 1683.
               decldyn(h2!x)
               // 1686.
               BREAK

         CASE s.rtdef: CASE s.fndef:
               // 1688.
               h5!x := nextparam()
               // 1693.
               declstat(h2!x, h5!x)
               // 1697.
               BREAK

         CASE s.and:
               // 1699.
               declnames(h2!x)
               // 1702.
               x := h3!x
               // 1704.    -> 1732
               LOOP
     }


AND decldyn(x) BE
{ // 1736.
  UNLESS x=0 DO
    // 1738.
    SWITCHON h1!x INTO
    { CASE s.name:
           // 1740.
           addname(x, s.local, ssp)
           // 1749.
           ssp := ssp + 1
           // 1754.
           ENDCASE     // -> 1822

       CASE s.comma:
           // 1756.
           addname(h2! x, s.local, ssp)
           // 1765.
           ssp := ssp + 1
           // 1770.
           decldyn(h3!x)
           // 1773.
           ENDCASE     // -> 1822

       CASE s.commalist:
           // 1775.
           FOR h = 2 TO h2!x+1 DO decldyn(h!x)
           // 1793.
           ENDCASE     // -> 1822

       DEFAULT:
           // 1795.
//sawritef("decldyn: x=%n -> [%n %n %n]*n", x, h1!x, h2!x, h3!x)
//abort(8878)
           transreport(103, x)
    }
}

AND declstat(x, l) BE
{ // 1824.
  LET t = cellwithname(x)
  // 1827.
  IF dvec!(t+1)=s.global DO
  { // 1835.
    LET n = dvec!(t+2)    //p6
    // 1841.
    addname(x, s.global, n)
//sawritef("declstat: globdecl=%n globdecls=%n globdeclt=%n*n", globdecl, globdecls, globdeclt)
//abort(3001)
    // 1849.
    IF globdecls+1>=globdeclt DO
    { // 1856.
      transreport(144, x)
    }
    // 1862.
    globdecl!globdecls := n
    // 1869.
    globdecl!(globdecls+1) := l
    // 1877.
    globdecls := globdecls + 2
    // 1882.
    RETURN
  }

  addname(x, s.fnlab, l)     // s.fnlab=39
}

AND decllabels(x) BE
{ // 1892.
  LET b = dvece
  // 1895.
  scanlabels(x)
  // 1898.
  checkdistinct(b, dvece)
}


AND checkdistinct(p, q) BE
  // 1906.
  FOR s = q-3 TO p BY -3 DO
  { // 1914.
    LET n = dvec!s
    // 1918.
    FOR r = p TO s-3 BY 3 DO
      // 1926.
      IF dvec!r=n DO transreport(142, n)
  }


AND addname(n, p, a) BE
{ // 1954.
  LET t = dvec+dvece   // t=p6
//sawritef("addname: Name %s p=%n a=%n dvec=%n dvece=%n dvect=%n*n",
//          @h3!n, p, a, dvec, dvece, dvect)
//abort(2001)
  // 1959.
  dvece := dvece + 3
//sawritef("addname: name=%s p-%n a=%n dvece=%n dvect=%n*n", @h3!n, p, a, dvece, dvect)
  // 1964.
  IF dvece>dvect DO   // -> 1975
  { // 1968.
//abort(1968)
    transreport(143, currentbranch)
    // 1975.
  }
  // 1975.
  h1!t, h2!t, h3!t := n, p, a
  // 1984.
  RETURN
}


AND cellwithname(n) = VALOF
{ // 1986.
  LET x = dvece
  // 1989.
  x := x - 3 REPEATUNTIL x=0 \/ dvec!x=n
  // 2001
  RESULTIS x
}


AND scanlabels(x) BE // 2004.
                     UNLESS x=0 DO
  // 2006.
  SWITCHON h1!x INTO
  { CASE s.colon:
      // 2008.
      h4!x := nextparam()
      // 2012.
      declstat(h2!x, h4!x)
      // 2017.

    CASE s.if: CASE s.unless: CASE s.while:
    CASE s.until: CASE s.switchon: CASE s.case:
      // 2017.
      scanlabels(h3!x)
      // 2020.
      ENDCASE   //   -> 2120.

    CASE s.semicolonlist:
      // 2022.
      FOR h = 2 TO h2!x+1 DO scanlabels(h!x)
      // 2040.
      ENDCASE   //   -> 2120.

    CASE s.semicolon:
      // 2042.
      scanlabels(h3!x)
      // 2045.
    CASE s.repeat: CASE s.repeatwhile:
    CASE s.repeatuntil: CASE s.default:
      // 2045.
      scanlabels(h2!x)
      // 2048.
      ENDCASE

    CASE s.test:
      // 2050.
      scanlabels(h3!x)
      // 2053.
      scanlabels(h4!x)
      // 2056.
      ENDCASE
  }




AND transdef(x) BE
{ // 2122.
//sawritef("transdef: calling transdyndefs(%n)*n", x)
  transdyndefs(x)
//sawritef("transdef: calling statdefs(%n)*n", x)
  // 2124.
  IF statdefs(x) DO
  { // 2129.
    LET l, s= nextparam(), ssp
//sawritef("transdef: statdefs(%n) op=%n returned TRUE, l=%n s=%n*n", x, h1!x, l, s)
//sawritef("transdef: calling out2(s.jump, %n)*n", l)
    // 2135.
    out2(s.jump, l)
//sawritef("transdef: calling transstatdefs(%n)*n", x)
    // 2141.
    transstatdefs(x)
    // 2144.
    ssp := s
//sawritef("transdef: calling out2(s.stack, %n)*n", ssp)
    // 2147.
    out2(s.stack, ssp)
//sawritef("transdef: calling out2(s.lab, %n)*n", l)
    // 2154.
    out2(s.lab, l)
    // 2160.
  }
//sawritef("transdef: returned from statdefs(%n)*n", x)
}


AND transdyndefs(x) BE
    // 2162.
    SWITCHON h1!x INTO
    { CASE s.and:
           // 2164.
           transdyndefs(h2!x)
           // 2167.
           x := h3!x
           // 2169.
           LOOP    //   -> 2162

       CASE s.vecdef:
           // 2171.
           out2(s.llp, vecssp)
           // 2178.
           ssp := ssp + 1
           // 2183.
           vecssp := vecssp + 1 + evalconst(h3!x)
           // 2193
           BREAK         //   -> 2222

       CASE s.valdef:
           // 2195.
           loadlist(h3!x)
           // 2198.
           BREAK

       DEFAULT:
           // 2200.
           BREAK

    } REPEAT

AND transstatdefs(x) BE
{ // 2224.
  WHILE h1!x=s.and DO
  { // 2226.
    transstatdefs(h2!x)
    // 2229.
    x := h3!x
    // 2231.
  }
//sawritef("transstatdefs: x=%n h1!x=%n*n", x, h1!x)
  // 2236.
  IF h1!x=s.fndef | h1!x=s.rtdef DO
  { // 2246.
    LET a, c = dvece, dvecp
    // 2252.
    AND bl, ll = breaklabel, looplabel
    // 2258.
    AND rl, cb = resultlabel, caseb
    // 2264.
    breaklabel, looplabel := -1, -1
    // 2269.
    resultlabel, caseb := -1, -1
    // 2273.

//sawritef("transstatdefs: calling compentry(%n, %n)*n", h2!x, h5!x)
    compentry(h2!x, h5!x)
    // 2278.
    ssp := savespacesize
    // 2282.

    dvecp := dvece
    // 2286.
//sawritef("transstatdefs: calling decldyn(%n)*n", h3!x)
    decldyn(h3!x)
//sawritef("transstatdefs: calling checkdistinct(%n, %n)*n", a, dvece)
    // 2289.
    checkdistinct(a, dvece)
//sawritef("transstatdefs: calling decllabels(%n)*n", h4!x)
    // 2295.
    decllabels(h4!x)
//sawritef("transstatdefs: calling out2(s.save, %n)*n", ssp)
    // 2298.
    out2(s.save, ssp)

    // 2305.
    TEST h1!x=s.fndef
    THEN { // 2310.
//sawritef("transstatdefs: calling load(%n)*n", h4!x)
           load(h4!x)
           // 2313.
//sawritef("transstatdefs: calling out1(s.fnrn)*n")
           out1(s.fnrn)
           // 2317.            -> 2326
         }
    ELSE { // 2319.
           trans(h4!x);
           // 2322.
           out1(s.rtrn)
         }
    // 2326.
//sawritef("transstatdefs: calling out2(s.endproc, 0)*n")
    out2(s.endproc, 0)
    // 2332.
    breaklabel, looplabel := bl, ll
    // 2338.
    resultlabel, caseb := rl, cb
    // 2344.
    dvece, dvecp := a, c
  }
  // 2350.
}

AND statdefs(x) = // 2352.
                  h1!x=s.fndef \/ h1!x=s.rtdef -> TRUE,
                  // 2365.
                  h1!x NE s.and -> FALSE,
                  // 2373.
                  statdefs(h2!x) -> TRUE,
                  // 2381.
                  statdefs(h3!x)
                  // 2384.

.

SECTION "TRN4"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/trnhdr.h"


LET jumpcond(x, b, lab) BE
{ // 2454.
  LET sw = b   // p6
  // 2456.
  UNLESS smallnumber(x) SWITCHON h1!x INTO
  { CASE s.false: // 2456.
                  b := NOT b
        
    CASE s.true: // 2466.
                 IF b DO out2(s.jump, lab)
                 // 2475.
                 RETURN

    CASE s.not: // 2476.
                jumpcond(h2!x, NOT b, lab)
                // 2484.
                RETURN

    CASE s.logand: // 2485. 
                   sw := NOT sw
    CASE s.logor:
      TEST // 2488.
           sw THEN { // 2491.
                     jumpcond(h2!x, b, lab)
                     // 2498.
                     jumpcond(h3!x, b, lab)
                     // 2505.   -> 2531
                   }

              ELSE { // 2507. 
                     LET m = nextparam()     // p7
                     // 2510.
                     jumpcond(h2!x, NOT b, m)
                     // 2518.
                     jumpcond(h3!x, b, lab)
                     // 2525.
                     out2(s.lab, m)
                     // 2531.
                   }
         // 2531.
         RETURN

    DEFAULT: // 2532.   -> 2560
    // 2534.  SWB and table
  }

  // 2560.
  load(x)
  // 2563.
  out2(b -> s.jt, s.jf, lab)
  // 2577.
  ssp := ssp - 1
  // 2582.
  RETURN
}

AND transswitch(x) BE
{ // 2584.
  LET p, b, dl = casep, caseb, defaultlabel  // p4  p5  p6
  // 2593.
  AND ecl = endcaselabel                     // p7
  // 2596.
  LET l = nextparam()                        // p8
  // 2599.
  endcaselabel := nextparam()
  // 2603.
  caseb := casep
  // 2607.
  out2(s.jump, l)
  // 2613.
  defaultlabel := 0
  // 2616.
  trans(h3!x)
  // 2619.
  out2(s.jump, endcaselabel)
  // 2626.
  out2(s.lab, l)
  // 2632.
  load(h2!x)
  // 2635.
  IF defaultlabel=0 DO defaultlabel := endcaselabel
  // 2643.
  out3(s.switchon, casep-p, defaultlabel)
  // 2655.
  FOR i = caseb TO casep-1 DO // 2664
                              out2(casek!i, casel!i)
  // 2684.
  ssp := ssp - 1
  // 2689.
  out2(s.lab, endcaselabel)
  // 2696.
  endcaselabel := ecl
  // 2699.
  casep, caseb, defaultlabel := p, b, dl
  // 2708.
  RETURN
  // 2709. NOP
}

AND transfor(x) BE
{ // 2710.
  LET a = dvece                          // p4
  // 2713.
  LET l, m = nextparam(), nextparam()    // p5  p6
  // 2719.
  LET bl, ll = breaklabel, looplabel     // p7  p8
  // 2725.
  LET k, n = 0, 0                        // p9  p10
  // 2728.
  LET step = 1                           // p11
  // 2730.
  LET s = ssp                            // p12
  // 2733.
  breaklabel, looplabel := 0, 0
  // 2738.
  load(h3!x)
  // 2743.
  k, n := s.ln, h4!x
  // 2748.
  UNLESS // 2748. 
         smallnumber(n) DO // -> 2775
    TEST // 2751
         h1!n=s.number   // -> 2764
    THEN { // 2759. 
           n := h2!n
           // 2762.  -> 2775
         }
    ELSE { // 2764.
           k, n := s.lp, ssp
           // 2770.
           load(h4!x)
           // 2775.
         }
  // 2775.
  addname(h2!x, s.local, s)
  // 2787.
  UNLESS // 2787.
         h5!x=0 DO // 2790.
                   step := evalconst(h5!x)
  // 2795.
  out1(s.store)
  // 2801.
  out2(s.jump, l)
  // 2810.
  decllabels(h6!x)
  // 2816.
  out2(s.lab, m)
  // 2825.
  trans(h6!x)
  // 2831.
  UNLESS looplabel=0 DO out2(s.lab, looplabel)
  // 2843.
  out2(s.lp, s); out2(s.ln, step)
  // 2861.
  out1(s.plus); out2(s.sp, s)
  // 2876
  out2(s.lab, l)
  // 2885.
  TEST step > 0              //  -> 2907
  THEN { // 2888.
         out2(s.lp,s)
         // 2897.
         out2(k,n)
         // 2905.   -> 2924
       }
  ELSE { // 2907.
         out2(k,n)
         // 2915.
         out2(s.lp,s)
         //2924.
       }
  // 2924.
  out2(s.endfor, m)
  // 2933.
  UNLESS breaklabel=0 DO out2(s.lab, breaklabel)
  // 2945.
  breaklabel, looplabel, ssp := bl, ll, s
  // 2954.
  out2(s.stack, ssp)
  // 2964.
  dvece := a
  // 2967.
  RETURN
}

.

SECTION "TRN5"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/trnhdr.h"

LET load(x) BE
{ // 2998.
  IF x=0 DO { // 3000.
              transreport(148, currentbranch)
              // 3007.
              loadzero()
              // 3009.
              RETURN
              // 3010.
            }
  // 3010.
  IF smallnumber(x) DO
  { // 3015.
    out2(s.ln, x)
    // 3021.
    ssp := ssp + 1
    // 3026.
    RETURN
  }
  // 3027.
  { LET op = h1!x
    // 3029.  -> 3410
    SWITCHON op INTO

    { CASE 38:   // Unused case constants to cause SWL to be used.
      CASE 29:
      CASE 27:
      CASE 26:
      CASE 18:
      DEFAULT:
        // 3031.
        transreport(147, currentbranch)
        // 3038.
        loadzero()
        // 3040.
        ENDCASE     //  -> 3496

      CASE s.byteap: // 3042.
                     op:=s.getbyte

      CASE s.div: CASE s.rem: CASE s.minus:
      CASE s.ls: CASE s.gr: CASE s.le: CASE s.ge:
      CASE s.lshift: CASE s.rshift:
        // 3045.
        load(h2!x)
        // 3048
        load(h3!x)
        // 3051.
        out1(op)
        // 3054.
        ssp := ssp - 1
        // 3059.
        ENDCASE          // -> 3496

      CASE s.vecap: CASE s.mult: CASE s.plus:
      CASE s.eq: CASE s.ne: CASE s.logand:
      CASE s.logor: CASE s.eqv: CASE s.neqv:
      { // 3061.
        LET a, b = h2!x, h3!x     // p5  p6
        //
 
        IF // 3061.
           smallnumber(a) |     // -> 3078
           // 3070.
           h1!a=s.name |        // -> 3078
           // 3074.
           h1!a=s.number DO     // -> 3082
        { // 3078.
          a, b := h3!x, h2!x // Make b (the right hand operand) a number is possible
        }
        // 3082.
        load(a)
        // 3085.
        load(b)
        // 3088.
        IF op=s.vecap DO   // -> 3098
        { // 3092.
          out1(s.plus)
          // 3096.
          op := s.rv
        }
        // 3098.
        out1(op)
        // 3101.
        ssp := ssp - 1
        // 3106.
        ENDCASE           // -> 3496
      }

      CASE s.neg: CASE s.not: CASE s.rv: CASE s.abs:
        // 3108.
        load(h2!x)
        // 3111.
        out1(op)
        // 3114.
        ENDCASE          // -> 3496

      CASE s.true: CASE s.false: CASE s.query:
        // 3116.
        out1(op)
        // 3119.
        ssp := ssp + 1
        // 3124.
        ENDCASE          // -> 3496

      CASE s.lv:
        // 3126.
        loadlv(h2!x)
        // 3129.
        ENDCASE          // -> 3496

      CASE s.number:
        // 3131.
        out2(s.ln, h2!x)
        // 33137.
        ssp := ssp + 1
        // 3142.
        ENDCASE          // -> 3496

      CASE s.string:
      { // 3144.
        out1(s.lstr)
        // 3148.
        outstring(@ h2!x)
        // 3152.
        ssp := ssp + 1
        // 3157.
        ENDCASE            // -> 3496
      }

      CASE s.name:
        // 3159.
        transname(x, s.lp, s.lg, s.ll, s.ln, s.lf)
        // 3177.
        ssp := ssp + 1
        // 3182.
        ENDCASE             // -> 3496

      CASE s.valof:
      { // 3184.
        LET rl = resultlabel      // p5
        // 3187
        LET a = dvece             // p6
        // 3190.
        decllabels(h2!x)
        // 3193.
        resultlabel := nextparam()
        // 3197.
        trans(h2!x)
        // 3200.
        out2(s.lab, resultlabel)
        // 3207.
        out2(s.rstack, ssp)
        // 3214.
        ssp := ssp + 1
        // 3219.
        dvece := a
        // 3222.
        resultlabel := rl
        // 3225.
        ENDCASE              // -> 3496
      }

      CASE s.fnap:
      { // 3227.
        LET s = ssp
        // 3230.
        ssp := ssp + savespacesize
        // 3234.
        out2(s.stack, ssp)
        // 3239.
        loadlist(h3!x)
        // 3242.
        load(h2!x)
        // 3245.
        out2(s.fnap, s)
        // 3250.
        ssp := s + 1
        // 3254.
        ENDCASE          // -> 3496
      }

      CASE s.cond:
      { // 3256.
        LET l, m = nextparam(), nextparam()   // p5  p6
        // 3262.
        LET s = ssp
        // 3265.
        jumpcond(h2!x, FALSE, m)
        // 3272.
        load(h3!x)
        // 3275.
        out2(s.res,l)
        // 3281.
        ssp := s; out2(s.stack, ssp)
        // 3291.
        out2(s.lab, m)
        // 3297.
        load(h4!x)
        // 3300.
        out2(s.res,l)
        // 3306.
        out2(s.lab, l)
        // 3312
        out2(s.rstack,s)
        // 3318.
        ENDCASE            // -> 3496
      }

      CASE s.table:
      { // 3320.
        LET m = nextparam()
        // 3322.
        LET a = h2!x
        // 3325.
        out2(s.lll, m)
        // 3331.
        out2(s.datalab, m)
        // 3337.
        ssp := ssp + 1
        // 3342.
        UNLESS smallnumber(a) DO          // -> 3400
        { // 3347.
          LET p, n = 0, 0
          // 3350.
          IF h1!a=s.comma DO p, n := a+1, 2
          // 3360.
          IF h1!a=s.commalist DO p, n := a+2, h2!a
          // 3370.
          UNLESS p=0 DO          // -> 3400
          { // 3373.
            FOR h = 0 TO n-1 DO  // -> 3394
              // 3380.
              out2(s.itemn, evalconst(h!p))
              // 3391.
            // 3398.
            ENDCASE         // -> 3496
          }
        }
        // 3400.
        out2(s.itemn, evalconst(a))
        // 3408.
        ENDCASE             // -> 3496
      }
      // 3410.  SWL 39 cases
    }
  }
}


AND loadlv(x) BE
{ // 3502.
  IF x=0 | smallnumber(x) GOTO err
  // 3511.
  SWITCHON h1!x INTO       // -> 3586

  { DEFAULT:
      // 3513.
      // NOP

err:  // 3514.
      transreport(113, currentbranch)
      // 3521.
      loadzero()
      // 3523.
      ENDCASE              // -> 3604

    CASE s.name:
      // 3525.
      transname(x, s.llp, s.llg, s.lll, 0, 0)
      // 3540.
      ssp := ssp + 1
      // 3545.
      ENDCASE              // -> 3604

    CASE s.rv:
      // 3547.
      load(h2!x)
      // 3550.
      ENDCASE              // -> 3604

    CASE s.vecap:
    { // 3552. 
      LET a, b = h2!x, h3!x
      // 3556.
      IF smallnumber(a) |     // -> 3565
         h1!a=s.name DO       // -> 3569
        // 3565.
        a, b := h3!x, h2!x
      // 3569.
      load(a)
      // 3572.
      load(b)
      // 3575.
      out1(s.plus) 
      // 3579.
      ssp := ssp - 1
      // 3584.
      ENDCASE           // -> 3604
    }
    // 3586.
    // SWB on 3 cases
  }
  // 3604.
  RETURN
}

// 3605.
// NOP

AND loadzero() BE
{ // 3606.
  out2(s.ln, 0)
  // 3612.
  ssp := ssp + 1
  // 3617.
  RETURN
}

AND loadlist(x) BE
{ // 3618.
  UNLESS x=0 DO // -> 3672
  { UNLESS // 3620.
           smallnumber(x) DO          // -> 3669
    { // 3624.
      LET p, n = 0, 0
      // 3627.
      IF h1!x=s.comma DO p, n := x+1, 2
      // 3637.
      IF h1!x=s.commalist DO p, n := x+2, h2!x
      // 3647.
      UNLESS p=0 DO                   // -> 3669
      { // 3650.
        FOR h = 0 TO n-1 DO load(h!p)
        // 3668.
        RETURN
      }
    }
    // 3669.
    load(x)
    // 3672.
  }
  // 3672.
  RETURN
}
// 3673.
// NOP

.

SECTION "TRN6"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/trnhdr.h"

LET evalconst(x) = VALOF
{ // 3708.
  LET a, b = 0, 0
  // 3711.
  IF x=0 DO { // 3714.
              transreport(117, currentbranch)
              // 3721.
              RESULTIS 0     // -> 4006
            }
  //3724.
  IF smallnumber(x) RESULTIS x
  // 3732.
  SWITCHON h1!x INTO   // -> 3789
  { DEFAULT:
      // 3734.
      transreport(118, x)
      // 3740.
      RESULTIS 0

    CASE s.name:
    { // 3743.
      LET t = cellwithname(x)     // p6
      // 3747.
      IF dvec!(t+1)=s.number RESULTIS dvec!(t+2)
      // 3761.
      transreport(119, x)
      // 3767
      RESULTIS 0
    }

    CASE s.number: // 3770. 
                   RESULTIS h2!x
    CASE s.true:   // 3773.
                   RESULTIS TRUE
    CASE s.query:
    CASE s.false:  // 3776.
                   RESULTIS FALSE

    CASE s.mult:   // dyadic operators
    CASE s.div:
    CASE s.rem:
    CASE s.plus:
    CASE s.minus:
    CASE s.lshift:
    CASE s.rshift:
    CASE s.logor:
    CASE s.logand:
    CASE s.eqv:
    CASE s.neqv:   // 3779.
                   b := evalconst(h3!x)

    CASE s.abs:    // monadic operators
    CASE s.neg:
    CASE s.not:    // 3783.
                   a := evalconst(h2!x)
                   // 3787.     -> 3872
    // 3789.
    // SWB 19 cases
  }

  // 3872.
  SWITCHON h1!x INTO               // -> 3943
  { CASE s.abs:   // 3874.
                  RESULTIS ABS a   // -> 4006
    CASE s.neg:   // 3880.
                  RESULTIS -a   // -> 4006
    CASE s.not:   // 3884.
                  RESULTIS ~a   // -> 4006

    CASE s.mult:  // 3888.
                  RESULTIS a * b   // -> 4006
    CASE s.div:   // 3893.
                  RESULTIS a / b   // -> 4006
    CASE s.rem:   // 3898.
                  RESULTIS a REM b   // -> 4006
    CASE s.plus:  // 3903.
                  RESULTIS a + b   // -> 4006
    CASE s.minus: // 3907.
                  RESULTIS a - b   // -> 4006
    CASE s.lshift:// 3912.
                  RESULTIS a << b   // -> 4006
    CASE s.rshift:// 3917.
                  RESULTIS a >> b   // -> 4006
    CASE s.logand:// 3922.
                  RESULTIS a & b   // -> 4006
    CASE s.logor: // 3927.
                  RESULTIS a | b   // -> 4006
    CASE s.eqv:   // 3932.
                  RESULTIS a EQV b   // -> 4006
    CASE s.neqv:  // 3938.
                  RESULTIS a NEQV b   // -> 4006
    // SWB 14 cases
  }
  // 4006.
  // RTN
}

// 4007.
// NOP

AND assign(x, y) BE
{ // 4008.
  IF x=0 |                        // -> 4017
     // 4010.
     smallnumber(x) |             // -> 4017
     // 4014.
     y=0 DO                       // -> 4025
  { // 4017.
    transreport(110, currentbranch)
    // 4024.
    RETURN
  }
  // 4025.
  SWITCHON h1!x INTO             // -> 4168
  { CASE s.comma:
    CASE s.commalist:
      // 4027.
      IF smallnumber(y) |       // -> 4036
         h1!x~=h1!y DO          // -> 4045
      { // 4036.
        transreport(112, currentbranch)
        // 4043.
        ENDCASE                 // -> 4198
      }

      { // 4045.
        LET l, n = h2, 2    // p5  p6
        // 4049.
        IF h1!x=s.commalist DO   // -> 4071
        { // 4054.
          l, n := h3, h2!x
          // 4056.
          UNLESS h2!y=n DO       // -> 4071
          { // 4062.
            transreport(112, currentbranch)
            // 4069.
            ENDCASE              // -> 4198
          }
        }
        // 
        FOR h = l TO l+n-1 DO   // -> 4091
          // 4079.
          assign(h!x, h!y)
      }
      // 4095.
      ENDCASE                   // -> 4198

    CASE s.name:
      // 4097.
      load(y)
      // 4100
      transname(x, s.sp, s.sg, s.sl, 0, 0)
      // 4115.
      ssp := ssp - 1
      // 4120.
      ENDCASE                  // -> 4198

    CASE s.byteap:
      // 4122.
      load(y)
      // 4125.
      load(h2!x)
      // 4128.
      load(h3!x)
      // 4131.
      out1(s.putbyte)
      // 4135.
      ssp:=ssp-3
      // 4140.
      ENDCASE                 // -> 4198

    CASE s.rv: CASE s.vecap:
      // 4142.
      load(y)
      // 4145.
      loadlv(x)
      // 4148.
      out1(s.stind)
      // 4152.
      ssp := ssp - 2
      // 4157.
      ENDCASE                 // -> 4198

    DEFAULT:
      // 4159.
      transreport(109, currentbranch)
      // 4177.           -> 4198
    //4168.
    // SWB 6 cases
  }
  // 4198.
  RETURN
}

// 4199.
// NOP

AND transname(x, p, g, l, n, f) BE
{ // This compile a name depending on how the name was declared.
  // s.local      use  p
  // s.global     use  g
  // s.label      use  l
  // s.numb       use  n  provided n is non zero
  // s.fnlab      use  f  provided f is non zero

  // There are only three call of transname depending on the context.

  // transname(s.lp,  s.lg,  s.ll, s.ln, s.lf)    if loading the Rvalue
  // transname(s.llp, s.llg, s.lll,   0,    0)    if loading the Lvalue
  // transname(s.sp,  s.sg,  s.sl,    0,    0)    if assigning to the name

  // 4202.
  LET t = cellwithname(x)           // t=p9
  // 4205.
  LET k, a = dvec!(t+1), dvec!(t+2) // k=p10  a=p11
  // 4216.
  LET op = g                        // op=p12   the Cintcode instruction
  // 4218.
  SWITCHON k INTO                 // -> 4274
  { DEFAULT:       // 4220.
                   transreport(115, x)
                   // 4229.
                   ENDCASE     // ->  4300

    CASE s.local:  // 4231.
                   IF t-dvecp<0 DO
                     // 4237.
                     transreport(116, x)
                   // 4246.
                   op := p

    CASE s.global: // 4248.
                   ENDCASE     //  -> 4300

    CASE s.label:  // 4250.
                   op := l
                   // 4252.
                   ENDCASE     //  -> 4300

    CASE s.fnlab:  // 5254.
                   n := f

    CASE s.number: // 4256.
                   TEST n=0
                   THEN // 4259. 
                        transreport(113, x)
                        // 4268.         -> 4272
                   ELSE // 4270.
                        op := n
                   // 4272.          -> 4300
    // 4274.
    // SWB 5 cases
  }

  // 4300.
  out2(op, a)
  // 4308.
  RETURN
}

// 4309.
// NOP

AND compentry(n, lab) BE  //  p3  p4
{ // 4310.
  LET s = @h3!n    // p5
  // 4312.
  LET len = s%0    // p6
  // 4315.
  out3(s.entry, len, lab)  // Based on BCPLTRN.map
  // 4322.
  FOR i = 1 TO len DO    // -> 4337
  { // 4328.
    LET ch = s%i   // p9
    // 4332.
    out1(ch)
    // 4334.
  }
  // 4341.
  RETURN
}

AND outstring(x) BE
{ // 4342.
  LET l = x%0
  // 4345.
  out1(l)
  // 4347.
  FOR i=1 TO l DO // 4353.
                  out1(x%i)
  // 4365.
  RETURN
}

AND out1(n) BE
{ // 4366.
  LET a = n>>7   // p4
  // 4369.
  TEST a               // -> 4382
  THEN { // 4371.
         out1pfx(a)
         // 4374.
         WRBIN(n & 127)
         // 4380.   -> 4385
       }
  ELSE { // 4382.
         WRBIN(n)
       }
  // 4385.
  RETURN
}

AND out1pfx(x) BE
{ // 4386.
  TEST x>=128                // -> 4404
  THEN { // 4390.
         out1pfx(x>>7)
         // 4396.
         WRBIN(x | 128)
         // 4402.   -> 4410
        }
  ELSE { // 4404.
         WRBIN(x | 128)
       }
  // 4410.
  RETURN
} 

// 4411.
// NOP

AND out2(x, y) BE
{ // 4412.
  out1(x)
  // 4414.
  out1(y)
  // 4417.
  RETURN
}

AND out3(x, y, z) BE
{ // 4418.
  out1(x)
  // 4420.
  out1(y)
  // 4423.
  out1(z)
  // 4426.
  RETURN
}

// 4427.
// NOP
