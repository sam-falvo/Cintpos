// This is a reconstruction of bcplsyn.b for the BBC BCPL System.
// When compiled by bbcbcpl using the 32-bit BCPL system it
// should generate an object file identical to bbccin/BCPLSYN.

// Reconstructed by Martin Richards (c) Mar 2017

// This reconstruction was made with the aid of oldcom/bcpl-syn.b
// and map/BCPLSYN.map created by: c df BCPLSYN.

// Comments such as // 1187: give byte addresses of positions
// in the object code. They refer to BCPL statements immediately
// below the comment.


SECTION "SYN1"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/synhdr.h"


// Start of the
// lexical abalyser.

LET nextsymb() BE
$(1 // 16:
    nlpending := FALSE

$(2 // Main loop
    // 19
    SWITCHON ch INTO
    $(s
     CASE '*C':
     CASE '*N':
       nlpending := TRUE

     CASE '*s':
     CASE '*t':
       // 24:
       rch() REPEATWHILE ch='*s'
       LOOP

     CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
     CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
       // 34:
       symb := s.number
       // 37.
       readnumber(10)
       // 40.
       RETURN

     CASE 'a':CASE 'b':CASE 'c':CASE 'd':CASE 'e':
     CASE 'f':CASE 'g':CASE 'h':CASE 'i':CASE 'j':
     CASE 'k':CASE 'l':CASE 'm':CASE 'n':CASE 'o':
     CASE 'p':CASE 'q':CASE 'r':CASE 's':CASE 't':
     CASE 'u':CASE 'v':CASE 'w':CASE 'x':CASE 'y':
     CASE 'z':
     CASE 'A':CASE 'B':CASE 'C':CASE 'D':CASE 'E':
     CASE 'F':CASE 'G':CASE 'H':CASE 'I':CASE 'J':
     CASE 'K':CASE 'L':CASE 'M':CASE 'N':CASE 'O':
     CASE 'P':CASE 'Q':CASE 'R':CASE 'S':CASE 'T':
     CASE 'U':CASE 'V':CASE 'W':CASE 'X':CASE 'Y':
     CASE 'Z':
       // 41:
       $( LET c = ch    // p3
          // 44.
          rch()
          // 46.
          rdtag(c)
          // 49.
          IF skipnode LOOP        // -> 19
          // 55.
          symb := lookupword()
          // 60.
          IF symb=s.get DO $( performget(); LOOP  $)
          // 67.
          RETURN
       $)

     CASE '{':
       // 68:
       symb := s.lsect
       // 72.
       rch()
       // 74.
       GOTO L128

     CASE '}':
       // 77:
       symb := s.rsect
       // 81.
       rch()
       // 83.
       GOTO L128

     CASE '$':
       // 86:
       multichar("()$<>[]{}", s.lsect, s.rsect,
                              s.setcond, s.lcond, s.rcond,
                              s.lsect, s.rsect, s.lsect, s.rsect, 0)
      // 120.
      IF symb=0 DO synreport(91)

L128: // 128:
      IF symb=s.lsect | symb=s.rsect DO
      $( // 138.
         rdtag('$')
         // 142.
         IF skipnode LOOP
         // 148.
         lookupword()
         // 150.
         RETURN
      $)

      $( // 151:
         LET op = symb
         // 154.
         rdtag('<')
         // 158.
         symb := lookupword()
         // 162.
         IF op=s.rcond DO
         $( // 167:
            IF skipnode=wordnode DO skipnode := 0
            // 176.
            LOOP
         $)

         // 178:
         IF skipnode LOOP

         // 184:
         IF op=s.setcond DO  //  -> 202
         $( h1!wordnode := // 189.
                           symb=s.true -> // 194.
                                          s.false,    // ->198
                                          // 197.
                                          s.true
                           // 198.   store A in h1!wordnode
            // 200.
            LOOP      //  -> 19
         $)

         // 202.
         IF symb=s.true LOOP
         // 209:
         skipnode := wordnode

         // 213:
         WHILE skipnode DO nextsymb()
         // 221.
         RETURN
     $)

     CASE '[':
     CASE '(':
       // 222:
       symb := s.lparen
       // 226
       BREAK                // -> 950

     CASE ']':
     CASE ')':
       // 228:
       symb := s.rparen
       // 232.
       BREAK

     CASE '#':
     $( // 234:
        LET radix = 8
        // 236.
        rch()
        // 238
        UNLESS '0'<=ch<='7' DO
        $( // 248.
           SWITCHON CAPCH(ch) INTO
           $( DEFAULT:  // 250.
                        synreport(33)
              CASE 'B': // 254.
                        radix := 2; ENDCASE
              CASE 'O': // 258.
                        radix := 8; ENDCASE
              CASE 'X': // 262.
                        radix := 16
           $)
           // 288:
           rch()
        $)
        //290
        readnumber(radix)
        // 293.
        symb := s.number
        // 296.
        RETURN
     $)

     CASE '?':
       // 297:
       symb := s.query
       // 301.
       BREAK

     CASE '+':
       // 303:
       symb := s.plus
       // 307.
       BREAK

     CASE ',':
       // 309:
       symb := s.comma
       // 313.
       BREAK

     CASE ';':
       // 315:
       symb := s.semicolon
       // 319.
       BREAK

     CASE '@':
       // 321:
       symb := s.lv
       // 324.
       BREAK

     CASE '&':
       // 326:
       symb := s.logand
       // 330.
       BREAK

     CASE '=':
       // 332:
       symb := s.eq
       // 336.
       BREAK

     CASE '!':
       // 338:
       symb := s.vecap
       // 341.
       BREAK

     CASE '%':
       // 343:
       symb := s.byteap
       // 347.
       BREAK

     CASE '**':
       // 349:
       symb := s.mult
       // 353.
       BREAK

     CASE '|':
       // 355:
       multichar("|", 0, s.logor)
       // 365:
       UNLESS symb=0 RETURN

cmnt:  // 370:
       UNTIL ch='*N' | ch='*C' | ch='*P' | ch=ENDSTREAMCH DO rch()
       // 392:
       LOOP

     CASE '/':
       // 394:
       multichar("\/**", s.logand, 0, -1, s.div)
       // 409:
       IF symb>0 RETURN
       // 414:
       IF symb=0 GOTO cmnt   // -> 370

       $( // 421:
          IF ch='**' DO
          $( // 427:
             rch()
             // 429.
             IF ch='/' BREAK      // -> 452
             // 437:
             LOOP        // -> 421
          $)
          // 439:
          IF ch=ENDSTREAMCH DO synreport(63)
          // 448.
          rch()
          // 450.
       $) REPEAT    // -> 421

       // 452:
       rch()
       // 454.
       LOOP       // -> 19

     CASE '~':
       // 456:
       multichar("=", s.ne, s.not)
       // 467.
       RETURN

     CASE '\':
       // 468:
       multichar("/=", s.logor, s.ne, s.not)
       // 482.
       RETURN

     CASE '<':
       // 483:
       multichar("=<", s.le, s.lshift, s.ls)
       // 497.
       RETURN

     CASE '>':
       // 498:
       multichar("=>", s.ge, s.rshift, s.gr)
       // 512.
       RETURN

     CASE '-':
       // 516:
       multichar(">", s.cond, s.minus)
       // 527.
       RETURN

     CASE ':':
       // 528:
       multichar("=", s.ass, s.colon)
       // 539.
       RETURN

     CASE '"':
     $( // 540:
        LET i = 0
        // 542.
        rch()

        // 544:
        UNTIL ch='"' DO
        $( // 546:
           IF i=255 DO synreport(34)
           // 555:
           i := i + 1
           // 558:
           wordv%i := rdstrch()
           // 566.
        $)

        // 572:
        wordv%0 := i
//sawritef("string len=%n ", i)
//FOR j = 0 TO i / bytesperword DO writef("%i3: %x8*n", j, wordv!j)
//sawritef("*n")
//abort(2003)
        // 579:
        symb := s.string
        // 582.
        BREAK          // -> 950
     $)

     CASE '`':
     CASE '*'':
       // 584:
       rch()
       // 586.
       decval := rdstrch()
       // 590.
       symb := s.number
       // 593.
       UNLESS ch='*'' | ch='`' DO synreport(34)
       // 607.:
       BREAK        // -> 950

     // 609:
     CASE '_':
     CASE '^':
     CASE 31:
     CASE 30:
     CASE 29:
     CASE 28:
     CASE 27:
     CASE 26:
     CASE 25:
     CASE 24:
     CASE 23:
     CASE 22:
     CASE 21:
     CASE 20:
     CASE 19:
     CASE 18:
     CASE 17:
     CASE 16:
     CASE 15:
     CASE 14:
     CASE 12:
     CASE 11:
     CASE 8:
     CASE 7:
     CASE 6:
     CASE 5:
     CASE 4:
     CASE 3:
     CASE 2:
     CASE 1:
     CASE 0:

     DEFAULT:
       // 609:
       ch := '*S'
       // 613.
       synreport(94)

     CASE '.':
     CASE ENDSTREAMCH:
       // 617:
       IF getp=0 DO
       $( // 621.
          symb := s.end
          // 625.
          BREAK         // -> 632
       $)
       // 632:

       // Indirect resolving words

       // 634:
       ENDREAD()
       // 636.
       skipnode := 0
       // 639.
       getp := getp - 3
       // 644.
       sourcestream := getv!getp
       // 649.
       SELECTINPUT(sourcestream)
       // 651.
       linecount := getv!(getp+1)
       // 663.
       ch := getv!(getp+2)
       // 671:
       LOOP             // ->19
   $)s

$)2 REPEAT

    // 950:
    rch()
    // 952.
$)1

AND multichar(chars, a, b, c, d, e, f, g, h, i, j) BE
$( // 1000.
   LET t = @chars
   // 1003.
   LET i, lim = 1, chars%0
   // 1009.
   rch()

   // 1013.             -> 1031
   UNTIL i>lim DO
   $( // 1015;
      IF ch=chars%i DO    // -> 1028
      $( // 1022:
         rch()
         // 1026.
         BREAK      // 1035
      $)
      // 1028:
      i := i+1
      // 1031.
   $)
   // 1035:
   symb := t!i
   // 1041.
$)

.

SECTION "SYN2"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/synhdr.h"

LET lookupword() = VALOF
$(1 // 1068:
    LET hashval = VALOF       // p3
    $( // 1068:
       LET res = wordv%0      // p3
       // 1073:
       FOR i = 1 TO res DO    // i=p4  lim=p5
         // 1079:
         res := (res+res+res + CAPCH(wordv%i))       // 1098:
       RESULTIS res & 127     // The size of the nametable is 128 words.
    $)

    // 1103;
    LET i = 0
    // 1105;
    wordnode := nametable!hashval
    // 1110;
    UNTIL wordnode=0 |
          compstring(wordnode+2, wordv)=0 DO
      // 1112:
      wordnode := h2!wordnode

    // 1129;
    IF wordnode=0 DO
    $( // 1133:
       LET wordsize = wordv%0 >> 1
       // 1140:
       wordnode := newvec(wordsize+2)
       // 1145:
       wordnode!0 := s.name
       // 1150;
       wordnode!1 := nametable!hashval
       // 1154;
       FOR i = 0 TO wordsize DO
         // 1160:
         wordnode!(i+2) := wordv!i
       // 1176;
       nametable!hashval := wordnode
    $)
//sawritef("%i5 -> Name node: %s  symb = %n*n", wordnode, @h3!wordnode, h1!wordnode)
    // 1181:
    RESULTIS h1!wordnode
$)1

AND compstring(s1, s2) = VALOF
$( // Return 0 if strings s1 and s2 are equal ignoring case
   // return 2 otherwise.
   // 1184:
   LET len1 = s1%0
   // 1187:
   UNLESS len1=s2%0 RESULTIS 2
   // 1196:
   FOR i = 1 TO len1 DO
     // 1202:
     UNLESS CAPCH(s1%i)=CAPCH(s2%i) RESULTIS 2

   // 1226:
   RESULTIS 0
$)

AND declsyswords() BE
$(1 // 1228.
    symb := TABLE
      // 1248:
      s.and,s.abs,
      s.be,s.break,s.by,
      s.case,
      s.do,s.default,
      s.eq,s.eqv,s.or,s.endcase,
      s.false,s.for,s.finish,
      s.goto,s.ge,s.gr,s.gr,s.global,s.get,
      s.if,s.into,
      s.let,s.lv,s.le,s.ls,s.ls,s.logor,
          s.logand,s.loop,s.lshift,
      s.manifest,
      s.ne,s.query,s.not,s.neqv,s.needs,
      s.or,
      s.resultis,s.return,s.rem,s.rshift,s.rv,
      s.repeat,s.repeatwhile,s.repeatuntil,
      s.switchon,s.static,s.section,
      s.to,s.test,s.true,s.do,s.table,
      s.until,s.unless,
      s.vec,s.valof,
      s.while,
      0

    // 1232:
    d("AND/ABS/*
      *BE/BREAK/BY/*
      *CASE/*
      *DO/DEFAULT/*
      *EQ/EQV/ELSE/ENDCASE/*
      *FALSE/FOR/FINISH/*
      *GOTO/GE/GR/GT/GLOBAL/GET/*
      *IF/INTO/*
      *LET/LV/LE/LS/LT/LOGOR/LOGAND/LOOP/LSHIFT//")

    // 1234:
    d("MANIFEST/*
      *NE/NIL/NOT/NEQV/NEEDS/*
      *OR/*
      *RESULTIS/RETURN/REM/RSHIFT/RV/*
      *REPEAT/REPEATWHILE/REPEATUNTIL/*
      *SWITCHON/STATIC/SECTION/*
      *TO/TEST/TRUE/THEN/TABLE/*
      *UNTIL/UNLESS/*
      *VEC/VALOF/*
      *WHILE/*
      *$//")

     // 1242:
     nulltag := wordnode
$)1

AND d(words) BE
$(1 // 1702:
    LET i, length = 1, 0
    $( // 1706:
       LET ch = words%i
       // 1710:
       TEST ch='/'
           THEN $( // 1715:
                   IF length=0 RETURN
                   wordv%0 := length
                   lookupword()
                   // 1727:
                   h1!wordnode := !symb
//sawritef("%i5 -> Name node: %s  symb = %n*n", wordnode, @h3!wordnode, h1!wordnode)
//abort(1115)
                   // 1731:
                   symb := symb + 1
                   length := 0
                $)
           ELSE $( // 1740:
                   length := length + 1
                   wordv%length := ch
                $)
       // 1750:
       i := i + 1
    $) REPEAT
$)1

.

SECTION "SYN3"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/synhdr.h"

LET rch() BE
{ { // 1788.
    ch := RDCH()

    IF // 1792.
      ch>=32 BREAK    // -> 1833
      
    IF // 1796.
      ch='*N' |
      ch='*P' |
      ch='*C' DO      // -> 1820
    { // 1810.
      ch := '*N'
      // 1813.
      linecount := linecount+1
      // 1818.
      BREAK           // -> 1833
    }

    IF // 1820.
      ch<0 BREAK      // -> 1833
    IF // 1824.
      ch='*T' BREAK   // -> 1833
    IF // 1827.
      ch=0 BREAK      // -> 1833
    // 1831.   -> 1788 
  } REPEAT

  // 1833.
  chcount := chcount + 1
  // 1838.
  chbuf!(chcount&63) := ch
  // 1847.
  // RTN
}

/* Original attempt
LET rch() BE
{ { // 1788.
    ch := RDCH()
    // 1792.
    //IF ch<32 DO // -> 1820    wrong but compiles to the right size
    IF ch>=32 BREAK // -> 1833  Using more optimisation
    { // 1796.
      IF ch='*N' | ch='*P' | ch='*C' DO   // -> 1820
      { // 1810.
        ch := '*N'
        // 1813.
        linecount := linecount+1
        // 1818.
        BREAK  // -> 1833
      }
      // 1820.
    }
    IF // 1820.
       ch<=0   BREAK   // -> 1833    comcount=17
    IF // 1824.
       ch='*T' BREAK   // -> 1833    comcount=18
       // 1827.
    IF ch=0    BREAK   // -> 1833    comcount=19
    // 1831.
    LOOP               // -> 1788 
  } REPEAT
   
  // 1833.
  chcount := chcount + 1
  // 1838.
  chbuf!(chcount&63) := ch
  // 1847.
  // RTN
}
******************/

AND wrchbuf() BE
$( // 1848:
   WRITES("*N...")
   // 1852:
   FOR p = chcount-63 TO chcount DO
   $( // 1862;
      LET k = chbuf!(p&63)
      // 1870:
      IF k>0 DO WRCH(k)
   $)
   // 1881:
   NEWLINE()
$)

AND rdtag(char) BE
$( // 1890:
   LET i = 1
   // 1892:
   wordv%i := char

   $( UNLESS // 1899:
             'A'<=ch<='Z' |
             // 1909.
             'a'<=ch<='z' |
             // 1919.
             '0'<=ch<='9' |
             // 1929.
             ch='.'       |
             FALSE        BREAK         // -> 1952
      // 1937:
      i := i+1
      // 1940:
      wordv%i := ch
      // 1948:
      rch()
   $) REPEAT

   // 1952:
   wordv%0 := i
$)

AND performget() BE
$( // 1960:
   LET s = 0      // p3
   // 1962:
   nextsymb()
   // 1964;
   UNLESS symb=s.string & getp+2<=getmax DO    // getmax=20
     // 1976:
     synreport(97)
   // 1980:
   UNLESS maxoption DO
   { //1984.
     IF FILENAME(wordv, 0)=0 &
        // 1992.
        FINDSTFILE(wordv)=0 DO    // -> 2020
     { // 1998.
       READ(wordv, 0, 0)
       // 2005.
       { LET filevec = FINDSTFILE(wordv)   //filevec=p4
         // 2010.
         IF filevec DO
         { // 2012.
           filevec!-1 := filevec!-1 & #x7FFF
           // 2020.
         }
       }
     }
   }
   // 2020.
   s := FINDINPUT(wordv)
   // 2025.
   UNLESS s DO       // 2034
   { // 2027.
     synreport(96, wordv)
   }
   // 2034.
   getv!getp := sourcestream
   // 2042.
   getv!(getp+1) := linecount
   // 2051.
   getv!(getp+2) := ch
   // 2060.
   getp := getp + 3
   // 2065.
   linecount := 1
   // 2068.
   sourcestream := s
   // 2071.
   SELECTINPUT(s)
   // 2073.
   rch()
   // 2075.
   RETURN
$)

AND readnumber(radix) BE
$( // 2076.
   LET d = value(ch)
   // 2082.
   decval := d
   // 2094
   IF d>=radix DO synreport(33)

   $( // 2091.
      rch()
      //2093.
      d := value(ch)
      // 2099.
      IF d>=radix RETURN
      // 2103.
      decval := radix*decval + d
      // 2110.           ->2091
   $) REPEAT
$)


AND value(ch) = VALOF
$( // 2112.
   LET c = CAPCH(ch)
   // 2115.
   RESULTIS '0'<=c<='9' -> c-'0',
            'A'<=c<='F' -> c-'A'+10,
            100
$)

AND rdstrch() = VALOF
{ { // 2148.
    LET k = ch
    // 2151.
    rch()
    // 2153.
    IF k='*N' DO synreport(34)
    // 2161.
    IF k='**' DO  // -> 2347
    { // 2166.
      IF ch='*N' | ch='*S' | ch=13 | ch='*T' DO
      { // 2184.
        rch() REPEATWHILE // 2186.
                          ch='*S' | ch='*N' | ch='*C' | ch='*T'
        // 2204.
        UNLESS ch='**' DO synreport(34)
        // 2213.
        rch()
        // 2215.
        LOOP   //  -> 2148
      }
      // 2217.
      k := ch
      // 2220.
      ch := CAPCH(ch)
      // 2224.
      IF ch='T' DO k := '*T'
      // 2230.
      IF ch='S' DO k := '*S'
      // 2239.
      IF ch='N' DO k := '*N'
      // 2247.
      IF ch='E' DO k := '*E'
      // 2256.
      IF ch='B' DO k := '*B'
      // 2264.
      IF ch='C' DO k := '*C'
      // 2273.
      IF ch='P' DO k := '*P'
  
      TEST // 2282.
           ch='X' |
           // 2288.
           ch='O' |
           // 2292.
          '0'<=ch<='9'
      THEN { // 2302.
             LET r, n = 8, 3
             // 2306.
             IF ch='O' DO rch()
             // 2314.
             IF ch='X' DO
             { // 2320.
               r, n := 16, 2
               // 2325.
               rch()
               // 2327.
             }
             // 2327.
             k := readoctalorhex(r, n)
             // 2334.
             IF k>255 DO synreport(34)
             // 2342.
             RESULTIS k  // don't translate *Xnn or *nnn
           }
      ELSE { // 2345.
              rch()
           }
    }
    // 2347.
    RESULTIS k
    // 2352.
  } REPEAT
  RETURN
}

AND readoctalorhex(radix,digits) = VALOF
$( // 2352.
   LET answer = 0
   // 2354.
   FOR j = 1 TO digits DO
   $( // 2360.
      LET valch = value(ch)
      // 2366.
      IF valch>=radix DO synreport(34)
      // 2373.
      answer:=answer*radix + valch
      // 2378.
      rch()
      //2380.
   $)
   // 2387
   RESULTIS answer
$)

.

SECTION "SYN4"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/synhdr.h"

LET start() =  VALOF
$(1 // 2438.
    LET a = 0    // p3
    // 2440.
    nametable!0 := 0
    // 2442.
    MOVE(nametable, nametable+1, 192)  // Clear vector nametable and chbuf
                                       // 192 = 128 +    nametable
                                       //        64      chbuf
    // 2453.
    chcount := 0
    // 2456.
    getp := 0
    // 2458.
    listp := gett
    // 2462. 
    //err.p, err.l := LEVEL(), exit    // g256    g257
    err.p, err.l := LEVEL(), fail    // g256    g257
    // 2470.
    skipnode := 0
    // 2473.
    blk := GETVEC(129)
    // 2479.
    h1!blk := 0
    // 2482.
    blklist := @blk
    // 2486.
    blkt := blk + 129      // blkt=g292  pointer to last word of blk
    // 2492.
    blkp := blk+1          // blkp=g293  pointer to first word of blk after the link word.
    // 2497.
    zeronode := list2(s.number, 0)   // zeronode=g295
    // 2504.
    declsyswords()
    // 2506.
    rch()
    //2508.
    IF ch=ENDSTREAMCH GOTO exit    // -> 2660
    // 2518.
    rec.p, rec.l := err.p, reclab   // rec.p=g461  rec.l=g461  err.p=g256 reclab=L2524

reclab:
    // 2524.
    nextsymb()
    // 2526.                   -> 2594

//sawritef("reclab: symb=%n*n", symb)
//abort(1114)
    $( LET rdsectionorneeds() = VALOF
       $( // 2528.
          LET op, a, b = symb, 0, 0   // p3  p4  p5
          // 2534.
          nextsymb()
          // 2536.
          UNLESS symb=s.string DO synreport(95)
          // 2545.
          a := rbexp()
          // 2548.
          IF op=s.section DO
             // 2553.
             WRITEF("Section %S*n", a+1)
//sawritef("rdsectionorneeds: a=%n op=%n*n", a, op)
//abort(7349)
          // 2560
          b := symb = s.needs -> rdsectionorneeds(), rdblockbody()
          RESULTIS list3(op, a, b)
          // 2582.
       $)

       // 2594.
       a := symb=s.section | symb=s.needs -> rdsectionorneeds(),
           rdblockbody()
       // 2612.
       UNLESS symb=s.end DO
       { // 2618.
         synreport(99)
         // 2622.
       }
    $)

    // 2622.       -> 2626
    UNTIL // 2626.
          ch='*n' |           // -> 2634
          ch=endstreamch DO
    { // 2624.
      rch()
      // 2626.
    }

    // 2634.
    rch() REPEATWHILE // 2636.
                      ch='*s' |    // -> 2634
                      // 2642.
                      ch='*t' |    // -> 2634
                      // 2645.
                      ch='*n'      // -> 2634
    // 2649.
    UNLESS ch=ENDSTREAMCH DO UNRDCH()
    // 2655.
    WRITES("Text read*n")
    // 2659.
    // NOP
fail:  // XLAB L14
exit:  // XLAB L15
//sawritef("Returning from start in bcplsyn with result %n*n", a)
    // 2660.
    RESULTIS a
$)1


AND newvec(n) = VALOF
{ // 2674.
  IF blkp+n<blkt DO // -> 2689
  { // 2680.
    blkt := blkt-n-1
    // 2685
    RESULTIS blkt
  }
  // 2687.
  TEST n > 13    // -> 2710
  THEN { // 2692.
         LET p = GETVEC(n+1)   // p4
         // 2697.
         h1!p := blkp-1
         // 2701.
         h1!blklist := p
         // 2704
         blklist := p
         // 2707.
         RESULTIS p+1    // -> 2738
       }
   
  ELSE { // 2710.
         LET p = GETVEC(129)
         // 2715.
         h1!p := 0
         // 2717.
         blklist := blkp-1
         // 2722.
         h1!blklist := p
         // 2725.
         blkp := p+1
         // 2729.
         blkt := p+129-n
         // 2738.
         RESULTIS blkt
       }
}

AND list1(x) = VALOF
$( // 2740.
   LET p = newvec(0)
   // 2744.
   p!0 := x
   // 2746.
//sawritef("%i5 -> [%n]*n", p, x)
   RESULTIS p
$)

AND list2(x, y) = VALOF
$( // 2748.
   LET p = newvec(1)
   // 2752.
   p!0, p!1 := x, y
   // 2758.
//sawritef("%i5 -> [%n %n]*n", p, x, y)
   RESULTIS p
$)

AND list3(x, y, z) = VALOF
$( // 2760.
   LET p = newvec(2)
   // 2764.
   p!0, p!1, p!2 := x, y, z
   // 2773.
//sawritef("%i5 -> [%n %n %n]*n", p, x, y, z)
   RESULTIS p
$)

AND list4(x, y, z, t) = VALOF
$( // 2776.
   LET p = newvec(3)
   // 2780.
   p!0, p!1, p!2, p!3 := x, y, z, t
   // 2792.
//sawritef("%i5 -> [%n %n %n %n]*n", p, x, y, z, t)
   RESULTIS p
$)

AND list5(x, y, z, t, u) = VALOF
$( // 2794.
   LET p = newvec(4)
   // 2798.
   p!0, p!1, p!2, p!3, p!4 := x, y, z, t, u
   // 2815.
//sawritef("%i5 -> [%n %n %n %n %n]*n", p, x, y, z, t, u)
   RESULTIS p
$)

AND list6(x, y, z, t, u, v) = VALOF
$( // 2818.
   LET p = newvec(5)
   // 2822.
   p!0, p!1, p!2, p!3, p!4, p!5 := x, y, z, t, u, v
   // 2844.
//sawritef("%i5 -> [%n %n %n %n %n %n]*n", p, x, y, z, t, u, v)
   RESULTIS p
$)


AND makelist(k, n) = VALOF
$( // 2846.
   LET p = newvec(n+1)    // p5
   // 2851.
   h1!p := k    // Typically s.commalist
   // 2854.
   h2!p := n    // Number of elements in the list
//sawritef("makelist: p=%n k=%n n=%n*n", p, k, n)
//FOR a = listp TO listp+n-1 DO
//  sawritef("%i5: %i5*n", a, !a) 
   // 2857.
   //FOR i = 0 TO n-1 DO p!(i+2) := listp!i
   MOVE(listp, p+2, n)  // Copy n items from listp to h3!p...
//sawritef("makelist: listp=%n*n", listp)
//sawritef("%i5 -> [%n %n", p, k, n)
//abort(2003)
//FOR i = 2 TO n+1 DO sawritef(" %n", p!i)
//sawritef("]*n")
//IF k=s.commalist DO
  //abort(8879)
   // 2866.
   RESULTIS p
$)

AND synreport(n, a) BE
$( // 2868.
   LET s = 0     // p5
   // 2870.
   s := VALOF SWITCHON n INTO
   $( DEFAULT: // 2872.
               a := n
               // 2874.
               RESULTIS "Error %N"
      CASE  6: // 2878
               RESULTIS "$( expected"
      CASE  7: // 2882.
               RESULTIS "$) expected"
      CASE  8:CASE 40:CASE 43:
               // 2886.
               RESULTIS "Name expected"
      CASE  9: // 2890.
               RESULTIS "Untagged $) mismatch"
      CASE 15:CASE 19:CASE 41:
               // 2894.
               RESULTIS ") missing"
      CASE 30: // 2898.
               RESULTIS "Bad condition"
      CASE 32: // 2902.
               RESULTIS "Bad expression"
      CASE 33: // 2906.
               RESULTIS "Bad number"
      CASE 34: // 2910.
               RESULTIS "Bad string*
                        * or character constant"
      CASE 42: // 2914.
               RESULTIS "Bad procedure heading"
      CASE 44:CASE 45:
               // 2918.
               RESULTIS "Bad declaration"
      CASE 50: // 2922.
               RESULTIS "Unexpected :"
      CASE 51: // 2926.
               RESULTIS "Bad command"
      CASE 54: // 2930.
               RESULTIS "ELSE expected"
      CASE 57:CASE 58:
               // 2934.
               RESULTIS "Bad FOR loop"
      CASE 60: // 2938.
               RESULTIS "INTO expected"
      CASE 61:CASE 62:
               // 2942.
               RESULTIS ": expected"
      CASE 63: // 2946.
               RESULTIS "**/ missing"
      CASE 91: // 2950.
               RESULTIS "Unexpected $"
      CASE 94: // 2954.
               RESULTIS "Bad character"
      CASE 95: // 2958.
               RESULTIS "Bad section name"
      CASE 96: // 2962.
               RESULTIS "Cannot GET %S"
      CASE 97: // 2966.
               RESULTIS "Bad GET directive"
      CASE 98: // 2970.
               RESULTIS "Program too large"
      CASE 99: // 2974.
               RESULTIS "Incorrect termination"
    $)

   // 3118.
   rc := n=96 -> RESULT2, 40
   // 3132.
   reportcount := reportcount+1
   // 3137.
   WRITEF("*NError near line %N:*n", linecount)
   // 3144.
   WRITEF(s, a)
   // 3149.
   wrchbuf()
   // 3151.
   IF n=1 | n=96 | n=98 DO    //   -> 3171   
   { // 3164.
     WRITES("*nFATAL ERROR*n")
     // 3168.
     GOTO fin  // L3226
   }

   // 3171.
   IF reportcount>reportmax DO   //   -> 3183
   $( // 3176.
      WRITES("*NTOO MANY ERRORS*n")
      // 3180.
      GOTO fin    // -> 3226
   $)

   // 3183.
   nlpending := 0

   // 3186.                   -> 3190
   UNTIL // 3190.
         symb=s.lsect |
         // 3196.
         symb=s.rsect |
         // 3200.
         symb=s.let |
         // 3205.
         symb=s.and |
         // 3210.
         symb=s.end |
         nlpending DO
   { // 3188.
     nextsymb()
   }   // 3219.
   LONGJUMP(rec.p, rec.l)
   // 3226.        -> 3242
fin:
   UNTIL getp=0 DO
   { // 3228.
     ENDREAD()
     // 3230.
     getp := getp-3
     // 3235.
     sourcestream := getv!getp
     // 3240.
     SELECTINPUT(sourcestream)
     // 3242. if getp~=0 J 3228
   }

   // 3246.
   LONGJUMP(err.p, err.l)
   // 3253.
   RETURN
   // 3254.

// Many string constants and resolving word
// followed by global initalisation data
$)

.

SECTION "SYN5"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/synhdr.h"

LET rdblockbody() = VALOF
$(1 // 3832.
    LET rp, rl = rec.p, rec.l    // p3  p4
    // 3838.
    LET a = 0                    // p5
    // 3940.
    LET ptr = @a                 // p6
//sawritef("rdblockbody entered, symb=%n*n", symb)
//abort(1113)
    $( // 3843.
       LET op = 0                // p7
       // 3845.
       rec.p, rec.l := LEVEL(), recover
       // 3853.
       ignore(s.semicolon)
       // 3857.    -> 3931
       SWITCHON symb INTO
       $(  CASE s.manifest:
           CASE s.static:
           CASE s.global:
                   // 3859.
                   op := symb
//sawritef("rdblockbody: op=%n calling nextsymb()*n", op)
                   // 3862.
                   nextsymb()
//sawritef("rdblockbody: calling rdsects(rdcdefs)*n", op)
                   // 3864.
                   !ptr := rdsect(rdcdefs)
//sawritef("rdblockbody: returned from rdsects(rdcdefs)*n", op)
                   // 3870.
                   ENDCASE     //  -> 3962

           CASE s.let:
//sawritef("LET just read, calling nextsymb()*n")
                   // 3872.
                   nextsymb()
//sawritef("LET just read, calling rdef()*n")
                   // 3874.
                   !ptr := rdef()
       recover:
                $( // 3878.
                   LET qtr = ptr              // p8
                   WHILE symb=s.and DO     // -> 3900
                   $( // 3882.
                      nextsymb()
                      // 3884.
                      !qtr := list3(s.and, !qtr, rdef())
                      // 3897.
                      qtr := @h3!(!qtr)
                      // 3900
                   $)
                   // 3906.
                   op := s.let
                   // 3909.
                   ENDCASE       //  -> 3962
                $)

           DEFAULT:// 3911.
                 { LET dummy = ?
                   !ptr := rdseq()
                   // 3915.
                   UNLESS symb=s.rsect | symb=s.end DO
                             synreport(51)
                   // 3929.
                 }

           CASE s.rsect: CASE s.end:
                   // 3929.
                   BREAK                  //  -> 3976
        $)

       // 3962.
       !ptr := list3(op, !ptr, 0)
       // 3971.
       ptr := @h3!(!ptr)
       // 3974.           -> 3843
    $) REPEAT

    // 3976.
    rec.p, rec.l := rp, rl
//sawritef("Returning from rdblockbody with result %n*n", a)
//abort(1116)
    // 3982.
    RESULTIS a
$)1

AND rdseq() = VALOF
$( // 3984.
   LET n = 0
   // 3986.
   LET q = listp     // q=p4  listp=g464
   // 3989.
   LET rp, rl = rec.p, rec.l   // p5  p6
   // 3995.
   rec.p, rec.l := LEVEL(), rec
   // 4003.

   $( // 4003.
      // NOP
mklist:
      // 4004.
      ignore(s.semicolon)
      // 4008.
      !listp := rcom()
      // 4012.
      listp, n := listp+1, n+1   // g464  p3
      // 4017.
   $) REPEATUNTIL // 4020.
                  symb=s.rsect |   //  -> 4030
                  symb=s.end       //  -> 4003

mkseq:
   // 4030.
   rec.p, rec.l := rp, rl
   // 4036.
   listp := q
   // 4039.
   IF n=1 RESULTIS !listp     //  -> 4094
   // 4047.
   IF n=2 RESULTIS list3(s.semicolon, listp!0, listp!1)   //  -> 4094
   // 4063.
   RESULTIS makelist(s.semicolonlist, n)  //  -> 4094

   // 4071.
   // NOP

rec: // Only reached if there was a syntax error.

   // 4072.
   IF symb=s.rsect |
      symb=s.end   DO // 4082.
                      GOTO mkseq
   // 4085.
   rec.p, rec.l := rp, rl
   // 4091.
   GOTO mklist

   // 4094.
   RETURN 
$)


AND rdcdefs() = VALOF
$( // 4096.
   LET n = 0                   // p3
   // 4098.
   LET rp, rl = rec.p, rec.l   // p4  p5
   // 4104.
   LET p = listp               // p6
   // 4107
   rec.p, rec.l := LEVEL(), rec

   $( // 4115.
//sawritef("rdcdefs: calling rname()*n")
      !listp := rname()
//sawritef("rdcdefs: returned from rname, listp=%n*n", listp)
//abort(2004)
      // 4119.
      listp := listp+1
      // 4124.
      UNLESS symb=s.eq | symb=s.colon DO
             // 4134.
             synreport(45)
      // 4138.
//sawritef("rdcdefs: calling nextsymb()*n")
      nextsymb()
      // 4140.
//sawritef("rdcdefs: calling rexp(0)*n")
      !listp := rexp(0)
      // 4145.
      listp, n := listp+1, n+2
//sawritef("rcdefs: returned from rexp(0), listp=%n n=%n*n", listp, n)
//abort(2005)
      // 4153.
      // NOP

rec:  // 4154.
//sawritef("rdcdefs: calling ignore(s.semicolon)*n")
      ignore(s.semicolon)
      // 4158.
   $) REPEATWHILE // 4158.
                  symb=s.name

   // 4162.
   listp := p
   // 4166.
   rec.p, rec.l := rp, rl
   // 4172.
//sawritef("rdcdefs: calling makelist(s.semicolonlist, %n)*n", n)
   RESULTIS makelist(s.semicolonlist, n)
$)

AND rdsect(r) = VALOF
$( // 4180.
   LET tag, a = wordnode, 0
//sawritef("rdsects: calling checkfor(s.lsect, 6)*n")
   // 4185.
   checkfor(s.lsect, 6)
   // 4191.
//sawritef("rdsects: calling r()*n")
   a := r()
//sawritef("rdsects: returned fron r()*n")
   // 4194.
   UNLESS symb=s.rsect DO synreport(7)
   // 4203.
   TEST tag=wordnode   // -> 4212
   THEN // 4208.
        nextsymb()
   ELSE IF // 4212.
           wordnode=nulltag DO
        $( // 4218.
           symb := 0
           // 4221.
           synreport(9)
           // 4224.
        $)
   // 4224.
   RESULTIS a
$)

AND rnamelist() = VALOF
$( // 4226.
   LET n, p = 0, listp

   $( // 4231.
      !listp := rname()
      // 4235.
      listp, n := listp+1, n+1
      // 4243.
      UNLESS symb=s.comma BREAK  //  -> 4255
      // 4251.
      nextsymb()
      // 4253.   -> 4231
   $) REPEAT
//sawritef("rdnamelist: %n names read*n", n)
   // 4255.
   listp := p
//FOR i = 0 TO n-1 DO
//{ LET name = listp!i
//  sawritef("%n: name %s %n -> [%n %n %x8]*n", i, @h3!name, name, h1!name, h2!name, h3!name)
//}
//abort(8892)  
   // 4258.
   IF n=1 RESULTIS !listp
   // 4266.
   IF n=2 RESULTIS list3(s.comma, listp!0, listp!1)
   // 4282.
   RESULTIS makelist(s.commalist, n)
   // 4288.
$)


AND rname() = VALOF
$( // 4290.
   LET a = wordnode
//sawritef("rname: calling checkfor(s.name, 8) symb=%n*n", symb)
   // 4293.
   checkfor(s.name, 8)
   // 4298.
//sawritef("%i5 -> Name node %s*n", wordnode, @h3!wordnode)
//sawritef("%i5 -> [%n %n %x8]*n", a, h1!a, h2!a, h3!a)
//abort(8889)
   RESULTIS a
   // 4299.
$)

AND ignore(item) BE IF // 4300.
                       symb=item DO
                         // 4304.
                         nextsymb()

AND checkfor(item, n) BE
$( // 4308.
   UNLESS symb=item DO synreport(n)
   // 4315.
   nextsymb()
   // 4317.
$)

.

SECTION "SYN6"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/synhdr.h"

LET rbexp() = VALOF
$(1 // 4372.
    LET a, op = 0, symb
    // 4377.
//sawritef("rbexp(): op=%n*n", op)

    SWITCHON symb INTO

 $( DEFAULT: // 4379.
             synreport(32)

    CASE s.query:
        // 4383.
        nextsymb()
        // 4385.
        RESULTIS list1(s.query)

    CASE s.true:
    CASE s.false:
    CASE s.name:
        // 4391.
        a := wordnode
        // 4394.
        nextsymb()
        // 4396.
        RESULTIS a

    CASE s.string:
     $( // 4399.
        LET wordsize = wordv%0/BYTESPERWORD   // p5
        // 4406.
        a := newvec(wordsize+1)
        // 4410.
        a!0 := s.string
        // 4412.
        FOR i = 0 TO wordsize DO a!(i+1) := wordv!i
        // 4432.
        nextsymb()
        // 4434.
        RESULTIS a
     $)

    CASE s.number:
     $( // 4437.
        LET k = decval    // p5
//sawritef("rbexp: decval=%n*n", decval)
        // 4440.
        nextsymb()
        // 4442.
        IF k=0 RESULTIS zeronode
        // 4449.
        IF smallnumber(k) RESULTIS k
        // 4457.
//{ LET res = list2(s.number, k)
//  sawritef("rbexp: returning number node %n -> [%n %n]*n", res, h1!res, h2!res)
//  RESULTIS res
//}
        RESULTIS list2(s.number, k)
     $)

    CASE s.lparen:
        // 4464.
        nextsymb()
        // 4466.
        a := rexp(0)
        // 4470.
        checkfor(s.rparen, 15)
        // 4477.
        RESULTIS a

    CASE s.valof:
        // 4480.
        nextsymb()
        // 4482.
        RESULTIS list2(s.valof, rcom())

    CASE s.vecap: // 4490
                  op := s.rv
    CASE s.lv:
    CASE s.rv:    // 4492.
                  nextsymb()
                  // 4494.
                  RESULTIS list2(op, rexp(37))    //  -> 4648

    CASE s.plus:  // 4504.
                  nextsymb()
                  // 4506.
                  RESULTIS rexp(34)        //  -> 4648

    CASE s.minus: // 4512.
                  nextsymb()
                  // 4514.
                  a := rexp(34)
                  // 4519.
                  IF smallnumber(a) RESULTIS list2(s.number, -a)
                  // 4531.
                  RESULTIS list2(s.neg, a)

    CASE s.not:   // 4539
                  nextsymb()
                  // 4541.
                  RESULTIS list2(s.not, rexp(24))

    CASE s.abs:   // 4552.
                  nextsymb()
                  // 4554.
                  RESULTIS list2(s.abs, rexp(35))

    CASE s.table: // 4565.
                  nextsymb()
                  // 4567.
                  RESULTIS list2(s.table, rexplist())
$)1


AND rexp(n) = VALOF
$(1 // 4650.
    LET a = rbexp()
    // 4652
    LET b, c, p, q = 0, 0, 0, 0   // p5 to p8

$(2 // 4658.
    LET op = symb
//sawritef("rexp(%n): a=%n -> [%n %n] op=%n*n", n, a, h1!a, h2!a, op)
    // 4661.
    IF nlpending RESULTIS a
    // 4668.
    SWITCHON op INTO

$(s DEFAULT: // 4670.
//sawritef("rexp(%n): returning a=%n -> [%n %n]*n", n, a, h1!a, h2!a)
             RESULTIS a

    CASE s.lparen: // 4673.
                   nextsymb()
                   // 4675.
                   b := 0
                   // 4677.
                   UNLESS symb=s.rparen DO b := rexplist()
                   // 4686.
                   checkfor(s.rparen, 19)
                   // 4693.
                   a := list3(s.fnap, a, b)
                   // 4700.
                   LOOP    //  -> 4658

    CASE s.vecap:  // 4703.
                   p := 40; GOTO lassoc

    CASE s.rv:     // 4709.
                   symb := s.vecap
    CASE s.byteap: // 4712.
                   p := 36; GOTO lassoc

    CASE s.rem:CASE s.mult:CASE s.div:
                   // 4718.
                   p := 35; GOTO lassoc

    CASE s.plus:CASE s.minus:
                   // 4724.
                   p := 34; GOTO lassoc

    CASE s.eq:CASE s.ne:
    CASE s.le:CASE s.ge:
    CASE s.ls:CASE s.gr:
           // 4730.
           IF n>=30 RESULTIS a

           $(r // 4738.
               nextsymb()
               // 4740.
               b := rexp(30)
               // 4745.
               a := list3(op, a, b)
               // 4753.
               TEST c=0 THEN // 4756
                             c :=  a
                             // 4758.   -> 4769
                        ELSE // 4760.
                             c := list3(s.logand, c, a)
               // 4769.
               a, op := b, symb
               // 4774.
           $)r REPEATWHILE // 4774.
                           s.eq<=op<=s.ge
           // 4783.
           a := c
           // 4785.
           LOOP       //  -> 4658

    CASE s.lshift:CASE s.rshift:
                   // 4787.
                   p, q := 25, 30; GOTO dyadic

    CASE s.logand: // 4796.
                   p := 23; GOTO lassoc

    CASE s.logor:  // 4802.
                   p := 22; GOTO lassoc

    CASE s.eqv:CASE s.neqv:
                   // 4808.
                   p := 21; GOTO lassoc

    CASE s.cond:
            // 4814.
            IF n>=13 RESULTIS a
            // 4822.
            nextsymb()
            // 4824.
            b := rexp(0)
            // 4828.
            checkfor(s.comma, 30)
            // 4835.
            a := list4(s.cond, a, b, rexp(0))
            // 4850.
            LOOP           //  -> 4658

    lassoc: // 4856. 
            q := p

    dyadic: // 4858.
            IF n>=p RESULTIS a
            // 4865.
            nextsymb()
            // 4865.
            a := list3(op, a, rexp(q))
            // 4878.
            LOOP          //  -> 4658
$)s
$)2 REPEAT
   // 4978.
   RETURN
$)1

AND rexplist() = VALOF
$( // 4980.
   LET a = 0
   // 4982.
   LET n = 0
   // 4983.
   LET q = listp

   $( // 4986.
      !listp := rexp(0)
      // 4989.
      listp, n := listp+1, n+1
      // 4999.
      UNLESS symb=s.comma BREAK
      // 5007.
      nextsymb()
      // 5009.    -> 4986
   $) REPEAT
   // 5011.
   listp := q
   // 5014.
   IF n=1 RESULTIS listp!0
   // 5022.
   IF n=2 RESULTIS list3(s.comma, listp!0, listp!1)
   // 5038.
   RESULTIS makelist(s.commalist, n)
$)


AND rdef() = VALOF
$(1 // 5046.
    LET n = rnamelist()
//sawritef("rdef just returned from rnamelist, now switching on symb %n*n", symb)
    // 5049.
    SWITCHON symb INTO

 $( CASE s.lparen:
      $( // 5051.
         LET a = 0
         // 5053.
         nextsymb()     // Get the symb after the lparen
         // 5055.
         UNLESS h1!n=s.name DO synreport(40)
         // 5063.
         IF symb=s.name DO a := rnamelist()
         // 5071.
         checkfor(s.rparen, 41)
//sawritef("rdef: fpl just read, a=%n*n", a)
//abort(8891)
         // 5078.
         IF symb=s.be DO
         $( // 5084.
            nextsymb()
            // 5086.
            RESULTIS list5(s.rtdef, n, a, rcom(), 0)   //  -> 5196
         $)
         // 5101.
         IF symb=s.eq DO
         $( // 5107.
            nextsymb()
            // 5109
            RESULTIS list5(s.fndef, n, a, rexp(0), 0)
         $)
         // 5125.
         synreport(42)
      $)

    DEFAULT:
         // 5129.
         synreport(44)

    CASE s.eq:
         // 5133.
         nextsymb()
         // 5135.
         IF symb=s.vec DO
         $( // 5141.
            nextsymb()
            // 5143.
            UNLESS h1!n=s.name DO synreport(43)
            // 5151.
            RESULTIS list3(s.vecdef, n, rexp(0))
         $)
         // 5163.
         RESULTIS list3(s.valdef, n, rexplist())
$)1

.

SECTION "SYN7"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"
GET "com16/synhdr.h"

LET rbcom() = VALOF
$(1 // 5238.
    LET a = ?
//sawritef("rbcom: symb=%n*n", symb)
    // 5238.
    SWITCHON symb INTO
 $( DEFAULT: // 5240.
             RESULTIS 0

    CASE s.name:CASE s.number:CASE s.string:
    CASE s.true:CASE s.false:
    CASE s.lv:CASE s.rv:CASE s.vecap:
    CASE s.lparen:
            // 5243.
            a := rexplist()   // p3
            // 5246.
            IF symb=s.ass DO
            $( // 5252.
               LET op = symb     // p4
               // 5253.
               nextsymb()
               // 5255.
               RESULTIS list3(op, a, rexplist())
            $)
            // 5365.
            IF smallnumber(a) DO synreport(51)
            // 5274.
            IF symb=s.colon DO
            $( // 5280
               UNLESS h1!a=s.name DO synreport(50)
               // 5288.
               nextsymb()
               // 5290.
               RESULTIS list4(s.colon, a, rbcom(),0)
            $)
            // 5303.
            IF h1!a=s.fnap DO
            $( // 5307.
               h1!a := s.rtap
               // 5310.
               RESULTIS a
            $)
            // 5313.
            synreport(51)
            // 5317.
            RESULTIS a

    CASE s.goto:CASE s.resultis:
            // 5320.
          { LET op = symb
            // 5323.
            nextsymb()
            // 5325.
            RESULTIS list2(op, rexp(0))
          }

    CASE s.if:CASE s.unless:
    CASE s.while:CASE s.until:
          { // 5334.
            LET op = symb  // p4
            // 5337.
            nextsymb()
            // 5339.
            a := rexp(0)
            // 5343.
            ignore(s.do)
            // 5347.
            RESULTIS list3(op, a, rcom())
          }

    CASE s.test:
          { // 5357.
            LET b = ?   //  p4
            // 5357.
            nextsymb()
            // 5359.
            a := rexp(0)
            // 5363.
            ignore(s.do)
            // 5367.
            b := rcom()
            // 5373.
            checkfor(s.or, 54)
            // 5377.
            RESULTIS list4(s.test, a, b, rcom())
          }

    CASE s.for:
        $(  LET i, j, k = ?, ?, 0  // p4 to p6
            // 5392.
            nextsymb()
            // 5394.
            a := rname()
            // 5397.
            checkfor(s.eq, 57)
            // 5404.
            i := rexp(0)
            // 5408.
            checkfor(s.to, 58)
            // 5415.
            j := rexp(0)
            // 5419.
            IF symb=s.by DO $( // 5425.
                               nextsymb()
                               // 5428.
                               k := rexp(0)
                            $)
            // 5431.
            ignore(s.do)
            // 5435.
            RESULTIS list6(s.for, a, i, j, k, rcom())  $)

    CASE s.loop:CASE s.break:CASE s.endcase:
    CASE s.return:CASE s.finish:
            // 5454.
            a := wordnode
            // 5457.
            nextsymb()
            // 5460.
            RESULTIS a

    CASE s.switchon:
            // 5462.
            nextsymb()
            // 5464.
            a := rexp(0)
            // 5468.
            checkfor(s.into, 60)
            // 5475.
            RESULTIS list3(s.switchon, a, rdsect(rdseq))

    CASE s.case:
          { LET b = ?
            // 5488.
            nextsymb()
            // 5491.
            a := rexp(0) // a=p3
            // 5494.
            checkfor(s.colon, 61)
            // 5501.
            b := rbcom()
            // 5504.
            RESULTIS list3(s.case, a, b)
          }

    CASE s.default:
            // 5514.
            nextsymb()
            // 5516.
            checkfor(s.colon, 62)
            // 5523.
            RESULTIS list2(s.default, rbcom())

    CASE s.lsect:
            // 5532.
            RESULTIS rdsect(rdblockbody)
$)1


AND rcom() = VALOF
$(1 // 5656.
    LET a = rbcom()
    // 5658.

    IF a=0 DO synreport(51)
    // 5665.
    WHILE symb=s.repeat | symb=s.repeatwhile |
                          symb=s.repeatuntil DO
    $( // 5667.
       LET op = symb
       // 5670.
       nextsymb()
       // 5672.
       TEST op=s.repeat
       THEN // 5677.
            a := list2(op, a)
       ELSE // 5685.
            a := list3(op, a, rexp(0))
       // 5695.
    $)
    // 5710.
    RESULTIS a
$)1


