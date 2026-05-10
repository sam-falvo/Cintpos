// This is a reconstruction of bcpl.b for the BBC BCPL System.
// When compiled by bbcbcpl using the 32-bit BCPL system it
// should generate an object file identical to bbccin/BCPL.

// Reconstructed by Martin Richards (c) Mar 2017

// This reconstruction was made with the aid of oldcom/bcpl.b
// and map/BCPL.map created by: c df BCPL.

// Comments such as // 207: give byte addresses of positions
// in the object code. They refer to BCPL statements immediately
// below the comment.

SECTION "BCPL"

GET "com16/LIBHDR.h"
GET "com16/SYSHDR.h"
GET "com16/bcpl.h"

LET callbcplseg(s, a) = VALOF
$( // 16:
   LET overseg = LOADSEG(s)
//sawritef("callbcplseg(%s, %n) called*n", s, a)
   IF overseg=0 DO
   $( // 21:
      rc := RESULT2
      // 25:
      WRITEF("*nCannot load %S", s)
      // 31:
      RESULTIS 0
   $)
   // 34:
   GLOBIN(overseg)
//sawritef("Seg %s loaded, calling START(%n)*n", s, a)
   // 37:
   a := START(a)
   // 41:
   GLOBUNIN(overseg)
   // 44:
   UNLOADSEG(overseg)
   // 47:
   RESULTIS a
$)

AND freeocode() BE
$( // 66:
   LET p = blk
   blk := 0
   // 72:
   WHILE p DO   // -> 81
   $( // 74:
      LET q = p    // q=p4
      p := !p
      FREEVEC(q)
   $)
   // 84:
   RETURN
$)

// Numbers in the range 1 to 899 are represented in the
// AE tree by values in this range. Other numbers are represented by
// pointers to number nodes such as [s.number 1234].
AND smallnumber(x) =
  // 86:
  0<x<900 -> // 93.
             TRUE,
             // 96.
             FALSE

AND STARTINIT() =
  // 98:
  550       // The size of the runtime stack used by the compiler.

LET START() BE
$( LET args = "BCPLARG"       // args=p3
   LET syn  = "BCPLSYN"       // syn = p4
   LET trn  = "BCPLTRN"       // trn = p5
   LET cg   = "BCPLCCG"       // ccg = p6
   LET stdin  = INPUT()          // stdin = p7
   LET stdout = OUTPUT()         // stdout = p8
   // 120:
   spacev := GETVEC(1800)
   // 128:
   blk := 0
   codestream := 0
//sawritef("132: bcpl: START: entered*n")
//abort(132)
   // 132:
   callbcplseg(args)
   // 136.
   UNLESS rc=0 GOTO fail
//sawritef("143: bcpl: calling callbcplseg(syn)*n")
//abort(143)
   // 143:
   UNLESS sourcestream=0 DO    // g286
   $( // 147:
      LET a = callbcplseg(syn)      // p9
//sawritef("152: bcpl: returned from callbcplseg(syn) with result %n*n",a)
//abort(152)
      // 152:
      IF ch=ENDSTREAMCH DO ENDREAD()
      // 159:
      IF a=0 | rc BREAK
      // 168:
      callbcplseg(trn, a)
//sawritef("bcpl: calling freeocode() after returning from trn*n")
      // 174:
      freeocode()
//sawritef("bcpl: ch=%n rc=%n*n", ch, rc)
      // 177:
   $) REPEATUNTIL ch=ENDSTREAMCH | rc~=0

   // 186:
   ENDREAD()
   // 188:
   SELECTOUTPUT(ocodeoutstream)
   WRCH(0)
   ENDWRITE()
   // 197:
   ocodeoutstream := 0
   // 198:
   SELECTOUTPUT(verstream)
//sawritef("bcpl: calling freeocode() at 204*n")
   // 204:
   freeocode()
//sawritef("bcpl: after freeocode() at 204, rc=%n*n", rc)
   // 207:
   TEST rc=0
   THEN $( // 211:
//sawritef("211: bcpl: calling cg*n")
           callbcplseg(cg)
           SELECTOUTPUT(verstream)
        $)
   ELSE $( // 221:
           WRITES("*nNo Code Generated")
        $)
   // 225:   filler
   // 226:
fail:
   NEWLINE()
//sawritef(" 228: bcpl: START compilation complete*n")
//abort(228)
   // 228:
   UNLESS verstream=stdout DO ENDWRITE()
   // 235:
   UNLESS stdin=INPUT() DO ENDREAD()
   // 242:
   IF codestream DO
   $( // 246.
      SELECTOUTPUT(codestream)
      // 248.
      ENDWRITE()
   $)
   // 250:
   SELECTOUTPUT(stdout)

   // 253:
   DELFILE("OCODE$$")
   // 257:
   STOP(rc)
   // 261.
$)


