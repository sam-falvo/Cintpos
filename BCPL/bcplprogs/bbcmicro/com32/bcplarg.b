// This is a reconstruction of bcplargs.b for the BBC BCPL System.
// When compiled by bbcbcpl using the 32-bit BCPL system it
// should generate an object file identical to bbccin/BCPLARGS.

// Reconstructed by Martin Richards (c) Mar 2017

// This reconstruction was made with the aid of oldcom/bcpl-args.b
// and map/BCPLARGS.map created by: c df BCPLARGS.

// Comments such as // 1187: give byte addresses of positions
// in the object code. They refer to the BCPL statements
// immediately below the comments.


SECTION "ARGS"

GET "libhdr"
GET "LIBHDR32.h"
GET "SYSHDR32.h"
GET "bcpl.h"

STATIC { rec.p=0; rec.l=0 }      // rec.p=L66  rec.l=L68

LET START() = VALOF
$( // 16.
   LET args = "FROM/A,TO/A,REPORT/K,NONAMES/S,MAX/S,SECTLEN/S"    // p3
   // 19.
   LET ocodename = "OCODE$$"  // $$ get overwritten by two digits    p4
   // 22.
   LET title = "*nBCPL - RCP V2.2*n"   // p5
   // 25.
   LET errarg = "Bad args for %S"      // p6
   // 28.
   LET errfil = "Cannot open %S"       // p7
   // 31.
   LET oldoutput = OUTPUT()            // p8
   // 34.
   LET argv = VEC 80                   // p9 -> p10..p90
   // 37.

   LET error(mess, a, b) BE         // p3, p4, p5    -> 176
   { // 40.
     rc := RESULT2
     // 44.
     WRITES("*nError. ")
     // 48.
     WRITEF(mess, a, b)
     // 55.
     NEWLINE()
     // 57.
     LONGJUMP(rec.p, rec.l)
     // 64.
     RETURN
     // 65.
   }
   // 176.
   rec.p := LEVEL()    // L66
   // 182.
   rec.l := fail       // L68
   // 186.
   rc := 0
   // 189.
   sourcestream := 0   // g286
   // 191.
   ocodeoutstream := 0
   // 193.
   codestream := 0
   // 195.
   verstream := oldoutput
   // 198
sawritef("bcplarg: calling RDARG(%s,..)*n", args)

   UNLESS RDARGS(args, argv, 80) DO
   { RESULT2 := 11
     // 216.
     error(errarg, args)
     // 224.
   }

   // 224.
   IF argv!2 DO                        // REPORT/K
   { // 228.
     verstream := FINDOUTPUT(argv!2)   // REPORT/K
     // 236.
     UNLESS verstream DO
     { //238.
       verstream := oldoutput
       // 241.
       error(errfil, argv!2)   // REPORT/K
       // 250.
     }
     SELECTOUTPUT(verstream)   // verstream=g252
  }
  // 256.
  WRITES(title)
  // 261.
  naming := ~argv!3         // NONAMES/S

sawritef("naming=%n*n", naming)

// spacev is allocated by bcpl.b and has upb 1800

// In bcplsyn spacev is used to hold the following vectors:

// nametable    size nametablesize=128   The hash table
// chbuf        size 64                  For error syntax messages
// wordv        size 128                 Used to hold names and strings
// getv         size 20                  To hold GET streams
// gett                                  Points to just beyond end of getv

// In bcpltrn spacev is used to hold the following vectors:

// dvec         size declsize=1280       The delcaration vector
// globdecl     size 160                 To hold global entry points
// casek        size 160                 To hold case constants
// casel        size 160                 To hold case label numbers

// In bcplccg spacev is used to hold the following vectors:

// spacev2    upb        The hash table

  // 266.
  nametable := spacev       // nametable=g265  spacev=g251
  // 270.
  dvec := nametable
  // 274.
  cgworkspace := nametable         // cgworkspace=g261
  // 276.
  cgworksize := 1750
  // 281.
  chbuf := nametable + 128
  // 287.
  wordv := chbuf + 64     // wordv=g267
  // 291.
  getv := wordv + 128     // getv=g266
  // 295.
  gett := getv + 20       // gett=g268
  // 299.
  declsize := 1280+500        // declsize=g281
  // 304.
  savespacesize := 3

  // 307.
  { LET globdeclsize = 160     // p91
    // 311.
    LET p = dvec + declsize  // p92    declsize=1280
    // 317.
    dvect := declsize
    // 321.
    globdecl := p
    // 325.
    globdeclt := globdeclsize
    // 329.
    casek := globdeclsize + p
    // 333.
    casel := 2*globdeclsize + p
    // 341.
    caset := globdeclsize
    // 345.
    maxoption := argv!4        // MAX/S    maxoption=g255

    sectionlen := argv!5       // SECTLEN/S
sawritef("bcplarg: sectionlen = %n*n", sectionlen)
  }
    // 349.
    UNLESS argv!4 DO               // MAX/S      -> 399
    { // 353.
      UNLESS FILENAME(argv!0, 0) |     // g60 -> 399
             // 363.
             FINDSTFILE(argv!0) DO     // g148 -> 399
      { // 370.
        READ(argv!0, 0, 0)             // FROM/A
        // 380.
        { LET filevec = FINDSTFILE(argv!0)   // filevec=p91
          IF filevec DO           // -> 399
          { // 389.
            filevec!-1 := filevec!-1 & #x7FFF
            // 399.
          }
        }
        // 399.
      }
      // 399.
    }

    // 399.
    sourcestream := FINDINPUT(argv!0)    // FROM/A
    // 406.
    UNLESS sourcestream DO     // -> 416
    { // 408.
      error(errfil, argv!0)
      // 416.
    }
//sawritef("sourcestream %s selected for input*n", argv!0)
    // 416.
    ocodeoutstream := FINDOUTPUT(ocodename)
    // 423.
    UNLESS ocodeoutstream DO      // 433
    { // 425.
      error(errfil, ocodename)
    } 
//sawritef("ocodeoutstream %s selected for output*n", ocodename)
//  }
  // 433.
  codestream := FINDOUTPUT(argv!1)
  // 441.
  UNLESS codestream DO      // -> 452
  { // 443.
    error(errfil, argv!1)
    // 452.
  }
//sawritef("codstream %s opened for output*n", argv!1)
  // 452.
  SELECTINPUT(sourcestream)
  // 458.
  linenumber := 1
  // 462.
  reportcount := 0
  // 465.
  RETURN
  // 465.
 
fail:
  // 466.
  CLOSEINPUT(sourcestream)
  // 472.
  CLOSEOUTPUT(ocodeoutstream)
  // 472.
  DELFILE(ocodename)
  // 483.
  UNLESS verstream=oldoutput DO 
    CLOSEOUTPUT(verstream)
  // 492.
  //RETURN
  // 493.
$)

AND CLOSEOUTPUT(s) BE
{ // 496.
  UNLESS s=0 DO
  $( // 498.
     SELECTOUTPUT(s)
     // 500.
     ENDWRITE()
  $)
  // 502.
  RETURN
}

AND CLOSEINPUT(s) BE
{ // 504.
  UNLESS s=0 DO
  $( // 506.
     SELECTINPUT(s)
     // 508.
     ENDREAD()
  $)
  // 510.
  RETURN
}
