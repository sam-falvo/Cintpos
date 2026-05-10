/*

This will be a modification of the Z80 assembler writen by Gray
Girling in 1980. This version will be compiled and run using the
modern 32 bit BCPL Cintcode system.

This modification is by Martin Richards (c) Oct 2020
*/

SECTION "Z80asm"

GET "libhdr"

GLOBAL $(
// dictionary
   dicposn          : ug+0
   getlab           : ug+1
   putlab           : ug+2
   printlabs        : ug+3
   deletelab        : ug+4
   deletelabs       : ug+5
   valtype          : ug+6
   freetype         : ug+7
   printtype        : ug+8
   checktype        : ug+9
   gettype          : ug+10
   looktype         : ug+11
   iden.valid       : ug+12
   badlabs          : ug+13
   gen.defs         : ug+14

// hex output
   binbuf           : ug+15
   putword          : ug+16
   putaddress       : ug+17
   putlabelspec     : ug+18
   movebinword      : ug+19
   hexoutwidth      : ug+20
   outhex           : ug+21
   code.gen         : ug+22
   entrypoint       : ug+23

// input & listing files
   ch               : ug+24
   linbufsize       : ug+25
   linbuf           : ug+26
   linbuf.out       : ug+27
   linbuf.top       : ug+28
   print.expansions : ug+29
   rch              : ug+30
   putch            : ug+31
   mywrch           : ug+32
   syswrch          : ug+33
   deleteline       : ug+34
   restartpage      : ug+35
   list             : ug+36
   outline          : ug+37
   newlyne          : ug+38
   position.ch      : ug+39
   outpos           : ug+40
   lineno           : ug+41
   neads            : ug+42

// text expansion
   macro.stack      : ug+43
   filling.macro    : ug+44
   expand.text      : ug+45
   macro.rdch       : ug+46
   macro.var        : ug+47
   call.macro       : ug+48
   end.macro.var    : ug+49
   add.local.text   : ug+50
   stacktext        : ug+51
   endline          : ug+52

// macros
   macro.wrch       : ug+53
   strexp           : ug+54
   is.strexp        : ug+55
   find.mnem        : ug+170
   callmacroproc    : ug+56
   macroproc        : ug+57
   endmacroproc     : ug+58
   rptmacroproc     : ug+59
   breakmacroproc   : ug+60
   ifproc           : ug+61
   elseproc         : ug+62
   fiproc           : ug+63
   optproc          : ug+64
   localproc        : ug+65
   printproc        : ug+66

// error processing
   errormess        : ug+67
   geterror         : ug+68
   error            : ug+69
   warn             : ug+70
   errorlabel       : ug+71
   errorlevel       : ug+72
   fatal            : ug+73
   warning          : ug+74
   warnvec          : ug+75
   errcount         : ug+76
// parsing
   item.type        : ug+77
   item.info        : ug+78
   item.start       : ug+79
   getitem          : ug+80
   scan             : ug+81
   checkandskip     : ug+82
   bracketed        : ug+83
   dontknow         : ug+84
   expression       : ug+85
   label.expression : ug+86
   pcrel.expression : ug+87
   get.and.declare  : ug+88
   is.type          : ug+89
   is.expression    : ug+90
   finishpass       : ug+91
   nextpass         : ug+92
   read.to.mnem     : ug+93
   parseline        : ug+94
   parse            : ug+95

// predeclared parsing functions
   dataproc         : ug+96
   titlproc         : ug+97
   equproc          : ug+98
   setproc          : ug+99
   defproc          : ug+100
   refproc          : ug+101
   wrefproc         : ug+102
   needsproc        : ug+103
   pragproc         : ug+104
   textproc         : ug+105
   getproc          : ug+106
   endproc          : ug+107
   absproc          : ug+108
   relproc          : ug+109
   orgproc          : ug+110
   storeproc        : ug+111
   spaceproc        : ug+112
   ejectproc        : ug+113
   listproc         : ug+114
// file list
   file.id          : ug+115
   max.files        : ug+116
   files            : ug+117
   line.of.file     : ug+118
   newfile          : ug+119
   endfile          : ug+120
   printfiles       : ug+121
   resetfiles       : ug+122
   file.number      : ug+123

// miscelaneous procedures for table building &c
   compstring       : ug+124
   getstr           : ug+125
   capitalch        : ug+126
   codes            : ug+127
   code.put         : ug+128
   getcode          : ug+129
   compcode         : ug+130
   reg.put          : ug+131
   getreg           : ug+132
   newf             : ug+133
   getf             : ug+134
   putf             : ug+135
   putwordf         : ug+136
   trim             : ug+137
   fitsmask         : ug+138

// memory allocation
   lex.space        : ug+139
   lab.space        : ug+140
   memory           : ug+141
   max.temp.used    : ug+142
   simplegetvec     : ug+143
   tempgetvec       : ug+144

// assembler state
   state            : ug+145
   pagetitle        : ug+146
   modulename       : ug+147
   mode             : ug+148
   pass             : ug+149
   sectno           : ug+150
   pc               : ug+151
   startpc          : ug+152
   def.count        : ug+153

// calls to main assembler
   initsyms         : ug+154
   initcodes        : ug+155
   startasm         : ug+156
   startparse       : ug+157
   newsection       : ug+158
   endsection       : ug+159
   endparse         : ug+160
   endasm           : ug+161
   useropt          : ug+162

// main assembler options
   name             : ug+163
   i.here           : ug+164
   i.elbkt          : ug+165
   i.erbkt          : ug+166
   i.strlbkt        : ug+167
   i.strrbkt        : ug+168
   i.endlab         : ug+169
   comntch          : ug+171
   comntcheck       : ug+172
   sepch            : ug+173
   binbufwidth      : ug+174
   mscharfirst      : ug+175
   msbytefirst      : ug+176
   bytesperasmword  : ug+177
   wordsperaddress  : ug+178
   cvtchar          : ug+179

// users options
   fromfile         : ug+180
   binfile          : ug+181
   out              : ug+182
   memsize          : ug+183
   pw               : ug+184
   throws           : ug+185
   allsyms          : ug+186
   short            : ug+187

// machine dependant procedures:
   get.time         : ug+188
   findasmin        : ug+189
   findgetin        : ug+190


    f.rinfo      //   'r' symbol information field
    f.ccinfo     //   'cc' symbol information field
    f.ddinfo     //   'dd','rr','pp' and 'qq' information field
    f.source     //   source field
    f.ddsource   //   destination field for double regs
    f.dest       //   destination field
    f.top        //   top two bits field
    f.ccsmall    //   field for condition code in JR

    /*    These fields are layed out as folows:

    -------------------------------------
    | f.top |   f.source  |   f.dest    |
    -------------------------------------

    -------------------------------------
    |       |f.ddsource|                |
    -------------------------------------

    */

    getrset
    scanreg
    getind
    getrorind
    getixyhl
    dwproc
    fmt0
    fmt1
    fmt2
    fmt3
    fmt4
    fmt5
    fmt6
    fmt7
    fmt8
    fmt9
$)


MANIFEST $(
    null = 0
    byte1 = #X00FF
    byte2 = #XFF00
    bit0 = #X0001
    bit1 = #X0002
    bit2 = #X0004
    bit3 = #X0008
    bit4 = #X0010
    bit5 = #X0020
    bit6 = #X0040
    bit7 = #X0080
    bit8 = #X0100
    bit9 = #X0200
    bit10= #X0400
    bit11= #X0800
    bit12= #X1000
    bit13= #X2000
    bit14= #X4000
    bit15= #X8000
    first = TRUE
    second = FALSE


    pl         = 55             // page length
    binbufsize = 128            // words in binary output buffer
    b.top      = 0
    b.nextmod  = 1
    c.str      = 0
    c.fn       = 1
    c.opstr    = 2
    in.lineno  = 0
    in.errcount = 1
    in.fileno  = 2
    in.filenogen = 3
    in.first   = 4
    in.second  = 5
    in.list    = 6
    in.memlevel = 7
    insize     = 8
    absolute   = 1
    relative   = -1
    e.memfull      =  -1
    e.nocodes      =  -2
    e.nospace      =  -3
    e.fullstring   =  4
    e.badsym       =  5
    e.nolab        =  6
    e.badcode      =  7
    e.badnum       =  8
    e.badbyte      =  9
    e.doublelab    =  10
    e.interror     =  -11
    e.badfile      =  -12
    e.badbase      =  13
    e.badlab       =  14
    e.titledone    =  15
    e.mtbuf        =  -16
    e.expected     =  17
    e.ftoosmall    =  19
    e.divzero      =  20
    e.notlab       =  21
    e.badform      =  22
    e.nostr        =  23
    e.filexs       =  24
    e.posnum       =  25
    e.noname       =  27
    e.badtype      =  28
    e.nobkt        =  29
    e.nocomnt      =  30
    e.badtext      =  31
    e.forward      =  32
    e.binfull      =  33
    e.badrel       =  34
    e.badext       =  35
    e.relsection   =  36
    e.baddef       =  37
    e.modeset      =  38
    e.newentry     =  39
    e.badloaddir   =  40
    e.overflow     =  -41
    e.macstack     =  42
    e.nomacterm    =  43
    e.bigmacro     =  -44
    e.noif         =  45
    e.nomacro      =  46
    e.e            =  48
    s.size  = 0
    s.str   = 0
    s.info  = 1
    type.none  = 0
    type.const = 1
    type.var   = 2
    type.lab   = 3
    type.ref   = 4
    type.text  = 5
    type.macro = 6
    type.      = type.macro+1
    type.mask  = byte1
    flag.double= bit8
    flag.rel   = bit9
    flag.def   = bit10
    flag.fwd   = bit11
    flag.needs = bit12
    flag.weak  = bit13
    flag.temp  = bit14
    type.def   = type.none | flag.def
    type.wref  = type.ref | flag.weak
    type.rellab= type.lab | flag.rel
    type.relvar= type.var | flag.rel
    type.relconst = type.const | flag.rel
    i.bad    = 0
    i.number = 1
    i.string = 2
    i.iden   = 3
    i.comma  = 4
    i.lsqb   = 5
    i.rsqb   = 6
    i.lbkt   = 7
    i.rbkt   = 8
    i.plus   = 9
    i.minus  = 10
    i.mult   = 11
    i.div    = 12
    i.immed  = 13
    i.space  = 14
    i.stop   = 15
    i.equals = 16
    i.semi   = 17
    i.and    = 18
    i.or     = 19
    i.shr    = 20
    i.shl    = 21
    i.end    = 22
    i.colon  = 23
    i.dollar = 24
    i.sstring= 25
    i.comnt  = 26
    i.gt     = 27
    i.lt     = 28
    i.percent= 29
    i.not    = 30
    i.cap    = 31
    i.ul     = 32
    i.pling  = 33
    cd.undef =-3
    cd.clear =-2
    cd.newpc =-1
    cd.data  = #X00
    cd.eof   = #X01
    cd.int   = #X02
    cd.ext   = #X03
    cd.rel   = #X04
    cd.module= #X05
    cd.code  = #X10
    cd.opt   = #X11
    cd.absint= #X12
    cd.wext  = #X13
    cd.prag  = #X15
    spec.size= 2
    //ag       =  ug+200 // Check with the cross reference listing
$)

//
//              In - line   Header   For  Z80  Assembler
//





MANIFEST
$(  e.ixyhl     =  e.e + 0
    e.r         =  e.e + 1
    e.dd        =  e.e + 3
    e.qq        =  e.e + 4
    e.rr        =  e.e + 5
    e.pp        =  e.e + 6
    e.cc        =  e.e + 7
    e.regexp    =  e.e + 8
    e.badreg    =  e.e + 9
    e.rhxy      =  e.e + 10
    e.badcc     =  e.e + 11
    e.badim     =  e.e + 13
    e.badds     =  e.e + 14
    e.range     =  e.e + 15
    e.bad16no   =  e.e + 16
    r.bad       =  0            // bad register
    b.r         =  bit15        // B, C, D, E, H, L, or A
    b.dd        =  bit14        // BC, DE, HL or SP
    b.qq        =  bit13        // BC, DE, HL or AF
    b.pp        =  bit12        // BC, DE, IX or SP
    b.rr        =  bit11        // BC, DE, IY or SP
    b.cc        =  bit10        // NZ, Z, NC, C, PO, PE, P=V, or M=NV
    b.ixyhl     =  bit9         // IX, IY, or HL
    b.rori      =  bit8         // R or I
    b.bcde      =  bit7         // BC or DE
    b.ixiy      =  bit6         // IX or IY
    r.b         =  b.r | #X0
    r.c         =  b.r | #X1 | b.cc | (#X3<<3)
    r.d         =  b.r | #X2
    r.e         =  b.r | #X3
    r.h         =  b.r | #X4
    r.l         =  b.r | #X5
    r.hl        =  b.dd | b.qq | b.ixyhl | #X6
    r.a         =  b.r | #X7
    r.bc        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X0
    r.de        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X1
    r.ix        =  b.pp | b.ixyhl | b.ixiy | #X6
    r.iy        =  b.rr | b.ixyhl | b.ixiy | #X6
    r.sp        =  b.dd | b.pp | b.rr | #X3
    r.af        =  b.qq | #X3
    r.r         =  b.rori | 1
    r.i         =  b.rori | 0
    cc.nz       =  b.cc | (#X0<<3)
    cc.z        =  b.cc | (#X1<<3)
    cc.nc       =  b.cc | (#X2<<3)
    cc.c        =  r.c
    cc.po       =  b.cc | (#X4<<3)
    cc.pe       =  b.cc | (#X5<<3)
    cc.p        =  b.cc | (#X6<<3)
    cc.m        =  b.cc | (#X7<<3)
    cc.v        =  cc.p
    cc.nv       =  cc.m
    fn.bit      = 0
    fn.normal   = 1
$)



/*



*************************************************************************
*  (C) Copyright 1980  Systems Research Group, University of Cambridge  *
*************************************************************************
*                                                                       *
*               G E N E R A L     A S S E M B L E R                     *
*                                                                       *
*                        S E C T I O N S                                *
*                                                                       *
*************************************************************************
**  C  Gray  Girling      COMPUTER LAB,  CAMBRIDGE           26.02.80  **
*************************************************************************

*/




//  LOG OF CHANGES:
//  ===============
//
//  Log entry is <Version number> <date> <initials> <change>
//
//  3.000  26.02.80  CGG   Relocation and externals added
//  3.006  21.03.80  CGG   Bug in external refs fixed
//  3.008  24.03.80  CGG   Machine type CHEX record produced
//  3.009  03.04.80  CGG   Obscure pagination bug fixed
//  3.011  11.04.80  CGG   Default for S option increased to 3000
//                         Forward references for EQU vars allowed
//  3.012  12.04.80  CGG   Fault in REF chains fixed
//                         Fewer zero length CHEX data records produced
//  3.013  15.04.80  CGG   NEEDS directive added
//  3.016  29.04.80  CGG   Outputs CHEX record type 12
//  3.017  01.05.80  CGG   Random store reads fixed
//  3.020  09.05.80  CGG   Stack & global vector requirements reduced
//  3.021  13.05.80  CGG   Listing bug fixed
//  3.022  16.05.80  CGG   Strings in dictionary tree lose pointer 8% S saved
//  3.023  21.05.80  CGG   CHEX generalized for 8X300
//  3.024  28.05.80  CGG   TITLE directive uses label for CHEX module name
//  3.026  05.06.80  CGG   Memory requirements reduced for PDP11
//  3.027  06.06.80  CGG   ~ or \ (not) allowed in expressions
//  3.028  11.07.80  CGG   NEEDS extended to pass option into CHEX output
//                         WREF added - generates new CHEX record
//  3.029  17.07.80  CGG   END <entrypoint> implemented
//  3.031  04.08.80  CGG   Gets text after last END right (and missing ENDs)
//  3.034  22.08.80  CGG   DEF <expression> feature added
//                         PRAG directive installed and CHEX generated
//  3.037  10.10.80  CGG   DAT-A pragmat generated
//                         Procedure for finding time installed in m/c
//                         dependent part of code
//                         File handling and looking up rationalized
//  3.043  11.11.80  CGG   Restriction on number of GET files removed
//  3.045  14.01.81  CGG   Bug in TEXT variables fixed
//  3.046  14.02.81  CGG   Dynamic DD names + GETLIB library installed for IBM
//  3.054  16.10.82  NJO   Dictionary tree section (ASM2) completely
//                         replaced by HASH TABLE mechanism with 60%
//                         increase in speed
//  3.056  12.01.82  CGG   New CHEX machine type record generated
//  3.057  26.01.82  CGG   TEXT variable stack changed to accomodate MACROs
//  3.058  27.01.82  CGG   RCH changed to fill macro text areas
//                         Macro creation added
//                         Repeat text command added
//  3.059  29.01.82  CGG   Conditional assembly added
//                         String expressions added
//  3.060  01.02.82  CGG   FLAG.TEMP variables added
//                         Parameter substitution added to macros
//                         GET bug (extra new line) finally fixed
//  3.061  03.02.82  CGG   Paramter substitution bug fixed
//                         READ.MACRO.BODY bug fixed
//                         TEMPVEC store management added
//  3.062  04.02.82  CGG   Random symbol generator added
//  3.063  05.02.82  CGG   Expanded Text symbol layout improved
//  3.064  08.02.82  CGG   Condtional assembly given parsing options
//                         Assembler variables added to Numeric Expressions
//                         Option changing mnemonic added
//  3.065  29.06.82  CGG   Bug in SET - picks up forward refs now
//  3.066  23.08.82  CGG   Bug in hashing function corrected
//  3.067  21.09.82  CGG   Bug in tab handling fixed (at last)
//  3.068  25.10.82  CGG   Bug in layout PROCs when not listing fixed
//  3.069  27.01.83  CGG   Proper findinput used in CAP GETASMIN routine




// [* denotes a change to the assembler which diverges it from the standard
//    versions produced at Cambridge University Computer Laboratory ]






///SECTION "asm1"




/*  These globals kept for their comments - in asmhdr now
GLOBAL $( // dontknow              // set after looking for a label
             memsize               // size of label memory
             binfile               // binary file stream pointer (output)
          // out                   // standard output file
          // error                 // error routine
          // pass                  // set to 'first' or 'second'
             lineno                // number of current line
             outpos                // page and line number of output
             errorlabel            // label to go to in the event of an error
             errorlevel            // dynamic level at which error occurs
          // name                  // name of this program
             throws                // TRUE if page throws are included in o/p
          // list                  // level at which listing takes place
             realbinary            // TRUE when binary & not intel hex produced
             fatal                 // TRUE when error stops second pass starting
             warning               // TRUE when a warning message is outstanding
             warn1                 // 1st error argument in a warning
             warn2                 // 2nd    "            "
             warn3                 // 3rd    "            "
             warn4                 // 4th    "            "
             warnpos               // character position at warning
             short                 // TRUE if listing to be compact
             allsyms               // TRUE if all symbols are to be printed in
                                   // the dictionary printout
             errcount              // number of errors in this parse
             sectno                // current sectno number
             envfile               // file for dumping environment into
             restartpage           // TRUE when page throw needed before output
             iden.valid            // TRUE when type has been checked correct
$)
*/





//
//                       ERROR  MANAGER
//






LET geterror(rc) = VALOF
SWITCHON rc INTO
$(  CASE e.memfull:     RESULTIS "assembler storage exhausted - try S option"
    CASE e.nocodes:     RESULTIS "mnemonic tables not initialised"
    CASE e.nospace:     RESULTIS "not enough space for execution"
    CASE e.fullstring:  RESULTIS "string is longer than 255 characters"
    CASE e.badsym:      RESULTIS "unrecognisable item"
    CASE e.nolab:       RESULTIS "label expected"
    CASE e.badcode:     RESULTIS "illegal mnemonic"
    CASE e.badnum:      RESULTIS "number expected"
    CASE e.badbyte:     RESULTIS "value too large for machine's word length"
    CASE e.doublelab:   RESULTIS "label is doubly defined"
    CASE e.interror:    RESULTIS "internal error number %N"
    CASE e.badfile:     RESULTIS "can*'t open file %S"
    CASE e.badbase:     RESULTIS "bad numeric base char - '%C'"
    CASE e.badlab:      RESULTIS "label is undefined"
    CASE e.titledone:   RESULTIS "title already given"
    CASE e.mtbuf:       RESULTIS "internal error - bad access to binary buffer"
    CASE e.expected:    RESULTIS "'%C' expected"
    CASE e.ftoosmall:   RESULTIS "number (%N) too large for field"
    CASE e.divzero:     RESULTIS "divide by zero"
    CASE e.notlab:      RESULTIS "symbol is not a label"
    CASE e.badform:     RESULTIS "mnemonic illegal in this format"
    CASE e.nostr:       RESULTIS "string expected"
    CASE e.filexs:      RESULTIS "too many files"
    CASE e.posnum:      RESULTIS "positive number expected"
    CASE e.noname:      RESULTIS "name expected"
    CASE e.badtype:     RESULTIS "wrong sort of name"
    CASE e.nobkt:       RESULTIS "missing bracket"
    CASE e.nocomnt:     RESULTIS "comment does not begin '%C'"
    CASE e.badtext:     RESULTIS "recursive text expansion illegal"
    CASE e.forward:     RESULTIS "illegal forwards reference"
    CASE e.binfull:     RESULTIS "too much binary generated on this line"
    CASE e.badrel:      RESULTIS "relocatable or external illegal here"
    CASE e.badext:      RESULTIS "external variable must appear on its own"
    CASE e.relsection:  RESULTIS "only one relocatable section allowed"
    CASE e.baddef:      RESULTIS "symbol never defined"
    CASE e.modeset:     RESULTIS "mode is already set"
    CASE e.newentry:    RESULTIS "entry point already given"
    CASE e.badloaddir:  RESULTIS "facility not available in loader format"
    CASE e.overflow:    RESULTIS "run out of address space"
    CASE e.macstack:    RESULTIS "more than %N recursive expansions"
    CASE e.nomacterm:   RESULTIS "no terminator for macro"
    CASE e.bigmacro:    RESULTIS "macro too long"
    CASE e.noif:        RESULTIS "no conditional assembly in progress"
    CASE e.nomacro:     RESULTIS "no macro expansion in progress"
    DEFAULT:            RESULTIS errormess(rc)
$)


AND errormess(rc) = "bad error code given!!"








//
//                      Undefined Library Functions
//






/*      The following are generally useful procedures some of which may be
    available on particular machines
*/




LET tempgetvec(n) = VALOF
// MACROS - in a later section assume the way this procedure works
// Reclamation of symbol space at the end of each pass assumes that
//    storage can be recovered by resetting MEMORY!0
// 28.01.82
$(  LET ans = null
    UNLESS memory!0+n+1 > memory!memsize THEN
    $(  ans := @memory!(memory!0)
        memory!0 := memory!0+n+1
        IF memory!0 > max.temp.used THEN max.temp.used := memory!0
//      writef("TEMPGETVEC(%N): pointer incremented from %X4 to %X4*N",
//             n, ans - memory, memory!0)
    $)
    RESULTIS ans
$)



LET simplegetvec(n) = VALOF
// MACROS - in a later section assume the way this procedure works
// Reclamation of symbol space at the end of each pass assumes that
//    storage can be recovered by resetting MEMORY!MEMSIZE
// 03.02.82
$(  LET ans = null
    TEST memory!0 > memory!memsize-(n+1) THEN error(e.memfull) ELSE
    $(  ans := @memory!(memory!memsize-(n+1))
        memory!memsize := memory!memsize - (n+1)
//      writef("SIMPLEGETVEC(%N): pointer decremented from %X4 to %X4*N",
//             n, memory!memsize, ans - memory)
    $)
    RESULTIS ans
$)



LET capitalch(ch) = ('a'<=ch<='z'->ch+'A'-'a',ch)



LET compstring(s1, s2) = VALOF
$(  /*    If you are interested in the speed of this assembler you might like
       to know that it spends up to 25% of its time in this routine.
          This is due to its use within the dictionary to look up symbols -
       a hash table dictionary might yield a significant improvement.
          26.02.80
    */
    LET d=(s1%0-s2%0)
    LET n=(d>0 -> s2%0, s1%0)
    LET i=1
    WHILE i<=n & s1%i=s2%i DO i:=i+1
    RESULTIS (i>n-> d, (s1%i - s2%i))
$)



LET getstr(string, memproc) = VALOF
$(  LET len=string%0/bytesperword
    LET s = memproc(len)
    TEST s=null THEN error(e.memfull) ELSE
    FOR i=0 TO len DO s!i:=string!i
    RESULTIS s
$)








///SECTION "asm2"



//
//                       DICTIONARY
//




/*     This module is responsible for all accesses to the dictionary -- which
    is a hash table.   To modules using these procedures the dictionary seems to
    hold a set of triples associating the name of a symbol with its type
    and a value (which must fit into one word).
       'findentry' is the only procedure to lookup a symbol name,  but other
    routines scan the whole hash table to:
             write out the symbol table - 'printlabs'
             delete all the symbols - 'deletelabs'
             generate a loader symbol table - 'gen.defs'
       Users of the general assembler are allowed to invent their own types
    and must redefine the relevent procedures in order that the above procedures
    take account of their existence.
       All symbols are inserted using 'putlab', and there are several procedures
    for retrieving and investigating the symbols subsequently.  These procedures
    set various descriptive flags:
              'dontknow'  -- TRUE if the value sought is a forward reference
              'iden.valid'-- TRUE if the symbol exists here on this pass
       26.02.80

       The symbol table is in fact a hash table.  Words in the table are
    anchors of chains of elements of a given hash value, linked through
    the field 't.link'.  The hash table is obtained on the first call of
    the 'findentry' routine.  Space is got by a call of 'simplegetvec'.
       NOTE that the hashing routines rely on strings being padded up to
    a full word size with 0 bytes, i.e. 'standard' BCPL strings.
       (Hashing code added by Nick Ody).
       16.07.81
*/




STATIC
$( hash.table      = 0
   hash.table.size = 100
$)




MANIFEST
$(  t.type   = 0
    t.link   = 1
    t.val    = 2
    t.info   = 3
    tentrysize = t.info+1
    t.str    = tentrysize
/*  // types:  (for reference)
    type.none   = 0
    type.const  = 1
    type.var    = 2
    type.lab    = 3
    type.ref    = 4
    type.text   = 5
    type.macro  = 6
    type.       = type.macro+1
    type.mask   = byte1
    flag.double = bit8
    flag.rel    = bit9
    flag.def    = bit10
    flag.fwd    = bit11
    flag.needs  = bit12
    flag.weak   = bit13
    flag.temp   = bit14
    type.wref   = type.ref | flag.def
    type.def    = type.none | flag.def
    type.rellab = type.lab | flag.rel
    type.relvar = type.var | flag.rel
    type.relconst = type.const | flag.rel
*/
$)






LET findentry (string) = VALOF
$(  /*    This procedure is the only one responsible for returning a pointer
       to the place in the symbol table from which the symbol's record should be
       referred. (i.e. a pointer to a pointer).  Apart from the procedures
       which traverse the symbol table it is the only one to access it.
          Since the interface encourages multiple calls on this procedure
       with the same arguments (comparing its type with several others in
       different modules for example) the last reference to the symbol table
       is cached in 'dicposn'.
          The reference to 'lex.space' is because, in a higher module, all
       strings are held in this piece of store - I deny responsibility for it!
       'dicposn' is initialised to 0 before a new string is read into
       'lex.space'
          01.05.80
          The 'string' passed in should be padded to a full word with 0s;
       this is relied on by several routines.
          16.07.81
    */
    IF dicposn\=0 & string=lex.space THEN
      RESULTIS dicposn

    IF hash.table = 0 THEN    // First call - obtain space for table
    $(  hash.table.size := (memsize - memory ! 0) / 12
       // Remaining memory DIV 12 makes table about half full
       // Round size up to a prime
try.next.size:
       FOR i = 2 TO hash.table.size - 1 DO
       $(  IF hash.table.size REM i = 0 THEN
           $(  hash.table.size := hash.table.size + 1
               // Wasn't prime - try again
               GOTO try.next.size
           $)
       $)
       hash.table := simplegetvec(hash.table.size)

       FOR i = 0 TO hash.table.size DO hash.table ! i := null
    $)

    dicposn := hash.chain.anchor(string) - t.link
    WHILE t.link ! dicposn \= null DO    // Now search the hash list for a match
    $(  LET dic.string = t.str + t.link ! dicposn

        FOR i = 0 TO string % 0 / bytesperword DO
        $(  IF string ! i \= dic.string ! i THEN
                GOTO next.string         // Match failed - go round again
        $)

    BREAK                                // OK - exit from loop

next.string:

        dicposn := t.link ! dicposn
    $)
    dicposn := dicposn + t.link
    RESULTIS dicposn
$)





AND hash.chain.anchor(string) = VALOF
//   Returns the address (in the hash table) of the anchor of
// the hash chain corresponding to the given string.
//
//   The hashing algorithm takes the exclusive-or of all the
// words of the string, and uses this value REM the hash table size,
// which is arranged to be prime.  This method relies on the trailing
// bytes of the string being padded with zeroes.
$(  LET hashval =  string ! 0
    FOR i = 1 TO string % 0 / bytesperword DO hashval := hashval NEQV string ! i
    RESULTIS hash.table + (ABS hashval) REM hash.table.size
$)




AND getsymb(s) = VALOF
$(  LET t = !findentry(s)
    LET symty = (t=null->type.none, t!t.type&type.mask)
    LET rel.pos=(t=null-> 1, t!t.info-lineno)
    IF t=null | rel.pos>=0 |
       symty=type.ref  | symty=type.none | (t!t.type&flag.fwd)\=0 THEN
       dontknow:=TRUE
    IF symty=type.ref & rel.pos>=0 THEN warn(e.forward)
    iden.valid:= \(t=null-> pass=second,
                  (t!t.type&flag.fwd)\=0 & rel.pos>=0)
    RESULTIS t
$)




AND getlab(s, ansvec) = VALOF
$(  /* if symbol 's' is found 'ansvec' is filled with its value in
       offset 0, its type in offset 1 and the address where its value
       field is kept in offset 2.  It is also returned as the result
       of the call.. 'null' is returned if the symbol was not found
    */
    LET ans=getsymb(s)
    UNLESS ans=null THEN
    $(  ansvec!0 := ((ans!t.type&type.mask) >= type. ->
                     valtype((ans!t.type&type.mask), ans!t.val), ans!t.val)
        ansvec!1 := ans!t.type & \flag.double
        ansvec!2 := @ans!t.val
        ans:=ansvec
    $)
    RESULTIS ans
$)



AND putlab(string,v,ty) = VALOF
$(  /*   Inserts 'string' with value 'v' and type 'ty' into the
       dictionary.  Returns TRUE if 's' was a forewards reference
       and FALSE otherwise.  Allows sensible type conversions and
       value redefinitions.
         A problem, in practice, is that, for efficiency's sake, many symbols
       are only set during the first pass and not checked again on the second
       -- since error messages cannot be generated on the first pass double
       definition is not explicitly reported.
         A second factor to consider with respect to this code, and with the
       surrounding code, is that it is important to give similar results
       on both first and second passes in order to avoid differences in the
       flow of control between the two passes.
         27.02.80
         If a new symbol is inserted it is placed at the end of the
       hash chain, this being pointed to by the result of a call to 'findentry'.
         16.07.81
    */
    LET t       = findentry(string)
    LET ans     = ?
    LET rel.posn= ?
    LET symty   = ?

    TEST !t = null THEN
    $(  // pointer to dictionary element is null
        // element does not exist for this string so create it
        LET length = (string % 0) / bytesperword
        LET new.elem = simplegetvec(tentrysize + length)

        new.elem ! t.val  := v      // Fill in fields of new element
        new.elem ! t.info := lineno
        new.elem ! t.type := ty
        FOR i = 0 TO length DO (new.elem + t.str) ! i := string ! i
        new.elem ! t.link := null   // Link it into end of chain
        !t                := new.elem
        IF pass = second & (ty & flag.temp) = 0 THEN error(e.interror, 6)
RESULTIS TRUE
    $) ELSE t := !t

    IF t=null THEN              // entry was not defined before
    TEST pass=second & (ty & flag.temp) = 0 THEN error (e.interror, 6) ELSE
RESULTIS TRUE

    symty:=t!t.type&type.mask   // extract type number
    rel.posn := t!t.info-lineno
    ans:= rel.posn>=0 | symty=type.ref  // forwards reference ?
//  writef("** putlab(%S,%X4,%N) forward=%C t!ty=%N t!val=%N*N",
//         string,v,ty,(ans->'T','F'),t!t.type,t!t.val)
    TEST pass=second & (t!t.type&flag.double)\=0 THEN
      // flag.double is SET if this symbol was doubly defined
      // during the first pass (when the error could not be reported)
      error(e.doublelab, string) ELSE
    $(  // if symbol was DEF'd it can now be redefined to its new type
        IF (ty & flag.def)\=0 THEN
        $(  // it is legal to redefine a REF as a DEF but not vice versa
            TEST symty\=type.ref & rel.posn<0 THEN
            $(  t!t.type:=t!t.type | flag.double
                // use 'warn' so that first pass will do the same as the
                // second. (otherwise binary output might get out of phase)
                warn(e.doublelab, t+t.str)
            $) ELSE
            t!t.type:=(symty=type.ref -> type.def, t!t.type | flag.def)
            IF symty=type.ref THEN
            $(  // if symbol was a ref change variables completely to make
                // it look as if it were defined as a DEF here
                t!t.info:=lineno
                symty := t!t.type & type.mask
                rel.posn := 0
            $)
            // error if a DEF variable was never defined during the first pass
            IF pass=second & symty=type.none THEN error(e.baddef)
        $)
        // sort out whether any type coersion being done is legal or not
        // redefining a variable to 'type.none' has no effect
        // (type.none is used as part of the primitive type for DEF)
        UNLESS (ty & type.mask) = type.none THEN
        TEST (ty&type.mask)=type.ref & (pass=second | (ty&flag.needs)\=0) THEN
            // a NEEDS variable (type.ref with flag.needs) can be redefined
            // an unlimited number of times without changing their value
            IF symty=type.ref THEN
            TEST (ty&flag.needs)=0 THEN t!t.val:=v
            ELSE t!t.type:=t!t.type | flag.needs  // make it a NEEDS variable
        ELSE
        TEST VALOF SWITCHON symty INTO
        // can this type be redefined ?
        $(  CASE type.none:    RESULTIS TRUE
            CASE type.const:
            CASE type.lab:     RESULTIS rel.posn=0
            CASE type.macro:   RESULTIS FALSE
            DEFAULT:           RESULTIS (ty&type.mask)=symty
        $) THEN
        $(  // yes! coersion is legal: delete old value
            deletetype(t!t.type, t!t.val)
            // redefine dictionary record
            t!t.val:=v
            t!t.info:=lineno
            t!t.type:=ty  | (t!t.type & (flag.double | flag.def))
        $) ELSE
        $(  // no! coersion is illegal: delete redefining value
            deletetype(t!t.type, t!t.val)
            // mark as a double definition and try to give error message
            t!t.type:=t!t.type | flag.double
            warn(e.doublelab, string)
        $)
    $)
    RESULTIS ans                // TRUE if 's' was a forewards reference
$)





AND deletelab(s, ty) = VALOF
$(  // locates symbol "S" and deletes it
    // returns TRUE if the symbol was found
    LET entry = findentry(s)
    LET ans = (pass=first)
    UNLESS !entry = 0 THEN
    TEST ((!entry)!t.type & type.mask) = (ty & type.mask) THEN
    $(  deletetype((!entry)!t.type, (!entry)!t.val)
        !entry := (!entry)!t.link
        ans := TRUE
    $) ELSE ans := FALSE
    RESULTIS ans
$)






//      Types can be invented by the user of the general assembler.
// new type number's (to go in the 'type.mask' field of the type part of
// a dictionary entry) may start at 'type.'.
//      If new types are used the procedures 'freetype', 'valtype', 'savetype'
// (no longer used), and 'printtype' must be defined to delete an object,
// return a value to be used in an arithmetic expression for that object,
// print out an object's creating line of text, and print out the object's
// value in an 11 character field, respectively.  Each of these procedures
// takes the type and the value of such an object as parameter.
//      Predeclared types are already dealt with.




AND looktype(ty, s) = VALOF
$(  LET t=!findentry(s)
    // iden.valid is TRUE if 's' is of type 'ty' and already declared
    iden.valid:=(t=null | t!t.info >= lineno -> FALSE,(t!t.type&type.mask)=ty)
    RESULTIS (iden.valid->t!t.val, 0)
$)



AND checktype (ty, s) = VALOF
$(  LET sym=getsymb(s)
    LET ans=FALSE
    TEST sym=null THEN
    IF pass=first THEN
    $(  dontknow:=TRUE
        ans:=TRUE
    $)
    ELSE ans:=((sym!t.type&type.mask) = ty)
//  writef("Checktype : ans = %s*n",(ans->"TRUE","FALSE"))
    RESULTIS ans
$)



AND gettype(ty,s) = VALOF
$(  // produces an error is 's' is not type 'ty'
    // returns the value of the symbol
    LET ans=getsymb(s)
    IF ans=null & pass=second |
       ans\=null & (ans!t.type&type.mask)\=ty
    DO warn(e.badtype)
//  writef("gettype : ans=%X4, its type = %N, name='%S'*N",
//         ans, ans!t.type, ans+t.str)
    RESULTIS (ans=null -> -1, ans!t.val)
$)


AND valtype(ty, v) = VALOF            // for redefinition
$(  warn(e.notlab)
    RESULTIS -1
$)



AND deletetype(ty, val) BE
SWITCHON ty & type.mask INTO
$(  CASE type.none:
    CASE type.const:
    CASE type.var:
    CASE type.lab:
    CASE type.ref:
    CASE type.text:
    CASE type.macro:
      ENDCASE
    DEFAULT:
      freetype(ty, val)
$)



AND freetype(ty, val) BE RETURN         // for redefinition


AND writetype(ty, val) BE
$(  LET string = VALOF
    SWITCHON ty INTO
    $(  CASE type.none:    RESULTIS "undefined  "
        CASE type.const:   RESULTIS "const  %X4"
        CASE type.var:     RESULTIS "var    %X4"
        CASE type.lab:     RESULTIS "lab    %X4"
        CASE type.ref:
             TEST val=#XFFFF THEN
                           RESULTIS "ref  UNUSED"
             ELSE
                           RESULTIS "ref   @%X4"
        CASE type.text:
             val := val % 0
                           RESULTIS "text %I3 ch"
        CASE type.macro:
             val := val!2 // macro.chars
                           RESULTIS "mac%I5 ch"
        DEFAULT:
             printtype(ty, val)                    // User defined type
             RETURN
    $)
    writef(string, val)
$)



AND printtype(ty,val) BE writef("%I5 %I5",ty, val)     // for redefinition





AND printlabs() BE
$(  // This procedure completely rewritten to print symbols out in
    // alphabetic order from the hash table (previous data structure
    // was a tree).
    //    16.07.81
    LET number.of.symbols = 0
    LET chpos             = 0

    // Count entries in hash table
    FOR anchor = hash.table TO hash.table + hash.table.size DO
    $(  LET entry = ! anchor
        WHILE entry \= null DO
        $(  LET line.no = t.info ! entry
            IF allsyms | file.number(@line.no) = 0 |
               (t.type ! entry & flag.double) \= 0 THEN
               number.of.symbols := number.of.symbols + 1
            entry := t.link ! entry
        $)
    $)

    UNLESS number.of.symbols = 0 THEN
    $(  LET list     = concatenate.hash.list()   // Get all symbols
        LET rels     = FALSE     // flags for particular key items
        LET defs     = FALSE
        LET weakrefs = FALSE

        // Now sort the symbol list by calling sort.list
        $(  LET compare(elem1, elem2) = compstring(elem1 + t.str, elem2 + t.str)
            sort.list(@list, null, null, t.link, compare)
        $)
        // reserve number of lines necessary for cross reference and for
        // key to symbols that are to be used
        neads(number.of.symbols/((pw+5)/(32+3))+
                          (mode=relative->1,0)+1+3+(badlabs>0->1,0)+1+1)
        // neads will have set 'restartpage' if there are not enough lines to
        // the current output page.
        // the 'wrch' will only write a page throw if 'throws' is TRUE
        IF restartpage THEN wrch('*P')
        writef("*N*NNames available in section %N:*N*N",sectno)
        WHILE list \= null DO
        $(  LET line     = t.info ! list
            LET file.no  = file.number(@line)
            LET typecode = t.type ! list
            LET string   = t.str + list
            MANIFEST  $(  outfield = 32  $)
            IF chpos+outfield>pw THEN
            $(  wrch('*N')
                IF restartpage THEN wrch('*P')
                chpos:=0
            $)
            IF allsyms | file.no=0 | (typecode&flag.double)\=0 THEN
            $(  chpos:=chpos+outfield
                writef((file.no =  0 -> "   ",
                        file.no < 10 -> "+%I1 ",
                                        "+%I2"), file.no)
                writef("%I4 ",line)
                FOR i=1 TO 9 DO wrch(string%0 >= i -> string%i, ' ')
                wrch('*S')
                writetype(typecode&type.mask, t.val ! list)
                wrch((typecode&flag.double) = 0 -> '*S','**')
                wrch((typecode&flag.rel)    = 0 -> '*S','R')
                wrch((typecode&flag.def)   \= 0 -> 'D',
                     (typecode&flag.weak)  \= 0 -> 'W','*S')
                rels:=rels | (typecode & flag.rel)\=0
                TEST (typecode & flag.def)=0 THEN
                    IF (typecode & flag.weak)\=0 THEN weakrefs:=TRUE
                ELSE defs:=TRUE
                UNLESS chpos+2>pw THEN
                $(  writes("  ")
                    chpos:=chpos+2
                $)
            $)
            list := t.link ! list
        $)
        writes("*N*N*N")

        IF badlabs>0 THEN writes("** - multiply defined label*N")
        IF rels      THEN writes("R - relocatable value*N")
        IF defs      THEN writes("D - internally defined value*N")
        IF weakrefs  THEN writes("W - weak reference*N")
        wrch('*N')
    $)
$)



AND deletelabs() BE
$(  LET entry = concatenate.hash.list()   // Get all symbols
    WHILE entry \= null DO                // and run down list deleting each
    $(  deletetype(t.type ! entry & type.mask, t.val ! entry)
        entry := t.link ! entry
    $)
    hash.table := 0                       // Table will be freed, so mark it
$)                                        // so that a new one will be obtained





AND concatenate.hash.list() = VALOF
//   Concatenate all the hash lists together and hang them from
// the normally unused last hash table slot.
//   The routine may be called more than once on the same table
//   The value returned is the address of the first list element.
$(  // The <HASH conditional compilation comments are used to collect
    // statistics about the hashing algorithm.
////*<HASH statistics
///   LET size.incidence.vec = VEC 5
///   LET max.chain.size     = 0
////*HASH>*/

   IF hash.table ! hash.table.size \= null THEN
     // List concatenated already, just return it
     RESULTIS hash.table ! hash.table.size

////*<HASH
///   FOR i = 0 TO 5 DO size.incidence.vec ! i := 0
////*HASH>*/

   FOR anchor = hash.table TO hash.table + hash.table.size - 1 DO
   $(
////*<HASH
///      LET count = 0
////*HASH>*/

      IF ! anchor \= null THEN
      $(  LET entry = ! anchor
////*<HASH
///          count := 1
////*HASH>*/
          WHILE t.link ! entry \= null DO
          $(  // Find the last element in the chain
              entry := t.link ! entry
////*<HASH
///              count := count + 1
////*HASH>*/
          $)

          // Now link the old chain onto the end
          // and put the new chain in the complete chain head
          t.link ! entry               := hash.table ! hash.table.size
          hash.table ! hash.table.size := ! anchor
          ! anchor := null
      $)

////*<HASH
///        IF count > max.chain.size THEN max.chain.size := count
///        IF count >= 5 THEN count := 5
///        size.incidence.vec ! count := size.incidence.vec ! count + 1
////*HASH>*/
    $)

////*<HASH
///    writef("Hash table size: %N,  chain length incidences:*N", hash.table.size)
///    FOR i = 0 TO 4 DO writef("  %N:  %N*N", i, size.incidence.vec ! i)
///    writef(">=5:  %N*N*
///           *longest: %N*N", size.incidence.vec ! 5, max.chain.size)
////*HASH>*/
    RESULTIS hash.table ! hash.table.size
$)



AND gen.defs() BE
// goes through the dictionary generating external/internal reference
// records in the output format for the relevent variables (REF & DEF)
$(  code.gen(cd.clear)
    FOR anchor = hash.table TO hash.table + hash.table.size DO
    $(  LET t = ! anchor
        WHILE t\=null DO
        $(  LET typecode = t.type ! t
            LET value    = t.val ! t
            LET string   = t.str + t

            IF (typecode&flag.double)\=0 THEN badlabs:=badlabs+1
            TEST (typecode & type.mask)=type.ref THEN
                IF (typecode&flag.needs)\=0 | value\=#XFFFF THEN
                    // i.e. never used!
                    code.gen( ((typecode&flag.weak)=0->cd.ext,cd.wext),
                              string, value /* last referance */)
            ELSE
               IF (typecode & flag.def)=flag.def &
                   (typecode&type.mask)\=type.none THEN
                   TEST (typecode & flag.rel)=0 & mode=relative
                   THEN code.gen(cd.absint, string, value /* address */)
                   ELSE code.gen(cd.int, string, value /* address */)
            t := t.link ! t
        $)
    $)
$)



AND sort.list(first.elem.ptr, last.elem.link, null.elem, link.field, compare) BE
//
//   Sort the list a pointer to the first element of which is given.
//
//   The list is single linked through the given link field and
// the null element reference is given in 'null.elem'.
//
//   The list is sorted using a user supplied comparison routine.
//
//   The comparison routine should accept 2 list elements as arguments.
// Sorting is such that for any two adjacent elements En and En+1 in
// the final list:
//                  compare(En, En+1) <= 0
//
//   For any two elements where compare(Em, En) = 0, their order in the
// final list will be the same as in the original list.
//
//   WARNING.    This routine uses the quicksort algorithm so
//               will be poor for an already-sorted list.
//
//   The first element of the sorted list will be inserted into
// the first element pointer, and 'last.elem.link' assigned to
// the link field of the last element of the sorted list.

WHILE TRUE DO
$(  LET first.elem  = ! first.elem.ptr
    LET sublist1    = ?
    LET sublist2    = ?
    LET length.diff = ?

    IF first.elem = null.elem THEN                 // Empty list
    $(  !first.elem.ptr := last.elem.link
        RETURN
    $)

    sublist1 := link.field ! first.elem
    sublist2 := null.elem
    IF sublist1 = null.elem THEN
    $(  link.field ! first.elem := last.elem.link
        RETURN
    $)

    $(  LET entry         = sublist1
        LET prev.entry    = @sublist1 - link.field
        LET sublist2.tail = @sublist2 - link.field

        length.diff := 0

        // Now pass through sublist1 removing all elements larger than
        // first.elem and placing them on sublist2.
        WHILE entry \= null.elem DO
        $(  TEST compare(entry, first.elem) > 0 THEN
            $(  LET next.entry = link.field ! entry

                // Remove element from sublist1
                link.field ! prev.entry := next.entry

                link.field ! sublist2.tail := entry     // Append to sublist2
                link.field ! entry         := null.elem
                sublist2.tail              := entry

                entry := next.entry
                length.diff := length.diff - 1
            $) ELSE
            $(  prev.entry := entry
                entry      := link.field ! entry
                length.diff := length.diff + 1
            $)
        $)
    $)

    // Now sort the shorter of the two sublists and
    // loop round to deal with the longer.
    TEST length.diff > 0 THEN
    $(  link.field ! first.elem := sublist2

        sort.list(first.elem + link.field, last.elem.link,
                               null.elem, link.field, compare)

        last.elem.link   := first.elem
        ! first.elem.ptr := sublist1
    $) ELSE
    $(  ! first.elem.ptr := sublist1

        sort.list(first.elem.ptr, first.elem,
                               null.elem, link.field, compare)

        first.elem.ptr   := first.elem + link.field
        ! first.elem.ptr := sublist2
    $)
$)







///SECTION "asm3"




////*<RSX
///GET "libhdr"
///GET "asmhdr"
////*RSX>*/


////*<CAP
///GET ".**.l.bcpl.libhdr"
///GET ".**.cgg.asmhdr"
////*CAP>*/

////*<IBM
///GET "LIBHDR"
///GET "ASMHDR"
////*IBM>*/

/////*<TRIPOS:
///GET "libhdr"
///GET "GRASM:asmhdr"
////*TRIPOS>*/



















//
//                    Loader   Output   Module
//






/*       This module is responsible for providing facilities for the
   generation of a load file -- The default format of such a file is
   an extension of Mostec style Intel hex  (see the Mostec Z80 assembler
   reference manual appendix C for a complete definition) called Cambridge Hex.
   Cambridge Hex is defined elsewhere.
         The chosen format is generally applicable and will be generalised
   for use on 16 bit machines.  It can be changed by assigning 'code.gen'
   to some other procedure which supports the same entry reasons (see
   'intel.hex'.
         'putword' and 'putaddress' can be used for the direct generation
   of absolute code.  'putlabelspec' has a more complicated argument and will
   generate external, relocatable or absolute data.  The object taken as an
   argument is generated in a vector of size 'spec.size' in the complimentary
   procedure 'labelexpression' declared in another module.
      26.02.80
*/





/*     The following are kept for reference & documentation
GLOBAL
$(     binbuf   : ioman+3       //  buffer for binary produced by that line
       pc       : ioman+6       //  contains expected value of program counter
$)
*/






LET fitsmask(n,m) = (n>=0 -> (n&\m)=0, (\(n&\m|m))=0 )


LET putword(n) BE
$(  // bytesperasmword is the number of bytes in the object machine's word
    // (i.e. the length of information corresponding to an increment in
    // address).  'bytesperword' is the number of bytes in a word on the
    // machine that we are executing on.
    LET m=(1<<(bytesperasmword*8))-1
    IF binbuf!b.nextmod=binbuf!b.top DO
       $(  IF binbuf!b.top >= binbufsize+b.nextmod DO error(e.binfull)
           binbuf!b.top:=binbuf!b.top+1
       $)
    binbuf!b.nextmod:=binbuf!b.nextmod+1
    binbuf!(binbuf!b.nextmod):=n & m
    UNLESS fitsmask(n,m) DO warn(e.badbyte)
$)



AND putaddress(n) BE
$(  LET m=(1<<(8*bytesperasmword))-1
    LET sign=n&\((1<< (8*wordsperaddress*bytesperasmword) )-1)
    FOR i=1 TO wordsperaddress DO
    TEST ~msbytefirst THEN
    $(  putword(m&n)
        m:=m << (8*wordsperaddress)
    $) ELSE
    putword((n >> (8*bytesperasmword*(wordsperaddress-i))) & m)
    UNLESS sign=0 | sign=(-1&(1 << (8*wordsperaddress*bytesperasmword) )-1) THEN
    error(e.badbyte)
$)



AND movebinword(places) BE
/*     This procedure will be rarely used:  it repositions the pointer to the
   binary buffer of bytes generated so far on this line 'binbuf' so that
   reference can be made to bytes formerly generated.
*/
TEST b.nextmod<binbuf!b.nextmod+places<=binbuf!b.top
THEN binbuf!b.nextmod:=binbuf!b.nextmod+places
ELSE error(e.mtbuf)



AND outhex(n) BE FOR i=bytesperasmword-1 TO 0 BY -1 DO
   writehex((n>>(i*8)) & byte1, 2)





AND code.gen(type, a1, a2) =

/// //-*<CHEX
    intelhex(type, a1, a2)
/// /-*CHEX>*/

/// /-*<TIMBIN
///    timbin(type, a1, a2)
/// /-*TIMBIN>*/

// this routine can be redefined in the user's part of the assembler
// to generate a different output format, the 'type's are as follows:
//    cd.undef   sets undefined byte filler
//    cd.clear   clears internal buffers
//    cd.newpc   changes loader's pc to 'arg1'
//    cd.data    outputs data byte 'arg1'
//    cd.eof     tidies up loader file (binout)
//    cd.int     declares internal symbol 'arg1'
//               at address 'arg2'
//    cd.ext     declares external symbol 'arg1'
//               last used at address 'arg2'
//    cd.rel     declares word at address 'arg1'
//               as relocatable
//    cd.module  declares module name 'arg1'
//    cd.code    declares code type 'arg1' with
//               'arg2' bytes per word
//    cd.absint  declares absolute internal symbol
//               'arg1' at address 'arg2'
//    cd.wext    declares weak external symbol
//               'arg1' last used at address 'arg2'
//    cd.opt     linker option string 'arg1'
//               interpreted at level 'arg2'
//    cd.prag    name of pragmat is 'arg1' text is
//               'arg2'



/// //-*<CHEX
AND intelhex(type, arg, arg1) BE
// this procedure generates "Cambridge Hex", see definition.
// It is compatable with "Intel Hex" if records begining '$' are
// taken as comments, as recomended.  Hence the code output
// should be able to be loaded by any standard Intel Hex loader.
// (this only applies to 8 bit machines).
UNLESS binfile=0 | pass=first THEN
$(
    LET hex=TABLE 0,  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
                      0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
    LET rel=TABLE 0,  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
                      0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
    STATIC
    $(  beginblock = 0     // begining of next block for map of store
        oldpc = -1         // calculated value of current pc
        pc.valid = FALSE   // FALSE until first pc is set
        first.valid = FALSE// last pc was first valid pc set
        overflowing = FALSE// TRUE when about to run out of address space
    $)
    LET alpharec(str, type, info) BE
    $(  // generates a checksummed record with string 'str' in the field
        // from columns 2 to 5 with record type 'type'.
        // 'info' represents the information necessary for the body of the
        // record.
        LET sum = type+(info&byte1)+((info&byte2) >> 8)    // checksum
        selectoutput(binfile)
        IF str=0 THEN str:="NoName"
        wrch('$')
        FOR i=1 TO 6 DO
        $(  LET ch=(i>str%0 -> '*S', str%i)
            wrch(ch)
            sum:=sum+cvtchar(ch)
        $)
        TEST type=cd.code   THEN writef("%X2%X4", type, info) ELSE
        TEST type=cd.module THEN writef("%X2%X2", type, info) ELSE
        $(  LET b=8*bytesperasmword
            LET m=(1<<b)-1
            writehex(type, 2)
            FOR i=wordsperaddress-1 TO 0 BY -1 DO
            outhex((info>>(i*b))&m)
        $)
        outhex(-sum)
        wrch('*N')
    $)
    LET clearbuf(hex, type) BE
    $(  // clears one of the buffers kept in this routine ('hex' or 'rel').
        // this buffer is pointed to by 'hex'.
        // generates a data or relocating information record depending upon
        // 'type'.
        LET blocklen=(type=cd.data -> hex!0, hex!0/2)
        selectoutput(binfile)
        IF pc.valid | blocklen>0 THEN
        $(  IF blocklen\=0 | type=cd.data THEN
            $(  LET blockaddr=(type=cd.data -> oldpc-blocklen, 0)
                LET sum=blocklen+(blockaddr >> 8)+(blockaddr & #XFF)+type
                wrch(type=cd.data -> ':','$')
                writehex(blocklen,2)
                writehex(blockaddr, 4)
                writehex(type, 2)
                FOR i=1 TO hex!0 DO
                $(  sum:=sum+hex!i
                    outhex(hex!i)
                $)
                outhex(-sum)
                wrch('*N')
            $)
            hex!0:=0
        $)
    $)
    LET saveout=output()
    LET savewrch=wrch
    wrch:=syswrch
    SWITCHON type INTO
    $(  CASE cd.undef:
             // 'arg' is the value for the byte used for undefined output
             ENDCASE
        CASE cd.clear:
            // clear internal buffers
            UNLESS hex!0=0 THEN clearbuf(hex, cd.data)
            clearbuf(rel, cd.rel)
            selectoutput(saveout)
            ENDCASE
        CASE cd.newpc:
            // program counter has changed
            UNLESS oldpc=arg THEN
            $(  TEST pc.valid THEN
                $(  LET s1=(arg>=0 -> +1,-1)
                    LET s2=(oldpc>=0 -> +1,-1)
                    IF hex!0>0 | (s1=s2->s1*(arg-oldpc),s2)<0 | first.valid THEN
                    clearbuf(hex, cd.data)
                    first.valid:=FALSE
                $) ELSE first.valid:=TRUE
        //      UNLESS mapfile=0 THEN
        //        $(  selectoutput(mapfile)
        //            writef("%X4 - %X4  (%N)*N",beginblock,
        //                   oldpc,oldpc-beginblock)
        //            beginblock:=arg
        //        $)
                oldpc:=arg
                selectoutput(saveout)
            $)
            pc.valid := TRUE
            ENDCASE
        CASE cd.data:
            $(  // output 'arg' to the intel hex file
                IF overflowing THEN error(e.overflow)
                hex!0:=hex!0+1
                hex!(hex!0):=arg
                overflowing := (oldpc = #XFFFF)
                // #XFFFF should be the following really:
                // (1 << (bytesperasmword * wordsperaddress)) - 1
                oldpc := oldpc + 1
                IF hex!0>=hexoutwidth THEN
                $(  clearbuf(hex, type)
                    selectoutput(saveout)
                $)
            $)
            ENDCASE
        CASE cd.eof:
            // arg is the programs start address
            IF overflowing & hex!0=0 THEN oldpc := #XFFFF
            clearbuf(hex, cd.data)
            clearbuf(rel, cd.rel)
            writef(":00%X401", arg)
            outhex( -(1+(arg&byte1)+((arg&byte2)>>8)) )
            wrch('*N')
            selectoutput(saveout)
            ENDCASE
        CASE cd.int:
        CASE cd.absint:
        CASE cd.wext:
        CASE cd.ext:
            // 'arg' holds the name of the symbol and:
            // 'arg1' is the address of the internal symbol, or
            // 'arg1' is the last symbol that used the external symbol
            alpharec(arg, type, arg1)
            selectoutput(saveout)
            ENDCASE
        CASE cd.rel:
            // store 'arg' as address in 'rel' buffer for relocation info
            rel!0 := rel!0+2
            rel!(rel!0-1) := (arg&byte2) >> 8
            rel!(rel!0)   := (arg&byte1)
            IF rel!0>=hexoutwidth THEN
            $(  clearbuf(rel, cd.rel)
                selectoutput(saveout)
            $)
            ENDCASE
        CASE cd.module:
            // 'arg' is TRUE if module is absolute FALSE otherwise,
            // 'arg1' is the name of the new module
            clearbuf(hex, cd.data)
            clearbuf(rel, cd.rel)
            alpharec(arg1, cd.module, (arg->0,1))
            pc.valid := FALSE
            selectoutput(saveout)
            ENDCASE
        CASE cd.code:
            // 'arg' is the name of the object machine
            // 'arg1' is packed as follows:
            //          bits 15 - 12:  bytes per addressable unit
            //          bits 11 -  8:  bytes per address
            //          bits 7  -  0:  flags (bit 0 = MSB first)
            UNLESS hex!0=0 THEN clearbuf(hex)
            clearbuf(rel, cd.rel)
            alpharec(arg, cd.code, arg1)
            selectoutput(saveout)
            ENDCASE
        CASE cd.opt:
            // 'arg' is the option string to be given
            // 'arg1' is the level of the option
            $(  LET sum= -type-(arg1 & #XFF) - arg%0
                selectoutput(binfile)
                FOR i=1 TO arg%0 DO sum:=sum-cvtchar(arg%i)
                writef("$%X2%X20011%S%X2*N",arg%0,arg1,arg,sum)
                selectoutput(saveout)
            $)
            ENDCASE
        CASE cd.prag:
            // 'arg' is the name of the pragmat begin given
            // 'arg1' is the pragmat string being generated
            $(  LET sum= -type-arg%0
                selectoutput(binfile)
                FOR i=1 TO arg%0 DO sum:=sum-cvtchar(arg%i)
                wrch('$')
                FOR i=1 TO 6 DO
                $(  LET ch=(i>arg1%0->'*S',arg1%i)
                    sum:=sum-cvtchar(ch)
                    wrch(ch)
                $)
                writef("%X2%X2%S%X2*N",type,arg%0,arg,sum)
                selectoutput(saveout)
            $)
            ENDCASE
        DEFAULT:
            error(e.interror, 8)
    $)
    wrch:=savewrch
$)
/// /-*CHEX>*/




/// /*<TIMBIN
AND timbin(type, arg, arg1) BE
// this procedure generates "TIMBIN".
// It is roughly compatable with TRIPOS binary, see definition.
UNLESS binfile=0 THEN
$(  MANIFEST $(  relbuf.size = 32  $)
    LET rel=TABLE 0,  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
                      0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
    STATIC
    $(  oldpc = -1            // calculated value of current pc
        pc.valid = FALSE      // FALSE until first pc is set
        first.valid = FALSE   // last pc was first valid pc set
        absolute.module=TRUE  // TRUE when the current module is absolute
        block.len = 0         // length of current data block
        block.list = null     // list of data block lengths
        block.list.top = null // top entry in the block list
        undef = 0             // value of undefined bytes
        last.end.out = 0      // last address in last block output
        pending.byte = -1     // -1 if no byte pending otherwise the byte
    $)
    LET abs.cmp(v1, v2) = VALOF
    $(  LET s1 = (v1>=0 -> +1, -1)
        LET s2 = (v2>=0 -> +1, -1)
        RESULTIS (s1=s2 -> s1*(v1-v2), s2)
    $)
    LET writeword(word) BE
    $(  wrch(word>>8)
        wrch(word & #XFF)
    $)
    LET clearbuf(rel) BE
    $(  // clears the buffer 'rel'
        // generates a relocating information record
        LET blocklen = rel!0
        IF pc.valid | blocklen>0 THEN
        $(  LET saveout = output()
            selectoutput(binfile)
            IF blocklen\=0 THEN
            $(  LET base = (block.list.top=null -> 0, block.list.top!2)
                writeword(1010)
                writeword(blocklen)
                FOR i=1 TO rel!0 DO writeword(rel!i-base)
            $)
            rel!0:=0
            selectoutput(saveout)
        $)
    $)
    LET new.buf() BE
    $(  // record new block
        TEST block.list.top=null THEN   // list it empty
        $(  block.list.top := simplegetvec(2)
            block.list := block.list.top
        $) ELSE
        $(  block.list.top!0 := simplegetvec(2)
            block.list.top := block.list.top!0
        $)
        block.list.top!0 := null
        block.list.top!1 := block.len
        block.list.top!2 := oldpc-block.len
        block.len := 0
    $)
    LET end.block() BE
    UNLESS block.list.top=null THEN
    $(  IF pending.byte>=0 THEN
        $(  LET saveout = output()
            selectoutput(binfile)
            writeword(absolute.module -> 1003, 1000)
            IF absolute.module THEN
            writeword(block.list.top!1 + block.list.top!2 - 1)
            writeword(1)
            writeword((pending.byte<<8) | (undef & #XFF))
            selectoutput(saveout)
            pending.byte := -1
        $)
        block.list.top := block.list.top!0
    $)
    LET begin.block() BE
    UNLESS block.list.top=null THEN
    IF pc.valid | block.list.top!1>0 THEN
    $(  LET blockaddr = block.list.top!2
        LET fore.block = (\absolute.module &
                          abs.cmp(blockaddr, last.end.out) > 0 ->
                          blockaddr - last.end.out, 0) +
                        (pending.byte>=0 -> 1,0)
        LET blocklen = block.list.top!1 + fore.block
        blockaddr := blockaddr - fore.block
//      writef("TIMBIN: next block %X4 - %X4 (fore block size %X4)*N",
//            blockaddr, blockaddr+blocklen, fore.block)
        UNLESS blocklen=0 & \absolute.module THEN
        $(  // allow zero length records for absolute records
            // (since they provide the information that the assembler
            // has visited that location) but not for relocatable
            // ones (since there is not location field).
            LET saveout = output()
            selectoutput(binfile)
            writeword(absolute.module -> 1003, 1000)
            IF absolute.module THEN writeword(blockaddr)
            writeword(blocklen/2)
            UNLESS absolute.module THEN
            $(  block.list.top!1 := blocklen
                block.list.top!2 := blockaddr  // to account for FORE.BLOCK
                IF pending.byte>=0 THEN wrch(pending.byte)
                FOR i=1 TO (fore.block & (\1)) - (pending.byte>=0 -> 1, 0) DO
                wrch(undef)
                pending.byte := ((fore.block&1)=1 -> undef, -1)
            $)
            last.end.out := blockaddr+blocklen
            selectoutput(saveout)
        $)
    $)
    LET savewrch=wrch
    wrch:=syswrch
    TEST pass=first THEN
    SWITCHON type INTO
    $(  CASE cd.undef:
            // 'arg' holds value for byte used for undefined code
            undef := arg
            ENDCASE
        CASE cd.clear:
            // clear internal buffers
//          writef("TIMBIN: line %i4 CLEAR*N", line.of.file)
            new.buf()
            rel!0 := 0
            ENDCASE
        CASE cd.newpc:
            // program counter has changed
//          writef("TIMBIN: line %I4 NEWPC %X4 (old PC = %X4)*N",
//                 line.of.file, arg, oldpc)
            UNLESS oldpc=arg THEN
            $(  TEST pc.valid THEN
                $(  LET s1=(arg>=0 -> +1,-1)
                    LET s2=(oldpc>=0 -> +1,-1)
                    IF block.len>0 | (s1=s2->s1*(arg-oldpc),s2)<0 |
                       first.valid THEN new.buf()
                    first.valid:=FALSE
                $) ELSE first.valid:=TRUE
                oldpc:=arg
                pc.valid:=TRUE
            $)
            ENDCASE
        CASE cd.data:
//          writef("TIMBIN: line %I4 DATA %X2 (buffer size = %N)*N",
//                 line.of.file, arg, block.len)
            block.len := block.len+1
            oldpc:=oldpc+1
            ENDCASE
        CASE cd.rel:
            // store 'arg' as address in 'rel' buffer for relocation info
//          writef("TIMBIN: line %I2 RELDATA %X4 (pc = %X4)*N",
//                 line.of.file, arg, oldpc)
            rel!0 := rel!0+1
            IF rel!0>=relbuf.size THEN
            $(  new.buf()
                rel!0 := 0
            $)
            ENDCASE
        CASE cd.module:
            // 'arg' is TRUE if module is absolute FALSE otherwise,
            // 'arg1' is the name of the new module
//          writef("TIMBIN: line %I4 MODULE*N", line.of.file)
            block.list := null
            block.list.top := null
            block.len := 0
            rel!0 := 0
            pc.valid := FALSE
            ENDCASE
        DEFAULT: ENDCASE
    $) ELSE   //  pass two
    SWITCHON type INTO
    $(  CASE cd.clear:
            // clear internal buffers
            end.block()
            clearbuf(rel)
            begin.block()
            ENDCASE
        CASE cd.newpc:
            // program counter has changed
            UNLESS oldpc=arg THEN
            $(  TEST pc.valid THEN
                $(  IF block.list.top!1>0 | abs.cmp(arg,oldpc) |
                    first.valid THEN
                    $(  TEST absolute.module THEN end.block() ELSE
                        block.list.top := block.list.top!0
                        clearbuf(rel)
                        begin.block()
                    $)
                    first.valid:=FALSE
                $) ELSE first.valid:=TRUE
                oldpc:=arg
                pc.valid:=TRUE
            $)
            ENDCASE
        CASE cd.data:
            $(  // output 'arg' to the intel hex file
                LET saveout = output()
                selectoutput(binfile)
                TEST pending.byte>=0 THEN
                $(  writeword((pending.byte<<8) | (arg&#XFF))
                    pending.byte := -1
                $) ELSE pending.byte := arg
                selectoutput(saveout)
                oldpc:=oldpc+1
            $)
            ENDCASE
        CASE cd.eof:
            $(  // arg is the programs start address
                LET saveout = output()
                selectoutput(binfile)
                writeword(1002)
                selectoutput(saveout)
            $)
            ENDCASE
        CASE cd.int:
        CASE cd.absint:
        CASE cd.wext:
        CASE cd.ext:
            // 'arg' holds the name of the symbol and:
            // 'arg1' is the address of the internal symbol, or
            // 'arg1' is the last symbol that used the external symbol
            error(e.badloaddir)
            ENDCASE
        CASE cd.rel:
            // store 'arg' as address in 'rel' buffer for relocation info
            rel!0 := rel!0+1
            rel!(rel!0)   := arg
            IF rel!0>=relbuf.size THEN
            $(  block.list.top := block.list.top!0
                clearbuf(rel, cd.rel)
                begin.block()
            $)
            ENDCASE
        CASE cd.module:
            // 'arg' is TRUE if module is absolute FALSE otherwise,
            // 'arg1' is the name of the new module
            absolute.module := arg
            pc.valid := FALSE
            last.end.out := 0             // address of end of last block
            block.list.top := block.list  // rewind to begining of the list
            begin.block()
            ENDCASE
        CASE cd.code:
            // 'arg' is the name of the object machine
            // 'arg1' is the number of bytes per word used (1)
            ENDCASE
        CASE cd.opt:
            // 'arg' is the option string to be given
            // 'arg1' is the level of the option
            error(e.badloaddir)
            ENDCASE
        CASE cd.prag:
            // 'arg' is the name of the pragmat begin given
            // 'arg1' is the pragmat string being generated
            ENDCASE
        DEFAULT:
            error(e.interror, 8)
    $)
    wrch:=savewrch
$)
/// /*TIMBIN>*/




AND putlabelspec(spec) BE
$(  /*    'spec' is a 3 element vector which is the result of parsing
       some label expression (see 'label.expression').  Offset 0 contains
       the value obtained.  Offset 1 can be 0 or 1 (and other values if
       the parsed expression was illegal or an external), 0 implies that
       the value is absolute.  Offset 2 is zero unless the expression was
       an external in which case it will contain the address of the external
       symbol's value field.
    */
    LET local.pc = pc+binbuf!b.top-b.nextmod
    TEST spec!2=0 THEN putaddress(spec!0) ELSE putaddress(!(spec!2))
    IF spec!1\=0 /* i.e. relocatable or external */ THEN
    TEST spec!2=0 /* not external */ THEN code.gen(cd.rel, local.pc)
    ELSE !(spec!2) := local.pc
$)











///SECTION "asm4"




////*<RSX
///NEEDS "TITLE"
///GET "libhdr"
///GET "asmhdr"
////*RSX>*/


////*<CAP
///GET ".**.l.bcpl.libhdr"
///GET ".**.cgg.asmhdr"
////*CAP>*/

////*<IBM
///GET "LIBHDR"
///GET "ASMHDR"
////*IBM>*/

/////*<TRIPOS:
///GET "libhdr"
///GET "GRASM:asmhdr"
////*TRIPOS>*/





//
//                  Character  Input  and  Output
//





/*  The following are kept for reference:
GLOBAL
$(     linbuf   : ioman+1       //  buffer for text of assembly line
       linbuf.out:ioman+2       //  next char no to be taken from linbuf
       ch       : ioman+5       //  last character read from the input file
       pagetitle: ioman+7       //  title string for the top of the page
       linbuf.top:ioman+8       //  top of line buffer pointer
$)
*/




STATIC $( saved.rdch = 0 $)





LET deleteline() BE
$(  linbuf.top := 0
    linbuf.out := 0
$)


AND putch(ch) BE
UNLESS linbuf.top >= linbufsize DO
   $(  linbuf.top := linbuf.top + 1
       linbuf.out:=linbuf.out+1
       linbuf%(linbuf.top) := ch
   $)



AND position.ch(pos) BE
$(  LET delim = linbuf%linbuf.top
    IF delim='*S' THEN   // attempt to stop layout justification in silly places
    TEST pos < linbuf.top THEN
    $(  LET n = linbuf.top - pos
        LET savepos = ?
        WHILE linbuf%(linbuf.top-1) = '*S' & n>0 DO
        $(  n := n-1
            linbuf.top := linbuf.top - 1
        $)
        linbuf%linbuf.top := delim
        savepos := linbuf.top
        WHILE ch='*S' & n>0 DO
        $(  rch()
            n := n-1
        $)
        // leave at least one space between this item and next non blank
        // character if possible
        TEST ch\='*S' & linbuf.top>savepos THEN linbuf.top := savepos+1 ELSE
        linbuf.top := savepos
        linbuf%linbuf.top := ch
    $) ELSE
    $(  FOR i=linbuf.top TO (pos-1>linbufsize -> linbufsize, pos-1) DO
        $(  linbuf%i := '*S'
            linbuf.top := linbuf.top + 1
        $)
        UNLESS linbuf.top > linbufsize THEN linbuf%linbuf.top := delim
    $)
$)




AND tab.rdch() = VALOF
$(  // this routine is used for 'rdch' when tabs are being expanded.
    // having a separate routine saves a test on a global for each
    // call of 'rdch': this results in a significant difference in speed.
    // the number of times that '*S' must be returned is kept in
    // 'linbuf.out' and is decremented on each call.  The original 'rdch'
    // is reenstated when the tab has been fully expanded.
    linbuf.out:=linbuf.out-1
    IF linbuf.out<=0 THEN
    $(  rdch := saved.rdch
        saved.rdch := tab.rdch
    $)
    RESULTIS '*S'
$)



AND rch() BE
$(  // the assembler spends a significant amount of time in this routine so
    // nothing must be done to slow it down.  It uses 'rdch' to read characters
    // and it accumulates them in the buffer 'linbuf', a pointer to the current
    // character of which is held in 'linbuf.top'.
    // Tabs are tested for and expanded on input: this being the proper place
    // to do them!
    // 'linbuf' is not used for any other purpose other than printing out in
    // the listing.  Hence its truncation when 'linbuf.top' reaches the set
    // value of 'linbufsize' represents the truncation of the listing set by
    // the P option.  (see initialisation in which 'linbufsize' is calculated
    // in terms of this value).
    ch:=rdch()
    IF ch='*T' THEN
    $(  linbuf.out := 7 - ((linbuf.top) REM 8)
        ch := '*S'
        UNLESS linbuf.out=0 THEN
        $(  saved.rdch := rdch
            rdch := tab.rdch
        $)
    $)
    UNLESS linbuf.top>=linbufsize DO
    IF print.expansions THEN
    $(  linbuf.top := linbuf.top + 1
        linbuf%(linbuf.top):=ch
    $)
    UNLESS filling.macro=0 THEN macro.wrch(filling.macro, ch)
$)






AND error(arg1,arg2,arg3,arg4) BE
$(

    /*  Note:  THIS  PROCEDURE  PRODUCES  A  DYNAMIC  GOTO
               SO  THAT  EXECUTION  CONTINUES
               AT  "ERRORLABEL"  ON  EXIT
    */

    // This routine is called either because there has been a 'warning'
    // or because there was a genuine error.  The difference being that
    // in a warning, the arguments are kept in 'warnvec' until a line is
    // assembled and then used in 'error' so that the normal processing
    // of the line is not affected.
    // If the routine is called as an 'error' when a warning is already
    // outstanding it is the warning that is printed: only one message
    // ever gets printed for any particular line.
    // If 'arg1' (the error code) is negative the error is judged to be
    // fatal and the flag 'finishpass' is set TRUE to terminate the
    // processing of the assembly.  It also writes a message to the initial
    // output stream of the program.
    // Error messages for the error codes are given by 'geterror'
    // This routine and 'outline' are the main two routines which write
    // to the output listing file.
    LET pos=item.start          // forward reference
    LET crosses = "**************************  "
    errcount:=errcount+1
//  writef("jumping to level %N, to label %N*N",errorlevel,errorlabel)
    TEST arg1<0
    THEN $(  finishpass:=TRUE
             fatal:=TRUE
         $)
    ELSE IF warning DO
            $(  arg1:=warnvec!1
                arg2:=warnvec!2
                arg3:=warnvec!3
                arg4:=warnvec!4
                warning:=FALSE
                pos:=warnvec!0
//              writef("Error is warning , pos=%N*N",pos)
            $)
    TEST pass=first & \finishpass
    THEN outline('*S', TRUE)
    ELSE $(  LET savelist=list
             LET charsperasmword = 2*bytesperasmword + 1
             LET error.message = geterror(arg1)
             neads(short->2,3)
             IF restartpage THEN wrch('*P')
             UNLESS fatal DO
                $(  list:=1
                    outline('**', TRUE)
                    list:=savelist
                $)
             TEST pass=first
             THEN writes("*N*N*N")
             ELSE UNLESS short DO
                     $(  writes(crosses)
                         FOR i=1 TO binbufwidth*charsperasmword+2+pos
                         DO wrch('*S')
                         writes("|*N")
                     $)
             writes(crosses)
             FOR i=1 TO binbufwidth*charsperasmword+2 DO wrch('*S')
             writef("%C Error: ",comntch)
             TEST error.message=0 THEN
             writef("number %N") ELSE
             writef(error.message,arg2,arg3,arg4)
             wrch('*N')
             IF finishpass DO
                $(  LET saveout=output()
                    selectoutput(out)
                    writef("*N*NFatal error (file %N line %N): ",
                           file.id, line.of.file)
                    TEST error.message = 0 THEN
                    writef("number %N", -arg1) ELSE
                    writef(error.message,arg2,arg3,arg4)
                    wrch('*N')
                    selectoutput(saveout)
                $)
         $)

    longjump(errorlevel,errorlabel)

$)


AND warn(arg1, arg2, arg3, arg4) BE UNLESS warning DO
   $(  warning:=TRUE
       warnvec!1:=arg1
       warnvec!2:=arg2
       warnvec!3:=arg3
       warnvec!4:=arg4
       warnvec!0:=item.start
//     writef("Warn called: warnvec!0=%N*N",warnvec!0)
   $)


//  AND executeanywarning() BE IF warning DO error(0)





AND outline(firstchar, print) BE
$(  // This procedure is produces all the lines of output for a line of
    // assembler input.  It also deals with '*P' and '*N' characters
    // generated by appropriate directives.
    STATIC $( line.just.outlined = -1 $)
    // This is used to ensure that the same line is not written out when two
    // successive calls of OUTLINE are made and none to NEWLINE in the mean-
    // time.  It assumes that LINENO -1 will never occur.
    // First the rest of the line is read so that it will all be accumulated
    // in 'linbuf' for output:
    WHILE ('*N'\=ch\=endstreamch) DO rch()
    // don't output anything on the first pass!
    TEST pass=second & (lineno \= line.just.outlined) THEN
    $(  // don't generate any output unless 'list' is positive either
        IF list > 0 & print THEN
        $(  LET bufpos=b.nextmod+1
            // this points into 'binbuf', a vector of hex produced by this
            // assembly line.  It is initialised to point at the begining and
            // is later used to advance in increments correspondin to the
            // maximum number of bytes that can be displayed on one line
            // (successive lots being output on successive lines on their own).
            LET firstflag = TRUE
            // This is true only for the first line written out.  The line read
            // into 'linbuf' is only output on the first line, successive lines
            // are marked by their program counter and the hex only.
            LET control.line = ?
            // This will be true if the only character on the line is '*P' or
            // '*C' or is empty (except of spaces): in this case the line is not
            // output but either the appropriate number of '*N's output or a new
            // page summoned (by seting 'restartpage')
            LET no.output = (rdch=macro.rdch) & \print.expansions
            // i.e. still in text expansion
            // If the end of the line happens to have fallen in the middle of
            // the expansion of a TEXT variable (see later) the generation of
            // output on this line is with  held until the TEXT expansion is
            // complete: care is taken to ensure that a deletion of the line
            // in one of the "text expansion lines" also results in the
            // deletion of the actual line.
            LET i=1
            // work out if this is a 'control.line':
            WHILE i<=linbuf.top & (linbuf%i='*S' | linbuf%i='*N') DO i:=i+1
            control.line:= i>linbuf.top | linbuf.top=0
            $(rpt
                // The code in this repeat loop generates a line of output
                // Lines continue to be printed until 'bufpos' reaches the
                // end of the hex buffer 'binbuf' (or stops if no output is to
                // be generated anyway).
                IF restartpage & (\control.line | \firstflag) THEN wrch('*P')
                // i.e. there is something to output and 'restart page' is
                // pending so do a page throw!
                UNLESS no.output | restartpage & control.line THEN
                UNLESS firstchar='*S' THEN wrch(firstchar)
                UNLESS no.output | binbuf!b.top<=b.nextmod & control.line THEN
                $(  IF firstchar='*S' THEN
                    // if the first character of this line is "free" and we
                    // are in a file (the number of which we are shortly to
                    // print) print a '+' for a warning.
                    wrch(file.id=0 -> '*S', '+')
                    UNLESS firstflag THEN
                    // keep 'startpc' up to date: it is a global which should
                    // always hold the program counter value at the begining
                    // of this line of output.  (i.e. the address of the first
                    // byte to be generated on or after this line).
                    startpc:=pc+(bufpos-(b.nextmod+1))
                    TEST file.id=0
                    THEN writef("  %I5 ",line.of.file) ELSE
                    // in a GET directive: print number of file
                    TEST file.id<10 THEN
                    writef("%I1  %I4 ",file.id,line.of.file)
                    ELSE writef("%I2 %I4 ",file.id,line.of.file)
                    // don't bother to print program counter on comment lines
                    TEST linbuf%1=comntch |
                         pc=startpc+binbuf!b.top-b.nextmod
                    THEN writes("      ")
                    ELSE writef("%X4  ",startpc)
//                  writef("bufpos = %N, binbufwidth= %N*N",bufpos,binbufwidth)
                    // print out hex generated on this line
                    FOR i=bufpos TO bufpos+binbufwidth-1 DO
                       TEST i<=binbuf!b.top
                       THEN $(  outhex(binbuf!i)
                                wrch('*S')
                            $)
                       ELSE FOR j=1 TO bytesperasmword*2+1
                            DO wrch('*S')
                    bufpos:=bufpos+binbufwidth
                $)
                // echo input line if this is the first line:
                TEST firstflag THEN
                $(  IF (control.line->\restartpage,\no.output) THEN
                    writes("  ")
                    FOR i=1 TO linbuf.top-1 DO
                    IF (control.line->\restartpage,\no.output) THEN
                    wrch(linbuf%i)
                    firstflag := FALSE
                    // If 'control.line' is TRUE then there is nothing worth
                    // printing on the line so dont write out the contents of
                    // the line if waiting for output before we start a new
                    // page (i.e. 'restartpage' is TRUE).  Otherwise don't
                    // bother if no output is to be generated anyway
                    // (i.e. 'no.output' is TRUE)
                    IF (control.line->\restartpage,\no.output) THEN wrch('*N')
                $) ELSE wrch('*N')
            $)rpt REPEATUNTIL bufpos>binbuf!b.top | no.output
        $)
        // tell loader format generator what our current program counter is:
        IF binbuf!b.top>b.nextmod THEN code.gen(cd.newpc, pc)
        // output data:
        FOR i=1+b.nextmod TO binbuf!b.top DO code.gen(cd.data, binbuf!i)
        // don't output this line again:
        line.just.outlined := lineno
    $) ELSE
    $(  pass := pass   // have to put some code here - so I put this
/// /*<TIMBIN
        // tell loader format generator what our current program counter is:
        ///IF binbuf!b.top>b.nextmod THEN code.gen(cd.newpc, pc)
        // tell loader how much output data:
        ///FOR i=1+b.nextmod TO binbuf!b.top DO code.gen(cd.data, binbuf!i)
/// /*TIMBIN>*/
    $)
    pc:=pc+binbuf!b.top-b.nextmod
    // set 'binbuf' to empty
    binbuf!b.top := b.nextmod
    binbuf!b.nextmod := binbuf!b.top
    ch:='*N'
$)





AND endline(print) BE outline('*S', print)




AND newlyne() BE
$(  deleteline()
    UNLESS ch=endstreamch THEN
    $(  lineno := lineno + 1
        UNLESS rdch = macro.rdch THEN line.of.file := line.of.file + 1
        rch()
    $)
$)





AND mywrch(c) BE
$(  // This procedure is used to replace the normal BCPL 'wrch': it manages
    // the output listing.  In particular it knows the lenght of a page 'pw'
    // and generates a heading at the top of each page when a '*P' is written.
    // When a '*P' is necessary as the next character (if there is any more
    // output) the flag 'restartpage' is SET.
    // It is up to other parts of the program to test this flag and write
    // '*P' before generating any non-blank output.  This is necessary in
    // order to prevent a final headed blank page.
    LET savewrch=wrch
    wrch:=syswrch
    SWITCHON c INTO
    $(  DEFAULT: wrch (c)
                 ENDCASE
        CASE '*N':
             outpos:=outpos+1
             wrch(c)
             IF (outpos&byte1)>pl THEN restartpage:=throws
             ENDCASE
        CASE '*P':
             restartpage:=FALSE
             UNLESS throws ENDCASE
             $(  LET len=?
                LET dat = VEC 10
                 dat := get.time(dat, 10)
                 wrch('*P')
                 writef("%S assembler %S",name,dat)
                 len:=10+name%0+1+dat%0
                 UNLESS pagetitle=0 DO
                    $(  FOR i=len TO (pw-pagetitle%0)/2
                        DO $(  wrch('*S'); len := len+1
                           $)
                        writes(pagetitle)
                        len:=len+pagetitle%0
                    $)
                 FOR i=len+1 TO pw-8 DO wrch('*S')
                 writef("Page %I3*N*N*N",(outpos&byte2)>>8)
                 outpos:= (outpos&byte2) + (1<<8)
             $)
    $)
    wrch:=savewrch
$)




AND neads(lines) BE
  IF lines+(outpos&byte1)>pl DO  restartpage:=TRUE















///SECTION "asm5"






//
//                      T E X T   M A C R O S
//


STATIC
$(  macro.savrdch= 0
$)





MANIFEST
$(  text.link  = 0      // pointer to next MACRO on the stack
    text.block = 1      // pointer to character buffer
    text.chars = 2      // number of characters in buffer
    text.chptr = 3      // number of character currently in CH
    text.savch = 4      // character extant when macro first called
    text.parms = 5      // pointer to list of formal parameter names
    text.times = 6      // number of times expansion is to be made
    textsize   = 7
$)




LET macro.var(start.pos, expansions) = VALOF
TEST expansions=0 THEN RESULTIS null ELSE
$(  // declares a macro and returns a pointer to its activation
    LET activation = tempgetvec(textsize-1)
    TEST activation=0 THEN
    $(  LET p = macro.stack
        LET n = 0
        WHILE p \= null DO
        $(  p := p!text.link
            n := n+1
        $)
        error(e.macstack, n)
    $) ELSE
    $(  activation!text.link  := null
        activation!text.block := null
        activation!text.chars := 0
        activation!text.chptr := start.pos - 1
        activation!text.parms := null
        activation!text.times := (expansions<0 -> expansions, expansions-1)
    $)
    RESULTIS activation
$)





AND call.macro(activation, text.area, charlen, ch1) BE
// Causes the macro ACTIVATION to be expanded taking characters from the
// text buffer TEXT.AREA (which is CHARLEN characters long in total) starting at
// character number ACTIVATION!TEXT.CHPTR.  The macro is expanded
// ACTIVATION!TEXT.TIMES number of times with the local variables given in
// ACTIVATION!TEXT.PARMS (these are the names of the FORMAL parameters).
// If ACTIVATION!TEXT.TIMES is negative the expansions will repeat indefinitely
// - a call to END.MACRO.VAR being used to eventually end the sequence.
//     05.02.82

// CH1 is the character which is first to be given to CH - if this is null
// (i.e. ENDSTREAMCH) the first character of the buffer is used.

// TEXT.AREA may be null - and in this case should be treated as containing
// no characters.

UNLESS activation=null THEN
$(  IF macro.stack = null THEN
    $(  macro.savrdch := rdch
        rdch := macro.rdch
    $)
    activation!text.link  := macro.stack
    macro.stack := activation
    activation!text.block := text.area
    activation!text.chars := (text.area=null -> 0, charlen)
    activation!text.savch := (print.expansions -> #X8000, #X0000) |
                             (ch1=endstreamch -> ch, 0)
    TEST ch1=endstreamch THEN rch() ELSE ch:=ch1
$)




AND macro.rdch() = VALOF
$(  // This routine either takes the next character in the current macro
    // text buffer or it gives the character that was extant at the time
    // of the expansion.  If that has been also read END.MACRO.VAR is called.
    // Text comes from the current buffer is repeated if the TEXT.TIMES field
    // has not yet reached zero.
    LET ch = ?
    LET character.found = ?
    $(rpt
        LET this.macro = macro.stack
        character.found := TRUE
        this.macro!text.chptr := this.macro!text.chptr + 1
        TEST this.macro!text.chptr > this.macro!text.chars THEN
        $(  // we know that all MACRO frames are allocated using TEMPGETVEC:
            // => we can deallocate the memory:
            IF macro.stack < memory!0+memory THEN
            memory!0 := macro.stack - memory
            macro.stack := macro.stack!text.link
            IF macro.stack = null THEN rdch := macro.savrdch
            character.found := FALSE
            // will try again with new macro
        $) ELSE
        TEST this.macro!text.chptr = this.macro!text.chars THEN
        $(  LET parms = this.macro!text.parms
            WHILE parms\=null DO
            $(  unstacktext(parms+1)
                parms := parms!0
            $)
            TEST this.macro!text.times\=0 THEN
            $(  UNLESS this.macro!text.times<0 THEN
                this.macro!text.times := this.macro!text.times-1
                this.macro!text.chptr := 0
                // it is assumed that TEXT variables are NEVER repeated!
                // (otherwise the above character pointer will be wrong)
                ch := (this.macro!text.block) % 0
            $) ELSE
            $(  ch := this.macro!text.savch
                print.expansions := (0 \= (#X8000 & ch))
                ch := ch & #X00FF
                IF ch=0 THEN character.found := FALSE
            $)
        $) ELSE ch := (this.macro!text.block) % (this.macro!text.chptr)
    $)rpt REPEATUNTIL character.found | macro.stack = null
    UNLESS character.found THEN ch := rdch()
    RESULTIS ch
$)




AND end.macro.var() = (macro.stack = null -> FALSE, VALOF
$(  LET this.macro = macro.stack
    this.macro!text.times := 0
    this.macro!text.chptr := this.macro!text.chars-1
    RESULTIS TRUE
$) )





AND unstacktext(text.symbol) BE deletelab(text.symbol, type.text)



AND stacktext(activation, parm.symbol, string) = VALOF
TEST activation=null THEN RESULTIS FALSE ELSE
$(  // make a local copy of the PARM.SYMBOL
    LET parm.value = getstr(string, tempgetvec)
    // must do this first since "string" might still be on TEMPGETVEC's stack
    LET len = 1 + parm.symbol%0/bytesperword
    LET local = tempgetvec(len)
    LET forward = TRUE
    TEST local=null THEN error(e.memfull) ELSE
    $(  // this store will be freed when the MACRO stack frame for
        // this ACTIVATION is poped
        FOR i=0 TO len-1 DO (local+1)!i := parm.symbol!i
        // chain text name into FORMAL parameter list
        local!0 := activation!text.parms
        activation!text.parms := local
        forward := putlab(local+1, parm.value, type.text | flag.temp)
    $)
    RESULTIS forward
$)














//
//                      F I L E    T A B L E
//



/*
GLOBAL $(
//  files: fileman
//  line.of.file
//  fileno
//  max.files
$)
*/


MANIFEST
$(  no.of.files = 0        // offset in 'files' of number of files GOT
    first.line  = 1        // offset in 'files' of number of first line
    first.file  = 2        // offset of first file entry in 'files'
$)




// Line number information has to be kept about each of the files opened
// using the GET directive.  A vector of pointers to information blocks is
// kept.  The information contained in them is the number of the line counter
// 'lineno' at the begining of the file, and when the file has been read, and
// the name of the file.
// GET directives can be nested so calculation of the actual number of the line
// within the current file is a little complex ('file.number')!




LET newfile(s) = VALOF
$(  TEST 0<=files!no.of.files<=max.files THEN
    IF pass=second THEN
    $(  LET newfile=simplegetvec(2+s%0/2)
        files!(files!no.of.files+1) := newfile
        TEST newfile=null THEN error(e.nospace) ELSE
        $(  FOR i=0 TO s%0/2 DO newfile!(2+i):=s!i
            newfile!0 := lineno     // first line
            newfile!1 := lineno     // last line
        $)
    $) ELSE error(e.filexs)
    files!no.of.files := files!no.of.files + 1
    RESULTIS files!no.of.files - first.file + 1
$)


AND endfile(fno) BE
    IF pass=second THEN files!(fno+first.file-1)!1 := lineno    // last line


AND printfiles() BE
IF allsyms DO
   FOR i=first.file TO files!no.of.files DO
   IF files!i \= 0 THEN
   $(  IF restartpage THEN wrch('*P')
       writef("file +%N is %S (%N lines)*N",
              i-first.file+1, files!i+2, files!i!1-files!i!0)
   $)


AND resetfiles(to.line) BE
$(  files!no.of.files:=first.file-1
    files!first.line:=to.line
    file.id:=0
$)



AND file.number(lv.line) = VALOF
$(  LET i=first.file
    LET j=?             // maximum file number included in found file
    LET n=?             // iterates through immediately included files
    LET fmax=files!0
    WHILE i<=fmax & !lv.line>files!i!0 DO i:=i+1
    i:=i-1
    j:=i                // highest number for an included file
    WHILE i>=first.file & !lv.line>files!i!1 DO i:=i-1
    n:=i+1              // first included file
    WHILE n<=j DO
    $(  LET top=files!n!1
        !lv.line := !lv.line - (top-files!n!0)  // subtract size of files
        n:=n+1
        WHILE n<=j & files!n!0<=top DO n:=n+1   // skip files within this one
    $)
    TEST i=first.file-1 THEN !lv.line := !lv.line + files!first.line ELSE
    !lv.line := !lv.line - files!i!0
    RESULTIS i-first.file+1
$)






//
//                         F I E L D S
//



MANIFEST
$(  f.shift = byte1
    f.len   = byte2
    partsize= 8
$)



LET getf(no, field) = VALOF
$(  LET s=(field & f.shift)
    LET l=(field & f.len)>>partsize
    RESULTIS (no>>s) & ((1<<l)-1)
$)


LET putf(no, field, too) = VALOF
$(  LET s=(field & f.shift)
    LET l=(field & f.len) >> partsize
    LET m=(1<<l)-1
    LET ans = (too & \(m<<s)) | ((no & m)<<s)
    UNLESS fitsmask(no, m) DO warn(e.ftoosmall,no)
    RESULTIS ans
$)


AND trim(no, field) = VALOF
$(  LET l=(field & f.len)>>partsize
    LET m=(1<<l)-1
    RESULTIS no&m
$)


AND newf(pos, len) = (pos&byte1) | ((len&byte1)<<partsize)


AND putwordf(no,field) BE
IF pass=second DO
   TEST binbuf!b.top<1
   THEN error(e.mtbuf)
   ELSE binbuf!(binbuf!b.top):=putf(no, field, binbuf!(binbuf!b.top))












//
//                          T R E E S
//



MANIFEST
$(  t.left  = 0
    t.right = 1
    t.str   = 2
    t.val   = 3
    t.size  = 4
$)



LET tree.put(lvtree, name, item) BE
$(rpt
    IF !lvtree=null
    DO $(  LET newitem=simplegetvec(t.size-1)
           newitem!t.left, newitem!t.right:= null, null
           newitem!t.str:=name
           newitem!t.val:=item
           !lvtree:=newitem
           RETURN
       $)
    $(  LET c=compstring((!lvtree)!t.str, name)
        IF c=0 error (e.interror, 7)
        lvtree := !lvtree+(c<0 -> t.left, t.right)
    $)
$)rpt REPEAT



AND tree.get(tree, name) = VALOF
$(rpt
    IF tree=null RESULTIS tree
    $(  LET c=compstring(tree!t.str, name)
        IF c=0 RESULTIS tree
        tree := tree!(c<0 -> t.left, t.right)
    $)
$)rpt REPEAT






//
//                         C O D E    T A B L E
//





MANIFEST
$(  c.size = 0
    c.str  = 0
    c.fn   = 1
    c.opstr= 2
$)


LET code.put(name, proc, len, wd1, wd2, wd3, wd4, wd5) BE
$(  LET l=3+(len>0->len,-len)
    LET c=simplegetvec(l-1)
    c!c.str:=name
    c!c.fn :=proc
    c!(c.opstr+0) := len
    FOR i=c.opstr+1 TO l-1 DO c!i:=(@wd1)!(i-(c.opstr+1))
    tree.put(@codes, name, c)
$)


AND getcode(s) = VALOF
$(  LET t=tree.get(codes, s)
    RESULTIS (t=null->null, t!t.val)
$)


AND compcode(s, c) = (0=compstring(s, c!c.str))


AND initcodes() BE error(e.nocodes)       // for redefinition







//
//                 R E G I S T E R    S Y M B O L S
//



STATIC  $(  syms=null  $)


MANIFEST
$(  s.size  = 0
    s.str   = 0
    s.info  = 1
    r.bad   = 0
$)


LET reg.put(reg, val) BE tree.put(@syms, reg, val)


AND getreg(s) = VALOF
$(  LET t=tree.get(syms,s)
    RESULTIS (t=null->r.bad,t!t.val)
$)

AND initsyms() BE RETURN          //  for redefinition








///SECTION "Asm-mac"







//
//                        M A C R O S
//





MANIFEST
$(  macro.next  = 0       // pointer to next macro on definitions stack
    macro.text  = 1       // address of text buffer
    macro.chars = 2       // number of characters in text area
    macro.parms = 3       // list of formal parameters
    macrosize   = 4
$)





LET callmacroproc(lab, macro) BE
$(  LET parm = macro!macro.parms
    LET macro.activation = macro.var(0, 1)
//  writef("Body of %S is:*N<", item.info)
//  FOR i=0 TO macro!macro.chars-1 DO wrch((macro!macro.text)%i)
//  writef(">*N(%N characters)*N", macro!macro.chars)
    get.and.declare(lab)
    WHILE is.strexp() DO
    $(  LET str = ?
        str := strexp(null, 0, FALSE)
//      writef("CALLMACRO: str = *"%S*"*N", str)
        UNLESS parm=null THEN
        $(  // declare local value of symbol <parm+1> as STR
//          writef("CALLMACRO: about to declare %S as *"%S*"*N", parm+1, s)
            stacktext(macro.activation, parm+1, str)
            parm := parm!0
//          writef("CALLMACRO: next PARM is #X%X8*N", parm)
        $)
        scan(i.comma)
    $)
    WHILE parm \= null DO
    $(  // for each formal paramter still in the list give it the value ""
//      writef("CALLMACRO: about to declare %S as null*N", parm+1)
        stacktext(macro.activation, parm+1, "")
        parm := parm!0
//      writef("CALLMACRO: next PARM is #X%X8*N", parm)
    $)
    endline(\print.expansions)
//  writef("Macro being called with text @%X8 %N chars, parmlink=#X%X8*N",
//         macro!macro.text, macro!macro.chars, macro!macro.parms)
    call.macro(macro.activation, macro!macro.text, macro!macro.chars, '*N')
$)





AND macroproc(lab) BE
$(  LET savelist = list     // assembler LIST variable - controlling listing
    LET text.size = ?
    getitem()
    TEST lab=null THEN error(e.nolab) ELSE
    $(  LET macro = ?
        macro := simplegetvec(macrosize-1)
        // declare the MACRO straight away!
        IF pass=first THEN putlab(lab, macro, type.macro)
        macro!macro.next  := null
        macro!macro.parms := null
        IF item.type = i.iden THEN
        $(  IF pass=first THEN
            add.macro.parm(macro, item.info)
            getitem()
            WHILE scan(i.comma) DO
            $(  TEST item.type = i.iden THEN
                    IF pass=first THEN add.macro.parm(macro, item.info)
                ELSE warn(e.nolab)
                getitem()
            $)
        $)
        IF comntcheck THEN
        UNLESS item.type=i.comnt | item.type=i.stop | item.type=i.end THEN
        warn(e.nocomnt, comntch)
        IF print.expansions THEN list:=0   // turn listing off
        TEST warning THEN
        $(  LET savelev = errorlevel
            LET savelab = errorlabel
            errorlevel := level()
            errorlabel := stoplab
            error(0)
stoplab:    errorlabel := savelab
            errorlevel := savelev
        $) ELSE endline(TRUE)
        text.size := read.macro.body(macro, pass=first)
        TEST text.size \= -1 THEN
        IF pass=first THEN
        $(  LET text.area = simplegetvec(text.size-1)
            LET text = macro!macro.text
            // copy text to temporary area:
            FOR i=0 TO text.size-1 DO text.area!i := text!i
            macro!macro.text := text.area
//          writef("Macro %S: text area @%X8 with %N chars, parms at #X%X8*N",
//                  lab, macro!macro.text, macro!macro.chars, macro!macro.parms)
        $)
        ELSE
        $(  list := savelist
            error(e.nomacterm)
        $)
        IF print.expansions THEN
        $(  list := savelist          // turn listing on again
            endline(FALSE)            // remove this line from listing too
        $)
    $)
$)




AND endmacroproc(lab) BE get.and.declare(lab)



AND rptmacroproc(lab) BE
$(  LET rpt.factor = -1        // causing indefinite expansion
    LET an.error = 0
    LET mark.on.temp = memory!0
    get.and.declare(lab)
    IF is.expression() THEN
    $(  rpt.factor := expression()
        TEST dontknow THEN an.error := e.forward ELSE
        IF rpt.factor < 0 THEN an.error := e.posnum
    $)
    TEST an.error \= 0 THEN error(an.error) ELSE
    $(  LET macro = tempgetvec(macrosize-1)
        LET savelist = list
        LET text.size = ?
        IF macro=null THEN error(e.memfull)
        IF print.expansions THEN list:=0  // turn listing off
        macro!macro.next  := null
        macro!macro.parms := null
        endline(TRUE)
        text.size := read.macro.body(macro, TRUE)
        TEST text.size \= -1 THEN
        $(  tempgetvec(text.size-1)
            // this should allocate the space waiting on top of the
            // TEMPGETVEC stack
            endline(TRUE)
            list := savelist              // restore listing
            call.macro(macro.var(0, rpt.factor),
                       macro!macro.text, macro!macro.chars, '*N')
        $) ELSE
        $(  list := savelist
            memory!0 := mark.on.temp
            error(e.nomacterm)
        $)
    $)
$)




AND breakmacroproc(label) = VALOF
$(  get.and.declare(label)
    UNLESS end.macro.var() THEN error(e.nomacro)
$)





AND localproc(lab) BE
$(  LET gen.label() = VALOF
    $(  LET new.label = TABLE 0,0,0,0
        LET unique.no = ?
        new.label%0 := 7
        new.label%1 := 'X'
        new.label%2 := sepch
        new.label%3 := sepch
        TEST pass=first THEN
        $(  STATIC $( passone.var = 0 $)
            unique.no := passone.var
            passone.var := passone.var + 1
        $) ELSE
        $(  STATIC $( passtwo.var = 0 $)
            unique.no := passtwo.var
            passtwo.var := passtwo.var + 1
        $)
        FOR i=0 TO 3 DO
        $(  LET digit = (unique.no >> 4*i) & #XF
            new.label%(4+i) := (digit > 9 -> 'A'+digit-10, '0'+digit)
        $)
        RESULTIS new.label
    $)
    get.and.declare(lab)
    TEST macro.stack=null THEN error(e.nomacro) ELSE
    $(  $(rpt
            TEST item.type=i.iden THEN
            stacktext(macro.stack, item.info, gen.label()) ELSE
            error(e.nolab)
            getitem()
        $)rpt REPEATUNTIL \scan(i.comma)
    $)
$)





AND printproc(lab) BE
$(  LET str = ?
    LET saveout = output()
    get.and.declare(lab)
    str := strexp(null, 0, TRUE)
    IF pass=second THEN
    $(  selectoutput(out)
        writef("%S: %S*N", name, str)
        selectoutput(saveout)
    $)
$)





AND read.macro.body(macro, write.macro) = VALOF
$(  // now in a position to parse the body of the macro:
    // This procedure returns the size, in words, of the block of
    // text that it reads into MACRO!MACRO.TEXT
    // It leaves the text on an unclaimed part of the TEMPVEC stack -
    // hence calls to either TEMPGETVEC or SIMPLEGETVEC should be
    // avoided until the information is either copied, claimed or
    // deliberately ignored.
    // If no ending mnemonic is found the size returned will be -1
    // It only changes the macro if WRITE.MACRO is TRUE
    LET mnemproc = ?
    LET lab = lab.space    // will be label for terminating mnemonic
    LET depth = 1
    LET size = -1
    LET notfound = FALSE
    LET text = (write.macro -> tempgetvec(-1), macro!macro.text)
    // TEXT holds the value to be insterted into MACRO!MACRO.TEXT when
    // the read is completed
    macro!macro.text := (write.macro -> text, null)
    // assigned NULL when WRITE.MACRO is FALSE so that no characters will
    // overwrite the buffer
    macro!macro.chars := 0 // initially
    filling.macro := macro // turns listing to text area on
    $(rpt
        mnemproc := find.mnem(@lab, 4,
                    macroproc, rptmacroproc, endmacroproc, endproc)
        TEST mnemproc=null | mnemproc=endproc THEN notfound := TRUE ELSE
        depth := depth + (mnemproc=endmacroproc -> -1, 1)
        UNLESS notfound | depth=0 THEN endline(TRUE)
    $)rpt REPEATUNTIL notfound | depth=0
    filling.macro := 0     // disengages MACRO.WRCH
    // if macro was too big MACRO.TEXT will be NULL (see MACRO.WRCH)
    IF macro!macro.text = null & write.macro THEN notfound := TRUE
    UNLESS macro!macro.text = null THEN
    $(  // must delete this line from text area:
        LET chptr = macro!macro.chars - 1
        // MACRO!MACRO.CHARS points to the character position just after
        // the one last read.  The character last read is the first one
        // after the last character in the current symbol.
        UNLESS chptr<=0 THEN
        chptr:=chptr-1 REPEATUNTIL chptr<=0 | text%chptr='*N'
        macro!macro.chars :=  (chptr<=0 -> 0, chptr+1)
        // the "1" above accounts for the '*N' that we are to add to the
        // end of the buffer (which we have just ensured does not end with
        // '*N').
    $)
    IF macro!macro.text\=null | \write.macro THEN
    size := (macro!macro.chars+bytesperword-1)/bytesperword
    macro!macro.text := text    // ensure MACRO.TEXT field is as on entry
    // call procedure which ended macro expansion:
    UNLESS mnemproc=null THEN mnemproc(lab)
    RESULTIS size
$)



AND add.macro.parm(macro, parm) BE
$(  LET parm.copy = simplegetvec(1 + parm%0/bytesperword)
    LET p = macro+macro.parms
    FOR i=0 TO parm%0/bytesperword DO
        (parm.copy+1)!i := parm!i
    parm.copy!0 := null
    WHILE !p \= null DO p := !p
    !p := parm.copy
$)




AND find.mnem(lv.lab, n, proc1, proc2, proc3, proc4, proc5, proc6) = VALOF
$(  // finds the first mnemonic the procedure for which is
    // one of the N procedures PROC1 .. PROC4 or returns NULL
    // if none is found.
    LET savelev = errorlevel
    LET savelab = errorlabel
    LET save.expand = expand.text
    LET mnem = ?
    LET mt.line = ?
    LET mnem.found = ?
    LET i = ?
    errorlevel := level()
    errorlabel := exitlabel
    expand.text := FALSE      // stop TEXT expansions during read
    $(rpt
        i := 0
        mnem := read.to.mnem(@mt.line, @mnem.found, lv.lab, FALSE)
        // do not allow declarations in parsing lines either
        UNLESS mnem=null THEN
            WHILE i<n & (@proc1)!i \= mnem!c.fn DO i:=i+1
        IF i=n | mnem=null THEN endline(TRUE) // none found on this line
exitlabel:
    $)rpt REPEATUNTIL item.type=i.end | fatal | i\=n & mnem\=null | finishpass
    expand.text := save.expand  // restore former text expansion mode
    // restore error jump label:
    errorlevel := savelev
    errorlabel := savelab
    RESULTIS (i=n | mnem=null -> null, mnem!c.fn)
$)





AND macro.wrch(macro, ch) BE
// This routine is called for every character read so long as
// FILLING.MACRO is not zero.  (see RCH)
$(  LET text = macro!macro.text
    UNLESS text=null THEN
    $(  LET chno = macro!macro.chars
        LET next.word = chno/bytesperword
        LET next.byte = chno REM bytesperword
        LET max.buf.size = memsize - (text-memory)
        TEST next.word > max.buf.size THEN
        $(  // disable further expansions
            macro!macro.text := null
            error(e.bigmacro)
        $) ELSE
        $(  text%chno := ch
            macro!macro.chars := chno+1
        $)
    $)
$)










///SECTION "asm-cond"







//
//                  Conditional    Assembly
//





LET ifproc(lab, test.spec) BE
$(  // TEST.SPEC has fields layed out as follows:
    //              bit 0   -  not (the following condition)
    //              bit 1   -  2 args if set, otherwise one
    //              bit 2   -  Less than if set. Otherwise equal.
    //              bit 3   -  Type is STRING if set, Number otherwise
    //              bit 7   -  Test is for defined symbol if set
    LET reverse   = (test.spec & bit0)\=0
    LET two.args  = (test.spec & bit1)\=0
    LET less.than = (test.spec & bit2)\=0
    LET is.string = (test.spec & bit3)\=0
    LET symbolset = (test.spec & bit7)\=0
    LET arg2 = (is.string -> "", 0)
    LET arg1 = ?
    LET ans = TRUE
    get.and.declare(lab)
    TEST symbolset THEN
    $(  LET type = type.lab // default type to check
        IF item.type=i.number THEN
        $(  type := item.info
            getitem()
            scan(i.comma)
        $)
        TEST item.type = i.iden THEN
        $(  looktype(type, item.info)
            ans := iden.valid
        $) ELSE error(e.nolab)
        getitem()
    $) ELSE
    $(  LET comparison = ?
        arg1 := (is.string -> strexp(null, 0, \two.args), expression())
        TEST dontknow THEN error(e.forward) ELSE
        $(  LET mark.mem = memory!0
            IF two.args THEN
            $(  IF is.string THEN arg1 := getstr(arg1, tempgetvec)
//              IF is.string THEN writef("first arg is *"%S*"*N", arg1)
                // make sure string is not overwritten
                UNLESS scan(i.comma) THEN error(e.expected, ',')
                arg2 := (is.string -> strexp(null, 0, FALSE), expression())
            $)
            comparison := (is.string -> compstring(arg2, arg1), arg2-arg1)
//          IF is.string THEN
//          writef("Comparing *"%S*" and *"%S*" dif=%N*N",arg1,arg2,comparison)
            ans := (less.than -> comparison<0, comparison=0)
            // reclaim TEMPGETVEC memory
            memory!0 := mark.mem
        $)
    $)
    IF reverse THEN ans := \ans
    TEST ans THEN
        IF print.expansions THEN endline(FALSE)
    ELSE TEST print.expansions THEN
    $(  LET savelist = list
        list := 0
        endline(FALSE)
        IF read.if.body(TRUE) THEN endline(FALSE)
        list := savelist
    $) ELSE
    $(  endline(TRUE)
        read.if.body(TRUE)
    $)
$)




AND elseproc(lab) BE
$(  get.and.declare(lab)
    // read through rest of IF body - ELSE not terminator
    TEST print.expansions THEN
    $(  LET savelist = list
        list := 0
        endline(TRUE)
        IF read.if.body(TRUE) THEN endline(FALSE)
        list := savelist
    $) ELSE
    $(  endline(TRUE)
        read.if.body(TRUE)
    $)
$)



AND fiproc(lab) BE
$(  get.and.declare(lab)
    IF print.expansions THEN endline(FALSE)
$)




AND read.if.body(else.is.a.terminator) = VALOF
$(  LET mnemproc = ?
    LET found = TRUE
    LET lab = lab.space
    LET depth = 1
    $(rpt
        mnemproc := find.mnem(@lab, 4, endproc, ifproc, fiproc, elseproc)
        TEST mnemproc=null | mnemproc=endproc THEN found := FALSE ELSE
        TEST mnemproc=elseproc THEN
            IF depth=1 THEN
            $(  depth := 0
                UNLESS else.is.a.terminator THEN error(e.noif)
            $)
        ELSE depth := depth + (mnemproc=fiproc -> -1, 1)
        UNLESS depth=0 | \found THEN endline(TRUE)
    $)rpt REPEATUNTIL depth=0 | \found
    UNLESS mnemproc=null THEN getitem()
    RESULTIS found
$)






//
//                   String   Expressions
//





LET strexp(buffer, startpos, allow.concatination) = VALOF
$(  // This procedure uses TEMPGETVEC space and returns a pointer to
    // a freed area at the top - consequently the next call to TEMPGETVEC
    // will destroy the answer returned.  The result must be copied
    // immediately if it is to be used.
    LET new = (buffer=0)
    LET ans = ?
    IF new THEN
    $(  buffer := tempgetvec(256/bytesperword)
        TEST buffer=0 THEN error(e.memfull) ELSE
        $(  buffer%0 := 0
            startpos := 1
        $)
    $)
    $(rpt
        ans := strterm(buffer, startpos)
        IF ans THEN startpos := buffer%0 + 1
    $)rpt REPEATUNTIL \allow.concatination | \ans | \scan(i.comma)
    IF new THEN memory!0 := buffer - memory   // return TEMPGETVECed space
    UNLESS ans THEN warn(e.nostr)
    RESULTIS buffer
$)





AND strterm(buffer, startpos) = VALOF
$(  LET ans = (item.type=i.comma | item.type=i.strrbkt)
    UNLESS ans THEN
    $(  ans := strpart(buffer, startpos)
        IF ans & scan(i.lbkt) THEN
        $(  LET chno = expression()
            LET len = 1               // default
            LET partlen = buffer%0 - startpos + 1
            TEST dontknow THEN warn(e.forward) ELSE
            TEST chno <= 0 THEN warn(e.posnum) ELSE
            $(  IF scan(i.comma) THEN
                $(  LET templen = expression()
                    TEST dontknow THEN warn(e.forward) ELSE
                    TEST templen < 0 THEN warn(e.posnum) ELSE
                    len := templen
                $)
                FOR i=startpos TO startpos+chno-2 DO
                    buffer%i := buffer%(i+chno-1)
                IF len > partlen - (chno-1) THEN
                FOR i=1 TO len-partlen+(chno-1) DO
                    buffer%(startpos+partlen-chno-1+i) := '*S'
                buffer%0 := startpos + len - 1
                UNLESS scan(i.rbkt) THEN error(e.expected, ')')
            $)
        $)
    $)
    RESULTIS ans
$)




AND strpart(buffer, startpos) = VALOF
$(  LET ans = TRUE
    IF item.type = i.pling THEN
    $(  LET save.expand = expand.text
        expand.text := FALSE
        ans := FALSE
        getitem()
        TEST item.type = i.iden THEN
        $(  LET macro = looktype(type.text, item.info)
            TEST iden.valid THEN
            $(  ans := TRUE
                item.info := macro
            $) ELSE warn(e.badtype)
        $) ELSE warn(e.badlab)
        expand.text := save.expand
    $)
    IF ans THEN
    TEST item.type = i.string | item.type = i.sstring | item.type = i.iden THEN
    $(  LET newlen = buffer%0 + item.info%0
        TEST newlen > 255 THEN
        $(  warn(e.fullstring)
            ans := FALSE
        $) ELSE
        $(  buffer%0 := newlen
            FOR i=1 TO item.info%0 DO buffer%(startpos+i-1) := item.info%i
        $)
        getitem()
    $) ELSE
    TEST scan(i.strlbkt) THEN
    $(  strexp(buffer, startpos)
        UNLESS scan(i.strrbkt) THEN warn(e.expected, ',')
    $) ELSE
    TEST scan(i.percent) THEN
    $(  LET format = 'N'
        LET width = 0
        LET i = expression()
        ans := FALSE
        TEST dontknow THEN warn(e.forward) ELSE
        $(  LET base.power = 0
            LET base.proc = null
            IF scan(i.colon) THEN
            TEST item.type \= i.iden THEN warn(e.badbase, ':') ELSE
            TEST item.info%0>2 THEN
               warn(e.badbase, item.info%3) ELSE
            $(  format := capitalch(item.info%1)
                IF item.info%0>1 THEN
                $(  LET widthch = item.info%2
                    TEST '0' <= widthch <= '9' THEN width := widthch-'0' ELSE
                    warn(e.badbase, widthch)
                $)
                getitem()
            $)
            base.proc := VALOF SWITCHON format INTO
            $(  CASE 'N':
                CASE 'D':
                CASE 'I': RESULTIS wrint
                CASE 'B': base.power := 1
                          RESULTIS wrbin
                CASE 'O': base.power := 3
                          RESULTIS wrbin
                CASE 'X': base.power := 4
                          RESULTIS wrbin
                CASE 'C': RESULTIS wrchr
                DEFAULT : RESULTIS null
            $)
            TEST base.proc=null THEN error(e.badbase, format) ELSE
            TEST base.proc(i, width, buffer, 255, base.power) THEN
            ans := TRUE ELSE warn(e.fullstring)
        $)
    $) ELSE ans := FALSE
    RESULTIS ans
$)





AND wrint(n, width, buffer, maxpos, dummy) = VALOF
$(  LET digs = VEC 10
    LET i = 0
    LET k = n
    LET error = FALSE
    IF n<0 THEN
    $(  width := width-1
        k := -n
    $)
    $(rpt
        digs!i := -(-k REM 10)      // to get MAXINT &c right
        k := k/10
        i := i+1
    $)rpt REPEATUNTIL k=0
    TEST buffer%0+(width-i) > maxpos THEN error := TRUE ELSE
    $(  FOR j=i+1 TO width DO
        $(  buffer%0 := buffer%0 + 1
            buffer%(buffer%0) := '*S'
        $)
        IF n<0 THEN
        TEST buffer%0+1 > maxpos THEN error:= TRUE ELSE
        $(  buffer%0 := buffer%0 + 1
            buffer%(buffer%0) := '-'
        $)
        TEST buffer%0+i > maxpos THEN error := TRUE ELSE
        FOR j = i-1 TO 0 BY -1 DO
        $(  buffer%0 := buffer%0 + 1
            buffer%(buffer%0) := digs!j+'0'
        $)
    $)
    RESULTIS \error
$)



AND wrbin(n, width, buffer, maxpos, power) = VALOF
$(  LET ok = (buffer%0+width <= maxpos)
    IF ok THEN
    $(  IF width>1 THEN wrbin(n>>power, width-1, buffer, maxpos, power)
        buffer%0 := buffer%0 + 1
        buffer%(buffer%0) := (TABLE '0','1','2','3',
                                    '4','5','6','7',
                                    '8','9','A','B',
                                    'C','D','D','F') ! (n&((1<<power)-1))
    $)
    RESULTIS ok
$)





AND wrchr(n, width, buffer, maxpos, dummy) = VALOF
$(  LET ok = (buffer%0+width <= maxpos)
    IF ok THEN
    $(  LET ch = n&#XFF
        IF width>1 THEN wrchr(n>>8, width-1, buffer, maxpos)
        buffer%0 := buffer%0 + 1
        buffer%(buffer%0) := (ch=0 -> '*S', ch)
    $)
    RESULTIS ok
$)






AND is.strexp() = VALOF
SWITCHON item.type INTO
$(  CASE i.iden:
    CASE i.string:
    CASE i.sstring:
    CASE i.pling:
    CASE i.percent:
        RESULTIS TRUE
    DEFAULT:
        RESULTIS item.type = i.strlbkt
$)












///SECTION "asm6"






//
//                    L E X I C A L    A N A L Y S E R
//






/*     These Globals kept for documentation purposes - copy in ASMHDR
GLOBAL $(
// item.type : lexanal+0        //  contains an 'i' number (see below)
// item.info : lexanal+1        //  additional information
// item.start: lexanal+2        //  character position of lexical symb
// getitem   :       ?+3        //  getitem() sets up item.info & item.type
// scan      :       ?+4        //  scan(item) scans item if item=item.type
// expression:       ?+5        //  no:=expression() sets dontknow parses exprn
// bracketed : lexanal+3        //  TRUE if last expression parsed was bracketed
// lex.space : lexanal+4        //  memory for lexical analyser's strings
$)
*/







// The following procedure is used whenever the value of a character is placed
// in the output hex.  It can be redefined by the user of the general assembler
// to generate something other than ASCII which is the default translation.



//*<ASCII// if on ascii machine no need to translate characters:
LET cvtchar (ch) = ch
//      // now code for translation if not on ascii machine:
/*
LET cvtchar(ch) = VALOF
SWITCHON ch INTO
$(  CASE 0   : RESULTIS 0          // '*0'
    CASE '*T': RESULTIS #X09
    CASE '*N':
    CASE 10  : RESULTIS #X0A       // '*L'
    CASE 12  : RESULTIS #X0C       // '*P'
    CASE 13  : RESULTIS #X0D       // '*C'
    CASE 27  : RESULTIS #X1B       // '*E'
    CASE '*S': RESULTIS #X20
    CASE '!': RESULTIS #X21
    CASE '"': RESULTIS #X22
    CASE '#': RESULTIS #X23
    CASE '$': RESULTIS #X24
    CASE '%': RESULTIS #X25
    CASE '&': RESULTIS #X26
    CASE '*'': RESULTIS #X27
    CASE '(': RESULTIS #X28
    CASE ')': RESULTIS #X29
    CASE '**': RESULTIS #X2A
    CASE '+': RESULTIS #X2B
    CASE ',': RESULTIS #X2C
    CASE '-': RESULTIS #X2D
    CASE '.': RESULTIS #X2E
    CASE '/': RESULTIS #X2F
    CASE '0': RESULTIS #X30
    CASE '1': RESULTIS #X31
    CASE '2': RESULTIS #X32
    CASE '3': RESULTIS #X33
    CASE '4': RESULTIS #X34
    CASE '5': RESULTIS #X35
    CASE '6': RESULTIS #X36
    CASE '7': RESULTIS #X37
    CASE '8': RESULTIS #X38
    CASE '9': RESULTIS #X39
    CASE ':': RESULTIS #X3A
    CASE ';': RESULTIS #X3B
    CASE '<': RESULTIS #X3C
    CASE '=': RESULTIS #X3D
    CASE '>': RESULTIS #X3E
    CASE '?': RESULTIS #X3F
    CASE '@': RESULTIS #X40
    CASE 'A': RESULTIS #X41
    CASE 'B': RESULTIS #X42
    CASE 'C': RESULTIS #X43
    CASE 'D': RESULTIS #X44
    CASE 'E': RESULTIS #X45
    CASE 'F': RESULTIS #X46
    CASE 'G': RESULTIS #X47
    CASE 'H': RESULTIS #X48
    CASE 'I': RESULTIS #X49
    CASE 'J': RESULTIS #X4A
    CASE 'K': RESULTIS #X4B
    CASE 'L': RESULTIS #X4C
    CASE 'M': RESULTIS #X4D
    CASE 'N': RESULTIS #X4E
    CASE 'O': RESULTIS #X4F
    CASE 'P': RESULTIS #X50
    CASE 'Q': RESULTIS #X51
    CASE 'R': RESULTIS #X52
    CASE 'S': RESULTIS #X53
    CASE 'T': RESULTIS #X54
    CASE 'U': RESULTIS #X55
    CASE 'V': RESULTIS #X56
    CASE 'W': RESULTIS #X57
    CASE 'X': RESULTIS #X58
    CASE 'Y': RESULTIS #X59
    CASE 'Z': RESULTIS #X5A
    CASE '[': RESULTIS #X5B
    CASE '\': RESULTIS #X5C
    CASE ']': RESULTIS #X5D
    CASE '^': RESULTIS #X5E
    CASE '_': RESULTIS #X5F
    CASE '`': RESULTIS #X60
    CASE 'a': RESULTIS #X61
    CASE 'b': RESULTIS #X62
    CASE 'c': RESULTIS #X63
    CASE 'd': RESULTIS #X64
    CASE 'e': RESULTIS #X65
    CASE 'f': RESULTIS #X66
    CASE 'g': RESULTIS #X67
    CASE 'h': RESULTIS #X68
    CASE 'i': RESULTIS #X69
    CASE 'j': RESULTIS #X6A
    CASE 'k': RESULTIS #X6B
    CASE 'l': RESULTIS #X6C
    CASE 'm': RESULTIS #X6D
    CASE 'n': RESULTIS #X6E
    CASE 'o': RESULTIS #X6F
    CASE 'p': RESULTIS #X70
    CASE 'q': RESULTIS #X71
    CASE 'r': RESULTIS #X72
    CASE 's': RESULTIS #X73
    CASE 't': RESULTIS #X74
    CASE 'u': RESULTIS #X75
    CASE 'v': RESULTIS #X76
    CASE 'w': RESULTIS #X77
    CASE 'x': RESULTIS #X78
    CASE 'y': RESULTIS #X79
    CASE 'z': RESULTIS #X7A
    CASE '{': RESULTIS #X7B
    CASE '|': RESULTIS #X7C
    CASE '}': RESULTIS #X7D
    CASE '~': RESULTIS #X7E
    DEFAULT: RESULTIS ch
$)
*/


MANIFEST
$(  uletter = 10-'A'
    lletter = 10-'a'
$)



LET getitem() = VALOF
$(  LET item.found = ?
    LET first.item.pos = -1

    // This procedure is the lexical analyser through which all input is taken.
    // Information about the lexical item just read is kept in:
    //      item.type         -  lexical item identifcation ('i.' constants)
    //      item.start        -  column at which item started
    //      item.info         -  extra information about item (e.g. value)
    // A work area called 'lex.space' is used to hold identifiers and strings
    // and a pointer to it is returned in 'item.info' when the item is either
    // of these types.   20.01.81

    // If an identifier is returned, the string is padded with zeroes up to
    // a whole word boundary.  This is required by some symbol table
    // manipulation routines.  16.07.81

    // The lexical item I.DUFFSYMBOL is now explicitly ignored and never
    // returned from this procedure - this enables it to be used to terminate
    // symbols with a non significant symbol.  04.02.82

    $(rpt
        LET p = 10
        item.type:=i.bad
        item.info:=null
        item.start:=linbuf.top-1
        item.found := TRUE
        ch := capitalch (ch)
        TEST 'A'<=ch<='Z' THEN
        $(  LET macro=?
            dicposn := 0
            p := 0
            $(rpt1
                IF p<255 DO p := p+1
                lex.space%p := ch
                rch()
                ch := capitalch (ch)
            $)rpt1 REPEATWHILE 'A'<=ch<='Z' | ch=sepch | '0'<=ch<='9'
            lex.space%0 := p
            p := p + 1
            UNTIL p REM bytesperword = 0 DO    // Now pad the string with 0s
            $(  lex.space%p := 0             // up to word boundary as
                p := p + 1                     // required by symb tab routines
            $)
            item.type := i.iden
            item.info := lex.space
            macro := looktype(type.text, item.info)
            IF iden.valid & expand.text THEN
            $(  IF print.expansions THEN
                $(  IF first.item.pos<0 THEN first.item.pos := linbuf.top
                    // delete name in listing:
                    linbuf.top := item.start
                $)
                call.macro(macro.var(1, 1), macro, macro%0+1, endstreamch)
                item.found := FALSE
            $)
        $) ELSE
        TEST ch=comntch THEN
        $(  // 'comntch' is a variable set by the user of the general assembler:
            // so it is checked first to override other definitions of that
            // character.
            item.type := i.comnt
            item.info := ch
        $) ELSE
        $(  LET no.rch=FALSE
            // this is TRUE unless a final 'rch' is needed to get the next char
            SWITCHON ch INTO
             // single character items first
             $(  CASE '*S':   item.type:=i.space            ; ENDCASE
                 CASE ',' :   item.type:=i.comma            ; ENDCASE
                 CASE '[' :   item.type:=i.lsqb             ; ENDCASE
                 CASE ']' :   item.type:=i.rsqb             ; ENDCASE
                 CASE '(' :   item.type:=i.lbkt             ; ENDCASE
                 CASE ')' :   item.type:=i.rbkt             ; ENDCASE
                 CASE '+' :   item.type:=i.plus             ; ENDCASE
                 CASE '-' :   item.type:=i.minus            ; ENDCASE
                 CASE '**':   item.type:=i.mult             ; ENDCASE
                 CASE '/' :   item.type:=i.div              ; ENDCASE
                 CASE '@' :   item.type:=i.immed            ; ENDCASE
                 CASE '=' :   item.type:=i.equals           ; ENDCASE
                 CASE ';' :   item.type:=i.semi             ; ENDCASE
                 CASE ':' :   item.type:=i.colon            ; ENDCASE
                 CASE '&' :   item.type:=i.and              ; ENDCASE
                 CASE '|' :   item.type:=i.or               ; ENDCASE
                 CASE '$' :   item.type:=i.dollar           ; ENDCASE
                 CASE '%':    item.type:=i.percent          ; ENDCASE
                 CASE '\':
                 CASE '~':    item.type:=i.not              ; ENDCASE
                 CASE '^':    item.type:=i.cap              ; ENDCASE
                 CASE '_':    item.type:=i.ul               ; ENDCASE
                 CASE '!':    item.type:=i.pling            ; ENDCASE
                 CASE '*N':   item.type:=i.stop;no.rch:=TRUE; ENDCASE
                 CASE endstreamch: item.type:=i.end
                                   no.rch:=TRUE
                                   ENDCASE
                 CASE '>' :
                 CASE '<' :
                   item.type := (ch='>' -> i.gt, i.lt)
                   item.info := ch
                   rch()
                   IF ch=item.info THEN
                   $(  item.type:=(ch='>'->i.shr,i.shl)
                       rch()
                   $)
                   no.rch:=TRUE
                   ENDCASE
                 CASE '"' :
                 CASE '*'':
                   $(  LET p = 0
                       LET del = ch
                       item.type := (ch='"'->i.string, i.sstring)
                       item.info := lex.space
                       rch()
                       UNTIL ch=del DO
                       $(  LET c = ?
                           IF ch='*N' | ch=endstreamch THEN
                           error(e.expected, del)
                           c := ch
                           IF c='**' DO
                           $(  rch()
                               c:=VALOF
                               SWITCHON capitalch(ch) INTO
                               $(  CASE '0': RESULTIS 0
                                   CASE 'T': RESULTIS '*T'
                                   CASE 'L': RESULTIS 10
                                   CASE 'P': RESULTIS '*P'
                                   CASE 'N': RESULTIS '*N'
                                   CASE 'C': RESULTIS 13
                                   CASE 'E': RESULTIS 27
                                   CASE 'S': RESULTIS '*S'
                                   CASE '*N':
                                     endline(TRUE)
                                     newlyne()
                                     rch() REPEATUNTIL ch\='*S'
                                     UNLESS ch=c DO error(e.expected,c)
                                     rch()
                                     LOOP
                                   DEFAULT: RESULTIS ch
                               $)
                           $)
                           IF p=255 DO error (e.fullstring)
                           p:=p+1
                           lex.space%p := c
                           rch()
                       $)
                       lex.space%0:=p
                   $)
                   ENDCASE
                 CASE '#':
                   rch ()
                   TEST '0'<=ch<='7' THEN p:=8 ELSE
                   $(  SWITCHON capitalch(ch) INTO
                        $(  CASE 'B':  p:=2 ; ENDCASE
                            CASE 'O':  p:=8 ; ENDCASE
                            CASE 'D':  p:=10; ENDCASE
                            CASE 'X':  p:=16; ENDCASE
                            DEFAULT :  error(e.badbase,ch)
                        $)
                        rch()
                   $)
                  CASE '0': CASE '1': CASE '2': CASE '3': CASE '4':
                  CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
                   $(  LET valid = FALSE
                       no.rch:=TRUE
                       item.type := i.number
                       item.info := 0
                       $(rpt1
                           LET d = ('0'<=ch<='9') -> ch-'0',
                           ('A'<=ch<='F') -> ch+uletter,
                           ('a'<=ch<='f') -> ch+lletter, 999
                           IF d=999
                           DO TEST valid
                              THEN ENDCASE
                              ELSE BREAK
                           UNLESS d<p BREAK
                           item.info:=item.info*p + d
                           rch()
                           valid := TRUE
                       $)rpt1 REPEAT
                       error(e.badbase,ch)
                   $)
                 DEFAULT: error(e.badsym)
             $)
             UNLESS no.rch THEN rch()
        $)
        IF item.found & first.item.pos>=0 THEN position.ch(first.item.pos)
        WHILE ch='*S' DO rch()
    $)rpt REPEATUNTIL item.found
    // REPEAT is also for macro expansion part of code (at the top)
    RESULTIS item.type
$)


AND scan(item) = VALOF
TEST item=item.type
THEN $(  getitem();  RESULTIS TRUE  $)
ELSE RESULTIS FALSE


LET checkandskip (item, errparm) BE
TEST item.type=item
THEN getitem ()
ELSE error (e.expected, errparm)












///SECTION "asm7"








//
//                       Parsing  Procedure
//



LET parse(file) BE
$(  /* this routine operates in conjunction with 'nextpass'
       to set up the two instances of the input file 'in.first'
       and 'in.second' and to alternate between them in order
       to parse each section of the text twice.
           The state of the parser at the beginning of each 'pass'
       is saved in the global workspace 'state' and is used to
       restore that state at the end of the first pass.
    */
    LET savewrch = wrch
    LET pass.first = ?
    LET pass.second = ?
    LET max.mem = 0
    LET endmissing = FALSE
    LET end.was.missing = FALSE
    syswrch := wrch
    warnvec := TABLE 0,0,0,0,0
    errcount := 0
    linbufsize  := pw+1 - (15+(1+2*bytesperasmword)*binbufwidth+2)
    linbuf      := simplegetvec(linbufsize/bytesperword+1)
    deleteline()
    item.start  := linbuf.top-1
    lex.space   := simplegetvec(256/bytesperword-1)
    lab.space   := simplegetvec(256/bytesperword-1)
    binbuf      := simplegetvec(binbufsize+b.nextmod)
    binbuf!b.top := b.nextmod
    binbuf!b.nextmod := binbuf!b.top
    macro.stack := null
    filling.macro := 0
    print.expansions := TRUE
    expand.text := TRUE
    entrypoint := #XFFFF
    sectno:=1
    deleteline()
    lineno:=0
    line.of.file:=0
    file.id:=0
    files := simplegetvec(max.files)
    files!0 := 0
    resetfiles(line.of.file)
    pc := 0
    mode := null
    badlabs := 0
    outpos := 1<<8
    wrch := mywrch
    def.count := 0
    max.temp.used := 0
    pagetitle := null
    modulename := null
    finishpass := FALSE
    fatal := FALSE
    ch := '*S'
    pass := first
    pass.first := findasmin(file)
    pass.second := (pass.first=0->0,findasmin(file))
    IF pass.first=0 | pass.second=0 THEN error(e.badfile, file)
    selectinput(pass.first)
    startparse()
/// //*<SECT:
    state := TABLE 0,0,0,0,0,0,0,0
    state!in.lineno := line.of.file
    state!in.errcount := errcount
    state!in.first := pass.first
    state!in.second := pass.second
    state!in.list := list
    state!in.memlevel := memory!memsize
/// /*SECT>*/
    code.gen(cd.module, TRUE, "")
    $(rpt
        LET had.text=FALSE
        had.text := had.text | parseline() REPEATUNTIL
                item.type=i.end | finishpass | fatal
        $(  LET mem.used = memsize - memory!memsize + max.temp.used
            IF mem.used>max.mem THEN max.mem:=mem.used
        $)
        UNLESS fatal THEN
        $(  end.was.missing:=endmissing
            endmissing:=(pass=first & item.type=i.end & had.text)
            IF item.type\=i.end | endmissing THEN nextpass()
        $)
    $)rpt REPEATUNTIL fatal | item.type=i.end & \endmissing
    wrch := savewrch
    pass := second      // nextpass might have left it as 'first'
    code.gen(cd.eof,entrypoint)
    endparse()
    $(  LET save.out=output()
        selectoutput(out)
        IF end.was.missing THEN
        $(  writes("END directive missing*N")
            errcount := errcount+1
        $)
        writef("Maximum workspace used is %N words*N",max.mem)
        selectoutput(save.out)
    $)
    selectinput(pass.first)
    endread()
    selectinput(pass.second)
    endread()
$)


AND startparse() BE RETURN      // for redefinition

AND endparse() BE RETURN        // for redefinition


AND nextpass() BE
$(  endparse()
    WHILE end.macro.var() DO rch()     // unstack any "open" macros
    TEST pass=first THEN
    $(  code.gen(cd.clear)    // clear CODE.GEN's buffers
        pass:=second
        IF mode=null THEN mode:=absolute
        code.gen(cd.module, mode=absolute, modulename)
        code.gen(cd.code,  name,  ((bytesperasmword & #XF) << 12) |
                 ((bytesperasmword * wordsperaddress & #XF) << 8) |
                 (msbytefirst -> #X01, #X00))
        $(  LET stamp=VEC 15
            code.gen(cd.prag, get.time(stamp, 15), "DAT-A")
        $)
        $(  LET saveout=output()
            selectoutput(out)
            wrch('*N')
            selectoutput(saveout)
        $)
/// //-*<SECT:
        errcount:=state!in.errcount
        list:=state!in.list
        line.of.file := state!in.lineno

        errcount:=0
/// /-*SECT>*/
        file.id := 0
        restartpage:=throws
        state!in.first:=input()
        selectinput(state!in.second)
    $) ELSE
    $(  gen.defs()  // clears internal buffers in CODE
/// //-*<SECT:
        pass:=first
        state!in.errcount:=errcount
        code.gen(cd.module, TRUE, "")     // to inform CODE.GEN of new module
        state!in.second:=input()
        state!in.list:=list
        state!in.lineno := line.of.file
/// /-*SECT>*/
        IF list>0 | allsyms | badlabs>0 THEN
        $(  printlabs()
            printfiles()
        $)
        deletelabs()
/// //-*<SECT:
        pagetitle:=null
        modulename:=null
        sectno:=sectno+1
        selectinput(state!in.first)
        memory!memsize := state!in.memlevel   // reset memory to previous limit
        memory!0 := 1                         // reset tempvec
        max.temp.used := 0
/// /-*SECT>*/
    $)
    resetfiles(line.of.file)
    print.expansions := TRUE
    expand.text := TRUE
    mode:=null
    pc:=0
    ch:='*n'
    lineno := 0
    finishpass:=FALSE
    def.count:=0
/// //-*<SECT:
    startparse()
/// /-*SECT>*/
$)



AND parseline() = VALOF
$(  LET savlev=errorlevel
    LET savlab=errorlabel
    LET mt.line=FALSE
    LET lab = lab.space
    LET mnem = ?
    LET found.mnem = ?
    errorlevel:=level()
    errorlabel:=xit
    mnem := read.to.mnem(@mt.line, @found.mnem, @lab, TRUE)
//  writef("Parseline: mnem = *"%S*" mt.line=%C found.mnem=%C lab=*"%S*"*N",
//         (mnem=null -> "<none>", mnem!c.str), (mt.line->'T','F'),
//         (found.mnem->'T','F'), (lab=0 -> "<none>", lab))
    IF found.mnem THEN
    TEST mnem=null THEN
    $(  // illegal mnemonic
//      writef("Parseline: calling error (#X%X8) lab=%X8 lev=%X8*N",
//             error, errorlabel, errorlevel)
        error(e.badcode)
    $) ELSE
    $(  FOR i=1 TO (mnem+c.opstr)!0 DO putword((mnem+c.opstr)!i)
//      writef("About to execute '%S'*N", item.info)
        dontknow := FALSE
        (mnem!c.fn) (lab, (mnem+c.opstr)!1)
        IF comntcheck THEN
        UNLESS item.type=i.comnt | item.type=i.stop | item.type=i.end
        THEN error(e.nocomnt, comntch)
    $)
    IF warning THEN error(0)
    endline(TRUE)
xit:
//-*<TRIPOS:
    IF testflags(1) THEN
    $(  writef("****** BREAK: in %S on %S pass of section %N*N", name,
                (pass=first -> "first", "second"), sectno)
        fatal := TRUE
    $)
    finishpass := finishpass | fatal
/// /-*TRIPOS>*/
    errorlevel:=savlev
    errorlabel:=savlab
//  writef("Parseline: end of %S pass line FATAL=%C FINISHPASS=%C MT.LINE=%C*N",
//         (pass=second -> "second","first"), (fatal->'T','F'),
//         (finishpass -> 'T','F'), (mt.line -> 'T','F'))
    RESULTIS \mt.line
$)





AND read.to.mnem(lv.mt.line, lv.found.mnem, lv.lab, active.read) = VALOF
$(  LET mnem = null
    startpc := pc
    warning := FALSE
    dontknow := FALSE
    newlyne()
    getitem()
    !lv.found.mnem := FALSE
    !lv.mt.line := (item.type=i.stop | item.type=i.end | item.type=i.comnt)
//  writef("Find.mnem: %Sactive read on line %N starting with item %N*N",
//         (active.read -> "","non-"), lineno, item.type)
    UNLESS !lv.mt.line THEN
    $(  TEST scan(i.space) THEN !lv.lab := null ELSE
        TEST item.type=i.iden THEN
        $(  // Copy the identifier string into label buffer.
            // A word copy is done in order to preserve the
            // 0s padding the string to word size, as these are
            // required by symbol table routines.  A word copy
            // should also be faster!  16.07.81 - NJO
            FOR i=0 TO item.info%0 / bytesperword DO (!lv.lab)!i := item.info!i
            getitem()
            scan(i.endlab)
        $)
        ELSE error(e.nolab)
        !lv.mt.line:=(!lv.lab=null &
                 (item.type=i.stop | item.type=i.end | item.type=i.comnt))
        TEST item.type=i.comnt | item.type=i.stop | item.type=i.end THEN
            IF active.read THEN get.and.declare(!lv.lab)
        ELSE
        $(  LET no.expand = scan(i.pling)
            TEST item.type\=i.iden THEN
            $(  UNLESS !lv.lab=null & active.read THEN get.and.declare(!lv.lab)
                error(e.badcode)
            $) ELSE
            $(  LET macro = ?
                UNLESS no.expand THEN
                macro := looktype(type.macro, item.info)
//              writef("           found symbol *"%S*" (%Sa macro)*N",item.info,
//                     (iden.valid -> "","not "))
                !lv.found.mnem := TRUE
                TEST \no.expand & iden.valid THEN
                $(  LET static.mnem = TABLE 0,0,0,0
                    mnem := static.mnem
                    mnem!c.opstr := 0               // don't put down any code
                    mnem!(c.opstr+1) := macro       // pass macro to CALL proc
                    mnem!c.fn := callmacroproc
                    mnem!c.str := item.type
                $) ELSE mnem := getcode(item.info)
            $)
        $)
    $)
    RESULTIS mnem
$)




AND get.and.declare(lab) BE
$(  // This is the standard procedure called by Mnemonic Procedures which want
    // any label on their line to be treated in the default way (i.e. have the
    // current program counter assigned to it).
    UNLESS lab=null THEN
    $(  IF mode=null THEN mode:=absolute
        IF pass=first THEN
            // DEF label if 'def.count' hasn't run out yet
            putlab(lab, pc, (mode=absolute->type.lab, type.rellab) |
                            (def.count>0->flag.def,0))
        UNLESS def.count=0 THEN def.count:=def.count-1
    $)
    getitem()
$)



AND newsection() BE RETURN            // for redefinition






//
//                     Expression   Analysis
//





LET is.expression() = VALOF
TEST item.type=i.here
THEN RESULTIS TRUE
ELSE SWITCHON item.type INTO
     $(  CASE i.iden:
         CASE i.number:
         CASE i.sstring:
         CASE i.minus:
         CASE i.plus:
         CASE i.not:
           RESULTIS TRUE
         DEFAULT:
           RESULTIS item.type=i.elbkt | item.type=i.strlbkt
     $)



AND expression() = VALOF
$(  LET spec=VEC spec.size
    label.expression(spec)
    UNLESS spec!1=0 THEN warn(e.badrel)
    RESULTIS spec!0
$)



AND pcrel.expression() = VALOF
$(  LET spec=VEC spec.size
    label.expression(spec)
    IF mode=null THEN mode:=absolute
    UNLESS mode=absolute THEN spec!1:=spec!1-1
    UNLESS spec!1=0 THEN warn(e.badrel)
    RESULTIS spec!0-pc
$)



AND label.expression(ansvec) = VALOF
$(  /*     This procedure parses an expression consisting of the operators
       |, &, <<,>>, +,-, *,/  with that relative precedence.  The components
       of the expression are parsed by 'term' (q.v.) and may consist of
       relative, absolute or external items.
           An expression including an external item may not contain any
       other item, nor a monadic -.  Expressions including relative items
       may not contain relative sub expressions in any operators other
       than + and - and must be formatted in such a way that the resulting
       expression has either an equal number of +ve and -ve relative sub
       expressions, in which case the resulting expression is absolute, or
       one more +ve relative sub expression than -ve, in which case the
       resulting expression is itself relative.
           A vector 'spec' is returned with the following values in the
       following offsets:

                offset 0:  the absolute or relative value of the expression
                           or, if the expression was an external, the address
                           from which this symbol was last referenced.
                offset 1:  the number of times that a relative offset must be
                           added to offset 0 at load time to get an absolute
                           value.  Only 0 (absolute) and 1 (relative) are
                           valid except if the expression was external in which
                           case this offset is large and non zero.
                offset 2:  zero unless the expression was external, in which
                           case it will be the address of the external symbols
                           value field, in which is maintained the assembly time
                           address of the last reference.
    */
    ande(ansvec)
    WHILE scan(i.or) DO
    $(  LET a=VEC 2
        ansvec!0 := ansvec!0 | ande(a)
        UNLESS ansvec!1=0 & a!1=0 THEN warn(e.badrel)
        bracketed:=FALSE
    $)
    RESULTIS ansvec
$)


AND ande(ansvec) = VALOF
$(  shifte(ansvec)
    WHILE scan(i.and) DO
    $(  LET s=VEC 2
        ansvec!0 := ansvec!0 & shifte(s)
        UNLESS ansvec!1=0 & s!1=0 THEN warn(e.badrel)
        bracketed:=FALSE
    $)
    RESULTIS ansvec!0
$)


AND shifte(ansvec) = VALOF
$(  LET not.val = scan(i.not)
    adde(ansvec)
    WHILE item.type=i.shr | item.type=i.shl DO
       $(  LET s=item.type
           LET a=VEC 2
           getitem()
           TEST s=i.shr
           THEN ansvec!0:=ansvec!0 >> adde(a)
           ELSE ansvec!0:=ansvec!0 << adde(a)
           UNLESS ansvec!1=0 & a!1=0 THEN warn(e.badrel)
           bracketed:=FALSE
       $)
    IF not.val THEN ansvec!0:=\ansvec!0
    RESULTIS ansvec!0
$)


AND adde(ansvec) = VALOF
$(  product(ansvec)
    $(rpt
        LET s = +1
        SWITCHON item.type INTO
        $(  DEFAULT:      RESULTIS ansvec!0
            CASE i.minus: s := -1
            CASE i.plus :
              $(  LET p=VEC 2
                  getitem ()
                  ansvec!0 := ansvec!0 + s * product (p)
                  ansvec!1 := ansvec!1 + s * p!1
                  IF ansvec!2 /* last external reference */ \= 0 | p!2 \= 0 THEN
                  warn(e.badext)
                  bracketed:=FALSE
              $)
              LOOP
        $)
    $)rpt REPEAT
$)


AND product(ansvec) = VALOF
$(  term(ansvec)
    WHILE item.type=i.div | item.type=i.mult DO
       $(  LET s=item.type
           LET t=VEC 2
           getitem()
           TEST s=i.div
           THEN TEST term(t)=0
                THEN warn(e.divzero)
                ELSE ansvec!0:=ansvec!0/t!0
           ELSE ansvec!0:=ansvec!0*term(t)
           UNLESS ansvec!1=0 & t!1=0 THEN warn(e.badrel)
           bracketed:=FALSE
       $)
    RESULTIS ansvec!0
$)



AND term(ansvec) = VALOF
$(  /*      This procedure parses the elements of an expression.  They
        may be preceeded by an arbitrary number of monadic sign symbols.
        They may be any of the following:

                a number: as defined by the lexical analyser 'getitem'
                a single quoted string: returns the value of the ascii
                        characters packed into a word.
                the 'here' symbol i.here: returns the value of the
                        program counter - a relative value in a relative
                        section and an absolute one in an absolute one.
                an external symbol: returns the address where it was last
                        used.  Offset 1 of 'ansvec' is set high to guarantee
                        that it will not indicate an absolute expression.
                        Offset 2 is set to a pointer to the information field
                        in the symbol's descriptor where the address where it
                        was last used is kept.
                a relative symbol: returns its relative address and sets
                        offset 1 of 'ansvec' to +/- 1 (depending upon the
                        number of monadic minus signs parsed).
                an absolute symbol: returns the value of the symbol and sets
                        offset 1 of 'ansvec' to 0.
                a bracketed expression: see 'label.expression'
    */
    LET sign = +1
    ansvec!0 := -1     // default value
    ansvec!1 := 0      // relocation count
    ansvec!2 := 0      // last reference of external symbol
    bracketed:=FALSE
    $(rpt
        SWITCHON item.type INTO
        $(  CASE i.minus:
                 sign := -sign
            CASE i.plus:
                 getitem ()
                 LOOP
            CASE i.iden:
                 $(  LET d=dontknow
                     UNLESS getlab(item.info, ansvec)=null THEN
                     $(  IF pass=second & (ansvec!1=type.none | \iden.valid)
                         THEN warn(e.badlab)
                         TEST (ansvec!1 /* type */ & type.mask) = type.def THEN
                             TEST mode=relative THEN
                             $(  ansvec!1:=1    // relocation count
                                 ansvec!2:=0
                             $) ELSE
                             $(  mode:=absolute
                                 ansvec!1:=0
                                 ansvec!2:=0
                             $)
                         ELSE
                         TEST (ansvec!1 /* type */ & flag.rel)\=0 THEN
                         $(  ansvec!1:=1   // relocation count
                             ansvec!2:=0
                         $) ELSE
                         TEST (type.mask&ansvec!1) /* type */ = type.ref THEN
                         $(  ansvec!1 := 1000   // a big number
                             IF sign=-1 THEN error(e.badext)
                         $) ELSE
                         $(  ansvec!1:=0
                             ansvec!2:=0
                         $)
                     $)
                     UNLESS iden.valid DO warn(e.badlab)
                     getitem()
                     dontknow:= dontknow | d
                 $)
                 ENDCASE
            CASE i.number:
                 ansvec!0:=item.info
                 getitem()
                 ENDCASE
            CASE i.sstring:
                 ansvec!0 := 0
                 FOR i=1 TO item.info%0 DO
                    ansvec!0:= (ansvec!0<<8) + cvtchar(item.info%i)
                 getitem()
                 ENDCASE
            DEFAULT:
                 TEST item.type=i.here THEN
                 $(  ansvec!0 := pc
                     TEST mode=relative THEN ansvec!1:=1 ELSE mode:=absolute
                     getitem ()
                 $) ELSE
                 TEST scan(i.elbkt) THEN
                 $(  label.expression(ansvec)
                     checkandskip(i.erbkt,')')
                     bracketed:=(sign=+1)
                     // in case '-' sign in front of bracket
                 $) ELSE
                 TEST item.type=i.strlbkt | item.type=i.percent |
                      item.type=i.pling THEN
                 $(  LET str = strexp(null, 0)
                     ansvec!0 := 0
                     FOR i=1 TO item.info%0 DO
                        ansvec!0 := (ansvec!0 << 8) + cvtchar(str%i)
                     getitem()
                 $) ELSE error(e.badnum)
        $)
        ansvec!0 := ansvec!0*sign
        ansvec!1 := ansvec!1*sign
        RESULTIS ansvec!0
    $)rpt REPEAT
$)



AND is.type(ty) = VALOF
$(  LET ans=FALSE
    IF item.type=i.iden DO ans:=checktype(ty, item.info)
    RESULTIS ans
$)



















///SECTION "asm8"





//
//                   MNEMONIC  IMPLEMENTATION
//






//      This is a list of predeclared Mnemonic Procedures.  They must be
// inserted into the tree of mnemonic definitions using 'code.put'.
// They interpret their command lines in standard ways that are likely to
// 'fit' (perhaps a little losely) the prefered directives given in a
// target assembler.





LET dataproc(lab) BE
$(  get.and.declare(lab)
    $(rpt
        TEST item.type=i.string | item.type=i.sstring | item.type=i.strlbkt THEN
        $(  LET offset=(item.type=i.string->0,1)
            IF item.type=i.strlbkt THEN item.info := strexp(null, 0, FALSE)
            FOR i=offset TO offset+(item.info%0-offset)/bytesperasmword DO
            $(  LET pack=0
                FOR j=0 TO bytesperasmword-1 DO
                 $(  LET char=(i*bytesperasmword+j>item.info%0 -> '*S',
                          cvtchar(item.info%(i*bytesperasmword+j)) )
                     TEST mscharfirst THEN pack:=(pack<<8) | char ELSE
                     pack := (pack>>8) | (char<<(8*(bytesperword-1)))
                  $)
                  putword(pack)
             $)
             getitem()
         $) ELSE TEST scan(i.equals) THEN
         TEST \is.expression() THEN error(e.badnum) ELSE
         $(  LET spec=VEC spec.size
             putlabelspec(label.expression(spec))
         $) ELSE
         TEST \is.expression() THEN error(e.badnum) ELSE
         $(  LET no = expression()
             putword(no)
         $)
    $)rpt REPEATWHILE scan(i.comma)
$)



AND titlproc(lab) BE
$(  getitem()
    TEST pass=first THEN
    $(  LET savout=output()
        STATIC
        $(  done=-1
        $)
        IF done<sectno THEN
        $(  pagetitle := getstr(strexp(null, 0, TRUE), simplegetvec)
            modulename := (lab=null -> pagetitle, getstr(lab, simplegetvec))
            selectoutput(out)
            writef("Assembling *"%S*" ", pagetitle)
            selectoutput(savout)
            done:=sectno
        $)
    $) ELSE
    $(  STATIC
        $(  done=-1
        $)
        LET str = ?
        UNLESS done<sectno THEN error(e.titledone)
        str := strexp(null, 0, TRUE)
        code.gen(cd.prag, str, "TITLE")
        done:=sectno
    $)
    getitem()
$)



AND equproc(lab) BE
TEST lab=null THEN error(e.nolab) ELSE
$(  getitem()
    TEST \is.expression() THEN warn(e.badnum) ELSE
    $(  LET spec=VEC spec.size
        label.expression(spec)
        TEST spec!2 /* external */\=0 THEN error(e.badrel) ELSE
        putlab(lab, spec!0, (dontknow & flag.fwd) |
                    (spec!1=0->type.const, type.relconst))
    $)
$)



AND setproc(lab) BE
TEST lab=null THEN error(e.nolab) ELSE
$(  getitem()
    TEST \is.expression() THEN error(e.badnum) ELSE
    $(  LET spec=VEC spec.size
        label.expression(spec)
        TEST dontknow THEN error(e.forward) ELSE
        TEST spec!2 /* external */\=0 THEN error(e.badrel) ELSE
        putlab(lab, spec!0, (spec!1=0->type.var, type.relvar))
    $)
    getitem()
$)



AND textproc(lab) BE
$(  LET str = ?
    getitem()
    IF lab=null DO error(e.nolab)
    str := strexp(null, 0, TRUE)
    IF pass=first DO putlab(lab, getstr(str, simplegetvec), type.text)
    getitem()
$)



AND endproc(lab) BE
$(  get.and.declare(lab)
    IF is.expression() THEN
    $(  LET spec=VEC spec.size
        LET e= !label.expression(spec)
        TEST spec!1\=0 & spec!1\=1 THEN  error(e.badrel) ELSE
        IF pass=second THEN
        TEST entrypoint=#XFFFF THEN
        entrypoint:=e
        ELSE error(e.newentry)
    $)
    finishpass:=TRUE
$)



AND orgproc(lab) BE
$(  LET spec=VEC spec.size
    get.and.declare(lab)
    label.expression(spec)
    TEST spec!1\=0 & spec!1\=1 THEN error(e.badrel) ELSE
    TEST dontknow THEN error(e.forward) ELSE
    $(  pc:=spec!0
        startpc:=pc
    $)
$)



AND storeproc(lab) BE
$(  LET i=?
    get.and.declare(lab)
    i:=expression()
    TEST dontknow THEN error(e.forward) ELSE
    TEST i<0 THEN error(e.posnum) ELSE
    $(  code.gen(cd.newpc, pc)
        pc:=pc+i
        code.gen(cd.newpc, pc)
    $)
$)



AND absproc(lab) BE
$(  pc:=0
    startpc:=0
    TEST mode=null THEN mode:=absolute ELSE error(e.modeset)
    UNLESS lab=null | pass=second THEN putlab(lab,pc,type.lab)
    getitem()
$)



AND relproc(lab) BE
$(  pc:=0
    startpc:=0
    TEST mode=null THEN mode:=relative ELSE error(e.modeset)
    UNLESS lab=null | pass=second THEN putlab(lab,pc,type.rellab)
    getitem()
$)



AND declaration.list(lab, default.value, type) BE
$(  get.and.declare(lab)
    $(rpt
        UNLESS item.type=i.iden THEN error(e.nolab)
        putlab(item.info, default.value, type)
        getitem()
    $)rpt REPEATUNTIL \scan(i.comma)
$)



AND refproc(lab) BE declaration.list(lab, #XFFFF, type.ref)



AND wrefproc(lab) BE declaration.list(lab, #XFFFF, type.wref)



AND needsproc(lab) BE
$(  // The #XFFFF in this procedures definition represents a null pointer in
    // the generated code.  This, like REF variables, is because all references
    // to this symbol will be assembled as a pointer to the last reference to
    // that symbol in the output code. (and so the first must be the null
    // pointer to terminate the list).
    // This feature is rather Intel/Mostec/Cambridge Hex ortientated and may
    // be improved at length.   20.01.81
    get.and.declare(lab)
    $(rpt
        TEST item.type=i.iden THEN
        putlab(item.info, #XFFFF, type.ref | flag.needs) ELSE
        $(  LET level=0
            UNLESS item.type=i.string | item.type=i.sstring THEN
            $(  level:=expression()
                scan(i.colon)
            $)
            TEST item.type=i.string | item.type=i.sstring THEN
            code.gen(cd.opt, item.info, level) ELSE error(e.nostr)
        $)
        getitem()
    $)rpt REPEATUNTIL \scan(i.comma)
$)



AND defproc(lab) BE
$(  //  DEF variables start of life with undefined types 'type.none' and are
    //  only given a type when they are defined and given a proper value.
    get.and.declare(lab)
    $(rpt
        TEST item.type=i.iden THEN
        $(  putlab(item.info, -1, type.def)
            getitem()
        $) ELSE
        $(  LET i=expression()
            TEST dontknow THEN error(e.forward) ELSE def.count:=i
        $)
    $)rpt REPEATUNTIL \scan(i.comma)
$)



AND pragproc(lab) BE
TEST lab=0 THEN error(e.nolab) ELSE
$(  LET str = ?
    getitem()
    str := strexp(null, 0, TRUE)
    IF pass=second THEN code.gen(cd.prag, str, lab)
    getitem()
$)



AND spaceproc(lab) BE
$(  LET spaces=?
    get.and.declare(lab)
    spaces:=expression()
    IF pass=second THEN
    TEST dontknow THEN error(e.forward) ELSE
    IF list>0 THEN
    $(  deleteline()
        FOR i=1 TO spaces-1 DO putch('*N')
    $)
$)


AND ejectproc(lab) BE
$(  LET lines=?
    get.and.declare(lab)
    lines := is.expression() -> expression(), pl
    IF pass=second THEN
    TEST dontknow THEN error(e.forward) ELSE
    IF list>0 THEN
    $(  deleteline()
        neads(lines)
    $)
$)


AND listproc(lab) BE
$(  get.and.declare(lab)
    UNLESS list>0 THEN deleteline()
    TEST is.expression() THEN
    list:=list+expression() ELSE list:=list+1
$)


AND getproc(lab) BE
$(  get.and.declare(lab)
    TEST (i.string\=item.type\=i.sstring)
    THEN error(e.nostr) ELSE
    $(  LET s=findgetin(item.info%0=0->fromfile, item.info)
        TEST s=0
        THEN error(e.badfile,item.info) ELSE
        $(  LET savein = input()
            LET saveln = line.of.file
            LET savelist = list
            LET savefno = file.id
            LET fno = newfile((item.info%0=0->fromfile,item.info))
            endline(TRUE)
            file.id:=fno
            selectinput(s)
            list:=list-1
            line.of.file:=0
            parseline() REPEATUNTIL
                item.type=i.end | finishpass | fatal
            endread()
            selectinput(savein)
//*<SECT:
            finishpass:=fatal
/*SECT>*/
            endfile(fno)
            file.id := savefno
            item.type := i.stop    // i.e. not i.end or we'll stop!
            ch  :=  '*N'           // or we'll stop because of end of file!
            line.of.file := saveln
            list := savelist
        $)
    $)
$)




/*  This procedure unnecessary since introduction of relocation 13.03.80
AND envproc(lab) BE
$(  get.and.declare(lab)
    TEST item.type\=i.string
    THEN error(e.nostr)
    ELSE IF pass=first DO savedic(findoutput(item.info))
    getitem()
$)
*/














///SECTION "asm9"









//
//                Command  Line  &  Machine  Dependent  Parts
//






// startasm is defined at the end of this file.
//LET startasm(version) = 0 //"Undefined assembler section"
//  This procedure is to be redefined in the user's part of the assembler.
//  The 'version' given is a number in the thousands.  The number of thousands
//  must match the number of thousands of the version of the general assembler
//  that a specific assembler was written under since changing this number
//  indicates a non compatible change in the interface.
//  Compatible changes iterate through the lower orders of the number.
//  If the version is found to be incompatible a string should be returned
//  giving the version of the assembler that was expected -- otherwise zero
//  should be returned.





LET endasm() BE RETURN



LET useropt(ch) = FALSE            // for redefinition


LET decodeopt(s) = VALOF
$(  LET error=FALSE
    LET temp = ?
    // defaults for options have already been set up
    UNLESS s=null DO FOR p=1 TO s%0 DO
    $(  TEST useropt(s%p) THEN LOOP ELSE
        SWITCHON capitalch(s%p) INTO
        $(  CASE 'P':
              /* include page throws in output */
              throws:=TRUE
              LOOP
            CASE 'C':
              /* compress output listing */
              short:=TRUE
              LOOP
            CASE 'W':
              /* page width (default is 76 chars) */
              temp := @pw
              ENDCASE
            CASE 'H':
              /* width of relocatable binary output (default 16 words) */
              temp := @hexoutwidth
              ENDCASE
            CASE 'S':
              /* amount of store available for label dictionary */
              temp := @memsize
              ENDCASE
            CASE 'L':
              /* depth of 'list 1' directives or getfiles to ignore */
              temp := @list
              ENDCASE
            CASE 'F':
              /* Set the maximum number of header files allowed */
              temp := @max.files
              ENDCASE
            CASE 'A':
              /* For All symbols to be printed in the dictionary */
              allsyms:=TRUE
            CASE ',': CASE '/': CASE '*S':
              LOOP
            DEFAULT:
              writef("Unknown option - *'%C*'*N",s%p)
              error:=TRUE
              LOOP
          $)
          // read in a number and put it in the location
          // indicated by temp:
          $(  LET flag = TRUE
              !temp := 0
              $(rpt
                  p := p+1
                  UNLESS p>s%0 DO SWITCHON s%p INTO
                  $(  CASE '1': CASE '2': CASE '3': CASE '4': CASE '5':
                      CASE '6': CASE '7': CASE '8': CASE '9': CASE '0':
                        !temp := 10*!temp + s%p - '0'
                        flag  := FALSE
                        LOOP
                      CASE '*S': IF flag LOOP
                  $)
                  IF flag DO !temp := -1
                  BREAK
              $)rpt REPEAT
              p := p-1
          $)
      $)
    IF list<0 DO list := 1
    hexoutwidth:=hexoutwidth/bytesperasmword
    error:=error | memsize<0 | pw<50 | \1<=hexoutwidth<=32 | max.files<=1
    RESULTIS error
$)





/// //-*<TRIPOS
MANIFEST
$(  bad.rc = 20
    startargsize = 100/bytesperword
$)
LET getargs(startarg) = VALOF
$(  LET vect = getvec(startargsize)
    IF vect\=0 THEN
    TEST 0=rdargs("Prog=From/a,To=Hex,List,Opt/k",vect, startargsize) THEN
    $(  freevec(vect)
        vect := 0
    $) ELSE
    IF vect!2~=0 THEN
    $(  // presume that options should be set up as for a listing:
        pw := 136         // page width for printer
        list := 1         // list one level by default
    $)
    RESULTIS vect
$)
LET findasmin(file.name) = VALOF
$(  LET rc=findinput(file.name)
    LET r2=result2
    IF rc=0 THEN
    $(  writef("Can't open '%S': ",file.name)
        fault(r2)
    $)
    RESULTIS rc
$)
LET findgetin(file.name) = VALOF
$(  LET ans=findinput(file.name)
    LET prefix="SYS:Ring."
    IF ans=0 & prefix%0+name%0<40 THEN
    $(  LET dir=VEC 20
        LET savedir=currentdir
        LET newdir=?
        FOR i=1 TO prefix%0 DO dir%i:=prefix%i
        FOR i=1 TO name%0 DO dir%(prefix%0+i):=name%i
        dir%0 := prefix%0+name%0
        newdir:=locatedir(dir)
        UNLESS newdir=0 THEN
        $(  currentdir:=newdir
            ans:=findinput(file.name)
            UNLESS newdir=savedir THEN freeobj(newdir)
            currentdir:=savedir
        $)
    $)
    RESULTIS ans
$)
LET get.time(vect, n) = VALOF
$(  LET v = TABLE  0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
    LET tim=v+5
    LET dat=v
    LET ans=vect
    STATIC $( got.dat = FALSE $)
    UNLESS got.dat THEN datstring(v)
    TEST n*2<=1+tim%0+dat%0 THEN ans:="" ELSE
    $(  FOR i=1 TO tim%0 DO vect%i:=tim%i
        vect%(tim%0+1):='*S'
        FOR i=1 TO dat%0 DO vect%(tim%0+i+1):=dat%i
        vect%0:=dat%0+tim%0+1
        got.dat := TRUE
    $)
    RESULTIS ans
$)
/// /-*TRIPOS>*/









LET start(startarg) BE
$(  LET error=TRUE
    LET args = 0   // argument vector
    LET initialised = FALSE
    LET listfile = ?

    writef("*nz80asm is going to be an assembler based on the one written*n")
    writef("for the Systems Research Group at Cambridge written by Gray Girling*n")
    writef("between 1980 and 1983. His original version was for a family of*n")
    writef("assemblers including ones for the 6502, the 6809 and the 8080*n")
    writef("This version is designed to run under the modern 32 BCPL Cintcode*n")
    writef("system to generate Intel Hex code from Z80 assembly language.*n*n")
    writef("This version is currently in the very early stages of development*n")
    writef("and does not work*n")

    $(  LET versionno = 3069
        LET versionid = "Version 3.069 27-Jan-83"
        LET asmver = ?
        msbytefirst := FALSE     // order of bytes in an address
        mscharfirst := TRUE      // order of characters in a word of a string
        binbufwidth := 4
        i.here := i.mult
        i.strlbkt := i.lt
        i.strrbkt := i.gt
        comntch:=';'
        i.elbkt := i.lbkt
        i.erbkt := i.rbkt
        i.endlab:=i.colon
        comntcheck := FALSE
        sepch := '.'
        bytesperasmword := 1
        wordsperaddress := 2
        name := "unnamed"
        asmver := startasm(versionno)
        TEST asmver\=0
        THEN writef("Incompatible assembler section version numbers*N*
             *General assembler id: %S*N*
             *Specific assembler id: %S*N",versionid,asmver)
        ELSE
        $(  // set up default values of options so that GETARGS can
            // change them should it so desire:
            memsize:=3000
            throws:=FALSE
            list:=0
            short:=FALSE
            allsyms:=FALSE
            pw:=80
            hexoutwidth:=32
            max.files:=25
            // now call machine specific routine to
            // return file names + option string
            args := getargs(startarg)
            TEST args=0 THEN
            writef("Bad arguments for %S assembler*N",name) ELSE
            TEST decodeopt(args!3) THEN
            writef("Bad OPT string for %S assembler*N", name) ELSE
            $(  LET outfile=args!2
                fromfile:=args!0
                writef("%S assembler.  %S*N",name,versionid)
                binfile := findoutput(args!1)
                listfile:= (outfile=0->output(), findoutput(outfile))
                out := output()
                memory := getvec(memsize)
                TEST memsize<100 | memory=0 THEN
                writes("Insufficient store for execution*N") ELSE
                TEST listfile=0
                THEN writef("Can't open %S for output*N",
                             (outfile=0 -> "OUTPUT stream", outfile) )
                ELSE error:=FALSE
                initialised:=TRUE
            $)
        $)
    $)
    UNLESS error THEN
    $(  UNLESS out=listfile DO selectoutput(listfile)
        memory!0 := 1    // initialise memory vector
        memory!memsize := memsize
        errorlevel := level()
        errorlabel := emergencyexit
        codes := null
writef("calling initcodes*n")
        initcodes()
        initsyms()
writef("calling parse*n")
        parse(fromfile)
emergencyexit:
        selectoutput(out)
        IF pass=second THEN
        $(  TEST  badlabs=0 & errcount=0 THEN
            $(  writes("No errors ")
                error:=FALSE
            $) ELSE
            $(  UNLESS errcount=0 THEN
                writef("%N error%S ", errcount, (errcount=1->"","s"))
                UNLESS errcount=0 | badlabs=0 THEN writes("and ")
                UNLESS badlabs=0 THEN
                writef("%N bad label%S ", badlabs, (badlabs=1->"","s"))
            $)
            writes("in this assembly*N")
        $)
    $)
    IF initialised THEN
    $(  UNLESS listfile=0 | listfile=out DO
        $(  selectoutput(listfile)
            endwrite()
        $)
        UNLESS binfile=0 DO
        $(  selectoutput(binfile)
            endwrite()
        $)
        selectoutput(out)
        UNLESS memory=0 DO freevec(memory)
    $)
    endasm()
    UNLESS args=0 THEN freevec(args)
    IF error | fatal THEN stop(bad.rc)
$)




/*

*************************************************************************
*  (C) Copyright 1980  Systems Research Group, University of Cambridge  *
*************************************************************************
*                                                                       *
*                   Z - 8 0     A S S E M B L E R                       *
*                                                                       *
*************************************************************************
**  C  Gray  Girling      COMPUTER LAB,  CAMBRIDGE           27.02.80  **
*************************************************************************

*/









//  LOG OF CHANGES:
//  ===============
//
//  Log entry is <date> <general assembler version no.> <initials> <change>
//
//  21.03.80  3.006  CGG   Changed from version 2 to version 3
//                         (i.e. relocation & externals added)








///SECTION "Z80"





/*
//
//                     Error  messages
//


MANIFEST
$(  e.ixyhl    =  e.e + 0
    e.r        =  e.e + 1
    e.dd       =  e.e + 3
    e.qq       =  e.e + 4
    e.rr       =  e.e + 5
    e.pp       =  e.e + 6
    e.cc       =  e.e + 7
    e.regexp   =  e.e + 8
    e.badreg   =  e.e + 9
    e.rhxy     =  e.e + 10
    e.badcc    =  e.e + 11
    e.badim    =  e.e + 13
    e.badds    =  e.e + 14
    e.range    =  e.e + 15
    e.bad16no  =  e.e + 16
$)
*/

LET errormess(rc) = VALOF
SWITCHON rc INTO
$(  CASE e.ixyhl:     RESULTIS "HL, IX or IY register expected"
    CASE e.r:         RESULTIS "B, C, D, E, H, L or A expected"
    CASE e.dd:        RESULTIS "BC, DE, HL or SP expected"
    CASE e.qq:        RESULTIS "BC, DE, HL or AF expected"
    CASE e.rr:        RESULTIS "BC, DE, IX or SP expected"
    CASE e.pp:        RESULTIS "BC, DE, IY or SP expected"
    CASE e.cc:        RESULTIS
                      "NZ, Z, NC, C, PO, PE, P, V, M, or NV expected"
    CASE e.regexp:    RESULTIS "register name expected"
    CASE e.badreg:    RESULTIS "not a valid register"
    CASE e.rhxy:      RESULTIS "(IX+d),(IY+d),(HL) or single reg expected"
    CASE e.badcc:     RESULTIS "NZ, Z, NC, or C expected"
    CASE e.badim:     RESULTIS "bad immediate mode specification"
    CASE e.badds:     RESULTIS "amount of store must be positive"
    CASE e.range:     RESULTIS "label is out of range"
    CASE e.bad16no:   RESULTIS "16 bit address or immediate value expected*N"
    DEFAULT:          RESULTIS "unknown return code given"
$)




/*
//
//                    Register  Descriptions
//




MANIFEST
$(  r.bad       =  0            // bad register
    b.r         =  bit15        // B, C, D, E, H, L, or A
    b.dd        =  bit14        // BC, DE, HL or SP
    b.qq        =  bit13        // BC, DE, HL or AF
    b.pp        =  bit12        // BC, DE, IX or SP
    b.rr        =  bit11        // BC, DE, IY or SP
    b.cc        =  bit10        // NZ, Z, NC, C, PO, PE, P=V, or M=NV
    b.ixyhl     =  bit9         // IX, IY, or HL
    b.rori      =  bit8         // R or I
    b.bcde      =  bit7         // BC or DE
    b.ixiy      =  bit6         // IX or IY
    r.b         =  b.r | #X0
    r.c         =  b.r | #X1 | b.cc | (#X3<<3)
    r.d         =  b.r | #X2
    r.e         =  b.r | #X3
    r.h         =  b.r | #X4
    r.l         =  b.r | #X5
    r.hl        =  b.dd | b.qq | b.ixyhl | #X6
    r.a         =  b.r | #X7
    r.bc        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X0
    r.de        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X1
    r.ix        =  b.pp | b.ixyhl | b.ixiy | #X6
    r.iy        =  b.rr | b.ixyhl | b.ixiy | #X6
    r.sp        =  b.dd | b.pp | b.rr | #X3
    r.af        =  b.qq | #X3
    r.r         =  b.rori | 1
    r.i         =  b.rori | 0
    cc.nz       =  b.cc | (#X0<<3)
    cc.z        =  b.cc | (#X1<<3)
    cc.nc       =  b.cc | (#X2<<3)
    cc.c        =  r.c
    cc.po       =  b.cc | (#X4<<3)
    cc.pe       =  b.cc | (#X5<<3)
    cc.p        =  b.cc | (#X6<<3)
    cc.m        =  b.cc | (#X7<<3)
    cc.v        =  cc.p
    cc.nv       =  cc.m
$)
*/



LET initsyms() BE
$(  reg.put("IY", r.iy)
    reg.put("DE", r.de)
    reg.put("BC", r.bc)
    reg.put("AF", r.af)
    reg.put("A",  r.a)
    reg.put("B",  r.b)
    reg.put("C",  r.c)
    reg.put("D",  r.d)
    reg.put("HL", r.hl)
    reg.put("H",  r.h)
    reg.put("E",  r.e)
    reg.put("I",  r.i)
    reg.put("IX", r.ix)
    reg.put("R",  r.r)
    reg.put("PE", cc.pe)
    reg.put("V",  cc.v)
    reg.put("P",  cc.p)
    reg.put("PO", cc.po)
    reg.put("SP", r.sp)
    reg.put("Z",  cc.z)
    reg.put("NC", cc.nc)
    reg.put("L",  r.l)
    reg.put("M",  cc.m)
    reg.put("NV", cc.nv)
    reg.put("NZ", cc.nz)
$)





//
//                    Code   for   Opcode   Formats
//


/*
GLOBAL
$(  f.rinfo   :  ag + 0   //   'r' symbol information field
    f.ccinfo  :  ag + 1   //   'cc' symbol information field
    f.ddinfo  :  ag + 2   //   'dd','rr','pp' and 'qq' information field
    f.source  :  ag + 3   //   source field
    f.ddsource:  ag + 4   //   destination field for double regs
    f.dest    :  ag + 5   //   destination field
    f.top     :  ag + 6   //   top two bits field
    f.ccsmall :  ag + 7   //   field for condition code in JR

    /*    These fields are layed out as folows:

    -------------------------------------
    | f.top |   f.source  |   f.dest    |
    -------------------------------------

    -------------------------------------
    |       |f.ddsource|                |
    -------------------------------------

    */

    getrset  :  ag + 8
    scanreg  :  ag + 9
    getind   :  ag + 10
    getrorind:  ag + 11
    getixyhl :  ag + 12
    dwproc   :  ag + 13
    fmt0     :  ag + 15
    fmt1     :  ag + 16
    fmt2     :  ag + 17
    fmt3     :  ag + 18
    fmt4     :  ag + 19
    fmt5     :  ag + 20
    fmt6     :  ag + 21
    fmt7     :  ag + 22
    fmt8     :  ag + 23
    fmt9     :  ag + 24

$)
*/

/*
MANIFEST
$(  fn.bit      = 0
    fn.normal   = 1
$)
*/




LET getrset(symtype) = VALOF
$(  LET ans=r.bad
    IF item.type=i.iden DO
       $(  LET r=getreg(item.info)
           UNLESS 0=(symtype&r) DO ans:=r
       $)
    UNLESS ans=r.bad DO getitem()
    RESULTIS ans
$)




LET putaddress(n) BE
$(  LET sign=n&~#XFFFF
    putword(n&255)
    putword( (n&(255<<8)) >> 8)
    UNLESS sign=(-1&~#XFFFF) | sign=0 THEN error(e.badbyte)
$)



AND scanreg(reg) = VALOF
$(  UNLESS item.type=i.iden RESULTIS FALSE
    UNLESS reg=getreg(item.info) RESULTIS FALSE
    getitem ()
    RESULTIS TRUE
$)



AND getind(shel, fntype) = VALOF
$(  LET r=getrset(b.ixyhl)
    TEST r=r.hl
    THEN $(  IF fntype=fn.bit DO putword(#XCB)
             putword(shel)
         $)
    ELSE TEST r=r.ix | r=r.iy
         THEN $(  LET i=0
                  putword(r=r.ix->#XDD, #XFD)
                  IF fntype=fn.bit DO putword(#XCB)
                  IF item.type=i.plus | item.type=i.minus THEN
                  $(  LET neg=(item.type=i.minus)
                      getitem()
                      i:= (neg -> -expression(),expression())
                  $)
                  TEST fntype=fn.bit
                  THEN $(  putword(i)
                           putword(shel)
                       $)
                  ELSE $(  putword(shel)
                           putword(i)
                       $)
              $)
         ELSE RESULTIS FALSE
    checkandskip(i.rbkt,')')
    RESULTIS TRUE
$)





AND getrorind( shel, rfield, fntype ) = VALOF
$(  /* 'fntype' is fn.normal, fn.bit
       'shel' is the template binary to be output
       'rfield' is the field in 'shel' in which the register spec is placed
                this field of 'shel' should be initialy 6 (#B110)
    */
    IF item.type=i.iden
    DO $(  LET r=getrset(b.r)
           IF r=r.bad RESULTIS FALSE
           IF fntype=fn.bit DO putword(#XCB)
           putword(shel)
           putwordf(getf(r,f.rinfo), rfield)
           RESULTIS TRUE
       $)
    UNLESS scan(i.lbkt) DO RESULTIS FALSE
    IF getind(shel, fntype)
    DO RESULTIS TRUE
    error(e.ixyhl)
$)



LET getixyhl(shel) = VALOF
$(  UNLESS (item.type=i.iden) RESULTIS FALSE
    $(  LET r=getreg(item.info)
        UNLESS r=r.hl | r=r.ix | r=r.iy RESULTIS FALSE
        TEST r=r.ix
        THEN putword(#XDD)
        ELSE IF r=r.iy DO putword(#XFD)
        putword(shel)
        getitem()
        RESULTIS TRUE
    $)
$)




AND dwproc(lab) BE
$(  LET spec=VEC spec.size
    get.and.declare(lab)
    putlabelspec(label.expression(spec)) REPEATWHILE scan(i.comma)
$)













///.




///SECTION "Z80a"




/*
//
//              In - line   Header   For  Z80  Assembler
//





MANIFEST
$(  e.ixyhl     =  e.e + 0
    e.r         =  e.e + 1
    e.dd        =  e.e + 3
    e.qq        =  e.e + 4
    e.rr        =  e.e + 5
    e.pp        =  e.e + 6
    e.cc        =  e.e + 7
    e.regexp    =  e.e + 8
    e.badreg    =  e.e + 9
    e.rhxy      =  e.e + 10
    e.badcc     =  e.e + 11
    e.badim     =  e.e + 13
    e.badds     =  e.e + 14
    e.range     =  e.e + 15
    e.bad16no   =  e.e + 16
    r.bad       =  0            // bad register
    b.r         =  bit15        // B, C, D, E, H, L, or A
    b.dd        =  bit14        // BC, DE, HL or SP
    b.qq        =  bit13        // BC, DE, HL or AF
    b.pp        =  bit12        // BC, DE, IX or SP
    b.rr        =  bit11        // BC, DE, IY or SP
    b.cc        =  bit10        // NZ, Z, NC, C, PO, PE, P=V, or M=NV
    b.ixyhl     =  bit9         // IX, IY, or HL
    b.rori      =  bit8         // R or I
    b.bcde      =  bit7         // BC or DE
    b.ixiy      =  bit6         // IX or IY
    r.b         =  b.r | #X0
    r.c         =  b.r | #X1 | b.cc | (#X3<<3)
    r.d         =  b.r | #X2
    r.e         =  b.r | #X3
    r.h         =  b.r | #X4
    r.l         =  b.r | #X5
    r.hl        =  b.dd | b.qq | b.ixyhl | #X6
    r.a         =  b.r | #X7
    r.bc        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X0
    r.de        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X1
    r.ix        =  b.pp | b.ixyhl | b.ixiy | #X6
    r.iy        =  b.rr | b.ixyhl | b.ixiy | #X6
    r.sp        =  b.dd | b.pp | b.rr | #X3
    r.af        =  b.qq | #X3
    r.r         =  b.rori | 1
    r.i         =  b.rori | 0
    cc.nz       =  b.cc | (#X0<<3)
    cc.z        =  b.cc | (#X1<<3)
    cc.nc       =  b.cc | (#X2<<3)
    cc.c        =  r.c
    cc.po       =  b.cc | (#X4<<3)
    cc.pe       =  b.cc | (#X5<<3)
    cc.p        =  b.cc | (#X6<<3)
    cc.m        =  b.cc | (#X7<<3)
    cc.v        =  cc.p
    cc.nv       =  cc.m
    fn.bit      = 0
    fn.normal   = 1
$)
*/



/*
GLOBAL
$(  f.rinfo   :  ag + 0   //   'r' symbol information field
    f.ccinfo  :  ag + 1   //   'cc' symbol information field
    f.ddinfo  :  ag + 2   //   'dd','rr','pp' and 'qq' information field
    f.source  :  ag + 3   //   source field
    f.ddsource:  ag + 4   //   destination field for double regs
    f.dest    :  ag + 5   //   destination field
    f.top     :  ag + 6   //   top two bits field
    f.ccsmall :  ag + 7   //   field for condition code in JR
    getrset   :  ag + 8
    scanreg   :  ag + 9
    getind    :  ag + 10
    getrorind :  ag + 11
    getixyhl  :  ag + 12
    dwproc    :  ag + 13
    fmt0      :  ag + 15
    fmt1      :  ag + 16
    fmt2      :  ag + 17
    fmt3      :  ag + 18
    fmt4      :  ag + 19
    fmt5      :  ag + 20
    fmt6      :  ag + 21
    fmt7      :  ag + 22
    fmt8      :  ag + 23
    fmt9      :  ag + 24
$)
*/


//
//                           Code  Formats
//








LET fmt0(lab) BE get.and.declare(lab)




AND fmt1(lab) BE
$(  LET r=?             // a register
    LET d=0             // a displacement
    LET i=?             // the result of an expression evaluation
    LET spec=VEC spec.size
    get.and.declare(lab)
    r:=getrset(b.r | b.rori | b.dd | b.ixyhl)
    TEST r\=r.bad
    THEN $(  checkandskip(i.comma,',')
             SWITCHON r INTO
             $(
               CASE r.a:
                 TEST scan(i.lbkt)
                 THEN UNLESS getind(#X7E /* 01 111 110 */, fn.normal)
                         $(  r:=getrset(b.bcde)
                             TEST r=r.bad
                             THEN $(  putword(#X3A /* 00 111 010 */)
                                      putlabelspec(label.expression(spec))
                                  $)
                             ELSE putword(r=r.bc->#X0A, #X1A)
                             checkandskip(i.rbkt, ')')
                         $)
                 ELSE $(  r:=getrset(b.rori | b.r)
                          TEST r=r.bad
                          THEN $(  putword(#X3E /* 00 111 110 */)
                                   putword(expression())
                               $)
                          ELSE TEST 0=(r&b.r)
                               THEN $(  putword(#XED /* 11 110 101 */)
                                        putword(r=r.i->#X57, #X5F)
                                    $)
                               ELSE $(  putword(#X78 /* 01 aaa rrr */)
                                        putwordf(getf(r,f.rinfo), f.dest)
                                    $)
                      $)
                 ENDCASE
               DEFAULT:   /* b.r set */
                 $(  LET shel=putf( getf(r, f.rinfo), f.source, #X46 )
                     /* i.e.:  01 rrr xxx */
                     UNLESS getrorind(shel, f.dest, fn.normal) DO
                        $(  putword(#X06 /* 00 rrr 110 */)
                            putwordf(getf(r, f.rinfo), f.source)
                            putword(expression())
                        $)
                 $)
                 ENDCASE
               CASE r.i:  CASE r.r:
                 $(  UNLESS scanreg(r.a) DO error(e.badreg)
                     putaddress(#X47ED)
                     putwordf( getf(r, f.rinfo), f.source )
                 $)
                 ENDCASE
               CASE r.ix: CASE r.iy: CASE r.hl:
               CASE r.bc: CASE r.de: CASE r.sp:      // i.e. 'dd'
                 $(  LET dd=(r=r.sp -> getrset(b.ixyhl), r.bad)
                     TEST dd=r.ix | r=r.ix
                     THEN putword(#XDD)
                     ELSE IF dd=r.iy | r=r.iy DO putword(#XFD)
                     TEST (r=r.sp) & (dd\=r.bad)
                     THEN putword(#XF9)
                     ELSE TEST dd\=r.bad THEN error(e.bad16no)
                     ELSE $(  label.expression(spec)
                              TEST bracketed
                              THEN TEST 0=(r&b.ixyhl)
                                   THEN putaddress(#X4BED)
                                   ELSE putword(#X2A)
                              ELSE putword(#X21 /* 00 100 001 */)
                              UNLESS 0\=(r&b.ixyhl)
                                 putwordf( getf(r, f.ddinfo), f.ddsource )
                              putlabelspec(spec)
                          $)
                 $)
                 ENDCASE
             $)
         $)
    ELSE $(  checkandskip(i.lbkt, '(')
             r:=getrset(b.ixyhl | b.bcde)
             TEST r=r.bad
             THEN $(  label.expression(spec)
                      checkandskip(i.rbkt, ')')
                      checkandskip(i.comma, ',')
                      TEST scanreg(r.a)
                      THEN $(  putword(#X32)
                               putlabelspec(spec)
                           $)
                      ELSE $(  r:=getrset(b.ixyhl | b.dd)
                               IF r=r.bad DO error(e.badreg)
                               TEST r=r.ix
                               THEN putword(#XDD)
                               ELSE IF r=r.iy DO putword(#XFD)
                               TEST 0\=(r&b.ixyhl)
                               THEN putword(#X22)
                               ELSE $(  putword(#XED)
                                        putword(#X43)
                                        putwordf(getf(r,f.ddinfo), f.ddsource)
                                    $)
                               putlabelspec(spec)
                           $)
                  $)
             ELSE $(  TEST 0\=(r&b.bcde)
                      THEN   /* BC or DE */
                           $(  putword(r=r.bc->#X02, #X12)
                               checkandskip(i.rbkt, ')')
                               checkandskip(i.comma, ',')
                               UNLESS scanreg(r.a) DO error(e.badreg)
                           $)
                      ELSE /* IX, IY or HL */
                           $(  TEST r=r.ix
                               THEN putword(#XDD)
                               ELSE IF r=r.iy DO putword(#XFD)
                               UNLESS r=r.hl
                               TEST scan(i.plus) THEN d:=expression() ELSE
                               IF scan(i.minus) THEN d:=-expression()
                               checkandskip(i.rbkt, ')')
                               checkandskip(i.comma, ',')
                               $(  LET s=getrset(b.r)
                                   TEST s=r.bad
                                   THEN $(  putword(#X36 /* 00 110 110 */)
                                            UNLESS r=r.hl DO putword(d)
                                            putword(expression())
                                        $)
                                   ELSE $(  putword(#X70 /* 01 110 000 */)
                                            putwordf(getf(s,f.rinfo), f.dest)
                                            UNLESS r=r.hl DO putword(d)
                                        $)
                               $)
                           $)
                  $)
         $)
$)





AND fmt2(lab, info) BE
$(  MANIFEST
    $(  sheln = #XC6    /* 11xxx110   -  immediate */
        sheli = #X86    /* 10xxx110   -  indirect or register */
    $)
    LET addopreg=r.a
    LET add   = (0=info)
    LET addop = (info<4 & info\=2)
    LET regset=b.dd
    get.and.declare(lab)
    IF addop DO
       $(  UNLESS scanreg(r.a) DO
              $(  addopreg:=getrset(b.ixyhl)
                  IF addopreg=r.bad DO error(e.badreg)
              $)
           checkandskip(i.comma, ',')
       $)
    TEST addopreg=r.a
    THEN UNLESS getrorind(putf(info, f.source, sheli), f.dest, fn.normal)
         /* not register or indirect */
         DO TEST is.expression()
            THEN $(  putword(putf(info, f.source, sheln))
                     putword(expression())
                 $)
            ELSE error(e.r)
         /* not immediate either */
    ELSE $(  TEST add
             THEN $(  TEST addopreg=r.ix
                      THEN $(  putword(#XDD)
                               regset:=b.pp
                           $)
                      ELSE IF addopreg=r.iy DO
                              $(  putword(#XFD)
                                  regset:=b.rr
                              $)
                      putword(#X09 /* 00 xx1 001 */)
                  $)
             ELSE $(  UNLESS addopreg=r.hl DO error(e.badreg)
                      IF addop DO putword(#XED)
                      putword(putf(\(info>>1), f.source, #X42 /* 01 xxi 010 */))
                  $)
             addopreg:=getrset(regset)
             IF addopreg=r.bad THEN
             error(regset=b.pp->e.pp, regset=b.rr->e.rr, e.dd)
             putwordf(getf(addopreg,f.ddinfo), f.ddsource)
         $)
$)







AND fmt3(lab, info) BE
$(  /* deals with shifts and rotates */
    LET shel=putf(info, f.source, #X06 /* 00 xxx 110 */)
    get.and.declare(lab)
    UNLESS getrorind(shel, f.dest, fn.bit /* shift formats */)
    DO error(e.rhxy)
$)





AND fmt4(lab, info) BE
$(  /* deals with bit operations */
    LET shel=putf(info, f.top, #X46 /* 01 bbb xxx */)
    get.and.declare(lab)
    shel:=putf(expression(), f.source, shel)
    checkandskip(i.comma, ',')
    UNLESS getrorind(shel, f.dest, fn.bit) DO error(e.rhxy)
$)



///.





///SECTION "Z80b"



/*
//
//              In - line   Header   For  Z80  Assembler
//





MANIFEST
$(  e.ixyhl     =  e.e + 0
    e.r         =  e.e + 1
    e.dd        =  e.e + 3
    e.qq        =  e.e + 4
    e.rr        =  e.e + 5
    e.pp        =  e.e + 6
    e.cc        =  e.e + 7
    e.regexp    =  e.e + 8
    e.badreg    =  e.e + 9
    e.rhxy      =  e.e + 10
    e.badcc     =  e.e + 11
    e.badim     =  e.e + 13
    e.badds     =  e.e + 14
    e.range     =  e.e + 15
    e.bad16no   =  e.e + 16
    r.bad       =  0            // bad register
    b.r         =  bit15        // B, C, D, E, H, L, or A
    b.dd        =  bit14        // BC, DE, HL or SP
    b.qq        =  bit13        // BC, DE, HL or AF
    b.pp        =  bit12        // BC, DE, IX or SP
    b.rr        =  bit11        // BC, DE, IY or SP
    b.cc        =  bit10        // NZ, Z, NC, C, PO, PE, P=V, or M=NV
    b.ixyhl     =  bit9         // IX, IY, or HL
    b.rori      =  bit8         // R or I
    b.bcde      =  bit7         // BC or DE
    b.ixiy      =  bit6         // IX or IY
    r.b         =  b.r | #X0
    r.c         =  b.r | #X1 | b.cc | (#X3<<3)
    r.d         =  b.r | #X2
    r.e         =  b.r | #X3
    r.h         =  b.r | #X4
    r.l         =  b.r | #X5
    r.hl        =  b.dd | b.qq | b.ixyhl | #X6
    r.a         =  b.r | #X7
    r.bc        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X0
    r.de        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X1
    r.ix        =  b.pp | b.ixyhl | b.ixiy | #X6
    r.iy        =  b.rr | b.ixyhl | b.ixiy | #X6
    r.sp        =  b.dd | b.pp | b.rr | #X3
    r.af        =  b.qq | #X3
    r.r         =  b.rori | 1
    r.i         =  b.rori | 0
    cc.nz       =  b.cc | (#X0<<3)
    cc.z        =  b.cc | (#X1<<3)
    cc.nc       =  b.cc | (#X2<<3)
    cc.c        =  r.c
    cc.po       =  b.cc | (#X4<<3)
    cc.pe       =  b.cc | (#X5<<3)
    cc.p        =  b.cc | (#X6<<3)
    cc.m        =  b.cc | (#X7<<3)
    cc.v        =  cc.p
    cc.nv       =  cc.m
    fn.bit      = 0
    fn.normal   = 1
$)
*/



/*
GLOBAL
$(  f.rinfo   :  ag + 0   //   'r' symbol information field
    f.ccinfo  :  ag + 1   //   'cc' symbol information field
    f.ddinfo  :  ag + 2   //   'dd','rr','pp' and 'qq' information field
    f.source  :  ag + 3   //   source field
    f.ddsource:  ag + 4   //   destination field for double regs
    f.dest    :  ag + 5   //   destination field
    f.top     :  ag + 6   //   top two bits field
    f.ccsmall :  ag + 7   //   field for condition code in JR
    getrset   :  ag + 8
    scanreg   :  ag + 9
    getind    :  ag + 10
    getrorind :  ag + 11
    getixyhl  :  ag + 12
    dwproc    :  ag + 13
    fmt0      :  ag + 15
    fmt1      :  ag + 16
    fmt2      :  ag + 17
    fmt3      :  ag + 18
    fmt4      :  ag + 19
    fmt5      :  ag + 20
    fmt6      :  ag + 21
    fmt7      :  ag + 22
    fmt8      :  ag + 23
    fmt9      :  ag + 24
    fmt10     :  ag + 25
    fmt11     :  ag + 26
    fmt12     :  ag + 27
    fmt13     :  ag + 28
    fmt14     :  ag + 29
    fmt15     :  ag + 30
    fmt16     :  ag + 31
    nolistproc:  ag + 32
$)
*/




//
//               More  Code  Formats
//



LET fmt5(lab) BE
$(  LET cc=?
    /* This is JP */
    get.and.declare(lab)
    TEST scan(i.lbkt)
    THEN $(  UNLESS getixyhl(#XE9 /* 11 101 011 */) DO error(e.ixyhl)
             checkandskip(i.rbkt, ')')
         $)
    ELSE $(  LET spec=VEC spec.size
             cc:=getrset(b.cc)
             TEST cc=r.bad
             THEN $(  putword(#XC3 /* 11 000 011 */)
                      putlabelspec(label.expression(spec))
                  $)
             ELSE $(  putword(#XC2 /* 11 ccc 010 */)
                      putwordf(getf(cc,f.ccinfo), f.source)
                      checkandskip(i.comma, ',')
                      putlabelspec(label.expression(spec))
                  $)
         $)
$)




AND fmt6(lab) BE
$(  LET cc=?
    LET spec=VEC spec.size
    /* this is CALL */
    get.and.declare(lab)
    cc:=getrset(b.cc)
    TEST cc=r.bad
    THEN putword(#XCD /* 11 001 101 */)
    ELSE $(  putword(#XCC /* 11 ccc 100 */)
             putwordf(getf(cc,f.ccinfo), f.source)
             checkandskip(i.comma, ',')
         $)
    putlabelspec(label.expression(spec))
$)



AND fmt7(lab) BE
$(  LET cc=?
    /* this is RET */
    get.and.declare(lab)
    cc:=getrset(b.cc)
    TEST cc=r.bad
    THEN putword(#XC9 /* 11 001 001 */)
    ELSE $(  putword(#XC8 /* 11 ccc 000 */)
             putwordf(getf(cc,f.ccinfo), f.source)
         $)
$)




AND jmp(lab, ccsym) BE
$(  LET cc=getf(ccsym, f.ccinfo)
    get.and.declare(lab)
    putword(#X20 /* 00 1cc 000 */)
    UNLESS cc<4 DO error(e.badcc)
    putwordf(cc, f.ccsmall)
    putword(checkrange(expression()-2))
$)




AND fmt8(lab) BE
$(  LET cc=?
    /* this is JR */
    get.and.declare(lab)
    cc:=getrset(b.cc)
    TEST cc=r.bad
    THEN $(  putword(#X18 /* 00 011 000 */)
             putword(checkrange(expression()-2))
         $)
    ELSE $(  UNLESS item.type=i.comma DO error(e.expected, ',')
             jmp(null, cc)
         $)
$)



AND checkrange(range) = VALOF
$(
    UNLESS -128<=range<127 DO warn(e.range)
    RESULTIS range
$)






LET fmt9(lab) BE
$(  get.and.declare(lab)
    /* this is DJNZ */
    putword(expression()-2)
$)




AND fmt10(lab) BE
$(  get.and.declare(lab)
    /* this is RST */
    putwordf(expression()>>3, f.source)
$)




AND fmt11(lab,info) BE
$(  LET shel=putf(info, f.dest, #X34 /* 00 rrr 110 */)
    get.and.declare(lab)
    /* this is INC and DEC */
    UNLESS getrorind(shel, f.source, fn.normal) DO
       $(  LET r=getrset(b.dd | b.ixiy)
           IF r=r.bad DO error(e.badreg)
           TEST r=r.ix
           THEN putword(#XDD)
           ELSE
                IF   r=r.iy DO putword(#XFD)
           putword(#X03 /* 00 rri 011 */)
           putwordf(info, f.source)
           UNLESS 0=(b.dd&r) DO putwordf(getf(r,f.ddinfo), f.ddsource)
       $)
$)



AND fmt12(lab) BE
$(  get.and.declare(lab)
    /* This is EX */
    TEST scan(i.lbkt)
    THEN $(  UNLESS scanreg(r.sp) DO error(e.badreg)
             checkandskip(i.rbkt,')')
             checkandskip(i.comma,',')
             UNLESS getixyhl(#XE3 /* 11 100 011 */) DO error(e.ixyhl)
         $)
    ELSE $(  TEST scanreg(r.de)
             THEN $(  putword(#XEB)
                      checkandskip(i.comma, ',')
                      UNLESS scanreg(r.hl) DO error(e.badreg)
                  $)
             ELSE TEST scanreg(r.af)
                  THEN $(  putword(#X08)
                           checkandskip(i.comma, ',')
                           UNLESS item.type=i.iden & r.af=getreg(item.info) DO
                              error(e.badreg)
                           /* Please look the other way ! */
                           TEST ch='*''
                           THEN rch() REPEATUNTIL ch\='*S'
                           ELSE error(e.badreg)
                           getitem()
                       $)
                  ELSE error(e.badreg)
         $)
$)







AND fmt13(lab, info) BE
$(  LET dd=?
    LET shel=putf(info, f.dest, #XE0 /* 11 qq0 iii */)
    get.and.declare(lab)
    /* this is PUSH and POP */
    dd:=getrset(b.qq | b.ixiy)
    IF dd=r.bad DO error(e.badreg)
    TEST dd=r.ix
    THEN putword(#XDD)
    ELSE IF dd=r.iy DO putword(#XFD)
    putword(shel)
    UNLESS 0=(dd&b.qq) DO putwordf(getf(dd,f.ddinfo), f.ddsource)
$)




///.





///SECTION "Z80c"





/*
//
//              In - line   Header   For  Z80  Assembler
//





MANIFEST
$(  e.ixyhl     =  e.e + 0
    e.r         =  e.e + 1
    e.dd        =  e.e + 3
    e.qq        =  e.e + 4
    e.rr        =  e.e + 5
    e.pp        =  e.e + 6
    e.cc        =  e.e + 7
    e.regexp    =  e.e + 8
    e.badreg    =  e.e + 9
    e.rhxy      =  e.e + 10
    e.badcc     =  e.e + 11
    e.badim     =  e.e + 13
    e.badds     =  e.e + 14
    e.range     =  e.e + 15
    e.bad16no   =  e.e + 16
    r.bad       =  0            // bad register
    b.r         =  bit15        // B, C, D, E, H, L, or A
    b.dd        =  bit14        // BC, DE, HL or SP
    b.qq        =  bit13        // BC, DE, HL or AF
    b.pp        =  bit12        // BC, DE, IX or SP
    b.rr        =  bit11        // BC, DE, IY or SP
    b.cc        =  bit10        // NZ, Z, NC, C, PO, PE, P=V, or M=NV
    b.ixyhl     =  bit9         // IX, IY, or HL
    b.rori      =  bit8         // R or I
    b.bcde      =  bit7         // BC or DE
    b.ixiy      =  bit6         // IX or IY
    r.b         =  b.r | #X0
    r.c         =  b.r | #X1 | b.cc | (#X3<<3)
    r.d         =  b.r | #X2
    r.e         =  b.r | #X3
    r.h         =  b.r | #X4
    r.l         =  b.r | #X5
    r.hl        =  b.dd | b.qq | b.ixyhl | #X6
    r.a         =  b.r | #X7
    r.bc        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X0
    r.de        =  b.dd | b.qq | b.pp | b.rr | b.bcde | #X1
    r.ix        =  b.pp | b.ixyhl | b.ixiy | #X6
    r.iy        =  b.rr | b.ixyhl | b.ixiy | #X6
    r.sp        =  b.dd | b.pp | b.rr | #X3
    r.af        =  b.qq | #X3
    r.r         =  b.rori | 1
    r.i         =  b.rori | 0
    cc.nz       =  b.cc | (#X0<<3)
    cc.z        =  b.cc | (#X1<<3)
    cc.nc       =  b.cc | (#X2<<3)
    cc.c        =  r.c
    cc.po       =  b.cc | (#X4<<3)
    cc.pe       =  b.cc | (#X5<<3)
    cc.p        =  b.cc | (#X6<<3)
    cc.m        =  b.cc | (#X7<<3)
    cc.v        =  cc.p
    cc.nv       =  cc.m
    fn.bit      = 0
    fn.normal   = 1
$)
*/



/*
GLOBAL
$(  f.rinfo   :  ag + 0   //   'r' symbol information field
    f.ccinfo  :  ag + 1   //   'cc' symbol information field
    f.ddinfo  :  ag + 2   //   'dd','rr','pp' and 'qq' information field
    f.source  :  ag + 3   //   source field
    f.ddsource:  ag + 4   //   destination field for double regs
    f.dest    :  ag + 5   //   destination field
    f.top     :  ag + 6   //   top two bits field
    f.ccsmall :  ag + 7   //   field for condition code in JR
    getrset   :  ag + 8
    scanreg   :  ag + 9
    getind    :  ag + 10
    getrorind :  ag + 11
    getixyhl  :  ag + 12
    dwproc    :  ag + 13
    fmt0      :  ag + 15
    fmt1      :  ag + 16
    fmt2      :  ag + 17
    fmt3      :  ag + 18
    fmt4      :  ag + 19
    fmt5      :  ag + 20
    fmt6      :  ag + 21
    fmt7      :  ag + 22
    fmt8      :  ag + 23
    fmt9      :  ag + 24
    fmt10     :  ag + 25
    fmt11     :  ag + 26
    fmt12     :  ag + 27
    fmt13     :  ag + 28
    fmt14     :  ag + 29
    fmt15     :  ag + 30
    fmt16     :  ag + 31
    nolistproc:  ag + 32
$)
*/





//
//               Yet  More  Code  Formats
//





LET fmt14(lab) BE
$(  /* this is IN */
    LET r = ?
    LET reg = ?
    get.and.declare(lab)
    TEST ~scan(i.lbkt) THEN
    $(  reg:=getrset(b.r)
        IF reg=r.bad DO error(e.r)
        checkandskip(i.comma, ',')
        checkandskip(i.lbkt, '(')
        r := getf(reg,f.rinfo)
    $) ELSE r := #X6                    // IN (C)
    TEST scanreg(r.c)
    THEN $(  putword(#XED)
             putword(#X40 /* 01 rrr 000 */)
             putwordf(r, f.source)
         $)
    ELSE $(  UNLESS reg=r.a DO error(e.expected, 'C')
             putword(#XDB)
             putword(expression())
         $)
    checkandskip(i.rbkt, ')')
$)





AND fmt15(lab) BE
$(  /*  This is OUT  */
    LET r=?
    get.and.declare(lab)
    checkandskip(i.lbkt, '(')
    TEST scanreg(r.c)
    THEN $(  checkandskip(i.rbkt, ')')
             checkandskip(i.comma, ',')
             r:=getrset(b.r)
             IF r=r.bad DO error(e.r)
             putword(#XED)
             putword(#X41 /* 01 rrr 001 */)
             putwordf(getf(r, f.rinfo), f.source)
         $)
    ELSE $(  putword(#XD3)
             putword(expression())
             checkandskip(i.rbkt, ')')
             checkandskip(i.comma, ',')
             UNLESS scanreg(r.a) DO error(e.badreg)
         $)
$)





AND fmt16(lab) BE
$(  get.and.declare(lab)
    /* This is IM */
    SWITCHON expression() INTO
    $(  CASE 0:  putword(#X46); ENDCASE
        CASE 1:  putword(#X56); ENDCASE
        CASE 2:  putword(#X5E); ENDCASE
        DEFAULT: error(e.badim)
    $)
$)




AND nolistproc(lab) BE
$(  get.and.declare(lab)
    list:=list-1  // listing variable
$)

// Start of <68000INIT    07.01.82        Version 3.056
/*
LET initcodes() BE
$(  // table for the Z80 assembler:
    // this table must be regenerated if the globals change
    LET tab = TABLE
    #X000002D4, #X000000DD, #X00000000, #X0000000A, #X0000007A, #X000002D4,
    #X00000000, #X000002D5, #X000000DD, #X00000000, #X00000011, #X0000003B,
    #X000002D5, #X00000007, #X000002D7, #X00000144, #X00000000, #X00000034,
    #X00000018, #X000002D7, #X0000000E, #X00000331, #X00000144, #X00000000,
    #X00000026, #X0000001F, #X00000331, #X00000015, #X00000337, #X000000DD,
    #X00000000, #X0000002D, #X0000005E, #X00000337, #X0000001C, #X00000333,
    #X000000EC, #X00000000, #XFFFFFFFF, #X000000EB, #X00000333, #X00000023,
    #X00000339, #X000000EC, #X00000000, #XFFFFFFFF, #XFFFFFFFF, #X00000339,
    #X0000002A, #X000002D8, #X000000DF, #X00000000, #X00000042, #X0000009D,
    #X000002D8, #X00000031, #X0000033D, #X000000E1, #X00000000, #XFFFFFFFF,
    #X000002A3, #X0000033D, #X00000038, #X000002D9, #X000000E2, #X00000000,
    #X00000049, #X00000050, #X000002D9, #X0000003F, #X000002DA, #X000000E3,
    #X00000000, #X00000207, #X00000065, #X000002DA, #X00000046, #X000002FE,
    #X000000E4, #X00000000, #X00000057, #X00000088, #X000002FE, #X0000004D,
    #X00000300, #X000000E5, #X00000000, #X000002B2, #X00000073, #X00000300,
    #X00000054, #X0000033B, #X000000E0, #X00000000, #XFFFFFFFF, #XFFFFFFFF,
    #X0000033B, #X0000005B, #X000002DD, #X000000E6, #X00000000, #X0000006C,
    #X00000081, #X000002DD, #X00000062, #X000002DF, #X000000DE, #X00000000,
    #XFFFFFFFF, #XFFFFFFFF, #X000002DF, #X00000069, #X00000304, #X000000EB,
    #X00000000, #X000000A4, #X00000096, #X00000304, #X00000070, #X0000033F,
    #X000000E9, #X00000000, #X000000BB, #XFFFFFFFF, #X0000033F, #X00000077,
    #X000002E1, #X000000EA, #X00000000, #X000000AB, #XFFFFFFFF, #X000002E1,
    #X0000007E, #X00000316, #X000000E7, #X00000000, #X0000008F, #X000000B3,
    #X00000316, #X00000085, #X00000317, #X000000EF, #X00000000, #XFFFFFFFF,
    #X000000E3, #X00000317, #X0000008C, #X00000311, #X00000157, #X00000000,
    #X000000DB, #X00000164, #X00000311, #X00000093, #X0000032F, #X000000E8,
    #X00000000, #XFFFFFFFF, #X000000F3, #X0000032F, #X0000009A, #X00000305,
    #X000000EE, #X00000000, #X000002BA, #X00000191, #X00000305, #X000000A1,
    #X000002E2, #X000000ED, #X00000000, #X000001EF, #X000000D3, #X000002E2,
    #X000000A8, #X0000032D, #X00000146, #X00000001, #X000000D9, #XFFFFFFFF,
    #X000002AA, #X0000032D, #X000000AF, #X00000340, #X00000146, #X00000001,
    #X00000027, #XFFFFFFFF, #X000000C3, #X00000340, #X000000B7, #X00000341,
    #X00000146, #X00000001, #X0000002F, #XFFFFFFFF, #X000000CB, #X00000341,
    #X000000BF, #X00000342, #X00000146, #X00000001, #X0000003F, #X00000140,
    #X000001DF, #X00000342, #X000000C7, #X000002E7, #X00000146, #X00000001,
    #X00000037, #X0000023F, #X000000FB, #X000002E7, #X000000CF, #X00000313,
    #X00000146, #X00000001, #X00000000, #X0000020F, #XFFFFFFFF, #X00000313,
    #X000000D7, #X00000319, #X00000146, #X00000001, #X00000076, #X0000011C,
    #XFFFFFFFF, #X00000319, #X000000DF, #X00000334, #X00000146, #X00000001,
    #X000000F3, #X0000028B, #XFFFFFFFF, #X00000334, #X000000E7, #X00000330,
    #X00000146, #X00000001, #X000000FB, #XFFFFFFFF, #XFFFFFFFF, #X00000330,
    #X000000EF, #X000002EA, #X00000146, #X00000001, #X00000017, #X00000103,
    #X000001B5, #X000002EA, #X000000F7, #X000002EB, #X00000146, #X00000001,
    #X00000007, #X0000010B, #X0000021F, #X000002EB, #X000000FF, #X000002ED,
    #X00000146, #X00000001, #X0000001F, #X00000113, #X000001C7, #X000002ED,
    #X00000107, #X000002EE, #X00000146, #X00000001, #X0000000F, #X000001D0,
    #X00000227, #X000002EE, #X0000010F, #X0000031B, #X00000146, #X00000002,
    #X000000ED, #X000000A0, #X00000125, #X0000012E, #X0000031B, #X00000117,
    #X0000031C, #X00000146, #X00000002, #X000000ED, #X000000B0, #XFFFFFFFF,
    #XFFFFFFFF, #X0000031C, #X00000120, #X0000031E, #X00000146, #X00000002,
    #X000000ED, #X000000A8, #X00000137, #X0000016D, #X0000031E, #X00000129,
    #X0000031F, #X00000146, #X00000002, #X000000ED, #X000000B8, #XFFFFFFFF,
    #XFFFFFFFF, #X0000031F, #X00000132, #X00000343, #X00000146, #X00000002,
    #X000000ED, #X000000A1, #X00000149, #X00000152, #X00000343, #X0000013B,
    #X00000344, #X00000146, #X00000002, #X000000ED, #X000000B1, #XFFFFFFFF,
    #XFFFFFFFF, #X00000344, #X00000144, #X00000346, #X00000146, #X00000002,
    #X000000ED, #X000000A9, #X0000015B, #X00000217, #X00000346, #X0000014D,
    #X00000347, #X00000146, #X00000002, #X000000ED, #X000000B9, #XFFFFFFFF,
    #XFFFFFFFF, #X00000347, #X00000156, #X00000315, #X00000146, #X00000002,
    #X000000ED, #X00000044, #XFFFFFFFF, #XFFFFFFFF, #X00000315, #X0000015F,
    #X00000321, #X00000146, #X00000002, #X000000ED, #X000000A2, #X00000176,
    #X0000017F, #X00000321, #X00000168, #X00000322, #X00000146, #X00000002,
    #X000000ED, #X000000B2, #X000001D7, #XFFFFFFFF, #X00000322, #X00000171,
    #X00000327, #X00000146, #X00000002, #X000000ED, #X000000AA, #X00000188,
    #X0000029B, #X00000327, #X0000017A, #X00000328, #X00000146, #X00000002,
    #X000000ED, #X000000BA, #XFFFFFFFF, #XFFFFFFFF, #X00000328, #X00000183,
    #X00000308, #X00000146, #X00000002, #X000000ED, #X000000A3, #XFFFFFFFF,
    #X0000019A, #X00000308, #X0000018C, #X0000030A, #X00000146, #X00000002,
    #X000000ED, #X000000B3, #X000001A3, #X000001AC, #X0000030A, #X00000195,
    #X0000030C, #X00000146, #X00000002, #X000000ED, #X000000AB, #XFFFFFFFF,
    #X000002C8, #X0000030C, #X0000019E, #X0000030F, #X00000146, #X00000002,
    #X000000ED, #X000000BB, #XFFFFFFFF, #XFFFFFFFF, #X0000030F, #X000001A7,
    #X000002F7, #X00000146, #X00000002, #X000000ED, #X0000004D, #X000001BE,
    #X00000267, #X000002F7, #X000001B0, #X000002F9, #X00000146, #X00000002,
    #X000000ED, #X00000045, #X0000022F, #XFFFFFFFF, #X000002F9, #X000001B9,
    #X000002F4, #X00000146, #X00000002, #X000000ED, #X0000006F, #X00000237,
    #XFFFFFFFF, #X000002F4, #X000001C2, #X000002F0, #X00000146, #X00000002,
    #X000000ED, #X00000067, #X000001F7, #XFFFFFFFF, #X000002F0, #X000001CB,
    #X00000324, #X00000147, #X00000000, #XFFFFFFFF, #X0000026E, #X00000324,
    #X000001D4, #X0000034A, #X00000148, #XFFFFFFFF, #X00000000, #X000001FF,
    #X000001E7, #X0000034A, #X000001DB, #X0000034F, #X00000148, #XFFFFFFFF,
    #X00000001, #XFFFFFFFF, #XFFFFFFFF, #X0000034F, #X000001E3, #X000002E4,
    #X00000148, #XFFFFFFFF, #X00000002, #XFFFFFFFF, #X00000247, #X000002E4,
    #X000001EB, #X000002F1, #X00000148, #XFFFFFFFF, #X00000003, #XFFFFFFFF,
    #X00000293, #X000002F1, #X000001F3, #X0000034B, #X00000148, #XFFFFFFFF,
    #X00000004, #X00000257, #XFFFFFFFF, #X0000034B, #X000001FB, #X000002DC,
    #X00000148, #XFFFFFFFF, #X00000005, #XFFFFFFFF, #XFFFFFFFF, #X000002DC,
    #X00000203, #X00000314, #X00000148, #XFFFFFFFF, #X00000006, #XFFFFFFFF,
    #XFFFFFFFF, #X00000314, #X0000020B, #X00000349, #X00000148, #XFFFFFFFF,
    #X00000007, #XFFFFFFFF, #XFFFFFFFF, #X00000349, #X00000213, #X000002F6,
    #X00000149, #XFFFFFFFF, #X00000000, #XFFFFFFFF, #XFFFFFFFF, #X000002F6,
    #X0000021B, #X000002F3, #X00000149, #XFFFFFFFF, #X00000001, #XFFFFFFFF,
    #XFFFFFFFF, #X000002F3, #X00000223, #X000002FB, #X00000149, #XFFFFFFFF,
    #X00000002, #XFFFFFFFF, #XFFFFFFFF, #X000002FB, #X0000022B, #X000002F5,
    #X00000149, #XFFFFFFFF, #X00000003, #XFFFFFFFF, #XFFFFFFFF, #X000002F5,
    #X00000233, #X000002E8, #X00000149, #XFFFFFFFF, #X00000004, #XFFFFFFFF,
    #X0000025F, #X000002E8, #X0000023B, #X000002E5, #X00000149, #XFFFFFFFF,
    #X00000005, #X0000024F, #XFFFFFFFF, #X000002E5, #X00000243, #X000002E6,
    #X00000149, #XFFFFFFFF, #X00000007, #XFFFFFFFF, #XFFFFFFFF, #X000002E6,
    #X0000024B, #X0000034C, #X0000014A, #XFFFFFFFF, #X00000001, #X00000275,
    #XFFFFFFFF, #X0000034C, #X00000253, #X000002E9, #X0000014A, #XFFFFFFFF,
    #X00000003, #XFFFFFFFF, #XFFFFFFFF, #X000002E9, #X0000025B, #X000002FC,
    #X0000014A, #XFFFFFFFF, #X00000002, #X0000027C, #XFFFFFFFF, #X000002FC,
    #X00000263, #X00000325, #X0000014B, #X00000000, #X00000283, #XFFFFFFFF,
    #X00000325, #X0000026B, #X0000034D, #X0000014C, #X00000000, #XFFFFFFFF,
    #XFFFFFFFF, #X0000034D, #X00000272, #X000002FD, #X0000014D, #X00000000,
    #XFFFFFFFF, #XFFFFFFFF, #X000002FD, #X00000279, #X00000326, #X0000014E,
    #X00000000, #XFFFFFFFF, #XFFFFFFFF, #X00000326, #X00000280, #X00000335,
    #X0000014F, #X00000001, #X00000010, #XFFFFFFFF, #XFFFFFFFF, #X00000335,
    #X00000287, #X000002F2, #X00000150, #X00000001, #X000000C7, #XFFFFFFFF,
    #XFFFFFFFF, #X000002F2, #X0000028F, #X0000032A, #X00000151, #XFFFFFFFF,
    #X00000004, #XFFFFFFFF, #X000002C1, #X0000032A, #X00000297, #X0000033E,
    #X00000151, #XFFFFFFFF, #X00000005, #XFFFFFFFF, #XFFFFFFFF, #X0000033E,
    #X0000029F, #X0000032E, #X00000152, #X00000000, #XFFFFFFFF, #XFFFFFFFF,
    #X0000032E, #X000002A7, #X00000302, #X00000153, #XFFFFFFFF, #X00000005,
    #XFFFFFFFF, #XFFFFFFFF, #X00000302, #X000002AE, #X00000307, #X00000153,
    #XFFFFFFFF, #X00000001, #XFFFFFFFF, #XFFFFFFFF, #X00000307, #X000002B6,
    #X0000032B, #X00000154, #X00000000, #XFFFFFFFF, #X000002D0, #X0000032B,
    #X000002BE, #X0000030E, #X00000155, #X00000000, #XFFFFFFFF, #XFFFFFFFF,
    #X0000030E, #X000002C5, #X0000032C, #X00000156, #X00000001, #X000000ED,
    #XFFFFFFFF, #XFFFFFFFF, #X0000032C, #X000002CC, #X02444200, #X04444546,
    #X42000000, #X02445700, #X03455155, #X03524546, #X04575245, #X46000000,
    #X03584F52, #X04544558, #X54000000, #X05544954, #X4C450000, #X0352454C,
    #X05535041, #X43450000, #X03535542, #X03535241, #X0353524C, #X03534346,
    #X03534C41, #X03534554, #X03524C41, #X04524C43, #X41000000, #X03525241,
    #X04525243, #X41000000, #X03525244, #X03534243, #X03525354, #X03525243,
    #X03524C44, #X02525200, #X03524C43, #X04524554, #X49000000, #X04524554,
    #X4E000000, #X02524C00, #X03524553, #X03524554, #X054E4545, #X44530000,
    #X04505241, #X47000000, #X04505553, #X48000000, #X034F5247, #X04504147,
    #X45000000, #X03504F50, #X044F5554, #X49000000, #X044F5449, #X52000000,
    #X044F5554, #X44000000, #X034F5554, #X044F5444, #X52000000, #X064E4F4C,
    #X49535400, #X034E4F50, #X024F5200, #X034E4547, #X03474554, #X044C4953,
    #X54000000, #X0448414C, #X54000000, #X034C4449, #X044C4449, #X52000000,
    #X034C4444, #X044C4444, #X52000000, #X03494E49, #X04494E49, #X52000000,
    #X024C4400, #X024A5000, #X024A5200, #X03494E44, #X04494E44, #X52000000,
    #X03494E43, #X02494E00, #X02494D00, #X03455858, #X02455800, #X03454E44,
    #X02454900, #X04444546, #X57000000, #X02445300, #X02444900, #X04444A4E,
    #X5A000000, #X04444546, #X4D000000, #X04444546, #X53000000, #X04444546,
    #X4C000000, #X03444546, #X03444543, #X03414253, #X03444141, #X0343504C,
    #X03434346, #X03435049, #X04435049, #X52000000, #X03435044, #X04435044,
    #X52000000, #X02435000, #X03414444, #X03414E44, #X03424954, #X0443414C,
    #X4C000000, #X03414443
    LET tab.start = 3
    LET locate(lv.tree, at) BE
    TEST !lv.tree<0 THEN !lv.tree:=0 ELSE
    $(  MANIFEST
        $(  c.size = 0; c.str  = 0; c.fn   = 1; c.opstr= 2
            t.left = 0; t.right= 1; t.str  = 2; t.val  = 3
            t.size = 4
        $)
        LET tree = !lv.tree+at
        LET val = tree!t.val+at
        val!c.fn := (@start-1)!(val!c.fn)
        val!c.str := val!c.str+at
        !lv.tree := !lv.tree+at
        tree!t.str := val!c.str
        tree!t.val := val
        locate(tree+t.left, at)
        locate(tree+t.right, at)
    $)
    locate(@tab.start, tab)
    codes := tab.start
$)
*/








// Start of STANDINIT
LET initcodes() BE
$(  code.put("DB",dataproc,0)
    code.put("DEFB",dataproc,0)
    code.put("DW",dwproc,0)
    code.put("DEFW",dwproc,0)
    code.put("DEFM",dataproc,0)
    code.put("DS",storeproc,0)
    code.put("DEFS",storeproc,0)
    code.put("EQU",equproc,0)
    code.put("DEF",defproc,0)
    code.put("REF",refproc,0)
    code.put("WREF",wrefproc,0)
    code.put("NEEDS",needsproc,0)
    code.put("PRAG",pragproc,0)
    code.put("DEFL",setproc,0)
    code.put("TEXT",textproc,0)
    code.put("TITLE",titlproc,0)
    code.put("ORG",orgproc,0)
    code.put("ABS",absproc,0)
    code.put("REL",relproc,0)
    code.put("GET",getproc,0)
    code.put("LIST",listproc,0)
    code.put("NOLIST",nolistproc,0)
    code.put("END",endproc,0)
    code.put("PAGE",ejectproc,0)
    code.put("SPACE",spaceproc,0)

// Start of MACROS:
    code.put("MACRO",macroproc,0)
    code.put("REPT",rptmacroproc,0)
    code.put("ENDM",endmacroproc,0)
    code.put("EXITM",breakmacroproc,0)
    code.put("IF",ifproc,-1,#X01)
    code.put("IFT",ifproc,-1,#X01)
    code.put("IFE",ifproc,-1,#X00)
    code.put("IFF",ifproc,-1,#X00)
    code.put("IFDEF",ifproc,-1,#X80)
    code.put("IFNDEF",ifproc,-1,#X81)
    code.put("IFB",ifproc,-1,#X08)
    code.put("IFNB",ifproc,-1,#X09)
    code.put("IFIDN",ifproc,-1,#X0A)
    code.put("IFDIF",ifproc,-1,#X0B)
    code.put("ELSE",elseproc,0)
    code.put("ENDIF",fiproc,0)
    code.put("LOCAL",localproc,0)
    code.put("OPT",optproc,0)
    code.put("PRINTX",printproc,0)
// End od MACROS

    code.put("EXX",fmt0,1,#XD9)
    code.put("DAA",fmt0,1,#X27)
    code.put("CPL",fmt0,1,#X2F)
    code.put("CCF",fmt0,1,#X3F)
    code.put("SCF",fmt0,1,#X37)
    code.put("NOP",fmt0,1,#X00)
    code.put("HALT",fmt0,1,#X76)
    code.put("DI",fmt0,1,#XF3)
    code.put("EI",fmt0,1,#XFB)
    code.put("RLA",fmt0,1,#X17)
    code.put("RLCA",fmt0,1,#X07)
    code.put("RRA",fmt0,1,#X1F)
    code.put("RRCA",fmt0,1,#X0F)
    code.put("LDI",fmt0,2,#XED,#XA0)
    code.put("LDIR",fmt0,2,#XED,#XB0)
    code.put("LDD",fmt0,2,#XED,#XA8)
    code.put("LDDR",fmt0,2,#XED,#XB8)
    code.put("CPI",fmt0,2,#XED,#XA1)
    code.put("CPIR",fmt0,2,#XED,#XB1)
    code.put("CPD",fmt0,2,#XED,#XA9)
    code.put("CPDR",fmt0,2,#XED,#XB9)
    code.put("NEG",fmt0,2,#XED,#X44)
    code.put("INI",fmt0,2,#XED,#XA2)
    code.put("INIR",fmt0,2,#XED,#XB2)
    code.put("IND",fmt0,2,#XED,#XAA)
    code.put("INDR",fmt0,2,#XED,#XBA)
    code.put("OUTI",fmt0,2,#XED,#XA3)
    code.put("OTIR",fmt0,2,#XED,#XB3)
    code.put("OUTD",fmt0,2,#XED,#XAB)
    code.put("OTDR",fmt0,2,#XED,#XBB)
    code.put("RETI",fmt0,2,#XED,#X4D)
    code.put("RETN",fmt0,2,#XED,#X45)
    code.put("RLD",fmt0,2,#XED,#X6F)
    code.put("RRD",fmt0,2,#XED,#X67)
    code.put("LD",fmt1,0)
    code.put("ADD",fmt2,-1,0)
    code.put("ADC",fmt2,-1,1)
    code.put("SUB",fmt2,-1,2)
    code.put("SBC",fmt2,-1,3)
    code.put("AND",fmt2,-1,4)
    code.put("XOR",fmt2,-1,5)
    code.put("OR",fmt2,-1,6)
    code.put("CP",fmt2,-1,7)
    code.put("RLC",fmt3,-1,0)
    code.put("RRC",fmt3,-1,1)
    code.put("RL",fmt3,-1,2)
    code.put("RR",fmt3,-1,3)
    code.put("SLA",fmt3,-1,4)
    code.put("SRA",fmt3,-1,5)
    code.put("SRL",fmt3,-1,7)
    code.put("BIT",fmt4,-1,1)
    code.put("SET",fmt4,-1,3)
    code.put("RES",fmt4,-1,2)
    code.put("JP",fmt5,0)
    code.put("CALL",fmt6,0)
    code.put("RET",fmt7,0)
    code.put("JR",fmt8,0)
    code.put("DJNZ",fmt9,1,#X10)
    code.put("RST",fmt10,1,#XC7)
    code.put("INC",fmt11,-1,4)
    code.put("DEC",fmt11,-1,5)
    code.put("EX",fmt12,0)
    code.put("PUSH",fmt13,-1,5)
    code.put("POP",fmt13,-1,1)
    code.put("IN",fmt14,0)
    code.put("OUT",fmt15,0)
    code.put("IM",fmt16,1,#XED)
$)
// End of STANDINIT


//
//                        Initialisation
//



LET startasm(version) = (version/1000)\=3 -> "version 3.008 27-Mar-80",
VALOF
$(  name:="Z80"
    i.here:=i.dollar
    comntcheck:=TRUE
    f.rinfo    := newf(0,3)
    f.ccinfo   := newf(3,3)
    f.ddinfo   := newf(0,2)
    f.source   := newf(3,3)
    f.ddsource := newf(4,2)
    f.dest     := newf(0,3)
    f.top      := newf(6,2)
    f.ccsmall  := newf(3,2)
    RESULTIS 0
$)
