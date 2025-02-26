// Standard MCPL header

MANIFEST
  B2Wsh=2   // for a 32 bit implementation
//B2Wsh=3   // for a 64 bit implementation

GLOBAL

globsize:0,    // the size of the global vector


start:1,       //        start()
stop:2,        //        stop(code)
sys:3,         // res := sys(op,...)
clihook:4,     //        clihook()
hptr:5,
changeco:6,    //        changeco(arg, cptr)
currco:7,      // the current coroutine
colist:8,      // the list of all coroutines
rootnode:9,    // the rootnode
result2:10,    // the second result of functions
intflag:11,    // res := intflag()
sardch:12,     // ch  := sardch()
sawrch:13,     //        sawrch(ch)
sawritef,      // standalone version of writef
trace,         // Turn on interpreter tracing
notrace,       // Turn off interpreter tracing
mkobj,         // obj := mkobj(fnv)
 
muldiv:19,     // res := muldiv(a, b, c)

createco:21,   // co  := createco(fn, stsize)
deleteco:22,   //        deleteco(co)
callco:23,     // arg := callco(co, arg)
cowait:24,     // arg := cowait(arg)
resumeco:25,   // arg := resumeco(co, arg)
initco:26,     // co  := initco(fn, stsize, a,...)
globin:28,     // res := globin(segl)
getvec:29,     // v   := getvec(upb)
freevec:30,    //        freevec(v)
abort:31,      //        abort(code, arg)

cis:40,        // the currently selected input stream
cos:41,        // the currently selected output stream
rdch:42,       // ch  := rdch()
unrdch:43,     // res := unrdch()
wrch:44,       //        wrch(ch)
findinput:45,  // scb := findinput(name)
findoutput:46, // scb := findoutput(name)
selectinput:47,//        selectinput(scb)
selectoutput:48,//       selectoutput(scb)
endread:49,    //        endread()
endwrite:50,   //        endwrite()
input:51,      // scb := input()
output:52,     // scb := output()
readn:60,      // n   := readn()
newline:61,    //        newline()
newpage:62,    //        newpage()
writed:63,     //        writed(n, d)
writeu:64,     //        writeu(n, d)

writeoct:66,   //        writeoct(n, d)
writehex:67,   //        writehex(n, d)
writes:68,     //        writes(str)
writet:69,     //        writet(str, d)
writef:70,     //        writef(format, a,...)
capitalch:75,  // ch  := capitalch(ch)
compch:76,     // res := compch(ch1, ch2)
compstring:77, // res := compstring(s1, s2)
rdargs:78,     // res := rdargs(keys, v, upb)
rditem:79,     // res := rditem(v, upb)
findarg:80,    // res := findarg(keys, item)
loadseg:81,    // seg := loadseg(name)
unloadseg:82,  //        unloadseg(segl)
callseg:83,    // res := callseg(name,  ... args)
deletefile:85, // res := deletefile(name)
renamefile:86, // res := renamefile(name1, name2)
randno:87,     // n   := randno(upb)
randseed:88,   // seed for randno
str2numb:89,   // n   := str2numb(string)
pathfindinput:90,  // scb := pathfidinput(name, pathname)
string_to_number:91

// Globals 133-149 are defined in CLIHDR

MANIFEST
  Endstreamch    = -1,
  Bytesperword   = 1<<B2Wsh,
  Bpw            = Bytesperword,
  Bitsperbyte    = 8,
  Bitsperword    = Bitsperbyte*Bpw,
  Minint         = 1<<(Bitsperword-1),  // = #x80....0
  Maxint         = Minint-1,            // = #x7F....F

  Mcaddrinc      = Bpw,
  Ug             = 200,

  T_hunk         =  1000,
  T_end          =  1002,

  Co_pptr        = 0,
  Co_parent      = 1,
  Co_list        = 2,
  Co_fn          = 3,
  Co_size        = 4,

// rootnode offsets
  Rtn_membase    =     0,
  Rtn_memsize    =     1,
  Rtn_blklist    =     2,
  Rtn_tallyv     =     3,
  Rtn_msyslib    =     4,
  Rtn_mlib       =     5,
  Rtn_mboot      =     6,
  Rtn_mcli       =     7,
  Rtn_keyboard   =     8,
  Rtn_screen     =     9,
  Rtn_upb        =    20,

  Initobj = 0  // Position of the Init method in every class




