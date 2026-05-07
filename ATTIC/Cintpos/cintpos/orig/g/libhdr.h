// Standard BCPL header for Cintcode TRIPOS

GLOBAL
$(
globsize:0
start:1        //        start(pkt)
stop:2         //        stop(code)
sys:3          //        sys(n,...)
clihook:4      //        clihook()
changeco:6     //        changeco(cptr, val)
currco:7       // the current coroutine
colist:8       // the list of all coroutines
srchwk:9       // the scheduler

result2:10
returncode:11
sardch:12      // ch  := sardch()
sawrch:13      //        sawrch(ch)

stackbase:14
tcb:15         // tcb of this task
taskid:16      // task id of this task

level:17       // p   := level()
longjump:18    //        longjump(p, l)
muldiv:19      // res := muldiv(a, b, c)

createco:21    // co  := createco(fn, stsize)
deleteco:22    //        deleteco(co)
callco:23      // arg := callco(co, arg)
cowait:24      // arg := cowait(arg)
resumeco:25    // arg := resumeco(co, arg)
initco:26      // co  := initco(fn, stsize, a,...)

globin:28      // res := globin(seg)
getvec:29      // v   := getvec(upb)
freevec:30     //        freevec(v)
abort:31       //        abort(code, arg)
packstring:32  // res := packstring(v, s)
unpackstring:33//        unpackstring(s, v)
mkobj:34       // obj := mkobj(upb, fns)******************

cis:40         // the currently selected input stream
cos:41         // the currently selected output stream
rdch:42        // ch  := rdch()
unrdch:43      // res := unrdch()
wrch:44        //        wrch(ch)
findinput:45   // scb := findinput(name)
findoutput:46  // scb := findoutput(name)
selectinput:47 //        selectinput(scb)
selectoutput:48//        selectoutput(scb)
endread:49     //        endread()
endwrite:50    //        endwrite()
input:51       // scb := input()
output:52      // scb := output()
flush:53       // res := flush()
readn:60       // n   := readn()
newline:61     //        newline()
newpage:62     //        newpage()
writed:63      //        writed(n, d)
writeu:64      //        writeu(n, d)
writen:65      //        writen(n)
writeoct:66    //        writeoct(n, d)
writehex:67    //        writehex(n, d)
writes:68      //        writes(str)
writet:69      //        writet(str, d)
writef:70      //        writef(format, a,...)
writebin:71    //        writebin(n, d)

capitalch:75   // ch  := capitalch(ch)
compch:76      // res := compch(ch1, ch2)
compstring:77  // res := compstring(s1, s2)
rdargs:78      // res := rdargs(keys, v, upb)
rditem:79      // res := rditem(v, upb)
findarg:80     // res := findarg(keys, item)
loadseg:81     // seg := loadseg(name)
unloadseg:82   //        unloadseg(segl)
callseg:83     // res := callseg(name,  ... args)
deletefile:85  // res := deletefile(name)
renamefile:86  // res := renamefile(name1, name2)
randno:87      // n   := randno(upb)
str2numb:88    // n   := str2numb(string)

createdev:100   // id  := createdev(dcb)
deletedev:101   // dcb := deletedev(id)
createtask:102  // id  := createtask(seglist, stsize, pri)
deletetask:103  // res := deletetask(id)
changepri:104   // res := changepri(id, pri)
setflags:105    // res := setflags(id, flags)
testflags:106   // res := testflags(flags)

hold:107        // res := hold(id)
release:108     // res := release(id)
taskwait:109    // pkt := taskwait()
qpkt:110        // res := qpkt(pkt)
dqpkt:111       // res := dqpkt(id, pkt)

endtask:115     //        endtask(seg)
delay:116       // res := delay(ticks)
sendpkt:117     // res := sendpkt(link, id, type, res1, res2, ... args)
returnpkt:118   // res := returnpkt(pkt, res1, res2)
initio:119      //        initio()
currentdir:120

readwords:121   // res := readwords(v, n)
writewords:122  //        writewords(v, n)

// Globals 133-149 are defined in CLIHDR

tidyup:150      // User redefinable task tidy up routine
datstring:151   // v   := datstring(v)
datstamp:152    // v   := datstamp(v)
deleteobj:153   // res := deleteobj(name)
renameobj:154   // res := renameobj(name1, name2)
findupdate:155  // scb := findupdate(name)
endstream:156   //        endstream(scb)
get2bytes:157   // wrd := get2bytes(v, wordoffset)
put2bytes:158   //        put2bytes(v, wordoffset, word)
pktwait:159    // pkt := pktwait(dest, pkt)
devicetask:160
//devicename:161
fault:162
consoletask:163
splitname:164
locateobj:165
freeobj:166    //        freedir(dir)
findobj:167
copyobj:168    // res := copyobj ( obj )
//copydir:168    // res := copyobj ( obj )
note:169       // res := note(scb, v)
point:170      // res := point(scb, v)

locatedir:171  // dir := locatedir(name)
createdir:172  // res := createdir(name)

// Standalone functions from Cintcode BLIB
sagetvec:180
safreevec:181
saloadseg:182
saunloadseg:183
saglobin:184
sadeletefile:185
sarenamefile:186
sawritef:187    //        sawritef(format, ..args..)

// Globals 190-199 reserved for linking loader

$)

MANIFEST
$(
endstreamch=-1
notinuse=-1
bytesperword=4
BitsPerWord=32
BitsPerByte=8
maxint=#x7FFFFFFF
minint=#x80000000
tickspersecond=50  // Changed 11 Sep 97
mcaddrinc=4

rootregs =1
regsclk  =10
regskbd  =20
regsdisc =30
regsdebug=40

globword= #xEFEF0000
entryword = #x0000DFDF
rootnode=100
ug=200
$)

MANIFEST
$(
// Coroutine fields
Co_pptr        = 0
Co_parent      = 1
Co_list        = 2
Co_fn          = 3
Co_size        = 4

// Standard task numbers
Task_cli            =     1
Task_debug          =     2
Task_consolehandler =     3
Task_filehandler    =     4
// States and flags
State_pkt           =     1
State_hold          =     2
State_wait          =     4
State_int           =     8
State_dead          =    12
Flag_break          =     1
// Standard packet offsets
Pkt_link            =     0
Pkt_id              =     1
Pkt_Devtaskid       =     1
Pkt_taskid          =     1
Pkt_devid           =     1
Pkt_type            =     2
Pkt_res1            =     3
Pkt_res2            =     4
Pkt_arg1            =     5
Pkt_arg2            =     6
Pkt_arg3            =     7
Pkt_arg4            =     8
Pkt_arg5            =     9
Pkt_arg6            =    10

// Rootnode offsets
Rtn_tasktab         =     0
Rtn_devtab          =     1
Rtn_tcblist         =     2
Rtn_crntask         =     3
Rtn_blklist         =     4
Rtn_debtask         =     5
Rtn_days            =     6
Rtn_mins            =     7
Rtn_ticks           =     8
Rtn_clwkq           =     9
Rtn_membase         =    10
Rtn_memsize         =    11
Rtn_info            =    12
Rtn_sys             =    13
Rtn_trapcode        =    14
Rtn_trapped         =    15

Rtn_blib            =    17
Rtn_boot            =    18
Rtn_klib            =    19
Rtn_tallyv          =    20

Rtn_upb             =    25

// Rootnode info field
Info_mctype         =     0
Info_assignments    =     1
Info_devices        =     2
Info_handlers       =     3

// TCB offsets
Tcb_link            =     0
Tcb_taskid          =     1
Tcb_pri             =     2
Tcb_wkq             =     3
Tcb_state           =     4
Tcb_flags           =     5
Tcb_stsiz           =     6
Tcb_seglist         =     7
Tcb_gbase           =     8
Tcb_sbase           =     9

Tcb_regs            =    10
Tcb_a               =    10
Tcb_b               =    11
Tcb_c               =    12
Tcb_p               =    13
Tcb_g               =    14
Tcb_intson          =    15
Tcb_pc              =    16
Tcb_count           =    17

Tcb_upb             =    17

// DCB offsets
Dcb_devid           =     1
Dcb_wkq             =     2
Dcb_startfn         =     3

// SYS functions
Sys_rti             =     1
Sys_saveregs        =     2
Sys_traceon         =     3
Sys_traceoff        =     4

Sys_sardch          =    10
Sys_sawrch          =    11
Sys_read            =    12
Sys_write           =    13
Sys_openread        =    14
Sys_openwrite       =    15
Sys_close           =    16
Sys_deletefile      =    17
Sys_renamefile      =    18

Sys_globin          =    20
Sys_globin          =    21
Sys_freevec         =    22
Sys_loadseg         =    23
Sys_globin          =    24
Sys_unloadseg       =    25
Sys_muldiv          =    26
Sys_intflag         =    28

Sys_intson          =    32
Sys_intsoff         =    33
Sys_pause           =    34
Sys_getshbyte       =    35
$)




