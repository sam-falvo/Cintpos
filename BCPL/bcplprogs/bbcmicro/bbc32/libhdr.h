// Standard BCPL header for both Cintsys and Cintpos

// Modified by Martin Richards (c) 6 Dec 2019

/*
22/08/2019
The thread and cvp fields have been modified to allow devices.c
to store 64 bit addresses even when the BCPL word length is
32 bits.

11/02/04 MR
Added binwrch, removed packstring, unpackstring and dqpkt
21/10/02 MR
Made compatible with libhdr of the standard BCPL distribution
*/

MANIFEST {
// For implementions for which BITSPERBCPLWORD is not a reserved word,
// uncomment one of the following lines.
BITSPERBCPLWORDx = 32
//BITSPERBCPLWORD = 64

B2Wsh = 1 + BITSPERBCPLWORDx/32  // =2 for 32- bit implementations
                                // =3 for 64-bit implementations
ON64 = -(BITSPERBCPLWORDx/64)    // Not using the equals operator
}

// All that follows is the same for both 32- and 64-bit Cintcode systems.

// Globals used in the standard (single threaded) BCPL Cintcode System
GLOBAL {
globsize:            0
start:               1
stop:                2
sys:                 3  //SYSLIB   MR 18/7/01
clihook:             4
muldiv:              5  //SYSLIB   changed to G:5 MR 6/5/05
changeco:            6  //SYSLIB   MR 6/5/04
currco:              7
colist:              8
rootnode:            9  // For compatibility with native BCPL
result2:            10
tempval:            11  // A memory location used by native code
                        // floating point operations.
cis:                12
cos:                13
currentdir:         14
level:              15
longjump:           16
createco:           17
deleteco:           18
callco:             19
cowait:             20
resumeco:           21
initco:             22
startco:            23
globin:             24
getvec:             25
rdargs2:            26   // MR 19/11/2014
freevec:            27
abort:              28
sysabort:           29
packstring:         30
unpackstring:       31
getword:            32
putword:            33
randno:             34
setseed:            35
sardch:             36
sawrch:             37
rdch:               38
binrdch:            39
unrdch:             40
wrch:               41
binwrch:            42
deplete:            43
readwords:          44
writewords:         45
initio:             46
splitname:          47
findinput:          48
findoutput:         49
findinoutput:       50
findupdate:         51
findstream:         52
pathfindinput:      53
getremipaddr:       54
settimeout:         55
selectinput:        56
selectoutput:       57
input:              58
output:             59
endread:            60
endwrite:           61
endstream:          62
note:               63
point:              64
rewindstream:       65
appendstream:       66
stepstream:         67
setrecordlength:    68
recordpoint:        69
recordnote:         70
get.record:         71
put.record:         72
writeflt:           73  // Added Feb 2018
readflt:            74  // Added Feb 2018
copyobj:            75
deletefile:         76
renamefile:         77
freeobj:            78
copydir:            79
locatedir:          80
locateobj:          81
createdir:          82
readn:              83
newline:            84
writed:             85
writen:             86
writehex:           87
writeoct:           88
writes:             89
writet:             90
writeu:             91
writez:             92
get.textblib:       93  //BLIB version
get.text:           93  //BLIB overridden version
writef:             94  //BLIB
sawritef:           95
capitalch:          96
compch:             97
compstring:         98
copystring:         99
string.to.number:  100
str2numb:          101
rdargs:            102
rditem:            103
findarg:           104
loadseg:           105
unloadseg:         106
callseg:           107
datstring:         108
datstamp:          109
dat.to.strings:    110
string.to.dat:     111
setbit:            112
testbit:           113
copy.words:        114
clear.words:       115
copy.bytes:        116
setlogname:        117
getlogname:        118
intflag:           119
newpage:           120
instrcount:        121
setbulk:           122
stackfree:         123  // Returns the number of free stack locations
settimeoutact:     124
deleteself:        125
codewrch:          126 // Write an extended character in UTF8 or GB2312 format
randseed:          127
delay:             128 // delay(msecs)
delayuntil:        129 // delayuntil(days, msecs)
findappend:        130 // Added 18/01/11
memoryfree:        131 // Returns the amount of free and used memory.

//##### CLI uses globals 132 - 149 #####

cli.tallyflag:     132
cli.init:          133  // Not used
cli.result2:       134
cli.data:          135  // CLI dependent data  MR 10/7/03
cli.commanddir:    136
cli.returncode:    137  // This holds the returncode of the most
                        // recently executed command. It can be
			// inspected by commands such as if and why,
			// and may also be passed back to the
			// enclosing operating system shell.
cli.commandname:   138
cli.faillevel:     139
cli.prompt:        140
cli.standardinput: 141
cli.currentinput:  142
cli.commandfile:   143  // Name of temporary command file used in
                        // command-commands
cli.status:        144  // Contains the CLI status flags such as
                        // clibit.noprompt, clibit.comcom or clibit.maincli

cli.preloadlist:   145
cli.currentoutput: 146
cli.defaultstack:  147  // The coroutine stack size of the next cli command.
cli.standardoutput:148
cli.module:        149  // The code segment of the currently executing
                        //command, if any.

//##### Cintpos uses globals 150 - 179 #####

srchwk:            150
tcb:               151
taskid:            152
createtask:        153
deletetask:        154
changepri:         155
setflags:          156
testflags:         157
hold:              158
unhold:            159;  release:           159
taskwait:          160
qpkt:              161
endtask:           162

sendpkt:           165 // Overridden when in multievent mode

returnpkt:         169

consoletask:       171
createdev:         172
deletedev:         173
fault:             174
set.process.name:  175

peercom:           179

writee:            180 // Write a floating point number in exponential form
setvec:            181 // (v, n, a1,..,a16) Copy values into v
 
// Globals 190-199 are variables not reset between CLI commands
current.language:  190 // Potentially used by get.text when converting
                       // an error number to text.
}

MANIFEST {

tg = 190   // First user global not reset between CLI commands
ug = 200   // First user global
bytesperword    = 1<<B2Wsh
bitsperbyte	= 8
bitsperword	= 32//bitsperbyte * bytesperword
mcaddrinc	= bytesperword
minint          = 1<<(bitsperword-1)  // = #x80....0
maxint          = minint - 1          // = #x7F....F

endstreamch	= -1  // ch returned at EOF
timeoutch	= -2  // ch returned when none available before timeout
pollingch	= -3  // ch returned when none available when polling

// Object module format types

t.hunk    = 1000 // A hunk in ASCII hex
t.reloc   = 1001
t.end     = 1002
t.hunk64  = 2000 // A hunk in ASCII hex for 64-bit Cintcode
t.reloc64 = 2001
t.end64   = 2002
t.bhunk   = 3000 // A hunk in binary
t.bhunk64 = 4000 // A hunk in binary for 64-bit Cintcode

globword  = #xFFFFFFFF8F8F0000  // MR 7/9/2006 (for 64-bit version)
stackword = #xFFFFFFFFABCD1234
deadcode  = #xFFFFFFFFDEADC0DE
sectword  = #x000000000000FDDF
entryword = #x000000000000DFDF

// Important global variable numbers
g.globsize = 0
g.sys      = 3
g.currco   = 7
g.colist   = 8
g.rootnode = 9

g.memsize  = 14
g.keyboard = 20
g.screen   = 21

// co-routine stackbase offsets

co.pptr = 0
co.parent=1
co.list=2
co.fn=3
co.size=4
co.c=5

InitObj  = 0  // Initialisation and closing methods for objects
CloseObj = 1

// Rootnode manifests

rootnodeaddr = 100  // MR 21/10/02 for compatibility with Cintpos
                    // Not used in native code versions

rtn.tasktab = 0
rtn.devtab=1
rtn.tcblist=2
rtn.crntask=3
rtn.blklist=4
rtn.tallyv=5

rtn.clkintson=6
rtn.lastch=7         // For sadebug polling input
rtn.insadebug=8      // Looked at by ttyin device

rtn.bptaddr=9        // Breakpoint addresses      ) MR 20/9/02
rtn.bptinstr=10      // Breakpoint instructions   )
rtn.dbgvars=11       // The Standalone Debug variables

rtn.clwkq=12         // 12
rtn.membase=13
rtn.memsize=14
rtn.info=15
rtn.sys=16
rtn.boo=17          // BOOT code
rtn.klib=18         // KLIB code segments
rtn.blib=19         // BLIB code segments
rtn.keyboard=20     // Keyboard stream
rtn.screen=21       // Screen stream

rtn.vecstatsv=22    // 22
rtn.vecstatsvupb=23 // 23

rtn.intflag=24      // Set to TRUE by ctrl-c and FALSE by sadebug
rtn.dumpflag=25     // =TRUE for memory dump to DUMP.mem
rtn.envlist=26      // List of logical name-value pairs
                    // used by setlogname and getlogname
rtn.abortcode=27    // Latest reason for leaving the interpreter
rtn.context=28      // Context of DUMP.mem
                    // 1 dump caused by second SIGINT
                    // 2 dump caused by SIGSEGV
                    // 3 fault in BOOT or standalone debug
                    // 4 dump by user calling sys(Sys.quit, -2)
                    // 5 dump caused by non zero user fault code
                    // 6 dump requestested from standalone debug

rtn.sysp=29         // Latest setting of p pointer at SIGINT or SIGSEGV
rtn.sysg=30         // Latest setting of p pointer at SIGINT or SIGSEGV
rtn.sysst=31        // Latest setting of st
                    // st = 0    in a user task, interrupts enabled
                    // st = 1    in BOOT,        interrupts disabled
                    // st = 2    in KLIB,        interrupts disabled
                    // st = 3    in the ISR      interrupts disabled

rtn.idletcb=32      // The IDLE TCB (for debugging)
rtn.adjclock=33     // Real time clock adjustment in minutes

rtn.dcountv=34      // the Debug Counts vectors

// The following four variables are set by boot.b
// and used by programs such as cli.b, bcpl.b and c.b

rtn.rootvar=35      // The environment variable giving the
                    // system root directory, eg "BCPLROOT" or "POSROOT"
rtn.pathvar=36      // The environment variable giving the directories
                    // searched by loadseg, eg "BCPLPATH" or "POSPATH"
rtn.hdrsvar=37      // The environment variable giving the directories
                    // containing BCPL headers, eg "BCPLHDRS" or "POSHDRS"
rtn.scriptsvar=38   // The environment variable giving the directories
                    // containing cli scripts, eg "BCPLSCRIPTS" or "POSSCRIPTS"
rtn.boottrace=39    // =0, 1, 2 or 3 as set by -v and -vv options to
                    // trace the progress of booting the system.

rtn.days=40         // Days since 1 Jan 1970 (1978 old dat format)
rtn.msecs=41        // Milliseconds since midnight
rtn.mins=42
rtn.msecs=42 // for old dat format
rtn.ticks=43        // =-1 for new dat format

rtn.mc0=44          // Machine address of the start of the
                    // Cintcode memory.
rtn.mc1=45          // Other values used by the MC package.
rtn.mc2=46       
rtn.mc3=47       

rtn.system=48       // =1 for cintsys, =2 for cintpos, =0 otherwise

rtn.icount=49       // Used by cinterp to enter polling code about
                    // 50 times per second.

rtn.joystickfd=50     // The joystick fd
rtn.joystickfd1=51    // The joystick fd second word
rtn.joybuttoncount=52 // The number of joystick buttons
rtn.joyaxiscount=53   // The number of joystick axes
rtn.joycurrbuttons=54 // The bit pattern of currently pressed buttons
rtn.joybuttons=55     // The bit pattern of recently pressed buttons
rtn.joyaxis0=56       // The value of axis0
rtn.joyaxis1=57       // The value of axis1
rtn.joyaxis2=58       // The value of axis2
rtn.joyaxis3=59       // The value of axis3
rtn.joyaxis4=60       // The value of axis4
rtn.joyaxis5=61       // The value of axis5
rtn.joyaxis6=62       // The value of axis6

rtn.hostaddsize=63    // Size in bits os a machine address on the
                      // host machine
  
rtn.upb = 80       // Leave some unused entries

// SYS functions
Sys.setcount        =  -1
Sys.quit            =   0
Sys.rti             =   1
Sys.saveregs        =   2
Sys.setst           =   3
Sys.tracing         =   4
Sys.watch           =   5
Sys.tally           =   6
Sys.interpret       =   7

Sys.sardch          =  10
Sys.sawrch          =  11
Sys.read            =  12
Sys.write           =  13
Sys.openread        =  14
Sys.openwrite       =  15
Sys.close           =  16
Sys.deletefile      =  17
Sys.renamefile      =  18
Sys.openappend      =  19

Sys.getvec          =  21
Sys.freevec         =  22
Sys.loadseg         =  23
Sys.globin          =  24
Sys.unloadseg       =  25
Sys.muldiv          =  26
Sys.intflag         =  28
Sys.setraster       =  29
Sys.cputime         =  30
Sys.filemodtime     =  31
Sys.setprefix       =  32
Sys.getprefix       =  33
Sys.graphics        =  34  // Not implemented

Sys.seek            =  38  // MR 10/11/01
Sys.tell            =  39  // MR 10/11/01
Sys.waitirq         =  40  // MR  4/02/02
Sys.lockirq         =  41  // MR 24/02/03
Sys.unlockirq       =  42  // MR 24/02/03
Sys.devcom          =  43  // MR  4/02/02
Sys.datstamp        =  44  // MR 29/03/10

Sys.filesize        =  46  // MR 15/03/02
Sys.openreadwrite   =  47  // MR 19/03/02
Sys.getsysval       =  48  // MR 18/11/02
Sys.putsysval       =  49  // MR 18/11/02
Sys.shellcom        =  50  // MR 13/01/03
Sys.getpid          =  51  // MR  7/10/03
Sys.dumpmem         =  52  // MR 29/10/03 Used only in BOOT.b
Sys.callnative      =  53  // MR 24/04/04
Sys.platform        =  54  // MR 06/04/05 architecture and OS
Sys.inc             =  55  // MR 17/12/04
Sys.buttons         =  56  // MR 21/06/06 Button on the GP2X
Sys.delay           =  57  // MR 01/05/10 Delay until a specified date and time
Sys.sound           =  58  // MR 11/09/07 Sound functions
Sys.callc           =  59  // MR 28/01/09 Call the C function
                           //             callc(args,g)
Sys.trpush          =  60  // MR 05/02/10 Push a trace value
Sys.settrcount      =  61  // MR 05/02/10 Set trcount
Sys.gettrval        =  62  // MR 05/02/10 Get a pushed trace value
Sys.flt             =  63  // MR 21/07/10 Floating point operations
Sys.pollsardch      =  64  // MR 07/03/11 Return next ch or -3
Sys.incdcount       =  65  // MR 06/03/12 Increment a specified debug counter.

Sys.sdl             =  66  // MR 30/05/12 SDL features
Sys.gl              =  67  // MR 12/01/14 OpenGL features
Sys.ext             =  68  // MR 14/04/14 EXT user extension features
Sys.joy             =  69  // MR 22/01/18 Joystick features


bootregs = 11 // Registers used to enter the function start in boot.b
cliregs  = 21 // Registers used by BOOT to start the CLI
klibregs = 21 // Registers used by BOOT to start KLIB
saveregs = 31 // Registers of an interrupt enabled user program at
              // the time a Cintpos interrupt was entered. These registers
              // are only valid when the interrupt service routine is
              // active. In this state register st=3.
isrregs  = 41 // Registers for the Cintpos interrupt service routine

id.inscb	= #x81  // MR 21/10/02
id.outscb	= #x82  // MR 21/10/02
id.inoutscb	= #x83  // MR 21/10/02
id.appendscb	= #x84  // MR 18/01/11

scbt.net     =  2  // Non interactive TCP/IP stream
scbt.file    =  1  // Non interactive disc file stream
scbt.ram     =  0  // Non interactive RAM stream
scbt.console = -1  // Interactive -- output triggered by '*n' etc
scbt.mbx     = -2  // Interactive MBX stream
scbt.tcp     = -3  // Interactive TCP/IP stream

scb.maxnamelen = 31

scb.id = 0         // id.inscb, id.outscb or id.inoutscb
scb.type=1           // <=0 interactive stream, >0 block file
scb.task=2           // 0 or the task associated with this stream
scb.buf=3            // 0 or the byte buffer for this stream
scb.pos=4            // position of next character to be transferred
scb.end=5            // number of valid bytes in the buffer or -1
scb.rdfn=6           // zero or function to replenish the buffer
scb.wrfn=7           // zero or function to deplete the buffer
scb.endfn=8          // zero or function to close down the stream
scb.block=9          // Current block number of a disc file
scb.write=10          // Buf updated to but not yet written to disc
scb.bufend=11         // Size of buf in bytes
scb.lblock=12         // Number of last block of a disc file
scb.ldata=13          // Bytes in last block of a disc file
scb.blength=14        // Length of a disc block in bytes (typically 4096)
scb.reclen=15         // Record length in bytes for some files
scb.fd=16             // File or mailbox descriptor MR 18/4/02
scb.fd1=17            // File or mailbox descriptor, second word
scb.timeout=18        // The stream timeout value in milli-seconds MR 26/3/02
                      // = 0  means no time out is to be applied
                      // =-1  only transfer data that is immediately possible
scb.timeoutact=19     // Action if a timeout occurs
                      // = 0  Try the operation again
                      // =-1  Abort the operation
                      // =-2  Return timeoutch
scb.encoding=29       // Unicode encoding: UTF8 (=-1) or GB2312 (=-2),
                      // used by uniwrch.
scb.name=30           // Pointer to name of stream, see below

scb.nameeend =50// scb.name + scb.maxnamelen/bytesperword
                      // Last word of space for name

scb.size =51//scb.nameend+1
scb.upb = 50//scb.size-1

// Unicode encodings
UTF8 = -1
GB2312 = -2

return.severe    =  20
return.hard      =  10
return.soft      =   5
return.ok        =   0
cli.module.gn    =  149
cli.initialstack =  50000       // Changed 21/5/2001
cli.initialfaillevel = return.hard

// cli.status flags
clibit.noprompt  =  #b000000001  // Don't output a prompt
clibit.eofdel    =  #b000000010  // Delete this task if EOF received
clibit.comcom    =  #b000000100  // Currently execution a command-command
clibit.maincli   =  #b000001000  // Executing the main CLI
clibit.newcli    =  #b000010000  // Executing a new CLI
clibit.runcli    =  #b000100000  // Executing a CLI invoked by run
clibit.mbxcli    =  #b001000000  // Executing an MBX CLI
clibit.tcpcli    =  #b010000000  // Execution a TCP CLI
clibit.endcli    =  #b100000000  // endcli has been executed on this CLI

//notinuse	= -1

// standard packet offsets

//pkt.link =  0
//pkt.id   =  1; pkt.devid =  1; pkt.devtaskid = 1; pkt.taskid = 1
//pkt.type =  2; pkt.op    =  2
//pkt.res1 =  3; pkt.r1    =  3
//pkt.res2 =  4; pkt.r2    =  4
//pkt.arg1 =  5; pkt.a1    =  5
//pkt.arg2 =  6; pkt.a2    =  6
//pkt.arg3 =  7; pkt.a3    =  7
//pkt.arg4 =  8; pkt.a4    =  8
//pkt.arg5 =  9; pkt.a5    =  9
//pkt.arg6 = 10; pkt.a6    = 10

// TCB offsets

tcb.link	=  0
tcb.taskid	=  1
tcb.pri		=  2
tcb.wkq		=  3
tcb.state	=  4
tcb.flags	=  5
tcb.stsiz	=  6
tcb.seglist	=  7
tcb.gbase	=  8
tcb.sbase	=  9
tcb.active	= 10 // TRUE if the task is fully activated

tcb.regs        = 11
tcb.a           = tcb.regs
tcb.b           = 12
tcb.c           = 13
tcb.p           = 14
tcb.g           = 15
tcb.st          = 16
tcb.pc          = 17
tcb.count       = 18

tcb.namebase    = 19 // Space for upto 15 chars of task name

tcb.upb = tcb.namebase + 15/bytesperword + 1


//flag.a       = 1<<0                // MR 05/05/10
//flag.b       = 1<<1                // Note that the bit
//flag.c       = 1<<2                // positions have changed
//flag.d       = 1<<3
//flag.e       = 1<<4

g.grfbase = 400 // Number of the first global in the Graphics library
g.sndbase = 400 // Number of the first global in the Sound library
g.sdlbase = 450 // Number of the first global in the SDL library
g.glbase  = 450 // Number of the first global in the GL library
g.extbase = 950 // Number of the first global in the EXT library
}

