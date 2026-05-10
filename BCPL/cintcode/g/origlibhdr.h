// This is the library header file used by origbcpl.b
// It is compatible with libhdr but much smaller.

// Modified by Martin Richards (c) Sept 2016

/*
11/02/04 MR
Added binwrch, removed packstring, unpackstring and dqpkt
21/10/02 MR
Made compatible with libhdr of the standard BCPL distribution
*/

MANIFEST {
B2Wsh = 2
}

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
returncode:         11
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
get_record:         71
put_record:         72
get_index_record:   73  // Not yet implemented
put_index_record:   74  // Not yet implemented
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
get_textblib:       93  //BLIB version
get_text:           93  //BLIB overridden version
writef:             94  //BLIB
sawritef:           95
capitalch:          96
compch:             97
compstring:         98
copystring:         99
string_to_number:  100
str2numb:          101
rdargs:            102
rditem:            103
findarg:           104
loadseg:           105
unloadseg:         106
callseg:           107
datstring:         108
datstamp:          109
dat_to_strings:    110
string_to_dat:     111
setbit:            112
testbit:           113
copy_words:        114
clear_words:       115
copy_bytes:        116
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
}

MANIFEST {

//tg = 190   // First user global not reset between CLI commands
ug = 200   // First user global

bytesperword    = 1<<B2Wsh

endstreamch	= -1  // ch returned at EOF
}

