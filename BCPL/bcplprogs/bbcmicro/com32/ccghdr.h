MANIFEST {
t.hunk  = 1000       // Object module item types.
t.bhunk = 3000       // binary hunk (not hex)
t.end   =  992

sectword   = #xFDDF   // SECTION name marker.
needsword  = #xFEED   // NEEDS name marker.
entryword  = #xDFDF   // Function namde marker.

// OCODE keywords.
s.true=4; s.false=5; s.rv=8; s.fnap=10
s.mult=11; s.div=12; s.rem=13
s.plus=14; s.minus=15; s.query=16; s.neg=17; s.abs=19
s.eq=20; s.ne=21; s.ls=22; s.gr=23; s.le=24; s.ge=25
s.not=30; s.lshift=31; s.rshift=32; s.logand=33
s.logor=34; s.eqv=35; s.neqv=36
s.lf=39; s.lp=40; s.lg=41; s.ln=42; s.lstr=43
s.ll=44; s.llp=45; s.llg=46; s.lll=47
s.needs=48; s.section=49; s.rtap=51; s.goto=52; s.finish=68
s.switchon=70; s.global=76; s.sp=80; s.sg=81; s.sl=82; s.stind=83
s.jump=85; s.jt=86; s.jf=87; s.endfor=88; s.xlab=89
s.lab=90; s.stack=91; s.store=92; s.rstack=93; s.entry=94
s.save=95; s.fnrn=96; s.rtrn=97; s.res=98
s.datalab=100; s.iteml=101; s.itemn=102; s.endproc=103; s.debug=109; s.none=111
s.getbyte=120; s.putbyte=121

h1=0; h2=1; h3=2  // Selectors.
}

GLOBAL {
fin.p:237; fin.l:238
errcount:291; errmax:292; gostream: 297

codegenerate: 399

// Global procedures.
rdn:211     // reads numbers from the OCODE buffer

cgsects    : 300//
rdn        : 301//
rdl        : 302//
rdgn       : 303//
newlab     : 304//
checklab   : 305//
cgerror    : 306//

initstack  : 310//
stack      : 311//
store      : 312//
scan       : 313//
cgpendingop: 314//
loadval    : 315//
loadba     : 316//
setba      : 317//

genxch     : 320//
genatb     : 321//
loada      : 322//
push       : 323//
loadboth   : 324//
inreg.a    : 325//
inreg.b    : 326//

addinfo.b  : 328//
addinfo.a  : 329//

pushinfo   : 724
xchinfo    : 725
atbinfo    : 726

setinfo.a  : 327//
setinfo.b  : 328//

forget.a   : 330//
forget.b   : 331//
forgetall  : 332//
forgetvar  : 333//
forgetallvars: 334//
mustforget : 335//
isnum      : 336//
iszero     : 337//
storet     : 338//
gensp      : 339//
loadt      : 340//
lose1      : 341//
swapargs   : 342//
cgbyteop   : 343//
cgstind    : 344//
storein    : 345//

cgrv       : 350//
cgplus     : 351//
//cgaddk     : 732
cgglobal   : 352//
cgentry    : 353//
cgsave     : 354//
cgapply    : 355//
cgreturn   : 356//
cgcondjump : 357//
jmpfn      : 358//
jfn0       : 359//
revjfn     : 360//
compjfn    : 361//
prepj      : 362//

cgswitch   : 370//
cgswitchb  : 371//
cgswitchl  : 372//
cgstring   : 373//
setlab     : 374//
cgdatalab  : 376//
cgstatics  : 377//
newblk     : 378//
appendblk  : 380//
freeblk    : 379//

initdatalists : 381//

geng       : 390//
gen        : 391//
genb       : 392//
genr       : 393//
genw       : 394//
checkspace : 395//
codeb      : 396//
codew      : 397//
coder      : 398//

getw       : 399//
putw       : 410//
aligneven  : 411//
chkrefs    : 412//
dealwithrefs:413//
genindword : 414//
inrange.d  : 415//
inrange.i  : 416//
fillref.d  : 417//
fillref.i  : 418//
relref     : 419//

outputsection : 420//
OBJWORD    : 421//
dboutput   : 422//

wrcode     : 424//

wrkn       : 500
///wrcode     : 501
wrfcode    : 502

// Global variables.
arg1       : 431//
arg2       : 432//

casek      : 434//
casel      : 435//

ssp        : 471//

dpblk      : 475//
dq         : 476//
dpblklist  : 477//

tempt      : 483//
tempv      : 484//
stv        : 480//
stvp       : 481//

dp         : 442//
freelist   : 443//

incode     : 445//
labv       : 446//

maxgn      : 450//
maxlab     : 451//
maxssp     : 452//

op         : 455//
labnumber  : 456//
pendingop  : 457//
procdepth  : 458//

progsize   : 460//

infok.a    : 461//
infon.a    : 462//
infok.b    : 463//
infon.b    : 464//
reflist    : 465//
refliste   : 466//
rlist      : 468//
rliste     : 469//
glist      : 438//
gliste     : 439//
nlist      : 440//
nliste     : 441//
skiplab    : 470//

debug      : 437//

oldoutput  : 482

op2str     : 700
sectpos    : 701
}


MANIFEST
{
// Value descriptors.
k.none=1; k.numb=2; k.fnlab=2
k.loc=3; k.glob=4; k.lab=5;
// 6,7,8 unknown 
k.lvloc=9; k.lvglob=10; k.lvlab=11
k.a=12; k.b=13; k.c=14
k.loc0=15; k.loc1=16; k.loc2=17; k.loc3=18; k.loc4=19
k.glob0=20; k.glob1=21; k.glob2=22

swapped=TRUE; notswapped=FALSE

// Global routine numbers.
gn.stop=2
}

// CINTCODE function codes.
MANIFEST {
f.k0   =   0
f.brk  =   2
f.code1=  13
f.lm   =  14
f.lm1  =  15
f.l0   =  16
f.fhop =  27
f.jeq  =  28
f.jeq0 =  30

f.k    =  32
f.kw   =  33
f.s0g  =  34
f.k0g  =  32

f.l0g  =  45
f.l1g  =  46
f.l2g  =  47
f.lg   =  48
f.sg   =  49
f.llg  =  50
f.ag   =  51
f.mul  =  52
f.div  =  53
f.rem  =  54
f.xor  =  55
f.sl   =  56
f.ll   =  58
f.jne  =  60
f.jne0 =  62

f.llp  =  64
f.llpw =  65
f.s0g1 =  66

f.k0g1  =  32+32
f.l0g1  =  45+32
f.l1g1  =  46+32
f.l2g1  =  47+32
f.lg1   =  48+32
f.sg1   =  49+32
f.llg1  =  50+32
f.ag1   =  51+32

f.add  =  84
f.sub  =  85
f.lsh  =  86
f.rsh  =  87
f.and  =  88
f.or   =  89
f.lll  =  90
f.jls  =  92
f.jls0 =  94

f.l    =  96
f.lw   =  97
f.s0g2 =  98

f.k0g2  =  32+64
f.l0g2  =  45+64
f.l1g2  =  46+64
f.l2g2  =  47+64
f.lg2   =  48+64
f.sg2   =  49+64
f.llg2  =  50+64
f.ag2   =  51+64

f.rv   = 116
f.rtn  = 123
f.jgr  = 124
f.jgr0 = 126

f.lp   = 128
f.lpw  = 129

f.lp0  = 128

f.sys  = 145   // Added by MR
f.swb  = 146
f.swl  = 147
f.st   = 148
f.st0  = 148
f.stp0 = 149
f.goto = 155
f.jle  = 156
f.jle0 = 158

f.sp   = 160
f.spw  = 161

f.sp0  = 160
f.s0   = 176
f.xch  = 181
f.gbyt = 182
f.pbyt = 183
f.atc  = 184
f.atb  = 185
f.j    = 186
f.jge  = 188
f.jge0 = 190

f.ap   = 192
f.apw  = 193

f.ap0  = 192

f.code2= 207
f.nop  = 208
f.a0   = 208
f.rvp0 = 211
f.st0p0= 216
f.st1p0= 218

f.a    = 224
f.aw   = 225

f.l0p0 = 224

f.neg  = 241
f.not  = 242
f.l1p0 = 240
f.l2p0 = 244
f.l3p0 = 247
f.l4p0 = 249

}
