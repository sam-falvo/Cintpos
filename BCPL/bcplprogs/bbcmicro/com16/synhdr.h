MANIFEST
$(
// selectors
h1=0; h2=1; h3=2; h4=3; h5=4; h6=5

// basic symbols
s.be=89; s.end=90; s.lsect=91; s.rsect=92
s.get=93; s.into=98
s.to=99; s.by=100; s.do=101; s.or=102
s.vec=103; s.lparen=105; s.rparen=106

// AE tree operators
s.number=1; s.name=2; s.string=3
s.valof=6; s.lv=7; s.vecap=9
s.byteap=28
s.cond=37; s.comma=38; s.table=39
s.and=40; s.valdef=41; s.vecdef=42
s.commalist=43; s.fndef=44; s.rtdef=45
s.ass=50; s.resultis=53; s.colon=54
s.test=55; s.for=56; s.if=57; s.unless=58
s.while=59; s.until=60; s.repeat=61
s.repeatwhile=62; s.repeatuntil=63
s.loop=65; s.break=66
s.endcase=69; s.case=71; s.default=72
s.semicolonlist=73; s.let=74; s.manifest=75; s.static=79
s.semicolon=97

// AE tree and OCODE operators
s.true=4; s.false=5
s.rv=8; s.fnap=10
s.mult=11; s.div=12; s.rem=13; s.plus=14
s.minus=15; s.query=16; s.neg=17; s.abs=19
s.eq=20; s.ne=21; s.ls=22; s.gr=23; s.le=24; s.ge=25
s.not=30; s.lshift=31; s.rshift=32; s.logand=33
s.logor=34; s.eqv=35; s.neqv=36
s.needs=48; s.section=49
s.rtap=51; s.goto=52
s.return=67; s.finish=68
s.switchon=70; s.global=76

s.setcond=107
s.lcond=108
s.rcond=109

nametablesize=47
getmax=20
wordmax=255/BYTESPERWORD+1
$)

GLOBAL
$(

nextsymb:400

lookupword:401
declsyswords:402

rch:403
wrchbuf:404
rdtag:405
performget:406
readnumber:407
rdstrch:408

newvec:209
list1:210
list2:211
list3:212
list4:213
list5:214
list6:215
synreport:216

rdblockbody:217
rdseq:218
rdcdefs:219
rdsect:220
rnamelist:221
rname:222
ignore:223
checkfor:224

rbexp:225
rexp:226
rexplist:227
rdef:228

rbcom:229
rcom:230
makelist:231


sourcestream:286
//ch:287
linecount:288

treep:292
treeq:293

nextsymb:400

lookupword:401
declsyswords:402

rch:403
wrchbuf:404
rdtag:405
performget:406
readnumber:407
rdstrch:408

newvec:409
list1:410
list2:411
list3:412
list4:413
list5:414
list6:415
synreport:416

rdblockbody:417
rdseq:418
rdcdefs:419
rdsect:420
rnamelist:421
rname:422
ignore:423
checkfor:424

rbexp:425
rexp:426
rexplist:427
rdef:428

rbcom:429
rcom:430
makelist:431
compstring:432


symb:450
decval:451
wordnode:452
wordv:267//
chbuf:269//
chcount:455
nlpending:456
nulltag:457
getv:266//
getp:459
nametable:265//
rec.p:461
rec.l:462
skipnode:463
listp:464
blklist:466
$)

