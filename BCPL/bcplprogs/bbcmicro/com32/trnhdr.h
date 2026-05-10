MANIFEST
$(
// selectors
h1=0; h2=1; h3=2; h4=3; h5=4; h6=5

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

// OCODE operators
s.lf=39; s.fnlab=39
s.lp=40; s.lg=41; s.ln=42; s.lstr=43; s.ll=44
s.llp=45; s.llg=46; s.lll=47
s.local=77; s.label=78
s.sp=80; s.sg=81; s.sl=82; s.stind=83
s.jump=85; s.jt=86; s.jf=87; s.endfor=88; s.xlab=89
s.lab=90; s.stack=91; s.store=92; s.rstack=93
s.entry=94; s.save=95; s.fnrn=96; s.rtrn=97
s.res=98; s.datalab=100; s.iteml=101; s.itemn=102
s.endproc=103; s.getbyte=120; s.putbyte=121
$)


GLOBAL
$(
nextparam:300
transreport:301

trans:302

declnames:303
decldyn:304
declstat:305
decllabels:306
checkdistinct:307
addname:308
cellwithname:309
scanlabels:310
transdef:311
transdyndefs:312
transstatdefs:313
statdefs:314

jumpcond:315
transswitch:316
transfor:317

load:318
loadlv:319
loadzero:320
loadlist:321

evalconst:322
assign:323
transname:324

complab:325
compentry:326
compdatalab:327
compjump:328
out1:329
out2:330
out3:333
outstring:336
wrpn:339
endocode:340
wrc:341

paramnumber:350
comcount:351
ssp:352
vecssp:353
currentbranch:354
//dvec:270//
dvece:357
dvecp:358
//dvect:271
//globdecl:272
globdecls:361
//globdeclt:273
//casek:274
//casel:275
casep:365
//caset:366
caseb:367
breaklabel:368
resultlabel:369
defaultlabel:370
endcaselabel:371
looplabel:372
ocount:373
$)

