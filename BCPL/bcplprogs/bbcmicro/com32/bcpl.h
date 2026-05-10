/*
This is the shared header file for the BBC BCPL compiler.
It is used by bcpl.b, bcplargs.b bcplsyn.b bcpltrn.b and bcplccg.b
*/

GLOBAL
$(
rc:250
spacev:251
verstream:252
ocodeinstream:454  // for bcplccg
ocodeoutstream:253  // for bcplccg
codestream:254
maxoption:255//

err.p:256
err.l:257
ocodefile:258

blk:259

// CG options
cgworksize:260//
cgworkspace:261//
naming:262//
callcounting:263
profcounting:264
nametable:265//
getv:266//
wordv:267//
gett:268//
chbuf:269//

// SYN - TRN globals
dvec:270
dvect:271
globdecl:272
globdeclt:273
casek:274
casel:275
caset:276

treesize:280
declsize:281
printtree:282
charcode:283
transchars:284
savespacesize:285
sourcestream:286
ch:287
linenumber:288
linecount:288
reportcount:289
errcount:290
errvec:291
blkt:292
blkp:293
treevec:294
zeronode:295
smallnumber:296

sectname: 297      // Only used in the 32-bit version
sectionlen: 298

bcplargs: 299   // MR 25/11/2019
bcplsys: 300
bcpltrn: 301
bcplcg: 302
$)

MANIFEST
$(
reportmax=10
$)

