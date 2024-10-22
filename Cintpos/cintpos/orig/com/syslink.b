/***********************************************************************
**             (C) Copyright 1982  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************
*                                                                      *
*  ######   #      #   ######   ##        ########  ##    ##  ##    ## *
* ########  ##    ##  ########  ##        ########  ###   ##  ##   ##  *
* ##         ##  ##   ##        ##           ##     ####  ##  ##  ##   *
* #######     ####    #######   ##           ##     ## ## ##  ####     *
*       ##     ##           ##  ##           ##     ##  ####  ## ##    *
*       ##     ##           ##  ##           ##     ##  ####  ##  ##   *
* ########     ##     ########  ########  ########  ##   ###  ##   ##  *
*  ######      ##      ######   ########  ########  ##    ##  ##    ## *
*                                                                      *
************************************************************************
**    Author:  Adrian Aylward                                         **
***********************************************************************/

// Modifications:
// 20 Jan 82 by BJK: Now detects empty object module files.

GET "LIBHDR"

MANIFEST
$(
errmax=10

s.word=1
s.numb=2
s.semicolon=3
s.comma=4
s.star=5
s.bra=6
s.ket=7
s.string=8
s.name=9

t.hunk=1000
t.reloc=1001
t.end=1002
t.abshunk=1003
t.absreloc=1004

secword=12345
$)

GLOBAL
$(
readdecls:150
addnamedlist:151
rdlist:152
rdname:153
rdword:154
rdnumb:155
rdsymb:156
rdinfo:157
rch:158
string:159
checkfor:160
synerror:161

scandecls:162
checkdistinct:163
lookup:164
sortlist:165
scaninfo:166
scanerror:167

loadall:168
loadlist:169
loadfile:170
loadinfo:171
preallocvec:172
loadvec:173
loadabsvec:174
genref:175
setrefs:176
append:177
list:178
newvec:179
discardvec:180
testbreak:181
error:182
writetomap:183
readword:184
writeword:185

verstream:200
mapstream:201
instream:202
outstream:203
exhausted:204
workv:205
workp:206
worksize:207
absmin:208
absmax:209
storemin:210
storemax:211
storep:212
storetop:213
memorysize:214
root:215
addrinc:216
seglist:217
initseglist:218
dcblist:219
driverlist:220
tasklist:221
inittask:222
tasktab:223
tasktabsize:224
devlist:225
devtab:226
devtabsize:227
tcbsize:228
info:229
rec.p:230
rec.l:231
symb:232
val:233
wordv:234
ch:235
chbuf:236
chcount:237
errcount:238
absvec:239
rootvec:240
tcbvec:241
reflist:242
lastsect:243
fullmap:244
$)

..

SECTION "SYSL1"

GET ""

LET start() BE
 $( LET v = VEC 63
    LET rdargs.string	= "FROM/A,TO/A,MAP/K,OPT/K"
    verstream := output()
    mapstream := 0
    instream := 0
    outstream := 0
    workv := 0
    writes("TRIPOS system linker*N")
    IF rdargs(rdargs.string, v, 63)=0 DO
       error("Bad args for key string *"%S*"", rdargs.string)
    instream := findinput(v!0)
    IF instream=0 DO
       error("Can't open %S", v!0)
    UNLESS v!2=0 DO
    $( mapstream := findoutput(v!2)
       IF mapstream=0 DO
          error("Can't open %S", v!2)
    $)
    outstream := findoutput(v!1)
    IF outstream=0 DO
       error("Can't open %S", v!1)
    fullmap := FALSE
    worksize := 6000
    UNLESS v!3=0 DO
    $( LET opts = v!3
       LET optn = opts%0
       LET i = 1
       WHILE i<=optn DO
       $( LET c = opts%i
          i := i+1
          SWITCHON capitalch(c) INTO
          $( CASE 'F':
                fullmap := TRUE
                LOOP

             CASE 'W':
                worksize := 0
                WHILE i<=optn & '0'<=opts%i<='9' DO
                $( worksize := worksize*10+opts%i-'0'
                   i := i+1 $)
                LOOP
          $)
       $)
    $)
    workv := getvec(worksize)
    IF workv=0 DO
       error("Can't get workspace")
    workp := workv+worksize
    writetomap("TRIPOS system link map of file %S*N",v!1)
    errcount := 0
    chbuf := v
    readdecls()
    IF errcount>0 DO error("Syntax error(s)")
    scandecls()
    IF errcount>0 DO error("Scan error(s)")
    loadall()
    writef("Resident code %U5 to %U5 (%U5 words)*N",
            storetop, storemax-1, storemax-storetop)
    UNLESS mapstream=0 DO
    $( selectoutput(mapstream)
       endwrite() $)
    freevec(workv)
 $)


AND readdecls() BE
 $( wordv := newvec(255/bytesperword)
    memorysize := rootnode!rtn.memsize
    absmin := rootnode
    absmax := rootnode+rtn.upb
    storemin := (absmax | 1)+1
    storemax := (memorysize<<10)-1
    root := rootnode
    addrinc := mcaddrinc
    seglist := 0
    initseglist := 0
    dcblist := 0
    driverlist := 0
    tasktabsize := 10
    devtabsize := 10
    tcbsize := tcb.upb
    tasklist := 0
    inittask := 0
    devlist := 0
    info := 0
    selectinput(instream)
    exhausted := FALSE
    FOR i = 0 TO 63 DO chbuf!i := -1
    chcount := 0
    rch()

    $( LET f = FALSE
       rec.p, rec.l := level(), reclab
 l:    rdsymb()
       IF symb=s.star DO
       $( f := TRUE
          GOTO l $)
       IF symb=s.semicolon GOTO reclab
       checkfor(s.word)
       val := findarg("SEGMENT=SEG,DCB,DRIVER,TASKTAB,*
                      *TASK,DEVTAB,DEVICE=DEV,*
                      *ABSMIN,ABSMAX,STOREMIN,STOREMAX,*
                      *MEMORYSIZE,ROOTNODE,*
                      *MCADDRINC,TCBSIZE,INFO",wordv)
       SWITCHON val INTO
       $( CASE 0:         // SEGMENT
             addnamedlist(f -> @initseglist,@seglist)
             ENDCASE

          CASE 1:         // DCB
             addnamedlist(@dcblist)
             ENDCASE

          CASE 2:         // DRIVER
             addnamedlist(@driverlist)
             ENDCASE

          CASE 3:        // TASKTAB
             tasktabsize := rdnumb()
             rdsymb()
             ENDCASE

          CASE 4:        // TASK
          $( LET a, n, p, s = 0, 0, 1000, 100
             n := rdnumb()
             $( rdword()
                val := findarg("PRIORITY=PRI,STACK,*
                               *SEGMENTS=SEGS",wordv)
                SWITCHON val INTO
                $( CASE 0:        // PRIORITY
                      p := rdnumb()
                      LOOP

                   CASE 1:        // STACK
                      s := rdnumb()
                      LOOP

                   CASE 2:        // SEGMENTS
                      a := list(4, n, p, s, rdlist())
                      BREAK

                   DEFAULT:
                      synerror("Invalid task keyword")
                $)
             $) REPEAT
             append(@tasklist, a)
             IF f DO inittask := a
             ENDCASE $)

          CASE 5:        // DEVTAB
             devtabsize := rdnumb()
             GOTO n

          CASE 6:        // DEVICE
          $( LET a, n, b, d = 0, 0, 0, 0
             n := rdnumb()
             $( rdsymb()
                UNLESS symb=s.word BREAK
                val := findarg("DCB,DRIVER",wordv)
                SWITCHON val INTO
                $( CASE 0:        // DCB
                      b := rdname()
                      LOOP

                   CASE 1:        // DRIVER
                      d := rdname()
                      rdsymb()
                      BREAK

                   DEFAULT:
                      synerror("Invalid device keyword")
                $)
             $) REPEAT
             a := list(3, n, b, d)
             append(@devlist, a)
             ENDCASE $)

          CASE 7:        // ABSMIN
             absmin := rdnumb()
             GOTO n

          CASE 8:        // ABSMAX
             absmax := rdnumb()
             GOTO n

          CASE 9:        // STOREMIN
             storemin := rdnumb()
             GOTO n

          CASE 10:       // STOREMAX
             storemax := rdnumb()
             GOTO n

          CASE 11:       // MEMORYSIZE
             memorysize := rdnumb()
             GOTO n

          CASE 12:       // ROOTNODE
             root := rdnumb()
             GOTO n

          CASE 13:       // MCADDRINC
             addrinc := rdnumb()
             GOTO n

          CASE 14:       // TCBSIZE
             tcbsize := rdnumb()
             GOTO n

          CASE 15:       // INFO
             info := rdinfo()
             GOTO n

          DEFAULT:
             synerror("Invalid keyword")

      n:     rdsymb()
       $)
       checkfor(s.semicolon)
 reclab:
    $) REPEATUNTIL exhausted
    append(@seglist, initseglist)
    endread()
    instream := 0
 $)


AND addnamedlist(lvlist) BE
 $( LET a = list(3, rdname(), 0, 0)
    a!2 := rdlist()
    append(lvlist, a)
 $)


AND rdlist() = VALOF
 $( LET a = 0
    LET e = @a
    rdsymb()
    UNLESS symb=s.semicolon DO
    $( checkfor(s.name)
       !e := list(1, string(wordv))
       e := !e
       rdsymb()
       UNLESS symb=s.comma BREAK
       rdsymb()
    $) REPEAT
    RESULTIS a
 $)


AND rdname() = VALOF
 $( rdsymb()
    checkfor(s.name)
    RESULTIS string(wordv)
 $)


AND rdword() = VALOF
 $( rdsymb()
    checkfor(s.word)
    RESULTIS wordv $)


AND rdnumb() = VALOF
 $( rdsymb()
    checkfor(s.numb)
    RESULTIS val $)


AND rdsymb() BE
 $( LET i = 0
    LET rad = 10
    LET neg = FALSE
    LET dig() = '0'<=ch<='9' -> ch-'0',
                'A'<=ch<='F' -> ch-'A'+10, 100
    testbreak()
l:  SWITCHON ch INTO
    $( CASE '*S': CASE '*T': CASE '*N':
          rch()
          GOTO l

       CASE '|':
          rch() REPEATUNTIL ch='*N' | ch=endstreamch
          GOTO l

       CASE endstreamch:
          exhausted := TRUE
       CASE ';':
          symb := s.semicolon
          ENDCASE

       CASE ',':
          symb := s.comma
          ENDCASE

       CASE '**':
          symb := s.star
          ENDCASE

       CASE '(':
          symb := s.bra
          ENDCASE

       CASE ')':
          symb := s.ket
          ENDCASE

       CASE '*"':
          symb := s.string
          $( rch()
             IF ch='*N' DO synerror("String contains newline")
             IF ch='*"' BREAK
             IF ch='**' DO
             $( rch()
                IF ch='E' DO ch := '*E'
                IF ch='N' DO ch := '*N'
             $)
             i := i+1
             IF i>255 DO synerror("String too long")
             wordv%i := ch
          $) REPEAT
          wordv%0 := i
          ENDCASE

       CASE '-':
          neg := TRUE
          rch()
          UNLESS ch='#' GOTO d
       CASE '#':
          rad := 8
          rch()
          IF capitalch(ch)='X' DO
          $( rad := 16
             rch()
          $)

   d:  DEFAULT:
          IF dig()<rad DO
          $( symb, val := s.numb, 0
             $( val := val*rad+dig()
                rch() $) REPEATWHILE dig()<rad
             IF neg DO val := -val
             RETURN
          $)
          IF rad>10 | neg DO synerror("Bad number")
          symb := s.word
          $( i := i+1
             IF i>255 DO synerror("Word too long")
             wordv%i := ch
             rch()
          $) REPEATUNTIL ch=endstreamch |
                   ch='*S' | ch='*T' | ch='*N' |
                   ch=';' | ch=',' | ch='|' |
                   ch='(' | ch=')' | ch='"'
          wordv%0 := i
          RETURN
    $)
    rch()
 $)


AND rdinfo() = VALOF
 $( LET v = newvec(2)
    rdsymb()
    v!0 := 0
    v!1 := symb
    SWITCHON symb INTO
    $( CASE s.word: CASE s.string:
          v!2 := string(wordv)
          ENDCASE

       CASE s.numb:
          v!2 := val
          ENDCASE

       CASE s.bra:
       $( LET a = v+2
          $( !a := rdinfo()
             a := !a
             rdsymb()
          $) REPEATWHILE symb=s.comma
          checkfor(s.ket)
          ENDCASE
       $)

       DEFAULT:
          synerror("Invalid info term")
    $)
    RESULTIS v
 $)


AND rch() BE
 $( ch := rdch()
    chcount := chcount+1
    chbuf!(chcount&63) := ch
 $)


AND string(s) = VALOF
 $( LET size = s%0/bytesperword
    LET v = newvec(size)
    FOR i = 0 TO size DO v!i := s!i
    RESULTIS v
 $)


AND checkfor(s) BE
    UNLESS s=symb |
          s=s.name & (symb=s.word | symb=s.string) DO
       synerror("%S expected",
           s=s.semicolon -> "';'",
           s=s.ket -> "')'",
           s=s.numb -> "Number",
           s=s.word -> "Word",
           s=s.name -> "Name", "?" )


AND synerror(f, a, b) BE
 $( writes("Error - ")
    writef(f, a, b)
    writes("*NNear ... ")
    FOR i = -63 TO 0 DO
    $( LET c = chbuf!(chcount+i & 63)
       IF c>=0 DO wrch(c) $)
    newline()
    testbreak()
    errcount := errcount+1
    IF errcount>=errmax DO error("Too many errors")
    UNTIL symb=s.semicolon DO rdsymb()
    longjump(rec.p, rec.l)
 $)

..

SECTION "SYSL2"

GET ""

LET scandecls() BE
 $( LET a = 0
    checkdistinct(seglist, "Segment")
    checkdistinct(driverlist, "Driver")
    checkdistinct(dcblist, "DCB")
    UNLESS 0<tasktabsize<1000 DO
       scanerror("Task table size %N invalid",
                  tasktabsize)
    tasktab := newvec(tasktabsize)
    FOR i = 1 TO tasktabsize DO
       tasktab!i := 0
    tasktab!0 := tasktabsize
    a := tasklist
    UNTIL a=0 DO
    $( LET n, p, l = a!1, a!2, a!4
       TEST 0<n<=tasktabsize THEN
       $( UNLESS tasktab!n=0 DO
             scanerror("Task %N declared twice", n)
          tasktab!n := a
          UNLESS p>0 DO
             scanerror("Task %N invalid priority %N",
                         n, p)
          UNTIL l=0 DO
          $( LET s = lookup(seglist, l!1)
             IF s=0 DO
                scanerror("Segment %S not declared", l!1)
             l!1 := s
             l := !l
          $)
       $)
       ELSE scanerror("Task ID %N invalid", n)
       a := !a
    $)
    sortlist(@tasklist)
    UNLESS 0<devtabsize<1000 DO
       scanerror("Device table size %N invalid",
                   devtabsize)
    devtab := newvec(devtabsize)
    FOR i = 1 TO devtabsize DO
       devtab!i := 0
    devtab!0 := devtabsize
    a := devlist
    UNTIL a=0 DO
    $( LET n, b, d = a!1, a!2, a!3
       TEST 1<-n<=devtabsize THEN
       $( UNLESS devtab!-n=0 DO
             scanerror("Device %N declared twice", n)
          devtab!-n := a
          TEST b=0 THEN
             scanerror("Device %N has no DCB", n)
          ELSE
          $( a!2 := lookup(dcblist, b)
             IF a!2=0 DO
                scanerror("DCB %S not declared", b)
          $)
          UNLESS d=0 DO
          $( a!3 := lookup(driverlist, d)
             IF a!3=0 DO
                scanerror("Driver %S not declared", d)
          $)
       $)
       ELSE scanerror("Device ID %N invalid", n)
       a := !a
    $)
    UNLESS info=0 DO scaninfo(info)
 $)


AND checkdistinct(list, str) BE
    UNTIL list=0 DO
    $( UNLESS lookup(!list, list!1)=0 DO
          scanerror("%S %S declared twice", str, list!1)
       list := !list
    $)


AND lookup(list, name) = VALOF
 $( IF list=0 RESULTIS 0
    IF compstring(list!1, name) = 0 RESULTIS list
    list := !list
 $) REPEAT


AND sortlist(lvlist) BE
    UNTIL !lvlist=0 DO
    $( LET maxp, lvmaxt = 0, 0
       LET t = lvlist
       UNTIL !t=0 DO
       $( IF (!t)!2>maxp DO
             lvmaxt, maxp := t, (!t)!2
          t := !t
       $)
       t := !lvmaxt
       !lvmaxt := !t
       !t := !lvlist
       !lvlist := t
       lvlist := t
    $)


AND scaninfo(v) BE
 $( LET s = v!1
    IF s=s.word DO
    $( LET n = v!2
       LET e = lookup(seglist, n)
       IF e=0 DO e := lookup(driverlist, n)
       IF e=0 DO scanerror("Name %S is not a resident*
                           *segment or driver", n)
       v!2 := e
    $)
    IF s=s.bra DO scaninfo(v!2)
    v := v!0
 $) REPEATUNTIL v=0


AND scanerror(f, a, b) BE
 $( writes("Error - ")
    writef(f, a, b)
    newline()
    testbreak()
    errcount := errcount+1
    IF errcount>=errmax DO error("Too many errors")
 $)

..

SECTION "SYSL3"

GET ""

LET loadall() BE
 $( LET a = 0
    LET previoustcb = 0
    absvec := newvec(absmax-absmin)
    rootvec := absvec-absmin+root
    FOR i = 0 TO absmax-absmin DO absvec!i := 0
    storep := storemax-1
    rewrite(storep, 0)
    reflist := 0
    writetomap("Segments*N")
    a := seglist
    UNTIL a = initseglist DO
    $( a!3 := loadlist(a!2)
       a := !a $)
    writetomap("*NTasks*N")
    tcbvec := newvec(tcbsize)
    FOR i = 0 TO tcbsize DO tcbvec!i := 0
    previoustcb := 0
    a := tasklist
    UNTIL a=0 DO
    $( LET n, p, s, l = a!1, a!2, a!3, a!4
       LET sl, ss, sv = l, 0, 0
       UNTIL l=0 DO
       $( ss := ss+1
          l := !l $)
       l, sv := sl, newvec(ss)
       sv!0 := ss
       FOR i = 1 TO ss DO
       $( sv!i := (l!1)!3
          l := !l $)
       l := sl
       sl := loadvec(sv, ss)
       discardvec(sv, ss)
       FOR i = 1 TO ss DO
       $( IF (l!1)!3=0 DO genref(sl+i, l!1)
          l := !l $)
       tcbvec!tcb.taskid := n
       tcbvec!tcb.pri := p
       tcbvec!tcb.state := state.dead
       tcbvec!tcb.stsiz := s
       tcbvec!tcb.seglist := sl
       l := loadvec(tcbvec, tcbsize)
       writetomap("Task %I2 TCB %U5 seglist %U5",n,l,sl)
       tasktab!n := l
       TEST previoustcb=0
         THEN rootvec!rtn.tcblist := l
         ELSE rewrite(previoustcb, l)
       previoustcb := l
       IF a=inittask DO rootvec!rtn.crntask := l
       a := !a
    $)
    tasktab := loadvec(tasktab, tasktabsize)
    writetomap("*NTask table %U5*N", tasktab)
    writetomap("Device drivers*N")
    a := driverlist
    UNTIL a=0 DO
    $( a!3 := loadlist(a!2, 0)
       a := !a $)
    writetomap("*NDevices*N")
    a := devlist
    UNTIL a=0 DO
    $( LET n, b, d = a!1, a!2, a!3
       LET d3 = d=0 -> 0, d!3
       writetomap("Device %I2", n)
       devtab!-n := loadlist(b!2)
       TEST lastsect=0 THEN
          devtab!-n := d3
       ELSE
          rewrite(lastsect, d3)
       a := !a $)
    devtab := loadvec(devtab, devtabsize)
    writetomap("*NDevice table %U5", devtab)
    UNLESS info=0 DO info := loadinfo(info)
    storetop := storep
    UNLESS initseglist=0 DO
       writetomap("*NInitialisation segments*N")
    a := initseglist
    UNTIL a=0 DO
    $( a!3 := loadlist(a!2, 0)
       a := !a $)
    setrefs()
    rewrite(storemin, storep-storemin+1)
    rootvec!rtn.tasktab := tasktab
    rootvec!rtn.devtab := devtab
    rootvec!rtn.blklist := storemin
    rootvec!rtn.memsize := memorysize
    rootvec!rtn.info := info
    loadabsvec(absmin, absvec, absmax-absmin)
    writetomap("*NAbs  store %U5 - %U5",
           absmin, absmax)
    writetomap("Free store %U5 - %U5 (%U5 words)",
           storemin, storetop-1, storetop-storemin)
    UNLESS inittask=0 DO
       writetomap("Initial task %N", inittask!1)
    selectoutput(outstream)
    writeword(t.end)
    endwrite()
    outstream := 0
    selectoutput(verstream)
 $)


AND loadlist(list) = VALOF
 $( LET first = 0
    lastsect := 0
    UNTIL list=0 DO
    $( LET seg = loadfile(list!1)
       IF first=0 DO first := seg
       list := !list
    $)
    RESULTIS first
 $)


AND loadfile(file) = VALOF
 $( LET first, tail = lastsect, lastsect
    LET relvec, reloc = 0, 0
    LET base, size = 0, 0
    LET lasthunk = t.end
    LET segtop = storep-1
    LET nohunks	= TRUE  // Unset when a hunk is read
    instream := findinput(file)
    IF instream=0 DO
       error("Can*'t open %S", file)
    IF fullmap DO
       writetomap("File %S", file)
    selectinput(instream)
    exhausted := FALSE

    $( LET type = readword()
       IF exhausted BREAK
       SWITCHON type INTO
       $( CASE t.reloc:
             UNLESS lasthunk=t.hunk GOTO err
          $( LET n = readword()
             IF exhausted | reloc=0 GOTO err
             FOR i = 1 TO n DO
             $( LET a = readword()+1
                IF exhausted GOTO err
                UNLESS 0<a<=size GOTO err
                relvec!a := relvec!a+reloc
             $)
             LOOP $)

          CASE t.absreloc:
             UNLESS lasthunk=t.abshunk GOTO err
          $( LET n = readword()
             IF exhausted | reloc=0 GOTO err
             FOR i = 1 TO n DO
             $( LET a = readword()-base
                IF exhausted GOTO err
                UNLESS 0<=a<=size GOTO err
                relvec!a := relvec!a+reloc
             $)
             LOOP $)

          CASE t.hunk: CASE t.abshunk:
             nohunks	:= FALSE  // and drop through ...
          CASE t.end:
             IF lasthunk=t.hunk DO
             $( IF fullmap TEST relvec!2=secword &
                                      (relvec+3)%0=17
                   THEN
                      writetomap("Section %S %U5 - %U5",
                         relvec+3, base+1, base+size)
                   ELSE
                      writetomap("Hunk %U5 - %U5",
                         base, base+size)
                loadvec(relvec, size)
                discardvec(relvec, size)
             $)
             IF lasthunk=t.abshunk DO
             $( LET f = FALSE
                IF fullmap DO
                   writetomap("Abshunk %U5 - %U5",
                         base, base+size)
                FOR i = 0 TO size DO
                $( LET absloc = base+i
                   TEST absloc-absmin>=0 &
                        absloc-absmax<=0
                   THEN
                      UNLESS relvec!i=0 DO
                         absvec!(absloc-absmin):=relvec!i
                   ELSE
                      f := TRUE
                $)
                IF f DO loadabsvec(base, relvec, size)
                discardvec(relvec, size)
             $)
             lasthunk := type
             IF type=t.end DO
             $( reloc := 0
                LOOP $)
             IF type=t.hunk DO
             $( size := readword()
                IF exhausted | size<0 GOTO err
                base := preallocvec(size)
                reloc := (base+1)*addrinc
                UNLESS lastsect=0 DO
                   rewrite(lastsect, base)
                IF first=tail DO first := base
                lastsect := base
                relvec := newvec(size)
                relvec!0 := 0
                UNLESS ABS readwords(relvec+1,size)=size
                   GOTO err
                LOOP $)
             IF type=t.abshunk DO
             $( base := readword()
                IF exhausted GOTO err
                size := readword()-1
                IF exhausted | size<0 GOTO err
                relvec := newvec(size)
                UNLESS ABS readwords(relvec,size+1)=size+1
                   GOTO err
                LOOP $)

          DEFAULT:
             GOTO err
       $)
    $) REPEAT
    IF nohunks THEN error("No code in object file %S", file)
    UNLESS lasthunk=t.end GOTO err
    endread()
    instream := 0
    UNLESS fullmap | segtop=storep-1 DO
       writetomap("File %S %U5 - %U5",
          file, lastsect, segtop)
    RESULTIS first

err:error("Format error in object file %S", file)
 $)


AND loadinfo(v) = VALOF
 $( LET s = v!1
    LET n = v!2
    IF s=s.numb   RESULTIS n
    IF s=s.word   RESULTIS n!3
    IF s=s.string RESULTIS loadvec(n, n%0/bytesperword)
    IF s=s.bra DO
    $( LET z = -1
       LET p = n
       LET w = 0
       $( z := z+1
          p := !p
       $) REPEATUNTIL p=0
       w := newvec(z)
       p := n
       FOR i = 0 TO z DO
       $( w!i := loadinfo(p)
          p := !p
       $)
       RESULTIS loadvec(w, z)
    $)
 $)


AND preallocvec(size) = VALOF
 $( LET blksize = (size+1 | 1)+1
    RESULTIS storep-blksize+1 $)


AND loadvec(v, size) = VALOF
 $( LET blksize = (size+1 | 1)+1
    storep := storep-blksize
    testbreak()
    selectoutput(outstream)
    writeword(t.abshunk)
    writeword(storep)
    writeword(size+2)
    writeword(blksize)
    writewords(v, size+1)
    selectoutput(verstream)
    RESULTIS storep+1
 $)


AND loadabsvec(base, v, size) BE
 $( testbreak()
    selectoutput(outstream)
    writeword(t.abshunk)
    writeword(base)
    writeword(size+1)
    writewords(v, size+1)
    selectoutput(verstream)
 $)


AND rewrite(loc, val) BE
    loadabsvec(loc, @val, 0)


AND genref(loc, seg) BE
    append(@reflist, list(2, loc, seg))


AND setrefs() BE
 $( LET l = reflist
    UNTIL l=0 DO
    $( rewrite(l!1, (l!2)!3)
       l := !l $)
 $)


AND append(lvlist, list) BE
 $( UNTIL !lvlist=0 DO lvlist := !lvlist
    !lvlist := list $)


AND list(n, a,b,c,d,e) = VALOF
 $( LET p = @n
    LET v = newvec(n)
    v!0 := 0
    FOR i = 1 TO n DO v!i := p!i
    RESULTIS v $)


AND newvec(size) = VALOF
 $( workp := workp-size-1
    IF workp-workv<0 DO error("Run out of workspace")
    RESULTIS workp
 $)


AND discardvec(v, size) BE
    IF workp=v DO workp := workp+size+1


AND testbreak() BE
    IF testflags(1) DO error("BREAK")


AND error(f, a, b) BE
 $( selectoutput(verstream)
    writef(f, a, b)
    writes(" - linking aborted*N")
    UNLESS instream=0 DO
    $( selectinput(instream)
       endread() $)
    UNLESS outstream=0 DO
    $( selectoutput(outstream)
       endwrite() $)
    UNLESS mapstream=0 DO
    $( selectoutput(mapstream)
        endwrite() $)
    UNLESS workv=0 DO freevec(workv)
    stop(20)
 $)


AND writetomap(f, a, b, c) BE
    UNLESS mapstream=0 DO
    $( LET o = output()
       selectoutput(mapstream)
       writef(f, a, b, c)
       newline()
       selectoutput(o)
    $)


AND readword() = VALOF
 $( LET w = 0
    UNLESS ABS readwords(@w, 1)=1 DO
       exhausted := TRUE
    RESULTIS w
 $)


AND writeword(w) BE
    writewords(@w, 1)
