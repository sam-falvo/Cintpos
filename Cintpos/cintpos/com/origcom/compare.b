// ******* Not for general release *******

    //
    //     COMPARE   written by Nick Maclaren
    //               transferred to CAP by Martyn Johnson
    //               and from CAP to TRIPOS by Brian Knight

    //     This program is designed to check two sequential character
    // files for altered, inserted or deleted lines.  At present it is
    // written purely in BCPL.
    //     The two modes produce either a printout of the differences
    // between the two files (difference mode) or edits necessary to
    // change the first file into the second (edit mode).
    //     Return codes are as follows:
    //         0 - the files are identical.
    //         5 - the files are not identical.
    //        10 - no match could be found.
    //        15 - the parameters are invalid or insufficient stack.
    //        20 - a file cannot be opened.

SECTION "COMPARE"

GET "LIBHDR"

MANIFEST $( dataposn = 1 $)

GLOBAL
    $(
    length : ug + 0; verify : ug + 1; mode : ug + 2; print : ug + 3
    mismatch : ug + 4; restore : ug + 5
    window : ug + 6; terminate : ug + 7
    bot1 : ug + 8; bot2 : ug + 9; top1 : ug + 10
    top2 : ug + 11; base1 : ug + 12
    base2 : ug + 13; limit1 : ug + 14; limit2 : ug + 15
    line1 : ug + 16; line2 : ug + 17
    eof1 : ug + 18; eof2 : ug + 19; stream1 : ug + 20
    stream2 : ug + 21; words : ug + 22
    linesper : ug + 23; printed : ug + 24
    current : ug + 25; equal : ug + 26
    brief : ug + 27
    file1 : ug + 28
    file2 : ug + 29
    parm  : ug + 30
    buffer : ug + 31
    tostream : ug + 32
    $)



LET start() BE $(start
    //
    // This decodes the parameters and prints the heading.
    // Parameter formats and values are:
    //     1) Spaces are permitted anywhere except within an integer,
    // and commas may be used to separate parameters.
    //     2) D or E.  The mode, for differences or edits respectively.
    //     3) B.  Brief output.  Neither the option reflection line
    // nor the completion messages will be printed.
    //     4) Zch.  Set terminator as in edit.  Only allowed in mode E.
    //     5) L+ or L-.  Check or not for the equality of line lengths.
    //     6) V+ or V-.  Verify or not in normal format.  Not allowed in
    // mode E.
    //     7) X+ or X-.  Verify or not in hexadecimal.  Not allowed in
    // mode E.
    //     8) Pn.  Set a printer limit of n to prevent excessive
    // numbers of differences.
    //     9) Wn.  Truncate all lines after n (> 0) data characters.
    //    10) Mn.  Search for up to n mismatching lines.
    //    11) Rn.  Require at most n (> 0) lines to restore equality
    // after a mismatch.
    //     Defaults:  D L- V+ X- W132 R5 Pmaxint; M is set to the
    // largest value that will fit in the available store.
    //     Notes: 1) record lengths will be verified only with L+V+X-.
    //            2) The algorithm will allow m1 lines of file1 to
    //        differ from m2 lines of file2 if there are at least
    //        min(R,max(m1,m2)+1) identical lines immediately after.
    //
    LET l,m,n,hex,temp = ?,?,0,' ',?
    LET parmf = VEC 50
    LET rdargs.string = "FILE1/A,FILE2/A,TO/K,OPT/K"
    LET argv = VEC 40

    LET equal(s1, s2, l1, l2) = VALOF
    $(
        LET l = l1<l2 -> l1, l2
        FOR i = 0 TO l-1 DO
            IF s1%i NE s2%i THEN RESULTIS FALSE
        IF l1=l2 THEN RESULTIS TRUE
        TEST l1>l2
        THEN FOR i = l TO l1-1 DO
            IF s1%i NE ' ' THEN RESULTIS FALSE
        ELSE FOR i = l TO l2-1 DO
            IF s2%i NE ' ' THEN RESULTIS FALSE
        RESULTIS TRUE
    $)

    IF rdargs(rdargs.string, argv, 40) = 0
    THEN
        $(
        writef("COMPARE: bad args for key string *"%S*"*N", rdargs.string)
        stop(15)
        $)
    parm := (argv!3 = 0 -> "", argv!3)
    file1, file2 := argv!0, argv!1

    tostream := 0
    UNLESS argv!2 = 0
    THEN
        $(
        tostream := findoutput(argv!2)
        IF tostream = 0
        THEN
            $(
            writef("Can't open %S*N", argv!2)
            stop(20)
            $)

        selectoutput(tostream)
        $)



    unpackstring(parm,parmf); parmf!(parmf!0+1) := '.'
    // Initialise everything to unset.
    length := ' '; verify := ' '; mode := ' '; print := -1
    mismatch := -1; window := -1; restore := -1; terminate := ' '
    brief := FALSE
    // The parm field loop.
    WHILE n < parmf!0 DO
        $(1 n := n+1
        SWITCHON parmf!n INTO $(2
    // Various characters and parameters.
CASE ' ': CASE ',': LOOP
DEFAULT:    GOTO parmerr
CASE 'D': CASE 'd': IF mode \= ' ' THEN GOTO parmerr
            mode := 'D'; LOOP
CASE 'B': CASE 'b': IF brief THEN GOTO parmerr; brief := TRUE; LOOP
CASE 'E': CASE 'e': IF mode \= ' ' THEN GOTO parmerr
            mode := 'E'; LOOP
CASE 'Z': CASE 'z': IF terminate \= ' ' THEN GOTO parmerr
            n := n+1 REPEATWHILE parmf!n = ' '
            IF n > parmf!0 THEN GOTO parmerr
            terminate := parmf!n; LOOP
    // On or off parameters.
CASE 'L': CASE 'l': temp := @length; GOTO alpha
CASE 'V': CASE 'v': temp := @verify; GOTO alpha
CASE 'X': CASE 'x': temp := @hex
alpha:      IF !temp \= ' ' THEN GOTO parmerr
            n := n+1 REPEATWHILE parmf!n = ' '
            TEST parmf!n = '+' | parmf!n = '-'
            THEN $(3 !temp := parmf!n; LOOP $)3
            ELSE GOTO parmerr
    // Integer parameters.
CASE 'P': CASE 'p': temp := @print; GOTO beta
CASE 'M': CASE 'm': temp := @mismatch; GOTO beta
CASE 'R': CASE 'r': temp := @restore; GOTO beta
CASE 'W': CASE 'w': temp := @window
beta:       IF !temp >= 0 THEN GOTO parmerr
            n := n+1 REPEATWHILE parmf!n = ' '
            l := parmerr; m := 0
    // Not really machine dependent.
            $(4 UNLESS '0' <= parmf!n <= '9' THEN GOTO l
                m := 10*m+(parmf!n-'0')
                l := gamma; n := n+1
            $)4 REPEAT
gamma:      !temp := m; n := n-1; LOOP
        $)2 $)1
    // Check values and insert defaults.
    IF length = ' ' THEN length := '-'
    IF mode = ' ' THEN mode := 'D'
    TEST mode = 'E'
    THEN $(5 IF verify \= ' ' | hex \= ' ' THEN GOTO parmerr
        TEST terminate \= ' '
    // Set terminator in edit.
        THEN $(17 writef("Z%C  ",terminate)
            IF brief THEN newline() $)17
        ELSE terminate := 'Z' $)5
    ELSE IF terminate \= ' ' THEN GOTO parmerr
    IF restore = 0 THEN GOTO parmerr
    IF mismatch < 0 THEN mismatch := 25
    IF restore < 0 THEN restore := 5
    IF window = 0 THEN GOTO parmerr
    IF window < 0 THEN window := 132
    words := window/bytesperword+dataposn

    buffer := getvec((mismatch+restore)*2*(words+1))
    IF buffer = 0
    THEN
        $(
        writes("Insufficient store for COMPARE*N")
        stop(20)
        $)


    IF print < 0 THEN print := (NOT 0) >> 1  || Maxint
    IF verify = ' ' THEN verify := '+'
    IF hex = ' ' THEN hex := '-'
    // Print heading if required.
    IF NOT brief
    THEN $(16 IF mode = 'E' THEN $(14 wrch('|'); wrch(' ') $)14
        writef("Options in effect for compare:  %C L%C ",mode,length)
        TEST mode = 'D'
        THEN writef("V%C X%C ",verify,hex)
        ELSE writef("Z%C ",terminate)
        writef("M%N R%N W%N P%N*N",mismatch,restore,window,print) $)16
    // Set up constants and start work.
    verify := (verify='+'->2,1)+(hex='+'->2,0)
    linesper := verify-(verify>2->2,1)+(verify=2&length='+'->1,0)

    main(buffer)

parmerr: writef("Invalid parameters:  %S*N",parm); quit(15)
$)start



AND main(buff) BE $(main
    //
    // This controls the comparison.  Its argument is a vector for
    // workspace.  The algorithm is to scan until a mismatch is found.
    // Then to search for identical lines between the files in such a
    // way as to minimise firstly the maximum of the two numbers of
    // differing lines (before the identical ones) and secondly the
    // difference between the numbers.  In case of a tie, file2 will
    // have more lines differing than file1.  End of file is treated as a
    // block of RESTORE identical lines which match no actual lines.
    // The number of identical lines required to restore the matched
    // state is min(RESTORE,max(number of differing lines in file1,
    // number of differing lines in file2)+1).  If no match is found by
    // the time the numbers of differing lines reaches MISMATCH, an
    // error exit is taken.
    //
    LET t1,t2 = ?,?
    LET errflag = FALSE
    t1 := mismatch+restore
    // Allocate space and assign pointers.
    bot1 := buff; top1 := bot1; line1 := 0
    bot2 := bot1+t1; top2 := bot2; line2 := 0
    base1 := bot2+t1; limit1 := base1+t1*words
    base2 := limit1; limit2 := base2+t1*words
    eof1 := FALSE; eof2 := FALSE; stream1 := 0; stream2 := 0
    current := 0; printed := 0
    pushup(1,mismatch+restore); pushup(2,mismatch+restore)
    // The comparison loop.
alpha: IF testflags(1) THEN quit(0)
       t1 := check()
    IF t1 \= 0
    THEN $(2 pushup(1,t1); pushup(2,t1); GOTO alpha $)2
    IF top1 <= bot1 & top2 <= bot2 THEN GOTO beta
    // A mismatch is found.
    errflag := TRUE
    FOR n = 1 TO mismatch DO $(3
        t1 := (n<restore->n+1,restore)
        IF compare(bot1+n,bot2+n,t1)
        THEN $(4 printer(n,n); pushup(1,n+t1)
            pushup(2,n+t1); GOTO alpha $)4
    // Try shifting up and down.
        FOR m = n-1 TO 0 BY -1 DO $(5
            IF compare(bot1+m,bot2+n,t1)
            THEN $(6 printer(m,n); pushup(1,m+t1)
                pushup(2,n+t1); GOTO alpha $)6
            IF compare(bot1+n,bot2+m,t1)
            THEN $(7 printer(n,m); pushup(1,n+t1)
                pushup(2,m+t1); GOTO alpha
    $)7 $)5 $)3
    // No match can be found.
    TEST mode = 'E' THEN writes("Q8  | ") ELSE newline()
    writef("After line %N of *'%S*' and line %N of *'%S*' no match could be found.*N",
        line1,file1,line2,file2)
    quit(10)
    // More or less successful end.
beta: IF NOT brief
    THEN $(10 TEST mode = 'E'
        THEN writes("| ")
        ELSE $(9 newline()
            IF NOT errflag THEN writes("No differences found.*N*N") $)9
        writef("Comparison completed after line %N of *'%S*'*
               * and line %N of *'%S*'.*N",
               line1,file1,line2,file2) $)10
    quit(errflag->5,0)
$)main



AND pushup(index,num) BE $(pushup
    //
    // This maintains the buffers.  Its arguments are an index to which
    // file and the number of lines to be added.  For each file there is
    // a circular buffer of records, each of which contains the length
    // and the contents of a line.  A vector of pointers indexes the
    // next records.  BOT points to the base of this vector, TOP points
    // to the current end of file in this vector (note that after the
    // first call to PUSHUP it has value BOT+MISMATCH+RESTORE until end
    // of file is reached; the only global indication of no more lines
    // is TOP <= BOT), BASE points to the base of the record buffer and
    // LIMIT points to just after the end of the record buffer.
    //
    LET t1,t2,t3,bot,top,base,limit,eof = ?,num,?,?,?,?,?,?
    // Select the file to manipulate.
    TEST index = 1
    THEN $(1 bot := bot1; top := top1; base := base1; limit := limit1;
        eof := eof1 $)1
    ELSE $(2 bot := bot2; top := top2; base := base2; limit := limit2;
        eof := eof2 $)2
    // Push up the circular buffer.
    t1 := bot+num
    TEST (t1>>1) < (top>>1)
    THEN $(3 FOR n = 0 TO top-t1-1 DO bot!n := t1!n; top := top-num $)3
    ELSE $(4 t2 := top-bot; top := bot $)4
    t3 := ([top>>1] > [bot>>1] ->!(top-1),limit)
    // Read in more records at end.
    FOR n = 1 TO num DO $(5 IF eof THEN BREAK
        t3 := ([(t3+words) >> 1] < [limit >> 1] ->t3+words,base); !top := t3
        t1 := read(index,t3+dataposn)
    // Deal with length etc.
        TEST t1 >= 0
        THEN $(6 t3!0 := t1; top := top+1 $)6
        ELSE eof := TRUE $)5
    // Reset file variables.
    TEST index = 1
    THEN $(7 top1 := top; eof1 := eof; line1 := line1+t2 $)7
    ELSE $(8 top2 := top; eof2 := eof; line2 := line2+t2 $)8
    RETURN
$)pushup



AND compare(chk1,chk2,num) = VALOF $(compare
    //
    // This compares blocks of lines.  Its arguments are pointers to the
    // buffer index vector.  End of file matches end of file only.
    //
    LET t1,t2,t3 = (top1-chk1),(top2-chk2),num
    IF t1 < num | t2 < num THEN $(2 t3 := t1
        IF t1 \= t2 THEN RESULTIS FALSE $)2
    FOR n = 0 TO t3-1 DO $(1 t1 := chk1!n; t2 := chk2!n
        IF length = '+' THEN IF t1!0 \= t2!0 THEN RESULTIS FALSE
        UNLESS equal(t1+dataposn,t2+dataposn,t1!0,t2!0)
        THEN RESULTIS FALSE $)1
    RESULTIS TRUE
$)compare



AND check() = VALOF $(check
    //
    // This checks the circular buffers to find the number of identical
    // lines.  End of file always gives inequality.
    //
    LET t1,t2,t3 = (top1-bot1),(top2-bot2),?
    t3 := (t1<t2->t1,t2)
    FOR n = 0 TO t3-1 DO $(1 t1 := bot1!n; t2 := bot2!n
        IF length = '+' THEN IF t1!0 \= t2!0 THEN RESULTIS n
        UNLESS equal(t1+dataposn,t2+dataposn,t1!0,t2!0)
        THEN RESULTIS n $)1
    RESULTIS t3
$)check



AND read(index,addr) = VALOF $(read
    //
    // This reads a record from one or other file.  Its arguments are
    // an index to which file and the address to which to transfer data.
    // It truncates it to the window size.  It returns the length or
    // -1 if end of file.  It gives a diagnostic if the files do not
    // exist.
    //
    LET t1 = (index=1->@stream1,@stream2)
    // Set up the correct stream, opening if necessary.
    IF current \= index
    THEN $(1 TEST !t1 = 0
        THEN $(2 !t1 := findinput(index=1->file1,file2)
            IF !t1 = 0
            THEN
                $(3
                TEST mode = 'E' THEN writes("| ") ELSE newline()
                writef("File %S cannot be opened*N*N",
                       (index=1->file1,file2))
                quit(20)
                $)3
             selectinput(!t1) $)2
        ELSE selectinput(!t1)
        current := index $)1
    // Read a line and sort out the length.
    t1 := readrec(addr)
    IF t1 > window+1 THEN t1 := window+1
    RESULTIS t1
$)read



AND printer(n1,n2) BE $(printer
    //
    // This produces edits or verifies differing lines, according to
    // the mode.  Its arguments are the number of differing lines in
    // each file.
    //
    LET t1,t2,t3 = ?,?,?
    LET s0 = VEC 1; LET s1 = VEC 1; LET s2 = VEC 1; LET s3 = VEC 1
    LET s4 = VEC 1; LET s5 = VEC 1; LET s6 = VEC 1
    LET s7,s8 = " to ","Printer limit exceeded."
    s0!0,s0!1 := "Line ","Lines "
    s1!0,s1!1 := "line ","lines "
    s2!0,s2!1 := " does"," do"
    s3!0,s3!1 := " occurs"," occur"
    s4!0,s4!1 := file1, file2
    s5!0,s5!1 := " is:"," are:"
    s6!0,s6!1 := "The line","The lines"
    TEST mode = 'E'
    // Produce edits.
    THEN $(0 TEST n1 = 0
        THEN $(1 wrch('I')
            TEST top1 <= bot1 THEN wrch('**') ELSE writen(line1+1) $)1
        ELSE $(2 wrch(n2>0->'R','D'); writen(line1+1)
            IF n1 > 1 THEN $(3 wrch(' '); writen(line1+n1) $)3 $)2
        newline(); printed := printed+(n2>0->n2+2,1)
    // Check printer limit before inserting lines.
        IF printed > print THEN $(h
            IF n2 > 0 THEN $(i wrch(terminate); newline() $)i
            writef("Q8  | %S*N",s8); quit(8) $)h
        FOR n = 0 TO n2-1 DO $(4 t1 := bot2!n
            writerec(t1+dataposn,t1!0) $)4
        IF n2 > 0 THEN $(5 wrch(terminate); newline() $)5
        RETURN
    $)0
    // Produce differences.
    ELSE $(6 newline()
        TEST n1 \= 0 & n2 \= 0
    // Replacement of lines.
        THEN $(7 t1 := (n1=1->0,1); t2 := (n2=1->0,1)
            writes(s0!t1); writen(line1+1)
            IF t1 = 1 THEN $(a writes(s7); writen(line1+n1) $)a
            writef(" of *'%S*'%S not match %S%N",s4!0,s2!t1,s1!t2,line2+1)
            IF t2 = 1 THEN $(b writes(s7); writen(line2+n2) $)b
            writef(" of *'%S*'", s4!1); newline()
    // Check printer limit before verifying lines.
            printed := printed+(verify>1->(n1+n2)*linesper+4,2)
            IF printed > print THEN $(f writef("*N%S*N",s8)
                quit(8) $)f
            IF verify \= 1
    // Verify lines.
            THEN $(8 writef("%S of *'%S*'%S",s6!t1,s4!0,s5!t1); prnt1(bot1,n1)
                writef("%S of *'%S*'%S",s6!t2,s4!1,s5!t2); prnt1(bot2,n2)
        $)8 $)7
    // Insertions or deletions.
        ELSE $(9 t1 := (n2=0->0,1); t2 := (t1=0->n1,n2)
            t3 := (t2=1->0,1); writes(s0!t3)
            writen((t1=0->line1,line2)+1)
            IF t3 = 1 THEN $(c writes(s7);
                 writen((t1=0->line1,line2)+t2) $)c
            writef(" of *'%S*'", s4!t1); writes(s3!t3)
    // Say where extra lines occur.
            TEST (t1=0->top2-bot2,top1-bot1) > 0
            THEN writef(" before line %N of *'%S*'",(t1=0->line2,line1)+1,
                s4!(1-t1))
            ELSE writef(" at the end of *'%S*'",s4!(1-t1))
            newline(); printed := printed+(verify>1->t2*linesper+3,2)
    // Check printer limit before verifying lines.
            IF printed > print THEN $(g writef("*N%S*N",s8)
                quit(8) $)g
            IF verify \= 1
    // Verify lines.
            THEN $(10 writef("%S of *'%S*'%S",s6!t3,s4!t1,s5!t3)
                prnt1((t1=0->bot1,bot2),t2)
         $)10 $)9 $)6
$)printer



AND prnt1(bot,n) BE $(prnt1
    //
    // This verifies lines.  Its arguments are a pointer to the buffer
    // index vector and the number of lines to be verified.
    //
    LET t1,t2 = ?,?
    newline()
    FOR m = 0 TO n-1 DO $(1 t1 := bot!m
    // Verify the line in the desired manner.
        IF verify = 2 & length = '+' THEN
            writef("The line length is %N.*N",!t1)
        IF verify = 2 | verify = 4 THEN $(2
            writerec(t1+dataposn,t1!0) $)2
        IF verify >= 3 THEN writerecx(t1+dataposn,t1!0) $)1
$)prnt1




AND quit(code) BE $(quit
    //
    // This is the exit routine.  Its argument is the return code.
    // It returns all the free store got for the program.
    //
    //IF mode = 'E' THEN $(1 wrch('|'); wrch(' ') $)1
    freevec(buffer)
    UNLESS stream1 = 0
    THEN $( selectinput(stream1); endread() $) || Sordid!
    UNLESS stream2 = 0
    THEN $( selectinput(stream2); endread() $)
    UNLESS tostream = 0 THEN endwrite()
    stop(code)
$)quit

    //
    // Machine dependent routines.
    //

AND writerecx(addr,len) BE $(writerecx
    //
    // This is machine dependent.  It is the equivalent of
    // WRITEREC but prints in hexadecimal.  Its arguments are the
    // vector and the number of characters.
    //
    LET t1,hextab = ?,TABLE '0','1','2','3','4','5','6','7','8','9','A',
        'B','C','D','E','F'
    FOR l = 0 TO len-1 DO $(1 t1 := addr%l
        wrch(hextab!(t1/16)); wrch(hextab!(t1 REM 16)) $)1
    newline()
$)writerecx



AND writerec(buff, len) BE
    $(
    || Writes len characters from buff
    FOR j = 0 TO len - 1 DO wrch(buff%j)
    newline()
    $)



AND readrec(buff) = VALOF
    $(
    || Reads one record into buff.
    || Result is number of characters read, or -1 if file
    || is exhausted.
    LET j = 0
    LET ch = rdch()
    UNTIL (ch = '*N') | (ch = endstreamch)
    DO $( buff%j := ch; ch := rdch(); j := j+1 $)

    IF (ch = endstreamch) & (j = 0) RESULTIS -1
    RESULTIS j
    $)
