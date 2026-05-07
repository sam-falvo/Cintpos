/*


















*************************************************************************
*  (C) Copyright 1981  Systems Research Group, University of Cambridge  *
*************************************************************************
*                                                                       *
*                             H  E  L  P                                *
*                                                                       *
*************************************************************************
**  C  Gray  Girling      COMPUTER LAB,  CAMBRIDGE           21.06.81  **
*************************************************************************



// Modified by MR 12/9/96


















*/



SECTION "Help"

GET "g/libhdr.h"
GET "g/clihdr.h"
//GET "TERMHDR"


STATIC    // not GLOBAL - so HELP can be CALLSEG'd
$(  standard_help  = TRUE    // STATIC incase CALLSEGED
    private_output = FALSE   // set when T commands cannot be allowed
    terminal_out   = 0       // stream to the terminal
    tracing        = FALSE   // TRUE when tracing info needed
    broken         = FALSE   // TRUE when a BREAK has been pressed
    page_len       = 21      // length of HELP page
    page_width     = 80      // Width of page
    main_help_dir  = 0       // String: giving name of the help directory
$)



MANIFEST
$(  argsize = 50             // store for help arguments
    max_args = 15            // maximum number of HELP items for request
    max_blank_lines = 5      // longest sequence of '*N's printed
$)




LET start(vect, helpdir, tofile, opt) BE
$(  LET arg = VEC argsize
    //  arg ! 0            holds a bit map of keys that have been
    //                     successfully matched in help files so far.
    //  arg + 1            holds vector of 'n' keys
    //  arg!(max_args+1)    holds name of an alternative HELP directory
    //  arg!(max_args+2)    holds name of optional output stream
    LET saveout = output()
/* MR 12/9/96
    $(  // set up size of the terminal screen - this code is not TRIPOS portable
        LET type =ROOTNODE ! RTN_TASKTAB ! CONSOLETASK ! TCB_GBASE ! TERM_GLOBAL
        UNLESS type=0
        $(  IF 40<=type!term_width<=150 THEN page_width := type!term_width
            IF 10<=type!term_depth<=80  THEN page_len   := type!term_depth-3
        $)
    $)
*/
    terminal_out := findoutput("**")
    selectoutput(terminal_out)
    IF VALOF
    $(  LET ans = FALSE
        TEST vect=0 THEN   // not CALLSEGed
            TEST 0=rdargs(",,,,,,,,,,,,,,,#HELPDIR/K,#TO/K,#TRACE/S",
                          arg+1, argsize-1)
            THEN writes("Arguments invalid for HELP*N") ELSE ans:=TRUE
        ELSE
        $(  // HELP has been CALLSEGed
            // VECT contains vector of up to VECT!0 help keywords from
            // VECT!1 to VECT!(VECT!0)
            // HELPDIR is a string naming an alternate HELP directory
            // TOFILE is a string naming an alternative output file
            // OPT is a bit array of options:
            //     1 & (OPT>>1)   -  TRACE: route to files is to be listed
            //     1 & (OPT>>1)   -  STAND: logging is to be allowed
            arg!0 := 0          // initially all keys are unmatched
            FOR i=1 TO max_args DO arg!i := (i>vect!0 ->0, vect!i)
            arg!(max_args+1) := helpdir
            arg!(max_args+2) := tofile
            arg!(max_args+3) := ( ((opt & #X01)=0) -> 0, 42)
            private_output := TRUE
            standard_help := ((opt & #X02) ~= 0)
            ans := TRUE
        $)
        RESULTIS ans
    $) THEN
    $(  LET default_help_dir = "SYS:HELP"
        LET to_file = (arg+1)!(max_args+1)
        LET list = (to_file=0 -> 0, findoutput(to_file))
        LET no_of_items = 0
        main_help_dir := ((arg+1)!max_args=0 ->
                           default_help_dir, (arg+1)!max_args)
        tracing := ((arg+1)!(max_args+2) \= 0)
        standard_help := standard_help & ((arg+1)!max_args=0)
        IF to_file~=0 THEN private_output := TRUE
        WHILE no_of_items<=max_args & (arg+1)!no_of_items~=0 DO
        no_of_items:=no_of_items+1
        IF no_of_items=0 THEN
        $(  arg!1 := "HELP"
            no_of_items := 1
        $)
        wrch('*C')    // make sure output starts at front of the line!
        TEST list=0 & to_file~=0 THEN
        toterm("HELP: can't open listing file *"%S*"*N",to_file) ELSE
        $(  LET saveout = output()
            IF list~=0 THEN selectoutput(list)
            UNLESS execute_help_directory(main_help_dir, no_of_items, arg) THEN
            log(no_of_items,arg)
            IF list~=0 THEN
            $(  endwrite()
                selectoutput(saveout)
            $)
        $)
    $)
    endwrite()
    selectoutput(saveout)
$)







AND execute_help_directory(dir, n, arg) = VALOF
$(  LET old_dir = currentdir
    LET found = FALSE
    currentdir := locatedir(dir)
    TEST currentdir=0 THEN
    toterm("HELP: can't find HELP directory *"%S*"*N",dir) ELSE
    $(  LET file=0
        LET the_match = 1<<0    // bit map indicating which key word matched
        LET findinproc = VEC 1     // vector of procedures
        LET procno = 0             // pointer into FINDPROC
        LET try_initial_file_name(f_name) = VALOF
        $(  LET file = findinput(f_name)
            IF file=0 THEN
            $(  LET last_ch = f_name%(f_name%0)
                IF capitalch(last_ch) = 'S' THEN
                $(  f_name%0 := f_name%0-1
                    file := findinput(f_name)
                    IF file=0 THEN f_name%0 := f_name%0+1
                $)
            $)
            RESULTIS file
        $)
        LET try_default_file_name(f_name) = VALOF
        // Default already tested if file name is only 1 character long
        TEST f_name%0<=1 THEN RESULTIS 0 ELSE
        $(  LET len = f_name%0      // try file names which are just initials
            LET file = ?
            f_name%0 := 1
            file := findinput(f_name)
            IF file=0 THEN f_name%0 := len
            RESULTIS file
        $)
        findinproc!0 := try_initial_file_name
        findinproc!1 := try_default_file_name
        IF tracing THEN toterm("HELP: using directory %S*N",dir)
//      toterm("EXECUTE HELP: keywords are:*N")
//      FOR i=1 TO n DO
//           toterm("EXECUTE HELP: No. %I2 *"%S*" %S*N", i,
//                  (arg!i=0->"<absent>", arg!i),
//                  (((arg!0>>(i-1))&1)=0 ->"","matched") )
        WHILE ~found & procno<=1 DO
        $(  LET i=1
            WHILE ~found & i<=n DO
            $(  LET thisarg = arg!i
                LET old_match = arg!0
                UNLESS thisarg%0 > 30 | ((old_match & the_match)~=0) THEN
                $(  LET f_name = VEC 16
                    FOR i=1 TO thisarg%0 DO f_name%i := file_ch(thisarg%i)
                    f_name%0 := thisarg%0
    //              toterm("EXECUTE HELP: trying file name *"%S*"*N", f_name)
                    file := (findinproc!procno)(f_name)
                    arg!0 := arg!0 | the_match
                    IF file~=0 THEN
                    $(  IF tracing THEN
                        toterm("HELP: using help file *"%S*"*N", f_name)
                        found := do_help(file, '**', endstreamch, FALSE, n, arg)
                        IF tracing & ~found THEN
                        toterm("HELP: no help from help file *"%S*"*N", f_name)
                    $)
                    UNLESS found THEN arg!0 := old_match
                $)
                the_match := the_match<<1
                i:=i+1
            $)
            procno := procno + 1
        $)
        UNLESS currentdir=old_dir DO freeobj(currentdir)
        currentdir := old_dir
    $)
    RESULTIS found
$)



AND file_ch(ch) =
    // only certain characters are allowed in file names
    // change those that are not into the '-' character
    // it is particularly important in the case of strings
    // which could be opened as some other kind of stream
    // name: e.g.  "mp:" or "help.rat"
    ( 'A' <= capitalch(ch) <= 'Z' -> ch,
    ( '0' <= ch <= '9' -> ch, '-'))




AND do_help(help, helpch, termch, all, n, args) = VALOF
$(  LET savein = input()
    LET this_file = (0=help)
    LET found = FALSE
    LET help_key_found = FALSE    // TRUE if help key (*?) found in the file
    LET no_good = FALSE           // TRUE if last match was for nothing (#N)
    LET line = VEC 30
    LET rdargs_string =
        "#KEYWORD,#FILE,#H/s,#T/s,#I/s,#C/s,#N/s,#TCH/k,#HCH/k,#FULL/s,../s"
    MANIFEST
    $(  l_key   = 0                 // string to be matched (or + or *)
        l_file  = 1                 // file to be executed if match
        l_h     = 2                 // execute as a help file
        l_t     = 3                 // execute by T ing file
        l_i     = 4                 // execute as an interactive help file
        l_c     = 5                 // execute file with CALLSEG
        l_n     = 6                 // succeed match but fail file
        l_tch   = 7                 // terminating 'end of file' character
        l_hch   = 8                 // alternative help character
        l_full  = 9                 // in help file do not stop at 1st match
        l_cont  = 10                // ignore this match
    $)
    UNLESS this_file THEN selectinput(help)
    WHILE (all |~found) & VALOF
    $(  LET ch=rdch()
        LET ans=?
        WHILE ch~=helpch & ch~=termch & ch~=endstreamch DO
        $(  WHILE ch~='*N' & ch~='*E' & ch~=endstreamch DO ch:=rdch()
            UNLESS ch=endstreamch THEN ch:=rdch()
        $)
        ans := (ch~=termch & ch~=endstreamch)
        IF ans THEN
        IF 0=rdargs(rdargs_string, line, 30) THEN
        $(  ans := FALSE
            toterm("HELP: Line does not conform to RDARGS string *N*"%S*"*N",
                   rdargs_string)
            TEST line!l_key=0 THEN toterm("(Empty line?)*N") ELSE
            toterm("(Line starts *"%S*")*N",line!l_key)
        $)
        IF test_break() THEN found:=TRUE
        RESULTIS ans & (all | ~found)
    $) DO
    UNLESS line!l_key=0 THEN
    $(  LET i=0
        LET match = FALSE
        LET caught = FALSE
        LET n_termch =
            (line!l_tch=0 -> ((line!l_file=0)->helpch, endstreamch),
             (line!l_tch%0=0 -> endstreamch, line!l_tch%1))
        LET n_helpch = (line!l_hch=0 -> '**',
                        line!l_hch%0=0 -> endstreamch, line!l_hch%1)
        LET n_all = (line!l_full~=0)
        LET h_help = (line!l_h~=0)        // help file help
        LET t_help = (line!l_t~=0)        // T file help
        LET i_help = (line!l_i~=0)        // interactive help
        LET c_help = (line!l_c~=0)        // CALLSEG help
        LET no_help = (line!l_n~=0)       // no help
        LET ignore_help = (line!l_cont~=0)// continue help
        help_key_found := TRUE
        no_good := FALSE
        WHILE i<=n-1 & ~caught DO
        $(  LET matched_set = args!0    // set of keys matched so far
            match := match_str(line!l_key, found, i, n, args)
            IF match THEN
            $(  // execute command denoted by RDARGS parameters:
                caught := TRUE
//              toterm("DO HELP: #H=%C #T=%C #I=%C #C=%C #N=%C ..=%C  *"%S*"*N",
//                     (h_help -> 'T','F'),
//                     (t_help -> 'T','F'),
//                     (i_help -> 'T','F'),
//                     (c_help -> 'T','F'),
//                     (no_help -> 'T','F'),
//                     (ignore_help -> 'T','F'),
//                     line!l_key)
                IF line!l_file=0 | i_help THEN skipterm(helpch)
                // skip over other 'labels'
                match := ~ignore_help
                no_good := no_help
                // this help file will fail if this is the last match
                // and no_help is set
                TEST h_help THEN
                UNLESS
                find_help(line!l_file, n_helpch, n_termch, n_all, n,args) THEN
                $(  args!0 := matched_set     // restore old set
                    match := FALSE
                $) ELSE
                TEST t_help THEN t_command(line!l_file, n_termch) ELSE
                TEST i_help THEN
                $(  LET saveout=output()
                    selectoutput(terminal_out)
                    type(0, (line!l_file=0 -> n_helpch, helpch))
                    selectoutput(saveout)
                    IF line!l_file=0 & helpch~=n_helpch THEN skipterm(helpch)
                    interactive_help(line!l_file, n_helpch, n_termch,
                                     n_all, n, args)
                $) ELSE
                TEST c_help THEN
                UNLESS call_command(line!l_file, n_all, n,args) THEN
                $(  args!0 := matched_set       // restore old set
                    match := FALSE
                $) ELSE
                UNLESS no_help THEN
                type(line!l_file, n_termch)
            $)
            found := found | match
            i:=i+1
        $)
    $)
//  toterm("FOUND = %C*N", (found->'T','F'))
    UNLESS help_key_found THEN
    toterm("HELP: no help key (*"%C*") found in HELP file*N", helpch)
    UNLESS this_file THEN
    $(  UNLESS cli_currentinput=input() THEN endread()
        selectinput(savein)
    $)
    RESULTIS found & ~no_good
$)




AND match_str(key, found, i, n, request_vec) = VALOF
$(  LET request = (request_vec+1)!i
    LET set = request_vec!0           // bit map of matched items
    LET ans = ?
    LET request_matched = TRUE
    LET key_len = key%0
    // strip trailing blanks off key for comparason
    WHILE key%(key%0) = '*S' DO key%0 := key%0-1  // assumes '*S'~=0
    ans := (0=compstring(key, request))
    IF ~ans THEN
    $(  LET lastch = request%(request%0)
        IF lastch='s' | lastch='S' THEN
        $(  request%0 := request%0 - 1
            ans := (0=compstring(key, request))
            request%0 := request%0 + 1
        $)
        UNLESS ans THEN request_matched := FALSE
        // put trailing blanks back before checking for + or *
        key%0 := key_len
        IF ~ans THEN
        TEST 0=compstring(key,"+") THEN
        $(  // "+" matches if all keys have already been matched
            ans := TRUE
            FOR i=0 TO n-1 DO
            $(  ans:=ans & ((set & (1<<i))~=0)
//              writef("%S - %Smatched*N",request_vec!(i+1),
//                      (((set & (1<<i))~=0) -> "", "not ") )
            $)
        $) ELSE
        IF ~found THEN
        ans:=(0=compstring(key,"**"))  // * matches everything
    $)
    IF request_matched THEN request_vec!0 := set | (1<<i)
//  toterm("MATCH:  '%S' %S= '%S'*N",key,(ans->"","~"),request)
    IF tracing & ans THEN
    toterm("HELP: *"%S*" key matched with *"%S*"*N", request, key)
    RESULTIS ans
$)




AND skipterm(termch) BE
$(  LET ch=rdch()
    WHILE ch~=endstreamch & ch=termch DO
    $(  ch:=rdch() REPEATUNTIL ch='*N' | ch=endstreamch
        ch:=rdch()
    $)
    unrdch()
$)





AND findhelpinput(file) = VALOF
TEST file%0=0 THEN RESULTIS 0 ELSE
$(  // To reduce the number of files needed short HELP files are provided
    // in the default HELP files A, B, C .. &c In order that HELP files
    // may refer to a file without knowing whether it is in one of these
    // default files or not both are tried:
    LET ans = findinput(file)
    IF ans=0 & file%0>1 THEN
    $(  LET len = file%0
        LET i=len
        WHILE i>=1 & file%i\='.' & file%i\=':' DO i:=i-1
        file%0 := ((file%i='.' | file%i=':') & i\=len -> i+1, 1)
        // get file name with just FILEs initial letter
        ans := findinput(file)
        // do not restore string - leave for subsequent printing out
    $)
    RESULTIS ans
$)







AND find_help(help_file, helpch, termch, all, n, args) = VALOF
TEST help_file~=0 & help_file%0>0 & help_file%1='@' THEN
$(  FOR i=2 TO help_file%0 DO help_file%(i-1) := help_file%i
    help_file%0 := help_file%0-1
    // i.e. remove initial '@' character
    IF help_file%0=0 THEN help_file := main_help_dir
    RESULTIS execute_help_directory(help_file, n, args)
$) ELSE
$(  LET this_file = (help_file = 0)
    LET help = (this_file -> 0, findhelpinput(help_file))
    LET ans = FALSE
    IF tracing THEN
    toterm("HELP: using *"%S*"*N",
           (this_file->"<same file>",help_file))
//  toterm("   FULL = %C   TCH = %C   HCH = %C*N",
//         (all -> 'T','F'), termch, helpch)
    TEST ~this_file & help=0 THEN
    toterm("HELP: Can't open HELP file *"%S*"*N",help_file) ELSE
    $(  ans:= do_help(help, helpch, termch, all, n, args)
        IF tracing & ~ans THEN
        toterm("HELP: no help from *"%S*"*N",
               (help_file=0 -> "<same file>", help_file))
    $)
    RESULTIS ans
$)




AND t_command(file_name, termch) BE
TEST private_output THEN
$(  toterm("HELP cannot do T command for help on this topic in this mode*N")
    writes("****** HELP: execute the following in command mode*N")
    type(file_name, termch)
$) ELSE
$(  // can't enforce the TERMCH
    LET t_file = (0=file_name -> input(),findhelpinput(file_name))
    TEST t_file=0 THEN
    toterm("HELP: can't open T file *"%S*"*N", file_name) ELSE
    $(  IF tracing THEN
        toterm("HELP: T ing *"%S*"*N", (file_name=0 ->"<same file>", file_name))
        UNLESS cli_currentinput=cli_standardinput THEN
        $(  selectinput(cli_currentinput)
            endread()
        $)
        cli_currentinput := t_file
    $)
$)



AND interactive_help(file, helpch, termch, all, n, args) BE
$(  LET savein=input()
    LET rdargs_bad = ?
    standard_help := FALSE
    selectinput(findinput("**"))
    toterm("? *E")
    rdargs_bad := (0=rdargs(",,,,,,,,,,,,,,,",args+1,argsize-1))
    args!0 := 0    // clear set of matched items to zero
    endread()
    selectinput(savein)
    TEST rdargs_bad THEN
    writes("HELP: too many qualifiers*N") ELSE
    IF args!1~=0 THEN
    $(  LET no_of_items = 0
        WHILE no_of_items<=max_args & (args+1)!no_of_items~=0 DO
        no_of_items:=no_of_items+1
        outch(0)    // clean page for output
        find_help(file, helpch, termch, all, no_of_items, args)
    $)
$)





AND call_command(file, all, n, args) = VALOF
TEST file=0 THEN
$(  toterm("HELP: no file to callseg with #C*N")
    RESULTIS FALSE
$) ELSE
$(  LET rc = ?
    IF tracing THEN toterm("HELP: callseging file *"%S*"*N", file)
    result2 := 0
    rc := callseg(file, args, n, all)
//  $(  LET r2=result2
//      writef("CALL: after callseg result2 = %N*N", r2)
//      result2 := r2
//  $)
    UNLESS result2=TRUE | result2=FALSE THEN
    $(  // if an error has occured during CALLSEG result2 will not
        // be one of the expected results (i.e. TRUE or FALSE) but
        // will be a system error return code!
        toterm("HELP: failed to load #C file *"%S*"*N", file)
        rc := FALSE
    $)
    RESULTIS rc
$)




AND type(file_name, termch) BE
$(  LET this_file = (0=file_name)
    LET message_file = (this_file -> 0,findhelpinput(file_name))
    TEST ~this_file & message_file=0 THEN
    toterm("HELP: Can't open TYPE file *"%S*"*N", file_name) ELSE
    $(  LET savein = input()
        LET ch = ?
        IF tracing THEN
        toterm("HELP: typing file *"%S*"*N",
               (this_file -> "<same file>", file_name))
        UNLESS this_file THEN selectinput(message_file)
        ch := rdch()
//      toterm("TYPE: termch = '%C'*N", termch)
        WHILE ch~=endstreamch & ch~=termch & ~test_break() DO
        $(  WHILE ch~='*N' & ch~=endstreamch DO ch := outch(ch)
            UNLESS ch=endstreamch THEN ch:=outch(ch)
//          TEST ch~=endstreamch & ch~='*N' THEN
//          toterm("First CH is '%C'*N", ch) ELSE
//          toterm("First CH is '%C'*N",
//                 (ch='*N' -> "**N", "endstreamch"))
        $)
//      toterm("TYPE finished ch='%S'*N",
//             (ch=termch -> "termch", ch=endstreamch -> "endstreamch",
//              ch='*N' -> "**N", "other"))
        IF ch=termch THEN unrdch()
        UNLESS this_file THEN
        $(  endread()
            selectinput(savein)
        $)
    $)
$)



AND outch(ch) = VALOF
$(  // This routine outputs a character and reads another in
    // Page waits to the terminal taking taking account of the following:
    //     a) lines too long for screen width
    //     b) the length of the screen in lines
    //     c) the characters '*N', '*C', '*P' and '*T'
    // Modified Sept 1981 by Piete Brooks to generalise a) & b) and do c) '*T'
    STATIC
    $(  lineno          = 0
        blank_lines     = 0
        line_pos        = 0
    $)

    LET wrc(ch) = VALOF
    $(  LET res = 0
        IF line_pos >= page_width THEN res := outch('*N')
        wrch(ch)
        line_pos := line_pos+1
        blank_lines := 0
        RESULTIS res
    $)

    TEST ch=0
    THEN lineno, blank_lines, line_pos := 0, 0, 0
    ELSE TEST output()=terminal_out
    $(  SWITCHON ch INTO
        $(  DEFAULT:
                ch := wrc(ch)
                UNLESS ch=0 RESULTIS ch
                ENDCASE
            CASE '*T':
                ch := wrc(' ') REPEATUNTIL line_pos REM 8 = 0
                UNLESS ch=0 RESULTIS ch
                ENDCASE
            CASE '*N':
                line_pos := 0
                UNLESS blank_lines > max_blank_lines THEN
                TEST lineno > page_len
                $(  LET savein = input()
                    selectinput(findinput("**"))
                    wrch('*E')
                    ch := rdch() REPEATUNTIL ch='*N' | ch=endstreamch
                    endread()
                    selectinput(savein)
                    lineno := 0
                $) ELSE
                $(  wrch(ch)
                    lineno := lineno + 1
                    blank_lines := blank_lines + 1
                $)
        $)
        ch := rdch()
        IF ch='*C' | ch='*P' THEN ch:='*N'
    $) ELSE
    $(  wrch(ch)
        ch:=rdch()      // output stream is to a file
    $)
    RESULTIS ch
$)




AND test_break() = VALOF
$(  IF testflags(1) THEN
    $(  toterm("****** BREAK: in HELP*N")
        broken := TRUE
    $)
    RESULTIS broken
$)




AND toterm(format, a1, a2, a3, a4, a5, a6, a7) BE
$(  LET saveout=output()
    selectoutput(terminal_out)
    writef(format, a1, a2, a3, a4, a5, a6, a7)
    selectoutput(saveout)
$)




//*<LOG
AND log(n, arg) BE
UNLESS n=0 THEN
$(  LET help_temp = "T:HELP-text"
    LET help_log = "FF020A22DD4D94CA"
    LET file = findoutput(help_temp)
    LET log_used = FALSE
    LET saveout = output()
    selectoutput(terminal_out)
    writes("HELP has no information on this topic*N")
//  writef("LOG: standard help = %C, file %C= 0*N",
//        (standard_help -> 'T','F'), (file=0 -> '*S','~'))
    $(  TEST standard_help & user_wants_to_log() THEN
        $(  selectoutput(file)
            writes("No HELP on *"")
            FOR i=1 TO n DO writef("%S ", (arg!i=0 -> "", arg!i))
            wrch('"')
            newline()
            log_used := TRUE
        $) ELSE
        $(  writes("(Use HELP TROUBLE to report any trouble with HELP)*N")
            selectoutput(file)
        $)
        endwrite()
        selectoutput(terminal_out)
        IF log_used THEN
        $(  callseg("SYS:C.SEND", help_log, help_temp, 0, "HELP")
            IF result2=0 THEN
            writes("Your request has been recorded*N")
        $)
        deleteobj(help_temp)
    $)
    selectoutput(saveout)
$)
/*/   // if not that log procedure then this one:



AND log(n, arg) BE
UNLESS n=0 THEN
$(  LET help_temp = "T:HELP-text"
    LET help_log = "SYS:HELP.pending"
    LET saveout = output()
    selectoutput(terminal_out)
    writes("HELP has no information on this topic*N")
//  writef("LOG: standard help = %C*N", (standard_help -> 'T','F'))
    IF standard_help & user_wants_to_log() THEN
    $(  LET in = findinput(help_log)
        LET out = findoutput(help_temp)
        LET log_used = FALSE
        TEST out=0 THEN
        writef("HELP: Can't open work file *"%S*"*N", help_temp) ELSE
        $(  LET saveoutput=output()
            selectoutput(out)
            TEST in=0 THEN
            writes("*****NHELP is pending on:*N*N") ELSE
            $(  LET ch = ?
                LET savein = input()
                selectinput(in)
                ch := rdch()
                WHILE ch\=endstreamch DO
                $(  wrch(ch)
                    ch:=rdch()
                $)
                endread()
                selectinput(savein)
            $)
            write_stamp()
            FOR i=1 TO n DO writef("%S ", (arg!i=0 -> "", arg!i))
            newline()
            endwrite()
            selectoutput(saveout)
            TEST renameobj(help_temp, help_log)=0 THEN
            writef("HELP: Can't rename *"%S*" as *"%S*"*N", help_temp, help_log)
            ELSE log_used := TRUE
        $)
        TEST NOT log_used THEN
        writes("(Use HELP TROUBLE to report any trouble with HELP)*N") ELSE
        writes("Your request has been recorded*N")
    $)
    selectoutput(saveout)
$)



AND write_stamp() BE
$(  LET v = VEC 14
    datstring(v)
    writef("%S %S ", v, v+5)
$)
/*LOG>*/



AND user_wants_to_log() = VALOF
$(  LET savein = input()
    LET ch = ?
    selectinput(findinput("**"))
    writes("If you do not want this request logged type an 'n' here: *E")
    ch:=rdch() REPEATUNTIL ch~='*S'
    endread()
    selectinput(savein)
    RESULTIS ~(ch='n' | ch='N')
$)

