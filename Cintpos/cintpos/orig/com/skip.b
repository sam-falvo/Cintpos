SECTION "Skip"
GET "Libhdr"
GET "Clihdr"

GLOBAL $( ch: ug $)

LET start() BE
$(  LET arg= VEC 50
    LET rc=return.hard
    TEST ~rdargs("LABEL",arg,50) THEN
    writes("Bad argument spec for Skip*N")
    ELSE TEST cli.standardinput = cli.currentinput THEN
    writes("Skip must be in a command file*N")
    ELSE
    $(  LET label= (0=arg!0->"",arg!0)
        LET lab = ?
        LET s = ?
        LET found = FALSE
        ch:='*N'
        WHILE ~found & ch~=endstreamch DO
        $(  ch:=rdch() REPEATUNTIL ch~=' '
            s:=rdstr()
            IF 0=compstring(s,"lab") THEN
            $(  WHILE ch=' ' DO ch:=rdch()
                lab:=rdstr()
                found:=(0=compstring(lab,label))
                freevec(lab)
            $)
            freevec(s)
            WHILE ch~='*N' & ch~=endstreamch DO ch:=rdch()
        $)
        TEST ~found THEN writef("Label *"%s*" not found by Skip*N",label)
        ELSE rc:=0
    $)
    stop(rc)
$)




AND rdstr() = VALOF
$(  LET s=getvec(127)
    LET i=1
    WHILE ch~='*N' & ch~=endstreamch & ch~=' ' DO
    $(  s%i:=ch
        ch:=rdch()
        i:=i+1
    $)
    s%0 := i-1
    RESULTIS s
$)

