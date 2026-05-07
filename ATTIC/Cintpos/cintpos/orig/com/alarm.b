// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "ALARM"

GET "g/libhdr.h"

GLOBAL $( timestr : ug $)

MANIFEST
    $(
    minsinday = 24*60
    tickspermin = 60*tickspersecond
    $)

LET start() BE
    $(
    LET argv = VEC 50
    LET v = VEC 14
    LET len = ?
    LET ticks = 0
    LET mins = 0
    LET hours,secs = ?, ?
    LET minstowait = ?

    IF rdargs("time=at/A,message", argv, 50) = 0
    THEN error()

    timestr := argv!0
    len := timestr%0

    // Allow mins &/or secs to be omitted

    SWITCHON len
    INTO
        $(
        CASE 8: // Full time
                secs := digit(7)*10 + digit(8)
                IF secs > 59 THEN error()
                ticks := secs * tickspersecond

        CASE 6: // Just hours and mins
        CASE 5: mins := digit(4)*10 + digit(5)
                IF mins > 59 THEN error()

        CASE 3: // Just hours
        CASE 2: hours := digit(1)*10 + digit(2)
                IF hours > 23 THEN error()
                mins := mins + (hours*60)
                ENDCASE

        DEFAULT: error()
        $)

    $( // Loop
    datstamp(v)
    minstowait := mins - v!1
    IF (minstowait = 0) |
       ((minstowait = 1) & (ticks < v!2))
    THEN BREAK // < 1 min to go
    IF minstowait < 0 THEN minstowait := minstowait + minsinday
    delay(tickspermin) // Wait one minute
    IF testflags(1)
    THEN
        $(
        writef("Alarm for %S cancelled*N", timestr)
        stop(0)
        $)
    $) REPEAT

    // Wait for the remaining fraction of a minute
    datstamp(v)
    ticks := minstowait*tickspermin + (ticks - v!2)
    IF ticks > 0 THEN delay(ticks)
    FOR j=1 TO 50 DO wrch(7) // ASCII Bells
    writef("*E*E*C****** Alarm: time is %S", datstring(v) + 5)
    UNLESS argv!1 = 0 writef(" - %S", argv!1)
    newline()
    $)


AND digit(n) = VALOF
    $(
    LET d = timestr%n
    UNLESS '0' <= d <= '9' THEN error()
    RESULTIS d - '0'
    $)


AND error() BE
    $(
    writes("Invalid parameter: format is HH:MM:SS*N")
    stop(20)
    $)
