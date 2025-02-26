/***********************************************************************
**             (C) Copyright 1980  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************

                 ##    ##   ######   ##        ######
                 ##    ##  ########  ##        #######
                 ##    ##  ##    ##  ##        ##    ##
                 ########  ##    ##  ##        ##    ##
                 ##    ##  ##    ##  ##        ##    ##
                 ##    ##  ##    ##  ##        ##    ##
                 ##    ##  ########  ########  #######
                 ##    ##   ######   ########  ######

************************************************************************
**    Author:   Brian Knight                       February 1980      **
***********************************************************************/


// Program to hold another task.
SECTION "hold"

GET "libhdr"
//GET "g/string-to-number.b"

LET start() BE
    $(
    LET argv = VEC 20
    LET task = ?

    IF rdargs("task/a", argv, 20) = 0
    THEN $( writes("Bad args*n"); stop(20) $)

    TEST string.to.number(argv!0)
    THEN task := result2
    ELSE $( writef("Bad number *"%s*"*n", argv!0); stop(20) $)

    IF hold(task)=0 THEN $( fault(result2); stop(20) $)
    $)
