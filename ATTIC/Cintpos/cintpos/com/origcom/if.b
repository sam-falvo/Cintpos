/***********************************************************************
**             (C) Copyright 1982  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************
*                                                                      *
*                          ########  ########                          *
*                          ########  ########                          *
*                             ##     ##                                *
*                             ##     ######                            *
*                             ##     ##                                *
*                             ##     ##                                *
*                          ########  ##                                *
*                          ########  ##                                *
*                                                                      *
************************************************************************
**    Author:  Adrian Aylward                                 1978    **
***********************************************************************/

// Modifications:
// 24 Feb 82 by BJK: Addition of MCTYPE keyword enabling machine type
//		     to be tested.


SECTION "IF"

GET "LIBHDR"
GET "CLIHDR"

LET start() BE
 $( LET v 		= VEC 80
    LET sw 		= FALSE
    LET rdargs.string	= "NOT/S,WARN/S,ERROR/S,FAIL/S,EQ/K,,EXISTS/K,MCTYPE/K"

    IF rdargs(rdargs.string, v, 80)=0 THEN GOTO badargs

    sw := VALOF
     $( IF v!1 & cli.returncode>= 5 RESULTIS TRUE
        IF v!2 & cli.returncode>=10 RESULTIS TRUE
        IF v!3 & cli.returncode>=20 RESULTIS TRUE

        UNLESS v!4=0 DO
         $( IF v!5=0 GOTO badargs
            IF compstring(v!4, v!5)=0 RESULTIS TRUE
         $)

        UNLESS v!6=0 DO
         $( LET s = locateobj(v!6)
            UNLESS s=0 DO
             $( freeobj(s)
                RESULTIS TRUE
             $)
         $)

        UNLESS v!7=0 // MCTYPE
        THEN IF compstring(v!7, rootnode ! rtn.info ! info.mctype)=0
             THEN RESULTIS TRUE

        RESULTIS FALSE
     $)

    IF v!0 DO sw := NOT sw

    UNLESS sw DO
     $( LET ch = unrdch() -> rdch(), '*N'
        UNTIL ch='*N' | ch=endstreamch DO ch := rdch()
     $)

    stop(cli.returncode)

badargs:
    writef("Bad args for key string *"%S*"*N", rdargs.string)
    stop(20)
    $)
