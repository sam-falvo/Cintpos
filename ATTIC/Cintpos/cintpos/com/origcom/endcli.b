/***********************************************************************
**             (C) Copyright 1981  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************
*                                                                      *
*      ########  ##    ##  ######      #####   ##        ########      *
*      ########  ###   ##  #######    #######  ##        ########      *
*      ##        ####  ##  ##    ##  ##        ##           ##         *
*      ######    ## ## ##  ##    ##  ##        ##           ##         *
*      ##        ##  ####  ##    ##  ##        ##           ##         *
*      ##        ##  ####  ##    ##  ##        ##           ##         *
*      ########  ##   ###  #######    #######  ########  ########      *
*      ########  ##    ##  ######      #####   ########  ########      *
*                                                                      *
************************************************************************
**    Author:  Brian Knight                          November 1981    **
***********************************************************************/



SECTION "ENDCLI"

GET "g/libhdr.h"
GET "g/clihdr.h"
GET "g/iohdr.h"

LET start() BE
    $(

    UNLESS cli_interactive
    THEN error("%s is only legal in interactive mode*N", cli_commandname)

    cli_standardinput!Scb_end := 0 // Cause it to be exhausted
    cli_background := TRUE
    writef("CLI task %N ending*N", taskid)
    $)


AND error(f, a) BE $( writef(f, a); stop(20) $)
