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
**                                                                    **
**    4 Dec 01  MR           Slightly modified                        **
***********************************************************************/


// Program to hold (typically another) task.

SECTION "hold"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 20
  LET task = ?

  UNLESS rdargs("TASK/N/A", argv, 20) DO
  { writes("Bad args*n")
    stop(20)
  }

  task := !(argv!0)

  UNLESS hold(task) DO
  { writef("Unable to hold task %n*n", task)
    stop(20)
  }
  RESULTIS 0
}

