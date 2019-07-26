/***********************************************************************
**             (C) Copyright 2005  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************

  ######    ########  ##        ########    ####     ######   ##    ## 
  #######   ########  ##        ########   ######   ########  ##   ##  
  ##    ##  ##        ##           ##     ##    ##  ##        ##  ##   
  ##    ##  ######    ##           ##     ########  #######   ####     
  ##    ##  ##        ##           ##     ##    ##        ##  ## ##    
  ##    ##  ##        ##           ##     ##    ##        ##  ##  ##   
  #######   ########  ########     ##     ##    ##  ########  ##   ##  
  ######    ########  ########     ##     ##    ##   ######   ##    ## 

************************************************************************
**    Author:   Martin Richards                      August 2005      **
**                                                                    **
***********************************************************************/


// Program to delete (typically another) task.

SECTION "deletetask"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 20
  LET taskid = ?

  UNLESS rdargs("task/a", argv, 20) DO
  { writes("Bad args*n")
    stop(20)
  }

  TEST string.to.number(argv!0)
  THEN taskid := result2
  ELSE { writef("Bad number *"%s*"*n", argv!0); stop(20) }

//sawritef("*nAbout to delete task %n*n", taskid)
//abort(1000)
  UNLESS deletetask(taskid) DO
  { LET res2 = result2
    writef("Unable to deletetask task %n, ",
            taskid, result2)
    SWITCHON res2 INTO
    { DEFAULT:  writef("result2 = %n*n", res2)
                ENDCASE
      CASE 101: writef("task does not exist*n")
                ENDCASE
      CASE 108: writef("task not deletable*n")
                ENDCASE
    }
    stop(20)
  }
  RESULTIS 0
}

