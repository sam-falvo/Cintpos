/***********************************************************************
**             (C) Copyright 1980  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************

                           ######   ########  ######## 
                          ########  ########  ######## 
                          ##        ##           ##    
                          #######   ######       ##    
                                ##  ##           ##    
                                ##  ##           ##    
                          ########  ########     ##    
                           ######   ########     ##    

                ########  ##          ####     ######    ######  
                ########  ##         ######   ########  ######## 
                ##        ##        ##    ##  ##        ##       
                ######    ##        ########  ##  ####  #######  
                ##        ##        ##    ##  ##    ##        ## 
                ##        ##        ##    ##  ##    ##        ## 
                ##        ########  ##    ##  ########  ######## 
                ##        ########  ##    ##   ######    ######  

************************************************************************
**    Author:   Martin Richards                    September 2003     **
**                                                                    **
***********************************************************************/


// Program to set the flags of a specified task.

SECTION "setflags"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 20
  LET task = taskid
  LET flags = 0

  UNLESS rdargs("task,a/s,b/s,c/s,d/s,e/s,quiet/s", argv, 20) DO
  { writes("Bad args*n")
    stop(20)
  }

  IF argv!0 TEST string_to_number(argv!0)
            THEN task := result2
            ELSE { writef("Bad number *"%s*"*n", argv!0); stop(20) }

  IF argv!1 DO flags := flags | flag_a
  IF argv!2 DO flags := flags | flag_b
  IF argv!3 DO flags := flags | flag_c
  IF argv!4 DO flags := flags | flag_d
  IF argv!5 DO flags := flags | flag_e

  UNLESS setflags(task, flags) DO
  { writef("Task %n does not exist*n", task)
    RESULTIS 0
  }

  flags := result2

  UNLESS argv!6 DO
  { writef("Previous setting:")
    UNLESS (flags&flag_a)=0 DO writes(" A")
    UNLESS (flags&flag_b)=0 DO writes(" B")
    UNLESS (flags&flag_c)=0 DO writes(" C")
    UNLESS (flags&flag_d)=0 DO writes(" D")
    UNLESS (flags&flag_e)=0 DO writes(" E")
    newline()
  }

  RESULTIS 0
}

