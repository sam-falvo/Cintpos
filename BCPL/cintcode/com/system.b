// This command identifies the currently running system

GET "libhdr"

LET start() = VALOF
{ SWITCHON rootnode!rtn_system INTO
  { DEFAULT:   writef("The current system is Unknown*n")
               ENDCASE

    CASE 1:    writef("The current system is Cintsys*n")
               ENDCASE

    CASE 2:    writef("The current system is Cintpos*n")
               ENDCASE
  }

  RESULTIS 0
}
