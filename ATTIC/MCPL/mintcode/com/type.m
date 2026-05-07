// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

MODULE type

GET "mcpl.h"

GLOBAL
  inputstream:Ug,
  outputstream,
  numbers,
  linenumber

MANIFEST
  Exit=100


FUN start : =>
  LET argv = VEC 50
  LET rc = 0
  LET ch = 0
  LET oldoutput = output()

  inputstream := 0
  outputstream := 0

  { UNLESS rdargs("FROM/A,TO,N/S", argv, 50) DO
    { writes "Bad args\n"
      rc := 20
      RAISE Exit
    }

    inputstream := findinput(argv!0)
    IF inputstream = 0 DO { writef("Can\'t open %s\n", argv!0)
                             rc := 20
                             RAISE Exit
                          }
    selectinput inputstream

    IF argv!1 DO { outputstream := findoutput(argv!1)
                   UNLESS outputstream DO
                   { writef("Can*'t open %s\n", argv!1)
                     rc := 20
                     RAISE Exit
                   }
                   selectoutput outputstream
                 }

    numbers := argv!2

    linenumber := 1

    { LET tab = 0

      { ch := rdch()
        IF intflag() DO { IF tab DO wrch '\n'
                          selectoutput oldoutput
                          writes "*BREAK\n"
                          rc := 5
                          RAISE Exit
                        }
        IF tab=0 DO { IF ch=Endstreamch RAISE Exit
                      IF numbers DO writef("%5d  ", linenumber)
                    }
        MATCH ch
        : '\c'|'\n'|'\p'=> linenumber++; wrch(ch); BREAK
 
        : '\e'          => linenumber++; tab := 8

        : '\t'          => wrch '\s' REPEATWHILE ++tab MOD 8

        : Endstreamch   => wrch '\n';            RAISE Exit
        :               => wrch ch;              tab++
        .
      } REPEAT
    } REPEAT
  } HANDLE : Exit => .

  IF inputstream  DO { selectinput inputstream;   endread()  }
  IF outputstream DO { selectoutput outputstream; endwrite() }
  RETURN rc
.

