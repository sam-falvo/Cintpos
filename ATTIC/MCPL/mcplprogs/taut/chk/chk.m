/*
(c) Martin Richards  22 December 1997

Modification record:
30/10/97  Initial implementation.


This program is an experimental tautology checker.  It is also an
experiment in using an object oriented style of program in MCPL.

The whole program is organised as a collection of objects which
communicate between themselves using the MCPL methed calling mechanism;

    Method#(object, arg1, arg2,...).

The modules that make up this program are:

    module     source     description

    chk        chk.m      The main steering program
    rnobj      rnobj.m    The tautology checker module
                          Currently r3obj.m or r8obj.m
    tgobj      tgobj.m    The  term generator

               chk.h      The common header file giving
                          the inter-module interface

The tautology checker itself is the object (rnobj), whose methods are:

Rn_Close#(rnobj)

    Close down the checker object, by freeing all its work space, but
    not its code.

Rn_AddTerm3#(rnobj, rel, x, y, z)

    Add the 3 variable term [rel, x, y, z] to the current set of terms.

Rn_AddTerm4#(rnobj, rel, x, y, z)
Rn_AddTerm5#(rnobj, rel, v3, v2,v1,v0)
Rn_AddTerm8#(rnobj, r7,r6,r5,r4,r3,r2,r1,r0, v7,v6,v5,v4,v3,v2,v1,v0)

    Add terms of other sizes to the term set.

Rn_PrTerms#(rnobj)

    Print the current set of terms.

Rn_PrMapping#(rnobj)

    Print the current variable mapping.

Rn_Compact#(rnobj)

    Compact the set of terms.

Rn_Check#(rnobj)

    Run the checking algorithm on the current set of terms. 
    It returns:    0 if error occured
                   1 if they are satisfiable
                   2 if they are not satisfiable
                   3 if unable to decide


A second object (tgobj) generates terms, possibly by reading data
from file. Its methods are:

Tg_Close#(termgen)

    Close down the term generator object, by freeing its work space,
    but not its code.

Tg_Gen#(termgen, a, b)

    Send terms for test (a,b) to the rnobj.

   

The main program (chk) provides a simple command language interface.
The commands take arguments using the MCPL rdargs mechanism. The
commands are as follows:

help
    Summarise the available commands.

g   "A1,A2"
    Get termgen to send terms to checker. A1 and A2 are option arguments
    passed to termgen.

compact
    Compact both the terms and the variables.

u
    Apply the unit rule to every term.

pair
    Apply the pair rule to every applicable pair of terms.

c
    Run the checker on the current set of terms.

t
    Print the current set of terms

m
    print the variable mapping.

to "FILE"
    Send output subsequent commands to specified file. Without an
    argument, it closes the file, if any, and send subsequent output to the 
    terminal.

q
    Terminate the program.

*/

GET "chk.h"

STATIC r2objseg, rnobjseg, tgobjseg,                 // code segments
       r2objfilename, rnobjfilename, tgobjfilename,  // code segment filenames
       tmax,                          // The maximum number of terms
       tostream=0,
       stdout


FUN bug : mess, a,b,c,d => writef(mess, a,b,c,d)
                           abort 999


FUN start : =>
  writes "Checker (22 Dec 97)\n"

  debug := 0

  LET argv = VEC 50

  UNLESS rdargs("TMAX,RN/K,TGEN/K", argv, 50) DO
  { writes "Bad arguments for: chk\n"
    RETURN 20
  }

  tmax := 50000
  IF argv!0 DO tmax := str2numb(argv!0)

  stdout := output()

  r2obj := 0
  r2objseg := 0
  r2objfilename := "r2obj"

  rnobj := 0
  rnobjseg := 0
  rnobjfilename := "r8obj"
  IF argv!1 DO rnobjfilename := argv!1

  tgobj := 0
  tgobjseg := 0
  tgobjfilename := "tgobj"
  IF argv!2 DO tgobjfilename := argv!2

  writef("Using RN %s  TGEN %s  and TMAX %d\n", 
         rnobjfilename, tgobjfilename, tmax)

  rnobjseg := loadseg r2objfilename
  UNLESS rnobjseg & globin r2objseg DO
  { writef("Unable to load %s\n", r2objfilename)
    RETURN 20
  }

  rnobjseg := loadseg rnobjfilename
  UNLESS rnobjseg & globin rnobjseg DO
  { writef("Unable to load %s\n", rnobjfilename)
    RETURN 20
  }

  tgobjseg := loadseg tgobjfilename
  UNLESS tgobjseg & globin tgobjseg DO
  { writef("Unable to load %s\n", tgobjfilename)
    RETURN 20
  }

  r2obj := mkR2obj()
  rnobj := mkRnobj()
  tgobj := mkTgobj()

// testR2()
// RETURN 0

  do_commands() HANDLE : E_FalseTermFound => writef "False term Found\n"
                       : E_NoTerms        => writef "No Terms\n"
                       : E_Syntax         => writef "Syntax error\n"
                       : E_Space, mess, a => writef(mess, a)
                       .


  Tg_Close#(tgobj)
  Rn_Close#(rnobj)
  R2_Close#(r2obj)
  IF r2objseg DO unloadseg r2objseg
  IF rnobjseg DO unloadseg rnobjseg
  IF tgobjseg DO unloadseg tgobjseg
  IF tostream DO { selectoutput tostream
                   endwrite()
                   selectoutput stdout
                 }

  RETURN 0

FUN eqstr : s, t => compstring(s, t) = 0

FUN do_commands : =>
  LET ch = '\n'

{ LET argv = VEC 100

  IF ch='\n' DO writes "chk> "

  LET item = rditem(argv, 100)

  MATCH item
  : 0     => RETURN  // Endstreamch

  : 3 | 4 => LOOP    // '\n' ';' or endstreamch

  : 1 | 2 => // Ordinary item, or quoted item
       IF eqstr(argv, "help")    DO { doHelp   (argv, 100)  // help
                                      EXIT
                                    }
       IF eqstr(argv, "ge")      DO { doGe     (argv, 100)  // ge
                                      EXIT
                                    }
       IF eqstr(argv, "gd")      DO { doGd     (argv, 100)  // gd
                                      EXIT
                                    }
       IF eqstr(argv, "g")       DO { doG      (argv, 100)  // g
                                      EXIT
                                    }
       IF eqstr(argv, "compact") DO { doCompact(argv, 100)  // compact
                                      EXIT
                                    }
       IF eqstr(argv, "u")       DO { doUnit   (argv, 100)  // u
                                      EXIT
                                    }
       IF eqstr(argv, "p")       DO { doPair   (argv, 100)  // p
                                      EXIT
                                    }
       IF eqstr(argv, "c")       DO { doCheck  (argv, 100)  // c
                                      EXIT
                                    }
       IF eqstr(argv, "t")       DO { doPrTerms(argv, 100)  // t
                                      EXIT
                                    }
       IF eqstr(argv, "map")     DO { doPrMap  (argv, 100)  // map
                                      EXIT
                                    }
       IF eqstr(argv, "q")       DO { writef "Quitting\n"   // q
                                      RETURN
                                    }
       IF eqstr(argv, "d")       DO { doDebug  (argv, 100)  // d
                                      EXIT
                                    }
       IF eqstr(argv, "to")      DO { doTo     (argv, 100)  // to
                                      EXIT
                                    }
       IF eqstr(argv, "not")     DO { doNot    (argv, 100)  // not
                                      EXIT
                                    }
       IF eqstr(argv, "and")     DO { doAnd    (argv, 100)  // and
                                      EXIT
                                    }
       IF eqstr(argv, "or")      DO { doOr     (argv, 100)  // or
                                      EXIT
                                    }
       IF eqstr(argv, "nand")    DO { doNand   (argv, 100)  // nand
                                      EXIT
                                    }
       IF eqstr(argv, "nor")     DO { doNor    (argv, 100)  // nor
                                      EXIT
                                    }
       IF eqstr(argv, "imp")     DO { doImp    (argv, 100)  // imp
                                      EXIT
                                    }
       IF eqstr(argv, "eqv")     DO { doEqv    (argv, 100)  // eqv
                                      EXIT
                                    }
       IF eqstr(argv, "xor")     DO { doXor    (argv, 100)  // xor
                                      EXIT
                                    }

       writef("Unknown command: %s\n", argv)

  : => RAISE 10
  .

  unrdch()          // Ignore text until newline or ';'
  ch := rdch()
  UNTIL ch='\n' OR ch=';' OR ch=Endstreamch DO ch := rdch()

} REPEAT

FUN doHelp :    =>
  newline()
  writes "help         print this text\n\
         \ge   exp     generate terms from immediate expression\n\
         \gd   file    generate terms from given file (dimacs format)\n\
         \g    n       generate terms for internal expression n\n\
         \compact      compact the terms and variables\n\
         \u            apply unit rules once\n\
         \p            apply pair rules once\n\
         \c            run the checker\n\
         \t            print the current set of terms\n\
         \map          print the current mapping\n\
         \q            leave the system\n\
         \d    n       set the debug level\n\
         \to   file    spool to specified file\n\
         \to           close the spool file\n\
         \not  x y     add term [NotY x y 0]\n\
         \and  x y z   add term [And  x y z]\n\
         \or   x y z   add term [Or   x y z]\n\
         \nand x y z   add term [Nand x y z]\n\
         \nor  x y z   add term [nor  x y z]\n\
         \imp  x y z   add term [Imp  x y z]\n\
         \eqv  x y z   add term [Eqv  x y z]\n\
         \xor  x y z   add term [Xor  x y z]\n"
  newline()

FUN doGe   :    v, n => 
  IF tostream DO selectoutput tostream
  TEST rdargs("EXP/A", v, n)
  THEN { IF rnobj DO Rn_Close#(rnobj)
         rnobj := mkR2obj()
         Tg_Gen#(tgobj, 0, v!0)
       }
  ELSE writes "Bad command\n"
  selectoutput stdout

FUN doGd   :    v, n => 
  IF tostream DO selectoutput tostream
  TEST rdargs("FILE/A", v, n)
  THEN { IF rnobj DO Rn_Close#(rnobj)
         rnobj := mkR2obj()
         Tg_Gen#(tgobj, 2, v!0)
       }
  ELSE writes "Bad command\n"
  selectoutput stdout

FUN doG    :    v, n => 
  IF tostream DO selectoutput tostream
  TEST rdargs("N/A", v, n)
  THEN { IF rnobj DO Rn_Close#(rnobj)
         rnobj := mkR2obj()
         Tg_Gen#(tgobj, 1, str2numb(v!0))
       }
  ELSE writes "Bad command\n"
  selectoutput stdout

FUN doCompact : v, n =>
  IF tostream DO selectoutput tostream
  MATCH Rn_Compact#(rnobj)
  : 0 => writes "-----Error\n"
  : 1 => writes "-----The terms are satisfiable\n"
  : 2 => writes "-----The terms cannot be satisfied\n"
  : 3 => writes "-----Unable to decide yet\n"
  :   => writes "Unexpected result from rnobj\n"
  .
  selectoutput stdout

FUN doUnit : v, n =>
  IF tostream DO selectoutput tostream
  MATCH Rn_Unit#(rnobj)
  : 0 => writes "-----Error\n"
  : 1 => writes "-----The terms are satisfiable\n"
  : 2 => writes "-----The terms cannot be satisfied\n"
  : 3 => writes "-----Unable to decide yet\n"
  :   => writes "Unexpected result from rnobj\n"
  .
  selectoutput stdout

FUN doPair : v, n =>
  IF tostream DO selectoutput tostream
  MATCH Rn_Pair#(rnobj)
  : 0 => writes "-----Error\n"
  : 1 => writes "-----The terms are satisfiable\n"
  : 2 => writes "-----The terms cannot be satisfied\n"
  : 3 => writes "-----Unable to decide yet\n"
  :   => writes "Unexpected result from rnobj\n"
  .
  selectoutput stdout

FUN doCheck :   v, n =>
  IF tostream DO selectoutput tostream
  MATCH Rn_Check#(rnobj)
  : 0 => writes "-----Error\n"
  : 1 => writes "-----The terms are satisfiable\n"
  : 2 => writes "-----The terms cannot be satisfied\n"
  : 3 => writes "-----Unable to decide yet\n"
  :   => writes "Unexpected result from rnobj\n"
  .
  selectoutput stdout

FUN doPrTerms : v, n =>
  IF tostream DO selectoutput tostream
  Rn_PrTerms#(rnobj)
  selectoutput stdout

FUN doPrMap :   v, n =>
  IF tostream DO selectoutput tostream
  R2_Print#(r2obj)
  selectoutput stdout

FUN doDebug :   v, n =>
  TEST rdargs("N", v, n)
  THEN { IF v!0 DO debug := str2numb(v!0)
         writef("debug level %d\n", debug)
       }
  ELSE writes "Bad command\n"

  

FUN doTo :      v, n =>
  TEST rdargs("TO", v, n)
  THEN { IF tostream DO { selectoutput tostream
                          endwrite()
                          selectoutput stdout
                          writef "TO stream closed\n"
                          tostream := 0
                        }
         UNLESS v!0 RETURN
         tostream := findoutput(v!0)
         TEST tostream THEN writef("Spooling to: %s\n", v!0)
                       ELSE writef("Can't open file: %s\n", v!0)
       }
       
  ELSE writes "Bad command\n"
  

FUN doNot :     v, n =>
  TEST rdargs("X/A,Y/A", v, n)
  THEN Rn_AddTerm3#(rnobj,
                    NotY, str2numb(v!0), str2numb(v!1), 0)
  ELSE writes "Bad command\n"

FUN doAnd :     v, n =>
  TEST rdargs("X/A,Y/A,Z/A", v, n)
  THEN Rn_AddTerm3#(rnobj,
                    And, str2numb(v!0), str2numb(v!1), str2numb(v!2))
  ELSE writes "Bad command\n"

FUN doOr :      v, n =>
  TEST rdargs("X/A,Y/A,Z/A", v, n)
  THEN Rn_AddTerm3#(rnobj,
                    Or, str2numb(v!0), str2numb(v!1), str2numb(v!2))
  ELSE writes "Bad command\n"

FUN doNand :     v, n =>
  TEST rdargs("X/A,Y/A,Z/A", v, n)
  THEN Rn_AddTerm3#(rnobj,
                    Nand, str2numb(v!0), str2numb(v!1), str2numb(v!2))
  ELSE writes "Bad command\n"

FUN doNor :      v, n =>
  TEST rdargs("X/A,Y/A,Z/A", v, n)
  THEN Rn_AddTerm3#(rnobj,
                    Nor, str2numb(v!0), str2numb(v!1), str2numb(v!2))
  ELSE writes "Bad command\n"

FUN doImp :     v, n =>
  TEST rdargs("X/A,Y/A,Z/A", v, n)
  THEN Rn_AddTerm3#(rnobj,
                    Imp, str2numb(v!0), str2numb(v!1), str2numb(v!2))
  ELSE writes "Bad command\n"

FUN doEqv :     v, n => 
  TEST rdargs("X/A,Y/A,Z/A", v, n)
  THEN Rn_AddTerm3#(rnobj,
                    Eqv, str2numb(v!0), str2numb(v!1), str2numb(v!2))
  ELSE writes "Bad command\n"

FUN doXor :     v, n => 
  TEST rdargs("X/A,Y/A,Z/A", v, n)
  THEN Rn_AddTerm3#(rnobj,
                    Xor, str2numb(v!0), str2numb(v!1), str2numb(v!2))
  ELSE writes "Bad command\n"


