/*
This program converts the numeric representation of BBC OCODE
into a more readable form.

Modified by Martin Richards (c) April 2017
*/

SECTION "procode"

GET "libhdr"
GET "ccghdr.h"

LET start() = VALOF
{ LET argv = VEC 20
  LET ocodein = ?
  AND ocodeprn = 0
  LET sysprint = output()
  IF rdargs("FROM,TO/K", argv, 20)=0 DO
  { writes("Bad args for procode*n")
    RESULTIS 20
  }
  IF argv!0=0 DO argv!0 := "OCODE$$"
  IF argv!1=0 DO argv!1 := "**"
  ocodein := findinput(argv!0)
  IF ocodein=0 DO
  { writef("Trouble with file %s*n", argv!0)
    RESULTIS 20
  }
  ocodeprn := findoutput(argv!1)
   
  IF ocodeprn=0 DO
  { writef("Trouble with file %s*n", argv!1)
    RESULTIS 20
  }
   
  writef("Converting %s to %s*n", argv!0, argv!1)
  selectinput(ocodein)
  selectoutput(ocodeprn)
  scan()
  endread()
  UNLESS ocodeprn=sysprint DO endwrite()
  selectoutput(sysprint)
  writef("Conversion complete*n")
  RESULTIS 0
}

// argument may be of form Ln
AND rdn() = VALOF
{ LET x = rdn1()
  //sawritef("rdn() => %i5*n", x)
  RESULTIS x
}

AND rdn1() = VALOF
{ LET n, res = binrdch(), ?
  IF (n & 128) = 0 RESULTIS n 
  res := n & 127

  { n := binrdch()
    res := (res << 7) + (n & 127)
    IF n=-1 RESULTIS 0
  } REPEATWHILE (n & 128) ~= 0
  RESULTIS res
}

// Read in a global number.
AND scan() BE
{ LET ocodeop = rdn()
  LET op0, op1, op2, op1l, len = 0, 0, 0, 0, -1

  SWITCHON ocodeop INTO

  { DEFAULT:         writef("Bad OCODE op %n*n", ocodeop)
                     abort(1000)
                     LOOP

    CASE 0:          RETURN
      
    CASE s.section:  op0, len := "SECTION", rdn(); ENDCASE
    CASE s.needs:    op0, len := "NEEDS",   rdn(); ENDCASE

    CASE s.lp:       op1 := "LP";            ENDCASE
    CASE s.lg:       op1 := "LG";            ENDCASE
    CASE s.ln:       op1 := "LN";            ENDCASE

    CASE s.lstr:     op0, len := "LSTR", rdn(); ENDCASE

    CASE s.true:     op0 := "TRUE";          ENDCASE
    CASE s.false:    op0 := "FALSE";         ENDCASE

    CASE s.llp:      op1 := "LLP";           ENDCASE
    CASE s.llg:      op1 := "LLG";           ENDCASE

    CASE s.sp:       op1 := "SP";            ENDCASE
    CASE s.sg:       op1 := "SG";            ENDCASE

    CASE s.lf:       op1l := "LF";           ENDCASE
    CASE s.ll:       op1l := "LL";           ENDCASE
    CASE s.lll:      op1l := "LLL";          ENDCASE
    CASE s.sl:       op1l := "SL";           ENDCASE
      
    CASE s.stind:    op0 := "STIND";         ENDCASE

    CASE s.rv:       op0 := "RV";            ENDCASE

    CASE s.mult:     op0 := "MULT";          ENDCASE
    CASE s.div:      op0 := "DIV";           ENDCASE
    CASE s.rem:      op0 := "REM";           ENDCASE
    CASE s.plus:     op0 := "PLUS";          ENDCASE
    CASE s.minus:    op0 := "MINUS";         ENDCASE
    CASE s.eq:       op0 := "EQ";            ENDCASE
    CASE s.ne:       op0 := "NE";            ENDCASE
    CASE s.ls:       op0 := "LS";            ENDCASE
    CASE s.gr:       op0 := "GR";            ENDCASE
    CASE s.le:       op0 := "LE";            ENDCASE
    CASE s.ge:       op0 := "GE";            ENDCASE
    CASE s.lshift:   op0 := "LSHIFT";        ENDCASE
    CASE s.rshift:   op0 := "RSHIFT";        ENDCASE
    CASE s.logand:   op0 := "LOGAND";        ENDCASE
    CASE s.logor:    op0 := "LOGOR";         ENDCASE
    CASE s.eqv:      op0 := "EQV";           ENDCASE
    CASE s.neqv:     op0 := "NEQV";          ENDCASE
    CASE s.not:      op0 := "NOT";           ENDCASE
    CASE s.neg:      op0 := "NEG";           ENDCASE
    CASE s.abs:      op0 := "ABS";           ENDCASE

    CASE s.endfor:   op1l := "ENDFOR";       ENDCASE
    CASE s.jt:       op1l := "JT";           ENDCASE
    CASE s.jf:       op1l := "JF";           ENDCASE

    CASE s.goto:     op0 := "GOTO";          ENDCASE

    CASE s.xlab:     op1l := "XLAB";         ENDCASE

    CASE s.lab:      op1l := "LAB";          ENDCASE

    CASE s.query:    op0 := "QUERY";         ENDCASE

    CASE s.stack:    op1 := "STACK";         ENDCASE

    CASE s.store:    op0 := "STORE";         ENDCASE

    CASE s.entry:    { LET lab = ?
                       len := rdn()
                       lab := rdn()
                       writef("ENTRY L%n", lab)
                       ENDCASE
                     }

    CASE s.save:     op1 := "SAVE";          ENDCASE

    CASE s.fnap:     op1 := "FNAP";          ENDCASE
    CASE s.rtap:     op1 := "RTAP";          ENDCASE

    CASE s.fnrn:     op0 := "FNRN";          ENDCASE
    CASE s.rtrn:     op0 := "RTRN";          ENDCASE

    CASE s.endproc:  op1 := "ENDPROC";       ENDCASE

    CASE s.res:      op1l := "RES";          ENDCASE
    CASE s.jump:     op1l := "JUMP";         ENDCASE

    CASE s.rstack:   op1 := "RSTACK";        ENDCASE

    CASE s.finish:   op0 := "FINISH";        ENDCASE

    CASE s.switchon: { LET n = rdn()
                       writef("SWITCHON %n L%n*n", n, rdn())
                       FOR i = 1 TO n DO
                       { writef("%i8   ", rdn())
                         writef("L%n*n", rdn())
                       }
                       newline()
                       LOOP
                     }

    CASE s.getbyte:  op0 := "GETBYTE";       ENDCASE
    CASE s.putbyte:  op0 := "PUTBYTE";       ENDCASE

    CASE s.global:   { LET n = rdn()
                       writef("GLOBAL %n*n", n)
                       FOR i = 1 TO n DO
                       { writef("%i8   ", rdn())
                         writef("L%n*n", rdn())
                       }
                       newline()
                       LOOP
                     }


    CASE s.datalab:  op1l := "DATALAB";      ENDCASE
    CASE s.itemn:    op1  := "ITEMN";        ENDCASE
  }

  UNLESS op0=0   DO writef("%S",     op0)
  UNLESS op1=0   DO writef("%S %n",  op1,  rdn())
  UNLESS op2=0   DO writef("%S %n %n",  op2,  rdn(), rdn())
  UNLESS op1l=0  DO writef("%S L%n", op1l, rdn())

  IF len>=0 DO { // Write a string of len characters
                 writef(" %n ", len)
                 FOR i = 1 TO len DO
                 { LET ch = rdn()
                   IF i REM 15 = 0 DO newline()
                   TEST 32<=ch<=127 THEN writef(" '%c'", ch)
                                    ELSE writef(" %i3 ", ch)
                 }
               }

  newline()
} REPEAT



