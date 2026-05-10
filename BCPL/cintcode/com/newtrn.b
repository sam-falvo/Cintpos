
// Althought SYN and TRN were combined into one section, they have now
// been separated because the combine section became to large to Cintcode.

SECTION "TRN"

GET "libhdr"
GET "bcplfecg"

// Global declarations for TRN

GLOBAL  {
trnext:trng
trans; destlabel; declnames; decldyn
genjumpotodest
declstat; checkdistinct; addname; cellwithname
transdef; scanlabel
transmatchlist
transnext; transexit
decllabels; undeclare
jumpcond; transswitch; transfor
assop2op; op2sfop; cv2flt; rel2patrel; patrel2rel
assign; load; fnbody; loadlv; loadlist
isflt; isconst; iszero; evalconst; transname; xref
genlab; labnumber
newblk
dvec; dvece; dvecp; dvect
caselist      // Linked list of case constants of the current switch
casecount     // If =-1 CASE and DEFAULT labels are illegal,
              // but ENDCASE may be allowed, eg in a loop in a switch.
		    
endcaselab    // If =-2 ENDCASE are illegal
              // If =-1 ENDCASE compiles as RTRN
	      // If >=0 ENDCASE compiles as jump endcaselab,
	      // allocating the label if necessary.
	      
defaultlab    // If casecount<0 DEFAuLT is illegal
              // If in a switch defaultlab will be a an
	      // allocated label.

resultlab     // =-2 RESULTIS illegal
              // RESULTIS is legal in repetitive commands within
	      // the current function.
              // If =-1 RESULTIS compiles as
	      // load the result followed by FNRN
	      // This is used when a VALOF expression is the body
	      // of a function.
	      // If >=0 RESULTIS compiles as
	      // load the result followed by RES resultlab
	      // allocating resultlab if necessary.
	      // If the matchcontext is s_patfndef, match item
	      // expressions are treated as operands of RESULTIS.

looplab       // If =-2 LOOP is illegal
              // If >=0 LOOP copiles as a jump
	      // allocating looplab if necessary.

breaklab      // If =-2 BREAK illegal
              // If =-1 BREAK compiles as RTRN
	      // If >=0 BREAK compiles as a jump
	      // allocating breaklab if necessary.

nextlab       // Label of the next match item, if any
              // If =-2 NEXT is illegal
              // If >=0 NEXT compiles as a jump
	      // allocating nextlab if necessary.
	
exitlab       // Label for the end of a match list
              // If =-2 EXIT is illegal
              // If >=0 EXIT compiles as a jump
	      // allocating exitlab if necessary.

context; comline; procname
matchcontext // Equals s_patfndef, s_patrtdef,
             //        s_matche, s_matche, s_everye, s_everyc,
	     // otherwise zero
patresultpos // Position of the result of an MATCH and EVERY expressions.
ssp; vecssp
gdeflist; gdefcount
outstring; out1; out2; out3; out4
floatingchk    // Made a global to avoid relative address out of range error

lasttrnglobal // Used to check for global overlap with cgg

}


LET genlab() = VALOF
{ labnumber := labnumber + 1
  RESULTIS labnumber
}
 
AND trnerr(mess, a, b) BE
{ LET fno = comline>>20
  LET lno = comline & #xFFFFF
  LET filename = sourcenamev!fno
  writes("Error ")
  UNLESS procname=0 DO writef("in %s ", @h3!procname)
  writef("near ")
  IF filename DO writef("%s", filename)
  writef("[%n]: ", lno)
  writef(mess, a, b)
  newline()
  IF hard DO abort(1000)
  errcount := errcount + 1
  IF errcount >= errmax DO { writes("*nCompilation aborted*n")
                             longjump(fin_p, fin_l)
                           }
}

AND newblk(x, y, z) = VALOF
{ // The is used for global and case lists 
  LET p = dvect - 3
  IF dvece>p DO { errmax := 0        // Make it fatal.
                  trnerr("More workspace needed")
                }
  p!0, p!1, p!2 := x, y, z
  dvect := p
  RESULTIS p
}

AND translate(x) BE
{ // First check that the trn globals do not overlap with
  // the codegenerator globals.
  LET lasttrngn = @lasttrnglobal - @glob0
  
  IF debug>0 DO writef("lasttrngn=%i3   cgg=%i3*n", lasttrngn, cgg)

  IF lasttrngn>=cgg DO
  { writef("SYSTEM ERROR: lasttrngn=%i3   cgg=%i3*n", lasttrngn, cgg)
    RESULTIS TRUE
  }
 
  dvec,  dvect := treevec, treep
  h1!dvec, h2!dvec, h3!dvec, h4!dvec := 0, 0, 0, 0
  dvece := dvec+4
  dvecp := dvece
//selectoutput(sysprint)

  // Clear the h2 field of all names in the name table
  // This will be used to the dvec cell for the current declaration
  // of the name.
  // Note that the size of dvec cells has increased to 4 to allow
  // for path variables in the pattern matching extension.
  
  FOR i = 0 TO nametablesize-1 DO
  { LET name = nametable!i // The first name in hash list i
    UNTIL name=0 DO
    { LET next = h2!name
      h2!name := 0 // Mark undeclared
//   writef("Undeclare %s*n", name+2)
      name := next
    }
  }

  // Initialise all the translation variable
  gdeflist, gdefcount := 0, 0
  caselist := 0
  casecount := -1 // CASE and DEFAULT illegal
  defaultlab := 0
  resultlab, breaklab, looplab, endcaselab := -2, -2, -2, -2
  nextlab, exitlab := -2, -2
  matchcontext, patresultpos := 0, 0
  context, comline, procname, labnumber := 0, 1, 0, 0
  ssp, vecssp := savespacesize, savespacesize

  WHILE x~=0 & (h1!x=s_section | h1!x=s_needs) DO
  { LET op, a = h1!x, h2!x
    out1(op)
    outstring(@h2!a)
    x:=h3!x
  }

  trans(x, 0)
  out2(s_global, gdefcount)
  UNTIL gdeflist=0 DO { out2(h2!gdeflist, h3!gdeflist)
                        gdeflist := h1!gdeflist
                      }  
}

LET trnext(next) BE { IF next<0 DO out1(s_rtrn)
                      IF next>0 DO out2(s_jump, next)
                    }
 
LET trans(x, next) BE
// x       is the parse tree of the command to translate
// next<0  compile x followed by RTRN
// next>0  compile x followed by JUMP next
// next=0  compile x only
{ LET op, sfop, sw, ff = ?, ?, FALSE, FALSE

  IF x=0 DO { trnext(next) // Compile RTRN, a jump or nothing
              RETURN
	    }

  op := h1!x // op is the leading operator of
             // the command to translate.

  SWITCHON op INTO
  { DEFAULT: trnerr("Compiler error in Trans, op = %s",
                    opname(op))
             RETURN
 
    CASE s_let:
    { // x -> [s_let, defs, body, lm]
      LET cc = casecount
      LET e, s, s1 = dvece, ssp, 0
      LET v = vecssp
      casecount := -1 // Disallow CASE and DEFAULT labels
                      // but ENDCASE may still be allowed.
      context, comline := x, h4!x
      declnames(h2!x)
      checkdistinct(e)
      vecssp, s1 := ssp, ssp
      ssp := s
      context, comline := x, h4!x
      transdef(h2!x)
      UNLESS ssp=s1 DO trnerr("Lhs and rhs do not match")
      UNLESS ssp=vecssp DO { ssp := vecssp; out2(s_stack, ssp) }
      out1(s_store)
      decllabels(h3!x)
      trans(h3!x, next)
      vecssp := v
      UNLESS ssp=s DO out2(s_stack, s)
      ssp := s
      casecount := cc
      undeclare(e)
      RETURN
    }
 
    CASE s_static:
    CASE s_global:
    CASE s_manifest:
    { LET e, s = dvece, ssp
      AND y = h2!x
      AND n = 0
      LET prevk = 0 // The previous integer or floating point value
      LET prevt = 0 // =0, s_notflt or s_flt
      LET cc = casecount
   
      casecount := -1 // Disallow CASE and DEFAULT labels
                      // but ENDCASE may still be allowed
      context, comline := x, h4!x
 
      WHILE y DO
      { LET name = h3!y // name is a Name or Flt node.
        LET fop = op    // = s_static, s_global or s_manifest
        ff := FALSE     // ff will only be TRUE for static and manifest
                        // names with the FLT tag. If TRUE the value of
                        // the constant will be a floating point number.
			// For glbal declarations the value is an
			// integer global number.

        context, comline := y, h5!y

        // If the name is prefixed by FLT remove the prefix and
        // modify fop as follows
        //     s_static   -> s_fstatic
        //     s_manifest -> s_fmanifest
        // and s_global   -> s_fglobal

        IF h1!name=s_flt DO name, fop := h2!name, op | s_fltbit

        // If fop is s_fstatic or s_fmanifest the constant
        // expression in evaluated in an FLT context, in all
        // other cases it is evaluated in a non FLT context.

        IF fop=s_fstatic | fop=s_fmanifest DO ff := TRUE

        TEST h4!y
        THEN { //sawritef("Calling evalconst*n")
               //plist(h4!y, 0, 5)
               //newline()
               n := evalconst(h4!y, ff)
               //writef("giving n=%n*n", n)
               //abort(1000)
             }
        ELSE { // The constant expression was not given so the
               // value is chosen as follows:

//sawritef("*nname=%s fop=%i3 fopname=%10t prevt=%i3 prevk=%x8(%i3) ff=%i3*n",
//    @h3!name, fop, opname(fop), prevt, prevk, prevk, ff)
//abort(9876)
               // If there was no previous value the value is
               // 0 or 0.0.
               TEST prevt=0
               THEN { n := ff -> flt0, 0
//sawritef("Setting n=%x8(%i3)*n", n, n)
                    }
               ELSE n := VALOF SWITCHON fop INTO
	            { DEFAULT:
		        trnerr("System error in trans")
			RESULTIS 0

                      CASE s_static:
		        // For s_static    the value is 0
                        RESULTIS 0

                      CASE s_fstatic:
		        // For s_fstatic    the value is 0.0
                        RESULTIS flt0

                      CASE s_manifest:
                        // for s_manifest  the next value is one larger
                        //                 than the previous value
                        //                 converted to integer, if necessary.
                        IF prevt=s_flt DO n := sys(Sys_flt, fl_fix, n)
                        RESULTIS n+1

                      CASE s_fmanifest:
                        // for s_fmanifest the value is 1.0 larger than the
                        //                 previous value is converted to
                        //                 floating point, if necessary.
                        IF prevt=s_notflt DO n := sys(Sys_flt, fl_float, n)
                        RESULTIS sys(Sys_flt, fl_add, n, flt1)

                      CASE s_global:
                      CASE s_fglobal:
                        // For s_global and s_fglobal the next value is a
                        // global number one larger than the previous one.
                        RESULTIS n+1
                    }

             }

        // prevk is the previous value (integer or floating point).
        // prevt is s_flt if the previous value was floating point.
        //       it is s_notflt the previous value was an integer.

        prevk := n
        prevt := ff -> s_flt, s_notflt
//sawritef("setting prevk=%n prevt=%n*n", prevk, prevt)

//sawritef("name=%s fop=%i3 fopname=%10t prevt=%i3 prevk=%x8(%i3) ff=%i3 n=%x8(%i3)*n",
//    @h3!name, fop, opname(fop), prevt, prevk, prevk, ff, n, n)

        IF op=s_static DO
        { LET k = n
          n := genlab()      // n is now the label for the static variable
          out2(s_datalab, n)
          out2(s_itemn, k)
        }

        IF op=s_global UNLESS 0<=n<=65535 DO
          trnerr("Global number %n too large for: %s*n", n, @h3!name)

        // n is a global number, a manifest value or a label for a
        // static variable or entry point.
        addname(name, fop, n, 0)
        IF xrefing DO xref(name,
                           (fop=s_global->"G:",
                            fop=s_fglobal->"FG:",
                            fop=s_static->"S:",
                            fop=s_fstatic->"FS:",
                            fop=s_manifest->"M:",
                            fop=s_fmanifest->"FM:",
                            "??:"),
                           n,
                           s_constdef
                          )
        y := h2!y
      }
 
      decllabels(h3!x)
      trans(h3!x, next)
      ssp := s
      casecount := cc
      undeclare(e)
      RETURN
    }

    CASE s_matchc: // x -> [s_matchc, args, mlist, ln]
    CASE s_everyc: //   |  [s_everyc, args, mlist, ln]
    { // This has much in common with a SWITCHON command in that is
      // can select one or more of many alternative and can use a
      // selection of simple commands to escape from a match item
      // or the entire matchlist. These commands are EXIT, NEXT,
      // BREAK, LOOP, ENDCASE and RESULTIS provided the jump does
      // not leave the current function or routine. Of course
      // RETURN is allowed as a special case.

      // No result location needs to be allocated
      
      LET argpos = ssp // The position of the first MATCH or
                       // EVERY argument, if any.

      context, comline := x, h4!x

      // Do not allocate space for a result because it is not needed.

      // Load the match arguments
      loadlist(h2!x)
      out1(s_store) // Ensure that the match arguments are in memory

      // Translate the match items
      transmatchlist(op,     // Context s_matchc or s_everyc
                     h3!x,   // mlist -> [matchitemc, plist, C, link, ln]
                     argpos) // Position of the first argument
      // This point is reached if all match items fail or FAIL is
      // encountered.
      trnext(next) // Compile RTRN, a jump or nothing.
      RETURN
    }
     
    CASE s_assmul: // Assignment operators that might
    CASE s_assdiv: // be promoted to floating point.
    CASE s_assmod:
    CASE s_assadd:
    CASE s_asssub:
    CASE s_ass:
      // Note that simultaneous assignments have already been
      // replaced by sequences of simple assignments.
      // Convert op:= to #op:= if either operand has the FLT tag.
      UNLESS isflt(h3!x) | isflt(h2!x) DO
      { // Compile a non FLT assignment
        context, comline := x, h4!x
        op := assop2op(op)
        assign(h2!x, h3!x, FALSE, op)
        trnext(next)
        RETURN
      }

      // Promote op:= to #op:=
      op := cv2flt(op)
      h1!x := op
      // Fall through

    CASE s_assfmul: // The floating point assignment operators
    CASE s_assfdiv:
    CASE s_assfmod:
    CASE s_assfadd:
    CASE s_assfsub:
    CASE s_fass:
      context, comline := x, h4!x
      op := assop2op(op)
      assign(h2!x, h3!x, TRUE, op)
      trnext(next)
      RETURN

    CASE s_assvecap:  // All the other assignment operators
    CASE s_asslshift:
    CASE s_assrshift:
    CASE s_asslogand:
    CASE s_asslogor:
    CASE s_asseqv:
    CASE s_assxor:
      context, comline := x, h4!x
      op := assop2op(op)
      assign(h2!x, h3!x, FALSE, op)
      trnext(next)
      RETURN
 
    CASE s_rtap:
    { LET s = ssp
      context, comline := x, h4!x
      ssp := ssp+savespacesize
      out2(s_stack, ssp)
      loadlist(h3!x) // Load arguments in non FLT mode
      load(h2!x, FALSE)
      out2(s_rtap, s)
      ssp := s
      trnext(next)
      RETURN
    }
 
    CASE s_goto:
      context, comline := x, h3!x
      load(h2!x, FALSE)
      out1(s_goto)
      ssp := ssp-1
      RETURN
 
    CASE s_colon:
      context, comline := x, h5!x
      out2(s_lab, h4!x)
      trans(h3!x, next)
      RETURN
 
    CASE s_unless: sw := TRUE
    CASE s_if:
      context, comline := x, h4!x

      { // Optimise IF/UNLESS exp BREAK/LOOP/ENDCASE/NEXT/EXIT,
        // if possible.
        LET bodyop = h1!(h3!x) // The leading operator of the body
        LET destlab = destlabel(bodyop)
        // destlab is the destination label if the body
        // was BREAK, LOOP or ENDCASE. Otherwise it is zero.
        IF destlab>0 DO
        { jumpcond(h2!x, ~sw, destlab) //???
          trnext(next)
//sawritef("IF exp BREAK/LOOP/ENDCASE optimised*n")
          RETURN
        }
      }

      TEST next>0 THEN { jumpcond(h2!x, sw, next)
                         trans(h3!x, next)
                       }
                  ELSE { LET l = genlab()
                         jumpcond(h2!x, sw, l)
                         trans(h3!x, next)
                         out2(s_lab, l)
                         trnext(next)
                       }
      RETURN
 
    CASE s_test:
    { LET l, m = genlab(), 0
      context, comline := x, h5!x
      jumpcond(h2!x, FALSE, l)
         
      TEST next=0 THEN { m := genlab(); trans(h3!x, m) }
                  ELSE trans(h3!x, next)
                     
      out2(s_lab, l)
      trans(h4!x, next)
      UNLESS m=0 DO out2(s_lab, m)
      RETURN
    }
 
    CASE s_break:
      context, comline := x, h2!x
      IF breaklab=-2 DO trnerr("BREAK not inside a repetitive command")
      IF breaklab=-1 DO { out1(s_rtrn); RETURN }
      IF breaklab= 0 DO breaklab := genlab()
      out2(s_jump, breaklab)
      RETURN
 
    CASE s_loop:
      context, comline := x, h2!x
      IF looplab=-2 DO trnerr("LOOP not inside a repetitive command")
      IF looplab=-1 DO { out1(s_rtrn); RETURN }
      IF looplab= 0 DO looplab := genlab()
      out2(s_jump, looplab)
      RETURN

    CASE s_next:          // MR 28/08/2021
      context, comline := x, h2!x
      IF nextlab=-2 DO trnerr("NEXT not inside a match list")
      IF nextlab=-1 DO { out1(s_rtrn); RETURN }
      IF nextlab= 0 DO nextlab := genlab()
      out2(s_jump, nextlab)
      RETURN

    CASE s_exit:          // MR 28/08/2021
      context, comline := x, h2!x
      IF exitlab=-2 DO trnerr("EXIT not inside a match list")
      IF exitlab=-1 DO { out1(s_rtrn); RETURN }
      IF exitlab= 0 DO exitlab := genlab()
      out2(s_jump, exitlab)
      RETURN

    CASE s_return:
      context, comline := x, h2!x
      out1(s_rtrn)
      RETURN
 
    CASE s_skip:  // MR 05/4/06
      trnext(next)
      RETURN

    CASE s_finish:
      context, comline := x, h2!x
      out1(s_finish)
      RETURN
 
    CASE s_resultis:
      context, comline := x, h3!x
      IF resultlab=-1 DO { fnbody(h2!x, FALSE); RETURN }
      UNLESS resultlab>0 DO trnerr("RESULTIS out of context")
      load(h2!x, FALSE)
      out2(s_res, resultlab)
      ssp := ssp - 1
      RETURN
 
    CASE s_while: sw := TRUE
    CASE s_until:
    { // If next>0, label to jump next to after completing the command.
      // If next=-1 compile RTRN when the command completes.
      // otherwise compile nothing after the command.
      LET L = genlab()  // Label for start of the body
      LET M = next>0 -> next, genlab()
      // M is the Label to jump to if the body is never executed.
      LET prevbreaklab, prevlooplab = breaklab, looplab
      context, comline := x, h4!x
      breaklab := next  // Destination, possibly -1 for RTRN
                        // It describes how to compile BREAK
			// If breaklab is zero it will be allocated
			// by the first occurence of BREAK.
      looplab  := 0     // This will be alllocated by LOOP, if necessary.
      jumpcond(h2!x, ~sw, M)
      out2(s_lab, L)    // Label the start of the body
      trans(h3!x, 0)    // Zero because the body will be followed by
                        // the conditional jump code
      IF looplab DO out2(s_lab, looplab) // Only compiled if LOOP occurred.
      context, comline := x, h4!x
      jumpcond(h2!x, sw, L) // Compile the conditional jump.
      UNLESS nextlab=M DO out2(s_lab, M)
      IF breaklab>0 DO out2(s_lab, breaklab) // A BREAK command
                                             // jumped to breaklab
      trnext(next) // Possibly compile RTRN or JUMP Lnext
      breaklab, looplab := prevbreaklab, prevlooplab
      RETURN
    }
 
    CASE s_repeatwhile: sw := TRUE
    CASE s_repeatuntil:
    { LET L = genlab()
      LET prevbreaklab, prevlooplab = breaklab, looplab
      context, comline := x, h4!x
      breaklab := next // Cause BREAK to compile RTRN, JUMP Lnext
                       // or JUMP Lbreaklab with newly allocated label
      looplab := 0     // Allocated by the first LOOP, if any
      out2(s_lab, L) // Label start of body
      trans(h2!x, 0) // Zero because it is followed by the conditional jump
      UNLESS looplab=0 DO out2(s_lab, looplab)
      context, comline := x, h4!x
      jumpcond(h3!x, sw, L)

      IF breaklab>0 & breaklab~=next DO out2(s_lab, breaklab)

      trnext(next)
      breaklab, looplab := prevbreaklab, prevlooplab
      RETURN
    }
 
    CASE s_repeat:
    { LET bl, ll = breaklab, looplab
      context, comline := x, h4!x
      breaklab, looplab := next, genlab()
      out2(s_lab, looplab)

      trans(h2!x, looplab)

      //IF next=0 & breaklab>0 DO out2(s_lab, breaklab)
      IF breaklab>0 DO out2(s_lab, breaklab)

      trnext(next)
      breaklab, looplab := bl, ll
      RETURN
    }
 
    CASE s_case:
    { LET l, k, p = genlab(), ?, caselist
      context, comline := x, h4!x
      k := evalconst(h2!x, FALSE)
      IF casecount<0 DO trnerr("CASE label out of context")
      WHILE p DO
      { IF h2!p=k DO trnerr("'CASE %n:' occurs twice", k)
        p := h1!p
      }
      caselist := newblk(caselist, k, l)
      casecount := casecount + 1
      out2(s_lab, l)
      trans(h3!x, next)
      RETURN
    }
 
    CASE s_default:
      context, comline := x, h3!x
      IF casecount<0 DO trnerr("DEFAULT out of context")
      IF defaultlab DO trnerr("DEFAULT defined twice")
      defaultlab := genlab()
      out2(s_lab, defaultlab)
      trans(h2!x, next)
      RETURN
 
    CASE s_endcase:
      context, comline := x, h2!x
      IF endcaselab=-2 DO trnerr("ENDCASE not inside a SWITCHON command")
      IF endcaselab=-1 DO out1(s_rtrn)
      IF endcaselab= 0 DO endcaselab := genlab()
      out2(s_jump, endcaselab)
      RETURN
 
    CASE s_switchon:
      transswitch(x, next)
      RETURN
 
    CASE s_for:
      transfor(x, next)
      RETURN
 
    CASE s_seq:
      trans(h2!x, 0)
      x := h3!x
  }
} REPEAT

AND genjumpoptodest(op, destptr) BE
{ // op is the conditional or unconditioal Cintcode jump imstruction
  // destptr is a pointer to the destination label, namely
  // @breaklab, @looplab, @endcaselab, @nextlab, @exitlab

    IF !destptr=-2 DO trnerr("Illegal BREAK,LOOP,ENDCASE,NEXT or EXIT")
    IF !destptr=-1 DO
    { IF matchcontext=s_patfndef DO { out1(s_fnrn); RETURN }
      IF matchcontext=s_patrtdef DO { out1(s_rtrn); RETURN }
      !destptr := 0
    }
    UNLESS !destptr DO !destptr := genlab()
    out2(op, !destptr)
    RETURN
}

AND destlabel(op) = VALOF SWITCHON op INTO
{ // Choose the label for a (possibly conditional) jump
  // for one of the following commands:
  // BREAK, LOOP, ENDCASE, NEXT or EXIT
  // Return zero if not one of these commands.
  DEFAULT:
    RESULTIS 0

  CASE s_loop:
    IF looplab<0 DO trnerr("Illegal use of LOOP")
    IF looplab=0 DO looplab := genlab()
    RESULTIS looplab

  CASE s_next:
    IF nextlab=-2 DO trnerr("Illegal use of NEXT")
    IF nextlab= 0 DO nextlab := genlab()
    RESULTIS nextlab

  CASE s_exit:
    IF exitlab=-2 DO trnerr("Illegal use of EXIT")
    IF exitlab= 0 DO exitlab := genlab()
    RESULTIS exitlab

  CASE s_break:
    IF breaklab=-2 DO trnerr("Illegal use of BREAK")
    IF breaklab= 0 DO breaklab := genlab()
    RESULTIS breaklab

  CASE s_endcase:
    IF endcaselab=-2 DO trnerr("Illegal use of ENDCASE")
    UNLESS endcaselab DO endcaselab := genlab()
    RESULTIS endcaselab
}

LET declnames(x) BE UNLESS x=0 SWITCHON h1!x INTO
  // x is the definition(s) following LET, so the leading operator is
  // one of s_vecdef, s_valdef, s_fndef, s_rtdef, s_patfndef, s_patrtdef
  // or s_and. This function adds names to the declaration vector.
{ DEFAULT:
    trnerr("Compiler error in Declnames, op=%s", opname(h1!x))
    RETURN
 
  CASE s_vecdef:
    context, comline := x, h4!x
    IF h1!(h2!x)=s_flt DO
    { trnerr("Vector name must not have the FLT tag")
      h2!x := h2!(h2!x) // Remove the FLT tag
    }
    decldyn(h2!x)
    RETURN
 
  CASE s_valdef:
    context, comline := x, h4!x
    decldyn(h2!x)
                 RETURN
 
  CASE s_rtdef:  // x -> [ rtdef, name, namelist, C, entrylab, ln ]
  CASE s_fndef:  //   |  [ fndef, name, namelist, E, entrylab, ln ]
    context, comline := x, h6!x
    // 28feb2023  Functions declarations my now use the FLT prefix
    // to indicate the result of a call has the FLT tag.
    //IF h1!(h2!x)=s_flt DO
    //{ trnerr("Function name must not have the FLT tag")
    //  h2!x := h2!(h2!x) // Remove the FLT tag
    //}
    h5!x := genlab()
    declstat(h2!x, h5!x) // Declare the fn or rt name
    RETURN
 
  CASE s_patrtdef: // x -> [ patrtdef, name, matchlist, entrylab, ln ]
  CASE s_patfndef: //   |  [ patfndef, name, matchlist, entrylab, ln ]
    context, comline := x, h5!x
    // 28feb2023  Functions declarations my now use the FLT prefix
    // to indicate the result of a call has the FLT tag.
    //IF h1!(h2!x)=s_flt DO
    //{ trnerr("Function name must not have the FLT tag")
    //  h2!x := h2!(h2!x) // Remove the FLT tag
    //}
    h4!x := genlab()
    declstat(h2!x, h4!x) // Declare the patfn or patrt name
    RETURN
 
  CASE s_and:
    declnames(h2!x)
    declnames(h3!x)
}
 
AND decldyn(x) BE UNLESS x=0 DO 
{ // x is a list of names with possible FLT tags.
  // They are added to the declaration vector with kind
  // s_local or s_flocal.
  LET k = s_local

  IF h1!x=s_flt DO
  { k := s_flocal     // x -> [s_flt, [s_name.chain,<caracters>]
    x := h2!x
  }

  IF h1!x=s_name DO
  { addname(x, k, ssp, 0)
    IF xrefing DO
      xref(x,
           (k=s_local -> "P:", "FP:"),
           ssp, s_local)
      ssp := ssp + 1
      RETURN
    }
 
  IF h1!x=s_comma DO
  { decldyn(h2!x)
    decldyn(h3!x)
    RETURN
  }
 
  trnerr("Compiler error in Decldyn")
}
 
AND declstat(x, lab) BE
{ // x is the name of a function, routine or a
  // pattern function or routine, or a label.
  LET c = cellwithname(x)
  LET fk = h2!c
  LET k = fk & s_fltmask 
  TEST k=s_global
  THEN { // x is being declared in the scope of a global
         // declaration of the same name. So the global
	 // must be initialised with the entry point.
	 // If either x or the global declaration has
	 // the FLT prefix they must both have this
	 // prefix.
	 LET gn = h3!c
         gdeflist := newblk(gdeflist, gn, lab)
         gdefcount := gdefcount + 1
         //addname(x, s_global, gn, 0)
         addname(x, fk, gn, 0)     // Modified 28feb2023
         IF xrefing DO
           xref(x,
                (fk=s_fglobal -> "FG:", "G:"),
                gn, h1!context)
         IF gdefsing DO
         { writef("G%i3 = %s*n", gn, @h3!x)
           //abort(1000)
         }
      }
 ELSE { addname(x, s_label, lab, 0)
        IF xrefing DO
          xref(x,
               (fk=s_flocal -> "FF:", "F:"),
               lab, h1!context)
      }
}
 
AND decllabels(x) BE
{ // Declare the labels in the body globale, static, manifest,
  // let bolcks and the body of rtdef, patrtdef and valof blocks,
  // and for commands.
  LET e = dvece
  scanlabels(x)
  checkdistinct(e)
}
 
AND checkdistinct(p) BE
{ LET lim = dvece - 4
  FOR q = p TO lim-4 BY 4 DO
  { LET n = h1!q
    FOR c = q+4 TO lim BY 4 DO
        IF h1!c=n DO trnerr("Name %s defined twice", @h3!n)
  }
}
 
AND addname(name, k, a, path) BE
{ LET p = dvece + 4
  IF p>dvect DO trnerr("More workspace needed")
  h1!dvece, h2!dvece, h3!dvece, h4!dvece := name, k, a, path
  h2!name := dvece // Remember the declaration
  dvece := p
  IF prtree2 DO
    sawritef("addname: name cell at=%n %s k=%n a=%n path=%x8*n",
            dvece-3, @h3!name, k, a, path)
}
 
AND undeclare(e) BE 
{ FOR t = e TO dvece-4 BY 4 DO
  { LET name = h1!t
    h2!name := 0   // Forget the declaration of this name.
  }
  dvece := e
}

AND cellwithname(n) = VALOF
{ // n is a name node not prefixed by FLT.
  LET t = h2!n
  IF t RESULTIS t  // It has been looked up before
  t := dvece
  t := t - 4 REPEATUNTIL h1!t=n | h1!t=0
  h2!n := t  // Associate the name with declaration item
  RESULTIS t
}
 
AND scanlabels(x) BE UNLESS x=0 SWITCHON h1!x INTO
 
{ CASE s_colon:   context, comline := x, h5!x
                  h4!x := genlab()
                  declstat(h2!x, h4!x)
 
  CASE s_if: CASE s_unless: CASE s_while: CASE s_until:
  CASE s_switchon: CASE s_case:
                  scanlabels(h3!x)
                  RETURN
 
  CASE s_seq:     scanlabels(h3!x)
 
  CASE s_repeat:
  CASE s_repeatwhile:
  CASE s_repeatuntil:
  CASE s_default: scanlabels(h2!x)
                  RETURN
 
  CASE s_test:    scanlabels(h3!x)
                  scanlabels(h4!x)
  DEFAULT:        RETURN
}
 
AND transdef(x) BE
{ LET ctxt, ln = context, comline
  transdyndefs(x)
  context, comline := ctxt, ln
  IF statdefs(x) DO { LET l, s = genlab(), ssp
                      out2(s_jump, l)
                      transstatdefs(x)
                      ssp := s
                      out2(s_stack, ssp)
                      out2(s_lab, l)
                    }
  context, comline := ctxt, ln
}
 
 
AND transdyndefs(x) BE SWITCHON h1!x INTO
{ CASE s_and:    transdyndefs(h2!x)
                 transdyndefs(h3!x)
                 RETURN
 
  CASE s_vecdef: context, comline := x, h4!x
                 out2(s_llp, vecssp)
                 ssp := ssp + 1
                 vecssp := vecssp + 1 + evalconst(h3!x, FALSE)
                 RETURN
 
  CASE s_valdef: // Compile initialisation code for declaration
                 // N = E  or  FLT N = E
                 context, comline := h3!x, h4!x
                 load(h3!x, h1!(h2!x)=s_flt -> TRUE, FALSE)
  DEFAULT:       RETURN
}
 
AND transstatdefs(x) BE SWITCHON h1!x INTO
{ CASE s_and:  transstatdefs(h2!x)
               transstatdefs(h3!x)
               RETURN
 
  CASE s_rtdef:  // x -> [ rtdef, name, namelist, C, entrylab, ln ]
  CASE s_fndef:  //   |  [ fndef, name, namelist, E, entrylab, ln ]
  { LET e, p = dvece, dvecp
    AND oldpn = procname
    AND xl = exitlab
    AND nl = nextlab
    AND bl, ll = breaklab,  looplab
    AND rl, el = resultlab, endcaselab
    AND cl, cc = caselist,  casecount
    AND argpos = savespacesize
    AND name = h2!x // The FLT tag if any will have been 
                    // removed by declstat.
    AND body = h4!x

    nextlab := -2
    exitlab := -2
    breaklab,  looplab    := -2, -2 // BREAK and LOOP illegal
    resultlab, endcaselab := -2, -2 // RESULTIS and ENDCASE illegal
    caselist,  casecount  :=  0, -1 // CASE and DEFAULT illegal
    procname := name
    context, comline := x, h6!x

    out2(s_entry, h5!x)
    outstring(@h3!procname)

    dvecp := dvece
    
    ssp := savespacesize

    decldyn(h3!x)    // Declare the formal parameters
    checkdistinct(e) // Check that they are distinct.

    out2(s_save, ssp)

    context, comline := body, h6!x
    TEST h1!x=s_rtdef
    THEN { LET e1 = dvece
           decllabels(body)
           trans(body, -1)     // Compile body followed by RTRN
           undeclare(e1)       // Undeclare the labels
         }
    ELSE { fnbody(body, FALSE) // Compile body in non FLT mode
         }                     // followed by FNRN
    out1(s_endproc)

    exitlab := xl
    nextlab := nl
    breaklab,  looplab    := bl, ll
    resultlab, endcaselab := rl, el
    caselist,  casecount  := cl, cc
    procname := oldpn
    dvecp := p
    undeclare(e) // Undeclare the formal parameters
    RETURN
  }
 
  CASE s_patrtdef: // x -> [ patrtdef, name, mlist, entrylab, ln ]
  CASE s_patfndef: //   |  [ patfndef, name, mlist, entrylab, ln ]
  { LET prevdvecp     = dvecp
    AND prevprocname  = procname

    AND bl, ll = breaklab,  looplab    // Thes must be set to make
    AND rl, el = resultlab, endcaselab // BREAK, LOOP, RESULTIS,
                                       // and ENDCASE escapes illegal.

    AND name   = h2!x // The FLT tag if any will have been 
                      // removed by declstat.
    AND mlist = h3!x
    AND argpos = savespacesize

    breaklab,  looplab    := -2, -2 // BREAK, LOOP illegal
    resultlab, endcaselab := -2, -2 // RESULTIS, RETURN illegal

    procname := name
    context, comline := x, h5!x

    out2(s_entry, h4!x)
    outstring(@h3!procname)

    // The arguments are laid out in the stack starting
    // at argpos.

    // Note that SAVE will be compiled by transmatchlist when it
    // knows how many arguments are inspected by the patterns.

    // Translate all the match items
    transmatchlist(h1!x,   // The context is s_patfndef or s_patrtdef
                   mlist,  // mlist -> [matchiteme, plist, E, link, ln]
                           //       |  [matchitemc, plist, C, link, ln]
                   argpos) // Position of the first match argument
    // This point in the compiled code will not be reached.
    out1(s_endproc)

    breaklab,  looplab    := bl, ll
    resultlab, endcaselab := rl, el

    procname := prevprocname
    dvecp := prevdvecp
  }
 
  DEFAULT:     RETURN
}

AND arglength(mlist) = VALOF
{ // mlist ->  [ matchiteme, plist, E, link, ln ]
  //       |   [ matchitemc, plist, C, link, ln ]
  // link points to the next match item, if any

  // Return the maximum number of arguments discovered in any
  // match item in mlist.
  LET res = 0

  WHILE mlist DO
  { IF h2!mlist DO
    { LET len = patarglength(h2!mlist)
      IF res < len DO res := len
    }
    mlist := h4!mlist
  }

  RESULTIS res
}

AND patarglength(plist) = VALOF
{ // plist => 0                           => zero
  //       |  [ comma. plist, plist ]     => sum of the two plists
  //       |  all othe pat nodes          => length 1
  IF plist=0  RESULTIS 0
  IF h1!plist=s_comma RESULTIS patarglength(h2!plist) + patarglength(h3!plist)
  RESULTIS 1
}

AND transnext() BE
{ IF nextlab =-2 DO trnerr("NEXT out of context")
  IF nextlab=-1 DO { out1(s_rtrn); RETURN }
  IF nextlab=0 DO nextlab := genlab()
  out2(s_jump, nextlab)
}

AND transexit() BE
{ IF exitlab =-2 DO trnerr("EXIT out of context")
  IF exitlab=-1 DO { out1(s_rtrn); RETURN }
  IF exitlab=0 DO exitlab := genlab()
  out2(s_jump, exitlab)
}

AND transmatchlist(mcontext, mlist, argpos) BE
{ // This function is only used to compile a match list in one of the
  // contexts specified by mcontext. It does not call itself.
  
  // mcontext is s_patfndef, s_patrtdef,
  //             s_matche,   s_matchc,
  //             s_everye or s_everyc

  // mlist ->  [ matchiteme, plist, E, link, ln ]
  //       or  [ matchitemc, plist, C, link, ln ]

  //           link points to the next match item node, if any.

  // argpos is the position relative to P of the first argument,
  // if any. These arguments are already laid out on the the top
  // of the stack.

  // dvece, nextlab, exitlab and casecount are saved and restored.
  // If in a pattern matching function or routine, dvecp, breaklab,
  // looplab, resultlab and endcaselab are also saved and restored.
  // Note that caselist is saved and restored by transswitch.

  // If the match context is s_matche or s_everye, patresultpos holds
  // the stack location relative to P for the result. This will be
  // just before the first argument position. In both cases this
  // location is initialised to zero.

  // The global nextlab is used to label the end of the  current
  // match item. It is used by the conditional jumps in the
  // implementation of patterns and in the compilation of NEXT.
  // It is set to -2 when not in a match construct. When greater
  // than 0 it holds the label number but is zero before the
  // label number is allocated. When set to -1 a jump to nextlab
  // is replaced by FNRN or RTRN depending on the context.

  // exitlab is used to label the end of the match sequence.
  // It is set to -2 when not compiling a match construct. If is
  // is set to -1 jumps to exitlab are replaced by FNRN or RTRN.
  // If exitlab is zero, it given a new label number and a jump
  // is compiled.

  // When the match context is patfndef the compilation of match item
  // expressions followed by FNRN causing the function to return.

  // When the match context is patrtdef the compilation of match item
  // commands are followed by RTRN causing the routine to return.

  // When the match context is s_matche or s_everye, a stack location
  // will have already be allocated and its position relative to P
  // will be in the global patresultpos. For matche, the compilation
  // of the match expressions will be followed by: SP patresultpos
  // and a jump to exitlab. For everye, the added code would be
  // LP patresultpos; ADD; SP patresultpos before falling into the
  // code for the next match item. This will cause the successful
  // results to be summed.
  
  // When the match context is matchc, the compilation of match commands
  // are followed by jumps to exitlab
  
  // When the match context is everyc, the compilation of match commands 
  // fall into the start of the next match item, if any.
  
  LET prevmatchcontext = matchcontext
  LET prevpatresultpos = patresultpos
  
  LET prevdvece      = dvece     // Save all the variables that
  LET prevdvecp      = dvecp     // must be preserved
  
  LET prevexitlab    = exitlab    // These quantities must be saved and
  LET prevnextlab    = nextlab    // restored before returning
  LET prevcasecount  = casecount

  LET prevbreaklab   = breaklab   // These are only restored if in
  LET prevlooplab    = looplab    // a pattern matching function or
  LET prevendcaselab = endcaselab // or routine
  LET prevresultlab  = resultlab
  
  LET op    = h1!mlist
  LET plist = h2!mlist
  LET body  = h3!mlist
  
  LET argcount = arglength(mlist) // The number of arguments
                                  // inspected by the match items

  // Note that the number of arguments supplied may be more or
  // less than the number inspected by the match items.

  // The translation of each match item starts with ssp set to
  // argpos+argcount. This ensures the the evaluation of the
  // bodies of match items do not corrupt arguments inspected by
  // later match items. This is necessary for EVERY constructs and
  // when NEXT is executed.

  matchcontext := mcontext
  
  // mcontext is placed in the global matchcontext since it affects
  // the translation of the NEXT and EXIT commands which might be
  // encountered in inner calls of functions such as trans and
  // transpattern. The previous version of matchcontext is restored
  // before returning from transmatchlist.
  
  exitlab := 0        // These are only allocated label if needed
  casecount := -1     // Disallow CASE and DEFAULT labels
  
  ssp := argpos+argcount
  TEST matchcontext=s_patfndef | matchcontext=s_patrtdef
  THEN { // We can now compile the SAVE statement since we know
         // how many arguments are present,
	 out2(s_save, ssp)  // Save the return link and set ssp
         // BREAK, LOOP, ENDCASE, RESULTIS are not allowed to
	 // cause a jump out of the current function or routine.
	 breaklab,     looplab := -2, -2
	 endcaselab, resultlab := -2, -2
       }
  ELSE { out2(s_stack, ssp) // Just set ssp
       }
  
  IF matchcontext=s_matche | matchcontext=s_everye DO
  { // For MATCH and EVERY expressions initialise the result location.
    out2(s_ln, 0)
    ssp := ssp+1
    out2(s_sp, patresultpos)
    ssp := ssp-1
  }
  
  // We are now ready to translate the match items.
  WHILE mlist DO
  { // mlist -> [ matchiteme, plist, E, link, ln ]
    //       |  [ matchitemc, plist, C, link, ln ]
    LET pattern, body = h2!mlist, h3!mlist
    LET prevdvece = dvece

    context, comline := mlist, h5!mlist
    ssp := argpos + argcount
    out2(s_stack, ssp)
    
    nextlab := 0
//    IF h4!mlist=0 & // Is it the last match item of a
//                    // function or routine
//       (matchcontext=s_patfndef | matchcontext=s_patrtdef) DO
//      nextlab := -1
      
    declpatnames(pattern,  // The pattern
                 argpos,   // Position relative to P of the first
                           // match argument
                 0,        // The current square bracket depth
                           // The arguments are at depth zero
                 0)        // Packed set of up to four 8-bit path offsets

    checkdistinct(prevdvece) // The pattern names must be distinct.

    // Translate the pattern causing a jump to next if not satisfied.
    transpattern(pattern, argpos,    // Argument position relative to P
                          0,         // The indirection depth
			  0)         // The indirection path

    // The pattern was matched successfully so translate
    // the body of the match item.

    SWITCHON matchcontext INTO
    { DEFAULT:
        trnerr("SYSTEM ERROR in transmatchlist")
        abort(999)
        ENDCASE

      CASE s_patfndef:
        fnbody(body, FALSE) // Compile body in non FLT mode
                            // followed by FNRN
        ENDCASE
	
      CASE s_patrtdef:
        decllabels(body)
        trans(body, -1)     // Compile body followed by RTRN
        ENDCASE
	
      CASE s_matche:
        // Compile the match item expression followed by
	// SP patresultpos and a jump to exitlab
        load(body, FALSE)
	out2(s_sp, patresultpos)
	ssp := ssp-1
	genjumpoptodest(s_jump, @exitlab)
        //UNLESS exitlab DO exitlab := genlab()
        //out2(s_jump, exitlab)
        ENDCASE
	
      CASE s_matchc:
        // Compile the match item command followed by
	// a jump to exitlab
        UNLESS exitlab DO exitlab := genlab()
        decllabels(body)
        trans(body, exitlab)
        ENDCASE
	
      CASE s_everye:
        load(body, FALSE)
        out2(s_lp, patresultpos)
	ssp := ssp+1
        out1(s_add)
	ssp := ssp-1
        out2(s_sp, patresultpos)
	ssp := ssp-1
        // Fall through to next matchitem
        ENDCASE
	
      CASE s_everyc:
        decllabels(body)
        trans(body, 0)
        // Fall through to next matchitem
        ENDCASE
    }

    // Compile a LAB statement for nextlab if needed
    IF nextlab>0 DO out2(s_lab, nextlab)
      
    undeclare(prevdvece)  // Undeclare the variables declared
                          // in this match item.

    // Translate the next match item, if any.
    mlist := h4!mlist
  }

  // Compile a LAB statement for exitlab is needed
  IF exitlab>0 DO out2(s_lab, exitlab)

  

  ssp := argpos
  out2(s_stack, ssp)   // Reset ssp

  exitlab := prevexitlab
  nextlab := prevnextlab
  casecount := prevcasecount

  IF op=s_patfndef | s_patrtdef DO
  { // Restore dvecp and the labels for
    // BREAK, LOOP, ENDCASE, RESULTIS which were disallowed
    dvecp := prevdvecp
    breaklab,     looplab := prevbreaklab,     prevlooplab
    endcaselab, resultlab := prevendcaselab, prevresultlab
  }

  matchcontext := prevmatchcontext
  patresultpos := prevpatresultpos
}

AND declpatnames(plist, argpos, depth, path) BE IF plist DO
{ LET op = h1!plist
  LET k = s_local

  // Check that depth and path are allowable
  IF depth>4 DO
    trnerr("Pattern depth is not allowed to be greater than 4")
  IF (path & 255) > 254 DO
    trnerr("The pattern list is too long")
    
  SWITCHON op INTO
  { DEFAULT:
      RETURN            // All other pattern operators

    CASE s_flt:
      plist := h2!plist     // x should point to [s_flt, [s_name, ... ]]
      op  := h1!plist
      UNLESS op=s_name DO
         trnerr("SYSTEM ERROR in declpatnames, FLT not followed by a name")
      k := s_flocal
      // Fall through to CASE s_name
      
    CASE s_name:
    { LET cell = cellwithname(plist)
     
      IF (h2!cell & s_fltmask) = s_manifest  RETURN
      // The name is not a manifest so define it as a path variable
      
      IF depth=0 DO { addname(plist, k, argpos, 0)
                      IF xrefing DO
                        xref(plist, (k=s_local -> "P:", "FP:"), argpos, k)
//writef("Declaring pattern variable %s to be %s %n*n",
//        @h3!plist, opname(k), argpos)
                      RETURN
                    }
      TEST k=s_local
      THEN k := s_path1  + depth - 1
      ELSE k := s_fpath1 + depth - 1

      addname(plist, k, argpos, path)
      IF xrefing DO xref(plist, (k=s_local -> "I:", "FI:"), argpos, k, path)
//writef("Declaring pattern variable %s to be %s %n path=%x8*n",
//        @h3!plist, opname(k), argpos, path)
      RETURN
    }
    
    CASE s_comma:
       declpatnames(h2!plist, argpos, depth, path)
       TEST depth=0
       THEN declpatnames(h3!plist, argpos+1, depth, path)
       ELSE declpatnames(h3!plist, argpos,   depth, path+1)
       RETURN

    CASE s_patand:
       declpatnames(h2!plist, argpos, depth, path)
       declpatnames(h3!plist, argpos, depth, path)
       RETURN

    CASE s_patptr:
       declpatnames(h2!plist, argpos, depth+1, path<<8)
       RETURN
  }
}

AND transpattern(x, argpos, depth, path) BE IF x DO
{ LET op = h1!x
  LET ff = FALSE // Assume integer tests
  LET patrelop = 0
  
//writef("transpattern: op=%s rsltpos=%n argpos=%n ssp=%n depth=%n path=%X8*n",
//        opname(op), patresultpos, argpos, ssp, depth, path)
//abort(1123)

  SWITCHON op INTO
  { DEFAULT:
      trnerr("SYSTEMERROR in transpattern, op=%s", opname(op))

    CASE s_break:   genjumpoptodest(s_jump, @breaklab);   RETURN
    CASE s_loop:    genjumpoptodest(s_jump, @looplab);    RETURN
    CASE s_endcase: genjumpoptodest(s_jump, @endcaselab); RETURN
    CASE s_next:    genjumpoptodest(s_jump, @nextlab);    RETURN
    CASE s_exit:    genjumpoptodest(s_jump, @exitlab);    RETURN

    CASE s_query:
      RETURN
  
    CASE s_fnum:
      ff := TRUE
    CASE s_number:
//writef("transpattern: do   number op=%s rsltpos=%n argpos=%n ssp=%n depth=%n path=%X8*n",
//        opname(op), patresultpos, argpos, ssp, depth, path)
      IF nextlab<=0 DO nextlab := genlab()
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      out2(s_ln, h2!x)
      ssp := ssp+1
      out1(ff -> s_feq, s_eq)
      ssp := ssp-1
      out2(s_jf, nextlab)
      ssp := ssp-1
//writef("transpattern: done number op=%s rsltpos=%n argpos=%n ssp=%n depth=%n path=%X8*n",
//        opname(op), patresultpos, argpos, ssp, depth, path)
      RETURN

    CASE s_flt:
      x := h2!x
      UNLESS h1!x=s_name DO trnerr("Operand of FLT should be a name")
      // Fall through to CASE s_name:
    CASE s_name:
    { LET cell = cellwithname(x)
      LET k, n = h2!cell, h3!cell
      IF cell & (k=s_manifest | k=s_fmanifest) DO
      { // Treat a manifest constant as an integer constant
        IF k=s_fmanifest DO ff := TRUE
        IF nextlab<=0 DO nextlab := genlab()
        oppath(s_lp, argpos, depth, path)
	ssp := ssp+1
        out2(s_ln, n)
	ssp := ssp+1
        out1(ff -> s_feq, s_eq)
	ssp := ssp-1
        out2(s_jf, nextlab)
	ssp := ssp-1
        RETURN
      }
      // A non manifest name is always successful.
      RETURN
    }
      
    CASE s_comma:
      transpattern(h2!x, argpos, depth, path)
      TEST depth
      THEN transpattern(h3!x, argpos,   depth, path+1)
      ELSE transpattern(h3!x, argpos+1, depth, path)
      RETURN

    CASE s_patand:
      // Form:  P Q
      // Compile code to jump to nextlab if
      // either P or Q fails to match the current location
      transpattern(h2!x, argpos, depth, path)
      transpattern(h3!x, argpos, depth, path)
      RETURN

    CASE s_frange:
      ff := TRUE
    CASE s_range:
      UNLESS ff IF isflt(h2!x) | isflt(h3!x) DO
      { op := cv2flt(op) // Promote to floating point if needed
        h1!x := op
	ff := TRUE
      }
      // P  .. Q  is equivalent to   >=P  <=Q
      // P #.. Q  is equivalent to  #>=P #<=Q
      // Compile code to jump to nextlab if
      // the current value is not in the range
      IF nextlab<=0 DO nextlab := genlab()
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(h2!x, ff)
      out1(ff -> s_fls, s_ls)
      ssp := ssp-1
      out2(s_jt, nextlab)
      ssp := ssp-1
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(h3!x, ff)
      out1(ff -> s_fgr, s_gr)
      ssp := ssp-1
      out2(s_jt, nextlab)
      ssp := ssp-1
      RETURN
      
    CASE s_pator:
    { // x is a collections of constants or ranges
      LET L = genlab()
      transpator(x, argpos, depth, path, L)
      IF nextlab=0 DO nextlab := genlab()
      out2(s_jump, nextlab) // Jump taken if the match failed
      out2(s_lab, L)        // Point reached if the match was successful
      RETURN
    }
    
    CASE s_patptr:
      transpattern(h2!x, argpos,   depth+1, path<<8)
      RETURN

     CASE s_patfeq:
     CASE s_patfne:
     CASE s_patfls:
     CASE s_patfgr:
     CASE s_patfle:
     CASE s_patfge:
       ff := TRUE
     CASE s_pateq:
     CASE s_patne:
     CASE s_patls:
     CASE s_patgr:
     CASE s_patle:
     CASE s_patge:
       UNLESS ff IF isflt(h2!x) DO
       { op := cv2flt(op) // Promote to floating point if needed
	 h1!x := op
	 ff := TRUE
       }
       oppath(s_lp, argpos, depth, path)
       ssp := ssp+1
       load(h2!x, FALSE)
       IF nextlab<=0 DO nextlab := genlab()
       out1(patrel2rel(op))
       ssp := ssp-1
       out2(s_jf, nextlab)
       ssp := ssp-1
       RETURN
  }
}

AND transpator(x, argpos, depth, path, L) BE
{ // L is the label for successful matches.
  LET op = h1!x
  LET ff = FALSE // Assume integer test unless floating point specified.
//writef("transpator: op=%s  argpos=%n depth=%n path=%X8 L=%n*n",
//        opname(h1!x), argpos, depth, path, L)
//abort(1124)

  SWITCHON op INTO
  { DEFAULT:
      trnerr("Unexpected operator in a pattern OR construct, op=%s",
              opname(op))

    CASE s_pator:
      transpator(h2!x, argpos, depth, path, L)
      transpator(h3!x, argpos, depth, path, L)
      RETURN

    CASE s_fnum:
      ff := TRUE
    CASE s_number:
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(x, ff)
      out1(ff -> s_feq, s_eq)
      ssp := ssp-1
      out2(s_jt, L)
      ssp := ssp-1
      RETURN

    CASE s_name:
    { LET cell = cellwithname(x)
      LET k, n = h2!cell, h3!cell
      UNLESS k=s_manifest | k=s_fmanifest DO
      { trnerr("The name %s should be a manifest constant", @h3!x)
        RETURN    
      }
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      IF k=s_fmanifest DO ff := TRUE
      load(x, ff)
      out1(ff -> s_feq, s_eq)
      ssp := ssp-1
      out2(s_jt, L)
      ssp := ssp-1
      RETURN
    }

    CASE s_frange:
      ff := TRUE
    CASE s_range:
    { LET M = genlab() // Jump to M if current value not in the range
      UNLESS ff IF isflt(h2!x) | isflt(h3!x) DO
      { op := cv2flt(op) // Promote to floating point if needed
        h1!x := op
	ff := TRUE
      }
      // P  .. Q  is equivalent to   >=P  <=Q
      // P #.. Q  is equivalent to  #>=P #<=Q
      // Compile jump to L if the match is successful
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      UNLESS isconst(h2!x) & isconst(h3!x) DO
        trnerr("The operands of a range must be constants")
      load(h2!x, ff)
      out1(ff -> s_fls, s_ls)
      ssp := ssp-1
      out2(s_jt, M) // Jump if current value too small
      ssp := ssp-1
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(h3!x, ff)
      out1(ff -> s_fle, s_le)
      ssp := ssp-1
      out2(s_jt, L)  // Jump to L if the range is satisfied
      ssp := ssp-1
      out2(s_lab, M) // The range test has failed
      RETURN
    }
  }
}

AND oppath(op,        // s_lp, s_llp or s_sp
           pos,
           depth,
	   path) BE
{ // Load or store a value specified by a path onto/from the top of the stack.
  // This function does not change ssp. That is done in the calling code.
//writef("oppath: pos=%n depth=%n path=%X8*n", pos, depth, path)
//abort(5524)
  LET offset = path&255
  
  IF depth<=0 DO
  { out2(op, pos)
    RETURN
  }
  oppath(s_lp, pos, depth-1, path>>8)
  IF offset DO
  { out2(s_ln, offset)
    out1(s_add)
  }
  IF op=s_lp DO
  { out1(s_rv)
    RETURN
  }
  IF op=s_sp DO
  { out1(s_stind)
    RETURN
  }
  IF op=s_llp RETURN
  trnerr("SYSTEM ERROR: in oppath")
}

AND statdefs(x) = h1!x=s_fndef | h1!x=s_rtdef       -> TRUE,
                  h1!x=s_patfndef | h1!x=s_patrtdef -> TRUE,
                  h1!x ~= s_and                     -> FALSE,
                  statdefs(h2!x)                    -> TRUE,
                  statdefs(h3!x)
 
 
LET jumpcond(x, b, L) BE
{ // L is a label number > 0
  LET sw = b

  SWITCHON h1!x INTO
  { CASE s_false:  b := NOT b
    CASE s_true:   IF b DO out2(s_jump, L)
                   RETURN
 
    CASE s_not:    jumpcond(h2!x, NOT b, L)
                   RETURN
 
    CASE s_logand: sw := NOT sw
    CASE s_logor:  TEST sw THEN { jumpcond(h2!x, b, L)
                                  jumpcond(h3!x, b, L)
                                  RETURN
                                }
 
                           ELSE { LET M = genlab()
                                  jumpcond(h2!x, NOT b, M)
                                  jumpcond(h3!x, b, L)
                                  out2(s_lab, M)
                                  RETURN
                                }
 
    DEFAULT:       load(x, FALSE)
                   out2(b -> s_jt, s_jf, L)
                   ssp := ssp - 1
                   RETURN
  }
}
 
AND transswitch(x, next) BE
{ LET cl, cc = caselist, casecount     // These must be saved and restored 
  LET dl, el = defaultlab, endcaselab
  
  LET L, dlab = genlab(), ?
  
  caselist, casecount, defaultlab := 0, 0, 0
  endcaselab := next
  UNLESS endcaselab DO endcaselab := genlab()
  
  context, comline := x, h4!x

  load(h2!x, FALSE)  // Evaluate the switch expression

  out2(s_res, L) // Make a jump to the end of the switch
                 // with the switch expression in<res>
  ssp := ssp-1

  // Compile the switch body collecting the case label data
  trans(h3!x, endcaselab)
 
  context, comline := x, h4!x
  out2(s_lab, L)      // The switch value is on the top of the stack
  out2(s_rstack, ssp) // Load <res> onto the top of the stack
  ssp := ssp+1

  dlab := defaultlab>0 -> defaultlab,
          endcaselab>0 -> endcaselab,
          genlab()

  // The switch expression value is on the top of the stack
  out2(s_switchon, casecount); out1(dlab) 
  WHILE caselist DO { out2(h2!caselist, h3!caselist)
                      caselist := h1!caselist
                    }
  ssp := ssp-1

  IF next=0                DO   out2(s_lab, endcaselab)
  IF next<0 & defaultlab=0 DO { out2(s_lab, dlab)
                                out1(s_rtrn)
                              }

  defaultlab, endcaselab := dl, el
  caselist,   casecount  := cl, cc
}
 
AND transfor(x, next) BE
{ // x -> [s_for, N, initval, lim, step, c, ln]
  LET e, m, blab = dvece, genlab(), 0
  LET bl, ll = breaklab, looplab
  LET cc = casecount
//  LET el = endcaselab
  LET k, n, step = 0, 0, 1
  LET s = ssp
  LET name = h2!x

  casecount := -1  // Disallow CASE and DEFAULT labels.   
  breaklab, looplab := next, 0
   
  context, comline := x, h7!x
 
  IF h1!name=s_flt DO
  { trnerr("FOR loop control variable must not have the FLT tag")
    name := h2!name  // Remove the FLT tag
    h2!x := name
  }

  addname(name, s_local, s, 0)
  load(h3!x, FALSE)       // The initial value

  // Set k, n to be the instruction to load the end limit, if there is one
  // k is zero if no end limit was specified.???
  IF h4!x TEST h1!(h4!x)=s_number
          THEN   k, n := s_ln, h2!(h4!x)
          ELSE { k, n := s_lp, ssp
                 load(h4!x, FALSE) // Place the end limit in the stack
               }
  // k=0 if there is no TO expression
  
  IF h5!x DO step := evalconst(h5!x, FALSE) // Set step if BY given
 
  out1(s_store)  // Ensure the control variable and possible end limit
                 // is stored in memory
   
  TEST k=s_ln & h1!(h3!x)=s_number  // check for constant limit expression
  THEN { // The initial and limit values are both constants 
         LET initval = h2!(h3!x)
         IF step>=0 & initval>n | step<0 & initval<n DO
         { // The body of this FOR loop will not be executed
	   TEST next<0
           THEN out1(s_rtrn)
           ELSE TEST next>0
                THEN out2(s_jump, next)
                ELSE { blab := breaklab>0 -> breaklab, genlab()
                       out2(s_jump, blab)
                     }
         }
       }
  ELSE { IF next<=0 DO blab := genlab()
         // Only perform a conditional jump if the TO expression was given.
	 IF k DO
         { out2(s_lp, s)
           out2(k, n)
           out1(step>=0 -> s_gr, s_ls)
           out2(s_jt, next>0 -> next, blab)
	 }
       }

  IF breaklab=0 & blab>0 DO breaklab := blab
   
  context, comline := x, h7!x
  out2(s_lab, m)
  decllabels(h6!x)
  trans(h6!x, 0)   // Translate the body of the for loop.
  UNLESS looplab=0 DO out2(s_lab, looplab)
  // Compile code to increment the control variable
  out2(s_lp, s); out2(s_ln, step); out1(s_add); out2(s_sp, s)
  TEST k
  THEN { // If the TO expression is given compile the conditional jump.
         out2(s_lp,s); out2(k,n); out1(step>=0 -> s_le, s_ge)
         out2(s_jt, m)
       }
  ELSE { // No TO expression given so compile an unconditional jump
         out2(s_jump, m)
       }
 
  IF next<=0 TEST blab>0 
             THEN                  out2(s_lab, blab)
             ELSE IF breaklab>0 DO out2(s_lab, breaklab)
  trnext(next)
  casecount := cc
  breaklab, looplab, ssp := bl, ll, s
  out2(s_stack, ssp)
  undeclare(e)
}

LET isflt(x) = x=0 -> FALSE, VALOF
{ // Return TRUE if expression x is an fnumber, a name declared
  // with the FLT tag or has a leading operator such as #+ or #-
  // that returns a floating point value. Remember the operators
  // such as + and - are converted to #+ and #- if they have
  // floating point operands.
  // Added 28 Feb 2023: If x is a function call f(args) return
  // TRUE if isflt(f) is TRUE, iereturn isflt(h2!x).
  SWITCHON h1!x INTO
  { DEFAULT:  RESULTIS FALSE

    CASE s_name: { LET c = cellwithname(x)
                   IF (h2!c & s_fltbit)=0 RESULTIS FALSE
                   RESULTIS TRUE
                 }

    CASE s_float: CASE s_fabs:
    CASE s_fpos:  CASE s_fneg:
    CASE s_fadd:  CASE s_fsub:
    CASE s_fmul:  CASE s_fdiv: CASE s_fmod:
    CASE s_fcond:
    CASE s_fnum:  RESULTIS TRUE

    CASE s_neg: CASE s_abs:
    CASE s_fnap:             // Added 28 Feb 2023
      RESULTIS isflt(h2!x)

    CASE s_add: CASE s_sub:
    CASE s_mul: CASE s_div: CASE s_mod:
      IF isflt(h2!x) | isflt(h3!x) RESULTIS TRUE
      RESULTIS FALSE

    CASE s_cond:
      IF isflt(h3!x) | isflt(h4!x) RESULTIS TRUE
      RESULTIS FALSE
  }    
}
 
LET load(x, ff) BE
{ // Translate expression x into Ocode.
  // This will add one to ssp.
  // If ff=TRUE, the expression is in an FLT context and will
  // convert, for example, + and - to #+ and #-.
  LET op = h1!x

  IF isconst(x) DO
  { out2(s_ln, evalconst(x, ff | isflt(x)))
    ssp := ssp + 1
    RETURN
  }
 
  SWITCHON op INTO
  { DEFAULT:
           trnerr("Compiler error in Load, op=%s", opname(op))
           out2(s_ln, 0)
           ssp := ssp + 1
           RETURN

    CASE s_break:   genjumpoptodest(s_jump, @breaklab);   ssp:=ssp+1;RETURN
    CASE s_loop:    genjumpoptodest(s_jump, @looplab);    ssp:=ssp+1;RETURN
    CASE s_endcase: genjumpoptodest(s_jump, @endcaselab); ssp:=ssp+1;RETURN
    CASE s_next:    genjumpoptodest(s_jump, @nextlab);    ssp:=ssp+1;RETURN
    CASE s_exit:    genjumpoptodest(s_jump, @exitlab);    ssp:=ssp+1;RETURN

    CASE s_of:
         { LET slct = evalconst(h2!x, FALSE) // Inserted 11/7/01
           LET len = slct>>24
           LET sh  = slct>>16 & 255
           LET offset = slct & #xFFFF
           load(h3!x, FALSE)
	   
           IF offset DO
           { out2(s_ln, offset)
             out1(s_add)
           }

           // Optimise accessing a complete word.
           IF sh=0 & (len=0 | len=wordbitlen) DO
           { out1(s_rv) // The source field is a complete word
             RETURN
           }

           // Compile (SLCT len:sh:0)(E+offset)
           TEST noselst
           THEN { // Old version not using SELLD
                  out1(s_rv)
                  IF sh DO
                  { out2(s_ln, sh)
                    out1(s_rshift)
                  }
                  IF len>0 & (len+sh~=wordbitlen) DO
                  { // Applying a mask is necessary
                    LET mask = (1<<len)-1
                    out2(s_ln, mask)
                    out1(s_logand)
                  }
                }
           ELSE { // New version using SELLD
                  out3(s_selld, len, sh)
                }
           RETURN
         }

    CASE s_div: CASE s_mod: CASE s_sub:
         // Convert to floating point if in FLT mode or
         // has a floating point operand.
         IF ff | isflt(x) DO
         { // Convert to floating point operators.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(op)
         ssp := ssp - 1
         RETURN
                      
    CASE s_fdiv: CASE s_fmod: CASE s_fsub:
         load(h2!x, TRUE)
         load(h3!x, TRUE)
         out1(op)
         ssp := ssp - 1
         RETURN

    CASE s_ls: CASE s_gr: CASE s_le: CASE s_ge:
         // Only convert to floating point if they have
         // a floating point operand.
         IF isflt(h2!x) | isflt(h3!x) DO
         { // Convert to floating point operators.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(op)
         ssp := ssp - 1
         RETURN

    CASE s_fls: CASE s_fgr: CASE s_fle: CASE s_fge:
         load(h2!x, TRUE)
         load(h3!x, TRUE)
         out1(op)
         ssp := ssp - 1
         RETURN
 
    CASE s_byteap:
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(s_getbyte)    // Compiling: E1%E2
         ssp := ssp - 1
         RETURN

    CASE s_lshift: CASE s_rshift:
         load(h2!x, FALSE)  // Compiling: E1<<E2  or  E1>>E2
         UNLESS iszero(h3!x,FALSE) DO
         { load(h3!x, FALSE)
           out1(op)
           ssp := ssp - 1
         }
         RETURN
 
    CASE s_eq: CASE s_ne:
         // Relational operators are only converted if they
         // have floating point operands.
         IF isflt(h2!x) | isflt(h3!x) DO
         { // Convert to floating point.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         GOTO intsymmetric

    CASE s_mul: CASE s_add:
          // Convert to floating point if in FLT mode or
          // has a floating point operand.
         IF ff | isflt(x) DO
         { h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         // Fall through

    CASE s_vecap:
    CASE s_logand: CASE s_logor: CASE s_eqv: CASE s_xor:
intsymmetric:
       // Symmetric non FLT dyadic operators.
       { LET a, b = h2!x, h3!x
         TEST h1!a=s_name   |
              h1!a=s_number |
              h1!a=s_fnum THEN { load(b, FALSE); load(a, FALSE) }
                          ELSE { load(a, FALSE); load(b, FALSE) }
         TEST op=s_vecap THEN out2(s_add, s_rv)
                         ELSE out1(op)
         ssp := ssp - 1
         RETURN
       }
 
    CASE s_fmul: CASE s_fadd: CASE s_feq: CASE s_fne:
       { LET a, b = h2!x, h3!x
         TEST h1!a=s_name   |
              h1!a=s_number |
              h1!a=s_fnum THEN { load(b, TRUE); load(a, TRUE) }
                          ELSE { load(a, TRUE); load(b, TRUE) }
         out1(op)
         ssp := ssp - 1
         RETURN
       }
 
    CASE s_neg: CASE s_abs:
       IF ff | isflt(x) DO
       { h1!x := cv2flt(op)
         load(x, TRUE)
         RETURN
      }
      load(h2!x, FALSE)
      out1(op)
      RETURN

    CASE s_fneg: CASE s_fabs:CASE s_fix:
      load(h2!x, TRUE)
      out1(op)
      RETURN
 
    CASE s_float: CASE s_not: CASE s_rv:
      load(h2!x, FALSE)
      out1(op)
      RETURN
 
    CASE s_true: CASE s_false: CASE s_query:
       out1(op)
       ssp := ssp + 1
       RETURN
 
    CASE s_lv:
       loadlv(h2!x)
       RETURN
 
    CASE s_number:
//sawritef("number %n  ff=%n*n", h2!x, ff)
       IF ff DO
       { // Convert the integer constant to floating point
         h1!x := s_fnum
         h2!x := sys(Sys_flt, fl_mk, h2!x, 0)
//sawritef("number converted to fnumber %13e*n", h2!x)
       }
       // Fall through
    CASE s_fnum:
       out2(s_ln, h2!x)
       ssp := ssp + 1
       RETURN
 
    CASE s_string:
       out1(s_lstr)
       outstring(@ h2!x)
       ssp := ssp + 1
       RETURN
 
    CASE s_name:
       transname(x, s_lp, s_lg, s_ll, s_lf, s_ln)
       ssp := ssp + 1
       RETURN
 
    CASE s_valof:
     { LET e, rl, el, cc = dvece, resultlab, endcaselab, casecount
       casecount := -2 // Disallow CASE & DEFAULT labels
       resultlab := genlab()
       decllabels(h2!x)
       trans(h2!x, 0)
       out2(s_lab, resultlab)
       out2(s_rstack, ssp)
       ssp := ssp + 1
       resultlab, endcaselab, casecount := rl, el, cc
       undeclare(e)
       RETURN
     }
 
    CASE s_matche:
    CASE s_everye:
    { // This is an expression but has much in common with a SWITCHON
      // command in that is can select one of many alternative and can
      // use various simple commands to escape from a match item
      // or the entire matchlist. These commands are EXIT, NEXT,
      // BREAK, LOOP, ENDCASE and RESULTIS, provided the jump does
      // not leave the current function or routine.
      
      LET argpos = ssp+1 // Position relative to P of first argument
                         // leaving space for the result location.
//writef("transmatchlist: matche/everye ssp=%n*n", ssp)

      // Allocate a stack location for the result
      patresultpos := ssp
      ssp := argpos
      out2(s_stack, ssp)
      out1(s_store)
      
      context, comline := x, h5!x

      // Load the MATCHe or EVERYe arguments
      loadlist(h2!x)
      out1(s_store) // Ensure that the arguments are in memory

      // Translate the match items
      transmatchlist(op,     // Context is s_matche or s_everye
                     h3!x,   // mlist -> [matchiteme, plist, E, link, ln]
                     argpos) // Position of the first match arg
      RETURN
    }
 
    
    CASE s_fnap:
     { LET s = ssp
       ssp := ssp + savespacesize
       out2(s_stack, ssp)
       loadlist(h3!x) // Load arguments in non FLT mode
       load(h2!x, FALSE)
       out2(s_fnap, s)
       ssp := s + 1
       RETURN
     }

    CASE s_fcond:
       ff := TRUE
       GOTO cond

    CASE s_cond:
       IF ff | isflt(x) DO
       { h1!x := s_fcond
         load(x, TRUE)
         RETURN
       }
cond:
     { LET l, m = genlab(), genlab()
       LET s = ssp
       jumpcond(h2!x, FALSE, m)
       load(h3!x, ff)
       out2(s_res,l)
       ssp := s; out2(s_stack, ssp)
       out2(s_lab, m)
       load(h4!x, ff)
       out2(s_res,l)
       out2(s_lab, l)
       out2(s_rstack,s)
       RETURN
     }
 
    CASE s_table:
     { LET m = genlab()
       out2(s_datalab, m)
       x := h2!x
       WHILE h1!x=s_comma DO
       { out2(s_itemn, evalconst(h2!x, FALSE))
         x := h3!x
       }
       out2(s_itemn, evalconst(x, FALSE))
       out2(s_lll, m)
       ssp := ssp + 1
       RETURN
     }
  }
}

AND fnbody(x, ff) BE SWITCHON h1!x INTO
{ // If ff is TRUE  compile x in FLT mode
  // If ff is FALSE compile x in non FLT mode
  // followed by FNRN
  DEFAULT:         load(x, ff)
                   out1(s_fnrn)
                   ssp := ssp - 1
                   RETURN
                   
  CASE s_valof: { LET e, rl, cc = dvece, resultlab, casecount
                  casecount := -1 // Disallow CASE & DEFAULT labels
                  resultlab := -1 // RES replaced by FNRN
                  decllabels(h2!x)
                  trans(h2!x, -1)
                  resultlab, casecount := rl, cc
                  undeclare(e)
                  RETURN
                }

  CASE s_fcond: { LET l = genlab()
                  jumpcond(h2!x, FALSE, l)
                  fnbody(h3!x, TRUE)
                  out2(s_lab, l)
                  fnbody(h4!x, TRUE)
                  RETURN
                }

  CASE s_cond:  { LET l = genlab()
                  IF ff | isflt(x) DO
                  { h1!x := s_fcond    // Replace -> by #->
                    fnbody(x, TRUE)
                    RETURN
                  }
                  jumpcond(h2!x, FALSE, l)
                  fnbody(h3!x, ff)
                  out2(s_lab, l)
                  fnbody(h4!x, ff)
                }
}
 
 
AND loadlv(x) BE
{ UNLESS x=0 SWITCHON h1!x INTO
  { DEFAULT:         ENDCASE
 
    CASE s_name:     transname(x, s_llp, s_llg, s_lll, 0, 0)
                     ssp := ssp + 1
                     RETURN
 
    CASE s_rv:       load(h2!x, FALSE)
                     RETURN
 
    CASE s_vecap: { LET a, b = h2!x, h3!x
                    TEST h1!a=s_name   |
                         h1!a=s_number |
                         h1!a=s_fnum THEN { load(b, FALSE); load(a, FALSE) }
                                     ELSE { load(a, FALSE); load(b, FALSE) }
                    out1(s_add)
                    ssp := ssp - 1
                    RETURN
                  }
  }

  trnerr("Ltype expression needed")
  out2(s_ln, 0)
  ssp := ssp + 1
}
 
AND loadlist(x) BE
{ // Load function, routine or MATCH arguments
  UNLESS x=0 TEST h1!x=s_comma
             THEN { loadlist(h2!x); loadlist(h3!x) }
             ELSE load(x, FALSE)
}

// The conversion function are:
//    op2sfop      convert an expression op to a selst sfop
//    assop2op     convert op:= to op
//    cv2flt       convert an integer op or assignment op
//                 to the floating point version.
//    patrel2rel convert a pattern relation to an ordinary relation

AND op2sfop(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("Syserr in op2sfop op=%s not in switch*n",
                          opname(op))
                 RESULTIS op

  CASE s_none:   RESULTIS sf_none

  CASE s_vecap:  RESULTIS sf_vecap

  CASE s_mul:    RESULTIS sf_mul
  CASE s_div:    RESULTIS sf_div
  CASE s_mod:    RESULTIS sf_mod
  CASE s_add:    RESULTIS sf_add
  CASE s_sub:    RESULTIS sf_sub

  CASE s_fmul:   RESULTIS sf_fmul
  CASE s_fdiv:   RESULTIS sf_fdiv
  CASE s_fmod:   RESULTIS sf_fmod
  CASE s_fadd:   RESULTIS sf_fadd
  CASE s_fsub:   RESULTIS sf_fsub

  CASE s_lshift: RESULTIS sf_lshift
  CASE s_rshift: RESULTIS sf_rshift
  CASE s_logand: RESULTIS sf_logand
  CASE s_logor:  RESULTIS sf_logor
  CASE s_eqv:    RESULTIS sf_eqv
  CASE s_xor:    RESULTIS sf_xor

}

AND assop2op(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("Syserr in assop2op unknown op=%s*n",
                          opname(op))
                 RESULTIS op

//  CASE  0:       RESULTIS 0

  CASE s_assfmul:   RESULTIS s_fmul
  CASE s_assfdiv:   RESULTIS s_fdiv
  CASE s_assfmod:   RESULTIS s_fmod
  CASE s_assfadd:   RESULTIS s_fadd
  CASE s_assfsub:   RESULTIS s_fsub

  CASE s_assmul:    RESULTIS s_mul
  CASE s_assdiv:    RESULTIS s_div
  CASE s_assmod:    RESULTIS s_mod
  CASE s_assadd:    RESULTIS s_add
  CASE s_asssub:    RESULTIS s_sub

  CASE s_assvecap:  RESULTIS s_vecap
  CASE s_asslshift: RESULTIS s_lshift
  CASE s_assrshift: RESULTIS s_rshift
  CASE s_asslogand: RESULTIS s_logand
  CASE s_asslogor:  RESULTIS s_logor
  CASE s_asseqv:    RESULTIS s_eqv
  CASE s_assxor:    RESULTIS s_xor

  CASE s_fass:
  CASE s_ass:       RESULTIS s_none
}

AND rel2patrel(op) = VALOF SWITCHON op INTO
{ DEFAULT:    writef("SYSTEM ERROR: in rel2patrel op=%s*n",
                     opname(op))
	      abort(999)
	      RESULTIS s_pateq
	      
  CASE s_eq:  RESULTIS s_pateq
  CASE s_feq: RESULTIS s_patfeq
  CASE s_ne:  RESULTIS s_patne
  CASE s_fne: RESULTIS s_patfne
  CASE s_le:  RESULTIS s_patle
  CASE s_fle: RESULTIS s_patfle
  CASE s_ge:  RESULTIS s_patge
  CASE s_fge: RESULTIS s_patfge
  CASE s_ls:  RESULTIS s_patls
  CASE s_fls: RESULTIS s_patfls
  CASE s_gr:  RESULTIS s_patgr
  CASE s_fgr: RESULTIS s_patfgr
}

AND patrel2rel(op) = VALOF SWITCHON op INTO
{ DEFAULT:    writef("SYSTEM ERROR: in patrel2rel op=%s*n",
                     opname(op))
	      abort(999)
	      RESULTIS s_eq
	      
  CASE s_pateq:  RESULTIS s_eq
  CASE s_patfeq: RESULTIS s_feq
  CASE s_patne:  RESULTIS s_ne
  CASE s_patfne: RESULTIS s_fne
  CASE s_patle:  RESULTIS s_le
  CASE s_patfle: RESULTIS s_fle
  CASE s_patge:  RESULTIS s_ge
  CASE s_patfge: RESULTIS s_fge
  CASE s_patls:  RESULTIS s_ls
  CASE s_patfls: RESULTIS s_fls
  CASE s_patgr:  RESULTIS s_gr
  CASE s_patfgr: RESULTIS s_fgr
}

AND cv2flt(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("Syserr in cv2flt op=%s not in switch*n",
                          opname(op))
                 RESULTIS op

  // Expression operators
  CASE s_neg:    RESULTIS s_fneg
  CASE s_abs:    RESULTIS s_fabs
  CASE s_number: RESULTIS s_fnum
  CASE s_mul:    RESULTIS s_fmul
  CASE s_div:    RESULTIS s_fdiv
  CASE s_mod:    RESULTIS s_fmod
  CASE s_add:    RESULTIS s_fadd
  CASE s_sub:    RESULTIS s_fsub
  CASE s_eq:     RESULTIS s_feq
  CASE s_ne:     RESULTIS s_fne
  CASE s_ls:     RESULTIS s_fls
  CASE s_gr:     RESULTIS s_fgr
  CASE s_le:     RESULTIS s_fle
  CASE s_ge:     RESULTIS s_fge
  CASE s_cond:   RESULTIS s_fcond

  // Pattern operators
  CASE s_range:  RESULTIS s_frange
  CASE s_pateq:  RESULTIS s_patfeq
  CASE s_patne:  RESULTIS s_patfne
  CASE s_patls:  RESULTIS s_patfls
  CASE s_patgr:  RESULTIS s_patfgr
  CASE s_patle:  RESULTIS s_patfle
  CASE s_patge:  RESULTIS s_patfge
  
  // Assignment operators
  CASE s_assmul: RESULTIS s_assfmul
  CASE s_assdiv: RESULTIS s_assfdiv
  CASE s_assmod: RESULTIS s_assfmod
  CASE s_assadd: RESULTIS s_assfadd
  CASE s_asssub: RESULTIS s_assfsub
  CASE s_ass:    RESULTIS s_fass
}

LET isconst(x) = VALOF
{ // Return TRUE if the expression x has a value that can
  // be determined at compile time. These are manifest names,
  // integer or floating point constants, the SLCT construct,
  // TRUE or FALSE, and any expression whose operands are constants
  // other than rv, vecap or byteap expressions.

  IF x=0 RESULTIS FALSE
 
  SWITCHON h1!x INTO
  { CASE s_name:
        { LET c = cellwithname(x)
          LET k = h2!c & s_fltmask
          RESULTIS k=s_manifest -> TRUE, FALSE
        }

    CASE s_fnum:
    CASE s_number:
    CASE s_slct:
    CASE s_true:
    CASE s_false:  RESULTIS TRUE
 
    CASE s_fneg:
    CASE s_fabs:
    CASE s_float:
    CASE s_fix:
    CASE s_neg:
    CASE s_abs:
    CASE s_not:    RESULTIS isconst(h2!x)
       
    CASE s_fmul:
    CASE s_fdiv:
    CASE s_fmod:
    CASE s_fadd:
    CASE s_fsub:
    CASE s_feq:
    CASE s_fne:
    CASE s_fls:
    CASE s_fgr:
    CASE s_fle:
    CASE s_fge:

    CASE s_mul:
    CASE s_div:
    CASE s_mod:
    CASE s_add:
    CASE s_sub:
    CASE s_lshift:
    CASE s_rshift:
    CASE s_logor:
    CASE s_logand:
    CASE s_eqv:
    CASE s_xor:
    CASE s_eq:
    CASE s_ne:
    CASE s_ls:
    CASE s_gr:
    CASE s_le:
    CASE s_ge:
                   UNLESS isconst(h2!x) RESULTIS FALSE
                   RESULTIS isconst(h3!x)

    CASE s_fcond:
    CASE s_cond:   UNLESS isconst(h2!x) RESULTIS FALSE
                   UNLESS isconst(h3!x) RESULTIS FALSE
                   RESULTIS isconst(h4!x)

    DEFAULT:       RESULTIS FALSE

  }
}

LET iszero(x, ff) = isconst(x) & evalconst(x, ff)=0 -> TRUE, FALSE

LET evalconst(x, ff) = VALOF
{ // If ff=TRUE the expression x is to be evaluated in
  // an FLT context, causing integer expression operators
  // to be automatically converted to their floating
  // point versions. Integer constants are also converted
  // to floating point.

  LET op, a, b = 0, 0, 0

  IF x=0 DO { trnerr("Compiler error in Evalconst")
              RESULTIS 0
            }

  IF isflt(x) DO ff := TRUE
 
  op := h1!x
//sawritef("evalconst: op=%s ff=%n*n", opname(op), ff)

  SWITCHON op INTO
  { CASE s_name: { LET c = cellwithname(x)
                   LET k = h2!c
                   LET a = h3!c
                   IF (k & s_fltmask)=s_manifest DO
                   { IF xrefing DO
                       xref(x,
                            (k=s_manifest -> "M:", "FM:"),
                            a, s_const)
                     RESULTIS a
                   }
                   TEST k
                   THEN trnerr("%s must be a MANIFEST constant", @h3!x)
                   ELSE trnerr("Name '%s' is not declared", @h3!x)
                   RESULTIS 0
                 }
 
    CASE s_number: UNLESS ff RESULTIS h2!x
                   // Convert from integer to floating point.
                   h1!x := s_fnum
                   h2!x := sys(Sys_flt, fl_mk, h2!x, 0)

    CASE s_fnum:   RESULTIS h2!x

    CASE s_true:   RESULTIS TRUE
    CASE s_false:  RESULTIS FALSE
    CASE s_query:  RESULTIS 0
 
    CASE s_slct: { LET len, sh, offset = 0, 0, 0     // Inserted 11/7/01
                   IF h2!x DO len    := evalconst(h2!x, FALSE)
                   IF h3!x DO sh     := evalconst(h3!x, FALSE)
                   IF h4!x DO offset := evalconst(h4!x, FALSE)
                   UNLESS 0<=len<=255 & 0<=sh<=255 & 0<=offset<=#xFFFF DO
                       trnerr("A field too large in a SLCT expression")
                   RESULTIS len<<24 | sh<<16 | offset
                 }

    CASE s_fneg:
    CASE s_fabs:
    CASE s_fix:    floatingchk()
                   a := evalconst(h2!x, TRUE)
                   ENDCASE

    CASE s_neg:
    CASE s_abs:    IF ff | isflt(x) DO 
                   { h1!x := cv2flt(op)
                     RESULTIS evalconst(x, TRUE)
                   }
                   a := evalconst(h2!x, FALSE)
                   ENDCASE

    CASE s_not:       
    CASE s_float:  a := evalconst(h2!x, FALSE)
                   ENDCASE

    CASE s_fmul:
    CASE s_fdiv:
    CASE s_fmod:
    CASE s_fadd:
    CASE s_fsub:
    CASE s_feq:
    CASE s_fne:
    CASE s_fls:
    CASE s_fgr:
    CASE s_fle:
    CASE s_fge:  floatingchk()
                 a, b := evalconst(h2!x, TRUE), evalconst(h3!x, TRUE)
                 ENDCASE

    CASE s_mul:
    CASE s_div:
    CASE s_mod:
    CASE s_add:
    CASE s_sub:  IF ff | isflt(x) DO
                 { // Convert to floating point.
                   h1!x := cv2flt(op)
                   RESULTIS evalconst(x, TRUE)
                 }
                 a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                 ENDCASE

    CASE s_eq:
    CASE s_ne:
    CASE s_ls:
    CASE s_gr:
    CASE s_le:
    CASE s_ge:   // Only convert to floating point if there is
                 // a floating point operand.
                 IF isflt(h2!x) | isflt(h3!x) DO
                 { // Convert to floating point.
                   h1!x := cv2flt(op)
                   RESULTIS evalconst(x, TRUE)
                 }
                 a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                 ENDCASE


    CASE s_lshift:
    CASE s_rshift:
    CASE s_logor:
    CASE s_logand:
    CASE s_eqv:
    CASE s_xor:    a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                   ENDCASE

    CASE s_fcond:  a, b := evalconst(h2!x, TRUE), evalconst(h3!x, TRUE)
                   ENDCASE

    CASE s_cond:   IF ff | isflt(x) DO
                   { // Convert to floating point.
                     h1!x := s_fcond
                     RESULTIS evalconst(x, TRUE)
                   }
                   a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                   ENDCASE

    DEFAULT:
  }
    
  SWITCHON h1!x INTO
  { CASE s_neg:    RESULTIS  -  a
    CASE s_abs:    RESULTIS ABS a
    CASE s_not:    RESULTIS NOT a
       
    CASE s_fneg:   RESULTIS sys(Sys_flt, fl_neg,   a)
    CASE s_fabs:   RESULTIS sys(Sys_flt, fl_abs,   a)
    CASE s_fix:    RESULTIS sys(Sys_flt, fl_fix,   a)
    CASE s_float:  RESULTIS sys(Sys_flt, fl_float, a)
       
    CASE s_fmul:   RESULTIS sys(Sys_flt, fl_mul, a,  b)
    CASE s_fdiv:   RESULTIS sys(Sys_flt, fl_div, a,  b)
    CASE s_fmod:   RESULTIS sys(Sys_flt, fl_mod, a,  b)
    CASE s_fadd:   RESULTIS sys(Sys_flt, fl_add, a,  b)
    CASE s_fsub:   RESULTIS sys(Sys_flt, fl_sub, a,  b)

    CASE s_feq:    RESULTIS sys(Sys_flt, fl_eq, a,  b)
    CASE s_fne:    RESULTIS sys(Sys_flt, fl_ne, a,  b)
    CASE s_fls:    RESULTIS sys(Sys_flt, fl_ls, a,  b)
    CASE s_fgr:    RESULTIS sys(Sys_flt, fl_gr, a,  b)
    CASE s_fle:    RESULTIS sys(Sys_flt, fl_le, a,  b)
    CASE s_fge:    RESULTIS sys(Sys_flt, fl_ge, a,  b)

    CASE s_mul:    RESULTIS a   *   b
    CASE s_add:    RESULTIS a   +   b
    CASE s_sub:    RESULTIS a   -   b
    CASE s_lshift: RESULTIS a   <<  b
    CASE s_rshift: RESULTIS a   >>  b
    CASE s_logor:  RESULTIS a   |   b
    CASE s_logand: RESULTIS a   &   b
    CASE s_eqv:    RESULTIS a  EQV  b
    CASE s_xor:    RESULTIS a  XOR  b
    CASE s_div:    RESULTIS b=0 -> 0, a  /  b
    CASE s_mod:    RESULTIS b=0 -> 0, a MOD b
    CASE s_eq:     RESULTIS a =  b
    CASE s_ne:     RESULTIS a ~= b
    CASE s_ls:     RESULTIS a <  b
    CASE s_gr:     RESULTIS a >  b
    CASE s_le:     RESULTIS a <= b
    CASE s_ge:     RESULTIS a >= b

    CASE s_cond:   RESULTIS a -> b, evalconst(h4!x, FALSE)
    CASE s_fcond:  RESULTIS a -> b, evalconst(h4!x, TRUE)
   
    DEFAULT:       ENDCASE
  }

  trnerr("Error in manifest expression, op = %s", opname(h1!x))
  RESULTIS 0
}

AND assign(lhs, rhs, ff, op) BE
// Compile a simple assignment: lhs := rhs, lhs #:= rhs or lhs op := rhs.
// Note that for simultaneous assignments have already been replaced by
// sequences of simple assignments by cvassign.

// If op=s_none the assigment is either lhs := rhs or lhs #:= rhs,
// otherwise it is of the form: lhs op:= rhs where op is one of
// the dyadic expression operators allowed in assignments, namely:
//    s_vecap,
//    s_mul,   s_div,  s_mod,  s_add,  s_sub,
//    s_fmul,  s_fdiv, s_fmod, s_fadd, s_fsub,
//    s_lshift, s_rshift, s_logand, s_logor, s_eqv or s_xor.

// ff=TRUE if the rhs is to be evaluated in FLT mode.

{ LET sfop = op2sfop(op)
  // sfop is either sf_none or
  // an operator used in SELST Ocode instructions, namely:
  //   sf_vecap,
  //   sf_mul,  sf_div,  sf_mod,  sf_add,    sf_sub,
  //   sf_fmul, sf_fdiv, sf_fmod, sf_fadd or sf_fsub.

  SWITCHON h1!lhs INTO
  { CASE s_name:        // name op:= E
      TEST op=s_none
      THEN { // Compile: name := E
             load(rhs, ff)
             transname(lhs, s_sp, s_sg, s_sl, 0, 0)
             ssp := ssp - 1
           }
      ELSE { // Compile: name sfop:= E
             TEST noselst
             THEN { // Load: lhs op rhs
                    // op is a dyadic operator
                    LET operator, a, b = op, lhs, rhs
                    load(@operator, ff)
                    transname(lhs, s_sp, s_sg, s_sl, 0, 0)
                    ssp := ssp - 1
                  }
             ELSE { load(rhs, ff)
                    loadlv(lhs)
                    out4(s_selst, sfop, 0, 0)
                    ssp := ssp - 2
                  }
           }
      RETURN
 
    CASE s_rv:
    CASE s_vecap:  IF op=s_none DO
                   { load(rhs, ff)
                     loadlv(lhs)
                     out1(s_stind)
                     ssp := ssp - 2
                     RETURN
                   }

                   // op is a dyadic expression operator allowed
                   // in assignments.
                   IF noselst DO
                   { // Load: lhs op rhs
                     // op is a dyadic operator
                     LET operator, a, b = op, lhs, rhs
                     // Load the expression whose tree node is [op,lhs,rhs]
                     load(@operator, ff)
                     loadlv(lhs)
                     out1(s_stind)
                     ssp := ssp - 2
                     RETURN
                   }

                   // Compile using SELST
                   load(rhs, ff)
                   loadlv(lhs)
                   out4(s_selst, sfop, 0, 0) 
                   ssp := ssp - 2
                   RETURN

    CASE s_of:   { LET slct = evalconst(h2!lhs, FALSE) // Inserted 11/7/01
                   LET len = slct>>24
                   LET sh  = slct>>16 & 255
                   LET offset = slct & #xFFFF
                   LET mask = -1
                   IF len>0 DO mask := (1<<len)-1
                   mask := mask<<sh
                   TEST noselst
                   THEN { TEST op=s_none
                          THEN { load(rhs, ff)
                               }
                          ELSE { // Load: lhs op rhs
                                 // op is a dyadic operator
                                 LET operator, a, b = op, lhs, rhs
                                 load(@operator, ff)
                               }
                          IF sh DO
                          { out2(s_ln, sh)
                            out1(s_lshift)
                          }

                          UNLESS mask=-1 DO
                          { load(h3!lhs, FALSE)
                            IF offset DO
                            { out2(s_ln, offset)
                              out1(s_add)
                            }
                            out1(s_rv)
                            out1(s_xor)
                            ssp := ssp-1
                            out2(s_ln, mask)
                            out1(s_logand) // bits to change in x
                            load(h3!lhs, FALSE)
                            IF offset DO
                            { out2(s_ln, offset)
                              out1(s_add)
                            }
                            out1(s_rv)
                            out1(s_xor)
                            ssp := ssp-1
                          }

                          load(h3!lhs, FALSE)
                          IF offset DO
                          { out2(s_ln, offset)
                            out1(s_add)
                          }
                          out1(s_stind)
                        }
                   ELSE { // Compile using SELST
                          load(rhs, ff)
                          load(h3!lhs, FALSE)
                          IF offset DO
                          { out2(s_ln, offset)
                            out1(s_add)
                          }
                          TEST len=0 & sh=0 & sfop=sf_none
                          THEN out1(s_stind) // Full word field
                                             // and no op.
                          ELSE out4(s_selst, sfop, len, sh) 
                        }
                   ssp := ssp-2
                   RETURN
                 }

    CASE s_byteap:
      TEST op=s_none
      THEN { // Compiling: E1%E2 := E3
             load(rhs, ff)
           }
      ELSE { // Compiling: E1%E2 op:= E3
             // op is a dyadic expression operator allowed
             // in assignments.
             LET operator, a, b = op, lhs, rhs

             // The destination is not a full word field,
             // so some operators are not permitted.
             IF sfop=sf_fmul | sfop=sf_fdiv | sfop=sf_fmod |
                sfop=sf_fadd | sfop=sf_fsub | sfop=sf_vecap DO
               trnerr("Bad op in E%E op:= E")

             // Load an expression whose tree node is [op,lhs,rhs]
             load(@operator, FALSE)
           }
      load(h2!lhs, FALSE)
      load(h3!lhs, FALSE)
      out1(s_putbyte)
      ssp:=ssp-3
      RETURN

    DEFAULT:
      trnerr("Ltype expression needed")
  }
}
 
 
AND transname(x, p, g, l, f, n) BE
{ // x is a name node
  // p is s_lp, s_llp or s_sp   It determines the code for path variables
  LET c = cellwithname(x)
  LET k, a, path = h2!c, h3!c, h4!c
 
  // Must deal with s_fglobal, s_flocal, s_f_static, s_flabel, s_fmanifest
  // as if they were the integer versions. The sole purpose of these
  // is to indicate than a name has been declared with the FLT tag.
  // Note that s_fglobal = s_global  + s_fltbit,
  // and       s_global  = s_fglobal & s_fltmask, etc
  // where s_fltbit is 128 s_fltmask is 127.

  SWITCHON k INTO
  { DEFAULT:        trnerr("Name '%s' not declared", @h3!x)
   
    CASE s_fglobal:
    CASE s_global:  out2(g, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "G:", "FG:"),
                           a, g)
                    RETURN
 
    CASE s_flocal:
    CASE s_local:   IF c<dvecp DO
                         trnerr("Dynamic free variable '%s' used", @h3!x)
                    out2(p, a)
                    //IF xrefing DO
                    //  xref(x,
                    //       ((k & s_fltbit)=0 -> "P:", "FP:"),
                    //       a, p)
                    RETURN
 
    CASE s_path1: CASE s_fpath1:
    CASE s_path2: CASE s_fpath2:
    CASE s_path3: CASE s_fpath3:
    CASE s_path4: CASE s_fpath4:
//   writef("transname: %s op=%s k=%s n=%n path=%x8*n",
//           @h3!x, opname(p), opname(k), n, path)
//   abort(5522)
                    IF c<dvecp DO
                         trnerr("Dynamic free variable '%s' used", @h3!x)
                    oppath(p,                      // s_lp, s_llp or s_sp
		           a,                      // argpos
		           (k&s_fltmask)-s_path1+1,// depth
		           path)                   // path
//   abort(5523)
                    //IF xrefing DO
                    //  xref(x,
                    //       ((k & s_fltbit)=0 -> "T:", "FT:"),
                    //       a, p)
                    RETURN
 
    CASE s_fstatic:
    CASE s_static:  out2(l, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "S:", "FS:"),
                           a, l)
                    RETURN
 
    CASE s_label:   IF f=0 DO
                    { trnerr("Misuse of entry name '%s'", @h3!x)
                      f := p
                    }
                    out2(f, a)
                    IF xrefing DO xref(x, "F:", a, f)
                    RETURN

    CASE s_fmanifest:
    CASE s_manifest:IF n=0 DO
                    { trnerr("Misuse of MANIFEST name '%s'", @h3!x)
                      n := p
                    }
                    out2(n, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "M:", "FM:"),
                           a, n)
  }
}

AND xref(x, kstr, n, op) BE
{ // Output a line of cross reference info
  // x is the name node
  // kstr, n describe how the name is being used
  // op decribes the context
  LET name = @h3!x
  LET fno = comline>>20
  LET lno = comline & #xFFFFF
  LET file = sourcenamev!fno
  writef("%s %s", name, kstr)
  TEST -10_000_000 <= n <= 10_000_000
  THEN writef("%n ", n)
  ELSE writef("#x%8x ", n)

  SWITCHON op INTO
  { DEFAULT:         writef("op%n", op); ENDCASE

    CASE s_fndef:    writef("FN");       ENDCASE
    CASE s_rtdef:    writef("RT");       ENDCASE
    CASE s_local:    writef("LOC");      ENDCASE
    CASE s_valdef:   writef("VAL");      ENDCASE
    CASE s_vecdef:   writef("VEC");      ENDCASE
    CASE s_constdef: writef("DEF");      ENDCASE
    CASE s_const:    writef("MAN");      ENDCASE
    CASE s_colon:    writef("LAB");      ENDCASE
    CASE s_sp:       writef("SP");       ENDCASE
    CASE s_sg:       writef("SG");       ENDCASE
    CASE s_sl:       writef("SL");       ENDCASE
    CASE s_llp:      writef("LLP");      ENDCASE
    CASE s_llg:      writef("LLG");      ENDCASE
    CASE s_lll:      writef("LLL");      ENDCASE
    CASE s_lp:       writef("LP");       ENDCASE
    CASE s_lg:       writef("LG");       ENDCASE
    CASE s_ll:       writef("LL");       ENDCASE
    CASE s_lf:       writef("LF");       ENDCASE
    CASE s_ln:       writef("LN");       ENDCASE
  }
  wrch(' ')
  IF file DO writef("%s", file)
  writef("[%n] ", lno)

  prctxt(context)

  newline()
}

AND prctxt(x) BE IF x DO 
{ LET op, str = h1!x, ""

  SWITCHON op INTO
  { DEFAULT:  prctxte(x, 7, 0); RETURN

    CASE s_fndef:
         writef("LET ")
         prctxte(h2!x, 7, 0)
         wrch('(')
         prctxte(h3!x, 7, 0)
         writef(")=..")
         RETURN

    CASE s_rtdef:
         writef("LET ")
         prctxte(h2!x, 7, 0)
         wrch('(')
         prctxte(h3!x, 7, 0)
         writef(")BE..")
         RETURN

    CASE s_valdef:
         writef("LET ")
         prctxte(h2!x, 6, 0)
         writef("=")
         prctxte(h3!x, 6, 0)
         RETURN

    CASE s_vecdef:
         writef("LET ")
         prctxte(h2!x, 6, 0)
         writef("=VEC ")
         prctxte(h3!x, 6, 0)
         RETURN

    CASE s_constdef:
         prctxte(h3!x, 6, 0)
         writef("=")
         prctxte(h4!x, 6, 0)
         RETURN

    CASE s_let:
         writef("LET ")
         prctxtd(h2!x, 6)
         writef("; ")
         prctxtc(h3!x, 6)
         RETURN
 
    CASE s_static:    writef("STATIC..");    RETURN
    CASE s_global:    writef("GLOBAL..");    RETURN
    CASE s_manifest:  writef("MANIFEST..");  RETURN


    CASE s_assvecap:  str := "!";    GOTO case_ass
    CASE s_assfmul:   str := "#**";  GOTO case_ass
    CASE s_assfdiv:   str := "#/";   GOTO case_ass
    CASE s_assfmod:   str := "#MOD"; GOTO case_ass
    CASE s_assfadd:   str := "#+";   GOTO case_ass
    CASE s_assfsub:   str := "#-";   GOTO case_ass
    CASE s_assmul:    str := "**";   GOTO case_ass
    CASE s_assdiv:    str := "/";    GOTO case_ass
    CASE s_assmod:    str := "MOD";  GOTO case_ass
    CASE s_assadd:    str := "+";    GOTO case_ass
    CASE s_asssub:    str := "-";    GOTO case_ass
    CASE s_asslshift: str := "<<";   GOTO case_ass
    CASE s_assrshift: str := ">>";   GOTO case_ass
    CASE s_asslogand: str := "&";    GOTO case_ass
    CASE s_asslogor:  str := "|";    GOTO case_ass
    CASE s_asseqv:    str := "EQV";  GOTO case_ass
    CASE s_assxor:    str := "XOR";  GOTO case_ass

    CASE s_fass:      str := "#";    GOTO case_ass

    CASE s_ass:       str := ""
case_ass:
         prctxte(h2!x, 4, 0)
         writef("%s:=", str)
         prctxte(h3!x, 4, 0)
         RETURN
 
    CASE s_rtap:
         prctxte(h2!x, 6, 12)
         writef("(")
         prctxte(h3!x, 6, 0)
         writef(")")
         RETURN
 
    CASE s_goto:
         writef("GOTO ")
         prctxte(h2!x, 6, 0)
         RETURN
 
    CASE s_colon:
         prctxte(h2!x, 6, 0)
         writef(":")
         prctxt(h3!x, 6)
         RETURN
 
    CASE s_unless:
    CASE s_if:
    CASE s_while:
    CASE s_until:
         writef(op=s_unless->"UNLESS ",
                op=s_if->"IF ",
                op=s_until->"UNTIL ",
                "WHILE "
               )
         prctxte(h2!x, 6, 0)
         writef(" DO ")
         prctxtc(h3!x, 6)
         RETURN

 
    CASE s_test:
         writef("TEST ")
         prctxte(h2!x, 6, 0)
         writef(" THEN ")
         prctxtc(h3!x, 6)
         writef(" ELSE ")
         prctxtc(h4!x, 6)
         RETURN
 
    CASE s_loop:
         writef("LOOP")
         RETURN
 
    CASE s_exit:
         writef("EXIT")
         RETURN
 
    CASE s_next:
         writef("NEXT")
         RETURN
 
    CASE s_skip:
         writef("{}")
         RETURN
 
    CASE s_break:
         writef("BREAK")
         RETURN
 
    CASE s_return:
         writef("RETURN")
         RETURN
 
    CASE s_finish:
         writef("FINISH")
         RETURN
 
    CASE s_resultis:
         writef("RESULTIS ")
         prctxte(h2!x, 6, 0)
         RETURN
 
    CASE s_repeatwhile:
    CASE s_repeatuntil:
         prctxtc(h2!x, 6)
         writef(op=s_repeatwhile -> " REPEATWHILE ", " REPEATUNTIL ")
         prctxte(h3!x, 6, 0)
         RETURN
 
    CASE s_repeat:
         prctxtc(h2!x, 6)
         writef(" REPEAT")
         RETURN
 
    CASE s_case:
         writef("CASE ")
         prctxte(h2!x, 6, 0)
         writef(":.. ")
         RETURN
 
    CASE s_default:
         writef("DEFAULT:..")
         RETURN
 
    CASE s_endcase:
         writef("ENDCASE")
         RETURN
 
    CASE s_switchon:
         writef("SWITCHON ")
         prctxte(h2!x, 6, 0)
         writef(" INTO..")
         RETURN
 
    CASE s_for:
         writef("FOR ")
         prctxte(h2!x, 6, 0)
         writef("=")
         prctxte(h3!x, 6, 0)
         writef(" TO ")
         prctxte(h4!x, 6, 0)
         IF h5!x DO { writef(" BY "); prctxte(h5!x, 6, 0) }
         writef(" DO..")
         RETURN
 
    CASE s_seq:
         prctxtc(h2!x, 6)
         writef(";")
         prctxtc(h3!x, 6)
         RETURN
  }
}

AND prctxtd(x, d) BE writef("..")
AND prctxtc(x, d) BE writef("..")

AND wrhexval(n) BE
{ LET lsdig = n & #xFF
  n := n>>8
  IF n DO wrhexval(n)
  writef("%2x", lsdig)
}

AND prctxte(x, d, prec) BE IF x DO
{ LET op = h1!x

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_number: 
                 { LET n = h2!x
                   TEST -1_000_000<=n<=1_000_000
                   THEN writef("%n", n)
                   ELSE { IF n<0 DO
		          { wrch('-')
			    n := -n
			  }
		          writef("#x")
			  wrhexval(n)
			}
                   RETURN
                 } 
    
    CASE s_fnum: 
                 { LET n = h2!x
                   writef("%5.3f", n)
                   RETURN
                 } 
    
    CASE s_flt :   writef("FLT %s", @h3!(h2!x)); RETURN
    CASE s_name:   writef("%s", @h3!x);          RETURN
    CASE s_true:   writef("TRUE");               RETURN
    CASE s_false:  writef("FALSE");              RETURN
    CASE s_query:  wrch('?');                    RETURN

    CASE s_string: 
                 { LET s = @h2!x
                   LET len = s%0
                   wrch('"')
                   FOR i = 1 TO len DO
                   { LET ch = s%i
                     IF i=6 & len>6+8 DO { writef("'"); LOOP }
                     IF i<=6 | i>len-8 DO // First 5 and last 8 chars
                     { SWITCHON ch INTO
                       { CASE '**': writef("****"); LOOP
                         CASE '*"': writef("***""); LOOP
                         CASE '*n': writef("**n");  LOOP
                       }
                       UNLESS 32<=ch<=127 DO ch := '?'
                       wrch(ch)
                     }
                   }
                   wrch('"')
                   RETURN
                 }

  }

  IF d=0 DO { writef("..."); RETURN }

  IF prec>=12 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_fnap:
         prctxte(h2!x, d-1, 11)
         wrch('(')
         prctxte(h3!x, d-1, 0)
         wrch(')')
         RETURN
  }

  IF prec>=11 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_slct:
      {  writes("SLCT ")
         prctxte(h2!x, d-1, 10)
         writes(":")
         prctxte(h3!x, d-1, 10)
         writes(":")
         prctxte(h4!x, d-1, 10)
         RETURN
      }

    CASE s_of:
    CASE s_byteap:
    CASE s_vecap:
         prctxte(h2!x, d-1, 10)
         writes(op=s_of->"::", op=s_byteap->"%", "!")
         prctxte(h3!x, d-1, 10)
         RETURN

    CASE s_float:
    CASE s_fix:
    CASE s_rv:
    CASE s_lv:
         writef(op=s_float->"FLOAT ",
                op=s_fix  ->"FIX ",
                op=s_rv   ->"!",
                            "@")
         prctxte(h2!x, d-1, 10)
         RETURN
  }

  IF prec>=10 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_mul: CASE s_div: CASE s_mod:
         prctxte(h2!x, d-1, 9)
         writef(op=s_mul->"**", op=s_div->"/", " MOD ")
         prctxte(h3!x, d-1, 9)
         RETURN

    CASE s_fmul: CASE s_fdiv: CASE s_fmod:
         prctxte(h2!x, d-1, 9)
         writef(op=s_fmul -> "#**",
                op=s_fdiv -> "#/",
                             "#MOD ")
         prctxte(h3!x, d-1, 9)
         RETURN
  }

  IF prec>=9 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_add:
    CASE s_sub:
         prctxte(h2!x, d-1, 8)
         writef(op=s_add->"+","-")
         prctxte(h3!x, d-1, 8)
         RETURN

    CASE s_fadd:
    CASE s_fsub:
         prctxte(h2!x, d-1, 8)
         writef(op=s_fadd->"#+","#-")
         prctxte(h3!x, d-1, 8)
         RETURN

    CASE s_neg:
    CASE s_fneg:
    CASE s_fabs:
    CASE s_abs:
         writef(op=s_neg ->"-",
                op=s_fneg->"#-",
                op=s_fabs->"#ABS ",
                           "ABS ")
         prctxte(h2!x, d-1, 8)
         RETURN
  }

  IF prec>=8 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_eq: CASE s_ne:
         prctxte(h2!x, d-1, 7)
         writef(op=s_eq->"=","~=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_feq: CASE s_fne:
         prctxte(h2!x, d-1, 7)
         writef(op=s_feq->"#=","#~=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_ls: CASE s_gr:
         prctxte(h2!x, d-1, 7)
         writef(op=s_ls->"<",">")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_fls: CASE s_fgr:
         prctxte(h2!x, d-1, 7)
         writef(op=s_fls->"#<","#>")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_le: CASE s_ge:
         prctxte(h2!x, d-1, 7)
         writef(op=s_le->"<=",">=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_fle: CASE s_fge:
         prctxte(h2!x, d-1, 7)
         writef(op=s_fle->"#<=","#>=")
         prctxte(h3!x, d-1, 7)
         RETURN
  }

  IF prec>=7 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_lshift: CASE s_rshift:
         prctxte(h2!x, d-1, 6)
         writef(op=s_lshift->"<<",">>")
         prctxte(h3!x, d-1, 6)
         RETURN
  }

  IF prec>=6 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_not:
         wrch('~')
         prctxte(h2!x, d-1, 5)
         RETURN
  }

  IF prec>=5 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_logand:
         prctxte(h2!x, d-1, 4)
         wrch('&')
         prctxte(h3!x, d-1, 4)
         RETURN
  }

  IF prec>=4 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_logor:
         prctxte(h2!x, d-1, 3)
         wrch('|')
         prctxte(h3!x, d-1, 3)
         RETURN
  }

  IF prec>=3 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_eqv:
    CASE s_xor:
         prctxte(h2!x, d-1, 2)
         writef(op=s_eqv->" EQV "," XOR ")
         prctxte(h3!x, d-1, 2)
         RETURN

  }

  IF prec>=2 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_cond:
    CASE s_fcond:
         prctxte(h2!x, d-1, 1)
         writef(op=s_cond -> "->", "#->")
         prctxte(h3!x, d-1, 1)
         writef(",")
         prctxte(h4!x, d-1, 1)
         RETURN
  }

  IF prec>=1 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: writef("Op%n", op); RETURN

    CASE s_table:
         writef("TABLE ")
         prctxte(h2!x, d-1, 0)
         RETURN
         
    CASE s_valof:
         writef("VALOF{")
         prctxtc(h2!x, d-1)
         wrch('}')
         RETURN

    CASE s_comma:
         prctxte(h2!x, d-1, 0)
         writef(",")
         prctxte(h3!x, d-1, 0)
         RETURN
  }
}


AND out1(x) BE wrn(x)
 
AND out2(x, y) BE { out1(x); out1(y) }
 
AND out3(x, y, z) BE { out1(x); out1(y); out1(z) }
 
AND out4(x, y, z, t) BE { out1(x); out1(y); out1(z); out1(t) }
 
AND outstring(s) BE FOR i = 0 TO s%0 DO out1(s%i)

