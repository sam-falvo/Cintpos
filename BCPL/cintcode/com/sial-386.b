/*
This program has been superceded by sial-686.b and
will soon be removed.
*/

SECTION "sial-386"

GET "libhdr"
GET "sial.h"

GLOBAL {
sialin: ug
asmout; stdin; stdout

rdf; rdp; rdg; rdk; rdw; rdl; rdc
rdcode

pval; gval; kval; k1val; k2val; wval; lval; mval

scan
cvf; cvfp; cvfg; cvfk; cvfw; cvfl

sectname
modletter
charv
labnumber
tracing
}

LET start() = VALOF
{ LET argv = VEC 20
  LET v    = VEC 20
  LET cv   = VEC 256/bytesperword

  sectname := v
  sectname%0 := 0
  modletter := 'A'
  charv := cv
  labnumber := 0

  asmout := 0
  stdout := output()
  IF rdargs("FROM,TO/K,-t/S", argv, 20)=0 DO
  { writes("Bad args for sial-386*n")
    RESULTIS 20
  }
  IF argv!0=0 DO argv!0 := "prog.sial"      // FROM
  IF argv!1=0 DO argv!1 := "prog.s"         // TO/K
  tracing := argv!2                         // -t/S

  sialin := findinput(argv!0)
  IF sialin=0 DO
  { writef("Trouble with file %s*n", argv!0)
    RESULTIS 20
  }
  asmout := findoutput(argv!1)
   
  IF asmout=0 DO
  { writef("Trouble with file %s*n", argv!1)
    RESULTIS 20
  }
   
  writef("Converting %s to %s*n", argv!0, argv!1)
  selectinput(sialin)
  selectoutput(asmout)

  writef("# Code generated by sial-386*n*n")
  writef(".text*n.align 16*n")

  scan()
  endread()
  UNLESS asmout=stdout DO endwrite()
  selectoutput(stdout)
  writef("Conversion complete*n")
  RESULTIS 0
}

AND nextlab() = VALOF
{ labnumber := labnumber+1
  RESULTIS labnumber
}

// argument may be of form Ln
AND rdcode(let) = VALOF
{ LET a, ch, neg = 0, ?, FALSE

  ch := rdch() REPEATWHILE ch='*s' | ch='*n'

  IF ch=endstreamch RESULTIS -1

  UNLESS ch=let DO error("Bad item, looking for %c found %c*n", let, ch)

  ch := rdch()

  IF ch='-' DO { neg := TRUE; ch := rdch() }

  WHILE '0'<=ch<='9' DO { a := 10*a + ch - '0'; ch := rdch()  }

  RESULTIS neg -> -a, a
}

AND rdf() = rdcode('F')
AND rdp() = VALOF { pval := rdcode('P'); RESULTIS pval }
AND rdg() = VALOF { gval := rdcode('G'); RESULTIS gval }
AND rdk() = VALOF { kval := rdcode('K'); RESULTIS kval }
AND rdk1() = VALOF { k1val := rdcode('K'); RESULTIS k1val }
AND rdk2() = VALOF { k2val := rdcode('K'); RESULTIS k2val }
AND rdw() = VALOF { wval := rdcode('W'); RESULTIS wval }
AND rdl() = VALOF { lval := rdcode('L'); RESULTIS lval }
AND rdm() = VALOF { mval := rdcode('M'); RESULTIS mval }
AND rdc() = rdcode('C')

AND error(mess, a, b, c) BE
{ LET out = output()
  UNLESS out=stdout DO
  { selectoutput(stdout)
    writef(mess, a, b, c)
    selectoutput(out)
  }
  writef(mess, a, b, c)
}

AND scan() BE
{ LET op = rdf()

  SWITCHON op INTO

  { DEFAULT:       error("# Bad SIAL op %n*n", op); LOOP

    CASE -1:       RETURN
      
    CASE f_lp:     cvfp("LP") // a := P!n
                   writef("*n movl %n(%%ebp),%%ebx", 4*pval)
                   ENDCASE
    CASE f_lg:     cvfg("LG") // a := G!n
                   writef("*n movl %n(%%esi),%%ebx", 4*gval)
                   ENDCASE
    CASE f_ll:     cvfl("LL") // a := !Ln
                   writef("*n movl L%c%n,%%ebx", modletter, lval)
                   ENDCASE

    CASE f_llp:    cvfp("LLP") // a := @ P!n
                   writef("*n leal %n(%%ebp),%%ebx", 4*pval)
                   writef("*n shrl $2,%%ebx")
                   ENDCASE
    CASE f_llg:    cvfg("LLG") // a := @ G!n
                   writef("*n leal %n(%%esi),%%ebx", 4*gval)
                   writef("*n shrl $2,%%ebx")
                   ENDCASE
    CASE f_lll:    cvfl("LLL") // a := @ !Ln
                   writef("*n leal L%c%n,%%ebx", modletter, lval)
                   writef("*n shrl $2,%%ebx")
                   ENDCASE
    CASE f_lf:     cvfl("LF") // a := byte address of Ln
                   writef("*n leal L%c%n,%%ebx", modletter, lval)
                   ENDCASE
    CASE f_lw:     cvfm("LW")
                   writef("*n movl M%c%n,%%ebx", modletter, mval)
                   ENDCASE

    CASE f_l:      cvfk("L") // a := n
                   IF kval=0 DO { writef("*n xorl %%ebx,%%ebx"); ENDCASE }
                   writef("*n movl $%n,%%ebx", kval)
                   ENDCASE
    CASE f_lm:     cvfk("LM") // a := -n
                   writef("*n movl $-%n,%%ebx", kval)
                   ENDCASE

    CASE f_sp:     cvfp("SP") // P!n := a
                   writef("*n movl %%ebx,%n(%%ebp)", 4*pval)
                   ENDCASE
    CASE f_sg:     cvfg("SG") // G!n := a
                   writef("*n movl %%ebx,%n(%%esi)", 4*gval)
                   ENDCASE
    CASE f_sl:     cvfl("SL") // !Ln := a
                   writef("*n movl %%ebx,L%c%n", modletter, lval)
                   ENDCASE

    CASE f_ap:     cvfp("AP") // a := a + P!n
                   writef("*n addl %n(%%ebp),%%ebx", 4*pval)
                   ENDCASE
    CASE f_ag:     cvfg("AG") // a := a + G!n
                   writef("*n addl %n(%%esi),%%ebx", 4*gval)
                   ENDCASE
    CASE f_a:      cvfk("A") // a := a + n
                   IF kval=0 ENDCASE
                   IF kval=1  DO { writef("*n incl %%ebx"); ENDCASE }
                   IF kval=-1 DO { writef("*n decl %%ebx"); ENDCASE }
                   writef("*n addl $%n,%%ebx", kval)
                   ENDCASE
    CASE f_s:      cvfk("S")  // a := a - n
                   IF kval=0 ENDCASE
                   IF kval=1  DO { writef("*n decl %%ebx"); ENDCASE }
                   IF kval=-1 DO { writef("*n incl %%ebx"); ENDCASE }
                   writef("*n subl $%n,%%ebx", kval)
                   ENDCASE

    CASE f_lkp:    cvfkp("LKP") // a := P!n!k
                   writef("*n movl %n(%%ebp),%%eax", 4*pval)
                   writef("*n movl %n(,%%eax,4),%%ebx", 4*kval)
                   ENDCASE
    CASE f_lkg:    cvfkg("LKG") // a := G!n!k
                   writef("*n movl %n(%%esi),%%eax", 4*gval)
                   writef("*n movl %n(,%%eax,4),%%ebx", 4*kval)
                   ENDCASE
    CASE f_rv:     cvf("RV")  // a := ! a
                   writef("*n movl (,%%ebx,4),%%ebx")
                   ENDCASE
    CASE f_rvp:    cvfp("RVP") // a := P!n!a
                   writef("*n addl %n(%%ebp),%%ebx", 4*pval)
                   writef("*n movl (,%%ebx,4),%%ebx")
                   ENDCASE
    CASE f_rvk:    cvfk("RVK") // a := a!k
                   writef("*n movl %n(,%%ebx,4),%%ebx", 4*kval)
                   ENDCASE
    CASE f_st:     cvf("ST") // !a := b
                   writef("*n movl %%ecx,(,%%ebx,4)")
                   ENDCASE
    CASE f_stp:    cvfp("STP") // P!n!a := b
                   writef("*n movl %n(%%ebp),%%eax", 4*pval)
                   writef("*n addl %%ebx,%%eax")
                   writef("*n movl %%ecx,(,%%eax,4)")
                   ENDCASE
    CASE f_stk:    cvfk("STK") // a!n := b
                   writef("*n movl %%ecx,%n(,%%ebx,4)", 4*kval)
                   ENDCASE
    CASE f_stkp:   cvfkp("STKP")  // P!n!k := a
                   writef("*n movl %n(%%ebp),%%eax", 4*pval)
                   writef("*n movl %%ebx,%n(,%%eax,4)", 4*kval)
                   ENDCASE
    CASE f_skg:    cvfkg("SKG") // G!n!k := a
                   writef("*n movl %n(%%esi),%%eax", 4*gval)
                   writef("*n movl %%ebx,%n(,%%eax,4)", 4*kval)
                   ENDCASE
    CASE f_xst:    cvf("XST") // !b := a
                   writef("*n movl %%ebx,(,%%ecx,4)")
                   ENDCASE

    CASE f_k:      cvfp("K") // Call  a(b,...) incrementing P by n
                   writef("*n movl %%ebx,%%eax")
                   writef("*n movl %%ecx,%%ebx")
                   writef("*n leal %n(%%ebp),%%edx", 4*pval)
                   writef("*n call **%%eax")
                   ENDCASE
    CASE f_kpg:    cvfpg("KPG") // Call Gg(a,...) incrementing P by n
                   writef("*n movl %n(%%esi),%%eax", 4*gval)
                   writef("*n leal %n(%%ebp),%%edx", 4*pval)
                   writef("*n call **%%eax")
                   ENDCASE

    CASE f_neg:    cvf("NEG") // a := - a
                   writef("*n negl %%ebx") 
                   ENDCASE
    CASE f_not:    cvf("NOT") // a := ~ a
                   writef("*n notl %%ebx") 
                   ENDCASE
    CASE f_abs:    cvf("ABS") // a := ABS a
                 { LET l = nextlab()
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n jge L%n", l)
                   writef("*n negl %%ebx")
                   writef("*nL%n:", l)
                   ENDCASE
                 }

    CASE f_xdiv:   cvf("XDIV") // a := a / b
                   writef("*n movl %%ebx,%%eax")
                   writef("*n cdq")
                   writef("*n idiv %%ecx")
                   writef("*n movl %%eax,%%ebx")
                   ENDCASE
    CASE f_xmod:   cvf("XMOD") // a := a MOD b
                   writef("*n movl %%ebx,%%eax")
                   writef("*n cdq")
                   writef("*n idiv %%ecx")
                   writef("*n movl %%edx,%%ebx")
                   ENDCASE
    CASE f_xsub:   cvf("XSUB") // a := a - b
                   writef("*n subl %%ecx,%%ebx")
                   ENDCASE

    CASE f_mul:    cvf("MUL") // a := b * a; c := ?
                   writef("*n movl %%ecx,%%eax")
                   writef("*n imul %%ebx") // currupts edx
                   writef("*n movl %%eax,%%ebx")
                   ENDCASE
    CASE f_div:    cvf("DIV")  // a := b / a; c := ?
                   writef("*n movl %%ecx,%%eax")
                   writef("*n cdq")
                   writef("*n idiv %%ebx")
                   writef("*n movl %%eax,%%ebx")
                   ENDCASE
    CASE f_mod:    cvf("MOD") // a := b MOD a; c := ?
                   writef("*n movl %%ecx,%%eax")
                   writef("*n cdq")
                   writef("*n idiv %%ebx")
                   writef("*n movl %%edx,%%ebx")
                   ENDCASE
    CASE f_add:    cvf("ADD") // a := b + a
                   writef("*n addl %%ecx,%%ebx")
                   ENDCASE
    CASE f_sub:    cvf("SUB") // a := b - a
                   writef("*n subl %%ecx,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE

    CASE f_eq:     cvf("EQ") // a := b = a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n seteb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_ne:     cvf("NE") // a := b ~= a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n setneb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_ls:     cvf("LS") // a := b < a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n setlb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_gr:     cvf("GR") // a := b > a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n setgb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_le:     cvf("LE") // a := b <= a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n setleb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_ge:     cvf("GE") // a := b >= a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n setgeb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE

    CASE f_eq0:    cvf("EQ0") // a := a = 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n seteb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_ne0:    cvf("NE0") // a := a ~= 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n setneb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_ls0:    cvf("LS0") // a := a < 0
                   writef("*n sarl $31,%%ebx")
                   ENDCASE
    CASE f_gr0:    cvf("GR0") // a := a > 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n setgb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_le0:    cvf("LE0") // a := a <= 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n setleb %%bl")
                   writef("*n movzbl %%bl,%%ebx")
                   writef("*n negl %%ebx")
                   ENDCASE
    CASE f_ge0:    cvf("GE0") // a := a >= 0
                   writef("*n sarl $31,%%ebx")
                   writef("*n notl %%ebx")
                   ENDCASE

    CASE f_lsh:    cvf("LSH") // a := b << a; b := ?
                   writef("*n xchgl %%ebx,%%ecx")
                   writef("*n cmpl $32,%%ecx")
                   writef("*n sbbl %%eax,%%eax")  // set eax to -1 or 0
                   writef("*n andl %%eax,%%ebx")  // set ebx to b or 0
                   writef("*n sall %%cl,%%ebx")   // now shift it
                   ENDCASE
    CASE f_rsh:    cvf("RSH") // a := b >> a; b := ?
                   writef("*n xchgl %%ebx,%%ecx")
                   writef("*n cmpl $32,%%ecx")
                   writef("*n sbbl %%eax,%%eax")  // set eax to -1 or 0
                   writef("*n andl %%eax,%%ebx")  // set ebx to b or 0
                   writef("*n shrl %%cl,%%ebx")   // now shift it
                   ENDCASE
    CASE f_and:    cvf("AND") // a := b & a 
                   writef("*n andl %%ecx,%%ebx") 
                   ENDCASE
    CASE f_or:     cvf("OR") // a := b | a 
                   writef("*n orl %%ecx,%%ebx") 
                   ENDCASE
    CASE f_xor:    cvf("XOR") // a := b XOR a
                   writef("*n xorl %%ecx,%%ebx") 
                   ENDCASE
    CASE f_eqv:    cvf("EQV") // a := b EQV a 
                   writef("*n xorl %%ecx,%%ebx") 
                   writef("*n notl %%ebx") 
                   ENDCASE

    CASE f_gbyt:   cvf("GBYT") // a := b % a
                   writef("*n movzbl (%%ebx,%%ecx,4),%%ebx") 
                   ENDCASE
    CASE f_xgbyt:  cvf("XGBYT") // a := a % b 
                   writef("*n movzbl (%%ecx,%%ebx,4),%%ebx") 
                   ENDCASE
    CASE f_pbyt:   cvf("PBYT") // b % a := c
                   writef("*n movb %%dl,(%%ebx,%%ecx,4)") 
                   ENDCASE
    CASE f_xpbyt:  cvf("XPBYT") // a % b := c 
                   writef("*n movb %%dl,(%%ecx,%%ebx,4)") 
                   ENDCASE

// swb       Kn Ld K1 L1 ... Kn Ln   Binary chop switch, Ld default
    CASE f_swb:    cvswb()
                   ENDCASE

// swl       Kn Ld L1 ... Ln         Label vector switch, Ld default
    CASE f_swl:    cvswl()
                   ENDCASE

    CASE f_xch:    cvf("XCH") // swap a and b
                   writef("*n xchgl %%ebx,%%ecx")
                   ENDCASE
    CASE f_atb:    cvf("ATB") // b := a
                   writef("*n movl %%ebx,%%ecx")
                   ENDCASE
    CASE f_atc:    cvf("ATC") // c := a
                   writef("*n movl %%ebx,%%edx")
                   ENDCASE
    CASE f_bta:    cvf("BTA") // a := b
                   writef("*n movl %%ecx,%%ebx")
                   ENDCASE
    CASE f_btc:    cvf("BTC") // c := b
                   writef("*n movl %%ecx,%%edx")
                   ENDCASE
    CASE f_atblp:  cvfp("ATBLP") // b := a; a := P!n
                   writef("*n movl %%ebx,%%ecx")
                   writef("*n movl %n(%%ebp),%%ebx", 4*pval)
                   ENDCASE
    CASE f_atblg:  cvfg("ATBLG") // b := a; a := G!n
                   writef("*n movl %%ebx,%%ecx")
                   writef("*n movl %n(%%esi),%%ebx", 4*gval)
                   ENDCASE
    CASE f_atbl:   cvfk("ATBL") // b := a; a := k
                   writef("*n movl %%ebx,%%ecx")
                   writef("*n movl $%n,%%ebx", kval)
                   ENDCASE

    CASE f_j:      cvfl("J") // jump to Ln
                   writef("*n jmp L%c%n", modletter, lval)
                   ENDCASE
    CASE f_rtn:    cvf("RTN") // procedure return
                   writef("*n movl 4(%%ebp),%%eax")
                   writef("*n movl 0(%%ebp),%%ebp")
                   writef("*n jmp **%%eax")
                   ENDCASE
    CASE f_goto:   cvf("GOTO") // jump to a
                   writef("*n jmp **%%ebx")
                   ENDCASE

    CASE f_res:    cvf("RES")   // <res> := A
                                // <res> is already in A
                                // nothing to do
                   ENDCASE

    CASE f_ldres:  cvf("LSRES") // A := <res>
                                // A and B already set properly
                   ENDCASE

    CASE f_ikp:    cvfkp("IKP") // a := P!n + k; P!n := a
                   writef("*n movl %n(%%ebp),%%ebx", 4*pval)
                   TEST kval=1
                   THEN writef("*n incl %%ebx")
                   ELSE TEST kval=-1
                        THEN writef("*n decl %%ebx")
                        ELSE writef("*n addl $%n,%%ebx", kval)
                   writef("*n movl %%ebx,%n(%%ebp)", 4*pval)
                   ENDCASE
    CASE f_ikg:    cvfkg("IKG") // a := G!n + k; G!n := a
                   writef("*n movl %n(%%esi),%%ebx", 4*gval)
                   TEST kval=1
                   THEN writef("*n incl %%ebx")
                   ELSE TEST kval=-1
                        THEN writef("*n decl %%ebx")
                        ELSE writef("*n addl $%n,%%ebx", kval)
                   writef("*n movl %%ebx,%n(%%esi)", 4*gval)
                   ENDCASE
    CASE f_ikl:    cvfkl("IKL") // a := !Ln + k; !Ln := a
                   writef("*n movl L%c%n,%%ebx", modletter, lval)
                   TEST kval=1
                   THEN writef("*n incl %%ebx")
                   ELSE TEST kval=-1
                        THEN writef("*n decl %%ebx")
                        ELSE writef("*n addl $%n,%%ebx", kval)
                   writef("*n movl %%ebx,L%c%n", modletter, lval)
                   ENDCASE
    CASE f_ip:     cvfp("IP") // a := P!n + a; P!n := a
                   writef("*n addl %n(%%ebp),%%ebx", 4*pval)
                   writef("*n movl %%ebx,%n(%%ebp)", 4*pval)
                   ENDCASE
    CASE f_ig:     cvfg("IG") // a := G!n + a; G!n := a
                   writef("*n addl %n(%%esi),%%ebx", 4*gval)
                   writef("*n movl %%ebx,%n(%%esi)", 4*gval)
                   ENDCASE
    CASE f_il:     cvfl("IL") // a := !Ln + a; !Ln := a
                   writef("*n addl L%c%n,%%ebx", modletter, lval)
                   writef("*n movl %%ebx,L%c%n", modletter, lval)
                   ENDCASE

    CASE f_jeq:    cvfl("JEQ") // Jump to Ln if b = a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n je L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jne:    cvfl("JNE") // Jump to Ln if b ~= a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n jne L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jls:    cvfl("JLS") // Jump to Ln if b < a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n jl L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jgr:    cvfl("JGR") // Jump to Ln if b > a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n jg L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jle:    cvfl("JLE") // Jump to Ln if b <= a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n jle L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jge:    cvfl("JGE") // Jump to Ln if b >= a
                   writef("*n cmpl %%ebx,%%ecx")
                   writef("*n jge L%c%n", modletter, lval)
                   ENDCASE

    CASE f_jeq0:   cvfl("JEQ0") // Jump to Ln if a = 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n je L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jne0:   cvfl("JNE0") // Jump to Ln if a ~= 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n jne L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jls0:   cvfl("JLS0") // Jump to Ln if a < 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n jl L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jgr0:   cvfl("JGR0") // Jump to Ln if a > 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n jg L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jle0:   cvfl("JLE0") // Jump to Ln if a <= 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n jle L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jge0:   cvfl("JGE0") // Jump to Ln if a >= 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n jge L%c%n", modletter, lval)
                   ENDCASE
    CASE f_jge0m:  cvfm("JGE0M") // Jump to Mn if a >= 0
                   writef("*n orl %%ebx,%%ebx")
                   writef("*n jge M%c%n", modletter, mval)
                   ENDCASE

    // The following five opcodes are never generated by
    // the BCPL compiler
    CASE f_brk:    cvf("BRK") // Breakpoint instruction
                   writef("*n BRK not implemented*n")
                   ENDCASE
    CASE f_nop:    cvf("NOP") // No operation
                   ENDCASE
    CASE f_chgco:  cvf("CHGCO") // Change coroutine
                   writef("*n CHGCO not implemented*n")
                   ENDCASE
    CASE f_mdiv:   cvf("MDIV") // a := Muldiv(P!3, P!4, P!5) 
                   writef("*n MDIV not implemented*n")
                   ENDCASE
    CASE f_sys:    cvf("SYS") // System function
                   writef("*n SYS not implemented*n")
                   ENDCASE

    CASE f_section:  cvfs("SECTION") // Name of section
                     FOR i = 0 TO charv%0 DO sectname%i := charv%i
                     ENDCASE
    CASE f_modstart: cvf("MODSTART") // Start of module  
                     sectname%0 := 0
                     ENDCASE
    CASE f_modend:   cvf("MODEND") // End of module 
                     modletter := modletter+1
                     ENDCASE
    CASE f_global:   cvglobal() // Global initialisation data
                     ENDCASE
    CASE f_string:   cvstring() // String constant
                     ENDCASE
    CASE f_const:    cvconst() // Large integer constant
                     ENDCASE

    CASE f_static:   cvstatic() // Static variable or table
                     ENDCASE
    CASE f_mlab:     cvfm("MLAB") // Destination of jge0m
                     writef("*nM%c%n:", modletter, mval)
                     ENDCASE
    CASE f_lab:      cvfl("LAB") // Program label
                     writef("*nL%c%n:", modletter, lval)
                     ENDCASE
    CASE f_lstr:     cvfm("LSTR") // a := Mn   (pointer to string)
                     writef("*n leal M%c%n,%%ebx", modletter, mval)
                     writef("*n shrl $2,%%ebx")
                     ENDCASE
    CASE f_entry:    cventry() // Start of a function
                     ENDCASE

    CASE f_float:    cvf("FLOAT")
                     writef("*n# FLOAT not yet implemented*n")
                     ENDCASE
    CASE f_fix:      cvf("FIX")
                     writef("*n# FIX not yet implemented*n")
                     ENDCASE
    CASE f_fabs:     cvf("FABS")
                     writef("*n# FABS not yet implemented*n")
                     ENDCASE

    CASE f_fmul:     cvf("FMUL")
                     writef("*n# FMUL not yet implemented*n")
                     ENDCASE
    CASE f_fdiv:     cvf("FDIV")
                     writef("*n# FDIV not yet implemented*n")
                     ENDCASE
    CASE f_fmod:     cvf("FMOD")
                     writef("*n# FMOD not yet implemented*n")
                     ENDCASE
    CASE f_fxmod:    cvf("FXMOD")
                     writef("*n# FXMOD not yet implemented*n")
                     ENDCASE
    CASE f_fadd:     cvf("FADD")
                     writef("*n# FADD not yet implemented*n")
                     ENDCASE
    CASE f_fsub:     cvf("FSUB")
                     writef("*n# FSUB not yet implemented*n")
                     ENDCASE
    CASE f_fxsub:    cvf("FXSUB")
                     writef("*n# FXSUB not yet implemented*n")
                     ENDCASE
    CASE f_fneg:     cvf("FNEG")
                     writef("*n# FNEG not yet implemented*n")
                     ENDCASE

    CASE f_feq:      cvf("FEQ")
                     writef("*n# FEQ not yet implemented*n")
                     ENDCASE
    CASE f_fne:      cvf("FNE")
                     writef("*n# FNE not yet implemented*n")
                     ENDCASE
    CASE f_fls:      cvf("FLS")
                     writef("*n# FLS not yet implemented*n")
                     ENDCASE
    CASE f_fgr:      cvf("FGR")
                     writef("*n# FGR not yet implemented*n")
                     ENDCASE
    CASE f_fle:      cvf("FLE")
                     writef("*n# FLE not yet implemented*n")
                     ENDCASE
    CASE f_fge:      cvf("FGE")
                     writef("*n# FGE not yet implemented*n")
                     ENDCASE
    CASE f_feq0:     cvf("FEQ0")
                     writef("*n# FEQ0 not yet implemented*n")
                     ENDCASE
    CASE f_fne0:     cvf("FNE0")
                     writef("*n# FNE0 not yet implemented*n")
                     ENDCASE
    CASE f_fls0:     cvf("FLS0")
                     writef("*n# FLS0 not yet implemented*n")
                     ENDCASE
    CASE f_fgr0:     cvf("FGR0")
                     writef("*n# FGR0 not yet implemented*n")
                     ENDCASE
    CASE f_fle0:     cvf("FLE0")
                     writef("*n# FLE0 not yet implemented*n")
                     ENDCASE
    CASE f_fge0:     cvf("FGE0")
                     writef("*n# FGE0 not yet implemented*n")
                     ENDCASE



    CASE f_jfeq:     cvfl("JFEQ")
                     writef("*n# JFEQ not yet implemented*n")
                     ENDCASE
    CASE f_jfne:     cvfl("JFNE")
                     writef("*n# JFNE not yet implemented*n")
                     ENDCASE
    CASE f_jfls:     cvfl("JFLS")
                     writef("*n# JFLS not yet implemented*n")
                     ENDCASE
    CASE f_jfgr:     cvfl("JFGR")
                     writef("*n# JFGR not yet implemented*n")
                     ENDCASE
    CASE f_jfle:     cvfl("JFLE")
                     writef("*n# JFLE not yet implemented*n")
                     ENDCASE
    CASE f_jfge:     cvfl("JFGE")
                     writef("*n# JFGE not yet implemented*n")
                     ENDCASE
    CASE f_jfeq0:    cvfl("JFEQ0")
                     writef("*n# JFEQ0 not yet implemented*n")
                     ENDCASE
    CASE f_jfne0:    cvfl("JFNE0")
                     writef("*n# JFNE0 not implemented*n")
                     ENDCASE
    CASE f_jfls0:    cvfl("JFLS0")
                     writef("*n# JFLS0 not yet implemented*n")
                     ENDCASE
    CASE f_jfgr0:     cvfl("JFGR0")
                     writef("*n# JFGR0 not yet implemented*n")
                     ENDCASE
    CASE f_jfle0:    cvfl("JFLE0")
                     writef("*n# JFLE0 not yet implemented*n")
                     ENDCASE
    CASE f_jfge0:    cvfl("JFGE0")
                     writef("*n# JFGE0 not yet implemented*n")
                     ENDCASE


    CASE f_selld:    cvfkk("SELLD")
                     writef("*n# SELLD not yet implemented*n")
                     ENDCASE
    CASE f_selst:    cvffkk("SELST")
                     writef("*n# SELST not yet implemented*n")
                     ENDCASE
    CASE f_xselst:   cvffkk("XSELST")
                     writef("*n# XSELST not yet implemented*n")
                     ENDCASE
  }

  newline()
} REPEAT

AND cvf(s)   BE writef("# %s", s)
AND cvfp(s)  BE writef("# %t7 P%n", s, rdp())
AND cvfkp(s) BE writef("# %t7 K%n P%n", s, rdk(), rdp())
AND cvfg(s)  BE writef("# %t7 G%n", s, rdg())
AND cvfkg(s) BE writef("# %t7 K%n G%n", s, rdk(), rdg())
AND cvfkl(s) BE writef("# %t7 K%n L%n", s, rdk(), rdl())
AND cvfpg(s) BE writef("# %t7 P%n G%n", s, rdp(), rdg())
AND cvfk(s)  BE writef("# %t7 K%n", s, rdk())
AND cvfw(s)  BE writef("# %t7 W%n", s, rdw())
AND cvfl(s)  BE writef("# %t7 L%n", s, rdl())
AND cvfm(s)  BE writef("# %t7 M%n", s, rdm())

AND cvfkk(s) BE writef("%t7 K%n K%n", s, rdk(), rdk())

AND cvffkk(s) BE
{ LET f   = rdk()
  LET len = rdk()
  LET sh  = rdk()
  LET fs  = sfname(f)
  writef("%t7 %s K%n K%n", s, fs, len, sh)
}

AND cvswl() BE
{ LET n = rdk()
  LET l = rdl()
  LET lab = nextlab()
  writef("# SWL K%n L%n", n, l)
  writef("*n orl %%ebx,%%ebx")
  writef("*n jl L%c%n", modletter, l)
  writef("*n cmpl $%n,%%ebx", n)
  writef("*n jge L%c%n", modletter, l)
  writef("*n jmp **L%n(,%%ebx,4)", lab)
  writef("*n .data")
  writef("*n .align 4")
  writef("*nL%n:", lab)
  FOR i = 1 TO n DO
  { writef("*n# L%n", rdl())
    writef("*n .long L%c%n", modletter, lval)
  }
  writef("*n .text")
}

AND cvswb() BE
{ LET n = rdk()
  LET l = rdl()
  writef("# SWB K%n L%n", n, l)
  FOR i = 1 TO n DO 
  { LET k = rdk()
    LET l = rdl()
    writef("*n# K%n L%n", k, l)
    writef("*n cmpl $%n,%%ebx", k)
    writef("*n je L%c%n", modletter, l)
  }
  writef("*n jmp L%c%n", modletter, l)
}

AND cvglobal() BE
{ LET n = rdk()
  writef("# GLOBAL K%n*n", n)
  IF sectname%0=0 FOR i = 0 TO 4 DO sectname%i := "prog"%i
  writef("*n.globl %s*n", sectname)
  writef("*n.globl _%s*n", sectname)
  writef("%s:*n", sectname)
  writef("_%s:*n", sectname)
  writef(" movl 4(%%esp),%%eax*n")
  FOR i = 1 TO n DO
  { LET g = rdg()
    LET n = rdl()
    writef("# G%n L%n*n", g, n)
    writef(" movl $L%c%n,%n(%%eax)*n", modletter, n, 4*g)
  }
  writef("# G%n", rdg())
  writef("*n ret*n")
}

AND rdchars() = VALOF
{ LET n = rdk()
  charv%0 := n
  FOR i = 1 TO n DO charv%i := rdc()
  RESULTIS n
}

AND cvstring() BE
{ LET lab = rdm()
  LET n = rdchars()
  writef("# STRING  M%n K%n", lab, n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
  writef("*n.data")
  writef("*n .align 4")
  writef("*nM%c%n:", modletter, lab)
  FOR i = 0 TO n DO writef("*n .byte %n", charv%i)
  writef("*n .text")
}

AND cvconst() BE
{ LET lab = rdm()
  LET w = rdw()
  writef("# CONST   M%n W%n", lab, w)
  writef("*n.data")
  writef("*n .align 4")
  writef("*nM%c%n:", modletter, lab)
  writef("*n .long %n", w)
  writef("*n .text")
}

AND cvstatic() BE
{ LET lab = rdl()
  LET n = rdk()
  writef("# STATIC  L%n K%n", lab, n)
  writef("*n.data")
  writef("*n .align 4")
  writef("*nL%c%n:", modletter, lab)
  FOR i = 1 TO n DO { writef("*n# W%n", rdw())
                      writef("*n .long %n", wval)
                    }
  writef("*n .text")
}

AND cvfs(s) BE
{ LET n = rdchars()
  writef("# %t7 K%n", s, n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
}

AND cventry() BE
{ LET n = rdchars()
  LET op = rdf()
  LET lab = rdl()
  writef("*n# Entry to: %s*n", charv)
  writef("# %t7 K%n", "ENTRY", n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
  newline()
  TEST op=f_lab THEN writef("# LAB     L%n*n", lab)
                ELSE writef("# Bad SIAL op F%n L%n*n", op, lab)
  writef("*nL%c%n:", modletter, lab)
  writef("*n movl %%ebp,0(%%edx)")   // NP!0 := P
  writef("*n movl %%edx,%%ebp")      // P    := NP
  writef("*n popl %%edx")
  writef("*n movl %%edx,4(%%ebp)")   // P!1  := return address
  writef("*n movl %%eax,8(%%ebp)")   // P!2  := entry address
  writef("*n movl %%ebx,12(%%ebp)")  // P!3  := arg1
}

AND sfname(sfop) = VALOF SWITCHON sfop INTO
{ DEFAULT:        RESULTIS "UNKNOWN"

  CASE 0:         RESULTIS "NULL"
  CASE sf_vecap:  RESULTIS "VECAP"
  CASE sf_fmul:   RESULTIS "FMUL"
  CASE sf_fdiv:   RESULTIS "FDIV"
  CASE sf_fmod:   RESULTIS "FMOD"
  CASE sf_fadd:   RESULTIS "FADD"
  CASE sf_fsub:   RESULTIS "FSUB"
  CASE sf_mul:    RESULTIS "MUL"
  CASE sf_div:    RESULTIS "DIV"
  CASE sf_mod:    RESULTIS "MOD"
  CASE sf_add:    RESULTIS "ADD"
  CASE sf_sub:    RESULTIS "SUB"
  CASE sf_lshift: RESULTIS "LSHIFT"
  CASE sf_rshift: RESULTIS "RSHIFT"
  CASE sf_logand: RESULTIS "LOGAND"
  CASE sf_logor:  RESULTIS "LOGOR"
  CASE sf_eqv:    RESULTIS "EQV"
  CASE sf_xor:    RESULTIS "XOR"
}
