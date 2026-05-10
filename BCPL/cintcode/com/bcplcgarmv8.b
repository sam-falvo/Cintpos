
SECTION "CGRPI"

// RPi code generator for 32-bit BCPL June 2016
 
// Author:  D J Allerton (d.j.allerton@sheffield.ac.uk)
 
// This code generator is based on the one written for the
// MC68000 by Martin Richards, which was based on the one
// for the PDP-11 at Cambridge.
 

GET "libhdr"
GET "bcplfecg"

MANIFEST 
{
    HARDWARE_DIVIDE = TRUE  // RPi model 3 has hardware division

    codespacesize = 50000
    staticslistsize = 500
	   
    r0 =  0  /* arithmetic registers r0-r9 */
    r1 =  1
    r2 =  2
    r3 =  3
    r4 =  4
    r5 =  5
    r6 =  6
    r7 =  7
    r8 =  8
    r9 =  9
    rg = 10  /* BCPL global vector */
    rp = 11  /* BCPL stack */
    ip = 12  /* not used */
    sp = 13  /* system stack */
    lr = 14  /* link reg */
    rx = 14  /* temporary reg, no need to remember */
    pc = 15
 
 
    //  CLASS Bits:
    //                 w   m  cr   r  r9  r8  r7  r6  r5  r4  r3  r2  r1  r0
    //         0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
 
    c_r        = #X0400    // Item is a register
    c_cr       = #X0800    // Value is in a register or slaved
    c_m        = #X1000    // alterable memory
    c_w        = #X2000    // constant
    c_regs     = #X03FF
  
    //  Items in Simulated Stack or Registers
 
    k_sh       = #10
    k_lv       = #20
  
    k_loc      = #01
    k_locsh    = k_loc + k_sh
    k_lvloc    = k_loc + k_lv
    k_lvlocsh  = k_loc + k_sh + k_lv
 
    k_glob     = #02
    k_globsh   = k_glob + k_sh
    k_lvglob   = k_glob + k_lv
    k_lvglobsh = k_glob + k_sh + k_lv
 
    k_lab      = #03
    k_labsh    = k_lab + k_sh
    k_lvlab    = k_lab + k_lv
    k_lvlabsh  = k_lab + k_sh + k_lv
 
    k_notsh    = #77 - k_sh

    k_fnlab    = #30 
    k_numb     = #40
    k_reg      = #50
 
    i_AND =    0    /* ARM instructions */
    i_EOR =    1
    i_SUB =    2
    i_RSB =    3
    i_ADD =    4
    i_TST =    8
    i_TEQ =    9
    i_CMP =    10
    i_CMN =    11
    i_ORR =    12
    i_MOV =    13
    i_BIC =    14
    i_MVN =    15

    i_MUL =    100  /* pseudo instructions */
    i_DIV =    101
    i_REM =    102
    i_NEG =    103
    i_ABS =    104
    i_NOT =    105
    i_LSHIFT = 106
    i_RSHIFT = 107
    i_RV =     108

    b_EQ =     0    /* condition codes */
    b_NE =     1
    b_GE =     10
    b_LS =     11
    b_GR =     12
    b_LE =     13
    b_BR =     14
    b_NONE =   99   /* pseudo branch - no code generated */

    allregsused = #x3FF
}

GLOBAL 
{
  cgsects : cgg
  initcg
  closecg

  // Global procedures.
  rdl
  rdgn
  newlab
  checklab
  cgerror

  initstack
  stack
  store
  scan
  cgpendingop

  cgdyadic
  cgmonadic
  cgreturn

  movetoanyr
  movetoanyrsh
  movetor

  cgsave

  nextfree
  forgetall
  forgetvar
  forgetallvars
  forgetr
  initslave

  storet
  loadt
  lose1
  remem
  swapargs
  cgstind
  storein

  cgrv
  cgglobal
  cgentry
  cgapply
  cgjump
  cgcmp

  slave

  cgswitch
  switchb
  cgstring
  setlab
  cgstatics
  getblk
  freeblk
  genbranch

  initdatalists

  chkstatics

  genw
  checkspace
  pack4b
  codew
  putw

  jmpfn
  compjfn

  outputsection
  dboutput
  wrkn
  class
  genbranchandlink
  operand2

  GenLoadConstant
  GenAddConstant
  GenCompare
  getblk4
  cgbyteap
  chkhwm
	
  // Global variables
  arg1
  arg2

  ssp

  tempt
  tempv
  stv
  stvp
  stvpstart
    
  dp
  freelist

  incode
  labv

  maxgn
  maxlab
  maxssp

  op
  labnumber
  pendingop
  procdepth

  progsize

  llist
  nlist
  nliste
  clist
  slist
    
  regsinuse
  regscontaining
  choosereg
  regswithinfo
  moveinfo
  datalabel
  blkupb
	
  staticslist
  staticslabels
  nstatics
  ocodename
  instrname
  condname
}


MANIFEST
{
    swapped=TRUE
    notswapped=FALSE

    // Global routine numbers.
    gn_stop       = 2
    gn_div        = -1
    gn_switch     = -2
	gn_staticaddr = -3
}

LET codegenerate(workspace, workspacesize) BE
{   initcg()

    IF T64 DO
    { 
        cgerror("Target word length of 64 bits is not yet available*n")
        errcount := errcount+1
        longjump(fin_p, fin_l)
    }

    
    IF workspacesize<2000 DO 
    { 
        cgerror("Too little workspace")
        errcount := errcount+1
        longjump(fin_p, fin_l)
    }

    progsize := 0
    op := rdn()
    cgsects(workspace, workspacesize)
    writef("Code size = %n bytes*n", progsize)
}

AND initcg() BE
{ //sawritef("CG Arm 32-bit (26 June 2016)*n")

  stv := getvec(codespacesize)
  IF stv=0 DO
  { cgerror("initcg: unable to allocate code workspace (%n)", codespacesize)
    stop(0)
  }
  stvp := 0
  staticslist := getvec(staticslistsize)
  IF staticslist=0 DO
  { cgerror("initcg: unable to allocate statics workspace (%n)",
             staticslistsize)
    stop(0)
  }
  staticslabels := getvec(staticslistsize)
  IF staticslabels=0 DO
  { cgerror("initcg: unable to allocate statics labels (%n)",
             staticslistsize)
    stop(0)
  }
}

AND closecg() BE
{
    outputsection()
    freevec(stv)
    freevec(staticslist)
    freevec(staticslabels)
}

AND cgsects(workvec, vecsize) BE UNTIL op=0 DO
{ 
    LET p = workvec

    stvpstart := stvp  // remember stvp at start of section
    tempv := p
    p := p+90
    tempt := p
    labv := p
    dp := workvec+vecsize
    labnumber := (dp-p)/10+10
    p := p+labnumber
    FOR lp = labv TO p-1 DO 
        !lp := -1
    slave := p
    p := p + 16  /* regs r0 to r9 */
    incode := FALSE
    maxgn := 0
    nstatics := 0
    maxlab := 0
    maxssp := 0
    procdepth := 0
    blkupb := 3  // some blks have 4 entries
    
    initstack(3)
    initdatalists()

    datalabel := 0
    initslave()

    IF debug>=6 DO
      writef("%i5:     %t8 0x%x8*n",
               stvp, ".word", 0)
    codew(0)  // reserve for size of section
    
    IF op=s_section DO
    {  MANIFEST 
       { upb=11  // Max length of entry name 
       }
      
       LET n = rdn()
       LET v = VEC upb/bytesperword
       v%0 := upb
       // Pack up to 11 character of the name into v including
       // the first and last five.
       TEST n<=11
       THEN { FOR i = 1 TO n DO 
                v%i := rdn()
              FOR i = n+1 TO 11 DO 
                v%i := '*s' // Pad with spaces
            }
       ELSE { FOR i = 1 TO 5 DO 
                v%i := rdn()
              FOR i = 6 TO n-6 DO 
                rdn() // Ignore the middle characters
              FOR i = 6 TO 11 DO 
                v%i := rdn()
              IF n>11 DO 
                v%6 := '*''
            }

       IF naming DO 
       { LET word0 = sectword
         LET word1 = pack4b(v%0, v%1, v% 2, v% 3)
         LET word2 = pack4b(v%4, v%5, v% 6, v% 7)
         LET word3 = pack4b(v%8, v%9, v%10, v%11)

         IF debug>=6 DO writef("%i5:     %t8 0x%x8*n", stvp, ".word", word0)
         codew(word0)
         IF debug>=6 DO writef("%i5:     %t8 0x%x8*n", stvp, ".word", word1)
         codew(word1)
         IF debug>=6 DO writef("%i5:     %t8 0x%x8*n", stvp, ".word", word2)
         codew(word2)
         IF debug>=6 DO writef("%i5:     %t8 0x%x8*n", stvp, ".word", word3)
         codew(word3)
       }
       op := rdn()
    }

    scan()
    op := rdn()
    putw(stvpstart, (stvp-stvpstart)/4)  // size of module in words
    progsize := stvp
}

AND gen_move_rq(op, rd, n) BE
{ // mov rd,#q (op is MOV or MVN) q is operand2 imm
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,#%n*n", stvp, instrname(op), rd, n)
  //abort(1000)
  codew(14 << 28 | 1 << 25 | op << 21 | rd << 12 | n)
}

AND gen_cond_move_rq(op, cond, rd, n) BE
{ // mov cond rd,#q (op is MOV or MVN) q is operand2 imm 
  IF debug>=6 DO
    writef("%i5:     %t6%s r%n,#%n*n",
            stvp, instrname(op), condname(cond), rd, n)
  codew(cond << 28 | 1 << 25 | op << 21 | rd << 12 | n)
}

AND gen_move_rn(op, rd, n, sh) BE
{ // mov rd,#n rot #sh (op is MOV or MVN) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,#%n rot #%n*n",
            stvp, instrname(op), rd, n, sh)
  codew(14 << 28 | 1 << 25 | op << 21 | rd << 12 | sh << 8 | n)
}

AND gen_move_rr(op, rd, rm) BE
{ // mov rd,rm (op is MOV or MVN) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n*n",
            stvp, instrname(op), rd, rm)
  codew(14 << 28 | op << 21 | rd << 12 | rm)
}

AND gen_move_rrshl(op, rd, rm, sh) BE
{ // mov rd,rm lsl #sh (op is MOV or MVN) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n lsl #%n*n",
            stvp, instrname(op), rd, rm, sh)
  codew(14 << 28 | op << 21 | rd << 12 | sh << 7 | rm)
}

AND gen_move_rrshr(op, rd, rm, sh) BE
{ // mov rd,rm lsr #sh (op is MOV or MVN) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n lsr #%n*n",
            stvp, instrname(op), rd, rm, sh)
  codew(14 << 28 | op << 21 | rd << 12 | sh << 7 | 2 << 4 | rm)
}

AND gen_move_rrrshl(op, rd, rs, rm) BE
{ // mov rd,rs, lsl rm (op is MOV or MVN) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n lsl r%n*n",
             stvp, instrname(op), rd, rm, rm)
  codew(14 << 28 | op << 21 | rd << 12 | rs << 8 | 1 << 4 | rm)
}

AND gen_move_rrrshr(op, rd, rs, rm) BE
{ // mov rd,rs, lsr rm (op is MOV or MVN) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n lsr r%n*n",
             stvp, instrname(op), rd, rm, rm)
  codew(14 << 28 | op << 21 | rd << 12 | rs << 8 | 3 << 4 | rm)
}

AND gen_arith_rrq(op, rd, rn, n) BE
{ // add rd,rn,#q (op is ADD,SUB,RSB,AND,EOR or ORR) q is operand2 imm 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n,#%n*n",
             stvp, instrname(op), rd, rn, n)
  codew(14 << 28 | 1 << 25 | op << 21 | rn << 16 | rd << 12 | n)
}

AND gen_arith_rrn(op, rd, rn, n, sh) BE
{ // add rd,rn,#n rot #sh (op is ADD,SUB,RSB,AND,EOR or ORR) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n,#%n*n",
             stvp, instrname(op), rd, rn, n) //#########
  codew(14 << 28 | 1 << 25 | op << 21 | rn << 16 | rd << 12 | sh << 8 | n)
}

AND gen_cond_arith_rrn(op, cond, rd, rn, n, sh) BE
{ // add cond rd,rn,#n rot #sh (op is ADD,SUB,RSB,AND,EOR or ORR) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n,#%n*n",
             stvp, instrname(op), rd, rn, n) //#########
  codew(cond << 28 | 1 << 25 | op << 21 | rn << 16 | rd << 12 | sh << 8 | n)
}

AND gen_arith_rrr(op, rd, rn, rm) BE
{ // add rd,rn,rm (op is ADD,SUB,RSB,AND,EOR or ORR) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n,r%n*n", stvp, instrname(op), rd, rn, rm)
  codew(14 << 28 | op << 21 | rn << 16 | rd << 12 | rm)
}

AND gen_arith_rrrshl(op, rd, rn, rm, sh) BE
{ // add rd,rn,rm lsl #sh (op is ADD,SUB,RSB,AND,EOR or ORR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n,r%n lsl #%n*n",
             stvp, instrname(op), rd, rn, rm, sh)
  codew(14 << 28 | op << 21 | rn << 16 | rd << 12 | sh << 7 | rm)
}

AND gen_arith_rrrshr(op, rd, rn, rm, sh) BE
{ // add rd,rn,rm lsr #sh (op is ADD,SUB,RSB,AND,EOR or ORR) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n,r%n lsr #%n*n",
             stvp, instrname(op), rd, rn, rm, sh)
  codew(14 << 28 | op << 21 | rn << 16 | rd << 12 | sh << 7 | 2 << 4 | rm)
}

AND gen_cmp_rq(op, rn, n) BE
{ // cmp rn,#q (op is CMP or CMN) q is operand2 imm 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,#%n*n",
             stvp, instrname(op), rn, n)
  codew(14 << 28 | 1 << 25 | op << 21 | 1 << 20 | rn << 16 | n)
}

AND gen_cmp_rn(op, rn, n, sh) BE
{ // cmp rn,#n rot #sh (op is CMP or CMN) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,#%n rot #%n*n",
             stvp, instrname(op), rn, n, sh)
  codew(14 << 28 | 1 << 25 | op << 21 | 1 << 20 | rn << 16 | sh << 8 | n)
}

AND gen_cmp_rr(op, rn, rm) BE
{ // cmp rn,rm (op is CMP or CMN) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n*n",
             stvp, instrname(op), rn, rm)
  codew(14 << 28 | op << 21 | 1 << 20 | rn << 16 | rm)
}

AND gen_b(cond, offset, lab) BE
{ // brcond offset 
  IF debug>=6 DO
    writef("%i5:     B%s      L%n*n",
             stvp, condname(cond), lab)
  codew(cond << 28 | #xA << 24 | (offset & #xffffff))
}

AND gen_bl(offset, lab) BE
{ // bl offset (op is BL) 
  IF debug>=6 DO
    writef("%i5:     %t8 L%n*n",
           stvp, "BL", lab)
  codew(14 << 28   | #xB << 24 | (offset & #xffffff))
}

AND gen_blx(rm) BE
{ // b rm (op is BLX) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n*n",
           stvp, "BLX", rm)
  codew(14 << 28   | #x12FFF30 | rm)
}

AND gen_ldr_rrn(rd, rn, n) BE
{ // ldr rd,[rn,#n] (op is LDR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,#%n]*n",
            stvp, "LDR", rd, rn, n)
  TEST n>=0
  THEN
       codew(14 << 28 | #x59 << 20 | rn << 16 | rd << 12 |  n & #xfff) // U=1
  ELSE
       codew(14 << 28 | #x51 << 20 | rn << 16 | rd << 12 | -n & #xfff) // U=0
}

AND gen_ldr_rrr(rd, rn, rm) BE
{ // ldr rd,[rn,rm] (op is LDR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n] ###############*n",
           stvp, "LDR", rd, rn, rm)
  codew(14 << 28 | #x79 << 20 | rn << 16 | rd << 12 | rm)
}

AND gen_ldr_rrrshl(rd, rn, rm, sh) BE
{ // ldr rd,[rn,rm lsl #sh] (op is LDR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n lsl #%n] ###############*n",
           stvp, "LDR", rd, rn, rm, sh)
  codew(14 << 28 | #x79 << 20 | rn << 16 | rd << 12 | sh << 7 | rm)
}

AND gen_ldr_rrrshr(rd, rn, rm, sh) BE
{ // ldr rd,[rd,rn lsr #sh] (op is LDR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n lsr #%n] ###############*n",
           stvp, "LDR", rd, rn, rm, sh)
  codew(14 << 28 | #x79 << 20 | rn << 16 | rd << 12 | sh << 7 | 2 << 4 | rm)
}

AND gen_ldrb_rrn(rd, rn, n) BE
{ // ldrb rd,[rn,#n] (op is LDRB)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,#%n] ###############*n",
           stvp, "LDRB", rd, rn, n)
  TEST n>=0
  THEN
       codew(14 << 28 | #x5D << 20 | rn << 16 | rd << 12 | n & #xfff)
  ELSE
       codew(14 << 28 | #x55 << 20 | rn << 16 | rd << 12 | -n & #xfff)
}

AND gen_ldrb_rrr(rd, rn, rm) BE
{ // ldrb rd,[rn,rm] (op is LDRB)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n] ###############*n",
           stvp, "LDRB", rd, rn, rm)
  codew(14 << 28 | #x7D << 20 | rn << 16 | rd << 12 | rm)
}

AND gen_ldrb_rrrshl(rd, rn, rm, sh) BE
{ // ldrb rd,[rn,rm lsl #sh] (op is LDRB)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n lsl #%n] ###############*n",
           stvp, "LDRB", rd, rn, rm, sh)
  codew(14 << 28 | #x7D << 20 | rn << 16 | rd << 12 | sh << 7 | rm)
}

AND gen_ldrb_rrrshr(rd, rn, rm, sh) BE
{ // ldrb rd,[rn,rm lsr #sh] (op is LDRB)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n[r%n,r%n lsr #%n]*n",
             stvp, "LDRB", rd, rn, rm, sh)
  codew(14 << 28 | #x7D << 20 | rn << 16 | rd << 12 | sh << 7 | 2 << 4 | rm)
}

AND gen_str_rrn(rd, rn, n) BE
{ // str rd,[rn,#n] (op is STR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,#%n]*n",
           stvp, "STR", rd, rn, n)
  TEST n>=0
  THEN
      codew(14 << 28 | #x58 << 20 | rn << 16 | rd << 12 | (n & #xfff))
  ELSE
      codew(14 << 28 | #x50 << 20 | rn << 16 | rd << 12 | (-n & #xfff))
}

AND gen_str_rrr(rd, rn, rm) BE
{ // str rd,[rn,rm] (op is STR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n]*n",
             stvp, "STR", rd, rn, rm)
  codew(14 << 28 | #x78 << 20 | rn << 16 | rd << 12 | rm)
}

AND gen_str_rrrshl(rd, rn, rm, sh) BE
{ // str rd,[rn,rm lsl #sh] (op is STR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n lsl #%n]*n",
           stvp, "STR", rd, rn, rm, sh)
  codew(14 << 28 | #x78 << 20 | rn << 16 | rd << 12 | sh << 7 | rm)
}

AND gen_str_rrrshr(rd, rn, rm, sh) BE
{ // str rd,[rn,rm lsr #sh] (op is STR)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n lsr #%n]*n",
             stvp, "STR", rd, rn, rm, sh)
  codew(14 << 28 | #x78 << 20 | rn << 16 | rd << 12 | sh << 7 | 2 << 4 | rm)
}

AND gen_strb_rrn(rd, rn, n) BE
{ // strb rd,[rn,#n] (op is STRB)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,#%n] ###############*n",
           stvp,"STRB", rd, rn, n)
  TEST n>=0
  THEN
      codew(14 << 28 | #x5C << 20 | rn << 16 | rd << 12 |  n & #xfff)
  ELSE
      codew(14 << 28 | #x54 << 20 | rn << 16 | rd << 12 | -n & #xfff)
}

AND gen_strb_rrr(rd, rn, rm) BE
{ // strb rd,[rn,rm] (op is STRB) 
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n] ###############*n",
           stvp, "STRB", rd, rn, rm)
  codew(14 << 28 | #x7C << 20 | rn << 16 | rd << 12 | rm)
}

AND gen_strb_rrrshl(rd, rn, rm, sh) BE
{ // strb rd,[rn,rm lsl #sh] (op is STRB)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n lsl #%n] ###############*n",
           stvp, "STRB", rd, rn, rm, sh)
  codew(14 << 28 | #x7C << 20 | rn << 16 | rd << 12 | sh << 7 | rm)
}

AND gen_strb_rrrshr(rd, rn, rm, sh) BE
{ // strb rd,[rn,rm lsr #sh] (op is STRB)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,[r%n,r%n lsl #%n] ###############*n",
           stvp, "STRB", rd, rn, rm, sh)
  codew(14 << 28 | #x7C << 20 | rn << 16 | rd << 12 | sh << 7 | 2 << 4 | rm)
}

AND gen_mul(rd, rm, rs) BE
{ // mul rd,rs,rm (MUL instruction)
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n,r%n*n",
           stvp, "MUL", rd, rm, rs)
  codew(14 << 28 | rd << 16 | rs << 8 | 9 << 4 | rm)
}

AND gen_nop() BE
{ // NOP
  IF debug>=6 DO
    writef("%i5:     %t8 r%n,r%n    @ NOP*n", stvp, instrname(i_MOV), 0, 0)
  gen_move_rr(i_MOV, r0, r0)
}

/* rdn() is provided by the compiler */

// Read in an OCODE label.
AND rdl() = VALOF
{ 
    LET l = rdn()
    
    IF maxlab<l DO 
    { 
        maxlab := l
        checklab() 
    }
    RESULTIS l
}

// Read in a global number.
AND rdgn() = VALOF
{ 
    LET g = rdn()
   
    IF maxgn<g DO 
        maxgn := g
    RESULTIS g
}


// Generate next label number.
AND newlab() = VALOF
{ 
    labnumber := labnumber-1
    checklab()
    RESULTIS labnumber
}


AND checklab() BE 
    IF maxlab>=labnumber DO
    { 
        cgerror("Too many labels - increase workspace")
        errcount := errcount+1
        longjump(fin_p, fin_l)
    }


AND cgerror(mes, a) BE
{ 
    writes("*n CG Error: ")
    writef(mes, a)
    newline()
    errcount := errcount+1
    IF errcount>errmax DO 
    { 
        writes("Too many errors*n")
        longjump(fin_p, fin_l)
    }
}


// Initialize the simulated stack (SS).
LET initstack(n) BE
{ 
    arg2, arg1, ssp := tempv, tempv+3, n
    pendingop := s_none
    h1!arg2, h2!arg2, h3!arg2 := k_loc, ssp-2, ssp-2
    h1!arg1, h2!arg1, h3!arg1 := k_loc, ssp-1, ssp-1
    IF maxssp<ssp DO 
        maxssp := ssp
}


// Move simulated stack (SS) pointer to N.
AND stack(n) BE
{ 
    IF maxssp<n DO 
        maxssp := n
    IF n>=ssp+4 DO 
    { 
        store(0, ssp-1)
        initstack(n)
        RETURN
    }

    WHILE n>ssp DO 
        loadt(k_loc, ssp)

    UNTIL n=ssp DO
    { 
        IF arg2=tempv DO
        { 
            TEST n=ssp-1
            THEN 
            { 
                ssp := n
                h1!arg1, h2!arg1, h3!arg1 := h1!arg2, h2!arg2, ssp-1
                h1!arg2, h2!arg2, h3!arg2 := k_loc, ssp-2, ssp-2
            }
            ELSE 
                initstack(n)
            RETURN
        }

        arg1, arg2, ssp := arg1-3, arg2-3, ssp-1
    }
}


// store all SS items from A to B in their true
// locations on the stack
AND store(a, b) BE
{
    FOR p = tempv TO arg1 BY 3 DO
    {
        LET s = h3!p
        
        IF s>b BREAK
        IF s>=a & h1!p>=k_reg DO 
            storet(p)
    }
    FOR p = tempv TO arg1 BY 3 DO
    { 
        LET s = h3!p
        
        IF s>b RETURN
        IF s>=a DO 
           storet(p)
    }
}


AND scan() BE
{ IF debug>=7 DO
  { writef("op=%t5", opname(op))
    UNLESS pendingop=s_none DO
      writef(" pdnop=%s", opname(pendingop))
    newline()
    dboutput()
  }

  SWITCHON op INTO
  { 
    DEFAULT:
      cgerror("Bad OCODE op %n", op)
      ENDCASE

    CASE 0:
      RETURN
      
    CASE s_needs:
    { 
      LET n = rdn()  // Ignore NEEDS directives.
      FOR i = 1 TO n DO 
        rdn()
      ENDCASE
    }

    CASE s_lp:   
      loadt(k_loc, rdn())  
      ENDCASE
    CASE s_lg:   
      loadt(k_glob, rdgn()) 
        ENDCASE
    CASE s_ll:   
      loadt(k_lab, rdl())  
      ENDCASE
    CASE s_lf:
      loadt(k_fnlab, rdl())  
      ENDCASE
    CASE s_ln:   
      loadt(k_numb, rdn())
      //abort(1001)
      ENDCASE
    CASE s_lstr:
      cgstring(rdn())
      ENDCASE

    CASE s_true: 
      loadt(k_numb, -1)
      ENDCASE
    CASE s_false:
      loadt(k_numb,  0)
      ENDCASE

    CASE s_llp:  
      loadt(k_lvloc,  rdn())
      ENDCASE
    CASE s_llg:  
      loadt(k_lvglob, rdgn())
      ENDCASE
    CASE s_lll:  
      loadt(k_lvlab,  rdl())
      ENDCASE

    CASE s_sp:   
      storein(k_loc,  rdn())
      ENDCASE
    CASE s_sg:   
      storein(k_glob, rdgn())
      ENDCASE
    CASE s_sl:   
      storein(k_lab,  rdl())
      ENDCASE

    CASE s_stind:
      cgstind()
      ENDCASE

    CASE s_rv:   
      cgrv()
      ENDCASE

    CASE s_mul:CASE s_div:CASE s_mod:
    CASE s_add:CASE s_sub:
    CASE s_eq: CASE s_ne:
    CASE s_ls:CASE s_gr:CASE s_le:CASE s_ge:
    CASE s_lshift:CASE s_rshift:
    CASE s_logand:CASE s_logor:CASE s_eqv:CASE s_xor:
    CASE s_not:CASE s_neg:CASE s_abs:
      cgpendingop()
      pendingop := op
      ENDCASE

    CASE s_jt:   
      cgjump(TRUE, rdl())
      ENDCASE

    CASE s_jf:   
      cgjump(FALSE, rdl())
      ENDCASE

    CASE s_goto: 
      cgpendingop()
      store(0, ssp-2)
      TEST h1!arg1=k_fnlab
      THEN { genbranch(b_BR, h2!arg1)
           }
      ELSE { LET r = movetoanyr(arg1)
             gen_move_rr(i_MOV, pc, r)  // mov pc,r
           }
      stack(ssp-1)
      incode := FALSE
      // this is a good place to deal with
      // outstanding forward references to statics
      chkstatics()
      ENDCASE

    CASE s_lab:
      cgpendingop()
      store(0, ssp-1)
      setlab(rdl())
      forgetall()
      incode := procdepth>0
      ENDCASE

    CASE s_query:
      loadt(k_loc, ssp)
      ENDCASE

    CASE s_stack:
      cgpendingop()
      stack(rdn())
      ENDCASE

    CASE s_store:
      cgpendingop(); 
      store(0, ssp-1)
      ENDCASE

    CASE s_entry:
    { LET l = rdl()
      LET n = rdn()
      cgentry(l, n)
      procdepth := procdepth + 1
      ENDCASE
    }

    CASE s_save:
      cgsave(rdn()) 
      ENDCASE

    CASE s_fnap:
    CASE s_rtap: 
      cgapply(op, rdn())
      ENDCASE

    CASE s_rtrn: 
      //cgpendingop()
      //IF debug>=6 DO writef("%i5:     LDM rp,{rp,pc}*n", stvp)
      //codew(#xE89B8800)   /* LDM rp,{rp,pc}  no inc */
      //incode := FALSE
      //chkstatics()
      //ENDCASE
                   
    CASE s_fnrn:
      cgreturn(op)
      //cgpendingop()
      //movetor(arg1, r0)
      //codew(#xE89B8800)   /* LDM rp,{rp,pc}  no inc */
      //stack(ssp-1)
      //incode := FALSE
      //chkstatics()
      ENDCASE

    CASE s_endproc:
      procdepth := procdepth - 1
      ENDCASE

    CASE s_res:
    CASE s_jump:
    { LET l = rdl()

      cgpendingop()
      store(0, ssp-2)
      TEST op=s_jump
      THEN { storet(arg1)
           }
      ELSE { movetor(arg1, r0)
             stack(ssp-1) 
           }

      { op := rdn()
        UNLESS op=s_stack BREAK
        stack(rdn())
      } REPEAT

      TEST op=s_lab
      THEN { LET m = rdl()
             UNLESS l=m DO 
             genbranch(b_BR, l)
             setlab(m)
             forgetall()
             incode := procdepth>0
             op := rdn()
           }
      ELSE { genbranch(b_BR, l)
             incode := FALSE
             chkstatics()
           }

      LOOP
    }

    // rstack always occurs immediately after a lab statement
    // at a time when cgpendingop() and store(0, ssp-2) have been called.
    CASE s_rstack: 
      stack(rdn()); 
      loadt(k_reg, r0); 
      ENDCASE

    CASE s_finish:  // Compile code for:  stop(0).
    { LET k = ssp
            
      stack(ssp+3)
      loadt(k_numb, 0)
      loadt(k_numb, 0)
      loadt(k_glob, gn_stop)
      cgapply(s_rtap, k)    // Simulate the call: stop(0, 0)
      ENDCASE
    }

    CASE s_switchon: 
      cgswitch()
      ENDCASE

    CASE s_getbyte:  
    CASE s_putbyte:  
      cgbyteap(op)
      ENDCASE

    CASE s_global:   
      cgglobal(rdn())
      RETURN

    CASE s_datalab:     /* check for a table or a static */
    { LET lab = rdl()
      LET ostatics = nstatics
				 
      op := rdn()

      WHILE op=s_itemn DO
      { nstatics := nstatics + 1
	staticslist!nstatics := rdn()
	staticslabels!nstatics := lab
        op := rdn()
      }
      IF nstatics > (ostatics + 1)  /* must be a TABLE rather than a STATIC */
      { FOR i = ostatics + 1 TO nstatics DO
	{ !nliste := getblk(0, lab, staticslist!i)
	  nliste, lab := !nliste, 0
        }
	nstatics := ostatics
      }
      LOOP
    }
  }

  op := rdn()
} REPEAT


// Compiles code to deal with any pending op.
LET cgpendingop() BE
{ 
    LET pndop = pendingop
    pendingop := s_none

    SWITCHON pndop INTO
    { 
        DEFAULT:      
            cgerror("Bad pendingop %n", pndop)

        CASE s_none:  
            RETURN

        CASE s_abs:   
            cgmonadic(i_ABS)
            RETURN

        CASE s_neg:   
            cgmonadic(i_NEG)
            RETURN

        CASE s_not:   
            cgmonadic(i_NOT)
            RETURN

        CASE s_eq: CASE s_ne:
        CASE s_ls: CASE s_gr:
        CASE s_le: CASE s_ge:
            {
                LET swapped = cgdyadic(i_CMP, TRUE)
                LET r = nextfree()
                LET cond = jmpfn(pndop)
                LET compcond = compjfn(cond)

                IF pndop=s_eq | pndop=s_ne DO
                    swapped := FALSE  // boolean test, so don't swap condition code for EQ or NE
                TEST swapped
                THEN
                {
                    gen_cond_move_rq(i_MVN, compcond, r, 0)  /* if TRUE set r=-1 mvn cond r,#0 */
                    gen_cond_move_rq(i_MOV, cond, r, 0)      /* if FALSE set r=0 mov cond r,#0 */
                }
                ELSE
                {
                    gen_cond_move_rq(i_MVN, cond, r, 0)      /* if TRUE set r=-1 mvn cond r,#0 */
                    gen_cond_move_rq(i_MOV, compcond, r, 0)  /* if FALSE set r=0 mov cond r,#0 */
                }
                forgetr(r)
                lose1(k_reg, r)
            }
            RETURN

        CASE s_sub: 
            cgdyadic(i_SUB, FALSE)
            RETURN
            
        CASE s_add:  
            cgdyadic(i_ADD, TRUE)
            RETURN

        CASE s_mul:
            cgdyadic(i_MUL, TRUE)
            RETURN
            
        CASE s_div:      /* registers r0,r1 returned, registers r2-r9 preserved*/
        CASE s_mod:
            TEST HARDWARE_DIVIDE
			THEN
			    TEST pndop=s_div
				THEN
				{
					LET rn = movetoanyr(arg2)  // numerator
					LET rd = movetoanyr(arg1)  // denominator
					codew(#xE710F010 | rn << 16 | rd << 8 | rn)  // DIVS rn,rn,rm
					forgetr(rn)
					lose1(k_reg, rn)
				}
				ELSE
				{
					LET rn = movetoanyr(arg2)  // numerator
					LET rd = movetoanyr(arg1)  // denominator
                    LET rt = nextfree()
					gen_move_rr(i_MOV, rt, rn)  // mov rt,rn
					codew(#xE710F010 | rn << 16 | rd << 8 | rn)  // DIVS rn,rn,rm
                    gen_mul(rn, rn, rd)
					gen_arith_rrr(i_SUB, rn, rt, rn)
					forgetr(rn)
					lose1(k_reg, rn)
				}
			ELSE
			{
				movetor(arg2, r1)  // arg2/arg1, numerator in r1
				movetor(arg1, r0)  // denominator in r0
				gen_ldr_rrn(rx, rg, gn_div * 4)  // ldr rx,[rg, 4*gn_div]
				gen_blx(rx)
				TEST pndop = s_mod
				THEN
				{
					lose1(k_reg, r1)
				}
				ELSE
				{
					lose1(k_reg, r0)
				}
			}
            RETURN

        CASE s_lshift:
            cgdyadic(i_LSHIFT, FALSE)
            RETURN
        
        CASE s_rshift:
            cgdyadic(i_RSHIFT, FALSE)
            RETURN
            
        CASE s_logand:
            cgdyadic(i_AND, TRUE)
            RETURN;
            
        CASE s_logor:
            cgdyadic(i_ORR, TRUE)
            RETURN;
            
        CASE s_eqv:
        CASE s_xor:
            cgdyadic(i_EOR, TRUE)
            IF pndop = s_eqv DO
            {
                LET r = movetoanyr(arg1)  // find which reg was used and 1's comp it
                gen_move_rr(i_MVN, r, r)  // mvn r,r
            }
            RETURN
    }
}


AND cgdyadic(Op, swapable) = VALOF
{
    LET k1, n1 = ?, ?
    LET k2, n2 = ?, ?
    LET swapped = FALSE
    LET r, s = ?, ?
    LET op2 = ?

    IF swapable & h1!arg2 = k_numb DO
    {
        swapargs()
        swapped := TRUE
    }

    IF Op = i_SUB & h1!arg2 = k_numb DO
    {
        swapargs()
        swapped := TRUE
        Op := i_RSB
    }
        
    k1, n1 := h1!arg1, h2!arg1
    k2, n2 := h1!arg2, h2!arg2
    
    IF k1 = k_numb & k2 = k_numb DO
    {
        LET n = ?
        
        SWITCHON Op INTO
        {
            CASE i_ADD:  
                n := n1 + n2
                ENDCASE
            CASE i_SUB:
                n := n2 - n1
                ENDCASE
            CASE i_RSB: 
                n := n1 - n2
                ENDCASE
            CASE i_MUL:  
                n := n1 * n2
                ENDCASE
            CASE i_DIV:  
                n := n2 / n1
                ENDCASE
            CASE i_REM:  
                n := n2 REM n1
                ENDCASE
            CASE i_ORR:
                n := n1 | n2
                ENDCASE
            CASE i_AND:
                n := n1 & n2
                ENDCASE
            CASE i_EOR:
                n := n1 NEQV n2
                ENDCASE
            CASE i_LSHIFT:
                n := n2 << n1
                ENDCASE
            CASE i_RSHIFT:
                n := n2 >> n1
                ENDCASE
            DEFAULT: 
                cgerror("unknown dyadic Op%n*n", Op)
                ENDCASE
        }
        lose1(k_numb, n)
        RESULTIS swapped
    }

    IF k1 = k_numb DO
    {
        r := movetoanyr(arg2)

        IF Op=i_ADD & n1 < 0 DO
            Op, n1 := i_SUB, -n1
            
        IF Op = i_LSHIFT | Op = i_RSHIFT DO
        {
            TEST n1 ~= 0
            THEN
            {
                TEST Op = i_LSHIFT
                THEN
                    gen_move_rrshl(i_MOV, r, r, n1)
                ELSE    
                    gen_move_rrshr(i_MOV, r, r, n1)
                forgetr(r)
                lose1(k_reg, r)
            }
            ELSE
            {
                stack(ssp-1)
            }
            pendingop := s_none
            RESULTIS swapped
        }
        
        IF Op = i_MUL DO
        {
            LET sh = 1
            
            SWITCHON n1 INTO
            {
                CASE 0:
                    gen_move_rq(i_MOV, r, 0)          // mov r,#0
                    forgetr(r)
                    remem(r, k_numb, 0)
                    ENDCASE
                CASE 1:
                    ENDCASE
                CASE 2:
                    gen_arith_rrr(i_ADD, r, r, r)     // add r,r,r
                    forgetr(r)
                    ENDCASE
                CASE -1:
                    gen_arith_rrq(i_RSB, r, r, 0)     // rsb r,r,#0
                    forgetr(r)
                    ENDCASE
                CASE -2:
                    gen_arith_rrq(i_RSB, r, r, 0)     // rsb r,r,#0
                    gen_arith_rrr(i_ADD, r, r, r)     // add r,r,r
                    forgetr(r)
                    ENDCASE
                DEFAULT:
                    sh := powerof2(n1)
                    IF sh > 0 DO
                    {
                        gen_move_rrshl(i_MOV, r, r, sh)      // mov r,r,lsl #sh 
                        IF (n1 < 0) DO
                        {
                            gen_arith_rrq(i_RSB, r, r, 1)    // rsb r,r,#1
                        }
                        forgetr(r)
                    }
            }
            IF (sh > 0) DO  // 0, 1, -1, 2, -2 or power of 2
            {
                lose1(k_reg, r)
                pendingop := s_none
                RESULTIS swapped
            }
        
            s := movetoanyr(arg1)
            gen_mul(r, r, s)   // mul r,r,s
            forgetr(r)
            lose1(k_reg, r)
            pendingop := s_none
            RESULTIS swapped
        }
        
        IF Op = i_CMP & n1 < 0 DO
        {
            Op := i_CMN
            n1 := -n1
            h2!arg1 := n1
        }
        
        op2 := operand2(n1)
        TEST op2>= 0
        THEN
        {
            TEST Op = i_CMP | Op = i_CMN
            THEN
                gen_cmp_rq(Op, r, op2)   // cmp r,#n1
            ELSE
            {
                gen_arith_rrq(Op, r, r, op2)    // Op r,r,#n1
                forgetr(r)
                lose1(k_reg, r)
            }
            RESULTIS swapped
        }
        ELSE
        {
            s := movetoanyr(arg1)
            TEST Op = i_CMP | Op = i_CMN
            THEN
            {
                gen_cmp_rr(Op, r, s)  // cmp r,t
            }
            ELSE
            {
                gen_arith_rrr(Op, r, r, s)   // Op r,r,s
                forgetr(r)
                lose1(k_reg, r)
            }
            RESULTIS swapped
        }
    }
    
    /* at this point, neither argument is a constant so use regs */

    r := movetoanyr(arg2)
    s := movetoanyr(arg1)

    SWITCHON Op INTO
    {
        CASE i_CMP:
            gen_cmp_rr(Op, r, s)   // cmp r,s
            ENDCASE
        CASE i_LSHIFT:
            gen_move_rrrshl(i_MOV, r, s, r)  // mov r,r,lsl s
            ENDCASE
        CASE i_RSHIFT:
            gen_move_rrrshr(i_MOV, r, s, r)  // mov r,r,lsr s
            ENDCASE
        CASE i_MUL:
            gen_mul(r, r, s)   // mul r,r,s
            ENDCASE
        DEFAULT:
            gen_arith_rrr(Op, r, r, s)   // Op r,r,s
            ENDCASE
    }
    
    UNLESS Op=i_CMP DO
    {
        forgetr(r)
        lose1(k_reg, r)
    }
    RESULTIS swapped
}

AND cgmonadic(Op) BE
{
    LET k, n = h1!arg1, h2!arg1
    LET r = ?
    
    IF k=k_numb DO
    {
        SWITCHON Op INTO
        {
            CASE i_NEG:  
                n := -n
                ENDCASE
            CASE i_ABS:  
                n := ABS n
                ENDCASE
            CASE i_NOT:  
                n := ~n
                ENDCASE
            DEFAULT: 
                cgerror("Unknown monadic constant Op (%n)*n", Op)
                ENDCASE
        }
        h2!arg1 := n
        RETURN
    }
    
    r := movetoanyr(arg1)
    SWITCHON Op INTO
    {
        CASE i_NEG:
            gen_arith_rrq(i_RSB, r, r, 0)   // neg r,r,#0
            ENDCASE
        CASE i_NOT:
            gen_move_rr(i_MVN, r, r)        // mvn r,r
            ENDCASE
        CASE i_ABS:
            gen_cmp_rn(i_CMP, r, 0, 0)                   // cmp r,#0
            gen_cond_arith_rrn(i_RSB, b_LS, r, r, 0, 0)  // rsb LS r,r,#0
            ENDCASE
        default: 
            cgerror("Unknown monadic op (%n)*n", Op)
            ENDCASE
    }
    forgetr(r)
}


AND movetoanyrsh(a) = VALOF
{
    LET r = -1
 
    SWITCHON h1!a INTO
    {
        CASE k_loc:
        CASE k_glob:
        CASE k_lab:
        CASE k_lvloc:
        CASE k_lvglob:
        CASE k_lvlab: 
            h1!a := h1!a + k_sh
            ENDCASE
 
        CASE k_numb:  
            h2!a := h2!a * 4
            ENDCASE
 
        DEFAULT:
            r := movetoanyr(a)
            gen_move_rrshl(i_MOV, r, r, 2)  // mov r,r lsl 2
            forgetr(r)
            ENDCASE
    }
 
    IF r<0 DO 
        r := movetoanyr(a)
    RESULTIS r
}
 

AND movetoanyr(a) = VALOF
{
    LET usedregs = regsinuse()
    LET k, n = h1!a, h2!a
    LET poss = ?
    
    IF k=k_reg DO  /* already in a register? */
    {
        RESULTIS n
    }

    // slaved registers that are free
    poss := class(a) & c_regs & NOT usedregs
    UNLESS poss=0 DO
        RESULTIS movetor(a, choosereg(poss))
    
    // suitable regs with no info that are free
    poss := c_regs & NOT (usedregs | regswithinfo())
    UNLESS poss=0 DO
        RESULTIS movetor(a, choosereg(poss))
        
    // suitable registers that are free
    poss := c_regs & NOT usedregs
    UNLESS poss=0 DO
        RESULTIS movetor(a, choosereg(poss))

    /* all regs in use - so free the oldest */
    FOR t=tempv TO arg1 BY 3 DO
    {
        IF regusedby(t) >= 0
        {
            storet(t)
            BREAK
        }
    }
    // try again
} REPEAT


AND movetor(a, r) = VALOF
{
    LET k, n = h1!a, h2!a
    LET cl = ?
    LET op2 = ?
    LET soffset = ?
	LET rn = ?
    LET ra = ?
		
    //check if a is already in register r
    IF k=k_reg & n=r DO
    {
        RESULTIS r
    }
    
    // free register R if necessary
    UNLESS regusedby(a)=r DO
    {
        freereg(r)
        k, n := h1!a, h2!a
    }
    
    cl := class(a)
    
	chkhwm()
	
    IF cl=0 SWITCHON k INTO
    {
        CASE k_lvloc:
        CASE k_lvlocsh:
            op2 := operand2(n*4)  // >=0 if 12 bit imm
            TEST n=0
            THEN
                gen_move_rr(i_MOV, r, rp)               // mov r,rp
            ELSE
                TEST op2 >= 0
                THEN
                    gen_arith_rrq(i_ADD, r, rp, op2)    // add r,rp,#n*4
                ELSE
                {
                    GenLoadConstant(rx, n*4)            // ldr rx,#n*4
                    gen_arith_rrr(i_ADD, r, rp, rx)     // add r,rp,rx 
                }
                
        shret:
                IF (k&k_sh)=0 DO
                    gen_move_rrshr(i_MOV, r, r, 2, 0)   // mov r,r,lsr #2
                GOTO ret
                
        CASE k_lvglob:
        CASE k_lvglobsh:
            op2 := operand2(n*4)  // >=0 if 12 bit imm
            TEST n=0
            THEN
                gen_move_rr(i_MOV, r, rg)               // mov r,rg
            ELSE
                TEST op2 >= 0
                THEN
                    gen_arith_rrq(i_ADD, r, rg, op2)    // add r,rg,#n*4
                ELSE
                {
                    GenLoadConstant(rx, n*4)            // ldr rx,#n*4
                    gen_arith_rrr(i_ADD, r, rg, rx)     // add r,rg,rx 
                }
            GOTO shret
        
        CASE k_fnlab:
            k := k_lvlabsh  // falls into CASE k_lvlabsh

        CASE k_lvlab:
        CASE k_lvlabsh:
		    soffset := staticoffset(n)
		    TEST soffset >= 0   // valid static label?
			THEN
			{
				ra := NOT (regswithinfo() | regsinuse())  // available regs
				IF r \= r0
				{
					FOR i=r1 TO r9 DO  // find a spare reg, but not r or r0
						UNLESS i=r DO
							UNLESS (ra & (1 << i))=0
							{
								rn := i
								gen_move_rr(i_MOV, rn, r0)
								BREAK
							}
				}
				GenLoadConstant(r0, soffset)
				gen_ldr_rrn(rx, rg, gn_staticaddr * 4)
				gen_blx(rx)
				IF r \= r0
				{
    				gen_move_rr(i_MOV, r, r0)
					gen_move_rr(i_MOV, r0, rn)  // put back r0
				}
			}
			ELSE
			{
				TEST labv!n >= 0
				THEN
					GenAddConstant(r, pc, labv!n - stvp - 12, TRUE)  // constant to be added to the pc
				ELSE
				{
					LET l = newlab()  // make a readonly static

					!nliste := getblk(0, l, 0)   // add a static Ll to hold offset to Ln
					nliste := !nliste
    
					slist := getblk4(slist, stvp, l, n)
					gen_ldr_rrn(rx, pc, 0)            // ldr rx,[pc,#0]   12-bit offset to be filled in later
					gen_arith_rrr(i_ADD, r, pc, rx)   // add r,pc,rx
				}
			}
            GOTO shret

        CASE k_locsh:
        CASE k_globsh:
        CASE k_labsh:
            h1!a := h1!a - k_sh
            movetor(a, r)
            gen_move_rrshl(i_MOV, r, r, 2)   // mov r,r lsl #2
            GOTO ret
        
        DEFAULT:
            cgerror("unknown type k=%n in movetor", k)
    }
    
    UNLESS (cl & c_cr) = 0 DO  // value already in a register
    {
        LET s = choosereg(cl & c_regs)
        IF (cl >> r  & 1) = 0 DO
        {
            gen_move_rr(i_MOV, r, s)
            moveinfo(s, r)
        }
        GOTO ret
    }
    
    SWITCHON k INTO
    {
        CASE k_numb:
            op2 := operand2(n)  // >=0 if 12 bit imm
            TEST op2 >= 0
            THEN
                gen_move_rq(i_MOV, r, op2)
            ELSE
            {
                op2 := operand2(~n)
                TEST op2 >= 0
                THEN
                    gen_move_rq(i_MVN, r, op2)
                ELSE
                    GenLoadConstant(r, n)
            }
            GOTO ret
        
        CASE k_lab:
		    ra := NOT (regswithinfo() | regsinuse())  // available regs
		    IF r \= r0
			{
			    FOR i=r1 TO r9 DO  // find a spare reg, but not r or r0
				    UNLESS i=r DO
    				    UNLESS (ra & (1 << i))=0
	    				{
						    rn := i
		    			    gen_move_rr(i_MOV, rn, r0)
							BREAK
			    		}
			}
			GenLoadConstant(r0, staticoffset(n))
			gen_ldr_rrn(rx, rg, gn_staticaddr * 4)
			gen_blx(rx)
		    gen_ldr_rrn(r, r0, 0)
			IF r \= r0
			{
			    gen_move_rr(i_MOV, r0, rn)  // put back r0
            }
			GOTO ret
            
        CASE k_loc:
            TEST n <= 1023 
            THEN
                gen_ldr_rrn(r, rp, n*4)     // ldr r,[rp, 4*n]
            ELSE
            {
                GenLoadConstant(rx, n*4)    // ldr rx,#n*4
                gen_ldr_rrr(r, rp, rx)      // ldr r,[rp,rx]
            }
            GOTO ret
        
        CASE k_glob:
            TEST n <= 1023
            THEN
            {
                gen_ldr_rrn(r, rg, n*4)     // ldr r,[rg, 4*n]
            }
            ELSE
            {
                GenLoadConstant(rx, n*4)    // ldr rx,#n*4
                gen_ldr_rrr(r, rg, rx)      // ldr r,[rg,rx]
            }
            GOTO ret
        
        DEFAULT:
            cgerror("unknown type in movetor %n", k)
    }
    
ret:
    forgetr(r)
    remem(r, k, n)
    h1!a, h2!a := k_reg, r
    RESULTIS r
}


AND staticoffset(n) = VALOF
{
    FOR i=1 TO nstatics DO
	    IF staticslabels!i = n
		    RESULTIS (i - 1) * 4
	RESULTIS -1
}


AND choosereg(regs) = VALOF
{
     //IF debug>5 DO
     //    writef("choosereg(%x4)*n", regs)
     FOR r = r0 TO r9 DO
         UNLESS (regs>>r&1)=0 RESULTIS r
     IF (regs&1)=0 DO 
         cgerror("choosereg: no free regs")
     RESULTIS r0
}


AND powerof2(n) = VALOF  /* return shift value 2,3,4,5... for 4,8,16,32... */
{
    LET q = 4
    
    n := ABS n

    FOR p=2 TO 30 DO
    {
        IF q = n DO
        {
            RESULTIS p
        }
        q := q + q
    }
    RESULTIS 0
}

// find which register, if any, is used by an SS item
AND regusedby(a) = VALOF
{
    IF h1!a=k_reg 
        RESULTIS h2!a
    RESULTIS -1
}
 
 
AND isfree(r) = VALOF
{
    FOR t=tempv TO arg1 BY 3 DO
        IF regusedby(t)=r 
            RESULTIS FALSE
    RESULTIS TRUE
}
 
 
// Free register R by storing the SS item (if any)
// that depends on it.
AND freereg(r) BE 
    FOR t=tempv TO arg1 BY 3 DO
        IF regusedby(t)=r DO
        {
            storet(t)
            BREAK
        }
 
AND nextfree() = choosereg(~(regswithinfo() | regsinuse()))

// Store the value of a SS item in its true stack location.
AND storet(a) BE
{ 
    LET s = h3!a
    LET r = ?
    
    IF h1!a=k_loc & h2!a=s DO
        RETURN
    r := movetoanyr(a)

    TEST s <= 1023 
    THEN
        gen_str_rrn(r, rp, s * 4)   // str r,[rp, 4*s]
    ELSE
    {
        GenLoadConstant(rx, s * 4)        // mov rx,#s*4
        gen_arith_rrr(i_ADD, rx, rp, rx)  // add rx,rp,rx
        gen_str_rrn(r, rx, 0)             // str r,[rx,0]
    }
    forgetvar(k_loc, s)
    remem(r, k_loc, s)
    h1!a, h2!a := k_loc, s
}


// Load an item (K,N) onto the SS. It may move SS items.
AND loadt(k, n) BE
{
    cgpendingop()
    TEST arg1+3=tempt
    THEN 
    { 
        storet(tempv)  // SS stack overflow.
        FOR t = tempv TO arg2+2 DO 
            t!0 := t!3
    }
    ELSE 
        arg1, arg2 := arg1+3, arg2+3
    h1!arg1, h2!arg1, h3!arg1 := k, n, ssp
    ssp := ssp + 1
    IF maxssp<ssp DO 
        maxssp := ssp
}


// Replace the top two SS items by (K,N)
AND lose1(k, n) BE
{ 
    ssp := ssp - 1
    TEST arg2=tempv
    THEN 
    { 
        h1!arg2, h2!arg2 := k_loc, ssp-2
        h3!arg2 := ssp-2
    }
    ELSE 
    { 
        arg1 := arg2
        arg2 := arg2-3
    }
    h1!arg1, h2!arg1, h3!arg1 := k, n, ssp-1
}


AND swapargs() BE
{ 
    LET k, n = h1!arg1, h2!arg1
    h1!arg1, h2!arg1 := h1!arg2, h2!arg2
    h1!arg2, h2!arg2 := k, n
}


AND cgstind() BE
{
    LET r1, r2 = ?, ?
    
    IF pendingop=s_add DO
    {
        IF h1!arg2=k_numb DO
            swapargs()
        IF h1!arg1=k_numb DO
        {
            LET n = h2!arg1
            r1 := movetoanyrsh(arg2)
            
            forgetr(r1)
            lose1(k_reg, r1)
            r2 := movetoanyr(arg2)
            TEST n <= 1023
            THEN
                gen_str_rrn(r2, r1, n*4)   // str r2,[r1,#n*4]
            ELSE
            {
                GenLoadConstant(rx, n*4)   // ldr rx,#n*4
                gen_str_rrr(r2, r1, rx)    // str r2,[r1,rx]
            }
            stack(ssp-2)
            pendingop := s_none
            forgetallvars()
            RETURN
        }
    }

    cgpendingop()
    r1 := movetoanyrsh(arg1)
    r2 := movetoanyr(arg2)
    forgetr(r1)
    gen_str_rrn(r2, r1, 0)             // str r2,[r1,#0]
    stack(ssp-2)
    forgetallvars()
}


// Store the top item of the SS in (K,N).
AND storein(k, n) BE
// K is K_LOC, K_GLOB or K_LAB.
{
    LET r = ?
    LET rn = ?
	LET ra = ?
	
    cgpendingop()
	chkhwm()
	
    r := movetoanyr(arg1)

    SWITCHON k INTO
    { 
        DEFAULT:
           cgerror("in storein %n", k)

        CASE k_loc:
            TEST n <= 1023
            THEN
            {
                gen_str_rrn(r, rp, n*4)    // str r,[rp,n*4]
            }
            ELSE
            {
                GenLoadConstant(rx, n*4) 
                gen_str_rrr(r, rx, rp)     // str r,[rx,rp]
            }
            ENDCASE

        CASE k_glob:
            TEST n <= 1023
            THEN
                gen_str_rrn(r, rg, n*4)    // str r,[rg,n*4]
            ELSE
            {
                GenLoadConstant(rx, n*4) 
                gen_str_rrr(r, rx, rg)     // str r,[rx,rg]
            }
            ENDCASE
            
        CASE k_lab:
		    ra := NOT (regswithinfo() | regsinuse())  // available regs
		    FOR i=r1 TO r9 DO  // find a spare reg, but not r or r0
			    UNLESS i=r DO
				    UNLESS (ra & (1 << i))=0
    				{
					    rn := i
	    			    gen_move_rr(i_MOV, rn, r0)
						BREAK
		    		}
			GenLoadConstant(r0, staticoffset(n))
			gen_ldr_rrn(rx, rg, gn_staticaddr * 4)
			gen_blx(rx)
			TEST r=r0
			THEN
		        gen_str_rrn(rn, r0, 0)
            ELSE
		        gen_str_rrn(r, r0, 0)
		    gen_move_rr(i_MOV, r0, rn)
			ENDCASE
    }
    forgetvar(k, n)
    remem(r, k, n)
    stack(ssp-1)
}

LET cgrv() BE
{
    LET r = ?
    
    IF pendingop=s_add DO
    {
        IF h1!arg2=k_numb DO
            swapargs()
        IF h1!arg1=k_numb DO
        {
            LET n = h2!arg1
            r := movetoanyrsh(arg2)
            TEST -1023 <= n <= 1023
            THEN
                gen_ldr_rrn(r, r, n*4)
            ELSE
            {
                GenLoadConstant(rx, n*4) 
                gen_ldr_rrr(r, rx, r)     // ldr r,[rx,r]
            }
            forgetr(r)
            lose1(k_reg, r)
            pendingop := s_none
            RETURN
        }
    }

    cgpendingop()
    r := movetoanyrsh(arg1)
    gen_ldr_rrn(r, r, 0)
    forgetr(r)
    h1!arg1, h2!arg1 := k_reg, r
}

AND cgbyteap(op) BE
{
    TEST op=s_getbyte
    THEN
    {
        LET r1, r2 = ?, ?
        
        cgpendingop()
        r2 := movetoanyr(arg2)
        r1 := movetoanyr(arg1)
        gen_ldrb_rrrshl(r2, r1, r2, 2)      // ldrb r2,[r1, r2, lsl #2]
        forgetr(r2)
        lose1(k_reg, r2)
    }
    ELSE
    {
        LET r1, r2, r3 = ?, ?, ?
        LET arg3 = ?
        
        cgpendingop()
        arg3 := arg2 - 3
        r1 := movetoanyr(arg1)
        r2 := movetoanyr(arg2)
        r3 := movetoanyr(arg3)
        gen_strb_rrrshl(r3, r1, r2, 2)  // STRB r3,[r1, r2 lsl #2]
        forgetallvars()
        stack(ssp-3)
    }
}


AND cgglobal(n) BE
{ incode := FALSE
  chkstatics()
  cgstatics()
	
  FOR i=1 TO nstatics DO
  { LET val = staticslist!i
    IF debug>=6 DO
    writef("%i5:     %t8 %n*n",
            stvp, ".word", val)
    codew(val)
  }
  IF debug>=6 DO
    writef("%i5:     %t8 %n*n",
            stvp, ".word", nstatics)
  codew(nstatics)

  IF debug>=6 DO
    writef("%i5:     %t8 %n*n",
            stvp, ".word", 0)
  codew(0)       // Compile Global initialisation data.
  FOR i = 1 TO n DO 
  { LET gn = rdgn()
    LET lab = rdl()
    IF debug>=6 DO writef("%i5:     %t8 %n*n",
                            stvp, ".word", gn)
    codew(rdgn())
    IF debug>=6 DO writef("%i5:     %t8 L%n @ Relative to start of section*n",
                            stvp, ".word", lab)
    codew(labv!rdl()-stvpstart) 
  }
  IF debug>=6 DO writef("%i5:     %t8 %n*n", stvp, ".word", maxgn)
  codew(maxgn)
}


AND cgentry(l, n) BE
{ MANIFEST { upb=11 } // Max length of entry name
   
  LET v = VEC upb/bytesperword

  v%0 := upb
  // Pack up to 11 character of the name into v including
  // the first and last five.
  TEST n<=11
  THEN { FOR i = 1 TO n DO 
           v%i := rdn()
         FOR i = n+1 TO 11 DO 
           v%i := '*s'
       }
  ELSE { FOR i = 1 TO 5 DO 
           v%i := rdn()
         FOR i = 6 TO n-6 DO 
           rdn() // Ignore the middle characters
         FOR i = 6 TO 11 DO 
           v%i := rdn()
         IF n>11 DO 
           v%6 := '*''
       }

  IF naming DO 
  { LET word0 = entryword
    LET word1 = pack4b(v%0, v%1, v% 2, v% 3)
    LET word2 = pack4b(v%4, v%5, v% 6, v% 7)
    LET word3 = pack4b(v%8, v%9, v%10, v%11)

    IF debug>=6 DO writef("%i5:     %t8 0x%x8*n", stvp, ".word", word0)
    codew(word0)
    IF debug>=6 DO writef("%i5:     %t8 0x%x8*n", stvp, ".word", word1)
    codew(word1)
    IF debug>=6 DO writef("%i5:     %t8 0x%x8*n", stvp, ".word", word2)
    codew(word2)
    IF debug>=6 DO writef("%i5:     %t8 0x%x8*n", stvp, ".word", word3)
    codew(word3)
  }
  IF debug>=6 DO 
    writef("// Entry to:   %s*n", v)
  setlab(l)
  forgetall()
  incode := TRUE
}


AND cgsave(n) BE
{ FOR r=r0 TO r3 DO
  { LET s = 3+r-r0
    IF s >= n BREAK
    remem(r, k_loc, s)
  }

  initstack(n)

  IF debug>=6 DO
    writef("%i5:     %t8 r4!,{rp,lr,pc}    @ inc r4*n", stvp, "STMIA")
  codew(#xE8A4C800)                    /* STM r4,{rp,lr,pc}     inc r4    */
  IF debug>=6 DO
    writef("%i5:     %t8 r4,{r0,r1,r2,r3}  @ no inc r4*n", stvp, "STM")
  codew(#XE884000F)                    /* STM r4,{r0,r1,r2,r3}  no inc r4 */
  gen_arith_rrq(i_SUB, rp, r4, 12)     /* SUB rp,r4,#12                   */
}


// Function or routine call.
AND cgapply(op, k) BE
{
    LET sa1 = k+3
    LET sa4 = k+6

    cgpendingop()
    
    /* store args 5,6.... */
    store(sa4+1, ssp-2)
    
    /* now deal with non-args */
    FOR t = tempv TO arg2 BY 3 DO
    {
        IF h3!t >= k BREAK
        IF h1!t >= k_reg DO 
            storet(t)
    }
    
    /* move args 1-4 to arg regs */
    FOR t = arg2 TO tempv BY -3 DO
    {
        LET s = h3!t
        LET r = s-k-3
        
        IF s < sa1 
            BREAK
        IF s <= sa4 & isfree(r) DO 
            movetor(t, r)
    }

    FOR t = arg2 TO tempv BY -3 DO
    {
        LET s = h3!t
        LET r = s-k-3
        IF s < sa1 
            BREAK
        IF s <= sa4 DO
            movetor(t, r)
    }
    
    /* deal with args not in SS */
    FOR s = sa1 TO sa4 DO
    {
        LET r = s-k-3
        IF s >= h3!tempv 
            BREAK
        IF regusedby(arg1) = r DO 
            movetor(arg1, r9)
        loadt(k_loc, s)
        movetor(arg1, r)
        stack(ssp-1)
    }

    GenAddConstant(r4, rp, 4*k, FALSE)           /* ADD r4,rp,#4*k  */
    TEST h1!arg1=k_fnlab
    THEN
        genbranchandlink(h2!arg1)
    ELSE
    {
        movetor(arg1, rx)
        gen_blx(rx)   /* BLX rx */ 
    }
    forgetall()
    stack(k)
    
    IF op = s_fnap DO
        loadt(k_reg, r0)
}


AND cgreturn(op) BE
{ cgpendingop()
  IF incode DO
  { IF op = s_fnrn DO
    { movetor(arg1, r0)
      stack(ssp - 1)
    }
    IF debug>=6 DO
      writef("%i5:     %t8 rp,{rp,pc}  @ no inc*n", stvp, "LDM")
    codew(#xE89B8800)   /* LDM rp,{rp,pc}  no inc */
  }
  incode := FALSE
  initstack(ssp)
}


// Used for OCODE operators JT and JF.
AND cgjump(b, l) BE
{ 
    LET f = jmpfn(pendingop)
    IF f<0 DO 
    { 
        loadt(k_numb,0)
        f := b_NE 
    }
    pendingop := s_none
    UNLESS b DO 
        f := compjfn(f)
    store(0, ssp-3)
    f := cgcmp(f)
    UNLESS f = b_NONE
        genbranch(f, l)
    stack(ssp-2)
}

AND jmpfn(op) = VALOF 
    SWITCHON op INTO
    { 
        DEFAULT:  RESULTIS -1
        CASE s_eq: RESULTIS b_EQ
        CASE s_ne: RESULTIS b_NE
        CASE s_ls: RESULTIS b_LS
        CASE s_gr: RESULTIS b_GR
        CASE s_le: RESULTIS b_LE
        CASE s_ge: RESULTIS b_GE
    }


AND compjfn(f) = f=b_EQ -> b_NE,
                 f=b_NE -> b_EQ,
                 f=b_LS -> b_GE,
                 f=b_GE -> b_LS,
                 f=b_GR -> b_LE,
                 f=b_LE -> b_GR,
                 f


AND cgcmp(f) = VALOF
{
    LET k1, n1 = h1!arg1, h2!arg1
    LET k2, n2 = h1!arg2, h2!arg2
    LET swapped = ?

    IF k1=k_numb & k2=k_numb DO
    {
        LET jumping = FALSE

        SWITCHON f INTO
        {
            CASE b_EQ:
                jumping := n1 = n2
                ENDCASE
            CASE b_NE:
                jumping := n1 ~= n2
                ENDCASE
            CASE b_GE:
                jumping := n2 >= n1
                ENDCASE
            CASE b_LS:
                jumping := n2 < n1
                ENDCASE;
            CASE b_GR:
                jumping := n2 > n1
                ENDCASE
            CASE b_LE:
                jumping := n2 <= n1
                ENDCASE
            DEFAULT:
                cgerror("unknown constant branch condition %n", f)
                ENDCASE         
        }
        TEST jumping
        THEN
            RESULTIS b_BR
        ELSE
            RESULTIS b_NONE  /* suppress the jump */
    }
    
    swapped := cgdyadic(i_CMP, TRUE)
    UNLESS swapped DO 
        RESULTIS f

    SWITCHON f INTO
    {
        CASE b_LS:
            RESULTIS b_GR
        CASE b_GR:
            RESULTIS b_LS
        CASE b_LE:
            RESULTIS b_GE
        CASE b_GE:
            RESULTIS b_LE
        DEFAULT:
            RESULTIS f
    }
}

AND genbranch(bfn, l) BE 
  IF incode DO
  { LET a = labv!l
    TEST a<0
    THEN { // label is unset?
           llist := getblk(llist, stvp, l) // make ref to L
           gen_b(bfn, 0, l) // compile branch instruction to be filled in later
         }
    ELSE { // no, the label was set
           gen_b(bfn, ((a-stvp-8)/4) & #xFFFFFF, l)
         } 
    IF bfn=b_BR DO
    { incode := FALSE
      chkstatics()
    }
  }
 
AND genbranchandlink(l) BE 
  IF incode DO
  { LET a = labv!l
    TEST a<0
    THEN { // label is unset?
           llist := getblk(llist, stvp, l) // make ref to L
           gen_bl(0, l)   // compile branch and link instruction to be
	                  // filled in later
         }
    ELSE { // no, the label was set
           gen_bl((a-stvp-8)/4 & #xFFFFFF, l)
	 }
  }
 
// Compiles code for SWITCHON.
LET cgswitch() BE
{ 
    LET n = rdn()     // Number of cases.
    LET dlab = rdl()  // Default label.
    LET ht1 = VEC 2048
    LET ht2 = VEC 2048
    LET casek = VEC 2048
    LET casel = VEC 2048
    LET htsize = 8
    LET htstart =?
    LET prime = ?
    LET m = 0
    LET s = ?
    LET exthtsize = ?
    
    cgpendingop()
    store(0, ssp-2)
    movetor(arg1, r0)
    forgetall()
    stack(ssp-1)

    IF n>=2048 DO
        cgerror("Too many SWITCHON cases (%n)", n)
        
    IF (n < 5) DO
    {
        FOR i=1 TO n DO
        {
            GenCompare(r0, rdn())
            genbranch(b_EQ, rdl())
        }
        genbranch(b_BR, dlab)
        RETURN
    }
    
    WHILE (n*3)/2 >= htsize DO
        htsize := htsize * 2
    
    FOR i=0 TO htsize-1 DO
        ht1!i, ht2!i := 0, 0  // all slots set empty
        
    // Read (K,L) pairs.
    FOR i=0 TO n-1 DO
    { 
        LET k = rdn()
        LET l = rdl()
        TEST k=0  // 0 is not a valid hash table entry 
        THEN
        {
            GenCompare(r0, 0)           // cmp r0,#0 
            genbranch(b_EQ, l)          // beq l
            LOOP
        }
        ELSE
        {
            casek!m := k
            casel!m := l
            m := m + 1
        }
    }

    prime := hashprime(htsize, m, casek) // 8-bit number to ensure single instruction

    // linear open hash
    FOR i=0 TO m-1 DO
    { 
        LET k = casek!i
        LET l = casel!i
        LET p = (k*prime) & (htsize-1)
        
        WHILE ht1!p ~= 0 DO
            p := (p+1) & (htsize-1)
        ht1!p := k
        ht2!p := l
    }

    GenLoadConstant(r1, prime*4)          // mov r1,#prime*4
    gen_mul(r2, r1, r0)                   // mul r2,r1,r0
    s := operand2((htsize-1)*4)
    TEST s>=0
    THEN
        gen_arith_rrq(i_AND, r1, r2, s)   // and r1,r2,#(htsize-1)*4
    ELSE
    {
        GenLoadConstant(r1, (htsize-1)*4) // mov r1,#(htsize-1)*4
        gen_arith_rrr(i_AND, r1, r2, r1)  // and r1,r2,r1
    }
    
    exthtsize := htsize 
    WHILE ht1!((exthtsize-1) REM htsize) ~= 0 DO    // add wrap around items until last item is zero
        exthtsize := exthtsize+1

    GenLoadConstant(r2, exthtsize*4-4)    // mov r2,#htsize*4-4 (-4 because SWITCHON code points to next word)
    gen_ldr_rrn(r3, rg, gn_switch*4)      // ldr r3,[rg,#gnswitch*4]  call switchon code in alib
    gen_blx(r3)                           // blx r3
    
    htstart := stvp   // hash table offsets are relative to the lr

    FOR i=0 TO exthtsize-1 DO
    { 
        codew(ht1!(i REM htsize))
    }
    FOR i=0 TO exthtsize-1 DO
    {
        LET x = ht2!(i REM htsize)
        codew(x=0 -> labv!dlab-htstart, labv!x-htstart)
    }    
}

AND hashprime(htsize, n, casek) = VALOF  // return prime producing minimum number of colllisions for the
{                                        // array casek containing n cases in a hash table of size htsize
    LET bestprime = ?
    LET mincollisions = maxint
    LET hashtab = VEC 2048
    LET primes = TABLE
         13,  17,  19,  23,  29,  31,  37,  41,  43,  47,
         53,  59,  61,  67,  71,  73,  79,  83,  89,  97,
        101, 103, 107, 109, 113, 127, 131, 137, 139, 149,
        151, 157, 163, 167, 173, 179, 181, 191, 193, 197,
        199, 211, 223, 227, 229, 233, 239, 241, 251
    
    FOR i = 0 TO 48 DO
    {
        LET collisions = 0

        FOR j=0 TO htsize-1 DO
            hashtab!j := 0

        FOR j=0 TO n-1 DO
        {
            LET k = (casek!j * primes!i) REM htsize
            {   
                TEST hashtab!k=0
                THEN
                {
                    hashtab!k := casek!j
                    BREAK
                }
                ELSE
                    k := (k+1) REM htsize
            } REPEAT
        }
        
        FOR j=0 TO n-1 DO
        {
            LET k = (casek!j * primes!i) REM htsize
            {
                TEST hashtab!k=casek!j
                THEN
                    BREAK
                ELSE
                {
                    k := (k+1) REM htsize
                    collisions := collisions+1
                }
            } REPEAT
        }
        IF collisions < mincollisions DO
        {
            mincollisions := collisions
            bestprime := primes!i
        }
    }
    RESULTIS bestprime
}
 
 
    //  CLASS Bits:
    //             s   w   m  cr   r  r9  r8  r7  r6  r5  r4  r3  r2  r1  r0
    //         0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
 
AND class(a) = VALOF
{
    LET k, n = h1!a, h2!a
    LET bits = regscontaining(k, n)
 
    //IF debug>5 DO
    //    writef("regscontaining(%n,%n) %x4*n", k, n, bits)
 
    SWITCHON k INTO
    {
        DEFAULT:
            // CASE k_lvloc:   CASE k_locsh:
            // CASE k_lvglob:  CASE k_globsh:
            // CASE k_lvlab:   CASE k_labsh:
            // CASE k_lvlabsh:
            ENDCASE
 
        CASE k_glob:
        CASE k_loc:
        CASE k_lab:
            bits := bits | c_m
            ENDCASE
  
        CASE k_numb:
            bits := bits | c_w
            ENDCASE
 
        CASE k_reg: 
            bits := bits | c_r | c_cr
    }
 
    //IF debug>5 DO
    //    writef("class(%n,%n) %x8*N", h1!a, h2!a, bits)
    RESULTIS bits
}
 
AND initslave() BE 
    FOR r = r0 TO r9 DO 
        slave!r := 0
 
AND forgetr(r) BE
    IF r <= r9
    {
        UNLESS slave!r=0 DO
        {
            LET a = @slave!r
            UNTIL !a = 0 DO
                a := !a
            !a := freelist
            freelist := slave!r
            slave!r := 0
        }
    }
 
AND forgetall() BE
{
    FOR r = r0 TO r9 DO 
        forgetr(r)
}
 
AND remem(r, k, n) BE
    IF r<= r9 & k<k_reg DO
        slave!r := getblk(slave!r, k, n)
 
AND moveinfo(s, r) BE
    UNLESS s=r DO
    { 
        LET p = slave!s
        forgetr(r)
        UNTIL p=0 DO
        {
            remem(r, h2!p, h3!p)
            p := !p
        }
    }
 

// Forget the slave information about the
// variable (K, N).
// K is one of: K_LOC, K_GLOB, K_LAB
AND forgetvar(k, n) BE
{
    FOR r = r0 TO r9 DO
    {
        LET a = @slave!r
        {
            LET p = !a
            IF p=0 BREAK
            TEST h3!p=n & (h2!p & k_notsh)=k
            THEN 
            {
                !a := !p   // free and unlink the item
                freeblk(p)
            }
            ELSE 
                a := p
        } REPEAT
    }
}
    
AND forgetallvars() BE  // Called after STIND or PUTBYTE.
    FOR r = r0 TO r9 DO
    {
        LET a = @slave!r
        {
            LET p = !a
            IF p=0 BREAK
            TEST h2!p < k_labsh
            THEN 
            {
                !a := !p   // free and unlink the item
                freeblk(p)
            }
            ELSE 
                a := p
        } REPEAT
    }


AND regscontaining(k, n) = VALOF
{
    LET regset = 0
 
    IF k=k_reg 
        RESULTIS 1<<n | c_cr+c_r
 
    FOR r = r0 TO r9 DO
        IF isinslave(r, k, n) DO
            regset := regset | (1<<r) | c_cr
 
    RESULTIS regset
}
 
AND inregs(r, regs) =
    r<0 | (regs>>r & 1)=0 | r>r9 -> FALSE, TRUE
 
AND isinslave(r, k, n) = VALOF
{
    LET p = slave!r
  
    UNTIL p=0 DO
    {
        IF h2!p=k & h3!p=n 
            RESULTIS TRUE
        p := !p
    }
 
    RESULTIS FALSE
}
 
AND regsinuse() = VALOF
{
    LET regset = 0
 
    FOR t = tempv TO arg1 BY 3 DO
        IF h1!t>=k_reg DO
        {
            LET r = h1!t REM 10  // regs r0 to r9
            IF h1!t=k_reg DO 
                r := h2!t
            regset := regset | (1<<r)
        }
    RESULTIS regset
}
 
AND regswithinfo() = VALOF
{
    LET regset = 0
    FOR r = r0 TO r9 DO
        UNLESS slave!r=0 DO 
            regset := regset | (1<<r)
    RESULTIS regset
}

// set the label L to the current location
AND setlab(l) BE
{ 
    IF debug>=6 DO 
        writef("%i5: L%n:*n", stvp, l)

    labv!l := stvp  // Set the label.
}
 
AND addoffset(a, n) BE
{
    LET x = getw(a)
    putw(a, x | n)
}
 
AND cgstring(n) BE
{ 
    LET lab, a = newlab(), n
    loadt(k_lvlab, lab)

    { // Start of packing loop
        LET t  = getblk(0, lab, 0) // The first item hold the label
        LET b, c, d, e, f, g, h = 0, 0, 0, 0, 0, 0, 0
        !nliste := t
        nliste := !nliste
        lab := 0                  // Clear the label for further items

        IF n>=1 DO b := rdn()
        IF n>=2 DO c := rdn()
        IF n>=3 DO d := rdn()
        n := n-4      // 1 to 4 bytes have been packed
        //TEST bigender
        //THEN h3!t := pack4b(a,b,c,d)
        //ELSE h3!t := pack4b(d,c,b,a)
        h3!t := pack4b(a,b,c,d)

        IF n<0 BREAK  // There are no more characters to pack

        a := rdn()
    } REPEAT
}


AND getblk(a, b, c) = getblk4(a, b, c, 0)


AND getblk4(a, b, c, d) = VALOF
{ 
    LET p = freelist
    TEST p=0 THEN 
    { 
        dp := dp-blkupb-1
        checkspace()
        p := dp 
    }
    ELSE 
        freelist := !p
    h1!p, h2!p, h3!p, h4!p := a, b, c, d
    RESULTIS p
}

AND freeblk(p) BE
{
    !p := freelist
    freelist := p    
}


AND cgitemn(n) BE
{
    LET p = getblk(0, datalabel, n)
    
    datalabel := 0
    !nliste := p
    nliste := p
}


// Compile static data.  It is only
// called at the outermost level
// There are no ITEML items since are regarded
// as constants so as to allow position independent
// code.  ITEML information is held on the LLIST
 
AND cgstatics() BE 
{ LET p = @llist   // branch label references (24-bit offset)

  UNTIL nlist=0 DO
  { LET nl = nlist  // data items
    nliste := @nlist  // All NLIST items will be freed.
    nl := !nl REPEATUNTIL nl=0 | h2!nl ~= 0
    setlab(h2!nlist)  // NLIST always starts labelled.

    { LET blk = nlist
      nlist := !nlist
      freeblk(blk)
      codew(h3!blk)
    } REPEATUNTIL nlist=0 | h2!nlist ~= 0
  }

  // Fill in possible branch refs
  { LET r = !p
    LET a, l = ?, ?
    IF r=0 BREAK
        
    a := h2!r
    l := h3!r
        
    TEST labv!l >= 0
    THEN { putw(a, getw(a) | ((labv!l-a-8)/4) & #xFFFFFF)
           !p := !r  // remove item from LLIST
           freeblk(r)
         }
    ELSE { p := r  // keep the item
         }
  } REPEAT
}


AND cgconstants() BE 
{
    LET p = @clist   // constant references (12-bit pc offset)

    // Fill in possible refs
    {
        LET r = !p
        LET a, n = ?, ?
        LET offset = ?
        
        IF r=0 BREAK
        
        a := h2!r
        n := h3!r
        
        offset := stvp-a-8
        IF offset > #xFFF DO
            cgerror("cgconstant: offset error %n stvp=%x8", offset, stvp)
        codew(n)
        putw(a, getw(a) | (offset & #xFFF))
        !p := !r  // remove item from CLIST
        freeblk(r)
    } REPEAT
}


AND chkhwm() BE  /* check pending 12-bit offsets before load or store instructions */
{
    LET p = @clist   // constant references (12-bit pc offset)
    LET offset = ?

    // Fill in possible refs
    {
        LET r = !p
        LET a, n = ?, ?
        IF r=0 BREAK
        
        a := h2!r
        n := h3!r
        
        offset := stvp-a-4  // -4 because the data word occurs after the inserted jump
        TEST offset > 3800  // leave until lastest opportunity (12-bit offset)
        THEN
        {
            //writef("chkoffset: offset=%n jmp added at %x8 for %x8*n", offset, stvp, a) // *** dja
            codew(#xEA000000)  // b *+8
            codew(n)
            putw(a, getw(a) | (offset & #xFFF))
            !p := !r  // remove item from CLIST
            freeblk(r)
        }
        ELSE
            p := r  // keep the item
    } REPEAT
}


AND cgstaticrefs() BE 
{
    LET p = @slist   // static references (12-bit pc offset)

    // Fill in possible refs
    {
        LET r = !p
        LET a = ?
        LET l1, l2 = ?, ?
        LET l1a, l2a = ?, ?
        
        IF r=0 BREAK
        
        a := h2!r
        l1 := h3!r
        l2 := h4!r
        l1a := labv!l1
        l2a := labv!l2
        
        TEST l1a >= 0 & l2a >= 0
        THEN
        {
            putw(a, getw(a) | ((l1a-a-8) & #xFFF))
            putw(l1a, l2a-a-12)
            !p := !r  // remove item from SLIST
            freeblk(r)
        }
        ELSE
            p := r  // keep the item
    } REPEAT
}


AND initdatalists() BE
{ 
    llist           := 0         // label references (24-bit offset)
    nlist,   nliste := 0, @nlist // static and data
    clist           := 0         // constant references (12-bit pc offset)
    slist           := 0         // static references (12-bit pc offset + 32-bit offset)
    freelist        := 0
}

AND checkspace() BE 
    IF stvp/4>=codespacesize DO
    { 
        cgerror("Program too large, %n bytes compiled", stvp)
        errcount := errcount+1
        longjump(fin_p, fin_l)
    }


AND pack4b(b0, b1, b2, b3) =
  bigender -> b0<<24 | b1<<16 | b2<<8 | b3,
              b3<<24 | b2<<16 | b1<<8 | b0

AND codew(w) BE
{
    putw(stvp, w)
    stvp := stvp + 4
}

AND putw(a, w) BE
   TEST bigender
   THEN stv%a, stv%(a+1), stv%(a+2), stv%(a+3) := w>>24 & #xff,w>>16 & #xff, w>>8 & #xff, w & #xff
   ELSE stv%(a+3), stv%(a+2), stv%(a+1), stv%a := w>>24 & #xff,w>>16 & #xff, w>>8 & #xff, w & #xff

AND getw(a) = 
   bigender -> stv%a<<24 | stv%(a+1)<<16 | stv%(a+2)<<8  | stv%(a+3),
               stv%a     | stv%(a+1)<<8  | stv%(a+2)<<16 | stv%(a+3)<<24

AND chkstatics() BE
{
    cgstatics()
    cgconstants()
    cgstaticrefs()
}

/*
ELF data is written before and after writing the 
position-independent block of program code.
Run 'readelf -a file.o' to view the ELF information.
The variables in this block are progsize (the total 
program size in bytes) and (progsize + #xDO)
*/
LET outputsection() BE
{
    LET Elf_blk1 = TABLE 
    #x464C457F, #x00010101, #x00000000, #x00000000, 
    #x00280001, #x00000001, #x00000000, #x00000000, 
    #x00000000, #x05000000, #x00000034, #x00280000, // word 8: progsize+#xB4 
    #x00010007, #x68732E00, #x74727473, #x2E006261, 
    #x74727473, #x2E006261, #x746D7973, #x2E006261, 
    #x6D6D6F63, #x00746E65, #x7373622E, #x61642E00, 
    #x2E006174, #x2E6C6572, #x74786574, #x65742E00, 
    #x00007478, #x00000000, #x00000000, #x00000000, 
    #x00000000, #x00000000, #x00000000, #x00000000, 
    #xFFF10004, #x00000000, #x00000000, #x00000000, 
    #x00040003, #x00000000, #x00000000, #x00000000, 
    #x00050003                                      // size: 45 words

    LET Elf_blk2 = TABLE
    #x00000000, #x00000000, #x00000000, #x00000000,
    #x00000000, #x00000000, #x00000000, #x00000000,
    #x00000000, #x00000000, #x00000001, #x00000003,
    #x00000000, #x00000000, #x00000034, #x0000003F,
    #x00000000, #x00000000, #x00000001, #x00000000,
    #x0000000B, #x00000003, #x00000000, #x00000000,
    #x00000073, #x00000001, #x00000000, #x00000000,
    #x00000001, #x00000000, #x00000013, #x00000002,
    #x00000000, #x00000000, #x00000074, #x00000040,
    #x00000002, #x00000004, #x00000004, #x00000010,
    #x00000039, #x00000001, #x00000006, #x00000000,
    #x000000B4, #x00000000, #x00000000, #x00000000, // word 45: progsize
    #x00000004, #x00000000, #x00000029, #x00000001,
    #x00000003, #x00000000, #x00000000, #x00000000, // word 54: progsize+#xB4
    #x00000000, #x00000000, #x00000004, #x00000000,
    #x0000002F, #x00000009, #x00000000, #x00000000,
    #x00000000, #x00000000, #x00000003, #x00000004, // word 64: progsize+#xB4
    #x00000004, #x00000008                          // size: 70 words

    LET outstream = output()
        
    selectoutput(tostream)

    FOR p=0 TO 44 DO
        TEST p=8
        THEN writew(stvp + #xB4)
        ELSE writew(Elf_blk1[p])

    FOR p=0 TO stvp/4-1 DO
        writew(getw(p * 4))
    
    FOR p=0 TO 69 DO
        TEST p=45
        THEN
            writew(stvp)
        ELSE
            TEST p=54 | p=64
            THEN
                writew(stvp + #xB4)
            ELSE
                writew(Elf_blk2[p])

    selectoutput(outstream)
}

AND writew(w) BE
    TEST bigender 
    THEN
    {
        wrch(w >> 24 & #xff)
        wrch(w >> 16 & #xff)
        wrch(w >>  8 & #xff)
        wrch(w       & #xff)
    }
    ELSE
    {
        wrch(w       & #xff)
        wrch(w >>  8 & #xff)
        wrch(w >> 16 & #xff)
        wrch(w >> 24 & #xff)
    }


AND dboutput() BE
{ IF debug>=8 DO
  { writes("REGS: ")
    FOR r = r0 TO r9 DO
    { LET p = slave!r
      IF p=0 LOOP
      writef(" R%N= ", r)
      WHILE p DO
      { wrkn(h2!p, h3!p)
        p := !p
      }
    }
    newline()
  }
 
  IF debug>=9 DO 
  { writes("STK: ")
    FOR p=tempv TO arg1 BY 3  DO
    { IF (p-tempv) MOD 30 = 10 DO 
        newline()
      wrkn(h1!p,h2!p)
      wrch('*s')
    }
    newline()
  }
  
  IF debug=10 DO
  { LET p = llist
    writes("LLIST:  ")
    WHILE p DO
    { writef("%N:L%N ", h2!p, h3!p)
      p := !p
    }
    newline()
    p := clist
    writes("CLIST:  ")
    WHILE p DO
    { writef("%N:%N ", h2!p, h3!p)
      p := !p
    }
    newline()
  }
}


AND wrkn(k,n) BE
{
    LET s = VALOF SWITCHON k INTO
    {
        DEFAULT:          RESULTIS "?"
        CASE k_numb:      RESULTIS "N%N"
        CASE k_loc:       RESULTIS "P%N"
        CASE k_glob:      RESULTIS "G%N"
        CASE k_lab:       RESULTIS "L%N"
        CASE k_locsh:     RESULTIS "PS%N"
        CASE k_globsh:    RESULTIS "GS%N"
        CASE k_labsh:     RESULTIS "LS%N"
        CASE k_lvloc:     RESULTIS "@P%N"
        CASE k_lvglob:    RESULTIS "@G%N"
        CASE k_lvlab:     RESULTIS "@L%N"
        CASE k_lvlocsh:   RESULTIS "@PS%N"
        CASE k_lvglobsh:  RESULTIS "@GS%N"
        CASE k_lvlabsh:   RESULTIS "@LS%N"
        CASE k_reg:       RESULTIS "R%N"
    }
    writef(s, n)
    wrch('*S')
}

AND ocodename1(op) = VALOF SWITCHON op INTO
{ DEFAULT:         RESULTIS "NONE"

  CASE s_lp:       RESULTIS "LP"
  CASE s_lg:       RESULTIS "LG"
  CASE s_ln:       RESULTIS "LN"

  CASE s_lstr:     RESULTIS "LSTR"

  CASE s_true:     RESULTIS "TRUE"
  CASE s_false:    RESULTIS "FALSE"

  CASE s_llp:      RESULTIS "LLP"
  CASE s_llg:      RESULTIS "LLG"

  CASE s_sp:       RESULTIS "SP"
  CASE s_sg:       RESULTIS "SG"

  CASE s_lf:       RESULTIS "LF"
  CASE s_ll:       RESULTIS "LL"
  CASE s_lll:      RESULTIS "LLL"
  CASE s_sl:       RESULTIS "SL"
      
  CASE s_stind:    RESULTIS "STIND"

  CASE s_rv:       RESULTIS "RV"

  CASE s_mul:      RESULTIS "MULT"
  CASE s_div:      RESULTIS "DIV"
  CASE s_mod:      RESULTIS "MOD"
  CASE s_add:      RESULTIS "ADD"
  CASE s_sub:      RESULTIS "SUB"
  CASE s_eq:       RESULTIS "EQ"
  CASE s_ne:       RESULTIS "NE"
  CASE s_ls:       RESULTIS "LS"
  CASE s_gr:       RESULTIS "GR"
  CASE s_le:       RESULTIS "LE"
  CASE s_ge:       RESULTIS "GE"
  CASE s_lshift:   RESULTIS "LSHIFT"
  CASE s_rshift:   RESULTIS "RSHIFT"
  CASE s_logand:   RESULTIS "LOGAND"
  CASE s_logor:    RESULTIS "LOGOR"
  CASE s_eqv:      RESULTIS "EQV"
  CASE s_xor:      RESULTIS "XOR"
  CASE s_not:      RESULTIS "NOT"
  CASE s_neg:      RESULTIS "NEG"
  CASE s_abs:      RESULTIS "ABS"

  CASE s_jt:       RESULTIS "JT"
  CASE s_jf:       RESULTIS "JF"

  CASE s_goto:     RESULTIS "GOTO"

  CASE s_lab:      RESULTIS "LAB"

  CASE s_query:    RESULTIS "QUERY"

  CASE s_stack:    RESULTIS "STACK"

  CASE s_store:    RESULTIS "STORE"

  CASE s_entry:    RESULTIS "ENTRY"

  CASE s_save:     RESULTIS "SAVE"

  CASE s_fnap:     RESULTIS "FNAP"
  CASE s_rtap:     RESULTIS "RTAP"

  CASE s_fnrn:     RESULTIS "FNRN"
  CASE s_rtrn:     RESULTIS "RTRN"

  CASE s_endproc:  RESULTIS "ENDPROC"

  CASE s_res:      RESULTIS "RES"
  CASE s_jump:     RESULTIS "JUMP"

  CASE s_rstack:   RESULTIS "RSTACK"

  CASE s_finish:   RESULTIS "FINISH"

  CASE s_switchon: RESULTIS "SWITCHON"

  CASE s_getbyte:  RESULTIS "GETBYTE"
  CASE s_putbyte:  RESULTIS "PUTBYTE"

  CASE s_global:   RESULTIS "GLOBAL"

  CASE s_datalab:  RESULTIS "DATALAB"
}

AND instrname(op) = VALOF SWITCHON op INTO
{ DEFAULT:       RESULTIS "NONE"

  CASE i_AND:    RESULTIS "AND"
  CASE i_EOR:    RESULTIS "EOR"
  CASE i_SUB:    RESULTIS "SUB"

  CASE i_RSB:    RESULTIS "RSB"

  CASE i_ADD:    RESULTIS "ADD"
  CASE i_TST:    RESULTIS "TST"

  CASE i_TEQ:    RESULTIS "TEQ"
  CASE i_CMP:    RESULTIS "CMP"

  CASE i_CMN:    RESULTIS "CMN"
  CASE i_ORR:    RESULTIS "ORR"

  CASE i_MOV:    RESULTIS "MOV"
//  CASE i_BTC:    RESULTIS "BTC"
  CASE i_MVN:    RESULTIS "MVN"
  CASE i_MUL:    RESULTIS "MUL"
      
  CASE i_DIV:    RESULTIS "DIV"

  CASE i_RV:     RESULTIS "RV"

  CASE i_REM:    RESULTIS "REM"
  CASE i_NEG:    RESULTIS "NEG"
  CASE i_ABS:    RESULTIS "ABS"
  CASE i_NOT:    RESULTIS "NOT"
  CASE i_LSHIFT: RESULTIS "LSHIFT"
  CASE i_RSHIFT: RESULTIS "RSHIFT"
  //CASE i_RV:     RESULTIS "RV"
}

AND condname(op) = VALOF SWITCHON op INTO
{ DEFAULT:       RESULTIS "NONE"
  CASE b_EQ:     RESULTIS "EQ"
  CASE b_NE:     RESULTIS "NE"
  CASE b_LS:     RESULTIS "LS"
  CASE b_GR:     RESULTIS "GR"
  CASE b_LE:     RESULTIS "LE"
  CASE b_GE:     RESULTIS "GE"
}

// if n can be formed as an 8-bit unsigned integer and a 4-bit shift,
// then the 12-bit operand2 value is returned, otherwise -1
AND operand2(n) = VALOF
{
    LET sh = 16
    
    WHILE ((n & 3) = 0) & ((n & #xFFFFFF00) ~= 0) DO
        sh, n := sh-1, n>>2
        
    IF (n & #xFFFFFF00) = 0
        RESULTIS (sh & #xf) << 8 | n

    RESULTIS -1
}


AND GenLoadConstant(r, n) BE
{
    LET s = operand2(n)
    
    IF s>=0 DO
    {
        gen_move_rq(i_MOV, r, s)  // mov r,#s
        RETURN
    }
    
    s := operand2(~n)

    IF s>=0 DO
    {
        gen_move_rq(i_MVN, r, s)  // mvn r,#s
        RETURN
    }
    
    clist := getblk(clist, stvp, n)  // 32-bit number to be generated asap
    gen_ldr_rrn(r, pc, 0)   // ldr r,[pc,#0]   offset to be filled in later
}


AND GenAddConstant(rd, rn, n, twowords) BE  // Rd := rn + #n
{
    LET op = i_ADD
    LET s = ?
    
    IF n<0 DO
        op, n := i_SUB, -n

    s := operand2(n)    
    IF s>=0 DO
    {
        IF twowords DO  // two words for branch offsets
            gen_nop()   // force 2nd instruction to be an addition
        gen_arith_rrq(op, rd, rn, s & #xfff) // add rd,rn,#s
        RETURN
    }
    
    clist := getblk(clist, stvp, n)  // 32-bit number to be generated asap
    gen_ldr_rrn(rx, pc, 0)           // ldr rx,[pc,#0]   offset to be filled in later
    gen_arith_rrr(op, rd, rn, rx)    // add rd,rn,rx 
}

AND GenCompare(r, n) BE
{
    LET s = operand2(n)

    TEST s>=0
    THEN
        gen_cmp_rq(i_CMP, r, s)  // cmp r,#s
    ELSE
    {
        s := operand2(-n)
        TEST s>=0
        THEN
            gen_cmp_rq(i_CMN, r, s)  // cmn r,#s
        ELSE
        {
            clist := getblk(clist, stvp, n)  // 32-bit number to be generated asap
            gen_ldr_rrn(rx, pc, 0)           // ldr rx,[pc,#0]   offset to be filled in later
            gen_cmp_rr(i_CMP, r, rx)
        }        
    }
}
