// This header file contains the interface between the standard
// BCPL compiler front end and its codegenerators.

/*
13/05/13
Extended to include globals used by bcpl64

05/01/11
Extended to include manifests and globals used by xbcpl and procode

*/

MANIFEST {

// Interface globals are betweeb ug and feg-1
intg=ug     // First of the interface globals, ie those
            // common to Lex, Syn, Trn and the codegenerator.
feg=intg+60 // First of the Lex/Syn globals
trng=feg+60 // First of the TRN globals
cgg=trng+60 // CG globals are cgg and above

//  Selectors
h1=0; h2; h3; h4; h5; h6; h7

// BCPL lexical tokens, tree and ocode operators

s_number=1
s_name; s_string; s_true; s_false
s_valof; s_lv; s_rv; s_vecap; s_fnap
s_query
s_neg; s_abs // Integer operators
s_mul; s_div; s_mod
s_add; s_sub
s_eq; s_ne; s_ls; s_gr; s_le; s_ge
s_slct; s_of                   // Inserted 11/7/01
s_byteap; s_mthap
s_not; s_lshift; s_rshift; s_logand; s_logor
s_eqv; s_xor; s_cond; s_comma; s_table
s_needs; s_section
s_ass
s_rtap; s_goto; s_resultis; s_colon
s_test; s_for; s_if; s_unless
s_while; s_until; s_repeat; s_repeatwhile; s_repeatuntil
s_skip // Added 22/6/05
s_loop; s_break; s_return; s_finish
s_endcase; s_switchon; s_case; s_default
s_seq; s_let; s_and; s_manifest; s_global; s_static
s_valdef; s_vecdef; s_constdef; s_const
s_fndef; s_rtdef; s_local; s_label

// Othe lexical token
s_be; s_lsect; s_rsect; s_get
s_semicolon; s_into; s_to; s_by; s_do; s_else
s_vec; s_lparen; s_rparen; s_sbra; s_sket; s_dot; s_eof
s_bitsperbcplword

// Used in the code generators and in bcplfe.b when compiling E1 := E2
// assop2op(op)=>s_none when op=s_ass or s_fass.
s_none

// Ocode operators
s_lf; s_lp; s_lg 
s_ln    // Load an integer or floating point constant
s_lstr; s_ll; s_llp; s_llg; s_lll 
s_sp; s_sg; s_sl; s_stind; s_jump; s_jt; s_jf; s_endfor
s_lab; s_stack; s_store; s_rstack; s_entry
s_save; s_fnrn; s_rtrn; s_res; s_datalab; s_itemn
s_endproc; s_getbyte; s_putbyte

// Floating point operators and assignment operators, added 15/07/10

s_fnum           // Floating point constants
s_float; s_fix; s_fabs
s_fmul; s_fdiv; s_fmod; s_fadd; s_fsub;  s_fpos; s_fneg
s_feq; s_fne; s_fls; s_fgr; s_fle; s_fge
s_fcond  // Added 18/02/18
s_fass   // Added 28/02/18 for #:= used by the FLT feature

// Assign operators -- added 15/07/10
// These are used by xbcpl.b not bcpl.b
s_assvecap
s_assmul; s_assdiv; s_assmod; s_assadd; s_asssub
s_assfmul; s_assfdiv; s_assfmod; s_assfadd; s_assfsub
s_asslshift; s_assrshift
s_asslogand; s_asslogor; s_asseqv; s_assxor


s_selld; s_selst // Added 19/07/10

s_fltop  // FLTOP is followed by one of the fl_ codes
         // eg FLTOP FADD to do:  a := b #+ a
         // or FLTOP FLOAT to do: a := FLOAT a


s_flt            // Added 10/02/18   For the FLT extension
s_notflt
s_fltbit =128
s_fltmask=127


s_fglobal   = s_global   + s_fltbit
s_flocal    = s_local    + s_fltbit
s_fstatic   = s_static   + s_fltbit
s_fmanifest = s_manifest + s_fltbit

sf_none=0     // Assignment operators
sf_vecap
sf_fmul
sf_fdiv
sf_fmod       // Incompatible change 26/11/18
sf_fadd
sf_fsub
sf_mul
sf_div
sf_mod
sf_add
sf_sub
sf_lshift
sf_rshift
sf_logand
sf_logor
sf_eqv
sf_xor
}

GLOBAL {
// Option variables, and fe-cg interface variables

nametable:intg; nametablesize
fin_p; fin_l; plist; treep; treevec

opname   // For lex tokens, tree and ocode ops
         // Used by both fe and cg
flopname // For ocode ops and Cintcode ops
sfname   // For ocode assignment ops used in SELST
cv2flt

// OCODE buffer variables
obuf; obufp; obufq; obuft; obufsize
rdn; wrn  

trnerr
translate
savespacesize
codegenerate

bigender         // Compiler options
naming
debug
eqcases
prtree
prtree2
bining
xrefing
gdefsing
hard             // Abort on errors
noselst          // TRUE if not compiling SELLD and SELST instructions.

objline1         // either "" or of form "#!..."
objline1written
optstring        // The opt argument
c64              // =TRUE if compiler is running on a 64-bit system
t64              // =TRUE if generating 64-bit Cintcode
wordbytelen      // = 4 or  8
wordbitlen       // =32 or 64
encoding         // Current encoding =RTF8 or GB2312
defaultencoding  // Default encoding, set by command args.

errcount; errmax
sourcestream; sysprint; ocodeout
gostream         // Stream for compiled code
//sourcenamev      // Fileno to string mapping -- not used by any CG.

// The following are to remove the need for the compiler to handle
// to floating point constants 0.0 and 1.0

flt0    // To hold floating point 0.0 constructed by sys(Sys_flt, fl_mk, 0, 0)
flt1    // To hold floating point 1.0 constructed by sys(Sys_flt, fl_mk, 1, 0)
flt10   // To hold floating point 10.0 constructed by sys(Sys_flt, fl_mk, 10, 0)
}

