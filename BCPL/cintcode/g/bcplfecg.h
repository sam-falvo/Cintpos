// This header file contains the interface between the standard
// BCPL compiler front end and its codegenerators.

/*
History

18/02/2026
Added cgdebughelp

03/09/2022
Added nocomments

24/08/2021
Extended for the MCPL style extensions, and adjusted feg, trng.
Added check in bcplfe.b for feg and trn validity.

13/05/2013
Extended to include globals used by bcpl64

05/01/2011
Extended to include manifests and globals used by xbcpl and procode

*/

MANIFEST {
c64 = BITSPERBCPLWORD=64 // Added 21/9/2019 replaces the global
                         // Use ON64 instead defined in libhdr.h
			 
// Interface globals are between ug and feg-1
intg=ug     // First of the interface globals, ie those
            // common to Lex, Syn, Trn and the codegenerator.
feg=intg+75 // First of the Lex/Syn globals
trng=feg+75 // First of the TRN globals, changed 11/09/2025
cgg=trng+90 // CG globals are cgg and above, changed 21/04/2022

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
s_slct; s_of                   // Inserted 11/7/2001
s_byteap; s_mthap
s_not; s_lshift; s_rshift; s_logand; s_logor
s_eqv; s_xor; s_cond; s_comma; s_table
s_needs; s_section
s_ass

s_rtap; s_goto; s_resultis; s_colon
s_test; s_for; s_if; s_unless
s_while; s_until; s_repeat; s_repeatwhile; s_repeatuntil
s_skip // Added 22/06/2005
s_next // Added 28/08/2021
s_exit // Added 28/08/2021 changed 08/10/2021
s_loop; s_break; s_return; s_finish
s_endcase; s_switchon; s_case; s_default

s_seq; s_let; s_and; s_manifest; s_global; s_static
s_valdef; s_vecdef; s_constdef; s_const
s_fndef; s_rtdef; s_local; s_label
s_path1; s_path2; s_path3; s_path4  // Added 30/08/2021 

// Tokens needed for the MCPL extensions
s_match; s_every  // Keywords for MATCH and EVERY expressions and commands
s_matche; s_matchc  // Operators for MATCH tree nodes
s_everye; s_everyc  // Operators for EVERY tree nodes
s_matchitemc; s_matchiteme // Operators for match item tree nodes
s_patfndef; s_patrtdef
s_yields           // The => operator
s_patptr; s_patseq
s_pateq;  s_patne;  s_patls;  s_patgr;  s_patle;  s_patge
s_patfeq; s_patfne; s_patfls; s_patfgr; s_patfle; s_patfge
s_patand; s_pator; s_range; s_frange


// Other lexical tokens
s_be; s_lsect; s_rsect; s_get
s_semicolon; s_into; s_to; s_by; s_do; s_else
s_vec; s_lparen; s_rparen; s_sbra; s_sket; s_dot; s_dots; s_eof
s_bitsperbcplword

// Used in the code generators and in bcplfe.b when compiling E1 := E2
// assop2op(op)=>s_none when op=s_ass or s_fass.
s_none

// Ocode operators
s_lf; s_lp; s_lg 
s_ln    // Load an integer or floating point constant
s_lflt  // Load a 64 bit floating point constant from a 32 bt one
        // This is only generated when a 32 bit compiler has
	// a 64 bit bit target.
s_lstr; s_ll; s_llp; s_llg; s_lll 
s_sp; s_sg; s_sl; s_stind; s_jump; s_jt; s_jf
s_endfor // endfor is no longer used,
	 // but is left in to leave the other constants unchanged
s_lab; s_stack; s_store; s_rstack; s_entry
s_save; s_fnrn; s_rtrn; s_res; s_datalab; s_itemn
s_itemflt // Only used by 32 bit bcpl compiling to 64 bit target
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

s_pos
s_line           // Added 10/02/18   For checksyn
s_comment        // Added 20/06/2022 for Ocode comments
	         // It is followed by comment text up to the next newline.

s_opmax          // This must be checked to see that it is less than 256

s_fltbit =256    // Changed 30/08/2021
s_fltmask=s_fltbit-1


s_fglobal   = s_global   + s_fltbit
s_flocal    = s_local    + s_fltbit
s_fpath1    = s_path1    + s_fltbit  // Added 30/08/2021
s_fpath2    = s_path2    + s_fltbit  // Added 30/08/2021
s_fpath3    = s_path3    + s_fltbit  // Added 30/08/2021
s_fpath4    = s_path4    + s_fltbit  // Added 30/08/2021
s_fstatic   = s_static   + s_fltbit
s_fmanifest = s_manifest + s_fltbit
s_flabel    = s_label    + s_fltbit  // Added 02/03/2023

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
// Globals shared by the frontend and codegenerators.
stdout:intg
stdin
stderr

nametable; nametablesize
fin_p; fin_l; plist; treep; treevec

fromfilename    // Compiler options
tofilename      // Derived from TO/K
errfilename
mapfilename     // Set by MAP/K option for the Z80 codegenerator
listfilename    // Set by LIST/K option

opname   // For lex tokens, tree and ocode ops
         // Used by both fe and cg
flopname // For ocode ops and Cintcode ops
sfname   // For ocode assignment ops used in SELST
cv2flt

// OCODE buffer variables
obuf; obufp; obufq; obuft; obufsize
rdn; wrn  

trnerr
translate        // Main function of the translation phase
codegenerate     // Codegenerator Main function
writeocode
procode

naming
bigender
eqcases
bining
xrefing
gdefsing
hdrs
defs             // Conditional compilation defs

defaultencoding  // Default encoding, set by command args.
encoding         // Current encoding =RTF8 or GB2312

savespacesize
hard             // Abort on errors

T16              // =TRUE if generating 16-bit target code
T32              // =TRUE if generating 32-bit target code
T64              // =TRUE if generating 64-bit target code

debug
noselst          // TRUE if not compiling SELLD and SELST instructions.


objline1         // either "" or of form "#!..."
objline1written
defstring        // The defs argument

compiling32to32  // =TRUE if 32-bit BCPL is compiling for a 32-bit target
compiling32to64  // =TRUE if 32-bit BCPL is compiling for a 64-bit target
compiling64to32  // =TRUE if 32-bit BCPL is compiling for a 32-bit target
compiling64to64  // =TRUE if 64-bit BCPL is compiling for a 64-bit target

targetbytelen    // = 2, 4 or  8
targetbitlen     // = 16, 32 or 64

errcount; errmax
fromstream       // The current source stream
sysprint         // The current output stream
ocodeout         // The output stream when writing numerical ocode
tostream         // Stream for compiled code
errstream

// The following are to remove the need for the compiler to handle
// to floating point constants 0.0 and 1.0

flt0  // Floating point 0.0 constructed by sys(Sys_flt, fl_mk, 0, 0)
flt1  // Floating point 1.0 constructed by sys(Sys_flt, fl_mk, 1, 0)
flt10 // Floating point 10.0 constructed by sys(Sys_flt, fl_mk, 10, 0)

helping // Added 06/03/2026
cgdebughelp

writeocode
procode

lastintglobal // Used to check for global overlap with syng
}

