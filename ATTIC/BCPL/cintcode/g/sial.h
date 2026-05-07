// Incompatible changes were made on 14/01/2019
// Programs usinf sial.h will need minor changes and will
// have to be recompiled.
// The changes replace f_rem and f_xrem by f_mod and f_xmod,
// the addition of f_fmod and f_fxmod and the renumbering of
// most of the f_ codes.

MANIFEST {    // sial opcodes and directives
f_1=1         // Used in the run length encoding scheme
f_2           // in compacted sial

f_lp      //  3
f_lg
f_ll

f_llp     //  6
f_llg
f_lll
f_lf
f_lw

f_l       // 11
f_lm

f_sp
f_sg
f_sl

f_ap      // 16
f_ag
f_a
f_s

f_lkp     // 20
f_lkg
f_rv
f_rvp
f_rvk
f_st
f_stp
f_stk
f_stkp
f_skg
f_xst

f_k       // 31
f_kpg

f_neg
f_not
f_abs

f_mul     // 36
f_div
f_xdiv
f_mod
f_xmod
f_add
f_sub
f_xsub

f_eq     // 44
f_ne
f_ls
f_gr
f_le
f_ge

f_eq0    // 50
f_ne0
f_ls0
f_gr0
f_le0
f_ge0

f_lsh     // 56
f_rsh
f_and
f_or
f_xor
f_eqv

f_gbyt    // 62
f_xgbyt
f_pbyt
f_xpbyt

f_swb
f_swl

f_xch    // 68
f_atb
f_atc
f_bta
f_btc
f_atblp
f_atblg
f_atbl

f_j     // 76
f_rtn
f_goto

f_ikp   // 79
f_ikg
f_ikl
f_ip
f_ig
f_il

f_jeq    // 85
f_jne
f_jls
f_jgr
f_jle
f_jge

f_jeq0  // 91
f_jne0
f_jls0
f_jgr0
f_jle0
f_jge0
f_jge0m

f_brk    // 9 8
f_nop
f_chgco
f_mdiv
f_sys

f_section   // 103
f_modstart
f_modend
f_global
f_string
f_const
f_static
f_mlab
f_lab
f_lstr
f_entry

f_float   // 114
f_fix
f_fabs
f_fmul
f_fdiv
f_fxdiv
f_fmod
f_fxmod
f_fadd
f_fsub
f_fxsub
f_fneg

f_feq   // 126
f_fne
f_fls
f_fgr
f_fle
f_fge

f_feq0   // 132
f_fne0
f_fls0
f_fgr0
f_fle0
f_fge0

f_jfeq   // 138
f_jfne
f_jfls
f_jfgr
f_jfle
f_jfge

f_jfeq0  // 144
f_jfne0
f_jfls0
f_jfgr0
f_jfle0
f_jfge0

f_res    // 155
f_ldres

f_selld
f_selst
f_xselst
}

// The assignment operator codes are copied from g/bcplfecg.h
MANIFEST {
sf_none=0     // Assignment operators
sf_vecap
sf_fmul
sf_fdiv
sf_fmod
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

