/*
** This is a 32 bit MINTCODE interpreter written in C
**
** (c) Copyright:  Martin Richards  21 July 1997
*/


#include <stdio.h>
#include <stdlib.h>

/* cinterp.h contains machine/system dependent #defines  */
#include "minterp.h"

extern FILE *logfp;

#define TRACINGyes
#define TALLYyes

extern INT32 result2;

extern int tracing;

extern INT32 *tallyv;
extern INT32 tallylim;

extern INT32 dosys(INT32 *p, INT32 *g);
extern INT32 muldiv(INT32 a, INT32 b, INT32 c);

extern void wrcode(char *form, INT32 f, INT32 a); 
extern void wrfcode(INT32 f);
extern void trace(INT32 pc, INT32 p, INT32 a, INT32 b);

#define Gn_currco      7
#define Gn_result2    10

/* MINTCODE function codes  */

#define F_0       0

#define F_brk     2
#define F_k0      0
#define F_lf     12
#define F_lm     14
#define F_lm1    15
#define F_l0     16
#define F_fhop   27
#define F_jeq    28

#define F_k      32
#define F_kh     33
#define F_kw     34
#define F_k0g    32
#define F_k0g1   (F_k0g+32)
#define F_k0gh   (F_k0g+64)
#define F_s0g    44
#define F_s0g1   (F_s0g+32)
#define F_s0gh   (F_s0g+64)
#define F_l0g    45
#define F_l0g1   (F_l0g+32)
#define F_l0gh   (F_l0g+64)
#define F_l1g    46
#define F_l1g1   (F_l1g+32)
#define F_l1gh   (F_l1g+64)
#define F_l2g    47
#define F_l2g1   (F_l2g+32)
#define F_l2gh   (F_l2g+64)
#define F_lg     48
#define F_lg1    (F_lg+32)
#define F_lgh    (F_lg+64)
#define F_sg     49
#define F_sg1    (F_sg+32)
#define F_sgh    (F_sg+64)
#define F_llg    50
#define F_llg1   (F_llg+32)
#define F_llgh   (F_llg+64)
#define F_ag     51
#define F_ag1    (F_ag+32)
#define F_agh    (F_ag+64)
#define F_mul    52
#define F_div    53
#define F_mod    54
#define F_xor    55
#define F_sl     56
#define F_ll     58
#define F_jne    60

#define F_llp    64
#define F_llph   65
#define F_llpw   66
#define F_add    84
#define F_sub    85
#define F_lsh    86
#define F_rsh    87
#define F_and    88
#define F_or     89
#define F_lll    90
#define F_jls    92

#define F_l      96
#define F_lh     97
#define F_lw     98
#define F_rv    116
#define F_rtn   123
#define F_jgr   124

#define F_lp    128
#define F_lph   129
#define F_lpw   130
#define F_lp0   128

#define F_sys   145
#define F_lvind 146
#define F_stb   147
#define F_st    148
#define F_stp0  149

#define F_jle   156

#define F_sp    160
#define F_sph   161
#define F_spw   162
#define F_sp0   160
#define F_s0    176
#define F_xch   181
#define F_indb  182
#define F_indb0 183
#define F_atc   184
#define F_atb   185
#define F_j     186
#define F_jge   188

#define F_ap    192
#define F_aph   193
#define F_apw   194
#define F_ap0   192

#define F_indw  205
#define F_lmh   206
#define F_btc   207
#define F_nop   208
#define F_a0    208
#define F_rvp0  211
#define F_st0p0 216
#define F_st1p0 218
#define F_cta   223

#define F_a     224
#define F_ah    225
#define F_aw    226
#define F_l0p0  224
#define F_s     237
#define F_sh    238
#define F_mdiv  239
#define F_chgco 240
#define F_neg   241
#define F_not   242

#define F_inc1b 243
#define F_inc4b 244
#define F_dec1b 245
#define F_dec4b 246
#define F_inc1a 247
#define F_inc4a 248
#define F_dec1a 249
#define F_dec4a 250
 
#define F_hand  252
#define F_unh   254
#define F_raise 255


/* The function interpret is designed to be separately compiled,
// and possibly implemented in assembly language.
//
// Unless either TRACINGyes or TALLYyes are defined, its only free
// variable is the function dosys(p, g).
//
// mem  is the pointer to the mintcode memory.
// regs is the position in the mintcode memory where the initial
//      value of the mintcode registers.
//
// interpret executes Mintcode instructions and returns with an
// integer result as follows:
//     0      sys(0, 0) called
//     1      Non existant instruction
//     2      Brk instruction
//     3      Zero count
//     4      Negative pc
//     5      Division by zero
//     n      sys(0, n) called
//
// On return the mintcode registers are dumped back in the vector regs
*/

INT32 interpret(INT32 regs, register unsigned char *B)
{ 
#ifdef BIGENDER
#define GH(x) ((INT32) B[x]<<8 | B[x+1])
#define GW(x) ((((INT32) B[x]<<8|B[x+1])<<8|B[x+2])<<8|B[x+3])
#else
#define GH(x) ((INT32) B[x+1]<<8 | B[x])
#define GW(x) ((((INT32) B[x+3]<<8|B[x+2])<<8|B[x+1])<<8|B[x])
#endif

   
   register INT32        a  = (PT(B+regs))[0];
   register INT32        b  = (PT(B+regs))[1];
   INT32                 c  = (PT(B+regs))[2];
   register INT32        p  = (PT(B+regs))[3];
   register INT32        g  = (PT(B+regs))[4];
   INT32                 st = (PT(B+regs))[5];
   register INT32        pc = (PT(B+regs))[6];
   register INT32     count = (PT(B+regs))[7];
   register INT32         h = (PT(B+regs))[8];

   register INT32 *Wp  = (PT(B+p)), /* Optimise access to the stack */
                  *Wg  = (PT(B+g)), /* Optimise access to the global vector */
                  *Wg1 = (PT(B+g))+256;

   INT32  res, k, i;

   if (pc<0) goto negpc;
   
fetch:
   if (count>=0) /* count>=0  means execute count instructions
                    count=-1 means go on for ever */
      if (count--==0) { res = 3; goto ret; }

#ifdef TRACINGyes
   if (tracing) trace(pc, p, a, b);
#endif

#ifdef TALLYyes
   if (pc<tallylim && pc>0) tallyv[pc]++;
#endif

   switch((int) B[pc++])

{  default:       /* Case F_0 has been added explicitly to               */
   case F_0:      /* improve the compiled code (with luck)               */
                  res = 1; pc--; goto ret; /* Unimplemented instruction  */

   case F_brk:    res = 2; pc--; goto ret;  /* BREAKPOINT  */
                 
   case F_k0+3:   Wp[ 3] = p; p +=  3<<2; goto applyk;
   case F_k0+4:   Wp[ 4] = p; p +=  4<<2; goto applyk;
   case F_k0+5:   Wp[ 5] = p; p +=  5<<2; goto applyk;
   case F_k0+6:   Wp[ 6] = p; p +=  6<<2; goto applyk;
   case F_k0+7:   Wp[ 7] = p; p +=  7<<2; goto applyk;
   case F_k0+8:   Wp[ 8] = p; p +=  8<<2; goto applyk;
   case F_k0+9:   Wp[ 9] = p; p +=  9<<2; goto applyk;
   case F_k0+10:  Wp[10] = p; p += 10<<2; goto applyk;
   case F_k0+11:  Wp[11] = p; p += 11<<2; goto applyk;
   applyk:        Wp    = PT(B+p);
                  Wp[1] = pc;
                  pc    = a;
                  Wp[2] = pc;
                  Wp[3] = a = b;
                  if (pc>=0) goto fetch;
                  goto negpc;

   case F_lf:    b = a; a = pc + (SBP B)[pc];     pc++; goto fetch;
 
   case F_lf+1:  b = a;
                 a = (pc>>1) + B[pc];
                 a = (a<<1) + (SHP B)[a];         pc++; goto fetch;

   case F_lm:    b = a; a = - (INT32)(B[pc++]);         goto fetch;

   case F_l0-1:  b = a; a = -1; goto fetch; 
   case F_l0:    b = a; a =  0; goto fetch;
   case F_l0+1:  b = a; a =  1; goto fetch;
   case F_l0+2:  b = a; a =  2; goto fetch;
   case F_l0+3:  b = a; a =  3; goto fetch;
   case F_l0+4:  b = a; a =  4; goto fetch;
   case F_l0+5:  b = a; a =  5; goto fetch;
   case F_l0+6:  b = a; a =  6; goto fetch;
   case F_l0+7:  b = a; a =  7; goto fetch;
   case F_l0+8:  b = a; a =  8; goto fetch;
   case F_l0+9:  b = a; a =  9; goto fetch;
   case F_l0+10: b = a; a = 10; goto fetch;

   case F_fhop:  a = 0; pc++;      goto fetch;

   case F_jeq:   if(b==a) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jeq+1: if(b==a) goto indjump;
                 pc++; goto fetch;
   case F_jeq+2: if(a==0) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jeq+3: if(a==0) goto indjump;
                 pc++; goto fetch;

   case F_k:      k = B[pc]; Wp[k] = p; p +=  k<<2;
                  Wp    = PT(B+p);
                  Wp[1] = pc+1;
                  pc    = a;
                  Wp[2] = pc;
                  Wp[3] = a = b;
                  if (pc>=0) goto fetch;
                  goto negpc;

   case F_kh:     k = GH(pc); Wp[k] = p; p +=  k<<2;
                  Wp    = PT(B+p);
                  Wp[1] = pc+2;
                  pc    = a;
                  Wp[2] = pc;
                  Wp[3] = a = b;
                  if (pc>=0) goto fetch;
                  goto negpc;

   case F_kw:     k = GW(pc); Wp[k] = p; p +=  k<<2;
                  Wp    = PT(B+p);
                  Wp[1] = pc+4;
                  pc    = a;
                  Wp[2] = pc;
                  Wp[3] = a = b;
                  if (pc>=0) goto fetch;
                  goto negpc;

   case F_k0g+3:  Wp[ 3] = p; p +=  3<<2; goto applyg;
   case F_k0g+4:  Wp[ 4] = p; p +=  4<<2; goto applyg;
   case F_k0g+5:  Wp[ 5] = p; p +=  5<<2; goto applyg;
   case F_k0g+6:  Wp[ 6] = p; p +=  6<<2; goto applyg;
   case F_k0g+7:  Wp[ 7] = p; p +=  7<<2; goto applyg;
   case F_k0g+8:  Wp[ 8] = p; p +=  8<<2; goto applyg;
   case F_k0g+9:  Wp[ 9] = p; p +=  9<<2; goto applyg;
   case F_k0g+10: Wp[10] = p; p += 10<<2; goto applyg;
   case F_k0g+11: Wp[11] = p; p += 11<<2; goto applyg;
   applyg:        Wp    = PT(B+p);
                  Wp[1] = pc+1;
                  pc    = Wg[B[pc]];
                  Wp[2] = pc;
                  Wp[3] = a;
                  if (pc>=0) goto fetch;
                  goto negpc;

   case F_s0g:   (PT B)[Wg[B[pc++]]+0] = a;                goto fetch;
   case F_l0g:   b = a; a = (PT B)[Wg[B[pc++]]+0];         goto fetch;
   case F_l1g:   b = a; a = (PT B)[Wg[B[pc++]]+1];         goto fetch;
   case F_l2g:   b = a; a = (PT B)[Wg[B[pc++]]+2];         goto fetch;
   case F_lg:    b = a; a = Wg[B[pc++]];                   goto fetch;
   case F_sg:    Wg[B[pc++]]  = a;                         goto fetch;
   case F_llg:   b = a; a = g+(B[pc++]<<2);                goto fetch;
   case F_ag:    a += Wg[B[pc++]];                         goto fetch;

   case F_mul:   a = b * a;        goto fetch;
   case F_div:   if(a==0) {res = 5; pc--; goto ret; } /* Division by zero */
                 a = b / a;        goto fetch;
   case F_mod:   if(a==0) {res = 5; pc--; goto ret; } /* Division by zero */
                 a = b % a;        goto fetch;
   case F_xor:   a = b ^ a;        goto fetch;

   case F_sl:   (PT B)[(pc+(SBP B)[pc])>>2] = a;       pc++; goto fetch;
   
   case F_sl+1: i = (pc>>1) + B[pc];
                i = (i<<1) + (SHP B)[i];
                (PT B)[i>>2] = a;                 pc++; goto fetch;

   case F_ll:   b = a; a = *(PT (B+pc+(SBP B)[pc]));pc++; goto fetch;

   case F_ll+1: i = (pc>>1) + B[pc];
                i = (i<<1) + (SHP B)[i];
                b = a; a = (PT B)[i>>2];          pc++; goto fetch;

   case F_jne:   if(b!=a) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jne+1: if(b!=a) goto indjump;
                 pc++; goto fetch;
   case F_jne+2: if(a!=0) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jne+3: if(a!=0) goto indjump;
                 pc++; goto fetch;

   case F_llp:  b = a; a = p+4*B[pc++];           goto fetch;
   case F_llph: b = a; a = p+4*GH(pc);   pc += 2; goto fetch;
   case F_llpw: b = a; a = p+4*GW(pc);   pc += 4; goto fetch;

   case F_k0g1+3:  Wp[ 3] = p; p +=  3<<2; goto applyg1;
   case F_k0g1+4:  Wp[ 4] = p; p +=  4<<2; goto applyg1;
   case F_k0g1+5:  Wp[ 5] = p; p +=  5<<2; goto applyg1;
   case F_k0g1+6:  Wp[ 6] = p; p +=  6<<2; goto applyg1;
   case F_k0g1+7:  Wp[ 7] = p; p +=  7<<2; goto applyg1;
   case F_k0g1+8:  Wp[ 8] = p; p +=  8<<2; goto applyg1;
   case F_k0g1+9:  Wp[ 9] = p; p +=  9<<2; goto applyg1;
   case F_k0g1+10: Wp[10] = p; p += 10<<2; goto applyg1;
   case F_k0g1+11: Wp[11] = p; p += 11<<2; goto applyg1;
   applyg1:        Wp    = PT(B+p);
                   Wp[1] = pc+1;
                   pc    = Wg1[B[pc]];
                   Wp[2] = pc;
                   Wp[3] = a;
                   if (pc>=0) goto fetch;
                   goto negpc;
 
   case F_s0g1:  (PT B)[Wg1[B[pc++]]+0] = a;               goto fetch;
   case F_l0g1:  b = a; a = (PT B)[Wg1[B[pc++]]+0];        goto fetch;
   case F_l1g1:  b = a; a = (PT B)[Wg1[B[pc++]]+1];        goto fetch;
   case F_l2g1:  b = a; a = (PT B)[Wg1[B[pc++]]+2];        goto fetch;
   case F_lg1:   b = a; a = Wg1[B[pc++]];                  goto fetch;
   case F_sg1:   Wg1[B[pc++]] = a;                         goto fetch;
   case F_llg1: b = a; a = g+1024+(B[pc++]<<2);            goto fetch;
   case F_ag1:   a += Wg1[B[pc++]];        goto fetch;

   case F_add:   a = b + a;        goto fetch;
   case F_sub:   a = b - a;        goto fetch;
   case F_lsh:   if (a>31) b=0; /* bug */
                 a = b << a;       goto fetch;
   case F_rsh:   if (a>31) b=0; /* bug */
                 a = (INT32)((UWD b)>>a); goto fetch;
   case F_and:   a = b & a;        goto fetch;
   case F_or:    a = b | a;        goto fetch;

   case F_lll:   b = a; a = pc+(SBP B)[pc];   pc++; goto fetch;
   case F_lll+1: b = a;
                 i = (pc>>1) + B[pc];
                 a = (i<<1) + (SHP B)[i];
                 pc++; goto fetch;

   case F_jls:   if(b<a) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jls+1: if(b<a) goto indjump;
                 pc++; goto fetch;
   case F_jls+2: if(a<0) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jls+3: if(a<0) goto indjump;
                 pc++; goto fetch;


   case F_l:     b = a; a = B[pc++];               goto fetch;
   case F_lh:    b = a; a = GH(pc);       pc += 2; goto fetch;
   case F_lw:    b = a; a = GW(pc);       pc += 4; goto fetch;

   case F_k0gh+3:  Wp[ 3] = p; p +=  3<<2; goto applygh;
   case F_k0gh+4:  Wp[ 4] = p; p +=  4<<2; goto applygh;
   case F_k0gh+5:  Wp[ 5] = p; p +=  5<<2; goto applygh;
   case F_k0gh+6:  Wp[ 6] = p; p +=  6<<2; goto applygh;
   case F_k0gh+7:  Wp[ 7] = p; p +=  7<<2; goto applygh;
   case F_k0gh+8:  Wp[ 8] = p; p +=  8<<2; goto applygh;
   case F_k0gh+9:  Wp[ 9] = p; p +=  9<<2; goto applygh;
   case F_k0gh+10: Wp[10] = p; p += 10<<2; goto applygh;
   case F_k0gh+11: Wp[11] = p; p += 11<<2; goto applygh;
   applygh:        Wp    = PT(B+p);
                   Wp[1] = pc+2;
                   pc    = Wg[GH(pc)];
                   Wp[2] = pc;
                   Wp[3] =  a;
                   if (pc>=0) goto fetch;
                   goto negpc;


   case F_s0gh:  (PT B)[Wg[GH(pc)]+0] = a;        pc += 2; goto fetch;
   case F_l0gh:  b = a; a = (PT B)[Wg[GH(pc)]+0]; pc += 2; goto fetch;
   case F_l1gh:  b = a; a = (PT B)[Wg[GH(pc)]+1]; pc += 2; goto fetch;
   case F_l2gh:  b = a; a = (PT B)[Wg[GH(pc)]+2]; pc += 2; goto fetch;

   case F_lgh:   b = a; a = Wg[GH(pc)];   pc += 2; goto fetch;

   case F_sgh:   Wg[GH(pc)]   = a;        pc += 2; goto fetch;

   case F_llgh: b = a; a = g+(GH(pc)<<2); pc += 2; goto fetch;

   case F_agh:   a += Wg[GH(pc)]; pc += 2; goto fetch;
   
   case F_rv:    a = (PT (B+a))[0]; goto fetch;
   case F_rv+1:  a = (PT (B+a))[1]; goto fetch;
   case F_rv+2:  a = (PT (B+a))[2]; goto fetch;
   case F_rv+3:  a = (PT (B+a))[3]; goto fetch;
   case F_rv+4:  a = (PT (B+a))[4]; goto fetch;
   case F_rv+5:  a = (PT (B+a))[5]; goto fetch;
   case F_rv+6:  a = (PT (B+a))[6]; goto fetch;

   case F_rtn:   a = c;
                 pc = Wp[1];
                 p  = (PT(B+p))[0];
                 Wp = PT(B+p);
                 goto fetch;

   case F_jgr:   if(b>a) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jgr+1: if(b>a) goto indjump;
                 pc++; goto fetch;
   case F_jgr+2: if(a>0) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jgr+3: if(a>0) goto indjump;
                 pc++; goto fetch;

   case F_lp:   b = a; a = Wp[B[pc++]];          goto fetch;
   case F_lph:  b = a; a = Wp[GH(pc)];  pc += 2; goto fetch;
   case F_lpw:  b = a; a = Wp[GW(pc)];  pc += 4; goto fetch;

   case F_lp0+3:   b = a; a = Wp[3];  goto fetch;
   case F_lp0+4:   b = a; a = Wp[4];  goto fetch;
   case F_lp0+5:   b = a; a = Wp[5];  goto fetch;
   case F_lp0+6:   b = a; a = Wp[6];  goto fetch;
   case F_lp0+7:   b = a; a = Wp[7];  goto fetch;
   case F_lp0+8:   b = a; a = Wp[8];  goto fetch;
   case F_lp0+9:   b = a; a = Wp[9];  goto fetch;
   case F_lp0+10:  b = a; a = Wp[10]; goto fetch;
   case F_lp0+11:  b = a; a = Wp[11]; goto fetch;
   case F_lp0+12:  b = a; a = Wp[12]; goto fetch;
   case F_lp0+13:  b = a; a = Wp[13]; goto fetch;
   case F_lp0+14:  b = a; a = Wp[14]; goto fetch;
   case F_lp0+15:  b = a; a = Wp[15]; goto fetch;
   case F_lp0+16:  b = a; a = Wp[16]; goto fetch;

   case F_sys: if (a==0) { res = Wp[4]; goto ret; }  /* finish      */
               c = dosys(Wp, Wg); 
               goto fetch;                           /* system call */

   case F_lvind:  a = b + 4*a;                goto fetch;
   case F_stb:    B[a] = b;                   goto fetch;

   case F_st:    (PT (B+a))[0] = b; goto fetch;
   case F_st+1:  (PT (B+a))[1] = b; goto fetch;
   case F_st+2:  (PT (B+a))[2] = b; goto fetch;
   case F_st+3:  (PT (B+a))[3] = b; goto fetch;

   case F_stp0+3: (PT (B+a))[Wp[3]] = b; goto fetch;
   case F_stp0+4: (PT (B+a))[Wp[4]] = b; goto fetch;
   case F_stp0+5: (PT (B+a))[Wp[5]] = b; goto fetch;

   case 155:     res = 1; pc--; goto ret; /* Illegal instruction  */

   case F_jle:   if(b<=a) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jle+1: if(b<=a) goto indjump;
                 pc++; goto fetch;
   case F_jle+2: if(a<=0) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jle+3: if(a<=0) goto indjump;
                 pc++; goto fetch;

   case F_sp:    Wp[B[pc++]] = a;                  goto fetch;
   case F_sph:   Wp[GH(pc)]  = a;         pc += 2; goto fetch;
   case F_spw:   Wp[GW(pc)]  = a;         pc += 4; goto fetch;

   case F_sp0+3:  Wp[3]  = a; goto fetch;
   case F_sp0+4:  Wp[4]  = a; goto fetch;
   case F_sp0+5:  Wp[5]  = a; goto fetch;
   case F_sp0+6:  Wp[6]  = a; goto fetch;
   case F_sp0+7:  Wp[7]  = a; goto fetch;
   case F_sp0+8:  Wp[8]  = a; goto fetch;
   case F_sp0+9:  Wp[9]  = a; goto fetch;
   case F_sp0+10: Wp[10] = a; goto fetch;
   case F_sp0+11: Wp[11] = a; goto fetch;
   case F_sp0+12: Wp[12] = a; goto fetch;
   case F_sp0+13: Wp[13] = a; goto fetch;
   case F_sp0+14: Wp[14] = a; goto fetch;
   case F_sp0+15: Wp[15] = a; goto fetch;
   case F_sp0+16: Wp[16] = a; goto fetch;

   case F_s0+1: a -= 1; goto fetch;
   case F_s0+2: a -= 2; goto fetch;
   case F_s0+3: a -= 3; goto fetch;
   case F_s0+4: a -= 4; goto fetch;

   case F_xch:  a = a^b; b = a^b; a = a^b;    goto fetch;

   case F_indb:   a = B[b+a];                 goto fetch;
   case F_indb0:  a = B[a];                   goto fetch;

   case F_atc:   c = a;                       goto fetch;
   case F_atb:   b = a;                       goto fetch;
   case F_j:     pc += (SBP B)[pc];           goto fetch;

 indjump:
   case F_j+1:   pc = (pc>>1) + B[pc];
                 pc = (pc<<1) + (SHP B)[pc];
                 goto fetch;

   case F_jge:   if(b>=a) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jge+1: if(b>=a) goto indjump;
                 pc++; goto fetch;
   case F_jge+2: if(a>=0) { pc += (SBP B)[pc];   goto fetch; }
                 pc++; goto fetch;
   case F_jge+3: if(a>=0) goto indjump;
                 pc++; goto fetch;

   case F_ap:    a += Wp[B[pc++]];         goto fetch;
   case F_aph:   a += Wp[GH(pc)]; pc += 2; goto fetch;
   case F_apw:   a += Wp[GW(pc)]; pc += 4; goto fetch;

   case F_ap0+3:  a = a + Wp[ 3]; goto fetch;
   case F_ap0+4:  a = a + Wp[ 4]; goto fetch;
   case F_ap0+5:  a = a + Wp[ 5]; goto fetch;
   case F_ap0+6:  a = a + Wp[ 6]; goto fetch;
   case F_ap0+7:  a = a + Wp[ 7]; goto fetch;
   case F_ap0+8:  a = a + Wp[ 8]; goto fetch;
   case F_ap0+9:  a = a + Wp[ 9]; goto fetch;
   case F_ap0+10: a = a + Wp[10]; goto fetch;
   case F_ap0+11: a = a + Wp[11]; goto fetch;
   case F_ap0+12: a = a + Wp[12]; goto fetch;

   case F_indw:   a = (PT (B+b))[a];          goto fetch;

   case F_lmh:   b = a; a = - (INT32)(GH(pc)); pc += 2; goto fetch;
                
   case F_btc:  c = b;                      goto fetch;

   case F_nop:          goto fetch;
   case F_a0+1: a += 1; goto fetch;
   case F_a0+2: a += 2; goto fetch;
   case F_a0+3: a += 3; goto fetch;
   case F_a0+4: a += 4; goto fetch;
   case F_a0+5: a += 5; goto fetch;

   case F_rvp0+3: a = (PT (B+a))[Wp[3]]; goto fetch;
   case F_rvp0+4: a = (PT (B+a))[Wp[4]]; goto fetch;
   case F_rvp0+5: a = (PT (B+a))[Wp[5]]; goto fetch;
   case F_rvp0+6: a = (PT (B+a))[Wp[6]]; goto fetch;
   case F_rvp0+7: a = (PT (B+a))[Wp[7]]; goto fetch;

   case F_st0p0+4: (PT B)[Wp[4]+0] = a;  goto fetch; /*????*/
   case F_st0p0+3: (PT B)[Wp[3]+0] = a;  goto fetch; /*????*/

   case F_st1p0+3: (PT B)[Wp[3]+1] = a;  goto fetch; /*????*/
   case F_st1p0+4: (PT B)[Wp[4]+1] = a;  goto fetch; /*????*/
   
   case F_cta:  a = c;                   goto fetch;

   case F_a:    a += B[pc++];            goto fetch;
   case F_ah:   a += GH(pc);   pc += 2;  goto fetch;
   case F_aw:   a += GW(pc);   pc += 4;  goto fetch;

   case F_l0p0+3:  b = a; a = (PT B)[Wp[ 3]+0]; goto fetch; /*????*/
   case F_l0p0+4:  b = a; a = (PT B)[Wp[ 4]+0]; goto fetch; /*????*/
   case F_l0p0+5:  b = a; a = (PT B)[Wp[ 5]+0]; goto fetch; /*????*/
   case F_l0p0+6:  b = a; a = (PT B)[Wp[ 6]+0]; goto fetch; /*????*/
   case F_l0p0+7:  b = a; a = (PT B)[Wp[ 7]+0]; goto fetch; /*????*/
   case F_l0p0+8:  b = a; a = (PT B)[Wp[ 8]+0]; goto fetch; /*????*/
   case F_l0p0+9:  b = a; a = (PT B)[Wp[ 9]+0]; goto fetch; /*????*/
   case F_l0p0+10: b = a; a = (PT B)[Wp[10]+0]; goto fetch; /*????*/
   case F_l0p0+11: b = a; a = (PT B)[Wp[11]+0]; goto fetch; /*????*/
   case F_l0p0+12: b = a; a = (PT B)[Wp[12]+0]; goto fetch; /*????*/

   case F_s:    a -= B[pc++];           goto fetch;
   case F_sh:   a -= GH(pc);   pc += 2; goto fetch;

   /* MDIV is called from muldiv(a,b,c) via SYSLIB */
   case F_mdiv:  a = muldiv(Wp[3], Wp[4], Wp[5]);
                 Wg[Gn_result2] = result2;
                 pc = Wp[1];       /* code for rtn  */
                 p  = (PT(B+p))[0];
                 Wp = PT(B+p);
                 goto fetch;

   /* CHGCO is called from chgco(val, cptr) via SYSLIB */
   case F_chgco: c = a;                            /* RES      := val   */
                 (PT(B+Wg[Gn_currco]))[0] = Wp[0]; /* currco!0 := !p    */
                 (PT(B+Wg[Gn_currco]))[6] = h;     /* currco!6 := h    */
                 pc = Wp[1];                       /* pc       := p!1   */
                 Wg[Gn_currco] = Wp[4];            /* currco   := cptr  */
                 p = (PT(B+Wp[4]))[0];             /* p        := cptr!0 */
                 h = (PT(B+Wp[4]))[6];             /* h        := cptr!6 */
                 Wp = PT(B+p);
                 goto fetch;

   case F_neg:   a = - a;          goto fetch;
   case F_not:   a = ~ a;          goto fetch;

   case F_inc1b: i=a; a = *PT(B+i)+1; *PT(B+i) = a;  goto fetch;
   case F_inc4b: i=a; a = *PT(B+i)+4; *PT(B+i) = a;  goto fetch;
   case F_dec1b: i=a; a = *PT(B+i)-1; *PT(B+i) = a;  goto fetch;
   case F_dec4b: i=a; a = *PT(B+i)-4; *PT(B+i) = a;  goto fetch;
   case F_inc1a: i=a; a = *PT(B+i); *PT(B+i) = a+1;  goto fetch;
   case F_inc4a: i=a; a = *PT(B+i); *PT(B+i) = a+4;  goto fetch;
   case F_dec1a: i=a; a = *PT(B+i); *PT(B+i) = a-1;  goto fetch;
   case F_dec4a: i=a; a = *PT(B+i); *PT(B+i) = a-4;  goto fetch;

   case F_hand:   (PT(B+a))[0] = p;
                  (PT(B+a))[1] = h;
                  (PT(B+a))[2] = pc + (SBP B)[pc];
                  h = a;
                  pc++;
                  goto fetch;

   case F_hand+1: (PT(B+a))[0] = p;
                  (PT(B+a))[1] = h;
                  i = (pc>>1) + B[pc];
                  (PT(B+a))[2] = (i<<1) + (SHP B)[i];
                  h = a;
                  pc++;
                  goto fetch;

   case F_unh:    h = (PT(B+h))[1];
                  goto fetch;

   case F_raise:  /* The three args of RAISE are in A B and C  */ 
                  if (h==0) { res = 6; goto ret; }
                  i = h;
                  p  = (PT(B+i))[0];
                  h  = (PT(B+i))[1];
                  pc = (PT(B+i))[2];
                  Wp = PT(B+p);
                  (PT(B+i))[0] = a;
                  (PT(B+i))[1] = b;
                  (PT(B+i))[2] = c;
                  goto fetch;
   }

negpc:
   res = 4;  /* negative pc  */ 
ret:
   (PT(B+regs))[0]  = a;    /* Save the machine registers  */
   (PT(B+regs))[1]  = b;
   (PT(B+regs))[2]  = c;
   (PT(B+regs))[3]  = p;
   (PT(B+regs))[4]  = g;
   (PT(B+regs))[5]  = st;
   (PT(B+regs))[6]  = pc;
   (PT(B+regs))[7]  = count;
   (PT(B+regs))[8]  = h;
   
   return res;
}

