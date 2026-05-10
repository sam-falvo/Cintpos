@
@  BCPL library - DJA 08 June 2016
@

@-------------------------------------------------------------------------------

                .text   

                .global _ALIB
                .global main
                
                .extern _BCPL
                .extern printf
                .extern malloc
                .extern free
                .extern fgetc
                .extern fputc
                .extern fopen
                .extern fclose
                .extern exit
                .extern stdin
                .extern stdout
                .extern sigemptyset
                .extern sigaction
                .extern system
                .extern gettimeofday
                
                .equ    stacksize, 20000
                .equ    gvsize,    1000
				.equ    svsize,    1000
				
                .equ    gn_div, -1              @ signed division
                .equ    gn_switch, -2           @ switchon code
                .equ    gn_staticaddr, -3       @ get address of a static variable
                
                .equ    gn_start, 1             @ blib globals
                .equ    gn_stop, 2
                .equ    gn_sys, 3
                .equ    gn_muldiv, 5
                .equ    gn_changeco, 6
                .equ    gn_currco, 7
                .equ    gn_result2, 10
                .equ    gn_cis, 12
                .equ    gn_cos, 13
                .equ    gn_level, 15
                .equ    gn_longjump, 16
                .equ    gn_backtrace, 26
                .equ    gn_freevec, 27
                .equ    gn_abort, 28
                .equ    gn_selectinput, 56
                .equ    gn_selectoutput, 57

                .equ    SA_SIGINFO,     4       @ signals.h constants
                .equ    SIGSEGV,        11
        		.equ    sa_flags,       132
	        	.equ    sa_mask,        4
		        .equ    sa_sigaction,   0

rg              .req    r10                     @ BCPL global vector  
rp              .req    r11                     @ BCPL stack pointer

_ALIB:

@-------------------------------------------------------------------------------

@ 32-bit DIV and REM from 'ARM System Developer's Guide'
@ Copyright (c) 2003, Andrew N. Sloss, Dominic Symes, Chris Wright
@ All rights reserved.

@ only uses registers r0-r4, with numerator in r1 and denominator in r0

d               .req    r0                      @ input denominator d, output quotient
r               .req    r1                      @ input numerator n, output remainder
t               .req    r2                      @ scratch register
q               .req    r3                      @ current quotient
sign            .req    r4

                .word   (_ALIBEND-_ALIB)/4
udiv:           
                mov     q, #0                   @ zero quotient
                rsbs    t, d, r, lsr#3          @ if ((r>>3)>=d) C=1 else C=0
                bcc     div_3bits               @ quotient fits in 3 bits
                rsbs    t, d, r, lsr#8          @ if ((r>>8)>=d) C=1 else C=0
                bcc     div_8bits               @ quotient fits in 8 bits
                mov     d, d, lsl#8             @ d = d*256
                orr     q, q, #0xFF000000       @ make div_loop iterate twice
                rsbs    t, d, r, lsr#4          @ if ((r>>4)>=d) C=1 else C=0
                bcc     div_4bits               @ quotient fits in 12 bits
                rsbs    t, d, r, lsr#8          @ if ((r>>8)>=d) C=1 else C=0
                bcc     div_8bits               @ quotient fits in 16 bits
                mov     d, d, lsl#8             @ d = d*256
                orr     q, q, #0x00FF0000       @ make div_loop iterate 3 times
                rsbs    t, d, r, lsr#8          @ if ((r>>8)>=d)
                movcs   d, d, lsl#8             @ { d = d*256
                orrcs   q, q, #0x0000FF00       @ make div_loop iterate 4 times}
                rsbs    t, d, r, lsr#4          @ if ((r>>4)<d)
                bcc     div_4bits               @   r/d quotient fits in 4 bits
                rsbs    t, d, #0                @ if (0 >= d)
                bcs     div_by_0                @   goto divide by zero trap
@ fall through to the loop with C=0
div_loop:       
                movcs   d, d, lsr#8             @ if (next loop) d = d/256
div_8bits:      @ calculate 8 quotient bits
                rsbs    t, d, r, lsr#7          @ if ((r>>7)>=d) C=1 else C=0
                subcs   r, r, d, lsl#7          @ if (C) r -= d<<7
                adc     q, q, q                 @ q=(q<<1)+C
                rsbs    t, d, r, lsr#6          @ if ((r>>6)>=d) C=1 else C=0
                subcs   r, r, d, lsl#6          @ if (C) r -= d<<6
                adc     q, q, q                 @ q=(q<<1)+C
                rsbs    t, d, r, lsr#5          @ if ((r>>5)>=d) C=1 else C=0
                subcs   r, r, d, lsl#5          @ if (C) r -= d<<5
                adc     q, q, q                 @ q=(q<<1)+C
                rsbs    t, d, r, lsr#4          @ if ((r>>4)>=d) C=1 else C=0
                subcs   r, r, d, lsl#4          @ if (C) r -= d<<4
                adc     q, q, q                 @ q=(q<<1)+C
div_4bits:      @ calculate 4 quotient bits
                rsbs    t, d, r, lsr#3          @ if ((r>>3)>=d) C=1 else C=0
                subcs   r, r, d, lsl#3          @ if (C) r -= d<<3
                adc     q, q, q                 @ q=(q<<1)+C
div_3bits:      @ calculate 3 quotient bits
                rsbs    t, d, r, lsr#2          @ if ((r>>2)>=d) C=1 else C=0
                subcs   r, r, d, lsl#2          @ if (C) r -= d<<2
                adc     q, q, q                 @ q=(q<<1)+C
                rsbs    t, d, r, lsr#1          @ if ((r>>1)>=d) C=1 else C=0
                subcs   r, r, d, lsl#1          @ if (C) r -= d<<1
                adc     q, q, q                 @ q=(q<<1)+C
                rsbs    t, d, r                 @ if (r>=d) C=1 else C=0
                subcs   r, r, d                 @ if (C) r -= d
                adcs    q, q, q                 @ q=(q<<1)+C; C=old q bit 31
div_next:       
                bcs     div_loop                @ loop if more quotient bits
                mov     r0, q                   @ r0 = quotient; r1=remainder
                mov     pc,lr                   @ return { r0, r1 } structure
div_by_0:       
                mov     r0, #-1
                mov     r1, #-1
                mov     pc,lr                   @ return { r0, r1 } structure

@-------------------------------------------------------------------------------
sdiv:                                           @ numerator in r1, denominator in r0
                push    {r2-r5,lr}              @ preserve r2,r3,r4,r5,lr
                mov     r4,#0                   @ sign
                mov     r5,r1                   @ remember sign of numerator
                movs    r1,r1                   @ check sign of denominator
                rsbmi   r1,r1,#0                @ if (denominator<0) denominator=-denominator
                mvnmi   r4,r4                   @ reverse sign
                movs    r0,r0                   @ check sign of numerator
                rsbmi   r0,r0,#0                @ if (numerator<0) numerator=-numerator
                mvnmi   r4,r4                   @ reverse sign
                bl      udiv                    @ (r0,r1)=(r1/r0,r1%r0)
                movs    r4,r4                   @ check sign of quotient
                rsbne   r0,r0,#0                @ if (sign!=0) negate result
                eors    r5,r1                   @ check if sign numerator = sign remainder
                rsblt   r1,r1,#0                @ if not remainder=-remainder
                pop     {r2-r5,pc}              @ restore r2,r3,r4,r5 and return

@-------------------------------------------------------------------------------
@ 64-bit/32-bit division based on algorithm by Conrad Hughes

muldiv:                                         @ a*b/c
                mov     r4,r2                   @ remember c
                mov     r5,#0                   @ count of sign reversals
                smulls  r2, r3, r0, r1          @ 64-bit result in r3:r2
                mov     r6,r3                   @ numerator msw
                bpl     md1                     @ numerator must be positive
                rsbs    r2,r2,#0                @ negate numerator double
                rsc     r3,r3,#0
                add     r5,#1                   @ sign reversal
md1:
                movs    r4,r4                   @ denominator must be negative
                bmi     md2
                rsb     r4,r4,#0                @ negate denominator
                add     r5,#1                   @ sign reversal
md2:            
                adds    r2,r2,r2

                .macro  divm                    @ macro for inner loop 
                adcs    r3,r4,r3,lsl #1
                subcc   r3,r3,r4
                adcs    r2,r2,r2
                .endm
                
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    
                divm    

                mov     r0,r2                   @ result in r2
                ands    r5,#1                   @ negate r0 iff one sign reversal 
                rsbeq   r0,r0,#0
                eors    r6,r3                   @ sign of remainder and numerator must be same
                rsbmi   r3,r3,#0                @ if not negate the remainder
                str     r3,[rg,#gn_result2*4]   @ remainder in result2
                mov     pc, lr                  @ return

@-------------------------------------------------------------------------------
@ on entry
@     r0 byte offset to the static variable
@ on exit
@ 	  r0 m/c addresses of the static variable 

staticaddr:     push    {r2,r3}                 @ save regs
                ldr     r2,=svec                @ get static region address
st1:            ldr     r3,[r2],#8              @ get next base address and bump r2 by 8
                cmp     lr,r3                   @ running in previous segment?
                blo     st1
                ldr     r3,[r2,#-4]             @ ptr to statics
				add     r0,r0,r3                @ include offset to variable
				pop     {r2,r3}                 @ restore regs
				mov     pc,lr                   @ return

@-------------------------------------------------------------------------------
@
@ on entry: 
@     r0 key
@     r1 cluster offset in hash table (bytes) 
@     r2 hash_table_size-4 in bytes
@     lr address of the hash table
@ registers r0-r9 are available

switch:         add     r1,r1,lr                @ cluster address
sw1:            ldr     r3,[r1],#4              @ try hash table entry and bump r1 by 4
                cmp     r0,r3                   @ key found?
                beq     swfound
                cmp     r3,#0                   @ zero entry?
                bne     sw1                     @ default case

swfound:        ldr     r1,[r1,r2]              @ get label offset (relative to lr)
                add     pc,lr,r1                @ go
                
@-------------------------------------------------------------------------------
level:          mov     r0,rp                   @ level()
                mov     pc,lr                   @ return

@-------------------------------------------------------------------------------
longjump:       mov     rp,r0                   @ longjump(lev, lab)
                mov     pc,r1                   @ jump

@-------------------------------------------------------------------------------
selectinput:    cmp     r0,#0                   @ selectinput(s)
                bne     selectin1               @ stdin?
                ldr     r0,=stdin               @ get stdin from libc
                ldr     r0,[r0,#0]
selectin1:      str     r0,[rg,#gn_cis*4]       @ set cis
                mov     pc,lr                   @ return
                
@-------------------------------------------------------------------------------
selectoutput:   cmp     r0,#0                   @ selectoutput(s)
                bne     selectout1              @ stdout?
                ldr     r0,=stdout              @ get stdout from libc
                ldr     r0,[r0,#0]
selectout1:     str     r0,[rg,#gn_cos*4]       @ set cos
                mov     pc,lr                   @ return
                
@-------------------------------------------------------------------------------
changeco:                                       @ changeco(val, cptr)

                                                @ r0 = val
                                                @ r1 = cptr (a BCPL pointer)

                mov     r2,#0
                ldr     r3,[rg, #gn_currco*4]   @ c := currco     -- a BCPL pointer
                str     rp,[r2, r3, lsl#2]      @ currco!0 := P   -- save the resumption point
                str     r1,[rg, #gn_currco*4]   @ currco := cptr  -- set current coroutine
                ldr     rp,[r2, r1, lsl #2]     @ P := !cptr      -- get the resumption point
                mov     pc,lr                   @ return val (in r0)

@-------------------------------------------------------------------------------
@ sys calls from BCPL: conversions to/from BCPL/C addresses done in blib
@ rp, rg and lr regs are saved before the call and restored on return
@-------------------------------------------------------------------------------
sys:            push    {rg,rp,lr}              @ save BCPL regs on system stack

                ldr     r4,=brtab               @ computed goto
                cmp     r0,#0                   @ check validity 1..8
                blt     sysexit
                cmp     r0,#8
                bgt     sysexit
                add     r4,r0,lsl #2
                mov     pc,r4

brtab:          b       sysexit                 @ sys(0) not used
                b       sys1
                b       sys2
                b       sys3
                b       sys4
                b       sys5
                b       sys6
                b       sys7
                b       sys8
                
sys1:           mov     r0,r1                   @ malloc(n)
                bl      malloc
                b       sysexit

sys2:           mov     r0,r1                   @ free(n)
                bl      free
                b       sysexit

sys3:           mov     r0,r1                   @ fgetc(s)
                bl      fgetc
                b       sysexit
                
sys4:           mov     r0,r1                   @ fputc(ch, s)
                mov     r1,r2
                bl      fputc
                b       sysexit
                
sys5:           mov     r0,r1                   @ fopen(filename, "rb" or "wb")
                mov     r1,r2
                bl      fopen
                b       sysexit
                
sys6:           mov     r0,r1                   @ fclose(s)
                bl      fclose
                b       sysexit

sys7:           mov     r0,r1                   @ system(str)
                mov     r0,r0, lsl #2           @ m/c address arg
                bl      system
                b       sysexit

sys8:           mov     r0,r1, lsl #2           @ gettimeofday(v)
                mov     r1,#0
                bl      gettimeofday
@               b       sysexit
                
sysexit:        pop     {rg,rp,pc}              @ restore BCPL env & return
                
@-------------------------------------------------------------------------------
main:
                push    {r10-r12,lr}            @ called from C
                push    {r0, r1}                @ save argc and argv
				
@ before we start, call sigaction to trap memory access errors

                add     fp,sp,#4                @ C entry
				sub     sp,sp,#20
				
                ldr     r0,=sigv                @ sigv.sa_flags = SA_SIGINFO
		        mov     r1,#SA_SIGINFO
	        	str     r1,[r0,#sa_flags]
		        add     r0,r0,#sa_mask
                bl      sigemptyset             @ sigemptyset(&sigv.sa_mask)

                ldr     r1,=sigv
		        ldr     r2,=exception
		        str     r2,[r1,#sa_sigaction]   @ sigv.sa_sigaction = exception
	        	mov     r0,#SIGSEGV
		        mov     r2,#0
                bl      sigaction               @ r0 = sigaction(SIGSEGV, &sigv, NULL)

                sub     sp,fp,#4                @ reset sp to entry state

                ldr     rp,=stack               @ rp points to the BCPL stack
                ldr     rg,=gvec                @ rg points to the global vector

@ chain though the sections to find blib
@ order of sections: leader, prog(s), blib, alib
                ldr     r0,=_BCPL               @ search from here for first BCPL section
                ldr     r4,=_ALIB               @ stop at _ALIB
                mov     r2,r0                   @ start of first section
f1:             mov     r5,r2                   @ remember address of previous section
                ldr     r1,[r2,#0]              @ section size words
                mov     r1,r1,lsl #2            @ section size bytes
                add     r2,r2,r1                @ address of next section
                cmp     r2,r4                   @ check upper limit
                bne     f1                      @ chain on to next section

@ r0 points to the start of the first BCPL section
@ r5 points to the BLIB section
                mov     r0,r0,lsr #2            @ BCPL address of first section
                mov     r1,rg,lsr #2            @ BCPL address of global vector
                mov     r2,#gvsize              @ maximum size of global vector in words
                ldr     r3,=svec                @ static vector
				mov     r3,r3,lsr #2            @ BCPL address
                add     r4,rp,#16               @ set up call to setgv
                add     r5,#36                  @ offset to first procedure
                blx     r5                      @ setgv() must be first procedure in blib
                
                ldr     r0,=stdin               @ get stdin from libc
                ldr     r0,[r0,#0]
                str     r0,[rg,#gn_cis*4]       @ set cis
                ldr     r0,=stdout              @ get stdout from libc
                ldr     r0,[r0,#0]
                str     r0,[rg,#gn_cos*4]       @ set cos

                pop     {r0,r1}                 @ retrieve command line argugents
                add     r4,rp,#12               @ next frame pointer
                ldr     r2,[rg,#gn_start*4]
                blx     r2                      @ BCPL start()
BCPL_exit:
                mov     r0,#0                   @ return code 0
                pop     {r10-r12,pc}            @ return to C
				
exception:      @ only get here from a run-time memory violation
                @ r1 points to siginfo_t
				
                mvn     r0,#0                   @ backtrace -1 means trap
@				ldr     r1,=sigv
                add     r4,rp,#12               @ set up call
                ldr     r2,[rg,#gn_backtrace*4]
                blx     r2                      @ no return from backtrace(-1, address)

abort:          mvn     r0,#0                   @ return code -1
                bl      exit                    @ call C exit

@-------------------------------------------------------------------------------
@ no statics
                .word   0

@-------------------------------------------------------------------------------
@               BCPL global vector

                .word   0
                .word   gn_stop
                .word   BCPL_exit-_ALIB
                .word   gn_div
                .word   sdiv-_ALIB
                .word   gn_switch
                .word   switch-_ALIB
                .word   gn_staticaddr
				.word   staticaddr-_ALIB
                .word   gn_muldiv
                .word   muldiv-_ALIB
                .word   gn_level
                .word   level-_ALIB
                .word   gn_longjump
                .word   longjump-_ALIB
                .word   gn_selectinput
                .word   selectinput-_ALIB
                .word   gn_selectoutput
                .word   selectoutput-_ALIB
                .word   gn_changeco
                .word   changeco-_ALIB
                .word   gn_sys
                .word   sys-_ALIB
                .word   gn_abort
                .word   abort-_ALIB
                .word   100                     @ max global

@------------------------------------------------------------------------------

_ALIBEND:
                .word   0                       @ extra zero - terminator for last module
                
                .ltorg
                .data
                
                .word   0                       @ global -3: get static variable address
                .word   0                       @ global -2: switch
                .word   0                       @ global -1: sdiv
gvec:
                .space  gvsize*4                @ global vector
sigv:
                .space  140                     @ struct sigaction
svec:
                .space  svsize*4                @ static vector
stack:
                .space  stacksize*4             @ BCPL stack
                
                .end
