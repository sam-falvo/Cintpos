# This is an assembly laguage version of the Cintcode interpreter
# modified for Cintpos by the addition of an interrupt mechanism.

# Linkage:
#   On entry 0(%esp)   is the return address
#            4(%esp)   is the first argument
#            8(%esp)   is the second argument
#            etc
#
#   %esp must be word aligned and point to the stack's bottom most
#        word of valid data
#   %ebp, %ebx, %edi, %esi and %esp must be preserved
#   %eax, $ecx, %edx
#
#   result in %eax
#
#   flag DF must be clear on entry and exit
#
#  Example:	f(a,b,c,d)
#
#  pushl d
#  pushl c
#  pushl b
#  pushl a
#  call  f     --->       ..
#                         ..
#              <---      ret
#  At this point %eax holds the result, if any,
#  and %esp has the value it had just before 'call f'
#
	

.text
 .align 4
#
# Define both cintasm and _cintasm so that it works on
# all versions of Linux.
# You may need to change the external reference dosys to _dosys
# (it occurs just once). This is needed for old versions of Linux
# and FreeBSD

.data

.globl cintasm
.globl _cintasm
cintasm:
_cintasm:
 pushl %ebp
 pushl %ebx
 pushl %edi
 pushl %esi
 subl $40,%esp
 movl 60(%esp),%eax      #  regs (first  argument) BCPL ptr to regs
 movl 64(%esp),%edi      #  mem  (second argument) m/c addr cintcode mem
	
# code to set the registers

 leal (%edi,%eax,4),%eax # get m/c addr of regs
 movl %eax,32(%esp)      # store m/c addr of regs vector

 movl 4*1(%eax),%ecx     # Cintcode B register
 movl 4*2(%eax),%ebx
 movl %ebx,28(%esp)      # Cintcode C register
 movl 4*3(%eax),%ebp
 addl %edi,%ebp          # m/c addr of P0
 movl 4*4(%eax),%ebx
 leal (%ebx,%edi),%edx
 movl %edx,36(%esp)      # save m/c address of G0
 shrl $2,%ebx
 movl %ebx,16(%esp)      # save BCPL address of G0
 movl 4*5(%eax),%ebx
 movl %ebx,24(%esp)      # cintcode ST
 movl 4*6(%eax),%esi
 addl %edi,%esi          # PC as m/c address
 movl 4*7(%eax),%ebx
 movl %ebx,20(%esp)      # Count register
 movl (%eax),%ebx        # A

 jmp fetchz


# Register usage at the moment of fetching the next
# Cintcode instruction.

# %eax  zero, except for least significant byte (%al)
# %ebx  Cintcode A
# %ecx  Cintcode B
# %edx  m/c address of G0
# %ebp  m/c address of P0
# %edi  m/c addr of cintcode memory
# %esi  Cintcode pc as m/c address
# %esp  points to cintasm local work space
#    64(%esp)   mem  -- m/c addr of cintcode memory
#    60(%esp)   regs -- bcpl pointer regs vector
#    56(%esp)   return address
#    52(%esp)   caller's %ebp
#    48(%esp)   caller's %ebx
#    44(%esp)   caller's %edi
#    40(%esp)   caller's %esi
#    36(%esp)   m/c addr of G0
#    32(%esp)   m/c addr of regs vector
#    28(%esp)   cintcode C reg
#    24(%esp)   cintcode ST
#    20(%esp)   cintcode Count (known to be <0 for cintasm)
#    16(%esp)   BCPL pointer to G0
#    12(%esp) ) Local
#    ...      )     scratch
#    OO(%esp) )          workspace

# The flag DF is clear, so that lodsb etc increment %esi

indjump:
 movb (%esi),%al
 leal (%esi,%eax,2),%eax       # %eax := pc + 2*B[pc]
 andb $254,%al                 # %eax &= #xFFFFFFFE
 movswl (%eax),%esi
 addl %eax,%esi                # %esi := %eax + SH[%eax]
 jmp fetchz
	
# Begin executing the Cintcode instructions

nojump:                     # eax is known to be 000000hh
 incl %esi                  # pc++

fetchz:                     # eax not known to be 000000hh
fetch:                      # eax is known to be 000000hh
# Check watch address
   cmpl $0,watchaddr
   je fetch1                # J if no watch address
   movl watchaddr,%eax
   movl (%eax),%eax
   cmpl watchval,%eax
   je fetch1                # J if value has not changed

   movl watchaddr,%eax
   subl %edi,%eax
   sarl $2,%eax
   movl %eax,4(%edi)        # W!1 := watchaddr - W
   movl watchval,%eax
   movl %eax,8(%edi)        # W!2 := watchval

   movl $11,%eax
   jmp ret1                 # Return with code 11
   
fetch1:                     # Check for interrupt
   jmp fetch3 #???????????????????????????????????????????
   cmpl $0,irq
   je fetch3                # J if no interrupt

   cmpl $0,24(%esp)
   jne fetch3               # J if interrupts disabled

      movl $125,%eax
   jmp ret1

   # Dispatch an interrupt
	
   movl $irq_mutex,%eax
   pushl %eax
   call pthread_mutex_lock
   addl $4,%esp
   movl 36(%esp),%edx       # Restore G address


   movl $0,irq

   movl irqfifop,%eax
   cmpl irqfifoq,%eax
   je fetch2

   movl irqfifop,%eax
   leal	0(,%eax,4),%edx
   movl	$irqfifov, %eax
   movl	(%edx,%eax), %eax   # devid = irqfifov[irqfifp]
	
   incl	irqfifop
   andl	$1023, irqfifop

# now save the registers in saveregs (=21)
 movl %eax,(%esp)           # save devid temporarily

 movl %ebx,4*(21+0)(%edi)   # A
 movl %ecx,4*(21+1)(%edi)   # B
 movl 28(%esp),%ebx
 movl %ebx,4*(21+2)(%edi)   # C
 subl %edi,%ebp
 movl %ebp,4*(21+3)(%edi)   # P
                    # G cannot have changed
	
 movl 24(%esp),%ebx
 movl %ebx,14*(21+5)(%edi)  # ST
 subl %edi,%esi
 movl %esi,4*(21+6)(%edi)   # PC
 movl 20(%esp),%ebx
 movl %ebx,4*(21+7)(%edi)   # Count
	

 # Now set the registers from regsint (=31)

 movl (%esp),%ebx           # A = devid
 movl $0, %ecx              # B = 0
 movl $0,28(%esp)           # C = 0
 movl 4*(31+3)(%edi),%ebp
 addl %edi,%ebp             # P = regsint!3
 movl 4*(31+4)(%edi),%eax
 leal (%eax,%edi),%edx      # G = regsint!4
 movl %edx,36(%esp)         # save m/c address of G
 shrl $2,%eax
 movl %eax,16(%esp)         # save BCPL address of G0
 movl $3,24(%esp)           # ST = 3
 movl 4*(31+6)(%edi),%esi
 addl %edi,%esi             # PC as m/c address
                            # Don't set Count
 movl %ebx,4*3(%ebp)        # P!3 := A    (BCPL calling convention)
 jmp fetchz

 movl $irq_mutex,%eax
 pushl %eax
 call pthread_mutex_unlock  # pthread_mutex_unlock(&irq_mutex);
 addl $4,%esp
 movl 36(%esp),%edx         # Restore G address
 jmp fetch                  # Start executing the interrupt routine


fetch2:                     # There was no interrupt
   movl $irq_mutex,%eax     # so unlock and resume normal execution
   pushl %eax
   call pthread_mutex_unlock
   addl $4,%esp
   movl 36(%esp),%edx       # Restore G address


fetch3:
	cmpl	$0, tracing
	je	notrace
	
        pushl   %eax   # Save %eax   -- save absolutely everything
        pushl   %ebx   # Save %ebx   -- for safety sake
        pushl   %ecx   # Save %ecx
        pushl   %edx   # Save %edx
        pushl   %ebp   # Save %ebp
        pushl   %edi   # Save %edi
        pushl   %esi   # Save %esi

	pushl	%ecx         # b     -- arg 4
	pushl	%ebx         # a     -- arg 3
   
        movl    %ebp,%edx
        subl    %edi,%edx
        shrl    $2,%edx
	pushl	%edx         # p     -- arg 2
   
        movl    %esi,%eax
        subl    %edi,%eax
	pushl	%eax         # pc    -- arg1
   
	call	trace        # if (tracing) trace(pc, p, a, b);
	addl	$16,%esp     # remove the four arguments from the stack

        popl    %esi   # Restore %esi  -- retore absolutely everything
        popl    %edi   # Restore %edi
        popl    %ebp   # Restore %ebp
        popl    %edx   # Restore %edx
        popl    %ecx   # Restore %ecx
        popl    %ebx   # Restore %ebx
        popl    %eax   # Restore %eax

notrace:
   
 movzbl 0(%esi),%eax        # Get the Cintcode function byte
 incl %esi                  # Increment the Cintcode pc
 jmp *runtbl(,%eax,4)       # Jump to the action routine

ret1:                       # Return from cintasm with result %eax
 movl %eax,(%esp)           # Save the result temporarily
 movl 32(%esp),%eax         # Get m/c addr of the regs vector

 movl %ebx,(%eax)    # A
 movl %ecx,4*1(%eax) # B
 movl 28(%esp),%ebx  #   -- note that it is now safe to use %ebp
 movl %ebx,4*2(%eax) # C
 subl %edi,%ebp
 movl %ebp,4*3(%eax) # P
 subl %edi,%edx
 movl %edx,4*4(%eax) # G
 movl 24(%esp),%ebx
 movl %ebx,4*5(%eax) # ST
 subl %edi,%esi
 movl %esi,4*6(%eax) # PC
 movl 20(%esp),%ebx
 movl %ebx,4*7(%eax) # Count
 movl (%esp),%eax    # recover return code

# and then return
 addl $40,%esp
 popl %esi
 popl %edi
 popl %ebx
 popl %ebp
 ret

negpc:
 movl $4,%eax
 jmp ret1

# frq=nnn  give the frequency of execution of each
#          cintcode operation when the bcpl compiler
#          compiles itself

rl0:   # Error     frq=0
rl1:   # Error     frq=0
 decl %esi
 movb $1,%al
 jmp ret1

rl2:   # brk     frq=0
 decl %esi
 movb $2,%al
 jmp ret1

rl3:   # k3     frq=3002
rl4:   # k4     frq=7738
rl5:   # k5     frq=4520
rl6:   # k6     frq=1
rl7:   # k7     frq=480
rl8:   # k8     frq=10
rl9:   # k9     frq=0
rl10:  # k10    frq=0
rl11:  # k11    frq=28
 leal (%ebp,%eax,4),%eax
 subl %edi,%ebp
 movl %ebp,(%eax)        # p[k] := p
 movl %eax,%ebp          # p := p+k
 subl %edi,%esi
 movl %esi,4(%ebp)       # p[1] := pc
 movl %ebx,8(%ebp)       # p[2] := a  (the new pc)
 leal (%edi,%ebx),%esi   # pc := a (new pc as m/c address)
 movl %ecx,%ebx          # a := b
 movl %ebx,12(%ebp)      # p[3] := a
 orl %esi,%esi
 js negpc
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl12:  # lf      frq=11729
 movl %ebx,%ecx             # b := a
 movsbl (%esi),%ebx         # a := pc + SB[pc]
 addl %esi,%ebx
 subl %edi,%ebx
 incl %esi                  # pc++
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl13:  # lf$     frq=4338
 movl %ebx,%ecx             # b := a
 movl %esi,%ebx
 andb $254,%bl              # a := pc & #xFFFFFFFE
 lodsb
 leal (%ebx,%eax,2),%ebx    # a := a + 2*B[pc++]
 movswl (%ebx),%eax
 addl %eax,%ebx             # a := a + SH[a]
 subl %edi,%ebx
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl14:  # lm      frq=598248
 movl %ebx,%ecx             # b := a
 movzbl (%esi),%ebx         # a := - B[pc++]
 incl %esi
 negl %ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl15:  # lm1     frq=163150
rl16:  # l0      frq=244421
rl17:  # l1      frq=740929
rl18:  # l2      frq=38430
rl19:  # l3      frq=49815
rl20:  # l4      frq=83246
rl21:  # l5      frq=1224
rl22:  # l6      frq=54857
rl23:  # l7      frq=8683
rl24:  # l8      frq=13745
rl25:  # l9      frq=96877
rl26:  # l10     frq=253528
 movl %ebx,%ecx
 leal -16(%eax),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl27:  # fhop     frq=2295
 xorl %ebx,%ebx       # a := 0
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl28:  # jeq     frq=758839
 cmpl %ebx,%ecx
 je jeq1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)
jeq1:
 movsbl (%esi),%eax
 addl %eax,%esi
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl29:  # jeq$     frq=0
 cmpl %ebx,%ecx
 jne nojump
 jmp indjump

rl30:  # jeq0     frq=253477
 orl %ebx,%ebx
 je jeq1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl31:  # jeq0$     frq=2926
 orl %ebx,%ebx
 jne nojump
 jmp indjump

rl32:  # k     frq=35172
 lodsb
callk:
 leal (%ebp,%eax,4),%eax
 subl %edi,%ebp
 movl %ebp,(%eax)        # p[k] := p
 movl %eax,%ebp          # p := p+k
 subl %edi,%esi
 movl %esi,4(%ebp)       # p[1] := pc
 movl %ebx,%esi          # pc := a
 movl %esi,8(%ebp)       # p[2] := pc
 movl %ecx,%ebx          # a := b
 movl %ebx,12(%ebp)      # p[3] := a
 addl %edi,%esi          # make pc a m/c address
 js  negpc
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl33:  # kh     frq=0
 lodsw
 jmp callk

rl34:  # kw     frq=0
 lodsl
 jmp callk

rl35:  # k3g      frq=204783
rl36:  # k4g      frq=135269
rl37:  # k5g      frq=263407
rl38:  # k6g      frq=13337
rl39:  # k7g      frq=4992
rl40:  # k8g      frq=4660
rl41:  # k9g      frq=1689
rl42:  # k10g     frq=310
rl43:  # k11g     frq=9
 leal -4*32(%ebp,%eax,4),%eax
 subl %edi,%ebp
 movl %ebp,(%eax)           # p[k] := p
 movl %eax,%ebp             # p := p+k
 movzbl (%esi),%eax         # n := B[pc]
 incl %esi                  # pc++
 subl %edi,%esi
 movl %esi,4(%ebp)          # p[1] := pc
 movl (%edx,%eax,4),%esi    # pc := g!n
 movl %esi,8(%ebp)          # p[2] := pc
 movl %ebx,12(%ebp)         # p[3] := a
 addl %edi,%esi             # make pc a m/c address
 js negpc
   jne fetchz
   movl $555,%eax
   jmp ret1
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl44:  # s0g     frq=401845
 movb (%esi),%al # n := B[pc]
 incl %esi              # pc++
 movl (%edx,%eax,4),%eax
 movl %ebx,(%edi,%eax,4)  # g!n!0 := a
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl45:  # l0g     frq=411817
 movl %ebx,%ecx
 movb (%esi),%al # n := B[pc]
 incl %esi              # pc++
 movl (%edx,%eax,4),%ebx
 movl (%edi,%ebx,4),%ebx  # a := g!n!0
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl46:  # l1g     frq=409459
 movl %ebx,%ecx
 movb (%esi),%al # n := B[pc]
 incl %esi              # pc++
 movl (%edx,%eax,4),%ebx
 movl 4*1(%edi,%ebx,4),%ebx # a := g!n!1
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl47:  # l2g     frq=3
 movl %ebx,%ecx
 lodsb
 movl (%edx,%eax,4),%ebx
 movl 4*2(%edi,%ebx,4),%ebx # a := g!n!2
 jmp fetch

rl48:  # lg     frq=1365544
 movl %ebx,%ecx             # b := a
 movb (%esi),%al            # n := B[pc]
 incl %esi                  # pc++
 movl (%edx,%eax,4),%ebx    # a := g!n
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl49:  # sg     frq=324122
 movb (%esi),%al            # n := B[pc]
 incl %esi                  # pc++
 movl %ebx,(%edx,%eax,4)    # g!n := a
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl50:  # llg     frq=0
 movl %ebx,%ecx
 lodsb
 movl 16(%esp),%ebx
 addl %eax,%ebx
 jmp fetch

rl51:  # ag     frq=7
 lodsb
 addl (%edx,%eax,4),%ebx
 jmp fetch

rl52:  # mul     frq=132122
 movl %ecx,%eax
 imul %ebx
 movl %eax,%ebx
 movl 36(%esp),%edx      # retore m/c addr of G0
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl53:  # div     frq=74675
 cmpl $0,%ebx
 je diverr
 movl %ecx,%eax
 cdq
 idiv %ebx
 movl %eax,%ebx
 movl 36(%esp),%edx      # retore m/c addr of G0
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

diverr:
 decl %esi
 movl $5,%eax
 jmp ret1

rl54:  # rem     frq=92754
 cmpl $0,%ebx
 je diverr
 movl %ecx,%eax
 cdq
 idiv %ebx
 movl %edx,%ebx
 movl 36(%esp),%edx      # retore m/c addr of G0
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl55:  # xor     frq=56780
 xorl %ecx,%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl56:  # sl     frq=0
 movsbl (%esi),%eax
 movl %ebx,(%esi,%eax)
 incl %esi
 jmp fetchz

rl57:  # sl$     frq=0
 lodsb
 leal -1(%esi,%eax,2),%eax   # %eax := pc + 2*B[pc]# pc++
 andb $254,%al               # %eax &= #xFFFFFFFE
 movswl (%eax),%edx
 movl %ebx,(%eax,%edx)       # W[%eax+SH[%eax]] := a
 movl 36(%esp),%edx
 jmp fetchz

rl58:  # ll     frq=0
 movl %ebx,%ecx              # b := a
 movsbl (%esi),%ebx
 movl (%esi,%ebx),%ebx       # a := pc + SB[pc]
 incl %esi                   # pc++
 jmp fetch

rl59:  # ll$     frq=0
 movl %ebx,%ecx              # b := a
 lodsb
 leal -1(%esi,%eax,2),%eax   # %eax := pc + 2*B[pc]# pc++
 andb $254,%al               # %eax &= #xFFFFFFFE
 movswl (%eax),%ebx
 movl (%eax,%ebx),%ebx       # a := W[%eax+SH[%eax]]
 jmp fetchz

rl60:  # jne     frq=412167
 cmpl %ebx,%ecx
 jne jne1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)
jne1:
 movsbl (%esi),%eax
 addl %eax,%esi
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl61:  # jne$     frq=17329
 cmpl %ebx,%ecx
 jz nojump
 jmp indjump

rl62:  # jne0     frq=673057
 orl %ebx,%ebx
 jne jne1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl63:  # jne0$     frq=8593
 orl %ebx,%ebx
 jz nojump
 jmp indjump

rl64:  # llp     frq=10685
 movl %ebx,%ecx           # b := a
 movl %ebp,%ebx           # a := p + B[pc++]
 subl %edi,%ebx
 shrl $2,%ebx
 lodsb
 addl %eax,%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl65:  # llph     frq=0
 movl %ebx,%ecx           # b := a
 movl %ebp,%ebx           # a := p + H[pc]# pc += 2
 subl %edi,%ebx
 shrl $2,%ebx
 lodsw
 addl %eax,%ebx
 jmp fetchz

rl66:  # llpw     frq=0
 movl %ebx,%ecx           # b := a
 movl %ebp,%ebx           # a := p + W[pc]# pc += 4
 subl %edi,%ebx
 shrl $2,%ebx
 lodsl
 addl %eax,%ebx
 jmp fetchz

rl67:  # k3g1     frq=62815
rl68:  # k4g1     frq=312332
rl69:  # k5g1     frq=93172
rl70:  # k6g1     frq=33086
rl71:  # k7g1     frq=56780
rl72:  # k8g1     frq=15758
rl73:  # k9g1     frq=25517
rl74:  # k10g1     frq=2673
rl75:  # k11g1     frq=3440
 leal -4*64(%ebp,%eax,4),%eax
 subl %edi,%ebp
 movl %ebp,(%eax)              # p[k] := p
 movl %eax,%ebp                # p := p+k
 movzbl (%esi),%eax            # n := B[pc]
 incl %esi                     # pc++
 subl %edi,%esi
 movl %esi,4(%ebp)             # p[1] := pc
 movl 4*256(%edx,%eax,4),%esi  # pc := g!(n+256)
 movl %esi,8(%ebp)             # p[2] := pc
 movl %ebx,12(%ebp)            # p[3] := a
 addl %edi,%esi                # make pc a m/c address
 js negpc
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl76:  # s0g1     frq=1639
 lodsb
 movl 4*256(%edx,%eax,4),%eax
 movl %ebx,(%edi,%eax,4)       # !(g!(n+256)) := a
 jmp fetchz
 
rl77:  # l0g1     frq=724
 movl %ebx,%ecx                # b := a
 lodsb
 movl 4*256(%edx,%eax,4),%ebx
 movl (%edi,%ebx,4),%ebx       # a := 0!(g!(n+256))
 jmp fetch

rl78:  # l1g1     frq=724
 movl %ebx,%ecx                # b := a
 lodsb
 movl 4*256(%edx,%eax,4),%ebx
 movl 4*1(%edi,%ebx,4),%ebx    # a := 1!(g!(n+256))
 jmp fetch

rl79:  # l2g1     frq=724
 movl %ebx,%ecx                # b := a
 lodsb
 movl 4*256(%edx,%eax,4),%ebx
 movl 4*2(%edi,%ebx,4),%ebx    # a := 2!(g!(n+256))
 jmp fetch

rl80:  # lg1     frq=249497
 movl %ebx,%ecx                # b := a
 movb (%esi),%al               # n := B[pc++]
 incl %esi
 movl 4*256(%edx,%eax,4),%ebx  # a := g!(n+256)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl81:  # sg1     frq=155081
 movb (%esi),%al               # n := B[pc++]
 incl %esi
 movl %ebx,4*256(%edx,%eax,4)  # g!(n+256) := b
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl82:  # llg1     frq=0
 movl %ebx,%ecx                # b := a
 movb (%esi),%al               # n := B[pc++]
 incl %esi
 leal 256(%eax),%ebx
 addl 16(%esp),%ebx            # a := @ g!(n+256)
 jmp fetch

rl83:  # ag1     frq=1290
 movb (%esi),%al               # n := B[pc++]
 incl %esi
 addl 4*256(%edx,%eax,4),%ebx  # a += g!(n+256)
 jmp fetch

rl84:  # add     frq=51328
 addl %ecx,%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl85:  # sub     frq=51606
 subl %ecx,%ebx
 negl %ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl86:  # lsh     frq=23772
 cmpl $31,%ebx
 setgb %al
 decl %eax
 andl %ecx,%eax
 xchgl %eax,%ebx
 xchgl %eax,%ecx
 sall %cl,%ebx   # a := b<<a
 movl %eax,%ecx
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl87:  # rsh     frq=65180
 cmpl $31,%ebx
 setgb %al
 decl %eax
 andl %ecx,%eax
 xchgl %eax,%ebx
 xchgl %eax,%ecx
 shrl %cl,%ebx   # a := b>>a
 movl %eax,%ecx
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl88:  # and     frq=192985
 andl %ecx,%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl89:  # or     frq=24123
 orl %ecx,%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl90:  # lll     frq=57746
 movl %ebx,%ecx
 movsbl (%esi),%ebx
 addl %esi,%ebx
 subl %edi,%ebx
 shrl $2,%ebx
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl91:  # lll$     frq=189
 movl %ebx,%ecx                # b := a
 lodsb
 leal -1(%esi,%eax,2),%eax     # %eax := pc + 2*B[pc]# pc++
 andb $254,%al                 # %eax &= #xFFFFFFFE
 movswl (%eax),%ebx
 addl %eax,%ebx                # a := %eax+SH[%eax]
 subl %edi,%ebx
 shrl $2,%ebx                  # a >>= 2
 jmp fetchz

rl92:  # jls     frq=293452
 cmpl %ebx,%ecx
 jl jls1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)
jls1:
 movsbl (%esi),%eax
 addl %eax,%esi
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl93:  # jls$     frq=6421
 cmpl %ebx,%ecx
 jge nojump
 jmp indjump

rl94:  # jls0     frq=3102
 orl %ebx,%ebx
 jl jls1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl95:  # jls0$     frq=0
 orl %ebx,%ebx
 jge nojump
 jmp indjump

rl96:  # l     frq=1098722
 movl %ebx,%ecx               # b := a
 movzbl (%esi),%ebx           # a := B[pc++]
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl97:  # lh     frq=84529
 movl %ebx,%ecx         # b := a
 movzwl (%esi),%ebx
 addl $2,%esi           # a := H[pc]; pc += 2 
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl98:  # lw     frq=0
 movl %ebx,%ecx         # b := a
 lodsl
 movl %eax,%ebx         # a := W[pc]; pc += 4
 jmp fetchz

rl99:  # k3gh     frq=320
rl100: # k4gh     frq=4633
rl101: # k5gh     frq=6808
rl102: # k6gh     frq=8806
rl103: # k7gh     frq=9358
rl104: # k8gh     frq=17
rl105: # k9gh     frq=0
rl106: # k10gh     frq=169
rl107: # k11gh     frq=0
 leal -4*96(%ebp,%eax,4),%eax
 subl %edi,%ebp
 movl %ebp,(%eax)         # p[k] := p
 movl %eax,%ebp           # p := p+k
 movzwl (%esi),%eax       # n := H[pc]
 addl $2,%esi             # pc += 2
 subl %edi,%esi
 movl %esi,4(%ebp)        # p[1] := pc
 movl (%edx,%eax,4),%esi  # pc := g!n
 movl %esi,8(%ebp)        # p[2] := pc
 movl %ebx,12(%ebp)       # p[3] := a
 addl %edi,%esi           # make pc a m/c address
 js negpc
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl108: # s0gh     frq=15601
 lodsw
 movl (%edx,%eax,4),%eax
 movl %ebx,(%edi,%eax,4)        # 0!(g!(H[pc])) := a; pc += 2
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl109: # l0gh     frq=9924
 movl %ebx,%ecx                 # b := a
 lodsw
 movl (%edx,%eax,4),%eax
 movl (%edi,%eax,4),%ebx        # a := 0!(g!(H[pc])); pc += 2
 jmp fetchz

rl110: # l1gh     frq=5952
 movl %ebx,%ecx                 # b := a
 lodsw
 movl (%edx,%eax,4),%eax
 movl 4*1(%edi,%eax,4),%ebx     # a := 1!(g!(H[pc])); pc += 2
 jmp fetchz

rl111: # l2gh     frq=0
 movl %ebx,%ecx                 # b := a
 lodsw
 movl (%edx,%eax,4),%eax
 movl 4*2(%edi,%eax,4),%ebx     # a := 2!(g!(H[pc])); pc += 2
 jmp fetchz

rl112: # lgh     frq=700955
 movl %ebx,%ecx                 # b := a
 movzwl (%esi),%ebx
 addl $2,%esi
 movl (%edx,%ebx,4),%ebx        # a := g!(H[pc]); pc += 2
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl113: # sgh     frq=297790
 movw (%esi),%ax
 addl $2,%esi
 movl %ebx,(%edx,%eax,4)        # g!(H[pc]) := a; pc += 2
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl114: # llgh     frq=14161
 movl %ebx,%ecx                 # b := a
 lodsw
 addl 16(%esp),%eax
 movl %eax,%ebx                 # a := @ g!(H[pc]); pc += 2
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl115: # agh     frq=90488
 movw (%esi),%ax
 addl $2,%esi
 addl (%edx,%eax,4),%ebx        # a += g!(H[pc]); pc += 2
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl116: # rv      frq=81916
rl117: # rv1     frq=18677
rl118: # rv2     frq=24044
rl119: # rv3     frq=942
rl120: # rv4     frq=0
rl121: # rv5     frq=303
rl122: # rv6     frq=392
 leal -116(%ebx,%eax),%eax
 movl (%edi,%eax,4),%ebx        # a := a!k
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl123: # rtn     frq=1315089
 movl 4(%ebp),%esi
 addl %edi,%esi                 # make %esi a m/c address
 movl (%ebp),%ebp
 addl %edi,%ebp                 # make %ebp a m/c address
    jmp fetchz
movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl124: # jgr     frq=234567
 cmpl %ebx,%ecx
 jg jgr1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)
jgr1:
 movsbl (%esi),%eax
 addl %eax,%esi
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl125: # jgr$     frq=0
 cmpl %ebx,%ecx
 jle nojump
 jmp indjump

rl126: # jgr0     frq=1664
 orl %ebx,%ebx
 jg jgr1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl127: # jgr0$     frq=0
 orl %ebx,%ebx
 jle nojump
 jmp indjump

rl128: # lp     frq=32473
 movl %ebx,%ecx              # b := a
 movb (%esi),%al
 incl %esi
 movl (%ebp,%eax,4),%ebx     # a := p!(B[pc++])
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl129: # lph     frq=0
 movl %ebx,%ecx              # b := a
 lodsw
 movl (%ebp,%eax,4),%ebx     # a := p!(H[pc]); pc += 2
 jmp fetchz

rl130: # lpw     frq=0
 movl %ebx,%ecx              # b := a
 lodsl
 movl (%ebp,%eax,4),%ebx     # a := p!(W[pc]); pc += 4
 jmp fetchz

rl131: # lp3     frq=1681284
 movl %ebx,%ecx
 movl 4*3(%ebp),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl132: # lp4     frq=593132
 movl %ebx,%ecx
 movl 4*4(%ebp),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl133: # lp5     frq=355769
 movl %ebx,%ecx
 movl 4*5(%ebp),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl134: # lp6     frq=200287
rl135: # lp7     frq=228093
rl136: # lp8     frq=126649
rl137: # lp9     frq=24237
rl138: # lp10     frq=11740
rl139: # lp11     frq=8112
rl140: # lp12     frq=1283
rl141: # lp13     frq=121
rl142: # lp14     frq=214
rl143: # lp15     frq=13779
rl144: # lp16     frq=45432
 movl %ebx,%ecx
 movl -4*128(%ebp,%eax,4),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl145: # sys     frq=554
 movl 4*4(%ebp),%eax       # Get arg 2 of sys(..)
   
 cmpl $-1,%ebx
 je sys_setcount
 cmpl $0,%ebx
 je sys_quit
 cmpl $1,%ebx
 je sys_rti
 cmpl $2,%ebx
 je sys_saveregs
 cmpl $3,%ebx
 je sys_setst
 cmpl $4,%ebx
 je sys_tracing
 cmpl $5,%ebx
 je sys_watch

 movl 16(%esp),%edx        # g as a BCPL pointer
 movl %ebp,%eax
 subl %edi,%eax
 shrl $2,%eax              # p as a BCPL pointer
 pushl %edx
 pushl %eax
##########################################################
 call dosys                # Un-comment one of these
#call _dosys               # as appropriate
##########################################################
 addl $8,%esp
 movl %eax,%ebx            # a := dosys(p, g)
 movl 36(%esp),%edx
 jmp fetchz

sys_setcount:              # CASE Sys_setcount:
	movl $199,%eax
	jmp ret1
 movl 20(%esp),%ebx        #   a := count
 movl %eax,20(%esp)        #   count := p!4
 movl $-1,%eax             #   res := -1
 jmp ret1                  #   GOTO ret

sys_quit:                  # CASE Sys_quit:
 je ret1                   #   res := p!4; GOTO ret

sys_rti:                   # CASE Sys_rti:
	#movl $201,%eax
	#jmp ret1
 # Now set the registers from arg 2 of sys(..)
 leal (%edi,%eax,4),%eax    # eax = m/c of registers

 movl (%eax),%ebx           # A
 movl 4*1(%eax),%ecx        # B
 movl 4*2(%eax),%esi        # B
 movl %esi,28(%esp)         # C
 movl 4*3(%eax),%ebp
 addl %edi,%ebp             # P
 movl 4*4(%eax),%edx
 addl %edi,%edx             # G
 movl %edx,36(%esp)         # save m/c address of G
 shrl $2,%edx
 movl %eax,16(%esp)         # save BCPL address of G0
 shll $2,%edx
 movl 4*5(%eax),%esi
 movl %esi,24(%esp)         # ST
 movl 4*6(%eax),%esi
 addl %edi,%esi             # PC as m/c address
                            # Don't set Count
 movl irqfifop, %eax
 cmpl irqfifoq, %eax
 je fetchz
 movl $1,irq
 jmp fetchz
   
sys_saveregs:              # CASE Sys_saveregs:
# now put registers back into regs
 leal (%edi,%eax,4),%eax   # get m/c addr of regs vector
	movl $202,%eax
	jmp ret1

 movl %ebx,(%eax)     # regs!0 = A
 movl %ecx,4*1(%eax)  # regs!1 = B
 movl 28(%esp),%ebx
 movl %ebx,4*2(%eax)  # regs!2 = C
 movl (%ebp),%ebx
 subl %edi,%ebx
 movl %ebx,4*3(%eax)  # regs!3 := P!0
 movl %edx,%ebx
 subl %edi,%ebx
 movl %ebx,4*4(%eax)  # G
 movl 24(%esp),%ebx
 movl %ebx,4*5(%eax)  # ST
 subl %edi,%esi
 movl %esi,4*6(%eax)  # PC
 movl 20(%esp),%ebx
 movl %ebx,4*7(%eax)  # Count
   
 movl (%eax),%ebx     # Restore A
 jmp fetchz
   
sys_setst:                 # CASE Sys_setst:
	#movl $203,%eax
	#jmp ret1
 movl %eax,24(%esp)        # st = Wp[4];
 movl $1,irq               # irq = 1; // check for interrupts, if any
 jmp fetchz                # goto fetch;
   
sys_tracing:               # CASE Sys_tracing:
 movl %eax,tracing         # tracing = Wp[4];
 jmp fetchz                # goto fetch;

sys_watch:                 # CASE Sys_watch:
	movl $205,%eax
	jmp ret1
 leal (%edi,%eax,4),%eax   # watchaddr = &W[Wp[4]];
 movl %eax,watchaddr
 movl (%eax),%eax
 movl %eax,watchval        # watchval = *watchaddr;
 jmp fetchz                # GOTO fetch


rl146: # swb     frq=48805
 incl %esi
 andl $0xFFFFFFFE,%esi     # round pc up to even address
 movb $1,%al               # i := 1

# There are at least 7 cases so unwind the first 3 iterations.

 cmpw (%esi,%eax,4),%bx    #   compare with case constant
 je swb3                   #   J if case found
 adcl %eax,%eax            #   if H[i]>=val then i := 2*i
                           #                else i := 2*i+1
 cmpw (%esi,%eax,4),%bx    #   compare with case constant
 je swb3                   #   J if case found
 adcl %eax,%eax            #   if H[i]>=val then i := 2*i
                           #                else i := 2*i+1
swb1:
 cmpw (%esi,%eax,4),%bx    # { compare with case constant
 je swb3                   #   J if case found
 adcl %eax,%eax            #   if H[i]>=val then i := 2*i
                           #                else i := 2*i+1
 cmpw (%esi),%ax           #
 jle swb1                  # } REPEATWHILE i<=n

swb2:                      # Set pc to default label
 leal 2(%esi),%esi
 movswl (%esi),%eax
 addl %eax,%esi            # set pc
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

swb3:                      # found (provided senior half zero)
 cmpl $0xFFFF,%ebx
 ja  swb2                  # J if senior half not zero
 leal 2(%esi,%eax,4),%esi
 movswl (%esi),%eax
 addl %eax,%esi            # set pc to case label
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl147: # swl     frq=85714
 incl %esi
 andl $0xFFFFFFFE,%esi     # round pc up to even address
 movw (%esi),%ax           # %eax := H[%esi] (= number of cases)
 addl $2,%esi              # %esi += 2
                           # %esi points to the default lab cell
 orl %ebx,%ebx
 jl swl1                   # J if value too small
 cmpl %eax,%ebx
 jge swl1                  # J if too large
 leal 2(%esi,%ebx,2),%esi  # get pointer to label cell
swl1:
 movswl (%esi),%eax
 addl %eax,%esi            # set pc
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl148: # st     frq=53452
 movl %ecx,(%edi,%ebx,4)   # a!0 := b
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl149: # st1     frq=36925
 movl %ecx,4*1(%edi,%ebx,4) # a!1 := b
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl150: # st2     frq=32011
 movl %ecx,4*2(%edi,%ebx,4) # a!2 := b
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl151: # st3     frq=5530
 movl %ecx,4*3(%edi,%ebx,4) # a!3 := b
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl152: # stp3     frq=2182
rl153: # stp4     frq=780
rl154: # stp5     frq=20
 movl -4*149(%ebp,%eax,4),%eax
 addl %ebx,%eax
 movl %ecx,(%edi,%eax,4)    # p!n!a := b
 jmp fetchz

rl155: # goto     frq=0
 leal (%edi,%ebx),%esi
 jmp fetch

rl156: # jle     frq=575294
 cmpl %ebx,%ecx
 jle jle1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)
jle1:
 movsbl (%esi),%eax
 addl %eax,%esi
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl157: # jle$     frq=12919
 cmpl %ebx,%ecx
 jg nojump
 jmp indjump

rl158: # jle0     frq=13814
 orl %ebx,%ebx
 jle jle1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl159: # jle0$     frq=0
 orl %ebx,%ebx
 jg nojump
 jmp indjump

rl160: # sp     frq=57497
 movb (%esi),%al
 incl %esi
 movl %ebx,(%ebp,%eax,4)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl161: # sph     frq=0
 lodsw
 movl %ebx,(%ebp,%eax,4)
 jmp fetchz

rl162: # spw     frq=0
 lodsw
 movl %ebx,(%ebp,%eax,4)
 jmp fetchz

rl163: # sp3     frq=448052
 movl %ebx,4*3(%ebp)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl164: # sp4     frq=988190
 movl %ebx,4*4(%ebp)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl165: # sp5     frq=344005
 movl %ebx,4*5(%ebp)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl166: # sp6     frq=163718
 movl %ebx,4*6(%ebp)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl167: # sp7     frq=233850
 movl %ebx,4*7(%ebp)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl168: # sp8     frq=109584
 movl %ebx,4*8(%ebp)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl169: # sp9     frq=110121
 movl %ebx,4*9(%ebp)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl170: # sp10     frq=56154
rl171: # sp11     frq=50794
rl172: # sp12     frq=20524
rl173: # sp13     frq=15806
rl174: # sp14     frq=4839
rl175: # sp15     frq=16120
rl176: # sp16     frq=33499
 movl %ebx,-4*160(%ebp,%eax,4)
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl177: # s1     frq=34291
 decl %ebx               # A := A-1
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl178: # s2     frq=4205
 subl $2,%ebx            # A := A-2
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl179: # s3     frq=26048
 subl $3,%ebx            # A := A-3
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl180: # s4     frq=3
 subl $4,%ebx            # A := A-4
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl181: # xch     frq=1761584
 xchgl %ecx,%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl182: # gbyt     frq=504790
 addl %edi,%ebx
 movzbl (%ebx,%ecx,4),%ebx        # a := b%a
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl183: # pbyt     frq=395227
 addl %edi,%ebx
 movb 28(%esp),%al
 movb %al,(%ebx,%ecx,4)           # b%a := c
 subl %edi,%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl184: # atc     frq=395227
 movl %ebx,28(%esp)               # c := a
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl185: # atb     frq=0
 movl %ebx,%ecx                   # b := a
 jmp fetch

rl186: # j     frq=302744
 movsbl (%esi),%eax               # pc += SB[pc]
 addl %eax,%esi
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl187: # j$     frq=150058
 movb (%esi),%al
 leal (%esi,%eax,2),%eax          # %eax := pc + 2*B[pc]
 andb $254,%al                    # %eax &= #xFFFFFFFE
 movswl (%eax),%esi
 addl %eax,%esi                   # %esi := %eax + SH[%eax]
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl188: # jge     frq=301004
 cmpl %ebx,%ecx
 jge jge1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)
jge1:
 movsbl (%esi),%eax
 addl %eax,%esi
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl189: # jge$     frq=0
 cmpl %ebx,%ecx
 jl nojump
 jmp indjump

rl190: # jge0     frq=47832
 orl %ebx,%ebx
 jge jge1
 incl %esi
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl191: # jge0$     frq=0
 orl %ebx,%ebx
 jl nojump
 jmp indjump

rl192: # ap     frq=6416
 lodsb
 addl (%ebp,%eax,4),%ebx
 jmp fetch

rl193: # aph     frq=0
 lodsw
 addl (%ebp,%eax,4),%ebx
 jmp fetchz

rl194: # apw     frq=0
 lodsl
 addl (%ebp,%eax,4),%ebx
 jmp fetchz

rl195: # ap3     frq=283379
 addl 4*3(%ebp),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl196: # ap4     frq=832703
 addl 4*4(%ebp),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl197: # ap5     frq=65255
rl198: # ap6     frq=11097
rl199: # ap7     frq=106439
rl200: # ap8     frq=3583
rl201: # ap9     frq=47609
rl202: # ap10     frq=1439
rl203: # ap11     frq=0
rl204: # ap12     frq=21
 addl -4*192(%ebp,%eax,4),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl205: # xpbyt     frq=326298
 addl %edi,%ecx
 movb 28(%esp),%al
 movb %al,(%ecx,%ebx,4)           # a%b := c
 subl %edi,%ecx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl206: # lmh     frq=1269
 movl %ebx,%ecx                   # b := a
 lodsw
 negl %eax
 movl %eax,%ebx                   # a := -H[pc]; pc += 2
 jmp fetchz

rl207: # btc     frq=184802
 movl %ecx,28(%esp)               # c := b
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl208: # nop     frq=0
 jmp fetch

rl209: # a1     frq=319289
 incl %ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl210: # a2     frq=69342
 addl $2,%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl211: # a3     frq=44520
rl212: # a4     frq=5224
rl213: # a5     frq=0
 leal -208(%ebx,%eax),%ebx
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl214: # rvp3     frq=1108
rl215: # rvp4     frq=1582
rl216: # rvp5     frq=30
rl217: # rvp6     frq=12697
rl218: # rvp7     frq=1449
 addl -4*211(%ebp,%eax,4),%ebx
 movl (%edi,%ebx,4),%ebx          # a := p!n!a
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl219: # st0p3     frq=10619
rl220: # st0p4     frq=7637
 movl -4*216(%ebp,%eax,4),%eax
 movl %ebx,(%edi,%eax,4)          # p!n!0 := a
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl221: # st1p3     frq=1455
rl222: # st1p4     frq=0
 movl -4*218(%ebp,%eax,4),%eax
 movl %ebx,4*1(%edi,%eax,4)       # p!n!1 := a
 jmp fetchz

rl223: # Error     frq=0
 decl %esi
 movb $1,%al
 jmp ret1

rl224: # a     frq=74587
 movb (%esi),%al
 incl %esi
 addl %eax,%ebx                   # a += B[pc++]
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl225: # ah     frq=6
 lodsw
 addl %eax,%ebx                   # a += H[pc]; pc += 2
 jmp fetchz

rl226: # aw        frq=0
 lodsl
 addl %eax,%ebx                   # a += W[pc]; pc += 4
 jmp fetchz

rl227: # l0p3      frq=65102
rl228: # l0p4      frq=612068
rl229: # l0p5      frq=17186
rl230: # l0p6      frq=8430
rl231: # l0p7      frq=30505
rl232: # l0p8      frq=14744
rl233: # l0p9      frq=0
rl234: # l0p10     frq=0
rl235: # l0p11     frq=0
rl236: # l0p12     frq=0
 movl %ebx,%ecx                   # b := a
 movl -4*224(%ebp,%eax,4),%ebx
 movl (%edi,%ebx,4),%ebx          # a := p!n!0
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl237: # s      frq=130833
 movb (%esi),%al
 incl %esi
 subl %eax,%ebx                   # a -= B[pc++]
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl238: # sh     frq=0
 movw (%esi),%ax
 addl $2,%esi
 subl %eax,%ebx                   # a -= H[pc]# pc += 2
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl239: # mdiv     frq=0     # sometimes causes floating point exception
 movl %ebx,%eax
 movl %edx,%ebx
 imull 16(%ebp)   # %eax:%edx := double length product
 idivl 20(%ebp)   # %eax = quotient, %edx = remainder
 xchgl %ebx,%edx
 movl %ebx,4*10(%edx)       # result2 := remainder
 movl %eax,%ebx             # a := quotient

 movl 4(%ebp),%esi          # return from BCPL function
 addl %edi,%esi             # pc = return address (m/c address)
 movl (%ebp),%ebp
 addl %edi,%ebp             # p  = old p pointer (m/c address)
   jmp fetchz
 movzbl (%esi),%eax
 incl %esi
 jmp *runtbl(,%eax,4)

rl240: # chgco     frq=2
 movl (%ebp),%esi
 movl 4*7(%edx),%eax
 movl %esi,(%edi,%eax,4)    # !currco := !p
 movl 4(%ebp),%esi
 addl %edi,%esi             # pc := p!1
 movl 16(%ebp),%eax
 movl %eax,4*7(%edx)        # currco := cptr
 movl (%edi,%eax,4),%ebp
 addl %edi,%ebp             # p := !cptr
 jmp fetchz

rl241: # neg     frq=297
 negl %ebx
 jmp fetch

rl242: # not     frq=196
 notl %ebx
 jmp fetch

rl243: # l1p3     frq=35547
rl244: # l1p4     frq=3525
rl245: # l1p5     frq=20773
rl246: # l1p6     frq=414
 movl %ebx,%ecx                 # b := a
 movl -4*240(%ebp,%eax,4),%ebx
 movl 4*1(%edi,%ebx,4),%ebx     # a := p!k!1
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl247: # l2p3     frq=22841
rl248: # l2p4     frq=5310
rl249: # l2p5     frq=32256
 movl %ebx,%ecx                 # b := a
 movl -4*244(%ebp,%eax,4),%ebx
 movl 4*2(%edi,%ebx,4),%ebx     # a := p!k!2
   jmp fetchz
 movb (%esi),%al
 incl %esi
 jmp *runtbl(,%eax,4)

rl250: # l3p3     frq=4185
rl251: # l3p4     frq=1
 movl %ebx,%ecx                 # b := a
 movl -4*247(%ebp,%eax,4),%ebx
 movl 4*3(%edi,%ebx,4),%ebx     # a := p!k!3
 jmp fetch

rl252: # l4p3     frq=449
rl253: # l4p4     frq=1
 movl %ebx,%ecx                 # b := a
 movl -4*249(%ebp,%eax,4),%ebx
 movl 4*4(%edi,%ebx,4),%ebx     # a := p!k!4
 jmp fetch

rl254: # Error     frq=0
rl255: # Error     frq=0
 decl %esi
 movb $1,%al
 jmp ret1

 .align 4

runtbl:
 .long   rl0,   rl1,   rl2,   rl3,   rl4,   rl5,   rl6,   rl7
 .long   rl8,   rl9,  rl10,  rl11,  rl12,  rl13,  rl14,  rl15
 .long  rl16,  rl17,  rl18,  rl19,  rl20,  rl21,  rl22,  rl23
 .long  rl24,  rl25,  rl26,  rl27,  rl28,  rl29,  rl30,  rl31
 .long  rl32,  rl33,  rl34,  rl35,  rl36,  rl37,  rl38,  rl39
 .long  rl40,  rl41,  rl42,  rl43,  rl44,  rl45,  rl46,  rl47
 .long  rl48,  rl49,  rl50,  rl51,  rl52,  rl53,  rl54,  rl55
 .long  rl56,  rl57,  rl58,  rl59,  rl60,  rl61,  rl62,  rl63
 .long  rl64,  rl65,  rl66,  rl67,  rl68,  rl69,  rl70,  rl71
 .long  rl72,  rl73,  rl74,  rl75,  rl76,  rl77,  rl78,  rl79
 .long  rl80,  rl81,  rl82,  rl83,  rl84,  rl85,  rl86,  rl87
 .long  rl88,  rl89,  rl90,  rl91,  rl92,  rl93,  rl94,  rl95
 .long  rl96,  rl97,  rl98,  rl99, rl100, rl101, rl102, rl103
 .long rl104, rl105, rl106, rl107, rl108, rl109, rl110, rl111
 .long rl112, rl113, rl114, rl115, rl116, rl117, rl118, rl119
 .long rl120, rl121, rl122, rl123, rl124, rl125, rl126, rl127
 .long rl128, rl129, rl130, rl131, rl132, rl133, rl134, rl135
 .long rl136, rl137, rl138, rl139, rl140, rl141, rl142, rl143
 .long rl144, rl145, rl146, rl147, rl148, rl149, rl150, rl151
 .long rl152, rl153, rl154, rl155, rl156, rl157, rl158, rl159
 .long rl160, rl161, rl162, rl163, rl164, rl165, rl166, rl167
 .long rl168, rl169, rl170, rl171, rl172, rl173, rl174, rl175
 .long rl176, rl177, rl178, rl179, rl180, rl181, rl182, rl183
 .long rl184, rl185, rl186, rl187, rl188, rl189, rl190, rl191
 .long rl192, rl193, rl194, rl195, rl196, rl197, rl198, rl199
 .long rl200, rl201, rl202, rl203, rl204, rl205, rl206, rl207
 .long rl208, rl209, rl210, rl211, rl212, rl213, rl214, rl215
 .long rl216, rl217, rl218, rl219, rl220, rl221, rl222, rl223
 .long rl224, rl225, rl226, rl227, rl228, rl229, rl230, rl231
 .long rl232, rl233, rl234, rl235, rl236, rl237, rl238, rl239
 .long rl240, rl241, rl242, rl243, rl244, rl245, rl246, rl247
 .long rl248, rl249, rl250, rl251, rl252, rl253, rl254, rl255

