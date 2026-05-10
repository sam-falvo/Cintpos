# This is the assembly code library for 64-bit native BCPL for
# AMD64 machines.
   
# Still under development

   
# C Linkage: Ie the linkage used when calling of callstart.

# On entry
#   0(%rsp)      Return address

#   &rdi        First integer or pointer argument
#   &rsi        Seconds integer or pointer argument
#   &rdx        Third integer or pointer argument
#   &rcx        Fourth integer or pointer argument
#   &r8         Fifth integer or pointer argument 
#   &r9         Sixth integer or pointer argument 

#   Other arguments are on the stack
#   Integer results are placed in %rax

#   %rbx %rbp %rdl %rsi %r12-%r15 must be preserved    
#   The other registers need not be preserved
#   
#   flag DF clear on entry and exit


.globl callstart
.globl _callstart

.text
	.align 16

# Typical callstart(p, g);
   
callstart:
_callstart:
 pushq %rbp
 pushq %rbx
 pushq %rdi
 pushq %rsi
 subl $40,%esp
 movl 60(%esp),%ebp      #  stackbase (first  argument)
 movl 64(%esp),%esi      #  gvec      (second argument)

# Save caller's FPH control word -- 16 bits
 fnstcw 36(%esp)

# Set FPH control word rounding to nearest with 24 bits precision
# suitable for 32 bit floating point to integer rounding.

# FPH codeword
#           IC     RC    PC
#    15 14 13 12  11 10 09 08  07 06 05 04  03 02 01 00
#                  0  0     Round to nearest
#                  0  1     Round towards - infinity
#                  1  0     Round towards + infinity
#                  1  1     Round towards zero
#                        0  0     24 bit procision
#                        1  0     53 bit procision
#                        1  1     64 bit procision

 fninit                  # Initialise the floating point hardware
 movzwl	36(%esp), %eax
 movb	$0xC0, %ah    
 movw	%ax, 38(%esp)
 fldcw	38(%esp)

# Register usage while executing 32-bit BCPL compiled code

# %eax  work register
# %ebx  Cintcode A
# %ecx  Cintcode B
# %edx  Cintcode C also used in division and remainder
# %rbp  The P pointer -- 64-bit m/c address
# %edi  work register
# %rsi  The G pointer -- 64-bit m/c address of Global 0
# %rsp  points to main work space
#    64(%esp)   gvec      (second arg of callstart)
#    60(%esp)   stackbase (first  arg of callstart)
#    56(%esp)   return address
#    52(%esp)   caller's %ebp
#    48(%esp)   caller's %ebx
#    44(%esp)   caller's %edi
#    40(%esp)   caller's %esi
#    36(%esp)   caller's FPH contol word
#    32(%esp)   
#    28(%esp)   
#    24(%esp)   
#    20(%esp)   
#    16(%esp)   
#    ...      ) space for args
#      (%esp) )    of external calls


   # make sure global 3 (sys) is defined
   movq $sys, 8*3(%rsi)
   # make sure global 6 (changeco) is defined
   movq $changeco, 8*6(%rsi)
   # make sure global 5 (muldiv) is defined
   movq $muldiv, 8*5(%rsi)

   # make BCPL call clihook(stackupb)
   movq stackupb,%rbx    # A := stackupb
   leaq 8*6(%rbp),%rdx    # NP := P + 6
   movq 8*4(%rsi),%rax    # clihook (G4) entry address
   call *%rax
   movq %rbx,%rax    # return the clihook result as callstart result
   
# Restore caller's FPH contol word -- 16 bits
 fldcw 36(%esp)
	
# and then return
 addq $40,%rsp
 popq %rsi
 popq %rdi
 popq %rbx
 popq %rbp
 ret

   	.align 16

   # res = sys(n, x, y, x)  the BCPL callable sys function
sys:
 movq %rbp,0(%rdx)   # NP!0 := P
 movq %rdx,%rbp      # P    := NP
 popq %rdx
 movq %rdx,8*1(%rbp) # P!1  := return address
 movq %rax,8*2(%rbp) # P!2  := entry address
 movq %rbx,8*3(%rbp) # P!3  := arg1

 movq %rsi,%rdx       # second arg (G) in edx
 movq %rbp,%rax       # first  arg (P) in eax
 pushq %rdx
 pushq %rax
 call dosys
# call _dosys
 addq $8*2,%rsp
 movq %rax,%rbx       # put result in Cintcode A register

 movq 8*1(%rbp),%rax
 movq 0(%rbp),%rbp
 jmp *%rax

# The following two functions use BCPL calling sequence.
   
changeco:     # changeco(cptr, arg)
 movq %rbp,0(%rdx)   # NP!0 := P
 movq %rdx,%rbp      # P    := NP
 popq %rdx
 movq %rdx,8*1(%rbp) # P!1  := return address
 movq %rax,8*2(%rbp) # P!2  := entry address
 movq %rbx,8*3(%rbp) # P!3  := arg1

 movq (%rbp),%rdx
 movq 8*7(%rsi),%rax
 movq %rdx,(,%rax,4)        # !currco := !p
 movq 4(%rbp),%rax          # pc := p!1
 movq 16(%rbp),%rdx
 movq %rdx,8*7(%rsi)        # currco := cptr
 movq 0(,%rdx,4),%rbp       # p := !cptr
 jmp *%rax

muldiv:
 movq %rbx,%rax
 movq %rdx,%rbx         # new P in ebx
 imulq 8*4(%rbx)        # %rax:%rdx := double length product
 idivq 8*5(%rbx)        # %rax = quotient, %rdx = remainder
 movq %rbx,4*10(%rsi)   # result2 := remainder
 movq %rax,%rbx         # a := quotient
 ret
   

