# C Linkage:
#   On entry 0(%esp)   is the return address
#            4(%esp)   is the first argument
#            8(%esp)   is the second argument
#            etc
#
#   %ebp, %ebx, %edi and %esi must be preserved
#
#   result in %eax
#
#   flag DF clear on entry and exit


.globl callstart

.data
hptr: 
 .long 0

.text
	.align 16

# callstart initialises globals sys, changeco and muldiv
#           then calls clihook(stackupb) returning its result.
   
callstart:
 pushl %ebp
 pushl %ebx
 pushl %edi
 pushl %esi
 subl $40,%esp
 movl 60(%esp),%ebp      #  stackbase (first  argument)
 movl 64(%esp),%esi      #  gvec      (second argument)

# Register usage while executing MCPL compiled code

# %eax  work register, entry address on MCPL call
# %ebx  Mintcode A
# %ecx  Mintcode B
# %edx  Mintcode C function and VALOF results
#                  NP on a function call and
#                  also used in division and remainder
# %ebp  The P pointer -- m/c address
# %edi  work register
# %esi  The G pointer -- m/c address of Global 0
# %esp  points to main work space
#    64(%esp)   gvec      (second arg of callstart)
#    60(%esp)   stackbase (first  arg of callstart)
#    56(%esp)   return address
#    52(%esp)   caller's %ebp
#    48(%esp)   caller's %ebx
#    44(%esp)   caller's %edi
#    40(%esp)   caller's %esi
#    36(%esp)   
#    32(%esp)   
#    28(%esp)   
#    24(%esp)   
#    20(%esp)   
#    16(%esp)   
#    ...      ) space for args
#    00(%esp) )    of external calls

   # make sure global 3 (sys) is defined
   movl $sys, 4*3(%esi)
   # make sure global 6 (changeco) is defined
   movl $changeco, 4*6(%esi)
   # make sure global 19 (muldiv) is defined
   movl $muldiv, 4*19(%esi)

   # MCPL call of clihook(stackupb)
   # leaving room for root coroutine info
   movl stackupb,%ebx
   leal 28(%ebp),%edx
   movl 16(%esi),%eax
   call *%eax
   
   movl %edx,%eax    # return the result of start
   
# and then return
 addl $40,%esp
 popl %esi
 popl %edi
 popl %ebx
 popl %ebp
 ret

   	.align 16

   # res = sys(n, x, y, x)  the BCPL callable sys function
sys:
 movl %ebp,0(%edx)   # NP!0 := P
 movl %edx,%ebp      # P    := NP
 popl %edx
 movl %edx,4(%ebp)   # P!1  := return address
 movl %eax,8(%ebp)   # P!2  := entry address
 movl %ebx,12(%ebp)  # P!3  := arg1

 movl %esi,%edx       # second arg in edx (the G pointer)
 movl %ebp,%eax       # first  arg in eax (the P pointer)
 pushl %edx
 pushl %eax
 call dosys
 addl $8,%esp
 movl %eax,%edx       # put result in Cintcode C register
   
 movl 4(%ebp),%eax
 movl 0(%ebp),%ebp
 jmp *%eax

changeco:               # changeco(val, cptr)
 movl %ebp,0(%edx)      # NP!0 := P    (old P)
 movl %edx,%ebp         # P    := NP
 popl %edx
 movl %edx,4(%ebp)      # P!1  := return address
 movl %eax,8(%ebp)      # P!2  := entry address
 movl %ebx,12(%ebp)     # P!3  := arg1

 movl (%ebp),%edx
 movl 4*7(%esi),%eax
 movl %edx,(%eax)       # !currco := !p

 movl 20(%esi),%edx
 movl %edx,24(%eax)     # currco!6 := h

 movl 4(%ebp),%eax      # pc := p!1

 movl 16(%ebp),%edx
 movl %edx,4*7(%esi)    # currco := cptr

 movl 0(%edx),%ebp      # p := !cptr

 movl 24(%edx),%edx     # h := currco!6
 movl %edx,20(%esi)

 movl %ebx,%edx         # c := a
 jmp *%eax

muldiv:
 movl %ebx,%eax
 movl %edx,%ebx         # new P in ebx
 imull 16(%ebx)         # %eax:%edx := double length product
 idivl 20(%ebx)         # %eax = quotient, %edx = remainder
 movl %ebx,4*10(%esi)   # result2 := remainder
 movl %eax,%edx         # c := quotient
 ret
   

