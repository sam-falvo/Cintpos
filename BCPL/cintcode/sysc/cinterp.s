	.file	"cinterp.c"
	.comm	memupbb,4,4
	.comm	tallylimb,4,4
	.globl	watchaddr
	.bss
	.align 4
	.type	watchaddr, @object
	.size	watchaddr, 4
watchaddr:
	.zero	4
	.globl	watchval
	.align 4
	.type	watchval, @object
	.size	watchval, 4
watchval:
	.zero	4
	.text
	.globl	interpret
	.type	interpret, @function
interpret:
.LFB0:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$204, %esp
	movl	12(%ebp), %eax
	movl	%eax, -112(%ebp)
	movl	-112(%ebp), %eax
	movl	592(%eax), %eax
	movl	%eax, -172(%ebp)
	movl	8(%ebp), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	.cfi_offset 3, -20
	.cfi_offset 6, -16
	.cfi_offset 7, -12
	movl	8(%ebp), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -176(%ebp)
	movl	8(%ebp), %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -156(%ebp)
	movl	8(%ebp), %eax
	addl	$3, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	sarl	$2, %eax
	movl	%eax, -152(%ebp)
	movl	8(%ebp), %eax
	addl	$4, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	sarl	$2, %eax
	movl	%eax, -108(%ebp)
	movl	8(%ebp), %eax
	addl	$5, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -104(%ebp)
	movl	8(%ebp), %eax
	addl	$6, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %esi
	movl	8(%ebp), %eax
	addl	$7, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -148(%ebp)
	movl	8(%ebp), %eax
	addl	$8, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -100(%ebp)
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	movl	-108(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -184(%ebp)
	movl	-108(%ebp), %eax
	addl	$256, %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -188(%ebp)
	movl	memupb, %eax
	sall	$2, %eax
	movl	%eax, -96(%ebp)
	movl	tallylim, %eax
	sall	$2, %eax
	movl	%eax, -92(%ebp)
.L2:
	movl	%esi, %eax
	cmpl	-96(%ebp), %eax
	ja	.L352
	jmp	.L3
.L353:
	nop
.L3:
	movl	watchaddr, %eax
	testl	%eax, %eax
	je	.L5
	movl	watchaddr, %eax
	movl	(%eax), %edx
	movl	watchval, %eax
	cmpl	%eax, %edx
	je	.L5
	movl	watchaddr, %eax
	movl	(%eax), %eax
	movl	%eax, watchval
	movl	-112(%ebp), %eax
	leal	4(%eax), %edx
	movl	watchaddr, %eax
	movl	%eax, %ecx
	movl	-112(%ebp), %eax
	movl	%ecx, %edi
	subl	%eax, %edi
	movl	%edi, %eax
	sarl	$2, %eax
	movl	%eax, (%edx)
	movl	-112(%ebp), %eax
	leal	8(%eax), %edx
	movl	watchval, %eax
	movl	%eax, (%edx)
	movl	$11, -144(%ebp)
	jmp	.L6
.L5:
	cmpl	$0, -148(%ebp)
	js	.L7
	cmpl	$0, -148(%ebp)
	jne	.L8
	movl	$3, -144(%ebp)
	jmp	.L6
.L8:
	subl	$1, -148(%ebp)
.L7:
	movl	tracing, %eax
	testl	%eax, %eax
	je	.L9
	movl	-176(%ebp), %eax
	movl	%eax, 12(%esp)
	movl	%ebx, 8(%esp)
	movl	-152(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	%esi, (%esp)
	call	trace
.L9:
	movl	%esi, %eax
	cmpl	-92(%ebp), %eax
	jae	.L10
	movl	tallyv, %edx
	movl	%esi, %eax
	sall	$2, %eax
	addl	%eax, %edx
	movl	(%edx), %eax
	addl	$1, %eax
	movl	%eax, (%edx)
.L10:
	subl	$1, -172(%ebp)
	cmpl	$0, -172(%ebp)
	jg	.L11
	movl	-112(%ebp), %eax
	movl	564(%eax), %eax
	movl	%eax, -88(%ebp)
	movl	-172(%ebp), %edi
	movl	%edi, -84(%ebp)
	movl	$0, -140(%ebp)
	movl	-112(%ebp), %eax
	movl	596(%eax), %eax
	movl	%eax, -80(%ebp)
	movl	-112(%ebp), %eax
	addl	$560, %eax
	movl	%eax, (%esp)
	call	timestamp
	movl	-112(%ebp), %eax
	addl	$564, %eax
	movl	(%eax), %eax
	subl	-88(%ebp), %eax
	subl	$20, %eax
	movl	%eax, -140(%ebp)
	cmpl	$-10, -140(%ebp)
	jge	.L12
	movl	$-10, -140(%ebp)
.L12:
	cmpl	$10, -140(%ebp)
	jle	.L13
	movl	$10, -140(%ebp)
.L13:
	movl	-112(%ebp), %eax
	movl	592(%eax), %eax
	movl	%eax, -172(%ebp)
	cmpl	$-3, -140(%ebp)
	jl	.L14
	cmpl	$3, -140(%ebp)
	jle	.L15
.L14:
	movl	-172(%ebp), %ecx
	imull	-140(%ebp), %ecx
	movl	$1374389535, %edx
	movl	%ecx, %eax
	imull	%edx
	sarl	$5, %edx
	movl	%ecx, %eax
	sarl	$31, %eax
	movl	%edx, %ecx
	subl	%eax, %ecx
	movl	%ecx, %eax
	subl	%eax, -172(%ebp)
	cmpl	$99999, -172(%ebp)
	jg	.L16
	movl	$100000, -172(%ebp)
.L16:
	cmpl	$1000000000, -172(%ebp)
	jle	.L17
	movl	$1000000000, -172(%ebp)
.L17:
	movl	-112(%ebp), %eax
	addl	$592, %eax
	movl	-172(%ebp), %edi
	movl	%edi, (%eax)
.L15:
	movl	$1374389535, %edx
	movl	-172(%ebp), %eax
	imull	%edx
	sarl	$4, %edx
	movl	-172(%ebp), %eax
	sarl	$31, %eax
	movl	%edx, %ecx
	subl	%eax, %ecx
	movl	%ecx, %eax
	movl	%eax, -172(%ebp)
.L11:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	addl	$1, %esi
	cmpl	$255, %eax
	ja	.L18
	movl	.L273(,%eax,4), %eax
	jmp	*%eax
	.section	.rodata
	.align 4
	.align 4
.L273:
	.long	.L18
	.long	.L19
	.long	.L20
	.long	.L21
	.long	.L22
	.long	.L23
	.long	.L24
	.long	.L25
	.long	.L26
	.long	.L27
	.long	.L28
	.long	.L29
	.long	.L30
	.long	.L31
	.long	.L32
	.long	.L33
	.long	.L34
	.long	.L35
	.long	.L36
	.long	.L37
	.long	.L38
	.long	.L39
	.long	.L40
	.long	.L41
	.long	.L42
	.long	.L43
	.long	.L44
	.long	.L45
	.long	.L46
	.long	.L47
	.long	.L48
	.long	.L49
	.long	.L50
	.long	.L51
	.long	.L52
	.long	.L53
	.long	.L54
	.long	.L55
	.long	.L56
	.long	.L57
	.long	.L58
	.long	.L59
	.long	.L60
	.long	.L61
	.long	.L62
	.long	.L63
	.long	.L64
	.long	.L65
	.long	.L66
	.long	.L67
	.long	.L68
	.long	.L69
	.long	.L70
	.long	.L71
	.long	.L72
	.long	.L73
	.long	.L74
	.long	.L75
	.long	.L76
	.long	.L77
	.long	.L78
	.long	.L79
	.long	.L80
	.long	.L81
	.long	.L82
	.long	.L83
	.long	.L84
	.long	.L85
	.long	.L86
	.long	.L87
	.long	.L88
	.long	.L89
	.long	.L90
	.long	.L91
	.long	.L92
	.long	.L93
	.long	.L94
	.long	.L95
	.long	.L96
	.long	.L97
	.long	.L98
	.long	.L99
	.long	.L100
	.long	.L101
	.long	.L102
	.long	.L103
	.long	.L104
	.long	.L105
	.long	.L106
	.long	.L107
	.long	.L108
	.long	.L109
	.long	.L110
	.long	.L111
	.long	.L112
	.long	.L113
	.long	.L114
	.long	.L115
	.long	.L116
	.long	.L117
	.long	.L118
	.long	.L119
	.long	.L120
	.long	.L121
	.long	.L122
	.long	.L123
	.long	.L124
	.long	.L125
	.long	.L126
	.long	.L127
	.long	.L128
	.long	.L129
	.long	.L130
	.long	.L131
	.long	.L132
	.long	.L133
	.long	.L134
	.long	.L135
	.long	.L136
	.long	.L137
	.long	.L138
	.long	.L139
	.long	.L140
	.long	.L141
	.long	.L142
	.long	.L143
	.long	.L144
	.long	.L145
	.long	.L146
	.long	.L147
	.long	.L148
	.long	.L149
	.long	.L150
	.long	.L151
	.long	.L152
	.long	.L153
	.long	.L154
	.long	.L155
	.long	.L156
	.long	.L157
	.long	.L158
	.long	.L159
	.long	.L160
	.long	.L161
	.long	.L162
	.long	.L163
	.long	.L164
	.long	.L165
	.long	.L166
	.long	.L167
	.long	.L168
	.long	.L169
	.long	.L170
	.long	.L171
	.long	.L172
	.long	.L173
	.long	.L174
	.long	.L175
	.long	.L176
	.long	.L177
	.long	.L178
	.long	.L179
	.long	.L180
	.long	.L181
	.long	.L182
	.long	.L183
	.long	.L184
	.long	.L185
	.long	.L186
	.long	.L187
	.long	.L188
	.long	.L189
	.long	.L190
	.long	.L191
	.long	.L192
	.long	.L193
	.long	.L194
	.long	.L195
	.long	.L196
	.long	.L197
	.long	.L198
	.long	.L199
	.long	.L200
	.long	.L201
	.long	.L202
	.long	.L203
	.long	.L204
	.long	.L205
	.long	.L206
	.long	.L207
	.long	.L208
	.long	.L209
	.long	.L210
	.long	.L211
	.long	.L212
	.long	.L213
	.long	.L214
	.long	.L215
	.long	.L216
	.long	.L217
	.long	.L218
	.long	.L219
	.long	.L220
	.long	.L221
	.long	.L222
	.long	.L223
	.long	.L224
	.long	.L225
	.long	.L353
	.long	.L227
	.long	.L228
	.long	.L229
	.long	.L230
	.long	.L231
	.long	.L232
	.long	.L233
	.long	.L234
	.long	.L235
	.long	.L236
	.long	.L237
	.long	.L238
	.long	.L239
	.long	.L240
	.long	.L18
	.long	.L241
	.long	.L242
	.long	.L243
	.long	.L244
	.long	.L245
	.long	.L246
	.long	.L247
	.long	.L248
	.long	.L249
	.long	.L250
	.long	.L251
	.long	.L252
	.long	.L253
	.long	.L254
	.long	.L255
	.long	.L256
	.long	.L257
	.long	.L258
	.long	.L259
	.long	.L260
	.long	.L261
	.long	.L262
	.long	.L263
	.long	.L264
	.long	.L265
	.long	.L266
	.long	.L267
	.long	.L268
	.long	.L269
	.long	.L270
	.long	.L271
	.long	.L272
	.text
.L18:
	movl	$1, -144(%ebp)
	subl	$1, %esi
	jmp	.L6
.L19:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, -76(%ebp)
	addl	$1, %esi
	cmpl	$18, -76(%ebp)
	ja	.L274
	movl	-76(%ebp), %eax
	sall	$2, %eax
	addl	$.L279, %eax
	movl	(%eax), %eax
	jmp	*%eax
	.section	.rodata
	.align 4
	.align 4
.L279:
	.long	.L275
	.long	.L276
	.long	.L274
	.long	.L277
	.long	.L277
	.long	.L277
	.long	.L278
	.long	.L278
	.long	.L278
	.long	.L278
	.long	.L278
	.long	.L277
	.long	.L277
	.long	.L278
	.long	.L278
	.long	.L278
	.long	.L278
	.long	.L278
	.long	.L278
	.text
.L274:
	movl	-112(%ebp), %eax
	leal	4(%eax), %edx
	movl	-76(%ebp), %eax
	movl	%eax, (%edx)
	movl	$14, -144(%ebp)
	jmp	.L6
.L275:
	movl	$-1, %ebx
	jmp	.L3
.L276:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, -136(%ebp)
	addl	$1, %esi
	cmpl	$127, -136(%ebp)
	jle	.L280
	subl	$256, -136(%ebp)
.L280:
	movl	-136(%ebp), %eax
	movl	%eax, 8(%esp)
	movl	%ebx, 4(%esp)
	movl	-76(%ebp), %eax
	movl	%eax, (%esp)
	call	doflt
	movl	%eax, %ebx
	jmp	.L3
.L277:
	movl	$0, 8(%esp)
	movl	%ebx, 4(%esp)
	movl	-76(%ebp), %eax
	movl	%eax, (%esp)
	call	doflt
	movl	%eax, %ebx
	jmp	.L3
.L278:
	movl	%ebx, 8(%esp)
	movl	-176(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	-76(%ebp), %eax
	movl	%eax, (%esp)
	call	doflt
	movl	%eax, %ebx
	jmp	.L3
.L271:  // selld
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, -72(%ebp)     // len = B[pc++]
	addl	$1, %esi
   
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, -68(%ebp)     // sh = B[pc++]
	addl	$1, %esi
   
	movl	$-1, -132(%ebp)
	cmpl	$0, -72(%ebp)
	je	.L281
	movl	-72(%ebp), %edx
	movl	$1, %eax
	movl	%edx, %ecx
	sall	%cl, %eax
	subl	$1, %eax
	movl	%eax, -132(%ebp)    // mask = (1<<len) - 1
.L281:
	movl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax        // %eax = W[a]
	movl	%eax, %edx          // %edx = W[a]
	movl	-68(%ebp), %eax     // %eax = sh
	movl	%eax, %ecx
	shrl	%cl, %edx           // %edx = W[a]<<sh
	movl	-132(%ebp), %eax    // %eax = mask
	andl	%edx, %eax          // %eax = (W[a]<<sh) & mask
	movl	%eax, %ebx          // a = (W[a]<<sh) & mask
	jmp	.L3

 .L272: // selst
	movl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%eax, -64(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, -60(%ebp)
	addl	$1, %esi
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, -56(%ebp)
	addl	$1, %esi
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, -52(%ebp)
	addl	$1, %esi
	cmpl	$0, -56(%ebp)
	jne	.L282
	movl	-52(%ebp), %edx
	movl	$-1, %eax
	movl	%edx, %ecx
	shrl	%cl, %eax
	movl	%eax, -128(%ebp)
	jmp	.L283
   
.L282:
	movl	-56(%ebp), %edx
	movl	$1, %eax
	movl	%edx, %ecx
	sall	%cl, %eax
	subl	$1, %eax
	movl	%eax, -128(%ebp)
.L283:
	movl	-64(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, %edx
	movl	-52(%ebp), %eax
	movl	%edx, %edi
	movl	%eax, %ecx
	shrl	%cl, %edi
	movl	%edi, %eax
	andl	-128(%ebp), %eax
	movl	%eax, -168(%ebp)
	movl	-168(%ebp), %eax
	movl	%eax, -48(%ebp)
	cmpl	$17, -60(%ebp)
	ja	.L284
	movl	-60(%ebp), %eax
	sall	$2, %eax
	addl	$.L303, %eax
	movl	(%eax), %eax
	jmp	*%eax
	.section	.rodata
	.align 4
	.align 4
.L303:
	.long	.L285
	.long	.L286
	.long	.L287
	.long	.L288
	.long	.L289
	.long	.L290
	.long	.L291
	.long	.L292
	.long	.L293
	.long	.L294
	.long	.L295
	.long	.L296
	.long	.L297
	.long	.L298
	.long	.L299
	.long	.L300
	.long	.L301
	.long	.L302
	.text
.L284:
	movl	$0, %ebx
	jmp	.L3
.L285:
	movl	-176(%ebp), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L286:
	movl	-168(%ebp), %eax
	addl	-176(%ebp), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L287:
	movl	-176(%ebp), %eax
	movl	%eax, -164(%ebp)
	leal	-168(%ebp), %eax
	flds	(%eax)
	leal	-164(%ebp), %eax
	flds	(%eax)
	fmulp	%st, %st(1)
	fstps	-160(%ebp)
	leal	-160(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L288:
	movl	-176(%ebp), %eax
	movl	%eax, -164(%ebp)
	leal	-168(%ebp), %eax
	flds	(%eax)
	leal	-164(%ebp), %eax
	flds	(%eax)
	fdivrp	%st, %st(1)
	fstps	-160(%ebp)
	leal	-160(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L289:
	movl	-176(%ebp), %eax
	movl	%eax, -164(%ebp)
	leal	-164(%ebp), %eax
	flds	(%eax)
	leal	-168(%ebp), %eax
	flds	(%eax)
	fld	%st(1)
	fld	%st(1)
.L305:
	fprem
	fnstsw	%ax
	sahf
	jp	.L305
	fstp	%st(1)
	fucomi	%st(0), %st
	jp	.L366
	fucomi	%st(0), %st
	je	.L367
	fstp	%st(0)
	fxch	%st(1)
	jmp	.L307
.L366:
	fstp	%st(0)
	fxch	%st(1)
.L307:
	fstps	4(%esp)
	fstps	(%esp)
	call	fmodf
	jmp	.L306
.L367:
	fstp	%st(1)
	fstp	%st(1)
.L306:
	fstps	-192(%ebp)
	movl	-192(%ebp), %eax
	movl	%eax, -160(%ebp)
	leal	-160(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L290:
	movl	-176(%ebp), %eax
	movl	%eax, -164(%ebp)
	leal	-168(%ebp), %eax
	flds	(%eax)
	leal	-164(%ebp), %eax
	flds	(%eax)
	faddp	%st, %st(1)
	fstps	-160(%ebp)
	leal	-160(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L291:
	movl	-176(%ebp), %eax
	movl	%eax, -164(%ebp)
	leal	-168(%ebp), %eax
	flds	(%eax)
	leal	-164(%ebp), %eax
	flds	(%eax)
	fsubrp	%st, %st(1)
	fstps	-160(%ebp)
	leal	-160(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L292:
	movl	-168(%ebp), %eax
	imull	-176(%ebp), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L293:
	movl	-168(%ebp), %eax
	movl	%eax, %edx
	sarl	$31, %edx
	idivl	-176(%ebp)
	movl	%eax, -168(%ebp)
	jmp	.L304
.L294:
	movl	-168(%ebp), %eax
	movl	%eax, %edx
	sarl	$31, %edx
	idivl	-176(%ebp)
	movl	%edx, %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L295:
	movl	-168(%ebp), %eax
	addl	-176(%ebp), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L296:
	movl	-168(%ebp), %eax
	subl	-176(%ebp), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L297:
	cmpl	$31, -176(%ebp)
	jle	.L308
	movl	$0, -168(%ebp)
.L308:
	movl	-168(%ebp), %eax
	movzbl	-176(%ebp), %ecx
	sall	%cl, %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L298:
	cmpl	$31, -176(%ebp)
	jle	.L309
	movl	$0, -168(%ebp)
.L309:
	movl	-168(%ebp), %eax
	movzbl	-176(%ebp), %ecx
	shrl	%cl, %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L299:
	movl	-168(%ebp), %eax
	andl	-176(%ebp), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L300:
	movl	-168(%ebp), %eax
	orl	-176(%ebp), %eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L301:
	movl	-168(%ebp), %eax
	xorl	-176(%ebp), %eax
	notl	%eax
	movl	%eax, -168(%ebp)
	jmp	.L304
.L302:
	movl	-168(%ebp), %eax
	xorl	-176(%ebp), %eax
	movl	%eax, -168(%ebp)
	nop
.L304:
	movl	-64(%ebp), %eax
	movl	(%eax), %edi
	movl	-168(%ebp), %eax
	xorl	-48(%ebp), %eax
	movl	-128(%ebp), %edx
	andl	%eax, %edx
	movl	%edx, -196(%ebp)
	movl	-52(%ebp), %eax
	movl	-196(%ebp), %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	movl	%edi, %edx
	xorl	%eax, %edx
	movl	-64(%ebp), %eax
	movl	%edx, (%eax)
	jmp	.L3
.L70:
	imull	-176(%ebp), %ebx
	jmp	.L3
.L71:
	testl	%ebx, %ebx
	jne	.L310
	movl	$5, -144(%ebp)
	jmp	.L6
.L310:
	movl	-176(%ebp), %eax
	movl	%eax, %edx
	sarl	$31, %edx
	idivl	%ebx
	movl	%eax, %ebx
	jmp	.L3
.L72:
	testl	%ebx, %ebx
	jne	.L311
	movl	$5, -144(%ebp)
	jmp	.L6
.L311:
	movl	-176(%ebp), %eax
	movl	%eax, %edx
	sarl	$31, %edx
	idivl	%ebx
	movl	%edx, %ebx
	jmp	.L3
.L102:
	addl	-176(%ebp), %ebx
	jmp	.L3
.L103:
	movl	-176(%ebp), %eax
	subl	%ebx, %eax
	movl	%eax, %ebx
	jmp	.L3
.L258:
	negl	%ebx
	jmp	.L3
.L45:
	movl	$0, %ebx
	addl	$1, %esi
	jmp	.L3
.L104:
	cmpl	$31, %ebx
	jle	.L312
	movl	$0, -176(%ebp)
.L312:
	movl	-176(%ebp), %eax
	movl	%ebx, %ecx
	sall	%cl, %eax
	movl	%eax, %ebx
	jmp	.L3
.L105:
	cmpl	$31, %ebx
	jle	.L313
	movl	$0, -176(%ebp)
.L313:
	movl	-176(%ebp), %eax
	movl	%ebx, %ecx
	shrl	%cl, %eax
	movl	%eax, %ebx
	jmp	.L3
.L259:
	notl	%ebx
	jmp	.L3
.L106:
	andl	-176(%ebp), %ebx
	jmp	.L3
.L107:
	orl	-176(%ebp), %ebx
	jmp	.L3
.L73:
	xorl	-176(%ebp), %ebx
	jmp	.L3
.L173:
	movl	%ebx, %esi
	jmp	.L2
.L20:
	movl	$2, -144(%ebp)
	subl	$1, %esi
	jmp	.L6
.L140:
	movl	%ebx, %eax
	addl	$6, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L139:
	movl	%ebx, %eax
	addl	$5, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L138:
	movl	%ebx, %eax
	addl	$4, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L137:
	movl	%ebx, %eax
	addl	$3, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L136:
	movl	%ebx, %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L135:
	movl	%ebx, %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L134:
	movl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L169:
	movl	%ebx, %eax
	addl	$3, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %edi
	movl	%edi, (%eax)
	jmp	.L3
.L168:
	movl	%ebx, %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %edx
	movl	%edx, (%eax)
	jmp	.L3
.L167:
	movl	%ebx, %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %ecx
	movl	%ecx, (%eax)
	jmp	.L3
.L166:
	movl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %edi
	movl	%edi, (%eax)
	jmp	.L3
.L257:
	movl	-184(%ebp), %eax
	addl	$28, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-180(%ebp), %edi
	movl	(%edi), %eax
	movl	%eax, (%edx)
	movl	-180(%ebp), %edi
	movl	4(%edi), %esi
	movl	-184(%ebp), %edx
	addl	$28, %edx
	movl	-180(%ebp), %edi
	movl	16(%edi), %eax
	movl	%eax, (%edx)
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	sarl	$2, %eax
	movl	%eax, -152(%ebp)
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	jmp	.L2
.L256:
	movl	-180(%ebp), %eax
	addl	$20, %eax
	movl	(%eax), %ecx
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %edx
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	movl	%ecx, 8(%esp)
	movl	%edx, 4(%esp)
	movl	%eax, (%esp)
	call	muldiv
	movl	%eax, %ebx
	movl	-184(%ebp), %edx
	addl	$40, %edx
	movl	result2, %eax
	movl	%eax, (%edx)
.L141:
	movl	-180(%ebp), %edi
	movl	4(%edi), %esi
	movl	-152(%ebp), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %eax
	sarl	$2, %eax
	movl	%eax, -152(%ebp)
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	jmp	.L2
.L200:
	movl	-176(%ebp), %eax
	sall	$2, %eax
	addl	%ebx, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %ebx
	jmp	.L3
.L201:
	movl	-176(%ebp), %eax
	sall	$2, %eax
	addl	%ebx, %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-156(%ebp), %eax
	movb	%al, (%edx)
	jmp	.L3
.L223:
	leal	0(,%ebx,4), %eax
	addl	-176(%ebp), %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-156(%ebp), %eax
	movb	%al, (%edx)
	jmp	.L3
.L202:
	movl	%ebx, -156(%ebp)
	jmp	.L3
.L225:
	movl	-176(%ebp), %eax
	movl	%eax, -156(%ebp)
	jmp	.L3
.L203:
	movl	%ebx, -176(%ebp)
	jmp	.L3
.L199:
	xorl	-176(%ebp), %ebx
	xorl	%ebx, -176(%ebp)
	xorl	-176(%ebp), %ebx
	jmp	.L3
.L164:
	movl	$1, -120(%ebp)
	leal	1(%esi), %eax
	sarl	%eax
	movl	%eax, -124(%ebp)
	movl	-124(%ebp), %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	movzwl	%ax, %eax
	movl	%eax, -44(%ebp)
	jmp	.L314
.L317:
	movl	-120(%ebp), %eax
	addl	%eax, %eax
	movl	%eax, -120(%ebp)
	movl	-120(%ebp), %edx
	movl	-124(%ebp), %eax
	addl	%edx, %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	movzwl	%ax, %eax
	movl	%eax, -40(%ebp)
	cmpl	-40(%ebp), %ebx
	jne	.L315
	movl	-120(%ebp), %eax
	addl	%eax, -124(%ebp)
	jmp	.L316
.L315:
	cmpl	-40(%ebp), %ebx
	jge	.L314
	addl	$1, -120(%ebp)
.L314:
	movl	-120(%ebp), %eax
	cmpl	-44(%ebp), %eax
	jle	.L317
.L316:
	addl	$1, -124(%ebp)
	movl	-124(%ebp), %eax
	leal	(%eax,%eax), %edx
	movl	-124(%ebp), %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	cwtl
	leal	(%edx,%eax), %esi
	jmp	.L2
.L165:
	leal	1(%esi), %eax
	sarl	%eax
	movl	%eax, -116(%ebp)
	movl	-116(%ebp), %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	movzwl	%ax, %eax
	movl	%eax, -36(%ebp)
	addl	$1, -116(%ebp)
	testl	%ebx, %ebx
	js	.L318
	cmpl	-36(%ebp), %ebx
	jge	.L318
	leal	1(%ebx), %eax
	addl	%eax, -116(%ebp)
.L318:
	movl	-116(%ebp), %eax
	leal	(%eax,%eax), %edx
	movl	-116(%ebp), %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	cwtl
	leal	(%edx,%eax), %esi
	jmp	.L2
.L163:
	testl	%ebx, %ebx
	je	.L321
	cmpl	$5, %ebx
	je	.L322
	cmpl	$-1, %ebx
	je	.L320
	movl	8(%ebp), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	movl	8(%ebp), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %edx
	movl	%edx, (%eax)
	movl	8(%ebp), %eax
	addl	$2, %eax
	sall	$2, %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-156(%ebp), %eax
	movl	%eax, (%edx)
	movl	8(%ebp), %eax
	addl	$3, %eax
	sall	$2, %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	movl	8(%ebp), %eax
	addl	$4, %eax
	sall	$2, %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-108(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	movl	8(%ebp), %eax
	addl	$5, %eax
	sall	$2, %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-104(%ebp), %eax
	movl	%eax, (%edx)
	movl	8(%ebp), %eax
	addl	$6, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%esi, (%eax)
	movl	8(%ebp), %eax
	addl	$7, %eax
	sall	$2, %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-148(%ebp), %eax
	movl	%eax, (%edx)
	movl	8(%ebp), %eax
	addl	$8, %eax
	sall	$2, %eax
	movl	%eax, %edx
	addl	-112(%ebp), %edx
	movl	-100(%ebp), %eax
	movl	%eax, (%edx)
	movl	-108(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	-152(%ebp), %eax
	movl	%eax, (%esp)
	call	dosys
	movl	%eax, %ebx
	jmp	.L3
.L320:
	movl	-148(%ebp), %ebx
	movl	-180(%ebp), %edi
	movl	16(%edi), %eax
	movl	%eax, -148(%ebp)
	movl	$-1, -144(%ebp)
	jmp	.L6
.L321:
	movl	-180(%ebp), %edi
	movl	16(%edi), %eax
	movl	%eax, -144(%ebp)
	jmp	.L6
.L322:
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%eax, watchaddr
	movl	watchaddr, %eax
	movl	(%eax), %eax
	movl	%eax, watchval
	jmp	.L3
.L162:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	64(%edi), %ebx
	jmp	.L3
.L161:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	60(%edi), %ebx
	jmp	.L3
.L160:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	56(%edi), %ebx
	jmp	.L3
.L159:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	52(%edi), %ebx
	jmp	.L3
.L158:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	48(%edi), %ebx
	jmp	.L3
.L157:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	44(%edi), %ebx
	jmp	.L3
.L156:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	40(%edi), %ebx
	jmp	.L3
.L155:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	36(%edi), %ebx
	jmp	.L3
.L154:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	32(%edi), %ebx
	jmp	.L3
.L153:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	28(%edi), %ebx
	jmp	.L3
.L152:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	24(%edi), %ebx
	jmp	.L3
.L151:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	20(%edi), %ebx
	jmp	.L3
.L150:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	16(%edi), %ebx
	jmp	.L3
.L149:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %edi
	movl	12(%edi), %ebx
	jmp	.L3
.L146:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L147:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	(%eax), %ebx
	addl	$2, %esi
	jmp	.L3
.L148:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$3, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$2, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	(%eax), %ebx
	addl	$4, %esi
	jmp	.L3
.L82:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %ebx
	addl	-152(%ebp), %ebx
	addl	$1, %esi
	jmp	.L3
.L83:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %ebx
	addl	-152(%ebp), %ebx
	addl	$2, %esi
	jmp	.L3
.L84:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$3, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$2, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %ebx
	addl	-152(%ebp), %ebx
	addl	$4, %esi
	jmp	.L3
.L194:
	movl	-180(%ebp), %eax
	addl	$64, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L193:
	movl	-180(%ebp), %eax
	addl	$60, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L192:
	movl	-180(%ebp), %eax
	addl	$56, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L191:
	movl	-180(%ebp), %eax
	addl	$52, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L190:
	movl	-180(%ebp), %eax
	addl	$48, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L189:
	movl	-180(%ebp), %eax
	addl	$44, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L188:
	movl	-180(%ebp), %eax
	addl	$40, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L187:
	movl	-180(%ebp), %eax
	addl	$36, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L186:
	movl	-180(%ebp), %eax
	addl	$32, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L185:
	movl	-180(%ebp), %eax
	addl	$28, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L184:
	movl	-180(%ebp), %eax
	addl	$24, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L183:
	movl	-180(%ebp), %eax
	addl	$20, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L182:
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L181:
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L178:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$1, %esi
	jmp	.L3
.L179:
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$2, %esi
	jmp	.L3
.L180:
	movl	%esi, %eax
	addl	$3, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$2, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$4, %esi
	jmp	.L3
.L130:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %ebx
	addl	$2, %esi
	jmp	.L3
.L98:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-188(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L66:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L131:
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$2, %esi
	jmp	.L3
.L99:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-188(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$1, %esi
	jmp	.L3
.L67:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$1, %esi
	jmp	.L3
.L132:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %ebx
	addl	-108(%ebp), %ebx
	addl	$2, %esi
	jmp	.L3
.L100:
	movl	%ebx, -176(%ebp)
	movl	-108(%ebp), %eax
	leal	256(%eax), %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	leal	(%edx,%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L68:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %ebx
	addl	-108(%ebp), %ebx
	addl	$1, %esi
	jmp	.L3
.L77:
	movl	%esi, %edx
	sarl	%edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	addl	%edx, %eax
	movl	%eax, -32(%ebp)
	movl	-32(%ebp), %eax
	leal	(%eax,%eax), %edx
	movl	-32(%ebp), %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	cwtl
	addl	%edx, %eax
	movl	%eax, -32(%ebp)
	movl	%ebx, -176(%ebp)
	movl	-32(%ebp), %eax
	sarl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L76:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%esi, %eax
	sarl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L75:
	movl	%esi, %edx
	sarl	%edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	addl	%edx, %eax
	movl	%eax, -32(%ebp)
	movl	-32(%ebp), %eax
	leal	(%eax,%eax), %edx
	movl	-32(%ebp), %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	cwtl
	addl	%edx, %eax
	movl	%eax, -32(%ebp)
	movl	-32(%ebp), %eax
	sarl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$1, %esi
	jmp	.L3
.L74:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%esi, %eax
	sarl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$1, %esi
	jmp	.L3
.L109:
	movl	%esi, %edx
	sarl	%edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	addl	%edx, %eax
	movl	%eax, -32(%ebp)
	movl	-32(%ebp), %eax
	leal	(%eax,%eax), %edx
	movl	-32(%ebp), %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	cwtl
	addl	%edx, %eax
	movl	%eax, -32(%ebp)
	movl	%ebx, -176(%ebp)
	movl	-32(%ebp), %eax
	movl	%eax, %ebx
	sarl	$2, %ebx
	addl	$1, %esi
	jmp	.L3
.L108:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%esi, %eax
	movl	%eax, %ebx
	sarl	$2, %ebx
	addl	$1, %esi
	jmp	.L3
.L44:
	movl	%ebx, -176(%ebp)
	movl	$10, %ebx
	jmp	.L3
.L43:
	movl	%ebx, -176(%ebp)
	movl	$9, %ebx
	jmp	.L3
.L42:
	movl	%ebx, -176(%ebp)
	movl	$8, %ebx
	jmp	.L3
.L41:
	movl	%ebx, -176(%ebp)
	movl	$7, %ebx
	jmp	.L3
.L40:
	movl	%ebx, -176(%ebp)
	movl	$6, %ebx
	jmp	.L3
.L39:
	movl	%ebx, -176(%ebp)
	movl	$5, %ebx
	jmp	.L3
.L38:
	movl	%ebx, -176(%ebp)
	movl	$4, %ebx
	jmp	.L3
.L37:
	movl	%ebx, -176(%ebp)
	movl	$3, %ebx
	jmp	.L3
.L36:
	movl	%ebx, -176(%ebp)
	movl	$2, %ebx
	jmp	.L3
.L35:
	movl	%ebx, -176(%ebp)
	movl	$1, %ebx
	jmp	.L3
.L34:
	movl	%ebx, -176(%ebp)
	movl	$0, %ebx
	jmp	.L3
.L33:
	movl	%ebx, -176(%ebp)
	movl	$-1, %ebx
	jmp	.L3
.L114:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %ebx
	addl	$1, %esi
	jmp	.L3
.L115:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%edx, %ebx
	orl	%eax, %ebx
	addl	$2, %esi
	jmp	.L3
.L116:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$3, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$2, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%edx, %ebx
	orl	%eax, %ebx
	addl	$4, %esi
	jmp	.L3
.L32:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %ebx
	negl	%ebx
	addl	$1, %esi
	jmp	.L3
.L224:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %ebx
	negl	%ebx
	addl	$2, %esi
	jmp	.L3
.L31:
	movl	%ebx, -176(%ebp)
	movl	%esi, %edx
	sarl	%edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	leal	(%edx,%eax), %ebx
	leal	(%ebx,%ebx), %edx
	movl	%ebx, %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	cwtl
	leal	(%edx,%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L30:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	leal	(%eax,%esi), %ebx
	addl	$1, %esi
	jmp	.L3
.L125:
	movl	-180(%ebp), %edx
	addl	$44, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$11, -152(%ebp)
	jmp	.L323
.L124:
	movl	-180(%ebp), %edx
	addl	$40, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$10, -152(%ebp)
	jmp	.L323
.L123:
	movl	-180(%ebp), %edx
	addl	$36, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$9, -152(%ebp)
	jmp	.L323
.L122:
	movl	-180(%ebp), %edx
	addl	$32, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$8, -152(%ebp)
	jmp	.L323
.L121:
	movl	-180(%ebp), %edx
	addl	$28, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$7, -152(%ebp)
	jmp	.L323
.L120:
	movl	-180(%ebp), %edx
	addl	$24, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$6, -152(%ebp)
	jmp	.L323
.L119:
	movl	-180(%ebp), %edx
	addl	$20, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$5, -152(%ebp)
	jmp	.L323
.L118:
	movl	-180(%ebp), %edx
	addl	$16, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$4, -152(%ebp)
	jmp	.L323
.L117:
	movl	-180(%ebp), %edx
	addl	$12, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$3, -152(%ebp)
.L323:
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	movl	-180(%ebp), %edx
	addl	$4, %edx
	leal	2(%esi), %eax
	movl	%eax, (%edx)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %esi
	movl	-180(%ebp), %eax
	addl	$8, %eax
	movl	%esi, (%eax)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	%ebx, (%eax)
	jmp	.L2
.L93:
	movl	-180(%ebp), %edx
	addl	$44, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$11, -152(%ebp)
	jmp	.L324
.L92:
	movl	-180(%ebp), %edx
	addl	$40, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$10, -152(%ebp)
	jmp	.L324
.L91:
	movl	-180(%ebp), %edx
	addl	$36, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$9, -152(%ebp)
	jmp	.L324
.L90:
	movl	-180(%ebp), %edx
	addl	$32, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$8, -152(%ebp)
	jmp	.L324
.L89:
	movl	-180(%ebp), %edx
	addl	$28, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$7, -152(%ebp)
	jmp	.L324
.L88:
	movl	-180(%ebp), %edx
	addl	$24, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$6, -152(%ebp)
	jmp	.L324
.L87:
	movl	-180(%ebp), %edx
	addl	$20, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$5, -152(%ebp)
	jmp	.L324
.L86:
	movl	-180(%ebp), %edx
	addl	$16, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$4, -152(%ebp)
	jmp	.L324
.L85:
	movl	-180(%ebp), %edx
	addl	$12, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$3, -152(%ebp)
.L324:
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	movl	-180(%ebp), %edx
	addl	$4, %edx
	leal	1(%esi), %eax
	movl	%eax, (%edx)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-188(%ebp), %eax
	movl	(%eax), %esi
	movl	-180(%ebp), %eax
	addl	$8, %eax
	movl	%esi, (%eax)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	%ebx, (%eax)
	jmp	.L2
.L61:
	movl	-180(%ebp), %edx
	addl	$44, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$11, -152(%ebp)
	jmp	.L325
.L60:
	movl	-180(%ebp), %edx
	addl	$40, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$10, -152(%ebp)
	jmp	.L325
.L59:
	movl	-180(%ebp), %edx
	addl	$36, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$9, -152(%ebp)
	jmp	.L325
.L58:
	movl	-180(%ebp), %edx
	addl	$32, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$8, -152(%ebp)
	jmp	.L325
.L57:
	movl	-180(%ebp), %edx
	addl	$28, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$7, -152(%ebp)
	jmp	.L325
.L56:
	movl	-180(%ebp), %edx
	addl	$24, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$6, -152(%ebp)
	jmp	.L325
.L55:
	movl	-180(%ebp), %edx
	addl	$20, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$5, -152(%ebp)
	jmp	.L325
.L54:
	movl	-180(%ebp), %edx
	addl	$16, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$4, -152(%ebp)
	jmp	.L325
.L53:
	movl	-180(%ebp), %edx
	addl	$12, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$3, -152(%ebp)
.L325:
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	movl	-180(%ebp), %edx
	addl	$4, %edx
	leal	1(%esi), %eax
	movl	%eax, (%edx)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %esi
	movl	-180(%ebp), %eax
	addl	$8, %eax
	movl	%esi, (%eax)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	%ebx, (%eax)
	jmp	.L2
.L29:
	movl	-180(%ebp), %edx
	addl	$44, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$11, -152(%ebp)
	jmp	.L326
.L28:
	movl	-180(%ebp), %edx
	addl	$40, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$10, -152(%ebp)
	jmp	.L326
.L27:
	movl	-180(%ebp), %edx
	addl	$36, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$9, -152(%ebp)
	jmp	.L326
.L26:
	movl	-180(%ebp), %edx
	addl	$32, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$8, -152(%ebp)
	jmp	.L326
.L25:
	movl	-180(%ebp), %edx
	addl	$28, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$7, -152(%ebp)
	jmp	.L326
.L24:
	movl	-180(%ebp), %edx
	addl	$24, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$6, -152(%ebp)
	jmp	.L326
.L23:
	movl	-180(%ebp), %edx
	addl	$20, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$5, -152(%ebp)
	jmp	.L326
.L22:
	movl	-180(%ebp), %edx
	addl	$16, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$4, -152(%ebp)
	jmp	.L326
.L21:
	movl	-180(%ebp), %edx
	addl	$12, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	addl	$3, -152(%ebp)
.L326:
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	movl	-180(%ebp), %eax
	addl	$4, %eax
	movl	%esi, (%eax)
	movl	%ebx, %esi
	movl	-180(%ebp), %eax
	addl	$8, %eax
	movl	%esi, (%eax)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	-176(%ebp), %ebx
	movl	%ebx, (%eax)
	jmp	.L2
.L50:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, -28(%ebp)
	movl	-28(%ebp), %eax
	sall	$2, %eax
	movl	-180(%ebp), %edx
	addl	%eax, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	movl	-28(%ebp), %eax
	addl	%eax, -152(%ebp)
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	movl	-180(%ebp), %edx
	addl	$4, %edx
	leal	1(%esi), %eax
	movl	%eax, (%edx)
	movl	%ebx, %esi
	movl	-180(%ebp), %eax
	addl	$8, %eax
	movl	%esi, (%eax)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	-176(%ebp), %ebx
	movl	%ebx, (%eax)
	jmp	.L2
.L51:
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, -28(%ebp)
	movl	-28(%ebp), %eax
	sall	$2, %eax
	movl	-180(%ebp), %edx
	addl	%eax, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	movl	-28(%ebp), %eax
	addl	%eax, -152(%ebp)
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	movl	-180(%ebp), %edx
	addl	$4, %edx
	leal	2(%esi), %eax
	movl	%eax, (%edx)
	movl	%ebx, %esi
	movl	-180(%ebp), %eax
	addl	$8, %eax
	movl	%esi, (%eax)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	-176(%ebp), %ebx
	movl	%ebx, (%eax)
	jmp	.L2
.L52:
	movl	%esi, %eax
	addl	$3, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$2, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, -28(%ebp)
	movl	-28(%ebp), %eax
	sall	$2, %eax
	movl	-180(%ebp), %edx
	addl	%eax, %edx
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	%eax, (%edx)
	movl	-28(%ebp), %eax
	addl	%eax, -152(%ebp)
	movl	-152(%ebp), %eax
	sall	$2, %eax
	movl	-112(%ebp), %edi
	addl	%eax, %edi
	movl	%edi, -180(%ebp)
	movl	-180(%ebp), %edx
	addl	$4, %edx
	leal	4(%esi), %eax
	movl	%eax, (%edx)
	movl	%ebx, %esi
	movl	-180(%ebp), %eax
	addl	$8, %eax
	movl	%esi, (%eax)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	-176(%ebp), %ebx
	movl	%ebx, (%eax)
	jmp	.L2
.L46:
	cmpl	%ebx, -176(%ebp)
	jne	.L327
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L327:
	addl	$1, %esi
	jmp	.L3
.L47:
	cmpl	%ebx, -176(%ebp)
	je	.L354
.L328:
	addl	$1, %esi
	jmp	.L3
.L48:
	testl	%ebx, %ebx
	jne	.L329
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L329:
	addl	$1, %esi
	jmp	.L3
.L49:
	testl	%ebx, %ebx
	je	.L355
.L330:
	addl	$1, %esi
	jmp	.L3
.L78:
	cmpl	%ebx, -176(%ebp)
	je	.L331
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L331:
	addl	$1, %esi
	jmp	.L3
.L79:
	cmpl	%ebx, -176(%ebp)
	jne	.L356
.L332:
	addl	$1, %esi
	jmp	.L3
.L80:
	testl	%ebx, %ebx
	je	.L333
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L333:
	addl	$1, %esi
	jmp	.L3
.L81:
	testl	%ebx, %ebx
	jne	.L357
.L334:
	addl	$1, %esi
	jmp	.L3
.L110:
	cmpl	%ebx, -176(%ebp)
	jge	.L335
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L335:
	addl	$1, %esi
	jmp	.L3
.L111:
	cmpl	%ebx, -176(%ebp)
	jl	.L358
.L336:
	addl	$1, %esi
	jmp	.L3
.L112:
	testl	%ebx, %ebx
	jns	.L337
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L337:
	addl	$1, %esi
	jmp	.L3
.L113:
	testl	%ebx, %ebx
	js	.L359
.L338:
	addl	$1, %esi
	jmp	.L3
.L142:
	cmpl	%ebx, -176(%ebp)
	jle	.L339
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L339:
	addl	$1, %esi
	jmp	.L3
.L143:
	cmpl	%ebx, -176(%ebp)
	jg	.L360
.L340:
	addl	$1, %esi
	jmp	.L3
.L144:
	testl	%ebx, %ebx
	jle	.L341
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L341:
	addl	$1, %esi
	jmp	.L3
.L145:
	testl	%ebx, %ebx
	jg	.L361
.L342:
	addl	$1, %esi
	jmp	.L3
.L174:
	cmpl	%ebx, -176(%ebp)
	jg	.L343
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L343:
	addl	$1, %esi
	jmp	.L3
.L175:
	cmpl	%ebx, -176(%ebp)
	jle	.L362
.L344:
	addl	$1, %esi
	jmp	.L3
.L176:
	testl	%ebx, %ebx
	jg	.L345
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L345:
	addl	$1, %esi
	jmp	.L3
.L177:
	testl	%ebx, %ebx
	jle	.L363
.L346:
	addl	$1, %esi
	jmp	.L3
.L206:
	cmpl	%ebx, -176(%ebp)
	jl	.L347
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L347:
	addl	$1, %esi
	jmp	.L3
.L207:
	cmpl	%ebx, -176(%ebp)
	jge	.L364
.L348:
	addl	$1, %esi
	jmp	.L3
.L208:
	testl	%ebx, %ebx
	js	.L349
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L349:
	addl	$1, %esi
	jmp	.L3
.L209:
	testl	%ebx, %ebx
	jns	.L365
.L350:
	addl	$1, %esi
	jmp	.L3
.L204:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movsbl	%al, %eax
	addl	%eax, %esi
	jmp	.L3
.L354:
	nop
	jmp	.L205
.L355:
	nop
	jmp	.L205
.L356:
	nop
	jmp	.L205
.L357:
	nop
	jmp	.L205
.L358:
	nop
	jmp	.L205
.L359:
	nop
	jmp	.L205
.L360:
	nop
	jmp	.L205
.L361:
	nop
	jmp	.L205
.L362:
	nop
	jmp	.L205
.L363:
	nop
	jmp	.L205
.L364:
	nop
	jmp	.L205
.L365:
	nop
.L205:
	movl	%esi, %edx
	sarl	%edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	leal	(%edx,%eax), %esi
	leal	(%esi,%esi), %edx
	movl	%esi, %eax
	addl	%eax, %eax
	addl	-112(%ebp), %eax
	movzwl	(%eax), %eax
	cwtl
	leal	(%edx,%eax), %esi
	jmp	.L3
.L222:
	movl	-180(%ebp), %eax
	addl	$48, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L221:
	movl	-180(%ebp), %eax
	addl	$44, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L220:
	movl	-180(%ebp), %eax
	addl	$40, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L219:
	movl	-180(%ebp), %eax
	addl	$36, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L218:
	movl	-180(%ebp), %eax
	addl	$32, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L217:
	movl	-180(%ebp), %eax
	addl	$28, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L216:
	movl	-180(%ebp), %eax
	addl	$24, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L215:
	movl	-180(%ebp), %eax
	addl	$20, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L214:
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L213:
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	jmp	.L3
.L210:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	addl	$1, %esi
	jmp	.L3
.L211:
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	addl	$2, %esi
	jmp	.L3
.L212:
	movl	%esi, %eax
	addl	$3, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$2, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-180(%ebp), %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	addl	$4, %esi
	jmp	.L3
.L133:
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	addl	$2, %esi
	jmp	.L3
.L101:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-188(%ebp), %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	addl	$1, %esi
	jmp	.L3
.L69:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	addl	%eax, %ebx
	addl	$1, %esi
	jmp	.L3
.L231:
	addl	$5, %ebx
	jmp	.L3
.L230:
	addl	$4, %ebx
	jmp	.L3
.L229:
	addl	$3, %ebx
	jmp	.L3
.L228:
	addl	$2, %ebx
	jmp	.L3
.L227:
	addl	$1, %ebx
	jmp	.L3
.L241:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	addl	%eax, %ebx
	addl	$1, %esi
	jmp	.L3
.L242:
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	addl	%eax, %ebx
	addl	$2, %esi
	jmp	.L3
.L243:
	movl	%esi, %eax
	addl	$3, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$2, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	addl	%eax, %ebx
	addl	$4, %esi
	jmp	.L3
.L254:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	subl	%eax, %ebx
	addl	$1, %esi
	jmp	.L3
.L255:
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	subl	%eax, %ebx
	addl	$2, %esi
	jmp	.L3
.L198:
	subl	$4, %ebx
	jmp	.L3
.L197:
	subl	$3, %ebx
	jmp	.L3
.L196:
	subl	$2, %ebx
	jmp	.L3
.L195:
	subl	$1, %ebx
	jmp	.L3
.L253:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$48, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L252:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$44, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L251:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$40, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L250:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$36, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L249:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$32, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L248:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$28, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L247:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$24, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L246:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$20, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L245:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L244:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L263:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$24, %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L262:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$20, %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L261:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L260:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L266:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$20, %eax
	movl	(%eax), %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L265:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L264:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L268:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	addl	$3, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L267:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	addl	$3, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L270:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	addl	$4, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L269:
	movl	%ebx, -176(%ebp)
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	addl	$4, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L127:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$2, %esi
	jmp	.L3
.L128:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$2, %esi
	jmp	.L3
.L129:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$2, %esi
	jmp	.L3
.L95:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-188(%ebp), %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L96:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-188(%ebp), %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L97:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-188(%ebp), %eax
	movl	(%eax), %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L63:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L64:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L65:
	movl	%ebx, -176(%ebp)
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	addl	$1, %esi
	jmp	.L3
.L126:
	movl	%esi, %eax
	addl	$1, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	movl	%eax, %edx
	sall	$8, %edx
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	orl	%edx, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$2, %esi
	jmp	.L3
.L94:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-188(%ebp), %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$1, %esi
	jmp	.L3
.L62:
	movl	%esi, %eax
	addl	-112(%ebp), %eax
	movzbl	(%eax), %eax
	movzbl	%al, %eax
	sall	$2, %eax
	addl	-184(%ebp), %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	addl	$1, %esi
	jmp	.L3
.L172:
	movl	-180(%ebp), %eax
	addl	$20, %eax
	movl	(%eax), %eax
	addl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %edx
	movl	%edx, (%eax)
	jmp	.L3
.L171:
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	addl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %ecx
	movl	%ecx, (%eax)
	jmp	.L3
.L170:
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	addl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %edi
	movl	%edi, (%eax)
	jmp	.L3
.L238:
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L237:
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L240:
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L239:
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	jmp	.L3
.L236:
	movl	-180(%ebp), %eax
	addl	$28, %eax
	movl	(%eax), %eax
	addl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L235:
	movl	-180(%ebp), %eax
	addl	$24, %eax
	movl	(%eax), %eax
	addl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L234:
	movl	-180(%ebp), %eax
	addl	$20, %eax
	movl	(%eax), %eax
	addl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L233:
	movl	-180(%ebp), %eax
	addl	$16, %eax
	movl	(%eax), %eax
	addl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L232:
	movl	-180(%ebp), %eax
	addl	$12, %eax
	movl	(%eax), %eax
	addl	%ebx, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	(%eax), %ebx
	jmp	.L3
.L352:
	nop
.L351:
.L4:
	movl	$4, -144(%ebp)
.L6:
	movl	$0, tracing
	movl	8(%ebp), %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%ebx, (%eax)
	movl	8(%ebp), %eax
	addl	$1, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-176(%ebp), %edx
	movl	%edx, (%eax)
	movl	8(%ebp), %eax
	addl	$2, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-156(%ebp), %edx
	movl	%edx, (%eax)
	movl	8(%ebp), %eax
	addl	$3, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-152(%ebp), %edx
	sall	$2, %edx
	movl	%edx, (%eax)
	movl	8(%ebp), %eax
	addl	$4, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-108(%ebp), %edx
	sall	$2, %edx
	movl	%edx, (%eax)
	movl	8(%ebp), %eax
	addl	$5, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-104(%ebp), %edx
	movl	%edx, (%eax)
	movl	8(%ebp), %eax
	addl	$6, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	%esi, (%eax)
	movl	8(%ebp), %eax
	addl	$7, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-148(%ebp), %edx
	movl	%edx, (%eax)
	movl	8(%ebp), %eax
	addl	$8, %eax
	sall	$2, %eax
	addl	-112(%ebp), %eax
	movl	-100(%ebp), %edx
	movl	%edx, (%eax)
	movl	-144(%ebp), %eax
	addl	$204, %esp
	popl	%ebx
	.cfi_restore 3
	popl	%esi
	.cfi_restore 6
	popl	%edi
	.cfi_restore 7
	popl	%ebp
	.cfi_def_cfa 4, 4
	.cfi_restore 5
	ret
	.cfi_endproc
.LFE0:
	.size	interpret, .-interpret
	.ident	"GCC: (Ubuntu/Linaro 4.6.3-1ubuntu5) 4.6.3"
	.section	.note.GNU-stack,"",@progbits
