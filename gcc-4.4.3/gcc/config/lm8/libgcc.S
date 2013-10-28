/* Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Beyond Semiconductor (www.beyondsemi.com)

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define ENTRY(sym)	\
	.global	sym	@\
	sym:

/* ABI dependant things */

#define STACK_ALIGN	1

#if defined(__CMODEL_SMALL__)

#define TMP_REGNUM1	r12
#define TMP_REGNUM2	r15

#define STACK_ALLOC(space)	\
	addi	r14,-(((space) + STACK_ALIGN - 1) & -STACK_ALIGN)	@\
@\
	mov	TMP_REGNUM1,r14

#define PP_ADDC(val)

#define PUSH_REG(reg)	\
	sspi	reg,TMP_REGNUM1						@\
	addi	TMP_REGNUM1,1

#define STACK_FREE(space)	\
	addi	r14,((space) + STACK_ALIGN - 1) & -STACK_ALIGN

#define POP_REG(reg)	\
	lspi	reg,TMP_REGNUM1						@\
	addi	TMP_REGNUM1,_lo(1)

#define STACK_OFF(off)	\
	mov	TMP_REGNUM1,r14						@\
	addi	TMP_REGNUM1,(off)

#define TMP_REGNUM2_SIZE	0
#define PUSH_TMP_REGNUM2
#define POP_TMP_REGNUM2

#define PROLOGUE_TMP_REGNUM2
#define EPILOGUE_TMP_REGNUM2
#define PROLOGUE_TMP_REGNUM2_R10_R11
#define EPILOGUE_TMP_REGNUM2_R10_R11

#define LOAD_TEMP_SP		\
	movi	r12,__irq_stack-14
	
#define ADD_TEMP_SP(val)	\
	addi	r12,(val)

#define PUSH_SP

#define SET_SP			\
	movi	r14,__irq_stack-14

#define POP_SP			\
	lspi	r14,r13

#define IRQ_STACK_SIZE	0x20

#elif defined(__CMODEL_MEDIUM__)

#define TMP_REGNUM1	r12
#define TMP_REGNUM2	r15

#define STACK_ALLOC(space)	\
	addi	r8,_lo(-(((space) + STACK_ALIGN - 1) & -STACK_ALIGN))	@\
	addic	r9,_hi(-(((space) + STACK_ALIGN - 1) & -STACK_ALIGN))	@\
@\
	/* Load the page pointers */					@\
	mov	r13,r9							@\
	mov	TMP_REGNUM1,r8

#define PP_ADDC(val)		\
	addic	r13,_lo(val)

/* Saves a register to stack.  Assumes the page pointers are setup already */
#define PUSH_REG(reg)	\
	sspi	reg,TMP_REGNUM1						@\
	addi	TMP_REGNUM1,_lo(1)					@\
	addic	r13,_hi(1)

#define STACK_FREE(space)	\
	addi	r8,_lo(((space) + STACK_ALIGN - 1) & -STACK_ALIGN)	@\
	addic	r9,_hi(((space) + STACK_ALIGN - 1) & -STACK_ALIGN)	@\

#define POP_REG(reg)	\
	lspi	reg,TMP_REGNUM1						@\
	addi	TMP_REGNUM1,_lo(1)					@\
	addic	r13,_hi(1)

#define STACK_OFF(off)	\
	mov	r13,r9							@\
	mov	TMP_REGNUM1,r8						@\
	addi	TMP_REGNUM1,_lo(off)					@\
	addic	r13,_hi(off)

#define TMP_REGNUM2_SIZE	0
#define PUSH_TMP_REGNUM2

#define POP_TMP_REGNUM2	

#define PROLOGUE_TMP_REGNUM2
#define EPILOGUE_TMP_REGNUM2
#define PROLOGUE_TMP_REGNUM2_R10_R11	\
	STACK_ALLOC(2)							@\
	PUSH_REG(r10)							@\
	PUSH_REG(r11)

#define EPILOGUE_TMP_REGNUM2_R10_R11	\
	STACK_OFF(0)							@\
	POP_REG(r10)							@\
	POP_REG(r11)							@\
	STACK_FREE(2)

#define LOAD_TEMP_SP		\
	movi	r12,_lo(__irq_stack-14)					@\
	movi	r13,_hi(__irq_stack-14)
	
#define ADD_TEMP_SP(val)	\
	addi	r12,_lo(val)						@\
	addic	r13,_hi(val)

#define PUSH_SP			\
	sspi	r8,r12							@\
	addi	r12,1							@\
	sspi	r9,r12

#define SET_SP			\
	movi	r8,_lo(__irq_stack-14)					@\
	movi	r9,_hi(__irq_stack-14)

#define POP_SP			\
	lspi	r8,r12							@\
	addi	r12,1							@\
	lspi	r9,r12

#define IRQ_STACK_SIZE	0x200

#elif defined(__CMODEL_LARGE__)

#define TMP_REGNUM1	r12
#define TMP_REGNUM2	r31

#define STACK_ALLOC(space)	\
	addi	r24,_lo(-(((space) + STACK_ALIGN - 1) & -STACK_ALIGN))	@\
	addic	r25,_hi(-(((space) + STACK_ALIGN - 1) & -STACK_ALIGN))	@\
	addic	r26,_higher(-(((space) + STACK_ALIGN - 1) & -STACK_ALIGN))	@\
	addic	r27,_highest(-(((space) + STACK_ALIGN - 1) & -STACK_ALIGN))	@\
@\
	/* Load the page pointers */					@\
	mov	r15,r27							@\
	mov	r14,r26							@\
	mov	r13,r25							@\
	mov	TMP_REGNUM1,r24

#define PP_ADDC(val)		\
	addic	r13,_lo(val)						@\
	addic	r14,_hi(val)						@\
	addic	r15,_hi(val)

/* Saves a register to stack.  Assumes the page pointers are setup already */
#define PUSH_REG(reg)	\
	sspi	reg,TMP_REGNUM1						@\
	addi	TMP_REGNUM1,_lo(1)					@\
	addic	r13,_hi(1)						@\
	addic	r14,_higher(1)						@\
	addic	r15,_highest(1)

#define STACK_FREE(space)	\
	addi	r24,_lo(((space) + STACK_ALIGN - 1) & -STACK_ALIGN)	@\
	addic	r25,_hi(((space) + STACK_ALIGN - 1) & -STACK_ALIGN)	@\
	addic	r26,_higher(((space) + STACK_ALIGN - 1) & -STACK_ALIGN)	@\
	addic	r27,_highest(((space) + STACK_ALIGN - 1) & -STACK_ALIGN)

#define POP_REG(reg)	\
	lspi	reg,TMP_REGNUM1						@\
	addi	TMP_REGNUM1,_lo(1)					@\
	addic	r13,_hi(1)						@\
	addic	r14,_higher(1)						@\
	addic	r15,_highest(1)

#define STACK_OFF(off)	\
	mov	r15,r27							@\
	mov	r14,r26							@\
	mov	r13,r25							@\
	mov	TMP_REGNUM1,r24						@\
	addi	TMP_REGNUM1,_lo(off)					@\
	addic	r13,_hi(off)						@\
	addic	r14,_higher(off)					@\
	addic	r15,_highest(off)

#define TMP_REGNUM2_SIZE	1
#define PUSH_TMP_REGNUM2	\
	PUSH_REG(r31)

#define POP_TMP_REGNUM2		\
	POP_REG(r31)

#define PROLOGUE_TMP_REGNUM2	\
	STACK_ALLOC(1)							@\
	PUSH_REG(TMP_REGNUM2)

#define EPILOGUE_TMP_REGNUM2	\
	STACK_OFF(0)							@\
	POP_REG(TMP_REGNUM2)						@\
	STACK_FREE(1)

#define PROLOGUE_TMP_REGNUM2_R10_R11	PROLOGUE_TMP_REGNUM2
#define EPILOGUE_TMP_REGNUM2_R10_R11	EPILOGUE_TMP_REGNUM2

#define LOAD_TEMP_SP		\
	movi	r12,_lo(__irq_stack-14)					@\
	movi	r13,_hi(__irq_stack-14)					@\
	movi	r14,_higher(__irq_stack-14)				@\
	movi	r15,_highest(__irq_stack-14)
	
#define ADD_TEMP_SP(val)	\
	addi	r12,_lo(val)						@\
	addic	r13,_hi(val)						@\
	addic	r14,_higher(val)					@\
	addic	r15,_highest(val)

#define PUSH_SP			\
	sspi	r24,r12							@\
	addi	r12,1							@\
	sspi	r25,r12							@\
	addi	r12,1							@\
	sspi	r26,r12							@\
	addi	r12,1							@\
	sspi	r27,r12

#define SET_SP			\
	movi	r24,_lo(__irq_stack-14)					@\
	movi	r25,_hi(__irq_stack-14)					@\
	movi	r26,_higher(__irq_stack-14)				@\
	movi	r27,_highest(__irq_stack-14)

#define POP_SP			\
	lspi	r24,r12							@\
	addi	r12,1							@\
	lspi	r25,r12							@\
	addi	r12,1							@\
	lspi	r26,r12							@\
	addi	r12,1							@\
	lspi	r27,r12

#define IRQ_STACK_SIZE	0x200

#endif

#ifdef L_ashlqi3
/* Shift r0 left by r1 bits */
ENTRY(__ashlqi3)
	testi	r1,0xff
	bz	2f
1:	clrc
	rolc	r0,r0
	addi	r1,-1
	bnz	1b
2:	ret
#endif

#ifdef L_ashlhi3
/* Shift { r1, r0 } left by r2 bits */
ENTRY(__ashlhi3)
	testi	r2,0xff
	bz	2f
1:	clrc
	rolc	r0,r0
	rolc	r1,r1
	addi	r2,-1
	bnz	1b
2:	ret
#endif

#ifdef L_ashlsi3
/* Shift { r3, r2, r1, r0 } left by r4 bits */
ENTRY(__ashlsi3)
	testi	r4,0xff
	bz	2f
1:	clrc
	rolc	r0,r0
	rolc	r1,r1
	rolc	r2,r2
	rolc	r3,r3
	addi	r4,-1
	bnz	1b
2:	ret
#endif

#ifdef L_ashrqi3
/* Shift r0 right (arithmetically) by r1 bits */
ENTRY(__ashrqi3)
	testi	r1,0xff
	bz	2f
1:	cmpi	r0,0x80	/* Set carry to the negated value of r0[7] */
	rorc	r0,r0
	xori	r0,0x80
	addi	r1,-1
	bnz	1b
2:	ret
#endif

#ifdef L_ashrhi3
/* Shift { r1, r0 } right (arithmetically) by r2 bits */
ENTRY(__ashrhi3)
	testi	r2,0xff
	bz	2f
1:	cmpi	r1,0x80	/* Set carry to the negated value of r1[7] */
	rorc	r1,r1
	xori	r1,0x80
	rorc	r0,r0
	addi	r2,-1
	bnz	1b
2:	ret
#endif

#ifdef L_ashrsi3
/* Shift { r3, r2, r1, r0 } right (arithmetically) by r4 bits */
ENTRY(__ashrsi3)
	testi	r4,0xff
	bz	2f
1:	cmpi	r3,0x80 /* Set carry to the negated value of r3[7] */
	rorc	r3,r3
	xori	r3,0x80
	rorc	r2,r2
	rorc	r1,r1
	rorc	r0,r0
	addi	r4,-1
	bnz	1b
2:	ret
#endif

#ifdef L_lshrqi3
/* Shift r0 right (logically) by r1 bits */
ENTRY(__lshrqi3)
	testi	r1,0xff
	bz	2f
1:	clrc
	rorc	r0,r0
	addi	r1,-1
	bnz	1b
2:	ret
#endif

#ifdef L_lshrhi3
/* Shift { r1, r0 } right (logically) by r2 bits */
ENTRY(__lshrhi3)
	testi	r2,0xff
	bz	2f
1:	clrc
	rorc	r1,r1
	rorc	r0,r0
	addi	r2,-1
	bnz	1b
2:	ret
#endif

#ifdef L_lshrsi3
/* Shift { r3, r2, r1, r0 } right (logically) by r4 bits */
ENTRY(__lshrsi3)
	testi	r4,0xff
	bz	2f
1:	clrc
	rorc	r3,r3
	rorc	r2,r2
	rorc	r1,r1
	rorc	r0,r0
	addi	r4,-1
	bnz	1b
2:	ret
#endif

#ifdef L_parityqi2
ENTRY(__parityqi2)
	movi	r1,8
	movi	r2,0

1:
	xor	r2,r0

	ror	r0,r0

	addi	r1,-1
	bnz	1b

	mov	r0,r2
	andi	r0,1
	ret
#endif

#ifdef L_parityhi2
ENTRY(__parityhi2)
	xor	r0,r1
	b	__parityqi2
#endif

#ifdef L_paritysi2
ENTRY(__paritysi2)
	xor	r0,r1
	xor	r0,r2
	xor	r0,r3
	b	__parityqi2
#endif

#ifdef L_popcountqi2
ENTRY(__popcountqi2)
	movi	r1,0
	movi	r2,8

1:
	clrc
	rolc	r0,r0

	addic	r1,0
	addi	r2,-1
	bnz	1b

	mov	r0,r1
	ret
#endif

#ifdef L_popcounthi2
ENTRY(__popcounthi2)
	movi	r2,0
	movi	r3,16

1:
	clrc
	rolc	r0,r0
	rolc	r1,r1

	addic	r2,0
	addi	r3,-1
	bnz	1b

	mov	r0,r2
	movi	r1,0
	ret
#endif

#ifdef L_popcountsi2
ENTRY(__popcountsi2)
	movi	r4,0
	movi	r5,32

1:
	clrc
	rolc	r0,r0
	rolc	r1,r1
	rolc	r2,r2
	rolc	r3,r3

	addic	r4,0
	addi	r5,-1
	bnz	1b

	mov	r0,r4
	movi	r1,0
	ret
#endif

#ifdef L_ffsqi2
ENTRY(__ffsqi2)
	movi	r1,0

	testi	r0,0xff
	bz	2f

1:
	addi	r1,1

	clrc
	rorc	r0,r0
	bnc	1b
2:
	mov	r0,r1
	ret
#endif

#ifdef L_ffshi2
ENTRY(__ffshi2)
	movi	r2,_lo(0)
	movi	r3,_hi(0)

	mov	TMP_REGNUM1,r0
	or	TMP_REGNUM1,r1
	bz	2f

1:
	addi	r2,_lo(1)
	addic	r3,_hi(1)

	clrc
	rorc	r1,r1
	rorc	r0,r0
	bnc	1b
2:
	mov	r0,r2
	mov	r1,r3
	ret
#endif

#ifdef L_ffssi2
ENTRY(__ffssi2)
	movi	r4,_lo(0)
	movi	r5,_hi(0)

	mov	r6,r0
	or	r6,r1
	or	r6,r2
	or	r6,r3
	bz	2f

1:
	addi	r3,_lo(1)
	addic	r4,_hi(1)

	clrc
	rorc	r3,r3
	rorc	r2,r2
	rorc	r1,r1
	rorc	r0,r0
	bnc	1b
2:
	mov	r0,r4
	mov	r1,r5
	ret
#endif

#ifdef L_ctzqi2
ENTRY(__ctzqi2)
	testi	r0,0xff
	bz	2f

	movi	r1,-1

1:
	addi	r1,1

	clrc
	rorc	r0,r0
	bnc	1b

	mov	r0,r1
2:	ret
#endif

#ifdef L_ctzhi2
ENTRY(__ctzhi2)
	mov	r4,r0
	or	r4,r1
	bz	2f

	movi	r2,_lo(-1)
	movi	r3,_hi(-1)

1:
	addi	r2,_lo(1)
	addic	r3,_hi(1)

	clrc
	rorc	r1,r1
	rorc	r0,r0
	bnc	1b

	mov	r0,r2
	mov	r1,r3
2:	ret
#endif

#ifdef L_ctzsi2
ENTRY(__ctzsi2)
	mov	r4,r0
	or	r4,r1
	or	r4,r2
	or	r4,r3
	bz	2f

	movi	r4,_lo(-1)
	movi	r5,_hi(-1)

1:
	addi	r3,_lo(1)
	addic	r4,_hi(1)

	clrc
	rorc	r3,r3
	rorc	r2,r2
	rorc	r1,r1
	rorc	r0,r0
	bnc	1b

	mov	r0,r4
	mov	r1,r5
2:	ret
#endif

#ifdef L_clzqi2
/* Count the leading zeroes in r0 */
ENTRY(__clzqi2)
	testi	r0,0xff
	bz	2f

	movi	r1,-1

1:
	addi	r1,1

	clrc
	rolc	r0,r0
	bnc	1b

	mov	r0,r1
2:	ret
#endif

#ifdef L_clzhi2
/* Count the leading zeroes in { r1, r0 } */
ENTRY(__clzhi2)
	mov	r2,r0
	or	r4,r1
	bz	2f

	movi	r2,_lo(-1)
	movi	r3,_hi(-1)

1:
	addi	r2,_lo(1)
	addic	r3,_hi(1)

	clrc
	rolc	r0,r0
	rolc	r1,r1
	bnc	1b

	mov	r0,r2
	mov	r1,r3
2:	ret
#endif

#ifdef L_clzsi2
/* Count the leading zeroes in { r3, r2, r1, r0 } */
ENTRY(__clzsi2)
	mov	r4,r0
	or	r4,r1
	or	r4,r2
	or	r4,r3
	bz	2f

	movi	r4,_lo(-1)
	movi	r5,_hi(-1)

1:
	addi	r3,_lo(1)
	addic	r4,_hi(1)

	clrc
	rolc	r0,r0
	rolc	r1,r1
	rolc	r2,r2
	rolc	r3,r3
	bnc	1b

	mov	r0,r4
	mov	r1,r5
2:	ret
#endif

#ifdef L_mulqi3
/* Multiply r0 by r1 and return the result in r0.  r2 is temporary */
ENTRY(__mulqi3)
	movi	r2,0
1:	testi	r0,0x01
	bz	2f
	add	r2,r1
2:	clrc
	rolc	r1,r1
	clrc
	rorc	r0,r0
	bnz	1b
	mov	r0,r2
	ret
#endif

#ifdef L_mulhi3
/* Multiply { r1, r0 } by { r3, r2 } and return the result in { r1, r0 }.
 * r4 and r5 are temporary */
ENTRY(__mulhi3)
	movi	r4,0
	movi	r5,0
1:	testi	r0,0x01
	bz	2f
	add	r4,r2
	addc	r5,r3
2:	clrc
	rolc	r2,r2
	rolc	r3,r3
	clrc
	rorc	r1,r1
	rorc	r0,r0
	bnz	1b
	mov	r0,r4
	mov	r1,r5
	ret
#endif

#ifdef L_mulsi3
/* Multiply { r3, r2, r1, r0 } by { r7, r6, r5, r4 } and return the
 * result in { r3,r2, r1, r0 } */
ENTRY(__mulsi3)
	PROLOGUE_TMP_REGNUM2_R10_R11

	movi	TMP_REGNUM1,0
	movi	TMP_REGNUM2,0
	movi	r10,0
	movi	r11,0

1:	testi	r0,0x01
	bz	2f

	add	TMP_REGNUM1,r4
	addc	TMP_REGNUM2,r5
	addc	r10,r6
	addc	r11,r7

2:	clrc
	rolc	r4,r4
	rolc	r5,r5
	rolc	r6,r6
	rolc	r7,r7
	clrc
	rorc	r3,r3
	rorc	r2,r2
	rorc	r1,r1
	rorc	r0,r0
	bnz	1b

	mov	r0,TMP_REGNUM1
	mov	r1,TMP_REGNUM2
	mov	r2,r10
	mov	r3,r11

	EPILOGUE_TMP_REGNUM2_R10_R11
	ret
#endif

#ifdef L_udivqi3
/* Divide r0 by r1, place the quotient r0 and the remainder in r2 */
ENTRY(__udivqi3)
	movi	r3,8
	movi	r2,0
	movi	r4,0

1:	clrc
	rolc	r0,r0
	rolc	r2,r2

	clrc
	rolc	r4,r4

	cmp	r2,r1
	bc	2f

	ori	r4,1
	sub	r2,r1

2:	addi	r3,-1
	bnz	1b

	mov	r0,r4
	ret
#endif

#ifdef L_umodqi3
ENTRY(__umodqi3)
	call	__udivqi3
	mov	r0,r2
	ret
#endif

#ifdef L_divqi3
ENTRY(__divqi3)
	movi	r5,0

	cmpi	r0,0x80
	bc	1f
	xori	r0,0xff
	addi	r0,1

	xori	r5,1
1:
	cmpi	r1,0x80
	bc	1f
	xori	r1,0xff
	addi	r1,1

	xori	r5,1
1:
	# r5 is not used by __udivqi3
	call	__udivqi3

	testi	r5,0xff
	bz	1f

	xori	r0,0xff
	addi	r0,1
1:
	ret
#endif

#ifdef L_modqi3
ENTRY(__modqi3)
	movi	r5,0

	cmpi	r0,0x80
	bc	1f
	xori	r0,0xff
	addi	r0,1

	xori	r5,1
1:
	cmpi	r1,0x80
	bc	1f
	xori	r1,0xff
	addi	r1,1

	xori	r5,1
1:
	# r5 is not used by __udivqi3
	call	__udivqi3

	testi	r5,0xff
	bz	1f

	xori	r2,0xff
	addi	r2,1

1:
	mov	r0,r2

	ret
#endif

#ifdef L_udivhi3
/* Divide { r1, r0 } by { r3, r2 }, place the result in { r1, r0 } and
 * the remainder into { r5, r4 }. */
ENTRY(__udivhi3)
	movi	TMP_REGNUM1,16

	movi	r4,0
	movi	r5,0

	movi	r6,0
	movi	r7,0

1:	clrc
	rolc	r0,r0
	rolc	r1,r1
	rolc	r4,r4
	rolc	r5,r5

	clrc
	rolc	r6,r6
	rolc	r7,r7

	cmp	r5,r3
	bc	2f
	bnz	3f
	cmp	r4,r2
	bc	2f

3:	ori	r6,1
	sub	r4,r2
	subc	r5,r3

2:	addi	TMP_REGNUM1,-1
	bnz	1b

	mov	r0,r6
	mov	r1,r7
	ret
#endif

#ifdef L_umodhi3
ENTRY(__umodhi3)
	call	__udivhi3

	mov	r0,r4
	mov	r1,r5

	ret
#endif

#ifdef L_divhi3
ENTRY(__divhi3)
	PROLOGUE_TMP_REGNUM2

	movi	TMP_REGNUM2,0

	cmpi	r1,0x80
	bc	1f
	xori	r0,0xff
	xori	r1,0xff
	addi	r0,_lo(1)
	addic	r1,_hi(1)

	xori	TMP_REGNUM2,1
1:
	cmpi	r3,0x80
	bc	1f
	xori	r2,0xff
	xori	r3,0xff
	addi	r2,_lo(1)
	addic	r3,_highest(1)

	xori	TMP_REGNUM2,1
1:
	# TMP_REGNUM2 is not used by __udivhi3
	call	__udivhi3

	testi	TMP_REGNUM2,0xff
	bz	1f

	xori	r0,0xff
	xori	r1,0xff
	addi	r0,_lo(1)
	addic	r1,_hi(1)
1:
	EPILOGUE_TMP_REGNUM2
	ret
#endif

#ifdef L_modhi3
ENTRY(__modhi3)
	PROLOGUE_TMP_REGNUM2

	movi	TMP_REGNUM2,0

	cmpi	r3,0x80
	bc	1f
	xori	r0,0xff
	xori	r1,0xff
	addi	r0,_lo(1)
	addic	r1,_hi(1)

	xori	TMP_REGNUM2,1
1:
	cmpi	r3,0x80
	bc	1f
	xori	r2,0xff
	xori	r3,0xff
	addi	r2,_lo(1)
	addic	r3,_hi(1)

	xori	TMP_REGNUM2,1
1:
	# TMP_REGNUM2 is not used by __udivhi3
	call	__udivhi3

	testi	TMP_REGNUM2,0xff
	bz	1f

	xori	r4,0xff
	xori	r5,0xff
	addi	r4,_lo(1)
	addic	r5,_hi(1)

1:
	mov	r0,r4
	mov	r1,r5
	EPILOGUE_TMP_REGNUM2
	ret
#endif

#ifdef L_udivsi3
/* Divide { r3, r2, r1, r0 } by { r7, r6, r5, r4 } and return the result
 * in { r3, r2, r1, r0 } and the remainder in { r7, r6, r5, r4 } */
ENTRY(__udivsi3)
	PROLOGUE_TMP_REGNUM2_R10_R11

	STACK_ALLOC(8)

	PUSH_REG(r0)
	PUSH_REG(r1)
	PUSH_REG(r2)
	PUSH_REG(r3)

	movi	r0,0
	movi	r1,0
	movi	r2,0
	movi	r3,0

	PUSH_REG(r0)
	PUSH_REG(r1)
	PUSH_REG(r2)
	PUSH_REG(r3)

	/* { r3, r2, r1, r0, MEM, MEM, MEM, MEM } is P */

	movi	r10,0

1:
	STACK_OFF(0)
	lspi	r11,TMP_REGNUM1
	clrc
	rolc	r11,r11
	sspi	r11,TMP_REGNUM1
	rorc	r11,r11	# Save carry

	STACK_OFF(1)
	rolc	r11,r11	# Restore carry
	lspi	r11,TMP_REGNUM1
	rolc	r11,r11
	sspi	r11,TMP_REGNUM1
	rorc	r11,r11	# Save carry

	STACK_OFF(2)
	rolc	r11,r11	# Restore carry
	lspi	r11,TMP_REGNUM1
	rolc	r11,r11
	sspi	r11,TMP_REGNUM1
	rorc	r11,r11	# Save carry

	STACK_OFF(3)
	rolc	r11,r11	# Restore carry
	lspi	r11,TMP_REGNUM1
	rolc	r11,r11
	sspi	r11,TMP_REGNUM1

	rolc	r0,r0
	rolc	r1,r1
	rolc	r2,r2
	rolc	r3,r3

	cmp	r3,r7
	bc	2f
	bnz	3f
	cmp	r2,r6
	bc	2f
	bnz	3f
	cmp	r1,r5
	bc	2f
	bnz	3f
	cmp	r0,r4
	bc	2f

3:	
	STACK_OFF(4)

	mov	r11,r10
	clrc
	rorc	r11,r11
	clrc
	rorc	r11,r11
	clrc
	rorc	r11,r11
	add	TMP_REGNUM1,r11
	PP_ADDC(0)

	movi	r11,0x80
	mov	TMP_REGNUM2,r10
	andi	TMP_REGNUM2,0x7
	bz	4f
5:	ror	r11,r11
	addi	TMP_REGNUM2,-1
	bnz	5b
4:

	lspi	TMP_REGNUM2,TMP_REGNUM1
	or	TMP_REGNUM2,r11
	sspi	TMP_REGNUM2,TMP_REGNUM1

	sub	r0,r4
	subc	r1,r5
	subc	r2,r6
	subc	r3,r7
2:
	addi	r10,1
	cmpi	r10,32
	bnz	1b

	mov	r4,r0
	mov	r5,r1
	mov	r6,r2
	mov	r7,r3

	STACK_OFF(4)
	POP_REG(r3)
	POP_REG(r2)
	POP_REG(r1)
	POP_REG(r0)

	STACK_FREE(8)

	EPILOGUE_TMP_REGNUM2_R10_R11
	ret
#endif

#ifdef L_umodsi3
ENTRY(__umodsi3)
	call	__udivsi3

	mov	r0,r4
	mov	r1,r5
	mov	r2,r6
	mov	r3,r7

	ret
#endif

#ifdef L_divsi3
ENTRY(__divsi3)
	STACK_ALLOC(TMP_REGNUM2_SIZE+1)
	PUSH_TMP_REGNUM2

	movi	TMP_REGNUM2,0

	cmpi	r3,0x80
	bc	1f
	xori	r0,0xff
	xori	r1,0xff
	xori	r2,0xff
	xori	r3,0xff
	addi	r0,_lo(1)
	addic	r1,_hi(1)
	addic	r2,_higher(1)
	addic	r3,_highest(1)

	xori	TMP_REGNUM2,1
1:
	cmpi	r7,0x80
	bc	1f
	xori	r4,0xff
	xori	r5,0xff
	xori	r6,0xff
	xori	r7,0xff
	addi	r4,_lo(1)
	addic	r5,_hi(1)
	addic	r6,_higher(1)
	addic	r7,_highest(1)

	xori	TMP_REGNUM2,1
1:
	sspi	TMP_REGNUM2,TMP_REGNUM1

	call	__udivsi3

	STACK_OFF(TMP_REGNUM2_SIZE)
	lspi	TMP_REGNUM2,TMP_REGNUM1

	testi	TMP_REGNUM2,0xff
	bz	1f

	xori	r0,0xff
	xori	r1,0xff
	xori	r2,0xff
	xori	r3,0xff
	addi	r0,_lo(1)
	addic	r1,_hi(1)
	addic	r2,_higher(1)
	addic	r3,_highest(1)

1:
	STACK_OFF(0)
	POP_TMP_REGNUM2
	STACK_FREE(TMP_REGNUM2_SIZE+1)

	ret
#endif

#ifdef L_modsi3
ENTRY(__modsi3)
	STACK_ALLOC(TMP_REGNUM2_SIZE+1)
	PUSH_TMP_REGNUM2

	movi	TMP_REGNUM2,0

	cmpi	r3,0x80
	bc	1f
	xori	r0,0xff
	xori	r1,0xff
	xori	r2,0xff
	xori	r3,0xff
	addi	r0,_lo(1)
	addic	r1,_hi(1)
	addic	r2,_higher(1)
	addic	r3,_highest(1)

	xori	TMP_REGNUM2,1
1:
	cmpi	r7,0x80
	bc	1f
	xori	r4,0xff
	xori	r5,0xff
	xori	r6,0xff
	xori	r7,0xff
	addi	r4,_lo(1)
	addic	r5,_hi(1)
	addic	r6,_higher(1)
	addic	r7,_highest(1)

	xori	TMP_REGNUM2,1
1:

	sspi	TMP_REGNUM2,TMP_REGNUM1

	call	__udivsi3

	STACK_OFF(TMP_REGNUM2_SIZE)
	lspi	TMP_REGNUM2,TMP_REGNUM1

	testi	TMP_REGNUM2,0xff
	bz	1f

	xori	r4,0xff
	xori	r5,0xff
	xori	r6,0xff
	xori	r7,0xff
	addi	r4,_lo(1)
	addic	r5,_hi(1)
	addic	r6,_higher(1)
	addic	r7,_highest(1)

1:
	mov	r0,r4
	mov	r1,r5
	mov	r2,r6
	mov	r3,r7

	STACK_OFF(0)
	POP_TMP_REGNUM2
	STACK_FREE(TMP_REGNUM2_SIZE+1)

	ret
#endif

#ifdef L_cmpqi2
/* compare r0 with r1, signed. r12, r13 are temporaries.
 * CF=1 iff r0 >= r1
 * ZF=1 iff r0 == r1
 */
ENTRY(__cmpqi2)
	mov	r13,r0
	sub	r13,r1   ;;  r13 = r0 - r1

	cmpi	r0,0x80
	rorc	r12,r0
	cmpi	r13,0x80
	rorc	r13,r1
	xor	r12,r13

	testi	r12,0x40
	bnz	1f
	movi	r12,0
1:

	xor	r12,r13  ;;  r12 = r12 ^ r13
	xori	r12,0x80 ;;  r12 = r12 ^ 0x80

	mov	r13,r0
	sub	r13,r1   ;;  r13 = r0 - r1

	rolc	r12,r12  ;;  set CF
	testi	r13,0xff ;;  set ZF
	ret
#endif

#ifdef L_cmphi2
/* compare { r1, r0 } with { r3, r2 }, signed.
 * r12, r13, are temporaries.
 * CF=1 iff { r1, r0 } >= { r3, r2 }
 * ZF=1 iff { r1, r0 } == { r3, r2 }
 */
ENTRY(__cmphi2)
	mov	r13,r0
	sub	r13,r2	;;  r13 = r0 - r2
	mov	r13,r1
	subc	r13,r3	;;  r13 = r1 - r3 - borrow

	cmpi	r1,0x80
	rorc	r12,r1
	cmpi	r13,0x80
	rorc	r13,r3
	xor	r12,r13

	testi	r12,0x40
	bnz	1f
	movi	r12,0
1:

	xor	r12,r13	;;  r12 = r12 ^ r13
	xori	r12,0x80 ;;  r12 = r12 ^ 0x80

	mov	r13,r0
	sub	r13,r2	;;  r13 = r0 - r2
	bnz	1f
	mov	r13,r1
	subc	r13,r3	;;  r13 = r1 - r3 - borrow
	bnz	1f

	rolc	r12,r12	;;  set CF
	setz		;;  set ZF
	ret

1:	rolc	r12,r12	;; set CF
	clrz		;; clear ZF
	ret
#endif

#ifdef L_cmpsi2
/* compare { r3, r2, r1, r0 } with { r7, r6, r5, r4 }, signed.
 * r12, r13, are temporaries.
 * CF=1 iff { r3, r2, r1, r0 } >= { r7, r6, r5, r4 }
 * ZF=1 iff { r3, r2, r1, r0 } == { r7, r6, r5, r4 }
 */
ENTRY(__cmpsi2)
	mov	r13,r0
	sub	r13,r4	;;  r13 = r0 - r4
	mov	r13,r1
	subc	r13,r5	;;  r13 = r1 - r5 - borrow
	mov	r13,r2
	subc	r13,r6	;;  r13 = r2 - r6 - borrow
	mov	r13,r3
	subc	r13,r7	;;  r13 = r3 - r7 - borrow

	cmpi	r3,0x80
	rorc	r12,r3
	cmpi	r13,0x80
	rorc	r13,r7
	xor	r12,r13

	testi	r12,0x40
	bnz	1f
	movi	r12,0
1:

	xor	r12,r13	;;  r12 = r12 ^ r13
	xori	r12,0x80 ;;  r12 = r12 ^ 0x80

	mov	r13,r0
	sub	r13,r4	;;  r13 = r0 - r4
	bnz	1f
	mov	r13,r1
	subc	r13,r5	;;  r13 = r1 - r5 - borrow
	bnz	1f
	mov	r13,r2
	subc	r13,r6	;;  r13 = r2 - r6 - borrow
	bnz	1f
	mov	r13,r3
	subc	r13,r7	;;  r13 = r3 - r7 - borrow
	bnz	1f

	rolc	r12,r12	;;  set CF
	setz		;;  set ZF
	ret

1:	rolc	r12,r12	;; set CF
	clrz		;; clear ZF
	ret
#endif

#ifdef L_ucmphi2
/* compare { r1, r0 } with { r3, r2 }, unsigned.
 * r12, r13 are temporaries.
 * CF=1 iff { r1, r0 } >= { r3, r2 }
 * ZF=1 iff { r1, r0 } == { r3, r2 }
 */
ENTRY(__ucmphi2)
	movi	r13,0

	mov	r12,r0
	sub	r12,r2	;;  r12 = r0 - r2
	bz	1f
	movi	r13,1
1:
	mov	r12,r1
	subc	r12,r3	;;  r12 = r1 - r3 - borrow
	bnz	1f

	testi	r13,0xff
1:	ret
#endif


#ifdef L_ucmpsi2
/* compare { r3, r2, r1, r0 } with { r7, r6, r5, r4 }, unsigned.
 * r12, r13 are temporaries.
 * CF=1 iff { r3, r2, r1, r0 } >= { r7, r6, r5, r4 }
 * ZF=1 iff { r3, r2, r1, r0 } == { r7, r6, r5, r4 }
 */
ENTRY(__ucmpsi2)
	movi	r13,0

	mov	r12,r0
	sub	r12,r4	;;  r12 = r0 - r4
	bz	1f
	movi	r13,1
1:
	mov	r12,r1
	subc	r12,r5	;; r12 = r1 - r5 - borrow
	bz	1f
	movi	r13,1
1:
	mov	r12,r2
	subc	r12,r6	;; r12 = r1 - r6 - borrow
	bz	1f
	movi	r13,1
1:
	mov	r12,r3
	subc	r12,r7	;; r12 = r1 - r7 - borrow
	bnz	1f

	;; High part is zero

	testi	r13,0xff
1:	ret
#endif

#ifdef L_irq_save_restore
ENTRY(__irq_save_restore)
ENTRY(__irq_save_restore2)
	ssp	r12,0
	ssp	r13,1
	ssp	r14,2
	ssp	r15,3

	LOAD_TEMP_SP

	# Only save the non-preserved registers (The rest are going to be
	# saved by the prologue of __IRQ and whatever it calls)
	sspi	r0,r12
	addi	r12,1
	sspi	r1,r12
	addi	r12,1
	sspi	r2,r12
	addi	r12,1
	sspi	r3,r12
	ADD_TEMP_SP(1)

	sspi	r4,r12
	addi	r12,1
	sspi	r5,r12
	addi	r12,1
	sspi	r6,r12
	addi	r12,1
	sspi	r7,r12
	ADD_TEMP_SP(1)

#ifndef __SIXTEEN_REGS__
#ifndef __CMODEL_LARGE__
	sspi	r28,r12
	addi	r12,1
	sspi	r29,r12
	addi	r12,1
	sspi	r30,r12
	addi	r12,1
	sspi	r31,r12
	ADD_TEMP_SP(1)
#endif
#endif

	PUSH_SP

#ifndef __CMODEL_MEDIUM__
#ifdef __CMODEL_LARGE__
	ADD_TEMP_SP(1)
#endif
	sspi	r10,r12
	ADD_TEMP_SP(1)
	sspi	r11,r12
#endif

	SET_SP

	call __IRQ

	LOAD_TEMP_SP

	lspi	r0,r12
	addi	r12,1
	lspi	r1,r12
	addi	r12,1
	lspi	r2,r12
	addi	r12,1
	lspi	r3,r12
	ADD_TEMP_SP(1)

	lspi	r4,r12
	addi	r12,1
	lspi	r5,r12
	addi	r12,1
	lspi	r6,r12
	addi	r12,1
	lspi	r7,r12
	ADD_TEMP_SP(1)

#ifndef __CMODEL_LARGE__
	lspi	r28,r12
	addi	r12,1
	lspi	r29,r12
	addi	r12,1
	lspi	r30,r12
	addi	r12,1
	lspi	r31,r12
	ADD_TEMP_SP(1)
#endif

	POP_SP

#ifndef __CMODEL_MEDIUM__
#ifdef __CMODEL_LARGE__
	ADD_TEMP_SP(1)
#endif
	lspi	r10,r12
	ADD_TEMP_SP(1)
	lspi	r11,r12
#endif

	lsp	r12,0
	lsp	r13,1
	lsp	r14,2
	lsp	r15,3
	iret

	.section .irq_stack,"aw",@nobits
	.align	2
	.space	IRQ_STACK_SIZE
__irq_stack:
	.previous
#endif

#ifdef L_prologue
#ifndef __SIXTEEN_REGS__
#ifdef __CMODEL_LARGE__
ENTRY(__prologue_save_r31)
	PUSH_REG(r31)
ENTRY(__prologue_save_r30)
	PUSH_REG(r30)
ENTRY(__prologue_save_r29)
	PUSH_REG(r29)
ENTRY(__prologue_save_r28)
	PUSH_REG(r28)
#endif

ENTRY(__prologue_save_r27)
	PUSH_REG(r27)
ENTRY(__prologue_save_r26)
	PUSH_REG(r26)
ENTRY(__prologue_save_r25)
	PUSH_REG(r25)
ENTRY(__prologue_save_r24)
	PUSH_REG(r24)
ENTRY(__prologue_save_r23)
	PUSH_REG(r23)
ENTRY(__prologue_save_r22)
	PUSH_REG(r22)
ENTRY(__prologue_save_r21)
	PUSH_REG(r21)
ENTRY(__prologue_save_r20)
	PUSH_REG(r20)
ENTRY(__prologue_save_r19)
	PUSH_REG(r19)
ENTRY(__prologue_save_r18)
	PUSH_REG(r18)
ENTRY(__prologue_save_r17)
	PUSH_REG(r17)
ENTRY(__prologue_save_r16)
	PUSH_REG(r16)
#endif

#ifdef __CMODEL_SMALL__
ENTRY(__prologue_save_r15)
	PUSH_REG(r15)
#endif
#ifdef __CMODEL_MEDIUM__
ENTRY(__prologue_save_r14)
	PUSH_REG(r14)
#endif
#ifdef __CMODEL_SMALL__
ENTRY(__prologue_save_r13)
	PUSH_REG(r13)
#endif

/* r12 is always temporary */

#ifdef __CMODEL_MEDIUM__
ENTRY(__prologue_save_r11)
	PUSH_REG(r11)
ENTRY(__prologue_save_r10)
	PUSH_REG(r10)
#endif

#ifndef __CMODEL_MEDIUM__
ENTRY(__prologue_save_r9)
	PUSH_REG(r9)
ENTRY(__prologue_save_r8)
	PUSH_REG(r8)
#endif

	ret
#endif

#ifdef L_epilogue
#ifndef __SIXTEEN_REGS__
#ifdef __CMODEL_LARGE__
ENTRY(__epilogue_restore_r31)
	POP_REG(r31)
ENTRY(__epilogue_restore_r30)
	POP_REG(r30)
ENTRY(__epilogue_restore_r29)
	POP_REG(r29)
ENTRY(__epilogue_restore_r28)
	POP_REG(r28)
#endif

ENTRY(__epilogue_restore_r27)
	POP_REG(r27)
ENTRY(__epilogue_restore_r26)
	POP_REG(r26)
ENTRY(__epilogue_restore_r25)
	POP_REG(r25)
ENTRY(__epilogue_restore_r24)
	POP_REG(r24)
ENTRY(__epilogue_restore_r23)
	POP_REG(r23)
ENTRY(__epilogue_restore_r22)
	POP_REG(r22)
ENTRY(__epilogue_restore_r21)
	POP_REG(r21)
ENTRY(__epilogue_restore_r20)
	POP_REG(r20)
ENTRY(__epilogue_restore_r19)
	POP_REG(r19)
ENTRY(__epilogue_restore_r18)
	POP_REG(r18)
ENTRY(__epilogue_restore_r17)
	POP_REG(r17)
ENTRY(__epilogue_restore_r16)
	POP_REG(r16)
#endif

#ifdef __CMODEL_SMALL__
ENTRY(__epilogue_restore_r15)
	POP_REG(r15)
#endif
#ifdef __CMODEL_MEDIUM__
ENTRY(__epilogue_restore_r14)
	POP_REG(r14)
#endif
#ifdef __CMODEL_SMALL__
ENTRY(__epilogue_restore_r13)
	POP_REG(r13)
#endif

/* r12 is always temporary */

#ifdef __CMODEL_MEDIUM__
ENTRY(__epilogue_restore_r11)
	POP_REG(r11)
ENTRY(__epilogue_restore_r10)
	POP_REG(r10)
#endif

#ifndef __CMODEL_MEDIUM__
ENTRY(__epilogue_restore_r9)
	POP_REG(r9)
ENTRY(__epilogue_restore_r8)
	POP_REG(r8)
#endif
	ret
#endif


