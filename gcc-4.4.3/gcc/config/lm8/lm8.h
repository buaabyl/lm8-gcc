/* Definitions of target machine for GNU compiler, Lattice Mico8 architecture.

   Copyright (C) 2009, 2010 Free Software Foundation, Inc.

   Contributed by Beyond Semiconductor (www.beyondsemi.com)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/*-------------------------------*/
/* Run-time Target Specification */
/*-------------------------------*/

/* Print subsidiary information on the compiler version in use.  */
#ifndef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (LatticeMico8)")
#endif

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "mcmodel=medium" }
#endif

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__lm8__");		\
      builtin_assert ("cpu=lm8");		\
      builtin_assert ("machine=lm8");		\
						\
      if (TARGET_SIXTEEN_REGS)			\
	builtin_define ("__SIXTEEN_REGS__");	\
      switch (lm8_cmodel)			\
	{					\
	case CM_SMALL:				\
	  builtin_define ("__CMODEL_SMALL__");	\
	  break;				\
	case CM_MEDIUM:				\
	  builtin_define ("__CMODEL_MEDIUM__");	\
	  break;				\
	case CM_LARGE:				\
	  builtin_define ("__CMODEL_LARGE__");	\
	  break;				\
	default:				\
	  gcc_unreachable ();			\
	}					\
    }						\
  while (0)

#undef  LIB_SPEC
#define LIB_SPEC ""

#undef  LINK_SPEC
#define LINK_SPEC "%{mcmodel=small:-mlm8_elf_small} \
 %{mcmodel=large:-mlm8_elf_large}"

#define OVERRIDE_OPTIONS lm8_override_options()

#define CAN_DEBUG_WITHOUT_FP

/*---------------------------------*/
/* Target machine storage layout.  */
/*---------------------------------*/

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

#ifdef IN_LIBGCC2
/* This is to get correct SI and DI modes in libgcc2.c.  */
#define UNITS_PER_WORD 2
#else
/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 1
#endif

#define POINTER_SIZE GET_MODE_BITSIZE (Pmode)

/* Maximum sized of reasonable data type DImode or DFmode ...  */
#define MAX_FIXED_MODE_SIZE 32

#define PARM_BOUNDARY 8

#define STACK_BOUNDARY 8

#define FUNCTION_BOUNDARY 8

#define EMPTY_FIELD_BOUNDARY 8

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 8

#define MAX_OFILE_ALIGNMENT (32768 * 8)

#define TARGET_VTABLE_ENTRY_ALIGN 8

#define STRICT_ALIGNMENT 0

/*----------------------------------------*/
/* Layout of source language data types.  */
/*----------------------------------------*/

#define INT_TYPE_SIZE (TARGET_INT8 ? 8 : 16)
#define SHORT_TYPE_SIZE (INT_TYPE_SIZE == 8 ? INT_TYPE_SIZE : 16)
#define LONG_TYPE_SIZE (INT_TYPE_SIZE == 8 ? 16 : 32)
#define LONG_LONG_TYPE_SIZE 32

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 32
#define LONG_DOUBLE_TYPE_SIZE 32

#define DEFAULT_SIGNED_CHAR 0

#define SIZE_TYPE							\
  ((Pmode == QImode) ? "unsigned char"					\
   : (Pmode == HImode) ? (INT_TYPE_SIZE == 8				\
			  ? "long unsigned int"				\
			  : "unsigned int")				\
   : (Pmode == SImode) ? (INT_TYPE_SIZE == 8				\
			  ? "long long unsigned int"			\
			  : "long unsigned int")			\
   : "long long unsigned int")

#define PTRDIFF_TYPE							\
  ((Pmode == QImode) ? "char"						\
   : (Pmode == HImode) ? (INT_TYPE_SIZE == 8				\
			  ? "long int"					\
			  : "int")					\
   : (Pmode == SImode) ? (INT_TYPE_SIZE == 8				\
			  ? "long long int"				\
			  : "long int")					\
   : "long long int")

#define WCHAR_TYPE_SIZE 16

/*---------------------------*/
/* Standard register usage.  */
/*---------------------------*/

#define LAST_LM8_REGNUM	       (TARGET_SIXTEEN_REGS ? 15 : 31)
#define FIRST_PSEUDO_REGISTER  36

#define R13_REGNUM  13
#define R14_REGNUM  14
#define R15_REGNUM  15

#define RV_REGNUM   0

#define SP_REGNUM_SMALL		14
#define SP_REGNUM_MEDIUM	8
#define SP_REGNUM_LARGE		24

#define SP_REGNUM				\
  ((Pmode == QImode) ? SP_REGNUM_SMALL		\
   : (Pmode == HImode) ? SP_REGNUM_MEDIUM	\
   : SP_REGNUM_LARGE)

#define FP_REGNUM_SMALL		15
#define FP_REGNUM_MEDIUM	10
#define FP_REGNUM_LARGE		28

#define FP_REGNUM				\
  ((Pmode == QImode) ? FP_REGNUM_SMALL		\
   : (Pmode == HImode) ? FP_REGNUM_MEDIUM	\
   : FP_REGNUM_LARGE)

#define TMP_REGNUM		12

#define G_REG_P(X) ((X) < FIRST_PSEUDO_REGISTER)

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   Proper values are computed in the CONDITIONAL_REGISTER_USAGE.  */
#define FIXED_REGISTERS   \
{ 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  1, 1, 1, 1 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.

   Proper values are computed in the CONDITIONAL_REGISTER_USAGE.  */
#define CALL_USED_REGISTERS \
{ 1, 1, 1, 1, 1, 1, 1, 1,   \
  0, 0, 0, 0, 0, 0, 0, 0,   \
  0, 0, 0, 0, 0, 0, 0, 0,   \
  0, 0, 0, 0, 0, 0, 0, 0,   \
  1, 1, 1, 1 }

/* Macro to conditionally modify fixed_regs/call_used_regs.  */
#define CONDITIONAL_REGISTER_USAGE lm8_conditional_register_usage ()

#define HARD_REGNO_NREGS(REGNO, MODE)					\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define HARD_REGNO_MODE_OK(REGNO, MODE) lm8_hard_regno_mode_ok(REGNO, MODE)

#define MODES_TIEABLE_P(MODE1, MODE2) 1

#define AVOID_CCMODE_COPIES

/*----------------------------------*/
/* Register classes and constants.  */
/*----------------------------------*/

enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES { "NO_REGS", "GENERAL_REGS", "ALL_REGS" }

#define REG_CLASS_CONTENTS	\
{				\
  {0x00000000,0x0},		\
  {0xffffffff,0xf},		\
  {0xffffffff,0xf}		\
}

#define REGNO_REG_CLASS(REGNO) \
  (G_REG_P(REGNO) ? GENERAL_REGS : NO_REGS)

#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define INDEX_REG_CLASS NO_REGS

#define REGNO_OK_FOR_INDEX_P(REGNO) 0

#define BASE_REG_CLASS GENERAL_REGS

#define REGNO_OK_FOR_BASE_P(REGNO) \
  (G_REG_P (REGNO) || G_REG_P ((unsigned) reg_renumber[REGNO]))

#define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

/*----------------------------------------*/
/* Stack Layout and Calling Conventions.  */
/*----------------------------------------*/

#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 1

#define STACK_POINTER_OFFSET 0

#define STARTING_FRAME_OFFSET 0

#define FIRST_PARM_OFFSET(FNDECL) 0

#define STACK_POINTER_REGNUM SP_REGNUM

#define FRAME_POINTER_REGNUM FP_REGNUM

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 32

/* FIXME - This is not yet supported.  */
#define STATIC_CHAIN_REGNUM 9

#define FRAME_POINTER_REQUIRED 0

#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM, SP_REGNUM_SMALL },		\
 { ARG_POINTER_REGNUM, SP_REGNUM_MEDIUM },		\
 { ARG_POINTER_REGNUM, SP_REGNUM_LARGE },		\
 { ARG_POINTER_REGNUM, FP_REGNUM_SMALL },		\
 { ARG_POINTER_REGNUM, FP_REGNUM_MEDIUM },		\
 { ARG_POINTER_REGNUM, FP_REGNUM_LARGE },		\
 { FP_REGNUM_SMALL, SP_REGNUM_SMALL },			\
 { FP_REGNUM_MEDIUM, SP_REGNUM_MEDIUM },		\
 { FP_REGNUM_MEDIUM+1, SP_REGNUM_MEDIUM+1 },		\
 { FP_REGNUM_LARGE, SP_REGNUM_LARGE },			\
 { FP_REGNUM_LARGE+1, SP_REGNUM_LARGE+1 },		\
 { FP_REGNUM_LARGE+2, SP_REGNUM_LARGE+2 },		\
 { FP_REGNUM_LARGE+3, SP_REGNUM_LARGE+3 }}

#define CAN_ELIMINATE(FROM, TO) lm8_can_eliminate (FROM, TO)

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)	\
  (OFFSET) = lm8_initial_elimination_offset (FROM, TO)

/*-----------------------------*/
/* Function argument passing.  */
/*-----------------------------*/

#define ACCUMULATE_OUTGOING_ARGS 1

#define RETURN_POPS_ARGS(DECL, FUNTYPE, SIZE) 0

/*--------------------------------*/
/* Passing Arguments in Registers */
/*--------------------------------*/

/* The first argument register.  */
#define LM8_FIRST_ARG_REG 0

/* The number of (integer) argument register available.  */
#define LM8_NUM_ARG_REGS 8

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)		\
  lm8_function_arg (CUM, MODE, TYPE, NAMED)

#define CUMULATIVE_ARGS int

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  (CUM) = 0

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  (CUM) += LM8_NUM_REGS2 (MODE, TYPE)

#define FUNCTION_ARG_REGNO_P(N)				\
  (((N) >= LM8_FIRST_ARG_REG)				\
   && ((N) < (LM8_FIRST_ARG_REG + LM8_NUM_ARG_REGS)))

/*--------------------*/
/* Function results.  */
/*--------------------*/

#define FUNCTION_VALUE(VALTYPE, FUNC)		\
  gen_rtx_REG (TYPE_MODE (VALTYPE), RV_REGNUM)

#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, RV_REGNUM)

#define FUNCTION_VALUE_REGNO_P(N) ((N) == RV_REGNUM)

#define RETURN_IN_MEMORY(TYPE) lm8_return_in_memory (TYPE)

#define DEFAULT_PCC_STRUCT_RETURN 0

/* Convert from bytes to ints.  */
#define LM8_NUM_INTS(X) (((X) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The number of (integer) registers required to hold a quantity of
   type MODE.  */
#define LM8_NUM_REGS(MODE) LM8_NUM_INTS (GET_MODE_SIZE (MODE))

/* The number of (integer) registers required to hold a quantity of
   TYPE MODE.  */
#define LM8_NUM_REGS2(MODE, TYPE)			\
  LM8_NUM_INTS ((MODE) == BLKmode ?			\
  int_size_in_bytes (TYPE) : GET_MODE_SIZE (MODE))

#define STRUCT_VALUE 0

/*---------------------------*/
/* Function entry and exit.  */
/*---------------------------*/

/*-------------*/
/* Profiling.  */
/*-------------*/

#define FUNCTION_PROFILER(FILE, LABELNO)

/*---------------*/
/* Trampolines.  */
/*---------------*/

/* No trampolines.  */
#define TRAMPOLINE_SIZE 0
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, CHAIN)

/*---------------------*/
/*  Addressing Modes.  */
/*---------------------*/

#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

#define MAX_REGS_PER_ADDRESS 1

#ifdef REG_OK_STRICT
#  define GO_IF_LEGITIMATE_ADDRESS(MODE, OPERAND, ADDR)	\
{							\
  if (lm8_legitimate_address_p (MODE, OPERAND, 1))	\
    goto ADDR;						\
}
#else
#  define GO_IF_LEGITIMATE_ADDRESS(MODE, OPERAND, ADDR)	\
  {							\
  if (lm8_legitimate_address_p (MODE, OPERAND, 0))	\
    goto ADDR;						\
}
#endif

#define STRICT_REG_OK_FOR_BASE_P(X)		\
  (REGNO_OK_FOR_BASE_P (REGNO (X)))
#define NONSTRICT_REG_OK_FOR_BASE_P(X)				\
  (G_REG_P (REGNO (X)) || !HARD_REGISTER_NUM_P (REGNO (X)))

#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) STRICT_REG_OK_FOR_BASE_P(X)
#else
#define REG_OK_FOR_BASE_P(X) NONSTRICT_REG_OK_FOR_BASE_P(X)
#endif

#define REG_OK_FOR_INDEX_P(X) 0

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

#define LEGITIMATE_CONSTANT_P(X) 1

/*-------------------------*/
/* Condition Code Status.  */
/*-------------------------*/

#define NOTICE_UPDATE_CC(EXP, INSN) lm8_notice_update_cc(EXP, INSN)

/*---------*/
/* Costs.  */
/*---------*/

#define SLOW_BYTE_ACCESS 1

#define NO_FUNCTION_CSE

#define MEMORY_MOVE_COST(MODE, CLASS, IN)		\
  ((MODE) == QImode ? 2					\
   : (MODE) == HImode ? 4				\
   : (MODE) == SImode ? 8				\
   : (MODE) == SFmode ? 8				\
   : 16)

#define BRANCH_COST(speed_p, predictable_p) 0

/*------------*/
/* Sections.  */
/*------------*/

#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

#define BSS_SECTION_ASM_OP "\t.section .bss"

/*-------------*/
/* Assembler.  */
/*-------------*/

#define ASM_COMMENT_START "#"

#define ASM_APP_ON "#APP\n"

#define ASM_APP_OFF "#NO_APP\n"

#define ASM_OUTPUT_DEF(FILE, LABEL1, LABEL2)	\
  do {						\
    fputc ( '\t', FILE);			\
    assemble_name (FILE, LABEL1);		\
    fputs ( " = ", FILE);			\
    assemble_name (FILE, LABEL2);		\
    fputc ( '\n', FILE);			\
  } while (0)

#define ASM_OUTPUT_LABEL(FILE, NAME)	\
  do {					\
    assemble_name (FILE, NAME);		\
    fputs (":\n", FILE);		\
  } while (0)

#define ASM_OUTPUT_LABELREF(FILE, NAME)	\
  do {					\
    const char *xname = (NAME);		\
    if (xname[0] == '@')		\
      xname += 1;			\
    if (xname[0] == '*')		\
      xname += 1;			\
    fputs (xname, FILE);		\
  } while (0)

#define ASM_OUTPUT_SYMBOL_REF(STREAM, SYMBOL)	\
  assemble_name (STREAM, XSTR (SYMBOL, 0));

#define GLOBAL_ASM_OP "\t.global\t"

#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)	\
  sprintf (STRING, "*.%s%lu", PREFIX, (unsigned long)(NUM))

#define HAS_INIT_SECTION 1

#undef DO_GLOBAL_CTORS_BODY
#define DO_GLOBAL_CTORS_BODY			\
{						\
  /* FIXME */					\
}

#undef DO_GLOBAL_DTORS_BODY
#define DO_GLOBAL_DTORS_BODY			\
{						\
  /* FIXME */					\
}

#define REGISTER_NAMES						\
{								\
 "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",		\
 "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",	\
 "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",	\
 "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",	\
 "arg0", "arg1", "arg2", "arg3"					\
}

#define PRINT_OPERAND_PUNCT_VALID_P(CHAR)			\
  (((CHAR) == '&') || ((CHAR) == '@') || ((CHAR) == '*'))

#define PRINT_OPERAND(FILE, X, CODE)		\
  lm8_print_operand (FILE, X, CODE)

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)	\
  lm8_print_operand_address (FILE, ADDR)

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."
#endif

/* Invoked just before function output. */
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)		\
  lm8_asm_declare_function_name (FILE, NAME, DECL)

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)		\
  do {								\
    fputs ("\t.comm ", STREAM);					\
    assemble_name (STREAM, NAME);				\
    fprintf (STREAM, ",%lu,1\n", (unsigned long)(SIZE));	\
  } while (0)

#define ASM_OUTPUT_BSS(FILE, DECL, NAME, SIZE, ROUNDED)		\
  asm_output_bss (FILE, DECL, NAME, SIZE, ROUNDED)	\

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)	\
  do {							\
    fputs ("\t.lcomm ", STREAM);			\
    assemble_name (STREAM, NAME);			\
    fprintf (STREAM, ",%d\n", (int)(SIZE));		\
  } while (0)

#define ASM_OUTPUT_SKIP(STREAM, N)				\
  fprintf (STREAM, "\t.skip %lu,0\n", (unsigned long)(N))

#define ASM_OUTPUT_ALIGN(STREAM, POWER)			\
  do {							\
      if ((POWER) > 1)					\
	fprintf (STREAM, "\t.p2align\t%d\n", POWER);	\
  } while (0)

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)		\
  do {							\
    char label[64];					\
    ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
    fprintf (FILE, "\n\t.word\t");			\
    assemble_name (FILE, label);			\
    fprintf (FILE, "\n");				\
  } while (0)

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)	\
  do {								\
    char label[64];						\
    fprintf (FILE, "\t.word\t(");				\
    ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);		\
    assemble_name (FILE, label);				\
    fprintf (FILE, "-");					\
    ASM_GENERATE_INTERNAL_LABEL (label, "L", REL);		\
    assemble_name (FILE, label);				\
    fprintf (FILE, ")\n");					\
  } while (0)

/*-------------*/
/* Debugging.  */
/*-------------*/

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

#define CAN_DEBUG_WITHOUT_FP

#define DEFAULT_GDB_EXTENSIONS 1

/*--------*/
/* Misc.  */
/*--------*/

#define CASE_VECTOR_MODE Pmode

#undef WORD_REGISTER_OPERATIONS

#define MOVE_MAX 4

#define SHIFT_COUNT_TRUNCATED 1

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define Pmode					\
  ((lm8_cmodel == CM_SMALL) ? QImode		\
   : (lm8_cmodel == CM_MEDIUM) ? HImode		\
   : SImode)

#define FUNCTION_MODE HImode

enum cmodel {
  CM_SMALL,
  CM_MEDIUM,
  CM_LARGE
};

extern enum cmodel lm8_cmodel;
