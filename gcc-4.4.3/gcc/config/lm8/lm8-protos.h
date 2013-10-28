/* Prototypes for exported functions defined in lm8.c
   
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

extern void lm8_override_options (void);
extern void lm8_conditional_register_usage (void);

extern void lm8_expand_prologue (void);
extern void lm8_expand_epilogue (int sibcall);

extern int lm8_can_eliminate(int, int);
extern int lm8_initial_elimination_offset (int, int);

#ifdef TREE_CODE
extern void lm8_asm_declare_function_name (FILE *, char *, tree);

#ifdef HAVE_MACHINE_MODES /* inside TREE_CODE */
extern rtx lm8_function_arg (CUMULATIVE_ARGS, enum machine_mode,
			     tree, int);
#endif /* HAVE_MACHINE_MODES inside TREE_CODE */
#endif /* TREE_CODE */

#ifdef RTX_CODE
extern void lm8_split_himode_move (rtx, rtx);
extern void lm8_split_himode_spill (rtx, rtx, rtx);
extern void lm8_split_psimode_move (rtx, rtx);
extern void lm8_split_psimode_spill (rtx, rtx, rtx);
extern void lm8_split_simode_move (rtx, rtx);
extern void lm8_split_simode_spill (rtx, rtx, rtx);
extern const char *lm8_output_arith (enum rtx_code, rtx *);
extern const char *lm8_output_logic (enum rtx_code, rtx *);
extern const char *lm8_output_imexport (bool, rtx *);

extern void lm8_notice_update_cc (rtx, rtx);
extern void lm8_print_operand (FILE *, rtx, int);
extern void lm8_print_operand_address (FILE *, rtx);

extern bool lm8_next_cc0_user_unsigned (rtx);
extern bool lm8_next_cc0_user_zf (rtx);

#ifdef HAVE_MACHINE_MODES /* inside RTX_CODE */
extern bool lm8_expand_move (enum machine_mode, rtx, rtx);
extern int lm8_legitimate_address_p (enum machine_mode, rtx, int);
extern const char *lm8_output_compare (rtx, enum machine_mode);
#endif /* HAVE_MACHINE_MODES inside RTX_CODE*/

#endif /* RTX_CODE */

#ifdef HAVE_MACHINE_MODES
extern int lm8_hard_regno_mode_ok (int, enum machine_mode);
#endif /* HAVE_MACHINE_MODES */
