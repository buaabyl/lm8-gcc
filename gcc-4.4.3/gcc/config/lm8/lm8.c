/* Subroutines used for code generation on the Lattice Mico8 architecture.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-codes.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "tree.h"
#include "output.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "obstack.h"
#include "function.h"
#include "recog.h"
#include "ggc.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"
#include "params.h"
#include "df.h"

/* Stack frame layout we use:

	+----------------------+	|
	| Saved registers      |	| Previous stack frame
	| Local variables      |	|
	| Called function args |	/
 r8'-->	|......................+
	| Pretended func. args | __AP   \ <- slots filled by current function
	+----------------------+	|
    __	| Saved registers      | __     | Current stack frame
 r10	| Local variables      |   FP   |
	| Called function args |	/
  r8--> +......................+

*/

struct lm8_frame_info
{
  HOST_WIDE_INT total_size;	/* number of bytes of entire frame.  */
  HOST_WIDE_INT pretend_size;	/* number of bytes we pretend caller did.  */
  HOST_WIDE_INT callee_size;	/* number of bytes to save callee saves.  */
  HOST_WIDE_INT locals_size;	/* number of bytes for local variables.  */
  HOST_WIDE_INT args_size;	/* number of bytes for outgoing arguments.  */
  unsigned int reg_save_mask;	/* mask of saved registers.  */
  int last_saved;	/* last saved register.  */
};

/* Current frame information calculated by lm8_compute_frame_size.  */
static struct lm8_frame_info current_frame_info;

/* Code model option.  */
enum cmodel lm8_cmodel;

static rtx (*lm8_gen_add3) (rtx, rtx, rtx);
static rtx (*lm8_gen_sub3) (rtx, rtx, rtx);
static rtx (*lm8_gen_call_prologue_saves) (rtx);
static rtx (*lm8_gen_call_epilogue_restores) (rtx);

enum insn_code lm8_reload_inhi, lm8_reload_insi,
	       lm8_reload_outhi, lm8_reload_outsi;

/* Prototypes for static functions.  */
static void expand_save_restore (struct lm8_frame_info *, int);
static void stack_adjust (HOST_WIDE_INT);
static void lm8_setup_incoming_varargs (CUMULATIVE_ARGS *, enum machine_mode,
					tree, int *, int);
static int lm8_arg_partial_bytes (CUMULATIVE_ARGS *, enum machine_mode,
				  tree, bool);
static bool lm8_return_in_memory (const_tree, const_tree);
static bool lm8_pass_by_reference (CUMULATIVE_ARGS *, enum machine_mode,
				   const_tree, bool);
static HOST_WIDE_INT lm8_compute_frame_size (int);
static rtx lm8_subword (enum machine_mode, rtx, int);
static enum reg_class lm8_secondary_reload (bool, rtx, enum reg_class,
					    enum machine_mode,
					    secondary_reload_info *);
static void lm8_output_function_prologue (FILE *, HOST_WIDE_INT);
static bool lm8_function_ok_for_sibcall (tree, tree);
static bool lm8_rtx_costs (rtx, int, int, int *, bool);

static void lm8_init_builtins (void);
static rtx lm8_expand_builtin (tree, rtx, rtx, enum machine_mode, int);

static tree lm8_handle_fndecl_attribute (tree *, tree, tree, int, bool *);

/* Table of valid machine attributes.  */
static const struct attribute_spec lm8_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "interrupt", 0, 0, true, false, false, lm8_handle_fndecl_attribute },
  /* End element.  */
  { NULL, 0, 0, false, false, false, NULL }
};

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE lm8_output_function_prologue
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE lm8_attribute_table
#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL lm8_function_ok_for_sibcall
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY lm8_return_in_memory
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS lm8_setup_incoming_varargs
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE lm8_pass_by_reference
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES lm8_arg_partial_bytes
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS lm8_rtx_costs

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD lm8_secondary_reload

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS lm8_init_builtins
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN lm8_expand_builtin

struct gcc_target targetm = TARGET_INITIALIZER;
  
/* Generate and emit RTL to save or restore callee save registers.  */
static void
expand_save_restore (struct lm8_frame_info *info, int prologue)
{
  unsigned int reg_save_mask = info->reg_save_mask;
  int regno;
  HOST_WIDE_INT offset_dif;
  rtx offset_dif_rtx, mem;
  rtx rX;
  rtx insn;
  bool setup_emitted = false;

  /* Exit early if there are no register marked to save/restore.  */
  if (!reg_save_mask)
    return;

  /* Callee saves are above outgoing arguments and locals.  */
  offset_dif = (current_frame_info.args_size
		+ current_frame_info.locals_size);

  /* rX must be caller saved to be used as a temp reg.  */
  rX = gen_rtx_REG (Pmode, TMP_REGNUM);

  if (TARGET_CALL_PROLOGUES)
    {
      if (offset_dif)
	{
	  insn = emit_move_insn (rX, GEN_INT (offset_dif));
	  if (prologue)
	    RTX_FRAME_RELATED_P (insn) = 1;
	  insn = (*lm8_gen_add3) (rX, rX, stack_pointer_rtx);
	  emit_insn (insn);
	}
      else
	insn = emit_move_insn (rX, stack_pointer_rtx);

      if (prologue)
	RTX_FRAME_RELATED_P (insn) = 1;

      gcc_assert (current_frame_info.last_saved >= 0);

      regno = current_frame_info.last_saved;
      if (prologue)
	insn = lm8_gen_call_prologue_saves (GEN_INT (regno));
      else
	insn = lm8_gen_call_epilogue_restores (GEN_INT (regno));

      emit_insn (insn);
      return;
    }
      
  for (regno = 0; regno <= LAST_LM8_REGNUM; regno++)
    {
      if (reg_save_mask & (1 << regno))
	{
	  /* Add offset difference to the temporary.  */
	  if (offset_dif)
	    {
	      offset_dif_rtx = GEN_INT (offset_dif);

	      if (setup_emitted)
		insn = (*lm8_gen_add3) (rX, rX, offset_dif_rtx);
	      else
		{
		  setup_emitted = true;

		  insn = emit_move_insn (rX, offset_dif_rtx);
		  if (prologue)
		    RTX_FRAME_RELATED_P (insn) = 1;
		  insn = (*lm8_gen_add3) (rX, rX, stack_pointer_rtx);
		}
	      emit_insn (insn);
	      if (prologue)
		RTX_FRAME_RELATED_P (insn) = 1;
	      mem = gen_rtx_MEM (word_mode, rX);
	    }
	  /* In case there is more than one register to save/restore,
	     initialize temporary with current SP value.  We will just
	     add offset difference to save/restore following registers.  */
	  else if (current_frame_info.callee_size / UNITS_PER_WORD > 1)
	    {
	      setup_emitted = true;

	      insn = emit_move_insn (rX, stack_pointer_rtx);
	      if (prologue)
		RTX_FRAME_RELATED_P (insn) = 1;
	      mem = gen_rtx_MEM (word_mode, rX);
	    }
	  /* One register and no offset - save/restore through SP.  */
	  else
	    mem = gen_rtx_MEM (word_mode, stack_pointer_rtx);
	    	    
	  if (prologue)
	    insn = emit_move_insn (mem, gen_rtx_REG (word_mode, regno));
	  else
	    insn = emit_move_insn (gen_rtx_REG (word_mode, regno), mem);

	  /* only prologue instructions which set the sp fp or save a
	     register should be marked as frame related.  */
	  if (prologue)
	    RTX_FRAME_RELATED_P (insn) = 1;

	  offset_dif = UNITS_PER_WORD;
	}
    }
}

static void
stack_adjust (HOST_WIDE_INT amount)
{
  rtx insn;

  insn = (*lm8_gen_add3) (stack_pointer_rtx,
			  stack_pointer_rtx, GEN_INT (amount));
  emit_insn (insn);
  if (amount < 0)
    RTX_FRAME_RELATED_P (insn) = 1;
}

/* Create and emit instructions for a functions prologue.  */
void
lm8_expand_prologue (void)
{
  rtx insn;
  int frame_size = get_frame_size ();

  lm8_compute_frame_size (frame_size);

  if (current_frame_info.total_size > 0)
    {
      /* Add space on stack new frame.  */
      stack_adjust (-current_frame_info.total_size);

      /* Save callee save registers.  */
      if (current_frame_info.reg_save_mask != 0)
	expand_save_restore (&current_frame_info, 1);

      /* Setup frame pointer if it's needed.  */
      if (frame_pointer_needed)
	{
	  /* Frame pointer offset.  */
	  HOST_WIDE_INT offset = (current_frame_info.locals_size
				  + current_frame_info.args_size);
	  if (offset)
	    {
	      insn = emit_move_insn (frame_pointer_rtx, GEN_INT (offset));
	      RTX_FRAME_RELATED_P (insn) = 1;

	      /* Add in sp.  */
	      insn = (*lm8_gen_add3) (frame_pointer_rtx,
				      frame_pointer_rtx, stack_pointer_rtx);
	      emit_insn (insn);
	    }
	  else
	    insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);

	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* Prevent prologue from being scheduled into function body.  */
      emit_insn (gen_blockage ());
    }
}

/* Create and emit instructions for a functions epilogue.  */
void
lm8_expand_epilogue (int sibcall)
{
  int frame_size = get_frame_size ();

  lm8_compute_frame_size (frame_size);

  if (current_frame_info.total_size > 0)
    {
      /* Prevent stack code from being reordered.  */
      emit_insn (gen_blockage ());

      /* Restore callee save registers.  */
      if (current_frame_info.reg_save_mask != 0)
	expand_save_restore (&current_frame_info, 0);

      /* Deallocate stack.  */
      stack_adjust (current_frame_info.total_size);
    }

  /* Return to calling function.  */
  if (!sibcall)
    emit_jump_insn (gen_return_internal ());
}

/* Return the bytes needed to compute the frame pointer
   from the current stack pointer.  */
static HOST_WIDE_INT
lm8_compute_frame_size (int size)
{
  int regno;
  HOST_WIDE_INT total_size, locals_size, args_size, pretend_size, callee_size;
  unsigned int reg_save_mask;
  unsigned int last_saved;

  locals_size = size;
  args_size = crtl->outgoing_args_size;
  pretend_size = crtl->args.pretend_args_size;
  callee_size = 0;
  reg_save_mask = 0;
  last_saved = -1;

  /* Build mask that actually determines which registers we save
     and calculate size required to store them in the stack.  */
  for (regno = 0; regno <= LAST_LM8_REGNUM; regno++)
    {
      if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
	{
	  reg_save_mask |= 1 << regno;
	  last_saved = regno;
	  callee_size += UNITS_PER_WORD;
	}
    }

  if (TARGET_CALL_PROLOGUES)
    callee_size = (last_saved - 7) * UNITS_PER_WORD;

  if ((int)callee_size < 0)
    callee_size = 0;

  /* Compute total frame size.  */
  total_size = pretend_size + args_size + locals_size + callee_size;

  /* Save computed information.  */
  current_frame_info.total_size = total_size;
  current_frame_info.callee_size = callee_size;
  current_frame_info.pretend_size = pretend_size;
  current_frame_info.locals_size = locals_size;
  current_frame_info.args_size = args_size;
  current_frame_info.reg_save_mask = reg_save_mask;
  current_frame_info.last_saved = last_saved;

  return total_size;
}

/* All direct functions are ok for sibcall optimization.  */
static bool
lm8_function_ok_for_sibcall (tree decl,
			    tree exp ATTRIBUTE_UNUSED)
{
  /* Never tailcall indirect functions.  */
  if (decl == NULL)
    return false;

  return true;
}


/* Copy VALUE to a register and return that register.  If new psuedos
   are allowed, copy it into a new register, otherwise use DEST.  */
static rtx
lm8_force_temporary (rtx dest, rtx value)
{
  if (can_create_pseudo_p ())
    return force_reg (HImode, value);
  else
    {
      emit_move_insn (copy_rtx (dest), value);
      return dest;
    }
}

/* Return a LO_SUM expression for ADDR.
   TEMP is used to load the high part into a register.  */
static rtx
lm8_split_symbol (rtx temp, rtx addr)
{
  rtx high;

  high = lm8_force_temporary (temp, gen_rtx_HIGH (HImode, copy_rtx (addr)));

  return gen_rtx_LO_SUM (HImode, high, addr);
}

/* If (set DEST SRC) is not a valid instruction, emit an equivalent
   sequence that is valid.  */
bool
lm8_expand_move (enum machine_mode mode, rtx dest, rtx src)
{
  if (!(register_operand (dest, mode)
	|| register_operand (src, mode)))
    {  
      emit_move_insn (dest, force_reg (mode, src));
      return true;
    }

  /* We need to deal with constants that would be legitimate
     immediate_operands but not legitimate move_operands.  */
  if (Pmode == HImode && CONSTANT_P (src) && !move_operand (src, mode))
    {
      emit_move_insn (dest, lm8_split_symbol (dest, src));
      return true;
    }

  return false;
}

/* Return one word of double-word value OP, taking into account the fixed
   endianness of certain registers.  HIGH_P is true to select the high part,
   false to select the low part.  */
static rtx
lm8_subword (enum machine_mode outermode, rtx op, int word)
{
  unsigned int byte;
  enum machine_mode mode;

  mode = GET_MODE (op);
  if (mode == VOIDmode)
    mode = GET_MODE_WIDER_MODE (word_mode);

  byte = word * UNITS_PER_WORD;

  if (MEM_P (op)
      && ! mode_dependent_address_p (XEXP (op, 0))
      && GET_MODE_SIZE (outermode) <= GET_MODE_SIZE (GET_MODE (op)))
    return adjust_address_nv (op, outermode, byte);

  return simplify_gen_subreg (outermode, op, mode, byte);
}

/* Split a HImode multiword move from SRC to DEST.  This move can be
   implemented using HIGH/LO_SUM pairs.  */
void
lm8_split_himode_move (rtx dest, rtx src)
{
  rtx low_src, low_dest;
  rtx high_src, high_dest;

  if (can_create_pseudo_p ()
      || !(MEM_P (src) || MEM_P (dest)))
    {
      low_src = lm8_subword (word_mode, src, 0);
      low_dest = lm8_subword (word_mode, dest, 0);

      high_src = lm8_subword (word_mode, src, 1);
      high_dest = lm8_subword (word_mode, dest, 1);

      /* Decide in which order to emit move insns.  */
      if (REG_P (low_dest)
	  && reg_overlap_mentioned_p (low_dest, src))
	{
	  emit_move_insn (high_dest, high_src);
	  emit_move_insn (low_dest, low_src);
	}
      else
	{
	  emit_move_insn (low_dest, low_src);
	  emit_move_insn (high_dest, high_src);
	}
    }
  else
    {
      rtx scratch = NULL_RTX;
      int offset = 0;

      if (MEM_P (src))
	{
	  scratch = XEXP (src, 0);

	  if (GET_CODE (scratch) == PLUS)
	    {
	      offset = INTVAL (XEXP (scratch, 1));
	      scratch = XEXP (scratch, 0);
	    }

	  gcc_assert (!reg_overlap_mentioned_p (scratch, dest));

	  low_src = change_address (src, word_mode, scratch);
	  high_src = change_address (src, word_mode, scratch);
	}
      else
	{
	  low_src = lm8_subword (word_mode, src, 0);
	  high_src = lm8_subword (word_mode, src, 1);
	}

      if (MEM_P (dest))
	{
	  scratch = XEXP (dest, 0);

	  if (GET_CODE (scratch) == PLUS)
	    {
	      offset = INTVAL (XEXP (scratch, 1));
	      scratch = XEXP (scratch, 0);
	    }

	  gcc_assert (!reg_overlap_mentioned_p (scratch, src));

	  low_dest = change_address (dest, word_mode, scratch);
	  high_dest = change_address (dest, word_mode, scratch);
	}
      else
	{
	  low_dest = lm8_subword (word_mode, dest, 0);
	  high_dest = lm8_subword (word_mode, dest, 1);
	}

      if (offset)
	emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (offset)));

      emit_move_insn (low_dest, low_src);
      emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (1)));
      emit_move_insn (high_dest, high_src);

      emit_insn ((*lm8_gen_sub3) (scratch, scratch, GEN_INT (offset + 1)));
    }
}

/* Split a SImode multiword move from SRC to DEST.  */
void
lm8_split_simode_move (rtx dest, rtx src)
{
  rtx ll_src, ll_dest;
  rtx lh_src, lh_dest;
  rtx hl_src, hl_dest;
  rtx hh_src, hh_dest;

  if (can_create_pseudo_p ()
      || !(MEM_P (src) || MEM_P (dest)))
    {
      ll_src = lm8_subword (word_mode, src, 0);
      ll_dest = lm8_subword (word_mode, dest, 0);

      lh_src = lm8_subword (word_mode, src, 1);
      lh_dest = lm8_subword (word_mode, dest, 1);

      hl_src = lm8_subword (word_mode, src, 2);
      hl_dest = lm8_subword (word_mode, dest, 2);

      hh_src = lm8_subword (word_mode, src, 3);
      hh_dest = lm8_subword (word_mode, dest, 3);

      /* Decide in which order to emit move insns.  */
      if (REG_P (ll_dest)
	  && reg_overlap_mentioned_p (ll_dest, src))
	{
	  emit_move_insn (hh_dest, hh_src);
	  emit_move_insn (hl_dest, hl_src);
	  emit_move_insn (lh_dest, lh_src);
	  emit_move_insn (ll_dest, ll_src);
	}
      else
	{
	  emit_move_insn (ll_dest, ll_src);
	  emit_move_insn (lh_dest, lh_src);
	  emit_move_insn (hl_dest, hl_src);
	  emit_move_insn (hh_dest, hh_src);
	}
    }
  else
    {
      rtx scratch = NULL_RTX;
      int offset = 0;

      if (MEM_P (src))
	{
	  scratch = XEXP (src, 0);

	  if (GET_CODE (scratch) == PLUS)
	    {
	      offset = INTVAL (XEXP (scratch, 1));
	      scratch = XEXP (scratch, 0);
	    }

	  gcc_assert (!reg_overlap_mentioned_p (scratch, dest));

	  ll_src = change_address (src, word_mode, scratch);
	  lh_src = change_address (src, word_mode, scratch);
	  hl_src = change_address (src, word_mode, scratch);
	  hh_src = change_address (src, word_mode, scratch);
	}
      else
	{
	  ll_src = lm8_subword (word_mode, src, 0);
	  lh_src = lm8_subword (word_mode, src, 1);
	  hl_src = lm8_subword (word_mode, src, 2);
	  hh_src = lm8_subword (word_mode, src, 3);
	}

      if (MEM_P (dest))
	{
	  scratch = XEXP (dest, 0);

	  if (GET_CODE (scratch) == PLUS)
	    {
	      offset = INTVAL (XEXP (scratch, 1));
	      scratch = XEXP (scratch, 0);
	    }

	  gcc_assert (!reg_overlap_mentioned_p (scratch, src));

	  ll_dest = change_address (dest, word_mode, scratch);
	  lh_dest = change_address (dest, word_mode, scratch);
	  hl_dest = change_address (dest, word_mode, scratch);
	  hh_dest = change_address (dest, word_mode, scratch);
	}
      else
	{
	  ll_dest = lm8_subword (word_mode, dest, 0);
	  lh_dest = lm8_subword (word_mode, dest, 1);
	  hl_dest = lm8_subword (word_mode, dest, 2);
	  hh_dest = lm8_subword (word_mode, dest, 3);
	}

      if (offset)
	emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (offset)));

      emit_move_insn (ll_dest, ll_src);
      emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (1)));
      emit_move_insn (lh_dest, lh_src);
      emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (1)));
      emit_move_insn (hl_dest, hl_src);
      emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (1)));
      emit_move_insn (hh_dest, hh_src);

      emit_insn ((*lm8_gen_sub3) (scratch, scratch, GEN_INT (offset + 3)));
    }
}

/* Split a HImode multiword spill from SRC to DEST.  */
void
lm8_split_himode_spill (rtx dest, rtx src, rtx scratch)
{
  rtx low_src, low_dest;
  rtx high_src, high_dest;

  rtx addr;

  if (MEM_P (src))
    {
      /* Unfortunatelly, gcc does not check if the destination reg
	 of an input reload overlaps with scratch register.  Re-use
	 address register in this case to split reload.  */
      if (reg_overlap_mentioned_p (scratch, dest))
	{
	  lm8_split_himode_move (dest, src);
	  return;
	}

      addr = XEXP (src, 0);

      high_src = change_address (src, word_mode, scratch);
      low_src = change_address (src, word_mode, scratch);

      high_dest = lm8_subword (word_mode, dest, 1);
      low_dest = lm8_subword (word_mode, dest, 0);
    }
  else if (MEM_P (dest))
    {
      addr = XEXP (dest, 0);

      high_dest = change_address (dest, word_mode, scratch);
      low_dest = change_address (dest, word_mode, scratch);

      high_src = lm8_subword (word_mode, src, 1);
      low_src = lm8_subword (word_mode, src, 0);
    }
  else
    gcc_unreachable ();

  if (GET_CODE (addr) == PLUS)
    {
      emit_move_insn (scratch, XEXP (addr, 0));
      emit_insn ((*lm8_gen_add3) (scratch, scratch, XEXP (addr, 1)));
    }
  else
    emit_move_insn (scratch, addr);

  emit_move_insn (low_dest, low_src);
  emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (1)));
  emit_move_insn (high_dest, high_src);
}

/* Split a SImode multiword spill from SRC to DEST.  */
void
lm8_split_simode_spill (rtx dest, rtx src, rtx scratch)
{
  rtx ll_src, ll_dest;
  rtx lh_src, lh_dest;
  rtx hl_src, hl_dest;
  rtx hh_src, hh_dest;

  rtx addr;

  if (MEM_P (src))
    {
      /* Unfortunatelly, gcc does not check if the destination reg
	 of an input reload overlaps with scratch register.  Re-use
	 address register in this case to split reload.  */
      if (reg_overlap_mentioned_p (scratch, dest))
	{
	  lm8_split_simode_move (dest, src);
	  return;
	}

      addr = XEXP (src, 0);

      ll_src = change_address (src, word_mode, scratch);
      lh_src = change_address (src, word_mode, scratch);
      hl_src = change_address (src, word_mode, scratch);
      hh_src = change_address (src, word_mode, scratch);

      ll_dest = lm8_subword (word_mode, dest, 0);
      lh_dest = lm8_subword (word_mode, dest, 1);
      hl_dest = lm8_subword (word_mode, dest, 2);
      hh_dest = lm8_subword (word_mode, dest, 3);
    }
  else if (MEM_P (dest))
    {
      addr = XEXP (dest, 0);

      ll_dest = change_address (dest, word_mode, scratch);
      lh_dest = change_address (dest, word_mode, scratch);
      hl_dest = change_address (dest, word_mode, scratch);
      hh_dest = change_address (dest, word_mode, scratch);

      ll_src = lm8_subword (word_mode, src, 0);
      lh_src = lm8_subword (word_mode, src, 1);
      hl_src = lm8_subword (word_mode, src, 2);
      hh_src = lm8_subword (word_mode, src, 3);
    }
  else
    gcc_unreachable ();

  if (GET_CODE (addr) == PLUS)
    {
      emit_move_insn (scratch, XEXP (addr, 0));
      emit_insn ((*lm8_gen_add3) (scratch, scratch, XEXP (addr, 1)));
    }
  else
    emit_move_insn (scratch, addr);

  emit_move_insn (ll_dest, ll_src);
  emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (1)));
  emit_move_insn (lh_dest, lh_src);
  emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (1)));
  emit_move_insn (hl_dest, hl_src);
  emit_insn ((*lm8_gen_add3) (scratch, scratch, GEN_INT (1)));
  emit_move_insn (hh_dest, hh_src);
}

/* Implement TARGET_SECONDARY_RELOAD to handle multiword spills.  */
enum reg_class
lm8_secondary_reload (bool in_p,
		      rtx x ATTRIBUTE_UNUSED,
		      enum reg_class cla ATTRIBUTE_UNUSED,
		      enum machine_mode mode,
		      secondary_reload_info *sri)
{ 
  if (MEM_P (x))
    switch (mode)
      {
      default:
	break;
      case HImode:
	sri->icode = in_p ? lm8_reload_inhi : lm8_reload_outhi;
	break;
      case SImode:
	sri->icode = in_p ? lm8_reload_insi : lm8_reload_outsi;
	break;
      }

  return NO_REGS;
}

/* Returns 1 if a value of mode MODE can be stored starting with hard
   register number REGNO.  */
int
lm8_hard_regno_mode_ok (int regno, enum machine_mode mode)
{
  /* The only thing that can go into FRAME_POINTER is a Pmode.  */
  if (regno == FRAME_POINTER_REGNUM && mode == Pmode)
    return 1;

  /* Otherwise disallow all regno/mode combinations that span F_P.  */
  if (regno < FRAME_POINTER_REGNUM
      && (regno + GET_MODE_SIZE (mode)) > FRAME_POINTER_REGNUM)
    return 0;

  if (mode == QImode)
    return 1;

  if (regno >= FRAME_POINTER_REGNUM
      && regno < (FRAME_POINTER_REGNUM + GET_MODE_SIZE (Pmode)))
    return 0;

  /* Modes larger than QImode occupy consecutive registers.  */
  if (regno + GET_MODE_SIZE (mode) > FIRST_PSEUDO_REGISTER)
    return 0;

  return 1;
}

/* Output arithmetic instruction sequence.  */
const char *
lm8_output_arith (enum rtx_code code, rtx *operands)
{
  enum machine_mode mode = GET_MODE (operands[0]);
  int size = GET_MODE_SIZE (mode);
  char insn[16];
  char op[8];
  const char *op1;
  char buf[] = "\t%_0,%_2";
  int i;

  switch (code)
    {
    case PLUS:
      op1 = "add"; break;
    case MINUS:
      op1 = "sub"; break;
    default:
      gcc_unreachable ();
    }
  strcpy (op, op1);
  if (which_alternative)
    strcat (op, "i");

  for (i = 0; i < size; i++)
    {
      strcpy (insn, op);
      if (i)
	strcat (insn, "c");
      buf[2] = buf[6] = 'A' + (char) i;
      strcat (insn, buf);
      output_asm_insn (insn, operands);
    }

  return "";
}

/* Output logic instruction sequence.  */
const char *
lm8_output_logic (enum rtx_code code, rtx *operands)
{
  enum machine_mode mode = GET_MODE (operands[0]);
  int size = GET_MODE_SIZE (mode);
  char insn[16];
  char op[8];
  const char *op1;
  char buf[] = "\t%_0,%_2";
  int i;

  switch (code)
    {
    case AND:
      op1 = "and"; break;
    case IOR:
      op1 = "or"; break;
    case XOR:
      op1 = "xor"; break;
    default:
      gcc_unreachable ();
    }
  strcpy (op, op1);
  if (which_alternative)
    strcat (op, "i");

  for (i = 0; i < size; i++)
    {
      strcpy (insn, op);
      buf[2] = buf[6] = 'A' + (char) i;
      strcat (insn, buf);
      output_asm_insn (insn, operands);
    }

  return "";
}

/* Return true if next cc0 user uses unsigned comparison.  */
bool
lm8_next_cc0_user_unsigned (rtx insn)
{
  rtx cc0_user;
  rtx body;
  rtx set;
  enum rtx_code code;

  cc0_user = next_cc0_user (insn);

  body = PATTERN (cc0_user);
  set = single_set (cc0_user);

  /* Users can be sCC and bCC.  */
  if (JUMP_P (cc0_user)
      && GET_CODE (body) == SET
      && SET_DEST (body) == pc_rtx
      && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
      && XEXP (XEXP (SET_SRC (body), 0), 0) == cc0_rtx)
    code = GET_CODE (XEXP (SET_SRC (body), 0));
  else if (set)
    code = GET_CODE (SET_SRC (body));
  else
    gcc_unreachable ();

  if (code == GT || code == GE || code == LT || code == LE)
    return false;

  return true;
}

/* Return true if next cc0 user uses zero flag comparison.  */
bool
lm8_next_cc0_user_zf (rtx insn)
{
  rtx cc0_user;
  rtx body;
  rtx set;
  enum rtx_code code;

  cc0_user = next_cc0_user (insn);

  body = PATTERN (cc0_user);
  set = single_set (cc0_user);

  /* Users can be sCC and bCC.  */
  if (JUMP_P (cc0_user)
      && GET_CODE (body) == SET
      && SET_DEST (body) == pc_rtx
      && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
      && XEXP (XEXP (SET_SRC (body), 0), 0) == cc0_rtx)
    code = GET_CODE (XEXP (SET_SRC (body), 0));
  else if (set)
    code = GET_CODE (SET_SRC (body));
  else
    gcc_unreachable ();

  if (code == EQ || code == NE)
    return true;

  return false;
}

/* Output compare sequence.  */
const char *
lm8_output_compare (rtx insn, enum machine_mode mode)
{
  if (lm8_next_cc0_user_unsigned (insn))
    switch (mode)
      {
      case QImode:
	return "cmp\tr0,r1";
      case HImode:
	return "call\t__ucmphi2";
      case SImode:
	return "call\t__ucmpsi2";
      default:
	gcc_unreachable ();
      }
  else
    switch (mode)
      {
      case QImode:
	return "call\t__cmpqi2";
      case HImode:
	return "call\t__cmphi2";
      case SImode:
	return "call\t__cmpsi2";
      default:
	gcc_unreachable ();
      }
}

/* Output import/export instruction sequence.  */
const char *
lm8_output_imexport (bool export_insn, rtx *operands)
{
  int size = GET_MODE_SIZE (Pmode);
  char insn[16];
  char op[8];
  char buf[] = "\tr1_,%_1";
  int i;
  
  strcpy (op, "mov");
  if (which_alternative)
    strcat (op, "i");

  for (i = size-1; i; i--)
    {
      strcpy (insn, op);
      buf[3] = '2' + (char) i;
      buf[6] = 'A' + (char) i;
      strcat (insn, buf);
      output_asm_insn (insn, operands);
    }

  strcpy (insn, export_insn ? "export" : "import");
  if (!which_alternative)
    strcat (insn, "i");

  strcat (insn, "\t%0,%A1");
  output_asm_insn (insn, operands);

  return "";
}

/* Override command line options.  */
void
lm8_override_options (void)
{
  if (lm8_cmodel_string != 0)
    {
      if (!strcmp (lm8_cmodel_string, "small"))
	lm8_cmodel = CM_SMALL;
      else if (!strcmp (lm8_cmodel_string, "medium"))
	lm8_cmodel = CM_MEDIUM;
      else if (!strcmp (lm8_cmodel_string, "large"))
	lm8_cmodel = CM_LARGE;
      else
	error ("bad value (%s) for -mcmodel switch", lm8_cmodel_string);
    }
  else
    lm8_cmodel = CM_MEDIUM;

  if (TARGET_SIXTEEN_REGS && lm8_cmodel == CM_LARGE)
    error ("-m16regs is incompatible with LARGE code model.");

  switch (Pmode)
    {
    case QImode:
      lm8_gen_add3 = gen_addqi3;
      lm8_gen_sub3 = gen_subqi3;
      lm8_gen_call_prologue_saves = gen_call_prologue_saves_qi;
      lm8_gen_call_epilogue_restores = gen_call_epilogue_restores_qi;
      lm8_reload_inhi = CODE_FOR_reload_inhi_qi;
      lm8_reload_insi = CODE_FOR_reload_insi_qi;
      lm8_reload_outhi = CODE_FOR_reload_outhi_qi;
      lm8_reload_outsi = CODE_FOR_reload_outsi_qi;
      break;

    case HImode:
      lm8_gen_add3 = gen_addhi3;
      lm8_gen_sub3 = gen_subhi3;
      lm8_gen_call_prologue_saves = gen_call_prologue_saves_hi;
      lm8_gen_call_epilogue_restores = gen_call_epilogue_restores_hi;
      lm8_reload_inhi = CODE_FOR_reload_inhi_hi;
      lm8_reload_insi = CODE_FOR_reload_insi_hi;
      lm8_reload_outhi = CODE_FOR_reload_outhi_hi;
      lm8_reload_outsi = CODE_FOR_reload_outsi_hi;
      break;

    case SImode:
      lm8_gen_add3 = gen_addsi3;
      lm8_gen_sub3 = gen_subsi3;
      lm8_gen_call_prologue_saves = gen_call_prologue_saves_si;
      lm8_gen_call_epilogue_restores = gen_call_epilogue_restores_si;
      lm8_reload_inhi = CODE_FOR_reload_inhi_si;
      lm8_reload_insi = CODE_FOR_reload_insi_si;
      lm8_reload_outhi = CODE_FOR_reload_outhi_si;
      lm8_reload_outsi = CODE_FOR_reload_outsi_si;
      break;

    default:
      gcc_unreachable ();
    }
}

/* Update register usage after having seen the compiler flags.  */
void
lm8_conditional_register_usage (void)
{
  int i;

  for (i = 0; i < LM8_NUM_REGS (Pmode); i++)
    {
      call_used_regs[SP_REGNUM + i] = fixed_regs [SP_REGNUM + i] = 1;
      call_used_regs[TMP_REGNUM + i] = 1;
    }

  switch (LM8_NUM_REGS (Pmode))
    {
    default:
      gcc_unreachable ();

    case 4:
      call_used_regs[R15_REGNUM] = fixed_regs [R15_REGNUM] = 1;
      call_used_regs[R14_REGNUM] = fixed_regs [R14_REGNUM] = 1;
      /* fallthru */

    case 2:
      call_used_regs[R13_REGNUM] = fixed_regs [R13_REGNUM] = 1;

    case 1:
      ;
    }

  /* Mark other possible eliminable registers as call-used.
     For some reason, register allocator marks them as ever live,
     and without this hack, prologue saves them to stack.  */
  switch (Pmode)
    {
    case QImode:
      call_used_regs [FP_REGNUM_MEDIUM] = 1;
      call_used_regs [FP_REGNUM_MEDIUM+1] = 1;
      call_used_regs [FP_REGNUM_LARGE] = 1;
      call_used_regs [FP_REGNUM_LARGE+1] = 1;
      call_used_regs [FP_REGNUM_LARGE+2] = 1;
      call_used_regs [FP_REGNUM_LARGE+3] = 1;
      break;
    case HImode:
      call_used_regs [FP_REGNUM_SMALL] = 1;
      call_used_regs [FP_REGNUM_LARGE] = 1;
      call_used_regs [FP_REGNUM_LARGE+1] = 1;
      call_used_regs [FP_REGNUM_LARGE+2] = 1;
      call_used_regs [FP_REGNUM_LARGE+3] = 1;
      break;
    case SImode:
      call_used_regs [FP_REGNUM_SMALL] = 1;
      call_used_regs [FP_REGNUM_MEDIUM] = 1;
      call_used_regs [FP_REGNUM_MEDIUM+1] = 1;
      break;
    default:
      gcc_unreachable ();
    }
    
  /* Disable not-implemented registers here.  */
  if (TARGET_SIXTEEN_REGS)
    for (i = 16; i <= 31; i++)
      fixed_regs[i] = call_used_regs[i] = 1;
}

/* Given FROM and TO register numbers, say whether this elimination is
   allowed.  If stack alignment is needed, we can only replace argument
   pointer with hard frame pointer, or replace frame pointer with stack
   pointer.  Otherwise, frame pointer elimination is automatically
   handled and all other eliminations are valid.  */
int
lm8_can_eliminate (const int from, const int to)
{
  if (from == ARG_POINTER_REGNUM)
    {
      if (to == STACK_POINTER_REGNUM)
	return !frame_pointer_needed;

      if (to == FRAME_POINTER_REGNUM)
	return 1;
    }

  if (from == FRAME_POINTER_REGNUM
      || (GET_MODE_SIZE (Pmode) > 0
	  && from == FRAME_POINTER_REGNUM+1)
      || (GET_MODE_SIZE (Pmode) > 2
	  && (from == FRAME_POINTER_REGNUM+2
	      || from == FRAME_POINTER_REGNUM+3)))
    return !frame_pointer_needed;

  return 0;
}

/* Return the offset between two registers, one to be eliminated,
   and the other its replacement, at the start of a routine.  */
int
lm8_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT offset = 0;
  int frame_size = get_frame_size ();

  lm8_compute_frame_size (frame_size);

  switch (from)
    {
    case ARG_POINTER_REGNUM:
      if (to == SP_REGNUM_SMALL
	  || to == SP_REGNUM_MEDIUM
	  || to == SP_REGNUM_LARGE)
	offset = (current_frame_info.total_size
		  -current_frame_info.pretend_size);
      else if (to == FP_REGNUM_SMALL
	       || to == FP_REGNUM_MEDIUM
	       || to == FP_REGNUM_LARGE)
	offset = current_frame_info.callee_size;
      else
	gcc_unreachable ();
      break;
  
    case FP_REGNUM_SMALL:
    case FP_REGNUM_MEDIUM:
    case FP_REGNUM_MEDIUM+1:
    case FP_REGNUM_LARGE:
    case FP_REGNUM_LARGE+1:
    case FP_REGNUM_LARGE+2:
    case FP_REGNUM_LARGE+3:
       if (to == SP_REGNUM_SMALL
	   || to == SP_REGNUM_MEDIUM
	   || to == SP_REGNUM_MEDIUM+1
	   || to == SP_REGNUM_LARGE
	   || to == SP_REGNUM_LARGE+1
	   || to == SP_REGNUM_LARGE+2
	   || to == SP_REGNUM_LARGE+3)
	offset = (current_frame_info.locals_size
		  + current_frame_info.args_size);
      else
	gcc_unreachable ();
      break;

    default:
      gcc_unreachable ();
    }

  return offset;
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P.  */
int
lm8_legitimate_address_p (enum machine_mode mode ATTRIBUTE_UNUSED,
			  rtx x, int strict)
{
   /* (rM) */
  if (!REG_P (x))
    return false;
  if ((strict && STRICT_REG_OK_FOR_BASE_P (x))
      || (!strict && NONSTRICT_REG_OK_FOR_BASE_P (x)))
    return true;

  return false;
}

/* Update the condition code in the INSN.  */
void
lm8_notice_update_cc (rtx body ATTRIBUTE_UNUSED, rtx insn)
{
  rtx set;
  
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect CC at all.  */
      break;

    case CC_SET_Z:
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
	{
	  cc_status.flags |= CC_NOT_SIGNED | CC_NO_OVERFLOW;
	  cc_status.value1 = SET_DEST (set);
	}
      break;

    case CC_SET_CZ:
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
	{
	  cc_status.flags |= CC_NOT_SIGNED;
	  cc_status.value1 = SET_DEST (set);
	}
      break;

    case CC_COMPARE:
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
	{
	  cc_status.flags |= CC_NOT_SIGNED;
	  cc_status.value1 = SET_SRC (set);
	}
      break;
      
    case CC_CLOBBER:
      /* Insn doesn't leave CC in a usable state.  */
      CC_STATUS_INIT;
      break;
    }
}

void
lm8_print_operand (FILE * file, rtx op, int letter)
{
  enum rtx_code code;
  int abcd;

  if (letter >= 'A' && letter <= 'D')
    abcd = letter - 'A';
  else
    abcd = -1;

  code = GET_CODE (op);

  if (letter == 'p')
    {
      int reg;

      gcc_assert (code == MEM);
      reg = REGNO (XEXP (op, 0));

      switch (LM8_NUM_REGS (Pmode))
	{
	case 4:
	  if (reg+2 != 14)
	    {
	      fprintf (file, "mov\tr15,%s\t# R15 update\n\t",
		       reg_names[reg+3]);
	      fprintf (file, "mov\tr14,%s\t# R14 update\n\t",
		       reg_names[reg+2]);
	    }
	  /* FALLTHRU */
	case 2:
	  if (reg+1 != 13)
	    fprintf (file, "mov\tr13,%s\t# R13 update\n\t",
		     reg_names[reg+1]);
	  /* FALLTHRU */
	case 1:
	  break;

	default:
	  gcc_unreachable ();
	}
      return;
    }

  if (code == REG)
    {
      int reg = REGNO (op);

      if (abcd > 0)
	reg += abcd;

      fprintf (file, "%s", reg_names[reg]);
    }
  else if (code == MEM)
    output_address (XEXP (op, 0));
  else
    {
      switch (abcd)
	{
	case -1:
	  output_addr_const (file, op);
	  return;

	case 0: fputs ("_lo(", file); break;
	case 1: fputs ("_hi(", file); break;
	case 2: fputs ("_higher(", file); break;
	case 3: fputs ("_highest(", file); break;
	default:
	  gcc_unreachable ();
	}
      output_addr_const (file, op);
      fputc (')', file);
    }
}

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */
void
lm8_print_operand_address (FILE * file, rtx addr)
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "%s", reg_names[REGNO (addr)]);
      break;

    case MEM:
      output_address (XEXP (addr, 0));
      break;

    default:
      fatal_insn ("invalid addressing mode", addr);
      break;
    }
}

/* Write the extra assembler code needed to declare
   a function properly.  */
void lm8_asm_declare_function_name (FILE * file, char * name, tree decl)
{
  ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");

  if (lookup_attribute ("interrupt", DECL_ATTRIBUTES (decl)))
    ASM_OUTPUT_TYPE_DIRECTIVE (file, "__irq_save_restore", "function");

  ASM_DECLARE_RESULT (file, DECL_RESULT (decl));
  ASM_OUTPUT_LABEL (file, name);
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */
rtx
lm8_function_arg (CUMULATIVE_ARGS cum, enum machine_mode mode,
		  tree type, int named)
{
  if (mode == VOIDmode)
    /* Compute operand 2 of the call insn.  */
    return GEN_INT (0);

  if (targetm.calls.must_pass_in_stack (mode, type))
    return NULL_RTX;

  if (named && cum < LM8_NUM_ARG_REGS)
    return gen_rtx_REG (mode, LM8_FIRST_ARG_REG + cum);

  return NULL_RTX;
}

/* Add a comment describing frame parameters into
   the instruction stream.  */
static void
lm8_output_function_prologue (FILE *f,
			      HOST_WIDE_INT frame_size ATTRIBUTE_UNUSED)
{
  lm8_compute_frame_size (frame_size);

  asm_fprintf (f, "\t# pretend_size = %wd, callee_size = %wd\n",
	       current_frame_info.pretend_size,
	       current_frame_info.callee_size);
  asm_fprintf (f, "\t# locals_size = %wd, args_size = %wd\n",
	       current_frame_info.locals_size,
	       current_frame_info.args_size);
  asm_fprintf (f, "\t# total_size = %wd, reg_save_mask = %#x\n",
	       current_frame_info.total_size,
	       current_frame_info.reg_save_mask);
}

/* Small structures are returned in a return register quad,
   others are passed through invisible first argument.  */

static bool
lm8_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  return ((int_size_in_bytes (type) > 4 * UNITS_PER_WORD)
	  || (int_size_in_bytes (type) == -1));
}

/* Perform any needed actions needed for a function that is
   receiving a variable number of arguments.  */
static void
lm8_setup_incoming_varargs (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			    tree type, int *pretend_size, int no_rtl)
{
  int first_anon_arg;
  tree fntype;
  int stdarg_p;

  fntype = TREE_TYPE (current_function_decl);
  stdarg_p = (TYPE_ARG_TYPES (fntype) != 0
	      && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		  != void_type_node));

  if (stdarg_p)
    first_anon_arg = *cum + LM8_FIRST_ARG_REG;
  else
    {
      /* this is the common case, we have been passed details setup
	 for the last named argument, we want to skip over the
	 registers, if any used in passing this named paramter in
	 order to determine which is the first registers used to pass
	 anonymous arguments.  */
      int size;

      if (mode == BLKmode)
	size = int_size_in_bytes (type);
      else
	size = GET_MODE_SIZE (mode);

      first_anon_arg =
	*cum + LM8_FIRST_ARG_REG +
	((size + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
    }

  if (no_rtl)
    return;

  if (first_anon_arg < (LM8_FIRST_ARG_REG + LM8_NUM_ARG_REGS))
    {
      int first_reg_offset = first_anon_arg;
      int size = LM8_FIRST_ARG_REG + LM8_NUM_ARG_REGS - first_anon_arg;
      rtx regblock;

      regblock = gen_rtx_MEM (BLKmode,
			      plus_constant (arg_pointer_rtx,
					     FIRST_PARM_OFFSET (0)));
      move_block_from_reg (first_reg_offset, regblock, size);

      *pretend_size = size * UNITS_PER_WORD;
    }
}

/* Small structures are passed in registers, other
   variable sized types are passed by reference.  */
static bool
lm8_pass_by_reference (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
		       enum machine_mode mode ATTRIBUTE_UNUSED,
		       const_tree type, bool named ATTRIBUTE_UNUSED)
{
  if (!type)
    return 0;

  return int_size_in_bytes (type) > 4 * UNITS_PER_WORD;
}

/* Split multi-word function argument that crosses the boundary to
   register passing and stack passing part.  */
static int
lm8_arg_partial_bytes (CUMULATIVE_ARGS *cum, enum machine_mode mode,
		       tree type, bool named ATTRIBUTE_UNUSED)
{
  if (*cum < LM8_NUM_ARG_REGS
      && (*cum + LM8_NUM_REGS2 (mode, type)) > LM8_NUM_ARG_REGS)
    return (LM8_NUM_ARG_REGS - *cum) * UNITS_PER_WORD;

  return 0;
}

/* Codes for all the LM8 builtins.  */
enum lm8_builtins
{
  LM8_BUILTIN_IMPORT,
  LM8_BUILTIN_EXPORT
};

/* Classifies the prototype of a builtin function.  */
enum lm8_function_type
{
  LM8_QI_FTYPE_SIZE,
  LM8_VOID_FTYPE_QI_SIZE,

  /* The last type.  */
  LM8_MAX_FTYPE_MAX
};

/* Specifies how a builtin function should be converted into rtl.  */
enum lm8_builtin_type
{
  /* The builtin corresponds directly to an .md pattern.  The return
     value is mapped to operand 0 and the arguments are mapped to
     operands 1 and above.  */
  LM8_BUILTIN_DIRECT,

  /* The builtin corresponds directly to an .md pattern.  There is no return
     value and the arguments are mapped to operands 0 and above.  */
  LM8_BUILTIN_DIRECT_NO_TARGET
};

/* LM8 builtin function support. */
struct builtin_description
{
  /* The name of the builtin function.  */
  const char *name;

  /* Corresponding function code. */
  const enum lm8_builtins code;

  /* Specifies how the function should be expanded.  */
  const enum lm8_builtin_type builtin_type;

  /* The function's prototype.  */
  const enum lm8_function_type function_type;
};

/* Define a LM8_BUILTIN_DIRECT_function.  */
#define DIRECT_BUILTIN(INSN, CODE, FUNCTION_TYPE)		\
  { "__builtin_" #INSN, CODE, LM8_BUILTIN_DIRECT, FUNCTION_TYPE }


/* Define a LM8_BUILTIN_DIRECT_NO_TARGET function.  */
#define DIRECT_NO_TARGET_BUILTIN(INSN, CODE, FUNCTION_TYPE)		\
  { "__builtin_" #INSN, CODE, LM8_BUILTIN_DIRECT_NO_TARGET, FUNCTION_TYPE }

static const struct builtin_description lm8_bdesc[] =
{
  DIRECT_BUILTIN (import, LM8_BUILTIN_IMPORT, LM8_QI_FTYPE_SIZE),
  DIRECT_NO_TARGET_BUILTIN (export, LM8_BUILTIN_EXPORT, LM8_VOID_FTYPE_QI_SIZE)
};

/* Init builtin functions.  This is called from TARGET_INIT_BUILTIN.  */
static void
lm8_init_builtins (void)
{
  const struct builtin_description *d;
  size_t i;
  tree types[(int) LM8_MAX_FTYPE_MAX];

  types[LM8_QI_FTYPE_SIZE]
    = build_function_type_list (char_type_node,
				size_type_node,
				NULL_TREE);
  types[LM8_VOID_FTYPE_QI_SIZE]
    = build_function_type_list (void_type_node,
				char_type_node, size_type_node,
				NULL_TREE);

  for (i = 0, d = lm8_bdesc;
       i < ARRAY_SIZE (lm8_bdesc);
       i++, d++)
    add_builtin_function (d->name, types[d->function_type], d->code,
			  BUILT_IN_MD, NULL, NULL_TREE);
}

/* Take argument ARGNO from EXP's argument list and convert it into a
   form suitable for input operand OPNO of instruction ICODE.  Return the
   value.  */
static rtx
lm8_prepare_builtin_arg (enum insn_code icode,
			 unsigned int opno, tree exp, unsigned int argno)
{
  tree arg;
  rtx value;
  enum machine_mode mode;

  arg = CALL_EXPR_ARG (exp, argno);
  value = expand_normal (arg);
  mode = insn_data[icode].operand[opno].mode;
  if (!insn_data[icode].operand[opno].predicate (value, mode))
    {
      value = copy_to_mode_reg (TYPE_MODE (TREE_TYPE (arg)), value);

      /* Check the predicate again.  */
      if (!insn_data[icode].operand[opno].predicate (value, mode))
	{
	  error ("invalid argument to builtin function");
	  return const0_rtx;
	}
    }

  return value;
}

/* Return an rtx suitable for output operand OP of instruction ICODE.
   If TARGET is non-null, try to use it where possible.  */
static rtx
lm8_prepare_builtin_target (enum insn_code icode, unsigned int op, rtx target)
{
  enum machine_mode mode;

  mode = insn_data[icode].operand[op].mode;
  if (target == 0 || !insn_data[icode].operand[op].predicate (target, mode))
    target = gen_reg_rtx (mode);

  return target;
}

/* Expand an LM8_BUILTIN_DIRECT function.  ICODE is the code of the
   .md pattern and ARGLIST is the list of function arguments.  TARGET,
   if nonnull, suggests a good place to put the result.
   HAS_TARGET indicates the function must return something.  */
static rtx
lm8_expand_builtin_direct (enum insn_code icode, rtx target, tree exp,
			   bool has_target)
{
  rtx ops[MAX_RECOG_OPERANDS];
  int opno = 0;
  int argno;

  if (has_target)
    {
      /* We save target to ops[0].  */
      ops[opno] = lm8_prepare_builtin_target (icode, opno, target);
      opno++;
    }

  /* Map the arguments to the other operands.  The n_operands value
     for an expander includes match_dups and match_scratches as well as
     match_operands, so n_operands is only an upper bound on the number
     of arguments to the expander function.  */
  gcc_assert (opno + call_expr_nargs (exp) <= insn_data[icode].n_operands);
  for (argno = 0; argno < call_expr_nargs (exp); argno++, opno++)
    ops[opno] = lm8_prepare_builtin_arg (icode, opno, exp, argno);

  switch (opno)
    {
    case 1:
      emit_insn (GEN_FCN (icode) (ops[0]));
      break;

    case 2:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1]));
      break;

    default:
      gcc_unreachable ();
    }
  return target;
}

/* Expand builtin functions.  This is called from TARGET_EXPAND_BUILTIN.  */
rtx
lm8_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
		    enum machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore ATTRIBUTE_UNUSED)
{
  const struct builtin_description *d;
  size_t i;
  enum insn_code icode;
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);

  for (i = 0, d = lm8_bdesc;
       i < ARRAY_SIZE (lm8_bdesc);
       i++, d++)
    if (d->code == fcode)
      {
	if (fcode == LM8_BUILTIN_IMPORT)
	  switch (Pmode)
	    {
	    case QImode: icode = CODE_FOR_importqi; break;
	    case HImode: icode = CODE_FOR_importhi; break;
	    case SImode: icode = CODE_FOR_importsi; break;

	    default:
	      gcc_unreachable ();
	    }
	else if (fcode == LM8_BUILTIN_EXPORT)
	  switch (Pmode)
	    {
	    case QImode: icode = CODE_FOR_exportqi; break;
	    case HImode: icode = CODE_FOR_exporthi; break;
	    case SImode: icode = CODE_FOR_exportsi; break;

	    default:
	      gcc_unreachable ();
	    }
	else
	  gcc_unreachable ();

	switch (d->builtin_type)
	  {
	  case LM8_BUILTIN_DIRECT:
	    return lm8_expand_builtin_direct (icode, target, exp, true);

	  case LM8_BUILTIN_DIRECT_NO_TARGET:
	    return lm8_expand_builtin_direct (icode, target, exp, false);

	  default:
	    gcc_unreachable ();
	  }
      }

  return NULL_RTX;
}

/* Handle "interrupt" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
lm8_handle_fndecl_attribute (tree *node, tree name,
			     tree args ATTRIBUTE_UNUSED,
			     int flags ATTRIBUTE_UNUSED,
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qs attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static bool
lm8_rtx_costs (rtx x ATTRIBUTE_UNUSED, int code,
               int outer_code ATTRIBUTE_UNUSED, int *total,
               bool speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case CONST_INT:
      /* These can be used anywhere. */
      *total = 0;
      return true;

    default:
      return false;
    }
}

