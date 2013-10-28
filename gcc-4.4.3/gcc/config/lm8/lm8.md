;; -*- Mode: Scheme -*-
;; Machine description of the Lattice Mico8 architecture for GNU C compiler.

;; Contributed by Beyond Semiconductor (www.beyondsemi.com)

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  

;; Special characters after '%':
;;  A  Handle low part of 16bit value.
;;  B  Handle high part.

(define_constants
  [(UNSPECV_PROLOGUE_SAVES	0)
   (UNSPECV_EPILOGUE_RESTORES	1)

   (UNSPECV_IMPORT		2)
   (UNSPECV_EXPORT		3)]
)

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one alternative will match.
(define_mode_iterator P [(QI "Pmode == QImode")
			 (HI "Pmode == HImode")
			 (SI "Pmode == SImode")])

;; Include predicate and constraint definitions
(include "predicates.md")
(include "constraints.md")

;; Condition code settings.
(define_attr "cc" "none,set_z,set_cz,compare,clobber"
  (const_string "none"))

;; Number of instructions
(define_attr "length" "" (const_int 1))

;; Integer word modes
(define_mode_iterator IMODE [QI HI SI])
(define_mode_iterator IMODE24 [HI SI])

;; ---------------------------------
;;              nop 
;; ---------------------------------

(define_insn "nop"  
  [(const_int 0)]
  ""
  "nop")

;; ---------------------------------
;;               mov 
;; ---------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:IMODE 0 "nonimmediate_operand" "")
	(match_operand:IMODE 1 "general_operand" ""))]
  ""
{
  if (lm8_expand_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

(define_insn_and_split "*movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,&r,m")
	(match_operand:SI 1 "nonimmediate_operand" "r,m,r"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "#"
  ""
  [(const_int 0)]
{
  lm8_split_simode_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "cc" "clobber")])

(define_insn "*movsi_imm"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "immediate_operand" "i"))]
  ""
  "movi\t%A0,%A1\n\tmovi\t%B0,%B1\n\tmovi\t%C0,%C1\n\tmovi\t%D0,%D1"
  [(set_attr "cc" "clobber")
   (set_attr "length" "4")])

(define_insn "*movhi_high"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(high:HI (match_operand:HI 1 "immediate_operand" "i")))]
  ""
  "movi\t%B0,_hi(%1)"
  [(set_attr "cc" "set_z")])

(define_insn "*movhi_lo_sum"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lo_sum:HI (match_operand:HI 1 "register_operand" "0")
                   (match_operand:HI 2 "immediate_operand" "i")))]
  ""
  "movi\t%A0,_lo(%2)"
 [(set_attr "cc" "set_z")])

(define_insn_and_split "*movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,&r,m")
	(match_operand:HI 1 "move_operand"         "r,i,m,r"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)"
  "#"
  "&& GET_CODE (operands[1]) != HIGH
   && GET_CODE (operands[1]) != LO_SUM"
  [(const_int 0)]
{
  lm8_split_himode_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "cc" "clobber")])

(define_expand "reload_in<IMODE24:mode>_<P:mode>"
  [(parallel [(match_operand:IMODE24 0 "register_operand" "=&r")
              (match_operand:IMODE24 1 "memory_operand" "m")
	      (match_operand:P 2 "register_operand" "=&r")])]
  ""
{
  lm8_split_<IMODE24:mode>mode_spill (operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "reload_out<IMODE24:mode>_<P:mode>"
  [(parallel [(match_operand 0 "memory_operand" "=m")
	      (match_operand:IMODE24 1 "register_operand" "r")
	      (match_operand:P 2 "register_operand" "=&r")])]
  ""
{
  lm8_split_<IMODE24:mode>mode_spill (operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "*movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:QI 1 "general_operand"       "r,i,m,r"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   mov\t%0,%1
   movi\t%0,%1
   %p1lspi\t%0,%1
   %p0sspi\t%1,%0"
  [(set_attr "cc" "set_z")])

;; ---------------------------------
;;            arithmetic 
;; ---------------------------------

(define_code_iterator arith [plus minus])
(define_code_attr comm [(plus "%") (minus "")])
(define_code_attr arith_insn [(plus "add") (minus "sub")])

(define_insn "<arith_insn><mode>3"
  [(set (match_operand:IMODE 0 "register_operand" "=r,r")
        (arith:IMODE (match_operand:IMODE 1 "register_operand"  "<comm>0,0")
		     (match_operand:IMODE 2 "nonmemory_operand" "r,i")))]
  ""
  "*return lm8_output_arith (<CODE>, operands);"
  [(set (attr "cc")
	(if_then_else (match_operand:QI 0 "" "")
		      (const_string "set_cz")
		      (const_string "clobber")))])

;; ---------------------------------
;;          one complement
;; ---------------------------------

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(not:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "xori\t%0,0xff"
  [(set_attr "cc" "set_z")])

;; ---------------------------------
;;             logical 
;; ---------------------------------

(define_code_iterator logic [and ior xor])

(define_insn "<code><mode>3"
  [(set (match_operand:IMODE 0 "register_operand" "=r,r")
        (logic:IMODE (match_operand:IMODE 1 "register_operand"  "%0,0")
		     (match_operand:IMODE 2 "nonmemory_operand" "r,i")))]
  ""
  "*return lm8_output_logic (<CODE>, operands);"
  [(set (attr "cc")
	(if_then_else (match_operand:QI 0 "" "")
		      (const_string "set_z")
		      (const_string "clobber")))])

;; ---------------------------------
;;         compare 
;; ---------------------------------

(define_expand "cmpqi"
  [(set (reg:QI 0) (match_operand:QI 0 "general_operand" ""))
   (set (reg:QI 1) (match_operand:QI 1 "general_operand" ""))
   (parallel [(set (cc0) (compare (reg:QI 0) (reg:QI 1)))
		   (clobber (reg:QI 12))
		   (clobber (reg:QI 13))])]
  ""
  "")

(define_peephole2
  [(set (reg:QI 0)
	(and:QI (reg:QI 0)
		(match_operand:QI 0 "nonmemory_operand" "")))
   (set (reg:QI 1)
	(const_int 0))
   (parallel [(set (cc0)
		   (compare (reg:QI 0) (reg:QI 1)))
	      (clobber (reg:QI 12))
	      (clobber (reg:QI 13))])]
  "lm8_next_cc0_user_zf (peep2_next_insn (2))
   && peep2_regno_dead_p (3, 0)
   && peep2_regno_dead_p (3, 1)"
  [(set (cc0) (compare
	        (and:QI (reg:QI 0) (match_dup 0))
		(const_int 0)))]	
  "")

;; If the insn sequence compares the result of a mask operation with an
;; immediate operand that has exactly one non-zero byte against zero
;; (i.e. '(<reg> & 0xa300) != 0'), convert the insn to a single testi
;; instruction with a single byte nonzero operand, but only if the
;; intermediate masking result is unused outside the comparison.
(define_peephole2
  [(set (reg:HI 0)
	(and:HI (reg:HI 0)
		(match_operand:HI 1 "one_byte_non_zero_operand" "")))
   (set (reg:QI 2)
	(const_int 0))
   (set (reg:QI 3)
	(const_int 0))
   (parallel [(set (cc0)
		   (compare (reg:HI 0) (reg:HI 2)))
	      (clobber (reg:QI 12))
	      (clobber (reg:QI 13))])]
  "lm8_next_cc0_user_zf (peep2_next_insn (3))
   && peep2_regno_dead_p (4, 0)
   && peep2_regno_dead_p (4, 1)
   && peep2_regno_dead_p (4, 2)
   && peep2_regno_dead_p (4, 3)"
  [(set (cc0) (compare
	        (and:QI (match_dup 0) (match_dup 1))
		(const_int 0)))]	
{
  unsigned HOST_WIDE_INT x = INTVAL (operands[1]);
  unsigned int i;

  for (i = 0; i < 2; i++)
    {
      if (x & 0xff)
        break;
      x >>= 8;
    }
  /* Remember that because of the "one_byte_non_zero_operand" predicate
   * x will now definatly fit into QImode (ie. x <= 0xff). */

  operands[1] = GEN_INT (trunc_int_for_mode (x, QImode));
  operands[0] = gen_rtx_REG (QImode, i);
})

;; If the insn sequence compares the result of a mask operation with an
;; immediate operand that has exactly one non-zero byte against zero
;; (i.e. '(<reg> & 0xa300) != 0'), convert the insn to a single testi
;; instruction with a single byte nonzero operand, but only if the
;; intermediate masking result is unused outside the comparison.
(define_peephole2
  [(set (reg:SI 0)
	(and:SI (reg:SI 0)
		(match_operand:SI 1 "one_byte_non_zero_operand" "")))
   (set (reg:SI 4)
	(const_int 0))
   (parallel [(set (cc0)
		   (compare (reg:SI 0) (reg:SI 4)))
	      (clobber (reg:QI 12))
	      (clobber (reg:QI 13))])]
  "lm8_next_cc0_user_zf (peep2_next_insn (2))
   && peep2_regno_dead_p (3, 0)
   && peep2_regno_dead_p (3, 1)
   && peep2_regno_dead_p (3, 2)
   && peep2_regno_dead_p (3, 3)
   && peep2_regno_dead_p (3, 4)
   && peep2_regno_dead_p (3, 5)
   && peep2_regno_dead_p (3, 6)
   && peep2_regno_dead_p (3, 7)"
  [(set (cc0) (compare
	        (and:QI (match_dup 0) (match_dup 1))
		(const_int 0)))]	
{
  unsigned HOST_WIDE_INT x = INTVAL (operands[1]);
  unsigned int i;

  for (i = 0; i < 4; i++)
    {
      if (x & 0xff)
        break;
      x >>= 8;
    }
  /* Remember that because of the "one_byte_non_zero_operand" predicate
   * x will now definatly fit into QImode (ie. x <= 0xff). */

  operands[1] = GEN_INT (trunc_int_for_mode (x, QImode));
  operands[0] = gen_rtx_REG (QImode, i);
})

(define_insn "*cmpqi_test_1"
  [(set (cc0)
	(compare
	  (and:QI (match_operand:QI 0 "register_operand" "%r,r")
		  (match_operand:QI 1 "nonmemory_operand" "r,i"))
	  (const_int 0)))]
  "reload_completed
   && lm8_next_cc0_user_zf (insn)"
  "@
   test\t%0,%1
   testi\t%0,%1"
  [(set_attr "cc" "set_z")])

(define_peephole2
  [(set (reg:QI 1)
	(match_operand:QI 0 "nonmemory_operand" ""))
   (parallel [(set (cc0)
		   (compare (reg:QI 0) (reg:QI 1)))
	      (clobber (reg:QI 12))
	      (clobber (reg:QI 13))])]
  "lm8_next_cc0_user_unsigned (peep2_next_insn (1))
   && peep2_regno_dead_p (2, 1)"
  [(set (cc0) (compare (reg:QI 0) (match_dup 0)))]
  "")

(define_insn "*cmpqi_uns_1"
  [(set (cc0)
	(compare (match_operand:QI 0 "register_operand" "r,r")
		 (match_operand:QI 1 "nonmemory_operand" "r,i")))]
  "reload_completed
   && lm8_next_cc0_user_unsigned (insn)"
  "@
   cmp\t%0,%1
   cmpi\t%0,%1"
  [(set_attr "cc" "compare")])

(define_insn "*cmpqi_1"
  [(set (cc0) (compare (reg:QI 0) (reg:QI 1)))
   (clobber (reg:QI 12))
   (clobber (reg:QI 13))]
  ""
  "*return lm8_output_compare (insn, QImode);"
  [(set_attr "cc" "compare")])


(define_expand "cmphi"
  [(set (reg:HI 0) (match_operand:HI 0 "move_operand" ""))
   (set (reg:HI 2) (match_operand:HI 1 "move_operand" ""))
   (parallel [(set (cc0) (compare (reg:HI 0) (reg:HI 2)))
		   (clobber (reg:QI 12))
		   (clobber (reg:QI 13))])]
  ""
  "")

(define_insn "*cmphi_1"
  [(set (cc0) (compare (reg:HI 0) (reg:HI 2)))
   (clobber (reg:QI 12))
   (clobber (reg:QI 13))]
  ""
  "*return lm8_output_compare (insn, HImode);"
  [(set_attr "cc" "compare")])

(define_expand "cmpsi"
  [(set (reg:SI 0) (match_operand:SI 0 "general_operand" ""))
   (set (reg:SI 4) (match_operand:SI 1 "general_operand" ""))
   (parallel [(set (cc0) (compare (reg:SI 0) (reg:SI 4)))
		   (clobber (reg:QI 12))
		   (clobber (reg:QI 13))])]
  ""
  "")

(define_insn "*cmpsi_1"
  [(set (cc0) (compare (reg:SI 0) (reg:SI 4)))
   (clobber (reg:QI 12))
   (clobber (reg:QI 13))]
  ""
  "*return lm8_output_compare (insn, SImode);"
  [(set_attr "cc" "compare")])

;; ---------------------------------
;;       unconditional branch
;; ---------------------------------

(define_insn "jump"
  [(set (pc)
  	(label_ref (match_operand 0 "" "")))]
  ""
  "b\t%0")

(define_expand "indirect_jump"
  [(set (pc)
  	(match_operand:HI 0 "register_operand" "r"))]
  ""
  "sorry (\"indirect jumps not implemented on this target\");")

;; ---------------------------------
;;        conditional branch
;; ---------------------------------

(define_code_iterator cmp_z [eq ne])
(define_code_attr z [(eq "z") (ne "nz")])

(define_insn "b<code>"
  [(set (pc)
	(if_then_else (cmp_z (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "b<z>\t%0\t# <code>")

(define_code_iterator cmp_c [ltu geu])
(define_code_attr c [(ltu "c") (geu "nc")])


(define_insn "b<code>"
  [(set (pc)
	(if_then_else (cmp_c (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "b<c>\t%0\t# <code>")

;; %%% We want to avoid these
(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bc\t%0\t# |\n\tbz\t%0\t# |leu"
  [(set_attr "length" "2")])

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bc\t.+3\t# |\n\tbnz\t%0\t# |gtu"
  [(set_attr "length" "2")])

(define_code_iterator invalid_cond [gt ge lt le])

(define_insn "b<code>"
  [(set (pc)
	(if_then_else (invalid_cond (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* gcc_unreachable ();")

;;
;; ---------------------------------
;;               call 
;; ---------------------------------

(define_expand "call"
  [(call (match_operand 0 "" "")
	 (match_operand 1 "" ""))]
  ""
{
  rtx addr = XEXP (operands[0], 0);
  if (!CONSTANT_ADDRESS_P (addr))
    sorry ("indirect calls not implemented on this target");
})

(define_expand "sibcall"
  [(call (match_operand 0 "" "")
	 (match_operand 1 "" ""))]
  ""
{
  rtx addr = XEXP (operands[0], 0);
  gcc_assert (CONSTANT_ADDRESS_P (addr));
})

(define_insn "*call"
  [(call (mem:HI (match_operand:P 0 "call_operand" "s"))
	 (match_operand 1 "" ""))]
  ""
{
  if (SIBLING_CALL_P (insn))
    return "b\t%0";
  else
    return "call\t%0";
})

(define_expand "call_value"
  [(set (match_operand 0 "" "")
	(call (match_operand 1 "" "")
	      (match_operand 2 "" "")))]
  ""
{
  rtx addr = XEXP (operands[1], 0);
  if (!CONSTANT_ADDRESS_P (addr))
    sorry ("indirect calls not implemented on this target");
})

(define_expand "sibcall_value"
  [(set (match_operand 0 "" "")
	(call (match_operand 1 "" "")
	      (match_operand 2 "" "")))]
  ""
{
  rtx addr = XEXP (operands[1], 0);
  gcc_assert (CONSTANT_ADDRESS_P (addr));
})

(define_insn "*call_value"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:HI (match_operand:P 1 "call_operand" "s"))
	      (match_operand 2 "" "")))]
  ""
{
  if (SIBLING_CALL_P (insn))
    return "b\t%1";
  else
    return "call\t%1";
})

(define_insn "return_internal"
  [(return)]
  ""
  "ret")

;; ---------------------------------
;;     function entry / exit 
;; ---------------------------------

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  lm8_expand_prologue ();
  DONE;
})

(define_insn "call_prologue_saves_qi"
  [(unspec_volatile:QI [(const_int 0)]
		       UNSPECV_PROLOGUE_SAVES)
   (use (match_operand:QI 0 "const_int_operand" ""))
   (use (reg:QI 12))]
  ""
  "call\t__prologue_save_r%0"
  [(set_attr "cc" "clobber")])

(define_insn "call_prologue_saves_hi"
  [(unspec_volatile:HI [(const_int 0)]
		       UNSPECV_PROLOGUE_SAVES)
   (use (match_operand:HI 0 "const_int_operand" ""))
   (use (reg:HI 12))]
  ""
  "call\t__prologue_save_r%0"
  [(set_attr "cc" "clobber")])

(define_insn "call_prologue_saves_si"
  [(unspec_volatile:SI [(const_int 0)]
		       UNSPECV_PROLOGUE_SAVES)
   (use (match_operand:SI 0 "const_int_operand" ""))
   (use (reg:SI 12))]
  ""
  "call\t__prologue_save_r%0"
  [(set_attr "cc" "clobber")])

(define_expand "epilogue"
  [(const_int 0)]
  ""
{
  lm8_expand_epilogue (false);
  DONE;
})

(define_insn "call_epilogue_restores_qi"
  [(unspec_volatile:QI [(const_int 0)]
		       UNSPECV_EPILOGUE_RESTORES)
   (use (match_operand:QI 0 "const_int_operand" ""))
   (use (reg:QI 12))]
  ""
  "call\t__epilogue_restore_r%0"
  [(set_attr "cc" "clobber")])

(define_insn "call_epilogue_restores_hi"
  [(unspec_volatile:HI [(const_int 0)]
		       UNSPECV_EPILOGUE_RESTORES)
   (use (match_operand:HI 0 "const_int_operand" ""))
   (use (reg:HI 12))]
  ""
  "call\t__epilogue_restore_r%0"
  [(set_attr "cc" "clobber")])

(define_insn "call_epilogue_restores_si"
  [(unspec_volatile:SI [(const_int 0)]
		       UNSPECV_EPILOGUE_RESTORES)
   (use (match_operand:SI 0 "const_int_operand" ""))
   (use (reg:SI 12))]
  ""
  "call\t__epilogue_restore_r%0"
  [(set_attr "cc" "clobber")])

(define_expand "sibcall_epilogue"
  [(const_int 0)]
  ""
{
  lm8_expand_epilogue (true);
  DONE;
})

;; ---------------------------------
;;             unspecs
;; ---------------------------------

(define_insn "import<mode>"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
	(unspec_volatile:QI [(match_operand:P 1 "nonmemory_operand" "r,K")]
			    UNSPECV_IMPORT))]
  ""
  "*return lm8_output_imexport (0, operands);"
  [(set_attr "cc" "clobber")])

(define_insn "export<mode>"
  [(unspec_volatile:QI [(match_operand:QI 0 "register_operand" "r,r")
			(match_operand:P 1 "nonmemory_operand" "r,K")]
		       UNSPECV_EXPORT)]
  ""
  "*return lm8_output_imexport (1, operands);"
  [(set_attr "cc" "clobber")])
