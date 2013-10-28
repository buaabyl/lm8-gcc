;; Predicate definitions for Lattice Mico8 architecture.
;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;
;; Contributed by Beyond Semiconductor (www.beyondsemi.com)
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Return true if OP is a valid call operand.
(define_predicate "call_operand"
  (match_code "symbol_ref"))

;; Returns 1 if OP is a valid move source operand

(define_predicate "move_operand"
  (match_operand 0 "general_operand")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return false;

    default:
      return true;
    }
})

;; Returns true if and only if there is only a single byte in the const_int
;; operand that is non zero.  ie., this predicate will match 0x00004f00, but
;; not 0x00104f00.
(define_predicate "one_byte_non_zero_operand"
  (match_code "const_int")
{
  unsigned HOST_WIDE_INT x = INTVAL (op);

  while (x)
    {
      if ((x & 0xff) && !(x & ~0xff))
        return true;
      if ((x & 0xff) && (x & ~0xff))
        return false;
      x >>= 8;
    }
  return true;
})

