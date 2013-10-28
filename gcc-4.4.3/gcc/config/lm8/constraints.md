;; Constraint definitions for Lattice Mico8 architecture.
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

(define_constraint "K"
  "Integer constant in the range 0 @dots{} 31."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 31")))
