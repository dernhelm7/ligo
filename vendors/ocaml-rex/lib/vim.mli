(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(** Perl-style regular expressions *)

exception Parse_error
exception Not_supported
(** Errors that can be raised during the parsing of the regular expression *)


type opt =
  [ `Ungreedy | `Dotall | `Dollar_endonly
  | `Multiline | `Anchored | `Caseless ]

val re : ?opts:opt list -> string -> Core.t
(** Parsing of a Perl-style regular expression *)

val compile : Core.t -> Core.re
(** (Same as [Re.compile]) *)

val compile_pat : ?opts:opt list -> string -> Core.re
(** Regular expression compilation *)

val pp : Core.t -> string
