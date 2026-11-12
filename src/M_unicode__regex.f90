!>
!!### NAME
!!   M_unicode__regex(3f) - [M_unicode__regex:REGULAR EXPRESSIONS] Overview
!!   of UTF-8 codepoint Regular Expression engine
!!   (LICENSE:MIT)
!!
!!### SYNOPSIS
!!
!!   Syntax
!!
!!     use M_unicode__regex, only: regex_t, match_t, &
!!                          regex_compile, regex_search, regex_replace, &
!!                          utoc, ctou
!!
!!     type(regex_t) :: re
!!     type(match_t) :: m
!!     integer(int32), allocatable :: pat(:), txt(:), repl(:), out(:)
!!     character(len=:), allocatable :: str
!!
!!     pat  = utoc(str)
!!     re   = regex_compile(pat)
!!     m    = regex_search(re, txt [, start_offset])
!!     out  = regex_replace(re, txt, repl [, replace_all])
!!     str  = ctou(out)
!!
!!### DESCRIPTION
!!
!!    The M_unicode__regex(3f)  module implements a self-contained,
!!    Unicode-aware regular expression compiler and backtracking
!!    Nondeterministic Finite Automaton (NFA) virtual machine in standard
!!    Fortran 2008.
!!
!!    All pattern compilation, text matching, and string replacement
!!    operate directly on 1-indexed arrays of UTF-32 scalar codepoints
!!    (integer(int32)).  Helper functions utoc(3) and
!!    ctou(3) convert between standard UTF-8 encoded Fortran
!!    character strings and UTF-32 codepoint arrays.
!!
!!### PUBLIC DERIVED TYPES
!!    type :: regex_t   Compiled regular expression bytecode container.
!!
!!            Components:
!!
!!                prog(:)     Allocatable array of VM instructions (private layout).
!!                num_groups  Integer count of capture groups including group 0
!!                            (1 <= num_groups <= 10).
!!                valid       Logical flag set to .true. if compilation succeeded,
!!                            or .false. if the pattern contained a syntax error
!!                            or exceeded group limits.
!!
!!    type :: match_t   Result structure returned by regex_search(3).
!!
!!            Components:
!!
!!                matched         Logical flag; .true. if a match was found.
!!                start_pos(0:9)  1-based starting codepoint index for each capture
!!                                group. Group 0 is the entire match; groups 1..9
!!                                are parenthesized subexpressions.
!!                end_pos(0:9)    1-based inclusive ending codepoint index for each
!!                                capture group. For an unmatched group or a zero-
!!                                length match, end_pos(i) < start_pos(i).
!!
!!### PUBLIC PROCEDURES
!!        regex_compile(pattern) result(re)
!!
!!            Compiles a UTF-32 regular expression pattern into NFA bytecode.
!!
!!            Arguments:
!!
!!                pattern     integer(int32), intent(in) :: pattern(:)
!!                            Array of Unicode codepoints representing the regex.
!!
!!            Return Value:
!!                re          type(regex_t)
!!                            Inspect re%valid before passing re to regex_search
!!                            or regex_replace. Invalid patterns return
!!                            re%valid = .false. without aborting.
!!
!!        regex_search(re, text [, start_offset]) result(m)
!!
!!            Searches text for the leftmost match of the compiled expression re.
!!
!!            Arguments:
!!                re            type(regex_t), intent(in)
!!                              Compiled regular expression. If .not. re%valid,
!!                              returns m%matched = .false. immediately.
!!                text          integer(int32), intent(in) :: text(:)
!!                              Subject text as an array of UTF-32 codepoints.
!!                start_offset  integer, intent(in), optional
!!                              1-based codepoint index at which to begin searching
!!                              (defaults to 1; values < 1 are clamped to 1).
!!
!!            Return Value:
!!                m             type(match_t)
!!                              If m%matched is .true., the matched slice for group
!!                              g (0 <= g < re%num_groups) is:
!!                                  text(m%start_pos(g) : m%end_pos(g))
!!                              whenever m%start_pos(g) >= 1 and
!!                              m%end_pos(g) >= m%start_pos(g).
!!
!!        regex_replace(re, text, replacement [, replace_all]) result(out_text)
!!
!!            Replaces matches of re in text with replacement, expanding capture
!!            group backreferences.
!!
!!            Arguments:
!!
!!                re           type(regex_t), intent(in)
!!                             Compiled regular expression.
!!                text         integer(int32), intent(in) :: text(:)
!!                             Subject text as an array of UTF-32 codepoints.
!!                replacement  integer(int32), intent(in) :: replacement(:)
!!                             Replacement template as UTF-32 codepoints. Supports
!!                             backreferences $0..$9 and \0..\9, where group 0 is
!!                             the full match and 1..9 are capture groups. Literal
!!                             '$' or '\' may be escaped by doubling ('$$' or '\\').
!!                replace_all  logical, intent(in), optional
!!                             If .true. (the default), replaces all non-overlapping
!!                             matches in text. If .false., replaces only the first
!!                             match.
!!
!!            Return Value:
!!
!!                out_text     integer(int32), allocatable :: out_text(:)
!!                             Newly allocated UTF-32 codepoint array containing
!!                             the resulting text.
!!
!!        utoc(str) result(cp)
!!
!!            Decodes a UTF-8 encoded Fortran character variable into an array of
!!            UTF-32 codepoints.
!!
!!            Arguments:
!!                str  character(len=*), intent(in)
!!                     Input UTF-8 byte sequence. Truncated or malformed UTF-8
!!                     sequences are replaced with U+FFFD (Replacement Character).
!!
!!            Return Value:
!!                cp   integer(int32), allocatable :: cp(:)
!!                     Array of decoded scalar codepoints (size 0 if len(str) == 0).
!!
!!        ctou(cp) result(str)
!!
!!            Encodes a UTF-32 codepoint array into a UTF-8 Fortran character string.
!!
!!            Arguments:
!!                cp   integer(int32), intent(in) :: cp(:)
!!                     Array of Unicode scalar values. Out-of-range values
!!                     (< 0 or > U+10FFFF) are encoded as U+FFFD.
!!
!!            Return Value:
!!                str  character(len=:), allocatable
!!                     UTF-8 encoded string.
!!
!!### REGULAR EXPRESSION SYNTAX
!!
!!        Atoms
!!
!!            c           Matches literal Unicode codepoint c.
!!            .           Matches any single codepoint except Line Feed (U+000A).
!!            [abc]       Character class matching any listed codepoint or range
!!                        (e.g., [a-z0-9_]). Supports \d, \w, \s, \n, \r, \t.
!!            [^abc]      Negated character class matching any codepoint not in the
!!                        listed set (including Line Feed).
!!            (expr)      Capturing group (up to 9 groups, numbered 1..9 left-to-
!!                        right by opening parenthesis).
!!            \x          Escaped character or shorthand class (see below).
!!
!!        Shorthand Classes & Escapes
!!
!!            \d, \D      ASCII digit [0-9] (\d) or non-digit (\D).
!!            \w, \W      Word character (\w): [0-9A-Za-z_] plus Latin-1 and Latin
!!                        Extended letters (U+00C0..U+00D6, U+00D8..U+00F6,
!!                        U+00F8..U+024F). \W matches any non-word codepoint.
!!            \s, \S      Whitespace (\s): ASCII space (U+0020), \t..\r (U+0009..
!!                        U+000D), non-breaking space (U+00A0), and Unicode spaces
!!                        (U+2000..U+200A). \S matches any non-whitespace codepoint.
!!            \n, \r, \t  Line Feed (U+000A), Carriage Return (U+000D), Tab (U+0009).
!!            \\, \., ... Escapes any metacharacter to match it literally.
!!
!!        Anchors & Zero-Width Assertions
!!
!!            ^           Matches start of text or immediately after Line Feed (U+000A).
!!            $           Matches end of text or immediately before Line Feed (U+000A).
!!            \b          Matches a Unicode word boundary (transition between \w and
!!                        \W, including start/end of text).
!!            \B          Matches a position that is not a word boundary.
!!
!!        Quantifiers (Greedy and Non-Greedy)
!!
!!            *           0 or more times (greedy).
!!            +           1 or more times (greedy).
!!            ?           0 or 1 time (greedy).
!!            {n}         Exactly n times (0 <= n <= 1000).
!!            {n,}        n or more times (greedy).
!!            {n,m}       Between n and m times inclusive (greedy, 0 <= n <= m <= 1000).
!!            *?, +?, ??, {n,}?, {n,m}?
!!                        Non-greedy (lazy) variants matching as few repetitions
!!                        as possible while still allowing the overall pattern to
!!                        succeed.
!!
!!        Alternation & Precedence
!!
!!            expr1|expr2 Matches expr1 or, if that fails, expr2.
!!            Precedence from highest to lowest:
!!                1. Atoms, character classes, escapes, parenthesized groups
!!                2. Quantifiers (*, +, ?, {n,m})
!!                3. Concatenation
!!                4. Alternation (|)
!!
!!### LIMITATIONS & NOTES
!!
!!    o   Maximum capture groups is 9 (MAX_GROUPS = 10 including group 0).
!!    o   Maximum repetition bound in {n,m} is 1000.
!!    o   The VM uses recursive backtracking; pathological nested quantifiers
!!        such as (a+)+b on non-matching input can exhibit exponential time
!!        and stack depth.
!!
!!### EXAMPLE
!!
!!
!! Sample program
!!
!!    program example
!!    use iso_fortran_env, only: int32
!!    use M_unicode__regex
!!    implicit none
!!    character(len=1), parameter :: BS = achar(92)
!!    type(regex_t)               :: re
!!    type(match_t)               :: m
!!    integer(int32), allocatable :: txt(:), out(:)
!!    character(len=:),allocatable :: pat
!!       pat='(\w+)\s+(🚀|🛸)'
!!       re = regex_compile(utoc(pat)
!!       if (.not. re%valid) error stop 'Invalid pattern'
!!       txt = utoc('Mission Zürich 🚀 ready')
!!       m = regex_search(re, txt)
!!       if (m%matched) then
!!          print '(A)', 'City:  ' // &
!!          & ctou(txt(m%start_pos(1):m%end_pos(1)))
!!          print '(A)', 'Craft: ' // &
!!          & ctou(txt(m%start_pos(2):m%end_pos(2)))
!!       endif
!!       out = regex_replace(re, txt, utoc('$2 -> $1'))
!!       print '(A)', 'Swapped: ' // ctou(out)
!!    end program example
!!
!!### CONTRIBUTORS
!!
!!  Developed with coding and debugging assistance from Gemini (Google).
!!
!!  Portions of this module (NFA compiler fixes, counted/lazy quantifiers,
!!  word boundaries, test suite, and documentation) were generated with
!!  assistance from Gemini (Google).
!!
!!### SEE ALSO
!!
!! iso_fortran_env(3), pcre2syntax(3), regex(7)
module M_unicode__regex
   use iso_fortran_env, only: int32
   implicit none
   private

   public :: regex_t, match_t
   public :: regex_compile, regex_search, regex_replace
   public :: utoc, ctou

   ! Instruction opcodes for the NFA Virtual Machine
   integer, parameter :: OP_CHAR      = 1
   integer, parameter :: OP_ANY       = 2
   integer, parameter :: OP_CLASS     = 3
   integer, parameter :: OP_NCLASS    = 4
   integer, parameter :: OP_SPLIT     = 5
   integer, parameter :: OP_JMP       = 6
   integer, parameter :: OP_SAVE      = 7
   integer, parameter :: OP_BOL       = 8
   integer, parameter :: OP_EOL       = 9
   integer, parameter :: OP_MATCH     = 10
   integer, parameter :: OP_WB        = 11  ! \b (word boundary)
   integer, parameter :: OP_NWB       = 12  ! \B (non-word boundary)

   integer(int32), parameter :: CP_BACKSLASH = 92_int32

   integer, parameter :: MAX_GROUPS   = 10  ! Group 0 (full match) + Groups 1..9
   integer, parameter :: MAX_SLOTS    = MAX_GROUPS * 2

   type :: char_range_t
      integer(int32) :: low
      integer(int32) :: high
   end type char_range_t

   type :: inst_t
      integer :: op = 0
      integer(int32) :: ch = 0
      integer :: x = 0
      integer :: y = 0
      type(char_range_t), allocatable :: ranges(:)
   end type inst_t

   type :: regex_t
      type(inst_t), allocatable :: prog(:)
      integer :: num_groups = 0
      logical :: valid = .false.
   end type regex_t

   type :: match_t
      logical :: matched = .false.
      integer :: start_pos(0:MAX_GROUPS-1) = 0
      integer :: end_pos(0:MAX_GROUPS-1)   = -1
   end type match_t

   ! Parser state
   type :: parser_t
      integer(int32), allocatable :: pat(:)
      integer :: pos = 1
      integer :: len = 0
      integer :: group_count = 0
      type(inst_t), allocatable :: code(:)
      integer :: code_len = 0
      logical :: err = .false.
   end type parser_t

contains

   !===========================================================================
   ! COMPILER (Recursive Descent Parser -> NFA Bytecode)
   !===========================================================================
   function regex_compile(pattern) result(re)
      integer(int32), intent(in) :: pattern(:)
      type(regex_t)              :: re
      type(parser_t)             :: p
      integer                    :: start_pc, end_pc

      p%pat = pattern
      p%len = size(pattern)
      p%pos = 1
      p%group_count = 0
      allocate(p%code(max(32, p%len * 4)))
      p%code_len = 0

      ! Save slot 0 (start of full match)
      start_pc = emit_save(p, 0)

      call parse_alt(p, start_pc, end_pc)
      if (p%err .or. p%pos <= p%len) then
         re%valid = .false.
         return
      end if

      ! Save slot 1 (end of full match) & Match instruction
      end_pc = emit_save(p, 1)
      end_pc = emit_inst(p, OP_MATCH)

      re%prog = p%code(1:p%code_len)
      re%num_groups = p%group_count + 1
      re%valid = .true.
   end function regex_compile

   subroutine ensure_capacity(p, extra)
      type(parser_t), intent(inout) :: p
      integer, intent(in), optional :: extra
      type(inst_t), allocatable     :: tmp(:)
      integer                       :: needed

      needed = p%code_len + 1
      if (present(extra)) needed = p%code_len + extra
      do while (needed > size(p%code))
         allocate(tmp(size(p%code) * 2))
         tmp(1:p%code_len) = p%code(1:p%code_len)
         call move_alloc(tmp, p%code)
      end do
   end subroutine ensure_capacity

   function emit_inst(p, op, ch, x, y) result(pc)
      type(parser_t), intent(inout)        :: p
      integer, intent(in)                  :: op
      integer(int32), intent(in), optional :: ch
      integer, intent(in), optional        :: x, y
      integer                              :: pc

      call ensure_capacity(p, 1)
      p%code_len = p%code_len + 1
      pc = p%code_len
      p%code(pc)%op = op
      p%code(pc)%ch = 0
      p%code(pc)%x  = 0
      p%code(pc)%y  = 0
      if (allocated(p%code(pc)%ranges)) deallocate(p%code(pc)%ranges)
      if (present(ch)) p%code(pc)%ch = ch
      if (present(x))  p%code(pc)%x  = x
      if (present(y))  p%code(pc)%y  = y
   end function emit_inst

   function emit_save(p, slot) result(pc)
      type(parser_t), intent(inout) :: p
      integer, intent(in)           :: slot
      integer                       :: pc
      pc = emit_inst(p, OP_SAVE, x=slot)
   end function emit_save

   ! Shifts [from_pc .. old_end] right by `delta` slots.
   ! Internal targets in [from_pc .. old_end] shift by +delta.
   ! Exit targets (== old_end + 1) are set to `new_exit`.
   subroutine shift_fragment(p, from_pc, old_end, delta, new_exit)
      type(parser_t), intent(inout) :: p
      integer, intent(in)           :: from_pc, old_end, delta, new_exit
      integer                       :: i

      call ensure_capacity(p, delta)
      do i = old_end, from_pc, -1
         p%code(i + delta) = p%code(i)
         if (p%code(i + delta)%op == OP_JMP .or. p%code(i + delta)%op == OP_SPLIT) then
            if (p%code(i + delta)%x >= from_pc .and. p%code(i + delta)%x <= old_end) then
               p%code(i + delta)%x = p%code(i + delta)%x + delta
            else if (p%code(i + delta)%x == old_end + 1) then
               p%code(i + delta)%x = new_exit
            end if

            if (p%code(i + delta)%y >= from_pc .and. p%code(i + delta)%y <= old_end) then
               p%code(i + delta)%y = p%code(i + delta)%y + delta
            else if (p%code(i + delta)%y == old_end + 1) then
               p%code(i + delta)%y = new_exit
            end if
         end if
      end do
      p%code_len = old_end + delta
   end subroutine shift_fragment

   ! Clones instructions [src_s .. src_e] to the end of p%code, adjusting internal jumps.
   subroutine clone_fragment(p, src_s, src_e, dst_s, dst_e)
      type(parser_t), intent(inout) :: p
      integer, intent(in)           :: src_s, src_e
      integer, intent(out)          :: dst_s, dst_e
      integer                       :: len_frag, delta, i, dst

      len_frag = src_e - src_s + 1
      call ensure_capacity(p, len_frag)
      dst_s = p%code_len + 1
      delta = dst_s - src_s

      do i = src_s, src_e
         dst = i + delta
         p%code(dst) = p%code(i)
         if (p%code(dst)%op == OP_JMP .or. p%code(dst)%op == OP_SPLIT) then
            if (p%code(dst)%x >= src_s .and. p%code(dst)%x <= src_e + 1) then
               p%code(dst)%x = p%code(dst)%x + delta
            end if
            if (p%code(dst)%y >= src_s .and. p%code(dst)%y <= src_e + 1) then
               p%code(dst)%y = p%code(dst)%y + delta
            end if
         end if
      end do

      p%code_len = p%code_len + len_frag
      dst_e = p%code_len
   end subroutine clone_fragment

   recursive subroutine parse_alt(p, start_pc, end_pc)
      type(parser_t), intent(inout) :: p
      integer, intent(out)          :: start_pc, end_pc
      integer                       :: s1, e1, s2, e2, old_end, jmp_pc, after_alt, i

      call parse_concat(p, s1, e1)
      if (p%err) return

      start_pc = s1
      end_pc   = e1

      do while (p%pos <= p%len)
         if (p%pat(p%pos) /= iachar('|')) exit
         p%pos = p%pos + 1

         old_end = p%code_len
         ! Redirect all exits from [s1..old_end] to sentinel (-999)
         call shift_fragment(p, s1, old_end, delta=1, new_exit=-999)

         ! Emit OP_JMP at end of branch 1 with sentinel (-999)
         jmp_pc = emit_inst(p, OP_JMP, x=-999)

         ! Parse branch 2
         call parse_concat(p, s2, e2)
         if (p%err) return

         after_alt = p%code_len + 1

         ! Wire OP_SPLIT at s1 -> branch 1 (s1 + 1) or branch 2 (s2)
         p%code(s1)%op = OP_SPLIT
         p%code(s1)%ch = 0
         p%code(s1)%x  = s1 + 1
         p%code(s1)%y  = s2
         if (allocated(p%code(s1)%ranges)) deallocate(p%code(s1)%ranges)

         ! Patch all sentinel exits in [s1+1 .. jmp_pc] to jump to after_alt
         do i = s1 + 1, jmp_pc
            if (p%code(i)%op == OP_JMP .or. p%code(i)%op == OP_SPLIT) then
               if (p%code(i)%x == -999) p%code(i)%x = after_alt
               if (p%code(i)%y == -999) p%code(i)%y = after_alt
            end if
         end do

         start_pc = s1
         end_pc   = e2
      end do
   end subroutine parse_alt

   recursive subroutine parse_concat(p, start_pc, end_pc)
      type(parser_t), intent(inout) :: p
      integer, intent(out)          :: start_pc, end_pc
      integer                       :: s, e, count

      start_pc = p%code_len + 1
      end_pc   = start_pc
      count    = 0

      do while (p%pos <= p%len)
         if (p%pat(p%pos) == iachar('|') .or. p%pat(p%pos) == iachar(')')) exit
         call parse_quant(p, s, e)
         if (p%err) return
         if (count == 0) start_pc = s
         end_pc = e
         count = count + 1
      end do

      if (count == 0) then
         ! Empty branch (e.g. `a|` or `(|a)`) -> no-op jump to next instruction
         start_pc = emit_inst(p, OP_JMP, x=0)
         p%code(start_pc)%x = start_pc + 1
         end_pc   = start_pc
      end if
   end subroutine parse_concat

   ! Parses `{n}`, `{n,}`, or `{n,m}` starting at p%pat(p%pos) == '{'.
   ! Sets valid_brace=.false. without error if `{` does not start a valid quantifier
   ! (so literal `{foo}` works), or sets p%err=.true. on malformed bounds like `{5,2}`.
   subroutine parse_brace_bounds(p, min_rep, max_rep, valid_brace)
      type(parser_t), intent(inout) :: p
      integer, intent(out)          :: min_rep, max_rep
      logical, intent(out)          :: valid_brace
      integer                       :: idx, val1, val2
      integer(int32)                :: ch

      valid_brace = .false.
      min_rep = 0
      max_rep = 0
      idx = p%pos + 1
      if (idx > p%len) return

      ch = p%pat(idx)
      if (ch < iachar('0') .or. ch > iachar('9')) return

      val1 = 0
      do while (idx <= p%len)
         ch = p%pat(idx)
         if (ch < iachar('0') .or. ch > iachar('9')) exit
         val1 = val1 * 10 + int(ch - iachar('0'))
         if (val1 > 1000) then
            p%err = .true.
            return
         end if
         idx = idx + 1
      end do
      if (idx > p%len) return
      if (p%pat(idx) == iachar('}')) then
         min_rep = val1
         max_rep = val1
         p%pos = idx + 1
         valid_brace = .true.
         return
      end if
      if (p%pat(idx) /= iachar(',')) return
      idx = idx + 1
      if (idx > p%len) return
      if (p%pat(idx) == iachar('}')) then
         min_rep = val1
         max_rep = -1
         p%pos = idx + 1
         valid_brace = .true.
         return
      end if
      ch = p%pat(idx)
      if (ch < iachar('0') .or. ch > iachar('9')) return
      val2 = 0
      do while (idx <= p%len)
         ch = p%pat(idx)
         if (ch < iachar('0') .or. ch > iachar('9')) exit
         val2 = val2 * 10 + int(ch - iachar('0'))
         if (val2 > 1000) then
            p%err = .true.
            return
         end if
         idx = idx + 1
      end do
      if (idx > p%len .or. p%pat(idx) /= iachar('}')) return
      if (val2 < val1) then
         p%err = .true.
         return
      end if
      min_rep = val1
      max_rep = val2
      p%pos = idx + 1
      valid_brace = .true.
   end subroutine parse_brace_bounds
   recursive subroutine parse_quant(p, start_pc, end_pc)
      type(parser_t), intent(inout) :: p
      integer, intent(out)          :: start_pc, end_pc
      integer                       :: atom_s, atom_e, old_end, jmp_pc, split_pc
      integer                       :: min_rep, max_rep, k, copy_s, copy_e, last_s, last_e
      logical                       :: has_quant, valid_brace, lazy
      integer(int32)                :: q
      call parse_atom(p, atom_s, atom_e)
      if (p%err) return
      start_pc = atom_s
      end_pc = atom_e
      has_quant = .false.
      lazy = .false.
      if (p%pos <= p%len) then
         q = p%pat(p%pos)
         if (q == iachar('*')) then
            p%pos = p%pos + 1
            has_quant = .true.
            if (p%pos <= p%len .and. p%pat(p%pos) == iachar('?')) then
               lazy = .true.
               p%pos = p%pos + 1
            end if
            old_end = p%code_len
            call shift_fragment(p, atom_s, old_end, delta=1, new_exit=old_end + 2)
            jmp_pc = emit_inst(p, OP_JMP, x=atom_s)
            p%code(atom_s)%op = OP_SPLIT
            p%code(atom_s)%ch = 0
            if (lazy) then
               p%code(atom_s)%x = jmp_pc + 1
               p%code(atom_s)%y = atom_s + 1
            else
               p%code(atom_s)%x = atom_s + 1
               p%code(atom_s)%y = jmp_pc + 1
            end if
            if (allocated(p%code(atom_s)%ranges)) deallocate(p%code(atom_s)%ranges)
            start_pc = atom_s
            end_pc = jmp_pc
         else if (q == iachar('+')) then
            p%pos = p%pos + 1
            has_quant = .true.
            if (p%pos <= p%len .and. p%pat(p%pos) == iachar('?')) then
               lazy = .true.
               p%pos = p%pos + 1
            end if
            split_pc = emit_inst(p, OP_SPLIT, x=0, y=0)
            if (lazy) then
               p%code(split_pc)%x = split_pc + 1
               p%code(split_pc)%y = atom_s
            else
               p%code(split_pc)%x = atom_s
               p%code(split_pc)%y = split_pc + 1
            end if
            start_pc = atom_s
            end_pc = split_pc
         else if (q == iachar('?')) then
            p%pos = p%pos + 1
            has_quant = .true.
            if (p%pos <= p%len .and. p%pat(p%pos) == iachar('?')) then
               lazy = .true.
               p%pos = p%pos + 1
            end if
            old_end = p%code_len
            call shift_fragment(p, atom_s, old_end, delta=1, new_exit=old_end + 2)
            p%code(atom_s)%op = OP_SPLIT
            p%code(atom_s)%ch = 0
            if (lazy) then
               p%code(atom_s)%x = p%code_len + 1
               p%code(atom_s)%y = atom_s + 1
            else
               p%code(atom_s)%x = atom_s + 1
               p%code(atom_s)%y = p%code_len + 1
            end if
            if (allocated(p%code(atom_s)%ranges)) deallocate(p%code(atom_s)%ranges)
            start_pc = atom_s
            end_pc = p%code_len
         else if (q == iachar('{')) then
            call parse_brace_bounds(p, min_rep, max_rep, valid_brace)
            if (p%err) return
            if (valid_brace) then
               has_quant = .true.
               if (p%pos <= p%len .and. p%pat(p%pos) == iachar('?')) then
                  lazy = .true.
                  p%pos = p%pos + 1
               end if
               if (max_rep == 0) then
                  p%code(atom_s)%op = OP_JMP
                  p%code(atom_s)%ch = 0
                  p%code(atom_s)%x = p%code_len + 1
                  p%code(atom_s)%y = 0
                  if (allocated(p%code(atom_s)%ranges)) deallocate(p%code(atom_s)%ranges)
                  start_pc = atom_s
                  end_pc = p%code_len
               else if (max_rep == -1) then
                  if (min_rep == 0) then
                     old_end = p%code_len
                     call shift_fragment(p, atom_s, old_end, delta=1, new_exit=old_end + 2)
                     jmp_pc = emit_inst(p, OP_JMP, x=atom_s)
                     p%code(atom_s)%op = OP_SPLIT
                     p%code(atom_s)%ch = 0
                     if (lazy) then
                        p%code(atom_s)%x = jmp_pc + 1
                        p%code(atom_s)%y = atom_s + 1
                     else
                        p%code(atom_s)%x = atom_s + 1
                        p%code(atom_s)%y = jmp_pc + 1
                     end if
                     if (allocated(p%code(atom_s)%ranges)) deallocate(p%code(atom_s)%ranges)
                     start_pc = atom_s
                     end_pc = jmp_pc
                  else
                     last_s = atom_s
                     last_e = p%code_len
                     do k = 2, min_rep
                        call clone_fragment(p, atom_s, atom_e, last_s, last_e)
                     end do
                     split_pc = emit_inst(p, OP_SPLIT, x=0, y=0)
                     if (lazy) then
                        p%code(split_pc)%x = split_pc + 1
                        p%code(split_pc)%y = last_s
                     else
                        p%code(split_pc)%x = last_s
                        p%code(split_pc)%y = split_pc + 1
                     end if
                     start_pc = atom_s
                     end_pc = split_pc
                  end if
               else
                  if (min_rep == 0) then
                     old_end = p%code_len
                     call shift_fragment(p, atom_s, old_end, delta=1, new_exit=old_end + 2)
                     p%code(atom_s)%op = OP_SPLIT
                     p%code(atom_s)%ch = 0
                     if (lazy) then
                        p%code(atom_s)%x = p%code_len + 1
                        p%code(atom_s)%y = atom_s + 1
                     else
                        p%code(atom_s)%x = atom_s + 1
                        p%code(atom_s)%y = p%code_len + 1
                     end if
                     if (allocated(p%code(atom_s)%ranges)) deallocate(p%code(atom_s)%ranges)
                     copy_s = atom_s + 1
                     copy_e = p%code_len
                     do k = 2, max_rep
                        call clone_fragment(p, copy_s, copy_e, last_s, last_e)
                        old_end = p%code_len
                        call shift_fragment(p, last_s, old_end, delta=1, new_exit=old_end + 2)
                        p%code(last_s)%op = OP_SPLIT
                        p%code(last_s)%ch = 0
                        if (lazy) then
                           p%code(last_s)%x = p%code_len + 1
                           p%code(last_s)%y = last_s + 1
                        else
                           p%code(last_s)%x = last_s + 1
                           p%code(last_s)%y = p%code_len + 1
                        end if
                        if (allocated(p%code(last_s)%ranges)) deallocate(p%code(last_s)%ranges)
                     end do
                     start_pc = atom_s
                     end_pc = p%code_len
                  else
                     do k = 2, min_rep
                        call clone_fragment(p, atom_s, atom_e, last_s, last_e)
                     end do
                     do k = min_rep + 1, max_rep
                        call clone_fragment(p, atom_s, atom_e, last_s, last_e)
                        old_end = p%code_len
                        call shift_fragment(p, last_s, old_end, delta=1, new_exit=old_end + 2)
                        p%code(last_s)%op = OP_SPLIT
                        p%code(last_s)%ch = 0
                        if (lazy) then
                           p%code(last_s)%x = p%code_len + 1
                           p%code(last_s)%y = last_s + 1
                        else
                           p%code(last_s)%x = last_s + 1
                           p%code(last_s)%y = p%code_len + 1
                        end if
                        if (allocated(p%code(last_s)%ranges)) deallocate(p%code(last_s)%ranges)
                     end do
                     start_pc = atom_s
                     end_pc = p%code_len
                  end if
               end if
            end if
         end if
! Reject chained quantifiers after optional ? modifier
         if (has_quant .and. p%pos <= p%len) then
            q = p%pat(p%pos)
            if (q == iachar('*') .or. q == iachar('+') .or. q == iachar('?')) then
               p%err = .true.
               return
            else if (q == iachar('{')) then
               call parse_brace_bounds(p, min_rep, max_rep, valid_brace)
               if (p%err .or. valid_brace) then
                  p%err = .true.
                  return
               end if
            end if
         end if
      end if
   end subroutine parse_quant
   recursive subroutine parse_atom(p, start_pc, end_pc)
      type(parser_t), intent(inout) :: p
      integer, intent(out)          :: start_pc, end_pc
      integer(int32)                :: c
      integer                       :: grp_idx, s1, s2, inner_s, inner_e
      if (p%pos > p%len) then
         p%err = .true.
         return
      end if
      c = p%pat(p%pos)
      if (c == iachar('*') .or. c == iachar('+') .or. c == iachar('?')) then
         p%err = .true.
         return
      end if
      p%pos = p%pos + 1
      select case (c)
       case (iachar('.'))
         start_pc = emit_inst(p, OP_ANY)
         end_pc = start_pc
       case (iachar('^'))
         start_pc = emit_inst(p, OP_BOL)
         end_pc = start_pc
       case (iachar('$'))
         start_pc = emit_inst(p, OP_EOL)
         end_pc = start_pc
       case (iachar('('))
         if (p%group_count + 1 >= MAX_GROUPS) then
            p%err = .true.
            return
         end if
         p%group_count = p%group_count + 1
         grp_idx = p%group_count
         s1 = emit_save(p, grp_idx * 2)
         call parse_alt(p, inner_s, inner_e)
         if (p%err) return
         if (p%pos > p%len .or. p%pat(p%pos) /= iachar(')')) then
            p%err = .true.
            return
         end if
         p%pos = p%pos + 1
         s2 = emit_save(p, grp_idx * 2 + 1)
         start_pc = s1
         end_pc = s2
       case (iachar('['))
         call parse_char_class(p, start_pc, end_pc)
       case (CP_BACKSLASH)
         if (p%pos > p%len) then
            p%err = .true.
            return
         end if
         c = p%pat(p%pos)
         p%pos = p%pos + 1
         call emit_escape(p, c, start_pc, end_pc)
       case default
         start_pc = emit_inst(p, OP_CHAR, ch=c)
         end_pc = start_pc
      end select
   end subroutine parse_atom
   subroutine append_shorthand_ranges(c, ranges, count, ok)
      integer(int32), intent(in)        :: c
      type(char_range_t), intent(inout) :: ranges(:)
      integer, intent(inout)            :: count
      logical, intent(out)              :: ok
      ok = .true.
      select case (c)
       case (iachar('d'))
         if (count + 1 > size(ranges)) then; ok = .false.; return; end if
         ranges(count + 1) = char_range_t(iachar('0'), iachar('9'))
         count = count + 1
       case (iachar('w'))
         if (count + 7 > size(ranges)) then; ok = .false.; return; end if
         ranges(count + 1) = char_range_t(iachar('0'), iachar('9'))
         ranges(count + 2) = char_range_t(iachar('A'), iachar('Z'))
         ranges(count + 3) = char_range_t(iachar('a'), iachar('z'))
         ranges(count + 4) = char_range_t(CP_BACKSLASH, CP_BACKSLASH)
         ranges(count + 5) = char_range_t(int(z'00C0'), int(z'00D6'))
         ranges(count + 6) = char_range_t(int(z'00D8'), int(z'00F6'))
         ranges(count + 7) = char_range_t(int(z'00F8'), int(z'024F'))
         count = count + 7
       case (iachar('s'))
         if (count + 4 > size(ranges)) then; ok = .false.; return; end if
         ranges(count + 1) = char_range_t(9, 13)
         ranges(count + 2) = char_range_t(32, 32)
         ranges(count + 3) = char_range_t(int(z'00A0'), int(z'00A0'))
         ranges(count + 4) = char_range_t(int(z'2000'), int(z'200A'))
         count = count + 4
       case default
         ok = .false.
      end select
   end subroutine append_shorthand_ranges
   subroutine emit_escape(p, c, start_pc, end_pc)
      type(parser_t), intent(inout) :: p
      integer(int32), intent(in)    :: c
      integer, intent(out)          :: start_pc, end_pc
      type(char_range_t)            :: tmp(16)
      integer                       :: cnt
      logical                       :: ok
      integer(int32)                :: lower_c
      select case (c)
       case (iachar('d'), iachar('D'), iachar('w'), iachar('W'), iachar('s'), iachar('S'))
         lower_c = ior(c, 32_int32)
         cnt = 0
         call append_shorthand_ranges(lower_c, tmp, cnt, ok)
         start_pc = emit_inst(p, merge(OP_CLASS, OP_NCLASS, c == lower_c))
         p%code(start_pc)%ranges = tmp(1:cnt)
       case (iachar('b'))
         start_pc = emit_inst(p, OP_WB)
       case (iachar('B'))
         start_pc = emit_inst(p, OP_NWB)
       case (iachar('n'))
         start_pc = emit_inst(p, OP_CHAR, ch=10_int32)
       case (iachar('r'))
         start_pc = emit_inst(p, OP_CHAR, ch=13_int32)
       case (iachar('t'))
         start_pc = emit_inst(p, OP_CHAR, ch=9_int32)
       case default
         start_pc = emit_inst(p, OP_CHAR, ch=c)
      end select
      end_pc = start_pc
   end subroutine emit_escape
   subroutine parse_char_class(p, start_pc, end_pc)
      type(parser_t), intent(inout) :: p
      integer, intent(out)          :: start_pc, end_pc
      logical                       :: negated, ok
      type(char_range_t)            :: temp_ranges(256)
      integer                       :: count
      integer(int32)                :: c1, c2, esc
      negated = .false.
      if (p%pos <= p%len .and. p%pat(p%pos) == iachar('^')) then
         negated = .true.
         p%pos = p%pos + 1
      end if
      count = 0
      do while (p%pos <= p%len)
         if (p%pat(p%pos) == iachar(']') .and. count > 0) then
            p%pos = p%pos + 1
            start_pc = emit_inst(p, merge(OP_NCLASS, OP_CLASS, negated))
            p%code(start_pc)%ranges = temp_ranges(1:count)
            end_pc = start_pc
            return
         end if
         if (p%pat(p%pos) == CP_BACKSLASH .and. p%pos + 1 <= p%len) then
            esc = p%pat(p%pos + 1)
            if (esc == iachar('d') .or. esc == iachar('w') .or. esc == iachar('s')) then
               p%pos = p%pos + 2
               call append_shorthand_ranges(esc, temp_ranges, count, ok)
               if (.not. ok) then
                  p%err = .true.
                  return
               end if
               cycle
            end if
         end if
         c1 = read_class_char(p)
         if (p%err) return
         if (p%pos + 1 <= p%len .and. p%pat(p%pos) == iachar('-') .and. &
            p%pat(p%pos + 1) /= iachar(']')) then
            p%pos = p%pos + 1
            c2 = read_class_char(p)
            if (p%err .or. c2 < c1) then
               p%err = .true.
               return
            end if
         else
            c2 = c1
         end if
         count = count + 1
         if (count > size(temp_ranges)) then
            p%err = .true.
            return
         end if
         temp_ranges(count) = char_range_t(c1, c2)
      end do
      p%err = .true. ! Unclosed '['
   end subroutine parse_char_class
   function read_class_char(p) result(c)
      type(parser_t), intent(inout) :: p
      integer(int32)                :: c
      if (p%pos > p%len) then
         p%err = .true.
         c = 0
         return
      end if
      c = p%pat(p%pos)
      p%pos = p%pos + 1
      if (c == CP_BACKSLASH) then
         if (p%pos > p%len) then
            p%err = .true.
            return
         end if
         c = p%pat(p%pos)
         p%pos = p%pos + 1
         select case (c)
          case (iachar('n')); c = 10_int32
          case (iachar('r')); c = 13_int32
          case (iachar('t')); c = 9_int32
         end select
      end if
   end function read_class_char
!===========================================================================
! EXECUTION ENGINE (Backtracking NFA VM with Submatch Captures)
!===========================================================================
   function regex_search(re, text, start_offset) result(m)
      type(regex_t), intent(in)     :: re
      integer(int32), intent(in)    :: text(:)
      integer, intent(in), optional :: start_offset
      type(match_t)                 :: m
      integer                       :: pos, first_pos, n, i
      integer                       :: saved(0:MAX_SLOTS-1)
      m%matched = .false.
      if (.not. re%valid) return
      n = size(text)
      first_pos = 1
      if (present(start_offset)) first_pos = max(1, start_offset)
      do pos = first_pos, n + 1
         saved = -1
         if (vm_exec(re%prog, text, n, 1, pos, saved)) then
            m%matched = .true.
            do i = 0, re%num_groups - 1
               m%start_pos(i) = saved(i * 2)
               m%end_pos(i) = saved(i * 2 + 1) - 1
            end do
            return
         end if
      end do
   end function regex_search
   recursive function vm_exec(prog, text, n, pc, sp, saved) result(ok)
      type(inst_t), intent(in)   :: prog(:)
      integer(int32), intent(in) :: text(:)
      integer, intent(in)        :: n, pc, sp
      integer, intent(inout)     :: saved(0:MAX_SLOTS-1)
      logical                    :: ok, prev_word, curr_word
      integer                    :: old_val, slot
      integer                    :: branch_saved(0:MAX_SLOTS-1)
      if (pc < 1 .or. pc > size(prog)) then
         ok = .false.
         return
      end if
      select case (prog(pc)%op)
       case (OP_MATCH)
         ok = .true.
       case (OP_CHAR)
         if (sp <= n .and. text(sp) == prog(pc)%ch) then
            ok = vm_exec(prog, text, n, pc + 1, sp + 1, saved)
         else
            ok = .false.
         end if
       case (OP_ANY)
         if (sp <= n .and. text(sp) /= 10_int32) then
            ok = vm_exec(prog, text, n, pc + 1, sp + 1, saved)
         else
            ok = .false.
         end if
       case (OP_CLASS)
         if (sp <= n .and. in_ranges(text(sp), prog(pc)%ranges)) then
            ok = vm_exec(prog, text, n, pc + 1, sp + 1, saved)
         else
            ok = .false.
         end if
       case (OP_NCLASS)
         if (sp <= n .and. .not. in_ranges(text(sp), prog(pc)%ranges)) then
            ok = vm_exec(prog, text, n, pc + 1, sp + 1, saved)
         else
            ok = .false.
         end if
       case (OP_BOL)
         if (sp == 1 .or. (sp > 1 .and. sp <= n + 1 .and. text(sp - 1) == 10_int32)) then
            ok = vm_exec(prog, text, n, pc + 1, sp, saved)
         else
            ok = .false.
         end if
       case (OP_EOL)
         if (sp == n + 1 .or. (sp <= n .and. text(sp) == 10_int32)) then
            ok = vm_exec(prog, text, n, pc + 1, sp, saved)
         else
            ok = .false.
         end if
       case (OP_WB, OP_NWB)
         prev_word = .false.
         if (sp > 1 .and. sp <= n + 1) prev_word = is_word_char(text(sp - 1))
         curr_word = .false.
         if (sp >= 1 .and. sp <= n) curr_word = is_word_char(text(sp))
         if ((prog(pc)%op == OP_WB .and. (prev_word .neqv. curr_word)) .or. &
            (prog(pc)%op == OP_NWB .and. (prev_word .eqv. curr_word))) then
            ok = vm_exec(prog, text, n, pc + 1, sp, saved)
         else
            ok = .false.
         end if
       case (OP_JMP)
         ok = vm_exec(prog, text, n, prog(pc)%x, sp, saved)
       case (OP_SPLIT)
         branch_saved = saved
         if (vm_exec(prog, text, n, prog(pc)%x, sp, branch_saved)) then
            saved = branch_saved
            ok = .true.
         else
            ok = vm_exec(prog, text, n, prog(pc)%y, sp, saved)
         end if
       case (OP_SAVE)
         slot = prog(pc)%x
         old_val = saved(slot)
         saved(slot) = sp
         if (vm_exec(prog, text, n, pc + 1, sp, saved)) then
            ok = .true.
         else
            saved(slot) = old_val
            ok = .false.
         end if
       case default
         ok = .false.
      end select
   end function vm_exec
   pure function is_word_char(c) result(res)
      integer(int32), intent(in) :: c
      logical                    :: res
      res = (c >= iachar('0') .and. c <= iachar('9')) .or. &
         (c >= iachar('A') .and. c <= iachar('Z')) .or. &
         (c >= iachar('a') .and. c <= iachar('z')) .or. &
         (c == iachar('_')) .or. &
         (c >= int(z'00C0') .and. c <= int(z'00D6')) .or. &
         (c >= int(z'00D8') .and. c <= int(z'00F6')) .or. &
         (c >= int(z'00F8') .and. c <= int(z'024F'))
   end function is_word_char
   pure function in_ranges(c, ranges) result(found)
      integer(int32), intent(in)     :: c
      type(char_range_t), intent(in) :: ranges(:)
      logical                        :: found
      integer                        :: i
      found = .false.
      do i = 1, size(ranges)
         if (c >= ranges(i)%low .and. c <= ranges(i)%high) then
            found = .true.
            return
         end if
      end do
   end function in_ranges
!===========================================================================
! STRING REPLACEMENT WITH BACKREFERENCES (\0..\9 or $0..$9)
!===========================================================================
   function regex_replace(re, text, replacement, replace_all) result(out_text)
      type(regex_t), intent(in)     :: re
      integer(int32), intent(in)    :: text(:)
      integer(int32), intent(in)    :: replacement(:)
      logical, intent(in), optional :: replace_all
      integer(int32), allocatable   :: out_text(:)
      type(match_t)                 :: m
      integer                       :: pos, n, out_len, cap, last_nonempty_end
      logical                       :: do_all
      do_all = .true.
      if (present(replace_all)) do_all = replace_all
      n = size(text)
      cap = max(32, n * 2)
      allocate(out_text(cap))
      out_len = 0
      pos = 1
      last_nonempty_end = -1
      do while (pos <= n + 1)
         m = regex_search(re, text, start_offset=pos)
         if (.not. m%matched) exit
! Skip empty match immediately adjacent to the end of a prior non-empty match
         if (m%end_pos(0) < m%start_pos(0) .and. m%start_pos(0) == last_nonempty_end + 1) then
            if (pos <= n) call append_slice(out_text, out_len, text(pos:pos))
            pos = pos + 1
            cycle
         end if
         if (m%start_pos(0) > pos) then
            call append_slice(out_text, out_len, text(pos : m%start_pos(0) - 1))
         end if
         call append_expanded(out_text, out_len, text, replacement, m)
         if (m%end_pos(0) >= m%start_pos(0)) then
            last_nonempty_end = m%end_pos(0)
            pos = m%end_pos(0) + 1
         else
            if (pos <= n) call append_slice(out_text, out_len, text(pos:pos))
            pos = pos + 1
         end if
         if (.not. do_all) exit
      end do
      if (pos <= n) then
         call append_slice(out_text, out_len, text(pos:n))
      end if
      out_text = out_text(1:out_len)
   end function regex_replace
   subroutine append_expanded(buf, buf_len, text, repl, m)
      integer(int32), allocatable, intent(inout) :: buf(:)
      integer, intent(inout)                     :: buf_len
      integer(int32), intent(in)                 :: text(:), repl(:)
      type(match_t), intent(in)                  :: m
      integer                                    :: i, rlen, grp
      integer(int32)                             :: c, nxt
      rlen = size(repl)
      i = 1
      do while (i <= rlen)
         c = repl(i)
         if ((c == CP_BACKSLASH .or. c == iachar('$')) .and. i < rlen) then
            nxt = repl(i + 1)
            if (nxt >= iachar('0') .and. nxt <= iachar('9')) then
               grp = int(nxt - iachar('0'))
               if (m%start_pos(grp) >= 1 .and. m%end_pos(grp) >= m%start_pos(grp)) then
                  call append_slice(buf, buf_len, text(m%start_pos(grp) : m%end_pos(grp)))
               end if
               i = i + 2
               cycle
            else if (nxt == c) then
               call append_slice(buf, buf_len, [c])
               i = i + 2
               cycle
            end if
         end if
         call append_slice(buf, buf_len, [c])
         i = i + 1
      end do
   end subroutine append_expanded
   subroutine append_slice(buf, buf_len, slice)
      integer(int32), allocatable, intent(inout) :: buf(:)
      integer, intent(inout)                     :: buf_len
      integer(int32), intent(in)                 :: slice(:)
      integer(int32), allocatable                :: tmp(:)
      integer                                    :: slen
      slen = size(slice)
      if (slen == 0) return
      do while (buf_len + slen > size(buf))
         allocate(tmp(size(buf) * 2))
         tmp(1:buf_len) = buf(1:buf_len)
         call move_alloc(tmp, buf)
      end do
      buf(buf_len + 1 : buf_len + slen) = slice
      buf_len = buf_len + slen
   end subroutine append_slice
!===========================================================================
! UTF-8 <-> UTF-32 CODEPOINT ARRAY HELPERS
!===========================================================================
   function utoc(str) result(cp)
      character(len=*), intent(in) :: str
      integer(int32), allocatable  :: cp(:)
      integer                      :: i, n, count, b1, b2, b3, b4
      n = len(str)
      allocate(cp(n))
      count = 0
      i = 1
      do while (i <= n)
         b1 = iachar(str(i:i))
         count = count + 1
         if (b1 < 128) then
            cp(count) = int(b1, int32)
            i = i + 1
         else if (iand(b1, 224) == 192 .and. i + 1 <= n) then
            b2 = iachar(str(i+1:i+1))
            cp(count) = int(ior(ishft(iand(b1, 31), 6), iand(b2, 63)), int32)
            i = i + 2
         else if (iand(b1, 240) == 224 .and. i + 2 <= n) then
            b2 = iachar(str(i+1:i+1))
            b3 = iachar(str(i+2:i+2))
            cp(count) = int(ior(ior(ishft(iand(b1, 15), 12), ishft(iand(b2, 63), 6)), iand(b3, 63)), int32)
            i = i + 3
         else if (iand(b1, 248) == 240 .and. i + 3 <= n) then
            b2 = iachar(str(i+1:i+1))
            b3 = iachar(str(i+2:i+2))
            b4 = iachar(str(i+3:i+3))
            cp(count) = int(ior(ior(ior(ishft(iand(b1, 7), 18), ishft(iand(b2, 63), 12)), &
               ishft(iand(b3, 63), 6)), iand(b4, 63)), int32)
            i = i + 4
         else
            cp(count) = int(z'FFFD', int32)
            i = i + 1
         end if
      end do
      cp = cp(1:count)
   end function utoc
   function ctou(cp) result(str)
      integer(int32), intent(in)                   :: cp(:)
      character(len=:), allocatable                :: str
      integer                                      :: i, pos, c
      allocate(character(len=max(1, size(cp) * 4)) :: str)
      pos = 0
      do i = 1, size(cp)
         c = int(cp(i))
         if (c < 0 .or. c > int(z'10FFFF')) c = int(z'FFFD')
         if (c < 128) then
            pos = pos + 1
            str(pos:pos) = achar(c)
         else if (c < 2048) then
            str(pos+1:pos+1) = achar(ior(192, ishft(c, -6)))
            str(pos+2:pos+2) = achar(ior(128, iand(c, 63)))
            pos = pos + 2
         else if (c < 65536) then
            str(pos+1:pos+1) = achar(ior(224, ishft(c, -12)))
            str(pos+2:pos+2) = achar(ior(128, iand(ishft(c, -6), 63)))
            str(pos+3:pos+3) = achar(ior(128, iand(c, 63)))
            pos = pos + 3
         else
            str(pos+1:pos+1) = achar(ior(240, ishft(c, -18)))
            str(pos+2:pos+2) = achar(ior(128, iand(ishft(c, -12), 63)))
            str(pos+3:pos+3) = achar(ior(128, iand(ishft(c, -6), 63)))
            str(pos+4:pos+4) = achar(ior(128, iand(c, 63)))
            pos = pos + 4
         end if
      end do
      str = str(1:pos)
   end function ctou
end module M_unicode__regex
