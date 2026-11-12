! example/unicode_redact.f90 (Word Boundaries, Alternation & Unicode Codepoint Indexing)

! Demonstrates Unicode-aware word boundaries (\b) and alternation (|) to
! redact sensitive terms while preserving compound words and multi-byte
! UTF-8 characters.

program unicode_redact
   use iso_fortran_env,  only : int32
   use M_unicode__regex, only : utoc, ctou
   use M_unicode__regex, only : regex_t, regex_compile, regex_replace
   implicit none

   character(len=1), parameter   :: BS = achar(92)
   type(regex_t)                 :: re
   integer(int32), allocatable   :: txt_cp(:), clean_cp(:)
   character(len=:), allocatable :: note, pat_str

   note = 'Project Phoenix (codename: Phoenix_2) meets in Zürich on Friday. ' // &
          'Do not share Phoenix credentials outside the Zürich office.'

   ! Match standalone words "Phoenix" or "Zürich", but NOT "Phoenix_2"
   pat_str = BS // 'b(Phoenix|Zürich)' // BS // 'b'

   re = regex_compile(utoc(pat_str))
   if (.not. re%valid) error stop 'Failed to compile redaction pattern'

   txt_cp   = utoc(note)
   clean_cp = regex_replace(re, txt_cp, utoc('[REDACTED]'), replace_all=.true.)

   print '(A)', 'Original:'
   print '(A)', '  ' // note
   print '(A)', 'Redacted:'
   print '(A)', '  ' // ctou(clean_cp)
end program unicode_redact
