! Demonstrates compiling a pattern with multiple capture groups, iterating
! through every match in a multi-line buffer using start_offset, and
! extracting submatches safely.
program log_parser
   use iso_fortran_env,  only : int32
   use M_unicode__regex, only : utoc, ctou
   use M_unicode__regex, only : regex_t, match_t, regex_compile, regex_search
   implicit none

   character(len=1), parameter   :: BS = achar(92)
   type(regex_t)                 :: re
   type(match_t)                 :: m
   integer(int32), allocatable   :: log_cp(:)
   integer                       :: offset, count
   character(len=:), allocatable :: raw_log, pat_str

   raw_log = &
      '2026-11-12 08:14:02 [INFO]  User "alice" connected from 10.0.1.42' // new_line('a') // &
      '2026-11-12 08:14:09 [WARN]  High latency (312ms) on node Zürich-01' // new_line('a') // &
      '2026-11-12 08:14:15 [ERROR] Failed login for user "bob" from 192.168.1.99'

   ! Pattern: ^(\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2})\s+\[(\w+)\]\s+(.*)$
   pat_str = '^(' // BS // 'd{4}-' // BS // 'd{2}-' // BS // 'd{2}' // BS // 's+' // &
             BS // 'd{2}:' // BS // 'd{2}:' // BS // 'd{2})' // BS // 's+' // &
             BS // '[(' // BS // 'w+)' // BS // ']' // BS // 's+(.*)$'

   re = regex_compile(utoc(pat_str))
   if (.not. re%valid) error stop 'Failed to compile log pattern'

   log_cp = utoc(raw_log)
   offset = 1
   count  = 0

   print '(A)', 'Parsed Log Entries:'
   print '(A)', '------------------------------------------------------------'

   do while (offset <= size(log_cp))
      m = regex_search(re, log_cp, start_offset=offset)
      if (.not. m%matched) exit

      count = count + 1
      print '(A,I0)', 'Entry #', count
      print '(A)', '  Timestamp : ' // ctou(log_cp(m%start_pos(1):m%end_pos(1)))
      print '(A)', '  Level     : ' // ctou(log_cp(m%start_pos(2):m%end_pos(2)))
      print '(A)', '  Message   : ' // ctou(log_cp(m%start_pos(3):m%end_pos(3)))

      offset = m%end_pos(0) + 1
   end do
end program log_parser
