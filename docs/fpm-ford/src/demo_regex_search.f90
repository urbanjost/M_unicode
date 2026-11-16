     program demo_regex_search
     use iso_fortran_env,  only : int32
     use M_unicode, only : utoc, ctou
     use M_unicode, only : regex_t, match_t
     use M_unicode, only : regex_compile, regex_search
     implicit none

     character(len=1),parameter   :: BS = achar(92)
     type(regex_t)                :: re
     type(match_t)                :: m
     integer(int32),allocatable   :: log_cp(:)
     integer                      :: offset, count, i
     character(len=:),allocatable :: raw_log, pat_str
     character(len=*),parameter   :: nl=new_line('a')

     raw_log = &
     '2026-11-12 08:14:02 [INFO] &
     & User "alice" connected from 10.0.1.42' &
     // nl // &
     '2026-11-12 08:14:09 [WARN] &
     & High latency (312ms) on node Zürich-01' &
     // nl // &
     ' 2026-11-12 08:14:15 [ERROR] &
     &Failed login for user "bob" from 192.168.1.99'

        ! Pattern:
        !^(\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2})\s+\[(\w+)\]\s+(.*)$
        ! some compilers treat backslash as an escape character
        ! so replace % with backslash
        pat_str = &
        & '^(%d{4}-%d{2}-%d{2}%s+%d{2}:%d{2}:%d{2})%s+%[(%w+)%]%s+(.*)$'
        do i=1,len(pat_str)
           select case(pat_str(i:i))
           case('%');pat_str(i:i)=BS
           end select
        enddo

        re = regex_compile(utoc(pat_str))
        if (.not. re%valid) error stop 'Failed to compile log pattern'

        log_cp = utoc(raw_log)
        offset = 1
        count  = 0

        print '(A)', 'Parsed Log Entries:'
        print '(A)', &
        &'------------------------------------------------------------'

        do while (offset <= size(log_cp))
           m = regex_search(re, log_cp, start_offset=offset)
           if (.not. m%matched) exit

           count = count + 1
           print '(A,I0)', 'Entry #', count
           print '(A)', '  Timestamp : ' // &
           ctou(log_cp(m%start_pos(1):m%end_pos(1)))
           print '(A)', '  Level     : ' // &
           ctou(log_cp(m%start_pos(2):m%end_pos(2)))
           print '(A)', '  Message   : ' // &
           ctou(log_cp(m%start_pos(3):m%end_pos(3)))

           offset = m%end_pos(0) + 1
        end do
     end program demo_regex_search
