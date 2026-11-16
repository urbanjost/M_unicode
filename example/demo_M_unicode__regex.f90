     program demo_M_unicode__regex
     use iso_fortran_env, only: int32
     use M_unicode__regex, only : regex_t, match_t
     use M_unicode__regex, only : regex_compile, regex_search, regex_replace
     use M_unicode__convert, only : utoc, ctou
     implicit none
     type(regex_t)                :: re
     type(match_t)                :: m
     integer(int32), allocatable  :: txt(:), out(:)
     character(len=:),allocatable :: pat
        pat='(\w+)\s+(🚀|🛸)'
        re = regex_compile(utoc(pat))
        if (.not. re%valid) error stop 'Invalid pattern'
        txt = utoc('Mission Zürich 🚀 ready')
        m = regex_search(re, txt)
        if (m%matched) then
           print '(A)', 'City:  ' // &
           & ctou(txt(m%start_pos(1):m%end_pos(1)))
           print '(A)', 'Craft: ' // &
           & ctou(txt(m%start_pos(2):m%end_pos(2)))
        endif
        out = regex_replace(re, txt, utoc('$2 -> $1'))
        print '(A)', 'Swapped: ' // ctou(out)
     end program demo_M_unicode__regex
