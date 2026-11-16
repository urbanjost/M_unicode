     program demo_regex_replace
        use iso_fortran_env, only: int32
        use M_unicode, only : regex_t, match_t
        use M_unicode, only : regex_compile,regex_search,regex_replace
        use M_unicode, only : utoc,ctou
        implicit none

        character(len=:), allocatable :: pat
        character(len=:), allocatable :: new
        type(regex_t)                 :: re
        type(match_t)                 :: m
        integer(int32), allocatable   :: txt(:), out(:)
        character(len=*),parameter    :: g='(*(g0))'

        txt = utoc('Launch Zürich 🚀 now')
        print g, 'Original   : ', ctou(txt)

        pat='(\w+)\s+(🚀|🛸)'
        print g, 'Pattern    : ', pat
        re = regex_compile(utoc(pat))
        if (.not. re%valid) error stop 'Invalid regular expression'

        m = regex_search(re, txt)

        if (m%matched) then
           print g, 'Full match : ', ctou(txt(m%start_pos(0):m%end_pos(0)))
           print g, 'Group 1    : ', ctou(txt(m%start_pos(1):m%end_pos(1)))
           print g, 'Group 2    : ', ctou(txt(m%start_pos(2):m%end_pos(2)))
        else
           print g, 'NO MATCH!'
        endif

        ! Replace using backreferences
        new='$2 -> $1'
        print g, 'Old        : ',ctou(txt)
        print g, 'New        : ',new
        out = regex_replace(re, txt, utoc(new))
        print g, 'Output     : ', ctou(out)

        ! Expected Output for Unicode-Aware Engines:
        print g, 'Expected   : ', 'Launch 🚀 -> Zürich now'

        ! How it works:
        ! \w+ matches the full word Zürich (including the ü) as Group 1,
        ! and 🚀 is captured as Group 2.
        ! The replacement swaps them around the arrow while leaving
        ! "Launch " and " now" untouched.
     end program demo_regex_replace
