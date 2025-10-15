    program demo_readline
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
    use,intrinsic :: iso_fortran_env, only : iostat_end
    use M_unicode, only : readline, assignment(=), ch=>character, ut=>unicode_type, trim, len
    implicit none
    type(ut)                     :: line
    character(len=:),allocatable :: aline
    integer,allocatable          :: ints(:)
    integer                      :: iostat
       open(unit=stdin,pad='yes')

       INFINITE: do
          line=readline(iostat=iostat)
          if(iostat.ne.0)exit
          aline=line
          ints=line
          write(*,'(*(g0,1x))')len(line),'['//aline//']',line%codepoint()
          ! alternatively
          write(*,'(*(g0,1x))')len(line),'['//ch(line)//']',ints
       enddo INFINITE

       if(iostat /= iostat_end)then
          write(*,*)'error reading input:',ch(trim(line))
       endif

    end program demo_readline
