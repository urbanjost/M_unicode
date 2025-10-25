      program demo_readline
      use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
      use,intrinsic :: iso_fortran_env, only : iostat_end
      use M_unicode, only : readline, len, trim
      use M_unicode, only : assignment(=), ch=>character, ut=>unicode_type
      implicit none
      type(ut)                     :: line
      character(len=:),allocatable :: aline
      integer,allocatable          :: ints(:)
      integer                      :: iostat
         open(unit=stdin,pad='yes')
         !
         INFINITE: do
            line=readline(iostat=iostat)
            if(iostat.ne.0)exit
            ! write the length, line in brackets and its Unicode codepoints
            write(*,'(*(g0,1x))')len(line),'['//ch(line)//']',line%codepoint()
            ! or assign the string to an allocatable array of integers
            ints=line
            ! and the string to a character variable
            aline=line
            write(*,'(*(g0,1x))')len(line),'['//ch(line)//']',ints
         enddo INFINITE
         !
         if(iostat /= iostat_end)then
            write(*,*)'error reading input:',ch(trim(line))
         endif
         !
      end program demo_readline
