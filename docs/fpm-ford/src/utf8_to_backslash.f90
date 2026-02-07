program demo_readline
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use,intrinsic :: iso_fortran_env, only : iostat_end
use M_unicode, only : readline, trim, add_backslash
use M_unicode, only : assignment(=), ch=>character, ut=>unicode_type
implicit none
type(ut)                     :: line
integer,allocatable          :: ints(:)
integer                      :: iostat
    open(unit=stdin,pad='yes')
    !
    INFINITE: do
      line=readline(iostat=iostat)
      if(iostat.ne.0)exit
      ! write the length, line in brackets and its Unicode codepoints
      line=add_backslash(line)
      write(*,'(*(g0))')ch(line)
   enddo INFINITE
   !
   if(iostat /= iostat_end)then
      write(*,*)'error reading input:',ch(trim(line))
   endif
   !
end program demo_readline
