program backslash_to_utf8
! @(#) convert backslash escape sequences to UTF-8
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use,intrinsic :: iso_fortran_env, only : iostat_end
use M_unicode, only : readline, remove_backslash
use M_unicode, only : assignment(=), ch=>character, ut=>unicode_type
implicit none
type(ut) :: line
integer  :: iostat
    open(unit=stdin,pad='yes')
    INFINITE: do
      line=readline(iostat=iostat)
      if(iostat.ne.0)exit
      line=remove_backslash(line)
      write(*,'(*(g0))')ch(line)
   enddo INFINITE
   if(iostat /= iostat_end)then
      write(*,*)'error reading input:',ch(line)
   endif
end program backslash_to_utf8
