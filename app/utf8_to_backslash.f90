program utf8_to_backslash
! @(#) convert UTF-8 to backslash escape sequences
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, iostat_end
use M_unicode, only : readline, add_backslash, assignment(=), ut=>unicode_type
implicit none
type(ut)                     :: line
integer                      :: iostat
   open(unit=stdin,pad='yes')
   INFINITE: do
      line=readline(iostat=iostat)
      if(iostat.ne.0)exit
      ! write the length, line in brackets and its Unicode codepoints
      line=add_backslash(line)
      write(*,'(*(g0))')line%character()
   enddo INFINITE
   if(iostat /= iostat_end)then
      write(*,*)'error reading input:',line%character()
   endif
end program utf8_to_backslash
