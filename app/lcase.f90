program utf8_lower
! @(#) lcase(1): convert UTF-8 input to lowercase
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, iostat_end
use M_unicode, only : readline, lower, assignment(=), ut=>unicode_type
implicit none
type(ut) :: line
integer  :: iostat
   open(unit=stdin,pad='yes')
   INFINITE: do
      line=lower(readline(iostat=iostat))
      if(iostat.ne.0)exit
      write(*,'(*(g0))')line%character()
   enddo INFINITE
   if(iostat /= iostat_end)then
      write(*,*)'error reading input:',line%character()
   endif
end program utf8_lower
