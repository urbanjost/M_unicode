program utf8_upper
! @(#) ucase(1): convert UTF-8 input to uppercase
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, iostat_end
use M_unicode, only : readline, upper, assignment(=), ut=>unicode_type
implicit none
type(ut) :: line
integer  :: iostat
   open(unit=stdin,pad='yes')
   INFINITE: do
      line=upper(readline(iostat=iostat))
      if(iostat.ne.0)exit
      write(*,'(*(g0))')line%character()
   enddo INFINITE
   if(iostat /= iostat_end)then
      write(*,*)'error reading input:',line%character()
   endif
end program utf8_upper
