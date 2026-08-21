program utf8_to_ucs4_code
! @(#) read utf-8 on stdin and generate Fortran statements using KIND='iso_10464' that represents the lines
use,intrinsic :: iso_fortran_env, only : iostat_end
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use,intrinsic :: iso_fortran_env, only : stdout => output_unit
use M_unicode, only : readline, len, trim, ch=>character
use M_unicode, only : assignment(=), ut=>unicode_type
use M_unicode, only : operator(==)
implicit none
character(len=*),parameter   :: form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter   :: g= '(*(g0))'
integer                      :: i
type(ut)                     :: line
integer                      :: iostat
integer                      :: icount

   !open (stdout, encoding='UTF-8')
   open(unit=stdin,pad='yes')

   icount=0
   INFINITE: do
      line=readline(iostat=iostat)
      icount=icount+1
      if(iostat.ne.0)exit
      call printline()
   enddo INFINITE
   !
   if(iostat /= iostat_end)then
      write(*,*)'error reading input line:',icount,':',ch(trim(line))
   endif

contains
subroutine printline()
   ! write the line out as a Fortran variable expression using the CHAR() function
   if(line.eq.'')then
      write(stdout,g) '! ISO-10646 ENCODING:',ch(line)
      write(stdout,g) 'character(len=*,kind=ucs4),parameter :: line',icount,'= ucs4_""'
      write(stdout,g)
   else
      write(stdout,g) '! ISO-10646 ENCODING:',ch(line)
      write(stdout,g) 'character(len=*,kind=ucs4),parameter :: line',icount,'= &'
      write(stdout,form)(line%codepoint(i,i),i=1,len(line))
      write(stdout,g)
   endif
end subroutine printline

end program utf8_to_ucs4_code
