program backslash_to_utf8
! @(#) convert backslash expand_html sequences to UTF-8
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use,intrinsic :: iso_fortran_env, only : iostat_end
use M_unicode, only : readline, expand_html
use M_unicode, only : assignment(=), ch=>character, ut=>unicode_type
implicit none
type(ut) :: line
integer  :: iostat
logical  :: triggered
    open(unit=stdin,pad='yes')
    triggered=.false.
    INFINITE: do
      line=readline(iostat=iostat)
      if(iostat.ne.0)exit
      line=expand_html(line)
      write(*,'(*(g0))')ch(line)
      triggered=.true.
   enddo INFINITE
   if(.not.triggered.and.iostat == iostat_end)then
      ! if null file dump names
      write(*,'(a)')'<html><body><pre>'
      line=expand_html()
      write(*,'(a)')'</pre></body></html>'
   elseif(iostat /= iostat_end)then
      write(*,*)'error reading input:',ch(line)
   endif
end program backslash_to_utf8
