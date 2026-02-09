program utf8_len
! @(#) read a utf-8 file and write it out with lines prefixed with glyph count of the line
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, iostat_end
use M_unicode, only : readline, len, trim, assignment(=), ch=>character, ut=>unicode_type
implicit none
intrinsic is_iostat_end
character(len=*),parameter    :: g='(*(g0))'
integer                       :: ulen
integer                       :: alen
integer                       :: iostat
integer                       :: icount
type(ut)                      :: uline 
   open(unit=stdin,pad='yes')
   icount=0
   INFINITE: do
      uline=readline(iostat=iostat)
      icount=icount+1
      if(iostat.ne.0)exit
      ulen=len(uline)
      alen=len(uline%character())
      write(stdout,'(i5,a,i5,": ",a)')ulen,merge(' /=',' ==',maxval(uline%codepoint()).gt.127),alen,uline%character()
   enddo INFINITE
   if(iostat /= iostat_end)then
      write(*,*)'<ERROR> failed on read of input line ',icount,':',ch(trim(uline))
   endif
end program utf8_len
