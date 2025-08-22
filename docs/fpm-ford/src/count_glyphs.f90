program count_glyphs
! @(#) read a utf-8 file and write it out with lines prefixed with glyph count of the line
use, intrinsic :: iso_fortran_env, only : output_unit, input_unit
use M_unicode
implicit none
intrinsic is_iostat_end
character(len=*),parameter    :: g= '(*(g0))'
integer                       :: length
integer                       :: iostat
character(len=1024)           :: aline 
type(unicode_type)            :: uline 
character(len=255)            :: iomsg

   do
      read(input_unit,'(a)',iostat=iostat,iomsg=iomsg)aline
      if(iostat.eq.0)then
         uline=trim(aline)
         length=len_trim(uline)
         write(output_unit,'(i9,": ",a)')length,uline%character(1,length)
      elseif(is_iostat_end(iostat))then
         exit
      else 
         write(output_unit,g)'<ERROR>',trim(iomsg)
         exit
      endif
   enddo

end program count_glyphs
