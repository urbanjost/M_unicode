program show_non_ascii
! @(#) read a utf-8 file and write and identify lines not composed entirely of ASCII-7
   use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
   use,intrinsic :: iso_fortran_env, only : iostat_end
   use M_unicode, only : readline, len, trim, add_backslash
   use M_unicode, only : assignment(=), ch=>character, ut=>unicode_type
   implicit none
   type(ut)                     :: line
   character(len=:),allocatable :: aline
   integer,allocatable          :: ints(:)
   integer                      :: iostat
   integer                      :: icount
   integer                      :: byte_count
   integer                      :: glyph_count
   open(unit=stdin,pad='yes')
!
   icount=0
   INFINITE: do
      icount=icount+1
      line=readline(iostat=iostat)
      if(iostat.ne.0)exit
      glyph_count=len(line)
      byte_count=len(ch(line))
      ints=line
      if(maxval(ints).gt.127)then
         ! write the line number, lengths and line in brackets
         write(*,'(i8,1x,i5,1x,i5,1x,*(g0,1x))')icount,glyph_count,byte_count,'['//ch(line)//']'
         ! write the line with all but ASCII7 replaced with escape codes
         write(*,'(21x,*(g0,1x))')'['//add_backslash(line)//']'
      endif
   enddo INFINITE
   !
   if(iostat /= iostat_end)then
      write(*,*)'<ERROR> failed on read of input line ',icount,':',ch(trim(line))
   endif
   !
end program show_non_ascii
