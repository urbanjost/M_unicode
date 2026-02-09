program non_ascii
! @(#) read a utf-8 file and write and identify lines not composed entirely of ASCII-7
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, iostat_end
use M_unicode, only : readline, len, trim, add_backslash, assignment(=), ch=>character, ut=>unicode_type
implicit none
type(ut)                     :: line
character(len=:),allocatable :: aline
integer,allocatable          :: ints(:)
integer                      :: iostat
integer                      :: icount
   open(unit=stdin,pad='yes')
   icount=0
   INFINITE: do
      icount=icount+1
      line=readline(iostat=iostat)
      if(iostat.ne.0)exit
      ints=line
      if(maxval(ints).gt.127)then
         ! write the line number line in brackets 
         write(*,'(i8,1x,a)')icount,'['//ch(line)//']'
         ! write the line with all but ASCII7 replaced with escape codes
         write(*,'(9x,a)')'['//add_backslash(line)//']'
      endif
   enddo INFINITE
   if(iostat /= iostat_end)then
      write(*,*)'<ERROR> failed on read of input line ',icount,':',ch(trim(line))
   endif
end program non_ascii
