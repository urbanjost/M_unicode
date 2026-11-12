program demo_regex_search
! @(#) read a utf-8 file and show lines containing specified regular expression with matched region in inverse
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, iostat_end, int32
use M_unicode, only : readline, len, trim, assignment(=), ch=>character, ut=>unicode_type, get_arg, ke=>keyword_expand, operator(//)
use M_unicode__regex, only : regex_t, match_t, regex_compile, regex_search, ctou
implicit none
type(ut)            :: line
type(regex_t)       :: re
type(match_t)       :: m
integer,allocatable :: ints(:), pats(:)
integer             :: iostat, offset, i, l, r, icount
   open(unit=stdin,pad='yes')
   icount=0
   pats=get_arg(1)
   re = regex_compile(pats)
   INFINITE: do
      ints=readline(iostat=iostat)
      if(iostat /= 0)exit
      icount=icount+1
      if (.not. re%valid) error stop 'Failed to compile regex pattern'
      m = regex_search(re, ints, start_offset=offset)
      if (.not. m%matched) cycle
      l=m%start_pos(0)
      r=m%end_pos(0)
      line= ctou(ints(:l-1)) // ke('<inverse>') // ctou(ints(l:r)) // ke('<reset>') // ctou(ints(r+1:))
      write(*,'(*(a))') icount, line%ch()
   enddo INFINITE
   if(iostat /= iostat_end)then
      write(*,*)'<ERROR> failed on read of input line ',icount,':',ch(trim(line))
   endif
end program demo_regex_search
