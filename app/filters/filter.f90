program demo_regex_search
! @(#) read a utf-8 file and show lines containing specified regular expression with matched region in inverse
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, iostat_end, int32, stdout=>output_unit
use M_unicode, only : lower, upper, add_backslash, add_html, expand_backslash, expand_html
use M_unicode, only : readline, len, trim, assignment(=), ch=>character, ut=>unicode_type, get_arg, ke=>keyword_expand, operator(//)
use M_unicode, only : regex_t, match_t, regex_compile, regex_search, ctou
implicit none
type(ut)                     :: line
type(regex_t)                :: re
type(match_t)                :: m
integer,allocatable          :: ints(:), pats(:)
integer                      :: iostat, offset, i, l, r, icount
character(len=:),allocatable :: subcommand
character(len=*),parameter   :: g='(*(g0,1x))'
abstract interface
   function noopt(utf8)
   import :: ut
   implicit none
   type(ut),intent(in)          :: utf8
   type(ut)                     :: noopt
   end function noopt
end interface
procedure(noopt),pointer :: finger => null()

   open(unit=stdin,pad='yes')
   icount=0

   subcommand=get_arg(1)
   select case(subcommand)
   case('help','')
      call help()
   case('search')
      call search()
   case('lower');                       finger => dummy1
   case('upper');                       finger => dummy2
   case('add_backslash','toescape');    finger => dummy3
   case('expand_backslash','noescape'); finger => dummy4
   case('add_html','tohtml');           finger => dummy5
   case('expand_html','nohtml');        finger => dummy6
   case('dumphtml') 
      ! if option dump names
      write(*,'(a)')'<html><body><pre>'
      line=expand_html()
      write(*,'(a)')'</pre></body></html>'
   case default
      call help()
      write(*,'(a)')'<ERROR>unknown subcommand '//subcommand
   end select
   if(associated(finger))then
      call read_and_write()
      nullify(finger)
   endif

! add_backslash, add_html, expand_backslash, expand_html
contains
!===================================================================================================================================
function dummy1(utf8); type(ut),intent(in) :: utf8; type(ut) :: dummy1; dummy1=lower(utf8);            end function dummy1
function dummy2(utf8); type(ut),intent(in) :: utf8; type(ut) :: dummy2; dummy2=upper(utf8);            end function dummy2
function dummy3(utf8); type(ut),intent(in) :: utf8; type(ut) :: dummy3; dummy3=add_backslash(utf8);    end function dummy3
function dummy4(utf8); type(ut),intent(in) :: utf8; type(ut) :: dummy4; dummy4=expand_backslash(utf8); end function dummy4
function dummy5(utf8); type(ut),intent(in) :: utf8; type(ut) :: dummy5; dummy5=add_html(utf8);         end function dummy5
function dummy6(utf8); type(ut),intent(in) :: utf8; type(ut) :: dummy6; dummy6=expand_html(utf8);      end function dummy6
!===================================================================================================================================
subroutine help()
character(len=*),parameter :: help_text(*)=[character(len=80) :: &
"================================================================================", &
" The filter(1) command takes the following keywords:                            ", &
"                                                                                ", &
"   search 'REGEX'  display lines matching the regular expression with inverse   ", &
"                   highlighting of the matches                                  ", &
"                                                                                ", &
"   help            display this help text                                       ", &
"                                                                                ", &
"   lower                        convert input to lowercase                      ", &
"   upper                        convert input to uppercase                      ", &
"                                                                                ", &
"   toescape, add_backslash      change multibyte characters to escape sequences ", &
"   noescape, expand_backslash   convert escape sequences to utf8                ", &
"                                                                                ", &
"   tohtml,   add_html           change multibyte characters to html entities    ", &
"   nohtml,   expand_html        convert html entities to utf8                   ", &
"   dumphtml                     dump HTML entity names                          ", &
"                                                                                ", &
"================================================================================"]
   write(stdout,'(a)')(trim(help_text(i)),i=1,size(help_text))
end subroutine help
!===================================================================================================================================
subroutine read_and_write()
type(ut) :: line
integer  :: iostat
   open(unit=stdin,pad='yes')
   INFINITE: do
      line=finger(readline(iostat=iostat))
      if(iostat /= 0)exit
      write(*,'(*(g0))')line%character()
   enddo INFINITE
   if(iostat /= iostat_end)then
      write(*,*)'error reading input:',line%character()
   endif
end subroutine read_and_write
!===================================================================================================================================
subroutine search()
   pats=get_arg(2)
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
end subroutine search
!===================================================================================================================================
end program demo_regex_search
