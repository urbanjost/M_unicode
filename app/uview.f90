program uview
! @(#) given starting and ending codepoint show unicode character in various formats
use, intrinsic :: iso_fortran_env, only : stdout => output_unit
use M_unicode,                     only : unicode_type, assignment(=), len
use M_unicode,                     only : ut=>unicode_type, ch=>character
implicit none
character(len=*),parameter   :: & 

   & form = '(1x,"char(int(z''",z0,"''),kind=ucs4)":,"// &")', &
   & form_M = '(1x,"int(z''",z0,"'')":,", &")'               , &
   & form_zhtml = '(1x,*(:"&#x",z0,";"))'                    , &
   & form_html = '(1x,*(:"&#",i0,";"))'

character(len=*),parameter   :: g= '(*(g0))'
integer                      :: i
integer                      :: ends(2)
integer,allocatable          :: codes(:)
character(len=:),allocatable :: argument
type(unicode_type)           :: ustr

   ends=0
   do i=1,min(2,command_argument_count())
      argument=getarg(i)
      read(argument,*)ends(i)
   enddo
   if(i.eq.2)ends(2)=ends(1)
   write(stdout,g)'START=',ends(1),' END=',ends(2)

   do i=ends(1),ends(2)
      ustr=i
      write(stdout,'(1x,i0)',advance='no') i          ! codepoint
      write(stdout,'(1x,a)',advance='no'),ch(ustr)    ! character  
      write(stdout,'(1x,''U\'',z8.8)',advance='no')i  ! U\00000064
      write(stdout,form_zhtml,advance='no')i          ! &#x4EBA;
      write(stdout,form_html,advance='no')i           ! &#20154;
      write(stdout,form,advance='no')i                ! char(int(z'nnn',kind=ucs4))
      write(stdout,form_M,advance='no')i              ! int(z'nnn')
      write(stdout,*) 
   enddo

contains

function getarg(arg) result(argument)
integer,intent(in)           :: arg
integer                      :: length
character(len=:),allocatable :: argument
   call get_command_argument(arg,length=length) ! get command line argument length
   allocate(character(len=length) :: argument)  ! allocate string big enough to hold argument
   call get_command_argument(arg,argument)      ! get command line argument as a string
end function getarg

end program uview
