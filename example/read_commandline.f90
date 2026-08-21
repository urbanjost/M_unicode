program uni_to_ftn
! @(#) take command line argument utf-8 text and generate Fortran statement that represents the string
use, intrinsic :: iso_fortran_env, only : stdout => output_unit
implicit none
character(len=*),parameter   :: form= '("char(int(z''",z0,"''))":,"// &")'
character(len=*),parameter   :: g= '(*(g0))'
integer                      :: i
character(len=:),allocatable :: command_line

   command_line=getargs()          ! get string containing all command arguments as CHARACTER bytes

   ! write the command line out as a Fortran variable expression using the CHAR() function
   write(stdout,g) '! ENCODING:[',command_line//']'
   write(stdout,g) 'character(len=*),parameter :: variable= &'
   write(stdout,form)(ichar(command_line(i:i)),i=1,len(command_line))
contains

function getargs() result(command_line)
integer                                :: length
character(len=:),allocatable           :: command_line
   call get_command(length=length)                 ! get command line length
   allocate(character(len=length) :: command_line) ! allocate string big enough to hold command line
   call get_command(command=command_line)          ! get command line as a string
   call get_command_argument(0,length=length)      ! remove argument 0
   command_line=command_line(length+2:)
end function getargs

end program uni_to_ftn
