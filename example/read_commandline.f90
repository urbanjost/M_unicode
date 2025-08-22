program uni_to_ftn
! @(#) take command line argument utf-8 text and generate Fortran statement that represents the string
use, intrinsic :: iso_fortran_env, only : output_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter             :: form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter             :: g= '(*(g0))'
integer                                :: i
character(len=:),allocatable           :: aline
character(len=:),allocatable           :: command_line
character(len=:,kind=ucs4),allocatable :: ustr
   command_line=getargs()          ! get string containing all command arguments as CHARACTER bytes
   ustr=utf8_to_ucs4(command_line) ! convert bytes to internal Fortran Unicode representation

   ! write the command line out as a Fortran variable expression using the CHAR() function
   open (output_unit, encoding='UTF-8')
   write(*,g) '! ENCODING:',command_line
   write(*,g) 'character(len=*,kind=ucs4),parameter :: variable= &'
   write(*,form)(ustr(i:i),i=1,len(ustr))
contains

function getargs() result(command_line)
integer                                :: length
character(len=:),allocatable           :: command_line
   call get_command(length=length)                 ! get command line length
   allocate(character(len=length) :: command_line) ! allocate string big enough to hold command line
   call get_command(command=command_line)          ! get command line as a string
   call get_command_argument(0,length=length)      ! remove argument 0
   command_line=adjustl(command_line(length+1:))
end function getargs

function utf8_to_ucs4(string) result(corrected)
character(len=*),intent(in)            :: string
character(len=:,kind=ucs4),allocatable :: corrected
character(len=(len(string)),kind=ucs4) :: line
integer                                :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')string
   rewind(lun)
   read(lun,'(A)')line
   close(lun)
   corrected=trim(line)
end function utf8_to_ucs4

function ucs4_to_utf8(ucs4_string) result(corrected)
character(len=*,kind=ucs4),intent(in) :: ucs4_string
character(len=:),allocatable          :: corrected
character(len=(len(ucs4_string)*4))   :: line
integer                               :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')ucs4_string
   rewind(lun)
   open(unit=lun,encoding='default')
   read(lun,'(A)')line
   close(lun)
   corrected=trim(line)
end function ucs4_to_utf8
   
end program uni_to_ftn
