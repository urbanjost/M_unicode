program random_range_demo
use, intrinsic :: iso_fortran_env, only: real64
use M_unicode, only : str_to_real, get_arg
! generate a random floating-point number between two bounds (low and high),
implicit none
real(kind=real64) :: val
integer           :: i
integer           :: iostat
integer           :: lun
integer           :: ierr
integer           :: isize
integer           :: icase
character(len=30),allocatable :: line(:)

   icase=get_arg(1,0)
   ! Initialize the random number generator seed once at the start of the program
   call random_seed()
   open(newunit=lun,status='scratch',form='formatted',action='readwrite')
   ! Generate random floats
   isize=1000000
   allocate(line(isize))
   do i = 1, isize
      val = random_uniform(-huge(0.0)/100,huge(0.0)/100)
      write(line(i),*) val
      !write(lun,*) val
   enddo
   !rewind(lun)
   !do i = 1, huge(0)
   do i = 1, isize
      !read(lun,*,iostat=iostat) line
      !if (iostat/=0)exit
      select case(icase)
      case(0)
         if(i.eq.1)write(*,*)'INTERNAL READ'
         read(line(i),*) val
      case(1)
         if(i.eq.1)write(*,*)'STR_TO_REAL'
         call str_to_real(line(i),val,ierr)
      case(2)
         write(*,*)i,line(i)
      case(3)
         read(line(i),*) val
         write(*,*)i,line(i),val
      case(4)
         call str_to_real(line(i),val,ierr)
         write(*,*)i,line(i),val
      case default
      end select
   enddo
   write(*,*)'CASE',icase

contains

! Function to return a random float in [low, high]
real function random_uniform(low, high)
real, intent(in) :: low, high
real             :: u
   ! Generates a uniform random number in [0.0, 1.0)
   call random_number(u)
   ! Scale and shift to [low, high]
   random_uniform = low + (high - low) * u
end function random_uniform

end program random_range_demo
