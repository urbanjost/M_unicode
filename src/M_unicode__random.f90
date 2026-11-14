module M_unicode__random
private
public :: random_string
!   o random_string(3f): create random string composed of provided characters of specified length
interface random_string
   module procedure :: random_string
end interface random_string
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!### NAME
!!    random_string(3f) - [M_random] create random string composed of
!!                        provided characters of specified length
!!    (LICENSE:MIT)
!!
!!### SYNOPSIS
!!
!!    function random_string(chars,length) result(out)
!!
!!     character(len=*),intent(in)   :: chars
!!     integer,intent(in)            :: length
!!     character(len=length)         :: out
!!
!!     type(unicode_type),intent(in) :: ustr
!!     integer,intent(in)            :: length
!!     type(unicode_type)            :: out(length)
!!
!!     integer,intent(in)            :: codes(:)
!!     integer,intent(in)            :: length
!!     integer                       :: out(length)
!!
!!### DESCRIPTION
!!    Given a set of characters and a length, generate a random string of
!!    the specified length composed of the given set of characters.
!!
!!### OPTIONS
!!    astr    character variable containing characters to construct OUT with
!!    ustr    string containing characters to construct OUT with
!!    codes   integer array of valid Unicode codepoint values
!!
!!    length  number of characters to place in output
!!
!!### RESULT
!!    out     string of LENGTH characters randomly filled with characters
!!            from CHARS
!!
!!### EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_random_string
!!     use M_unicode, only : random_string
!!     character(len=64) :: hexstring
!!     ! ensure getting an initially pseudo-random sequence
!!        call random_init(.false., .true.)
!!        hexstring=random_string('0123456789abcdef',len(hexstring))
!!        ! write random hexadecimal value for use
!!        ! as something like an X11 authorization key
!!        write(*,'(a)')hexstring
!!     end program demo_random_string
!!
!!    Results
!!
!!     2363a3589736e23be0137ec7ebc9d74297a963f27958a176daea3dd850ed8487
!!
!!### AUTHOR
!!    John S. Urban
!!
!!### LICENSE
!!    MIT License
!-----------------------------------------------------------------------------------------------------------------------------------
function random_string(codes,length) result(out)
integer,intent(in)              :: codes(:)
integer,intent(in)              :: length
integer                         :: out(length)
   real                         :: x
   integer                      :: ilen
   integer                      :: i
   ilen=size(codes)
   if(ilen.gt.0)then
      do i=1,length
         call random_number(x)
         out(i)=nint(real(ilen-1)*x)+1
      enddo
   else
      out=0
   endif
end function random_string
!-----------------------------------------------------------------------------------------------------------------------------------
end module M_unicode__random
