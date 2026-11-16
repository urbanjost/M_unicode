module M_unicode__random
!   o random_string(3f): create random string composed of provided characters of specified length
private
public :: random_string

interface random_string
   module procedure :: random_string
end interface random_string

contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    random_string(3f) - [M_random] create random string composed of
!!                        provided characters of specified length
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
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
!!##DESCRIPTION
!!    Given a set of characters and a length, generate a random string of
!!    the specified length composed of the given set of characters.
!!
!!##OPTIONS
!!    astr    character variable containing characters to construct OUT with
!!    ustr    string containing characters to construct OUT with
!!    codes   integer array of valid Unicode codepoint values
!!
!!    length  number of characters to place in output
!!
!!##RESULT
!!    out     string of LENGTH characters randomly filled with characters
!!            from CHARS
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_random_string
!!     use iso_fortran_env, only : stdout => output_unit
!!     use M_unicode,       only : random_string, ut=>unicode_type
!!     use M_unicode,       only : assignment(=), ch=>character
!!     character(len=:),allocatable :: aset
!!     type(ut)                     :: uset, ulets
!!     integer,allocatable          :: iset(:), ilets(:)
!!     integer                      :: i
!!     character(len=*),parameter   :: g='("[",g0,"]")'
!!
!!      ! ensure getting an initially pseudo-random sequence
!!        call random_init(.false., .true.)
!!
!!      ! ASCII-7 CHARACTER variable
!!        ! random hexadecimal values are often used as authorization keys
!!        aset='0123456789abcdef'
!!        write(stdout,g)aset, random_string(aset,64)
!!
!!      ! using type(unicode_type)
!!        ! pick letters from Greek uppercase alphabet
!!        uset='ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ'
!!        write(stdout,g) uset%ch(), ch(random_string(uset,16))
!!
!!      ! using codepoints
!!        ! ADE values for ASCII-7 uppercase
!!        iset=[(i,i=65,90)]
!!        ilets=random_string(iset,32)
!!        ! convert the codepoint values to a string
!!        ulets=ilets
!!        write(stdout,g) ulets%ch()
!!
!!     end program demo_random_string
!!
!!    Results
!!
!!     > [0123456789abcdef]
!!     > [b4b43bb527d410326d7b8f7a4273ca6710b1a6b1acf78ec51bbd70d8e224223e]
!!     > [ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]
!!     > [ΜΒΘΘΛΓΘΦΘΑΣΠΕΤΦΕ]
!!     > [POFBDMVWNKDXCMVOEJLPKDBTNGEMUYDO]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    MIT License
!-----------------------------------------------------------------------------------------------------------------------------------
function random_string(codes,length) result(out)
integer,intent(in)              :: codes(:)
integer,intent(in)              :: length
integer                         :: out(length)
   real                         :: x
   integer                      :: number_of_chars
   integer                      :: i
   number_of_chars=size(codes)
   if(number_of_chars.gt.0)then
      do i=1,length
         call random_number(x)
         out(i)=codes(nint(real(number_of_chars-1)*x)+1)
      enddo
   else
      out=0
   endif
end function random_string
!-----------------------------------------------------------------------------------------------------------------------------------
end module M_unicode__random
