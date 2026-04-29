      program fortran_ints
      use M_unicode, only : ut=>unicode_type,assignment(=)
      use M_unicode, only : adjustr, verify, trim, len
      use M_unicode, only : write(formatted)
      use M_unicode, only : operator(.cat.)
      use M_unicode, only : operator(==)
      implicit none
      integer :: i
      character(len=*),parameter :: asciiints(*)=[character(len=10) :: &
       "+1 ", &
       "3044848 ", &
       "30.40 ", &
       "September ", &
       "1 2 3", &
       "  -3000 ", &
       " "]
       type(ut),allocatable :: ints(:)
       allocate(ints(size(asciiints))) ! gfortran bug
       ints=asciiints
       ints=trim(ints)
       ! show if strings pass or fail the test done by isint(3)
       write(*,"('is integer?')")
       do i=1,size(ints)
         write(*,'("|",DT,T14,"|",l1,"|")') ints(i), isint(ints(i))
       enddo
       ! elemental
       write(*,"(*(g0,1x))") isint(ints)

      contains

      impure elemental function isint(line) result (lout)
      use M_unicode, only : adjustl, verify, trim
      !
      ! determine if string is a valid integer representation
      ! ignoring trailing spaces and leading spaces
      !
      character(len=*),parameter :: digits="0123456789"
      type(ut),intent(in)        :: line
      type(ut)                   :: name
      logical                    :: lout
         lout=.false.
         ! make sure at least two characters long to simplify tests
         name=adjustl(line).cat.'  '
         ! blank string
         if( name == '' )return
         ! allow one leading sign
         if( verify(name%sub(1,1),ut('+‐-')) == 0 ) name=name%sub(2,len(name))
         ! was just a sign
         if( name == '' )return
         lout=verify(trim(name), digits)  == 0
      end function isint

      end program fortran_ints
