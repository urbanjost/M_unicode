program checkform
! check if string is of form NN‐HHHHH
use iso_fortran_env, only : stdout => output_unit
use M_unicode,       only : verify, unicode_type, assignment(=)
use M_unicode,       only : ut=>unicode_type
implicit none
character(len=*),parameter :: g='(*(g0,1x))'

character(len=*),parameter :: int='1234567890'
character(len=*),parameter :: hex='abcdefABCDEF0123456789'
logical                    :: lout
type(unicode_type)         :: chars
type(unicode_type)         :: str
integer                    :: iostat

   ! preferred, but not required if not supported
   !open(stdout,encoding='utf-8',iostat=iostat)

   chars='32‐af43d'
   lout=.true.

   ! are the first two characters integer characters?
   str = chars%character(1,2)
   lout = (verify( str, ut(int) ) == 0) .and.lout

   ! is the third character a dash?
   str = chars%character(3,3)
   lout = (verify( str, ut('‐-') ) == 0) .and.lout

   ! is remaining string a valid representation of a hex value?
   str = chars%character(4,8)
   lout = (verify( str, ut(hex) ) == 0) .and.lout

   if(lout)then
      write(stdout,g)trim(chars%character()),' passed'
   else
      write(stdout,g)trim(chars%character()),' failed'
   endif
end program checkform
