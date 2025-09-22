program checkform
! check if string is of form NN‐HHHHH
use M_unicode, only : verify, unicode_type, assignment(=)
use M_unicode, only : ut=>unicode_type
implicit none
character(len=*),parameter :: g='(*(g0,1x))'

integer                    :: pos
character(len=*),parameter :: int='1234567890'
character(len=*),parameter :: hex='abcdefABCDEF0123456789'
logical                    :: lout
type(unicode_type)         :: chars
type(unicode_type)         :: str
type(unicode_type)         :: set

   chars='32‐af43d'
   lout=.true.

   ! are the first two characters integer characters?
   str = chars%character(1,2)
   lout = lout.and.verify( str, ut(int) ) == 0

   ! is the third character a dash?
   str = chars%character(3,3)
   lout = lout.and.verify( str, ut('‐-') ) == 0

   ! is remaining string a valid representation of a hex value?
   str = chars%character(4,8)
   lout = lout.and.verify( str, ut(hex) ) == 0

   if(lout)then
      write(*,g)trim(chars%character()),' passed'
   else
      write(*,g)trim(chars%character()),' failed'
   endif
end program checkform
