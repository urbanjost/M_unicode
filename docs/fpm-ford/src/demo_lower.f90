program demo_lower
use M_unicode, only : lower, unicode_type, assignment(=)
implicit none
character(len=*),parameter :: g='(*(g0))'
type(unicode_type) :: pangram
type(unicode_type) :: lower_pangram
   ! a sentence containing every letter of the English alphabet
   pangram="THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
   write(*,g)pangram%character()
   lower_pangram=lower(pangram)
   write(*,g)lower_pangram%character()
end program demo_lower
