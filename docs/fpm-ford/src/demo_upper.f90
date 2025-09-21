program demo_upper
use M_unicode, only : upper, unicode_type, assignment(=)
implicit none
character(len=*),parameter :: g='(*(g0))'
type(unicode_type) :: pangram
type(unicode_type) :: upper_pangram
   ! a sentence containing every letter of the English alphabet
   pangram="The quick brown fox jumps over the lazy dog."
   write(*,g)pangram%character()
   upper_pangram=upper(pangram)
   write(*,g)upper_pangram%character()
end program demo_upper
