program test_for_iso_10646
use M_unicode
use iso_fortran_env, only : output_unit
implicit none
type(unicode_type) :: smiley
   smiley=[int(z'1F603')]
   open(output_unit,encoding='utf-8')
   write(*,*)'Smiling face with open mouth',smiley%character() ! 😃
end program test_for_iso_10646
