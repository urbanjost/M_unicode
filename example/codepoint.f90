program test_for_iso_10646
use M_unicode
use iso_fortran_env, only : stdout => output_unit
implicit none
type(unicode_type) :: smiley
   smiley=[int(z'1F603')]
   write(stdout,*)'Smiling face with open mouth',smiley%character() ! 😃
end program test_for_iso_10646
