program test_for_iso_10646
use M_unicode
use iso_fortran_env, only : stdout => output_unit
implicit none
type(unicode_type) :: smiley
type(unicode_type),allocatable :: arr(:)
integer :: i

   smiley=[int(z'1F603')]
   write(stdout,*)'Smiling face with open mouth',smiley%character() ! 😃

   smiley='😃'
   write(stdout,*)'one CHARACTER string to one UNICODE_TYPE string',smiley%character()

   smiley=['1😃','2😃','3😃','4😃']
   write(stdout,'(a,*(:"[",a,"]"))')'four CHARACTER strings becomes one UNICODE_TYPE string ',smiley%character()

   arr=['1😃','2😃','3😃','4😃']
   write(stdout,'(a,*(:"[",a,"]"))')'four CHARACTER strings becomes four UNICODE_TYPE string ',(arr(i)%character(),i=1,size(arr))

   smiley=128515
   write(stdout,*)'one codepoint ',smiley%character(),smiley%codepoint()

   smiley=[128515,128515]
   write(stdout,*)'arr of codepoint ',smiley%character(),smiley%codepoint()

end program test_for_iso_10646
