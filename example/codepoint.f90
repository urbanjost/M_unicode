program test_for_iso_10646
use M_unicode
use iso_fortran_env, only : stdout => output_unit
implicit none
type(unicode_type) :: smiley

   smiley=[int(z'1F603')]
   write(stdout,*)'Smiling face with open mouth',smiley%character() ! 😃

   smiley='😃'
   write(stdout,*)'one CHARACTER string to one UNICODE_TYPE string',smiley%character()

   smiley=['1😃','2😃','3😃','4😃']

   write(stdout,*)'four CHARACTER strings becomes one UNICODE_TYPE string ',smiley%character()

   smiley=128515
   write(stdout,*)'one pointcode ',smiley%character(),smiley%codepoint()

   smiley=[128515,128515]
   write(stdout,*)'array of pointcode ',smiley%character(),smiley%codepoint()

end program test_for_iso_10646
