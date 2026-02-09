program main_greek
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use,intrinsic :: iso_fortran_env, only : stdout=>output_unit
use M_unicode, only : ut=>unicode_type, assignment(=)
use M_unicode, only : remove_backslash, add_backslash
use M_unicode, only : ch=>character
use M_unicode, only : operator(==)
type(ut) :: glyph
type(ut) :: uline
type(ut) :: uline2
character(len=:),allocatable :: aline
integer  :: i

   ! case of escaped values should be ignored
   write(stdout,'(a)')ch(remove_backslash('\u03C0 and \u03c0'))

   ! RHS is the integer code point, LHS is a string
   do i=int(z'0370'),int(z'03FF')
      glyph=i
      write(stdout,'("U+",z4.4,1x,a)')i, ch(glyph)
   enddo

   ! create a single string composed of the main Greek letters
   uline=[(i,i=int(z'0370'),int(z'03ff'))]
   ! print the string using the CHARACTER() function
   write(stdout,'(a)')ch(uline)
   ! create a character variable of escaped numeric values from the string
   aline=add_backslash(uline)
   write(stdout,'(a)')aline
   ! change the backslash-encoded variable back to the original string
   uline2=remove_backslash(aline)
   write(stdout,'(a)')ch(uline2)
   write(stdout,'(*(g0))')'round trip ',merge('PASSED','FAILED',uline==uline2)
   write(stdout,'(a)')

end program main_greek
