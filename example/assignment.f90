program test_for_iso_10646
!Improve assignment
!
! Make lhs=rhs work when rhs is an array of character variables 
! but lhs is a scalar or an array.
!
use M_unicode
use iso_fortran_env, only : stdout => output_unit
implicit none
type(unicode_type)             :: scalar
type(unicode_type),allocatable :: arr(:)
integer                        :: i

   scalar=['1😃','2😃','3😃','4😃']
   write(stdout,'(a,*(:"[",a,"]"))') &
   & 'four CHARACTER strings becomes one UNICODE_TYPE string ', &
   & scalar%character()

   arr=['1😃','2😃','3😃','4😃']
   write(stdout,'(a,*(:"[",a,"]"))') &
   & 'four CHARACTER strings becomes four element UNICODE_TYPE string array ', &
   & (arr(i)%character(),i=1,size(arr))

end program test_for_iso_10646
