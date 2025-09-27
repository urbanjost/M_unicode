program bom_exe
!
! create a Fortran source file starting with a utf-8 BOM to see if your
! compiler will compile it or fail because a character is not in the
! Fortran character set outside of a comment or literal string
!
use iso_fortran_env, only : stdout => output_unit
use M_unicode
implicit none
type(unicode_type) :: U_BOM
integer            :: iostat
   open(stdout,encoding='utf-8',iostat=iostat)
   U_bom=[int(z'FEFF')]
   write(stdout,'(a)',advance='no')U_bom%character()
   write(stdout,'(a)') &

    'program testit ! Unicode BOM encoded to utf-8 bytes by Fortran' ,&
    '   write(stdout,*)"File starts with BOM from UCS-4 write!"'          ,&
    'end program testit'

end program bom_exe
