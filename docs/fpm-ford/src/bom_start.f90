program bom_exe
!
! create a Fortran source file starting with a utf-8 BOM to see if your
! compiler will compile it or fail because a character is not in the
! Fortran character set outside of a comment or literal string
!
use iso_fortran_env, only : stdout => output_unit
use M_unicode, only : unicode_type, assignment(=), unicode
implicit none
type(unicode_type) :: UT_bom
integer            :: iostat
   !open(stdout,encoding='utf-8',iostat=iostat)
   UT_bom=[int(z'FEFF')]
   UT_bom=unicode%bom
   write(stdout,'(a)',advance='no')UT_bom%character()
   write(stdout,'(a)') &

    'program testit ! Unicode BOM encoded to utf-8 bytes by Fortran' ,&
    '   write(stdout,*)"This source file starts with BOM from UCS-4 write!"'          ,&
    'end program testit'

end program bom_exe
