program note
use iso_fortran_env, only : stdout => output_unit
use M_unicode
implicit none
integer,parameter  :: pointer(*) = [int(z'1FBC1'), int(z'1FBC2'), int(z'1FBC3')]
type(unicode_type) :: uline
integer            :: iostat

   ! preferred, but not required if not supported
   open(stdout,encoding='utf-8',iostat=iostat)

   uline=pointer
   write(stdout,'(*(g0))')uline%character()

end program note
