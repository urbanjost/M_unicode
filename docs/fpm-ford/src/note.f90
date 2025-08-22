program note
use M_unicode
implicit none
integer,parameter :: pointer(*) = [int(z'1FBC1'), int(z'1FBC2'), int(z'1FBC3')]
type(unicode_type) :: uline
   uline=pointer
   write(*,'(*(g0))')uline%character()
end program note
