program concatenate
use M_unicode, only : operator(//)
use M_unicode, only : ut=>unicode_type, ch=>character
type(ut) :: string
integer  :: ten=10,twenty=20
   string='so '//ten//'+'//twenty//'='//ten+twenty
   write(*,*)string%character()
   string=10 // ' ' // 20 // ' ' // 3.14159265 // ' ' // (3,4) // ' ' // .true.
   write(*,*)string%character()
   string=10 // 20 
   write(*,*)string%character()
end program concatenate
