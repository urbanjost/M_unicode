program concatenate
use M_unicode, only : operator(//)
use M_unicode, only : ut=>unicode_type, ch=>character
type(ut) :: string
integer  :: ten=10,twenty=20
character(len=:),allocatable :: line
   ! ifx requires (), flang_new and gfortran do not at the moment
   string='so '//ten//'+'//twenty//'='//(ten+twenty)
   write(*,*)string%character()
   string=10 // ' ' // 20 // ' ' // 3.14159265 // ' ' // (3,4) // ' ' // .true.//(1.0d0/3.0d0)
   write(*,*)string%character()
   string=10 // 20 
   write(*,*)string%character()
   line='abc'//'def'
   write(*,*)line
   string='abc'//ten
   write(*,*)string%character()
   string=ten//'abc'
   write(*,*)string%character()
end program concatenate
