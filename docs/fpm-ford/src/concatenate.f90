program concatenate
use M_unicode, only : operator(.cat.), operator(.cat.)
use M_unicode, only : ut=>unicode_type, ch=>character
type(ut) :: string
integer  :: ten=10,twenty=20
character(len=:),allocatable :: line
   ! ifx requires (), flang_new and gfortran do not at the moment
   string='so ' .cat. ten .cat. '+' .cat. twenty .cat. '=' .cat. (ten+twenty)
   write(*,*)string%character()
   string=10 .cat. ' ' .cat. 20 .cat. ' ' .cat. 3.14159265 .cat. ' ' .cat. (3,4) .cat. ' ' .cat. .true. .cat. (1.0d0/3.0d0)
   write(*,*)string%character()
   string=10 .cat. 20 
   write(*,*)string%character()
   line='abc' // 'def'
   write(*,*)line
   string='abc' .cat. ten
   write(*,*)string%character()
   string=ten .cat. 'abc'
   write(*,*)string%character()
end program concatenate
