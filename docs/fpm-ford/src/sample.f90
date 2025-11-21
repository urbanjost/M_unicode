program assign_exe
use iso_fortran_env, only : stdout => output_unit
use M_unicode,       only : len, len_trim, repeat, trim, adjustr, adjustl
use M_unicode,       only : character
use M_unicode,       only : assignment(=), unicode_type
implicit none
character(len=*),parameter   :: cat='(*(g0))'             ,&
                                gap='(*(g0,1x))'          ,&
                                dash='(*(g0,"-"))'          ,&
                                bracket='(*("[",g0,"]":))'
type(unicode_type)           :: uline, substring

   uline="   Until you stop, you won't be able to.   "
   call showme(uline)
   uline="   Доки не впріти, доти не вміти.   "
   call showme(uline)

   print * 

   write(stdout,gap)'third word is: ',character(uline,12,17) ! substring

   substring=character(uline,20,32)
   write(stdout,gap)'substring is: ',character(substring)

   ! can set with codepoints
   uline=[32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288]
   write(stdout,gap)'different kinds of spaces:',uline%character()

contains

subroutine showme(uline)
type(unicode_type),intent(in) :: uline
integer                       :: i
   write(stdout,cat)' ',repeat('1234567890',len(uline)/10+1)
   write(stdout,bracket)character(uline)
   write(stdout,cat)'length in bytes is: ',len(uline%character())
   write(stdout,cat)'length in glyphs is: ',len(uline)
   write(stdout,cat)'trimmed length in glyphs is: ',len_trim(uline)
   write(stdout,cat)'adjustr:[',character(adjustr(uline)),']'
   write(stdout,cat)'adjustl:[',character(adjustl(uline)),']'
   write(stdout,cat)'cropped:[',character(trim(adjustl(uline))),']'
   write(stdout,gap)'codepoints:',uline%codepoint()
   write(stdout,dash)'glyphs:',(character(uline%sub(i,i)),i=1,len(uline))
   write(stdout,*)
end subroutine showme

end program assign_exe
