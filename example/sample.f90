program assign_exe
use iso_fortran_env, only : stdout => output_unit
use M_unicode,       only : len, len_trim, repeat, trim, adjustr, adjustl
use M_unicode,       only : character
use M_unicode,       only : assignment(=), unicode_type
implicit none
character(len=*),parameter   :: g='(*(g0))'
character(len=:),allocatable :: aline
type(unicode_type)           :: uline, substring
character(len=*),parameter   :: smiley='😃'
integer                      :: iostat

   ! preferred, but not required if not supported
   open(stdout,encoding='utf-8',iostat=iostat)

   aline="Доки не впріти, доти не вміти."

   write(stdout,g)'123456789012345678901234567890'
   write(stdout,g)aline
   write(stdout,g)'length in bytes is: ',len(aline)
   uline=aline
   write(stdout,g)'length in glyphs is: ',len(uline)

   write(stdout,g)'string is: ',character(uline)
   write(stdout,g)'third word is: ',character(uline,9,14) ! substring

   substring=character(uline,17,29)
   write(stdout,g)'string is: ',character(substring)

   uline=repeat(smiley,30)
   write(stdout,g) character(uline)

   write(stdout,g) len_trim(uline)
   uline=aline//'      '
   write(stdout,g) len_trim(uline)

   uline=[32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288]
   write(stdout,g)'spaces:',character(uline),len(uline),len_trim(uline)

   uline=[32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288]
   uline=trim(uline)
   write(stdout,g)'trim:','[',character(uline),']'

   uline='    this  is just a    string        '
   write(stdout,g)'adjustr:','[',character(uline),'] ==> [',character(adjustr(uline)),']'
   uline='    this  is just a    string        '
   write(stdout,g)'adjustl:','[',character(uline),'] ==> [',character(adjustl(uline)),']'

   !write(stdout,g)uline
end program assign_exe
