program assign_exe
use M_unicode, only : len, len_trim, repeat, trim, adjustr, adjustl
use M_unicode, only : character
use M_unicode, only : assignment(=), unicode_type
character(len=*),parameter   :: g='(*(g0))'
character(len=:),allocatable :: aline
type(unicode_type)           :: uline, substring
character(len=*),parameter   :: smiley='😃'

   aline="Доки не впріти, доти не вміти."

   write(*,g)'123456789012345678901234567890'
   write(*,g)aline
   write(*,g)'length in bytes is: ',len(aline)
   uline=aline
   write(*,g)'length in glyphs is: ',len(uline)

   write(*,g)'string is: ',character(uline) 
   write(*,g)'third word is: ',character(uline,9,14) ! substring

   substring=character(uline,17,29)
   write(*,g)'string is: ',character(substring) 

   uline=repeat(smiley,30)
   write(*,g) character(uline)

   write(*,g) len_trim(uline)
   uline=aline//'      '
   write(*,g) len_trim(uline)

   uline=[32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288]
   write(*,g)'spaces:',character(uline),len(uline),len_trim(uline)

   uline=[32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288]
   uline=trim(uline)
   write(*,g)'trim:','[',character(uline),']'

   uline='    this  is just a    string        '
   write(*,g)'adjustr:','[',character(uline),'] ==> [',character(adjustr(uline)),']'
   uline='    this  is just a    string        '
   write(*,g)'adjustl:','[',character(uline),'] ==> [',character(adjustl(uline)),']'

   !write(*,g)uline
end program assign_exe
