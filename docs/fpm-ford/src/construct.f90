program demo_construct
use M_unicode
type(unicode_type)           :: ustr
character(len=*), parameter  :: g='(*(g0))'
character(len=80), parameter :: fmts(*)=[character(len=80) :: &
 '(*(g0,1x))'              ,&
 '(*(z0,1x))'              ,&
 "(8('int(z""',z0,'"")',:,','))" ]
integer                      :: i

   ! Constructors
   ! UNICODE_VARIABLE = CHARACTER(LEN=*)|UNICODE_VARIABLE|INTEGER_ARRAY

   ustr= 'Hello World and Ni Hao -- 你好  '

   write (*,g) character(ustr) ! convert to intrinsic CHARACTER variable
   write (*,g) len(ustr)
   write (*,g) len_trim(ustr)
   write (*,g) index(ustr,'你')

   ! OOPS
   ! VARIABLE%CHARACTER(start,end,step) returns a CHARACTER string
   ! VARIABLE%BYTE() returns an array of CHARACTER(len=1) values
   ! VARIABLE%CODEPOINT() returns an integer array of Unicode values

   write (*,g)  ustr%character()      ! convert to CHARACTER variable
   write (*,g)  ustr%character(27,28) ! similar to LINE(27:28)
   write (*,g)  ustr%character(len(ustr),1,-1) ! reverse string
   write (*,g)  ustr%byte()           ! convert to CHARACTER(LEN=1) type
   do i=1,size(fmts)
      write (*,fmts(i)) ustr%codepoint(27,28) ! convert to Unicode codepoints
   enddo

end program demo_construct
