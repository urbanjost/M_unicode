program testit
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
use M_unicode
type(unicode_type)          :: ustr
character(len=*), parameter :: g='(*(g0))'
character(len=*), parameter :: gx='(*(g0,1x))'
character(len=*), parameter :: gh='(*(z0,1x))'
character(len=*), parameter :: gz="(8('int(z""',z0,'"")',:,','))"

   ! preferred, but not required if not supported

   ! Constructors
   ! UNICODE_VARIABLE = CHARACTER(LEN=*)|INTEGER_ARRAY
   !
   ! VARIABLE%CHARACTER(start,end,step) returns a CHARACTER string
   ! VARIABLE%BYTE() returns an array of CHARACTER(len=1) values
   ! VARIABLE%CODEPOINT() returns an integer array of Unicode values

   ustr= 'Hello World and Ni Hao -- 你好  '

   write (stdout,g) character(ustr) ! convert to intrinsic CHARACTER variable
   write (stdout,g) len(ustr)
   write (stdout,g) len_trim(ustr)
   write (stdout,g) index(ustr,'你')

   ! OOPS
   write (stdout,g)  ustr%character()      ! convert to CHARACTER variable
   write (stdout,g)  ustr%character(27,28) ! similar to LINE(27:28)
   write (stdout,g)  ustr%character(len(ustr),1,-1) ! reverse string
   write (stdout,g)  ustr%byte()           ! convert to CHARACTER(LEN=1) type
   write (stdout,gx) ustr%codepoint()      ! convert to Unicode codepoints

end program testit
