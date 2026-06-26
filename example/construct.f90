program demo_construct
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
use M_unicode
type(unicode_type)             :: ustr
type(unicode_type),allocatable :: uarr(:)
character(len=:),allocatable   :: astr
integer                        :: i
integer,allocatable            :: iarr(:)
character(len=*), parameter    :: all='(*(g0))'
character(len=80), parameter   :: fmts(*)=[character(len=80) :: &
 '(*(g0,1x))'              ,&
 '(*(z0,1x))'              ,&
 "(8('int(z""',z0,'"")',:,','))" ]

   ! Constructors
   ! UNICODE_VARIABLE = CHARACTER(LEN=*)|UNICODE_VARIABLE|INTEGER_ARRAY

   ! Using a character array to define a string array
   uarr= unicode_type([ character(len=80) :: &
   'Confucius never claimed to be a prophet, '       ,&
   'but I think he foresaw AI! He said '             ,&
   ''                                                ,&
   ' "学而不思则罔，思而不学则殆"'                   ,&
   'or'                                              ,&
   ' (xué ér bù sī zé wǎng, sī ér bù xué zé dài),'   ,&
   'which is also'                                   ,&
   ' "To learn without thinking is to be lost, '     ,&
   ' to think without learning is to be in danger".'])

   uarr=trim(uarr) ! trim fixed-length bytes to ragged array

   do i=1,size(uarr) ! show lengths in glyphs
      astr=uarr(i) ! assign string to character
      write(*,'(i3,": ",a)')len(uarr(i)),astr
   enddo
   print *
   ! string to integer array of codepoints
   iarr=uarr(4)%sub(3,10)
   do i=1,3
      print fmts(i), iarr 
   enddo
   print *
   ! assign character variable to string
   ustr= 'Hello World and Ni Hao -- 你好  '

   ! convert to intrinsic CHARACTER variable 
   write (*,all) character(ustr) 
   ! use intrinsic function names on strings
   write (*,all) len(ustr)
   write (*,all) len_trim(ustr)
   write (*,all) index(ustr,'你')

   print all, 'OOPS'
   ! VARIABLE%CHARACTER(start,end,step) returns a CHARACTER string
   ! VARIABLE%BYTE() returns an array of CHARACTER(len=1) values
   ! VARIABLE%CODEPOINT() returns an integer array of Unicode values

   write (*,all)  ustr%character()      ! convert to CHARACTER variable
   write (*,all)  ustr%character(27,28) ! similar to LINE(27:28)
   write (*,all)  ustr%character(len(ustr),1,-1) ! reverse string
   write (*,all)  ustr%byte()           ! convert to CHARACTER(LEN=1) type
   do i=1,size(fmts)
      write (*,fmts(i)) ustr%codepoint(27,28) ! convert to Unicode codepoints
   enddo

end program demo_construct
