program testit
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit

! user-defined type to hold Unicode text
use M_unicode, only : unicode_type

! convert unicode_type to CHARACTER variables
use M_unicode, only : character

! intrinsic overloads
use M_unicode, only : adjustl, adjustr
use M_unicode, only : trim, len, len_trim
use M_unicode, only : index, scan, verify
use M_unicode, only : repeat
use M_unicode, only : split, tokenize
use M_unicode, only : upper, lower
use M_unicode, only : sort

! operators (and overloads) and assignment
use M_unicode, only : assignment(=)
use M_unicode, only : operator(<=), lle
use M_unicode, only : operator(<),  llt
use M_unicode, only : operator(/=), lne
use M_unicode, only : operator(==), leq
use M_unicode, only : operator(>),  lgt
use M_unicode, only : operator(>=), lge
use M_unicode, only : operator(//)

! low-level text conversion to integer codepoint arrays:
use M_unicode, only : utf8_to_codepoints, codepoints_to_utf8

! example usage:
implicit none
type(unicode_type)          :: ustr
character(len=*), parameter :: g='(*(g0))'
character(len=*), parameter :: gx='(*(g0,1x))'
character(len=*), parameter :: gh='(*(z0,1x))'
integer                     :: iostat

   ! preferred, but not required if not supported
   !open(stdout,encoding='utf-8',iostat=iostat) 

   ! Constructors
   ! UNICODE_VARIABLE= UNICODE_VARIABLE|CHARACTER(LEN=*)|INTEGER_ARRAY
   !
   ! VARIABLE%CHARACTER(start,end,step) returns a CHARACTER string
   ! VARIABLE%BYTE() returns an array of CHARACTER(len=1) values

   ustr= 'Hello World and Ni Hao -- 你好  '

   write (stdout,g) character(ustr) ! convert to intrinsic CHARACTER variable
   write (stdout,g) len(ustr)
   write (stdout,g) len_trim(ustr)
   write (stdout,g) index(ustr,'你')

   ! OOPS
   write (stdout,g)  ustr%character()      ! convert to CHARACTER variable
   write (stdout,g)  ustr%character(27,28) ! similar to LINE(27:28) for CHARACTER
   write (stdout,g)  ustr%character(len(ustr),1,-1) ! reverse string
   write (stdout,g)  ustr%byte()           ! convert to CHARACTER(LEN=1) type
   ! print 
   write (stdout,gx) ustr%codepoint()      ! convert to Unicode codepoints
   write (stdout,gh) ustr%codepoint()      ! convert to Unicode codepoints

end program testit
