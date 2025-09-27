program demo_scan
use iso_fortran_env, only : stdout => output_unit
use M_unicode,       only : scan, unicode_type, assignment(=)
use M_unicode,       only : ut=>unicode_type
implicit none
character(len=*),parameter :: g='(*(g0,1x))'
type(ut)                   :: line
type(ut)                   :: set
integer                    :: iostat

   ! preferred, but not required if not supported
   open(stdout,encoding='utf-8',iostat=iostat)

   line='parsley😃sage😃rosemary😃😃thyme'
   set='😃'
   write(stdout,g) '123456789012345678901234567890123456789012345678901234567890'
   write(stdout,g) line%character()
   write(stdout,g) scan(line, set)
   write(stdout,g) scan(line, set, back=.true.)
   write(stdout,g) scan(line, set, back=.false.)
   write(stdout,g) scan(line, unicode_type("NOT"))
   write(stdout,g) 'OOP'
   write(stdout,g) line%scan(set)
   write(stdout,g) line%scan(ut("o"))
end program demo_scan
