program demo_scan
use M_unicode, only : scan, unicode_type, assignment(=)
use M_unicode, only : ut=>unicode_type
implicit none
character(len=*),parameter :: g='(*(g0,1x))'
type(ut)                   :: line
type(ut)                   :: set
integer                    :: pos
   line='parsley😃sage😃rosemary😃😃thyme'
   set='😃'
   write(*,g) '123456789012345678901234567890123456789012345678901234567890'
   write(*,g) line%character()
   write(*,g) scan(line, set)        
   write(*,g) scan(line, set, back=.true.) 
   write(*,g) scan(line, set, back=.false.) 
   write(*,g) scan(line, unicode_type("NOT")) 
   write(*,g) 'OOP'
   write(*,g) line%scan(set)
   write(*,g) line%scan(ut("o"))
end program demo_scan
