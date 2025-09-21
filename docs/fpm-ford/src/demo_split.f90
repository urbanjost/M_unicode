program demo_split
use M_unicode, only : split, unicode_type, assignment(=), len, character
use M_unicode, only : ut=>unicode_type
implicit none
character(len=*),parameter :: g='(*(g0,1x))'
type(ut)                   :: proverb
type(ut)                   :: delims
type(ut),allocatable       :: array(:)
type(ut)                   :: word
integer                    :: first
integer                    :: last
integer                    :: pos
integer                    :: i

   delims= '=|; '

   proverb="Más vale pájaro en mano, que ciento volando."
   call printwords(proverb)

   ! there really are not spaces between these glyphs
   array=[ &                                
    ut("七転び八起き。"), &                   
    ut("転んでもまた立ち上がる。"), &         
    ut("くじけずに前を向いて歩いていこう。&
    ")] 
   call printwords(array)

   write(*,g)'OOP'
   array=proverb%split(ut(' '))
   write(*,'(*(:"[",a,"]"))')(character(array(i)),i=1,size(array))

contains
impure elemental subroutine printwords(line)
type(ut),intent(in) :: line
   pos = 0
   write(*,g)line%character(),len(line)
   do while (pos < len(line))
       first = pos + 1
       call split (line, delims, pos)
       last = pos - 1
       print g, line%character(first,last),first,last,pos
   end do
end subroutine printwords

end program demo_split

