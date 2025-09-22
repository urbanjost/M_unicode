program demo_sort
use M_unicode, only : sort, unicode_type, assignment(=)
use M_unicode, only : ut=>unicode_type
implicit none
character(len=*),parameter :: g='(*(g0))'
integer,parameter  :: isz=4
type(unicode_type) :: rr(isz)
integer            :: ii(isz)
integer            :: i
   write(*,g)'sort array with sort(3f)'
   rr=[ &
    ut("the"),   &
    ut("quick"), &
    ut("brown"), &
    ut("fox") ]

   call sort(rr,ii)
   write(*,g)'original order'
   do i=1,size(rr)
      write(*,g)rr(i)%character()
   enddo
   write(*,g)'sorted order'
   do i=1,size(rr)
      write(*,g)rr(ii(i))%character()
   enddo
end program demo_sort
