program demo_sort
use iso_fortran_env, only : stdout => output_unit
use M_unicode,       only : sort, unicode_type, assignment(=)
use M_unicode,       only : ut=>unicode_type
implicit none
character(len=*),parameter :: g='(*(g0))'
integer,parameter          :: isz=4
type(unicode_type)         :: rr(isz)
integer                    :: ii(isz)
integer                    :: i
integer                    :: iostat

   ! preferred, but not required if not supported
   open(stdout,encoding='utf-8',iostat=iostat)

   write(stdout,g)'sort array with sort(3f)'
   rr=[ &
    ut("the"),   &
    ut("quick"), &
    ut("brown"), &
    ut("fox") ]

   call sort(rr,ii)
   write(stdout,g)'original order'
   do i=1,size(rr)
      write(stdout,g)rr(i)%character()
   enddo

   write(stdout,g)'sorted order'
   do i=1,size(rr)
      write(stdout,g)rr(ii(i))%character()
   enddo

end program demo_sort
