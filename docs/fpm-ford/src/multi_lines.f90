program multi_line
use iso_fortran_env, only : stdout => output_unit
use M_unicode
use M_unicode, only : ut=>unicode_type
implicit none
integer              :: i, longest
type(ut),allocatable :: upagain(:)

upagain=[ &                                ! ROMANIZATION                         ! ENGLISH
 ut("七転び八起き。"), &                   ! Nanakorobi yaoki.                    ! Fall seven times, stand up eight.
 ut("転んでもまた立ち上がる。"), &         ! Koronde mo mata tachiagaru.          ! Even if you fall down, you will get up again.
 ut("くじけずに前を向いて歩いていこう。")] ! Kujikezu ni mae o muite aruite ikou. ! Don't be discouraged, just keep walking forward.
!
   longest=0 ! get longest trimmed line
   do i=1,size(upagain)
      longest=max(longest,len_trim(upagain(i)))
      write(stdout,*)'LEN=',len_trim(upagain(i))
   enddo
   write(stdout,*)'longest=',longest

   write(stdout,*)
   do i=1,size(upagain)
      write(stdout,'(g0)')character(upagain(i)),len_trim(upagain(i))
   enddo

end program multi_line
