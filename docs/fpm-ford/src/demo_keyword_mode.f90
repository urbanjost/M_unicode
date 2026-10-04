      program demo_keyword_mode
      use M_unicode, only : keyword, keyword_mode, character
      implicit none
      character(len=:),allocatable :: lines(:)
      character(len=:),allocatable :: outlines(:)
      integer :: i
         lines=[character(len=110):: &
         &'<delta>',   &
         &'<E><g> c=<pi><times>d </g></E>',  &
         &'<omega>',   &
         &' ']

         outlines=character(keyword(lines))
         write(*,'(a)')(trim(outlines(i)),i=1,size(outlines))

         call keyword_mode(action='raw_mode')   ! write as-is
         write(*,'(a)')character(keyword(lines))

         call keyword_mode(action='plain_mode')  ! return to default mode
         write(*,'(a)')character(keyword(lines))

         call keyword_mode(action='alias_mode')  ! return to default mode
         write(*,'(a)')character(keyword(lines))

         call keyword_mode(action='dump')

      end program demo_keyword_mode
