      program demo_keyword
      use M_unicode, only : keyword, keyword_mode, keyword_update
      use M_unicode, only : ch=>character

         call printstuff('raw_mode')
         call printstuff('alias_mode')

         write(*,'(a)') 'ADDING A CUSTOM SEQUENCE:'
         call keyword_update('blink',char(27)//'[5m')
         call keyword_update('/blink',char(27)//'[25m')
         write(*,'(a)') ch(keyword('<blink>Items for Friday</blink>'))

      contains
      subroutine printstuff(action)
      character(len=*),intent(in)  :: action
      character(len=:),allocatable :: array(:)

        call keyword_mode(action=action)

        array=[character(len=60) :: &
         'TEST ACTION='//action,    &
         'c=<pi><times>d',          &
         'a=<pi><times>r<S2>',      &
         '<Delta><delta>',          &
         'Copyright<copy>']

        write(*,'(a)') ch(keyword(array))

      end subroutine printstuff
      end program demo_keyword
