       program demo_keyword_update
       use M_unicode, only : keyword_expand, keyword_update, ch=>character
       character(len=*),parameter :: a='(a)'
          write(*,'(a)') ch(keyword_expand('<clear>TEST CUSTOMIZATIONS:'))
          ! add custom keywords

          call keyword_update('blink',char(27)//'[5m','')
          call keyword_update('/blink',char(27)//'[25m','')
          write(*,*)
          write(*,a) ch(keyword_expand('<blink>Items for Friday</blink>'))

          call keyword_update('ouch',keyword_expand( &
          ' <R><bo><w>BIG mistake!</R></w> '))
          write(*,*)
          write(*,a)ch(keyword_expand('<ouch> Did not see that coming.'))

          write(*,*)
          write(*,a) ch(keyword_expand( &
          'ORIGINALLY: <r>Apple</r>, <b>Sky</b>, <g>Grass</g>'))

          ! delete
          call keyword_update('r')
          call keyword_update('/r')

          ! replace (or create)
          call keyword_update('b','<<<<')
          call keyword_update('/b','>>>>')

          write(*,*)
          write(*,a) ch(keyword_expand( &
          'CUSTOMIZED: <r>Apple</r>, <b>Sky</b>, <g>Grass</g>'))
          write(*,a) ch(keyword_expand('<reset>'))
       end program demo_keyword_update
