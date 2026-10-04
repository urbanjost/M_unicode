       program demo_keyword_update
       use M_unicode, only : keyword, keyword_update, ch=>character
          write(*,'(a)') ch(keyword('<clear>TEST CUSTOMIZATIONS:'))
          ! add custom keywords

          call keyword_update('blink',char(27)//'[5m','')
          call keyword_update('/blink',char(27)//'[25m','')
          write(*,*)
          write(*,'(a)') ch(keyword('<blink>Items for Friday</blink>'))

          call keyword_update('ouch',keyword( &
          ' <R><bo><w>BIG mistake!</R></w> '))
          write(*,*)
          write(*,'(a)') ch(keyword('<ouch> Did not see that coming.'))

          write(*,*)
          write(*,'(a)') ch(keyword( &
          'ORIGINALLY: <r>Apple</r>, <b>Sky</b>, <g>Grass</g>'))

          ! delete
          call keyword_update('r')
          call keyword_update('/r')

          ! replace (or create)
          call keyword_update('b','<<<<')
          call keyword_update('/b','>>>>')

          write(*,*)
          write(*,'(a)') ch(keyword( &
          'CUSTOMIZED: <r>Apple</r>, <b>Sky</b>, <g>Grass</g>'))
          write(*,'(a)') ch(keyword('<reset>'))
       end program demo_keyword_update
