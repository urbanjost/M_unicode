   program demo_ichar
   use M_unicode, only : assignment(=),ch=>character
   use M_unicode, only : ut=>unicode_type, write(formatted)
   use M_unicode, only : ichar, escape
   implicit none
   type(ut),allocatable :: lets(:)
   integer,allocatable  :: ilets(:)

      lets=[ut('😃'),ut('🩷'),ut('👣'),ut('🫒'), &
           & ut('🧲'),ut('✔'),ut('🟧'),ut('🟣')]
      write(*,'(*(DT,1x))')lets
      ilets=ichar(lets)
      write(*,'(*(g0,1x))')ilets
      write(*,'(*(z0,1x))')ilets

      ! alternatively define LETS with escape codes
      if(allocated(lets))deallocate(lets)
      allocate(lets(8)) ! gfortran bug
      lets=['\U0001F603','\U0001FA77','\U0001F463','\U0001FAD2', &
          & '\U0001F9F2','\U00002714','\U0001F7E7','\U0001F7E3']
      lets=escape(lets)
      ! write as an array
      write(*,'(*(a,1x))')ch(lets)
      write(*,'(*(z0,1x))')ichar(lets)
      ! OOPS
      write(*,'(*(z0,1x))')lets%ichar()
   end program demo_ichar
