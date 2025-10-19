   program demo_ichar
   use M_unicode, only : ut=>unicode_type, write(formatted)
   use M_unicode, only : ichar, write(formatted)
   implicit none
   type(ut),allocatable :: lets(:)
   integer,allocatable  :: ilets(:)

      lets=[ut('😃'),ut('🩷'),ut('👣'),ut('🫒'), &
           & ut('🧲'),ut('✔'),ut('🟧'),ut('🟣')]
      write(*,'(*(DT,1x))')lets
      write(*,'(*(g0,1x))')ichar(lets)
      write(*,'(*(z0,1x))')ichar(lets)
   end program demo_ichar
