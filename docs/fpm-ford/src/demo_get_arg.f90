     program demo_get_arg
     use M_unicode, only : get_arg, ut=> unicode_type, ch=>character
     use M_unicode, only : assignment(=), operator(//), write(formatted)
     implicit none
     integer                      :: position
     type(ut)                     :: udefault
     type(ut)                     :: uvalue
     type(ut)                     :: smiley
     integer                      :: ierr
     character(len=*),parameter   :: g='(*(g0))'

     integer                      :: i
     real                         :: r
     doubleprecision              :: d
     logical                      :: tf
     character(len=:),allocatable :: c

     ! basics
     do i=1,command_argument_count()
        write(*,g)i,' ',get_arg(i)
     enddo
     !
     smiley=128515 ! smiley emoji set with Unicode code point
     udefault='Wish I was there '//smiley//'!'
     !
     if(command_argument_count().eq.0)then
        call execute_command_line(get_arg(0)//' one T 100 100.2 300.0e3 ')
     else
        do position=0,command_argument_count()

           write(*,g) repeat('=',50)

           ! returns type(unicode_type)
           uvalue=get_arg(position, udefault)
           write(*,g)uvalue%character()
           ! oop
           write(*,'("OOP:",DT)')udefault%get_arg(position)

           ! returns character which is assigned to type(unicode_type)
           uvalue=get_arg(position, udefault%character() )
           write(*,g)uvalue%character()

           write(*,g)get_arg(position,' character default')

           c=get_arg(position)

           tf=get_arg(position,.false.,ierr=ierr)
           if(ierr.eq.0)write(*,g)position,' ',c,' is a good logical: ',tf

           r=get_arg(position,0.0,ierr=ierr)
           if(ierr.eq.0)write(*,g)position,' ',c,' is a good real: ',r

           d=get_arg(position,0.0d0,ierr=ierr)
           if(ierr.eq.0)write(*,g)position,' ',c,' is a good double: ',d

           i=get_arg(position,0,ierr=ierr)
           if(ierr.eq.0)write(*,g)position,' ',c,' is a good integer: ',i

        enddo
     endif
     end program demo_get_arg
