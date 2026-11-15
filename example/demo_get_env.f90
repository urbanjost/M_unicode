   program demo_get_env
   use M_unicode, only : get_env, get_arg
   use M_unicode, only : ut => unicode_type
   use M_unicode, only : assignment(=), operator(//)
   implicit none
   character(len=*),parameter   :: g='(*(g0))'
   integer                      :: ierr
   character(len=:),allocatable :: HOME
   type(ut)                     :: uname
   type(ut)                     :: udefault
   type(ut)                     :: uvalue
   type(ut)                     :: smiley
   integer                      :: i

     smiley = 128515 ! set with Unicode code point
     udefault = 'Have a nice day '//smiley//'!' 

     !Basics
      HOME=get_env('HOME','UNKNOWN')
      write(*,'(a)')HOME
      write(*,'(a)')Get_env('PATH')

     ! arguments can be type(unicode_type) or character
     ! but type(unicode_type) is always returned if either is
     ! type(unicode_type)
      do i = 1,2
         select case (i)
         case (1); uname = 'HOME'
         case (2); uname = 'UNKNOWN'
         end select

           uvalue = get_env(uname,             udefault             )
           write(*,*)uvalue%character()

           uvalue = get_env(uname%character(), udefault%character() )
           write(*,*)uvalue%character()

           uvalue = get_env(uname,             udefault%character() )
           write(*,*)uvalue%character()

           uvalue = get_env(uname%character(), udefault             )
           write(*,*)uvalue%character()

           uvalue = get_env(uname)
           write(*,*)uvalue%character()

           uvalue = get_env(uname%character())
           write(*,*)uvalue%character()

        enddo

     !call this program setting enviroment variable 
     ! STOP=RUN unless STOP=RUN is already set

     !otherwise print various environment variable values
     !converted to various types

      if(get_env('STOP').eq.'RUN')then
         write(*,g)repeat('-',80)
         write(*,g)get_env('CHARACTER','string')
         write(*,g)get_env('INTEGER',100)
         write(*,g)get_env('REAL',200.0)
         write(*,g)get_env('DOUBLE',300.0d0)
         write(*,g)get_env('LOGICAL',.true.)

         write(*,g)repeat('-',80)
         write(*,g)get_env('CHARACTER','string',ierr=ierr)
         write(*,*)'ierr=',ierr
         write(*,g)get_env('INTEGER',100,ierr=ierr)
         write(*,*)'ierr=',ierr
         write(*,g)get_env('REAL',200.0,ierr=ierr)
         write(*,*)'ierr=',ierr
         write(*,g)get_env('DOUBLE',300.0d0,ierr=ierr)
         write(*,*)'ierr=',ierr
         write(*,g)get_env('LOGICAL',.true.)
         write(*,*)'ierr=',ierr

         write(*,g)repeat('-',80)
         write(*,g)get_env('CHARACTER')
         write(*,g)get_env('HOME')
       else
         write(*,g)repeat('-',80)
         call execute_command_line('env STOP=RUN '//get_arg(0))
         call execute_command_line('env STOP=RUN CHARACTER=aaaa &
         & INTEGER=1 REAL=2.3 DOUBLE=444444444444 '//get_arg(0))
         call execute_command_line('env STOP=RUN CHARACTER=bbbb &
         & INTEGER=1 REAL=2.3 DOUBLE=44.555 '//get_arg(0))
         call execute_command_line('env STOP=RUN CHARACTER=cccc &
         & INTEGER=asdf REAL=asdf DOUBLE=adsf '//get_arg(0))
         write(*,g)repeat('-',80)
         stop
      endif

   end program demo_get_env
