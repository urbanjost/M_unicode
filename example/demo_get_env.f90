program demo_get_env
use M_unicode, only : get_env, ut => unicode_type
use M_unicode, only : assignment(=), operator(//)
implicit none
type(ut) :: name
type(ut) :: default
type(ut) :: value
type(ut) :: smiley
integer  :: i
character(len=*),parameter :: bracket = '(1x,*("[",a,"]",:))'

   smiley = 128515 ! set with Unicode code point
   default = 'Have a nice day '//smiley//'!' ! set with unicode_type
   
   do i = 1,2
      select case (i)
      case (1); name = 'HOME'
      case (2); name = 'UNKNOWN'
      end select
         !
         ! arguments can be type(unicode_type) or character
         ! but type(unicode_type) is always returned
      value = get_env(name,             default             )
      write(*,*)value%character()
   
      value = get_env(name%character(), default%character() )
      write(*,*)value%character()
   
      value = get_env(name,             default%character() )
      write(*,*)value%character()
   
      value = get_env(name%character(), default             )
      write(*,*)value%character()
   
      value = get_env(name)
      write(*,*)value%character()
   
      value = get_env(name%character())
      write(*,*)value%character()
   
   enddo

end program demo_get_env
