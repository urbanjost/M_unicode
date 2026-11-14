   program demo_ctou
   use m_unicode, only : ctou
   implicit none
   !'Noho me ka hau’oli' !(Be happy)
   integer,parameter :: codepoints(*)=[ &
      & 78,111,104,111,&
      & 32,109,101, &
      & 32,107,97, &
      & 32,104,97,117,8217,111,108,105]
   character(len=:),allocatable :: string
   character(len=1),allocatable :: bytes(:)
   character(len=*),parameter   :: space='(*(g0,1x))'
   character(len=*),parameter   :: z='(a,*(z0,1x))'

     write(*,space)'CODEPOINTS:', codepoints
     write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
   !
     string = ctou(codepoints)
     write(*,space)'STRING:', string
   !
     write(*,space)'How long is this string in glyphs? '
     write(*,space)size(codepoints)
   !
     write(*,space)'How long is this string in bytes? '
     write(*,space)len(string)
   !
   end program demo_ctou
