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
     character(len=*),parameter   :: g='(*(g0,1x))'
     character(len=*),parameter   :: z='(a,*(z0,1x))'

       write(*,g)'CODEPOINTS:', codepoints
       write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
       string = ctou(codepoints)
       write(*,g)'STRING:', string
       write(*,g)'length in glyphs ', size(codepoints)
       write(*,g)'length in bytes  ', len(string)

     end program demo_ctou
