   program demo_utoc
   use m_unicode, only : utoc
   implicit none
   character(len=*),parameter   :: g='(*(g0,1x))'
   character(len=*),parameter   :: string = 'Noho me ka hau’oli'
   integer,allocatable          :: codepoints(:)
   integer,parameter            :: answer(*) = [ & !(Be happy)
      & 78,111,104,111,&
      & 32,109,101, &
      & 32,107,97, &
      & 32,104,97,117,8217,111,108,105]

      write(*,g) 'STRING:', string
      codepoints = utoc(string)
      write(*,g) 'CODEPOINTS:', codepoints
      write(*,g) 'length in glyphs ', size(codepoints)
      write(*,g) 'length in bytes  ', len(string)
   end program demo_utoc
