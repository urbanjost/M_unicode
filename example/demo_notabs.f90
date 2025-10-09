    program demo_notabs
    use M_unicode, only : notabs, ch=>character, replace
    use M_unicode, only : assignment(=), ut=> unicode_type
    implicit none
    type(ut)                     :: in
    character(len=:),allocatable :: dat
    integer                      :: i
       dat='  this is my string  '
       ! change spaces to tabs to make a sample input
       do i=1,len(dat)
          if(dat(i:i) == ' ')dat(i:i)=char(9)
       enddo
       in=dat
       in=notabs(in)
       write(*,'("[",a,"]")')ch(in)
       in=replace(in,ut(' '),ut('_'))
       write(*,'("[",a,"]")')ch(in)
    end program demo_notabs
