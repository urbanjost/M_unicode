     program demo_tokenize
     use M_unicode, only : tokenize, ut=>unicode_type,ch=>character
     use M_unicode, only : assignment(=),operator(/=)
     implicit none
     !
     ! some useful formats
     character(len=*),parameter ::       &
      & brackets='(*("[",g0,"]":,","))' ,&
      & a_commas='(a,*(g0:,","))'       ,&
      & gen='(*(g0))'
     !
     ! Execution of TOKEN form (return array of tokens)
     !
        block
        type(ut)                   :: string
        type(ut),allocatable       :: tokens(:)
        integer                    :: i
        character(len=*),parameter :: set=' ,'

        ! basics

           call basics( ut(''               ))
           call basics( ut(' '              ))
           call basics( ut('  '             ))
           call basics( ut('G'              ))
           call basics( ut('     G'         ))
           call basics( ut('     G    '     ))
           call basics( ut('     G    e  '  ))
           call basics( ut('G    e'         ))

        ! assigns the value ['first ','second','third ' ] to TOKENS
           string = 'first,second,third'
           call tokenize(string, set, tokens )
           write(*,brackets)ch(tokens)

           string =    'first,second,,fourth'
           call tokenize(string, set, tokens )
           write(*,brackets)ch(tokens)

           string =    'first,second,,,fifth'
           call tokenize(string, set, tokens )
           write(*,brackets)ch(tokens)

           string = '  first second  third       '
           write(*,gen)'Parse on spaces ...'
           call tokenize(string, set=' ', tokens=tokens )
           write(*,brackets)ch(tokens)

           write(*,gen)'Parse on semicolons and commas ...'
           string = '  first,second ,third       '
           call tokenize(string, set=';,', tokens=tokens )
           write(*,brackets)ch(tokens)

           string = '  first , second ,third       '
           call tokenize(string, set=' ,', tokens=tokens )
           write(*,brackets)(tokens(i)%character(),i=1,size(tokens))

           ! remove blank tokens
           tokens=pack(tokens, tokens /= '' )
           write(*,brackets)ch(tokens)
     !
        endblock
     !
     ! Execution of BOUNDS form (return position of tokens)
     !
        block
        type(ut)                   :: string
        character(len=*),parameter :: set = " ,"
        integer,allocatable        :: first(:), last(:)

           write(*,gen)repeat('1234567890',6)

           string = 'first,second,,fourth'
           write(*,gen)ch(string)

           call tokenize (string, set, first, last)
           write(*,a_commas)'FIRST=',first
           write(*,a_commas)'LAST=',last
           write(*,a_commas)'HAS LENGTH=',last-first.gt.0

        endblock
        contains
        subroutine basics(string)
        type(ut),intent(in)  :: string
        type(ut),allocatable :: tokens(:)
           call tokenize(string,' ', tokens )
           write(*,brackets)string%character(),"<==>",ch(tokens)
        end subroutine basics
     !
     end program demo_tokenize
