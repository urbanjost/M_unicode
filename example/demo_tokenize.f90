program demo_tokenize
use M_unicode, only : assignment(=), unicode_type, tokenize, len, character
character(len=*),parameter       :: g0='(*(g0))'
character(len=*),parameter       :: g1='(*(g0,1x))'
type(unicode_type),allocatable   :: tokens(:)
type(unicode_type),allocatable   :: separators(:)
type(unicode_type)               :: delims
type(unicode_type)               :: herbs
integer,allocatable,dimension(:) :: begins
integer,allocatable,dimension(:) :: ends
integer                          :: i

   delims = ',&'
   herbs  = 'parsley,sage,rosemary,&thyme'
   write(*,*)herbs%character()

   write(*,g0)
   
   write(*,g0)'expecting'
   write(*,g0)' tokens    =[parsley][sage][rosemary][][thyme]'
   write(*,g0)' separators=,,,&'
   CALL TOKENIZE (herbs, delims, tokens, separators)
   write(*,g0)'got'
   write(*,g0)" tokens    =",('['//tokens(i)%character(),']',i=1,size(tokens))
   write(*,g0)" separators=",(separators(i)%character(),i=1,size(separators))

   write(*,g0)
   
   write(*,g0)'expecting'
   write(*,*)'begins=',[1, 9, 14, 23, 24]
   write(*,*)'ends=  ',[7, 12, 21, 22, 28]
   CALL TOKENIZE (herbs, delims, begins, ends)
   write(*,g0)'got'
   write(*,*)'begins=',begins
   write(*,*)'ends=  ',ends

   write(*,g0)

   write(*,g0)'OOP'
   tokens=herbs%tokenize(delims)
   write(*,g0)" tokens    =",('['//tokens(i)%character(),']',i=1,size(tokens))
   herbs='parsley/sage/rosemary//thyme'
   delims='/'
   tokens=herbs%tokenize(delims)
   write(*,g0)" tokens    =",('['//tokens(i)%character(),']',i=1,size(tokens))
   herbs='parsley😃sage😃rosemary😃😃thyme'
   delims='😃'
   write(*,g0)' ',delims%character()
   write(*,g1)' ',delims%codepoint()
   tokens=herbs%tokenize(delims)
   write(*,g0)" tokens    =",('['//tokens(i)%character(),']',i=1,size(tokens))


end program demo_tokenize
