program demo_tokenize
use iso_fortran_env, only : stdout => output_unit
use M_unicode,       only : assignment(=), unicode_type, tokenize, len, character
implicit none
character(len=*),parameter       :: g0='(*(g0))'
character(len=*),parameter       :: g1='(*(g0,1x))'
type(unicode_type),allocatable   :: tokens(:)
type(unicode_type),allocatable   :: separators(:)
type(unicode_type)               :: delims
type(unicode_type)               :: herbs
integer,allocatable,dimension(:) :: begins
integer,allocatable,dimension(:) :: ends
integer                          :: i
integer                          :: iostat

   ! preferred, but not required if not supported
   !open(stdout,encoding='utf-8',iostat=iostat)

   delims = ',&'
   herbs  = 'parsley,sage,rosemary,&thyme'
   write(stdout,*)herbs%character()

   write(stdout,g0)

   write(stdout,g0)'expecting'
   write(stdout,g0)' tokens    =[parsley][sage][rosemary][][thyme]'
   write(stdout,g0)' separators=,,,&'
   CALL TOKENIZE (herbs, delims, tokens, separators)
   write(stdout,g0)'got'
   write(stdout,g0)" tokens    =",('['//tokens(i)%character(),']',i=1,size(tokens))
   write(stdout,g0)" separators=",(separators(i)%character(),i=1,size(separators))

   write(stdout,g0)

   write(stdout,g0)'expecting'
   write(stdout,*)'begins=',[1, 9, 14, 23, 24]
   write(stdout,*)'ends=  ',[7, 12, 21, 22, 28]
   CALL TOKENIZE (herbs, delims, begins, ends)
   write(stdout,g0)'got'
   write(stdout,*)'begins=',begins
   write(stdout,*)'ends=  ',ends

   write(stdout,g0)

   write(stdout,g0)'OOP'
   tokens=herbs%tokenize(delims)
   write(stdout,g0)" tokens    =",('['//tokens(i)%character(),']',i=1,size(tokens))
   herbs='parsley/sage/rosemary//thyme'
   delims='/'
   tokens=herbs%tokenize(delims)
   write(stdout,g0)" tokens    =",('['//tokens(i)%character(),']',i=1,size(tokens))
   herbs='parsley😃sage😃rosemary😃😃thyme'
   delims='😃'
   write(stdout,g0)' ',delims%character()
   write(stdout,g1)' ',delims%codepoint()
   tokens=herbs%tokenize(delims)
   write(stdout,g0)" tokens    =",('['//tokens(i)%character(),']',i=1,size(tokens))


end program demo_tokenize
