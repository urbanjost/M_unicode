   program fortran_symbol_name
   use M_unicode, only : ut=>unicode_type, trim, verify, len
   use M_unicode, only : ch=>character
   use M_unicode, only : write(formatted)
   implicit none
   integer :: i
   type(ut),allocatable :: symbols(:)
      symbols=[ &
       ut('A_'), ut('10'), ut('a10'), ut('September'), ut('A B'), &
       ut('_A'), ut(' ')]

      do i=1,size(symbols)
         write(*,'(1x,DT,T11,"|",l2)')symbols(i),fortran_name(symbols(i))
      enddo

   contains

   impure elemental function fortran_name(line) result (lout)
   !
   ! determine if a string is a valid Fortran name
   ! ignoring trailing spaces (but not leading spaces)
   !
   character(len=*),parameter :: int="0123456789"
   character(len=*),parameter :: lower="abcdefghijklmnopqrstuvwxyz"
   character(len=*),parameter :: upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   character(len=*),parameter :: allowed=upper//lower//int//"_"

   type(ut),intent(in)        :: line
   type(ut)                   :: name
   logical                    :: lout
      name=trim(line)
      if(len(name).ne.0)then
         ! first character is alphameric
         lout = verify(name%sub(1,1), lower//upper) == 0  &
          ! other characters are allowed in a symbol name
          & .and. verify(name,allowed) == 0           &
          ! allowable length
          & .and. len(name) <= 63
      else
         lout = .false.
      endif
   end function fortran_name

   end program fortran_symbol_name
