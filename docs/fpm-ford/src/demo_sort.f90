   program demo_sort
   use iso_fortran_env, only : stdout => output_unit
   use M_unicode,       only : sort, unicode_type, assignment(=)
   use M_unicode,       only : ut=>unicode_type, write(formatted)
   use M_unicode,       only : ch=>character
   use M_unicode,       only : lower, operator(<)
   implicit none
   character(len=*),parameter :: g='(*(g0,1x))'
   integer,parameter          :: isz=4
   type(unicode_type)         :: rr(isz)
   integer                    :: ii(isz)
   integer                    :: i
      !
      write(stdout,g)'sort array with sort(3f)'
      rr=[ &
       ut("the"),   &
       ut("quick"), &
       ut("brown"), &
       ut("fox") ]
      !
      write(stdout,g)'original order'
      write(stdout,g)ch(rr)
      !
      call sort(rr,ii)
      !
      write(stdout,g)'sorted order'
      ! convert to character
      do i=1,size(rr)
         write(stdout,'(i3.3,1x,a)')i,rr(ii(i))%character()
      enddo
      !
      write(stdout,g)'reorder original'
      rr=rr(ii)
      write(stdout,g)ch(rr)

      print g, 
      write(stdout,g)'input for custom sort'
      rr=[ut("Banana"), ut("apple"), ut("Cherry"), ut("orange")]
      call print_output()

      print g,  'convert to lower and sort (likely creating temporary array)'
      call sort(lower(rr), ii )
      call print_output()
   
      print g,  'Sort using custom rule'
      call sort(rr, ii, comparison_func=case_insensitive_compare)
      call print_output()

   contains

   subroutine print_output()
   character(len=:),allocatable :: output(:)
   integer                      :: i
      output=rr(ii)
      print g, ( output(i),i=1,size(rr) )
      print g, 
   end subroutine print_output

   function case_insensitive_compare(str1, str2) result (answer)
   ! case-insensitive comparison
   type(unicode_type), intent(in) :: str1, str2
   logical                        :: answer
      answer = lower(str1) < lower(str2)
   end function case_insensitive_compare

   end program demo_sort
