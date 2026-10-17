program demo_sort
use iso_fortran_env, only : stdout => output_unit
use M_unicode,       only : unicode_type
use M_unicode,       only : ut=>unicode_type, write(formatted)
use M_unicode,       only : ch=>character
use M_unicode,       only : lower, sort, expand_backslash
use M_unicode,       only : operator(<), assignment(=), operator(==)
implicit none
character(len=*),parameter :: g='(*(g0,1x))', g0='(g0)'
type(unicode_type),allocatable :: rr(:)
type(unicode_type),allocatable :: answer(:)
integer,allocatable            :: ii(:)
integer                        :: i
   !
   print g,'sort array with sort(3f)'
   rr=[ &
      ut("the"),   &
      ut("quick"), &
      ut("brown"), &
      ut("fox") ]
   allocate(ii(size(rr)))
   !
   print g,'original order'
   print g,ch(rr)
   !
   call sort(rr,ii)
   !
   print g,'sorted order'
   ! convert to character
   do i=1,size(rr)
      write(stdout,'(i3.3,1x,a)')i,rr(ii(i))%character()
   enddo
   !
   print g,'reorder original'
   rr=rr(ii)
   print g,ch(rr)

   print g,
   print g,'input for custom sort'
   rr=[ut("Banana"), ut("apple"), ut("Cherry"), ut("orange")]
   call print_output()

   print g,  'convert to lower and sort (likely creating temp array)'
   call sort(lower(rr), ii )
   call print_output()

   print g,  'Sort ignoring case by using custom rule'
   call sort(rr, ii, comparison_func=case_insensitive_compare)
   call print_output()

   print g,  'Sort in German dictionary order using custom rule'
   ! A list of German words designed to test a dictionary
   rr=[ &
      ut("Band"),        & ! (Standard B)
      ut("Buße"),        & ! (Contains ß — often sorted as 'ss')
      ut("Öl"),          & ! (Umlaut Ö at the start)
      ut("Ufer"),        & ! (Standard U)
      ut("Apfel"),       & ! (Standard A)
      ut("Oase"),        & ! (Standard O)
      ut("Bus"),         & ! (Standard B)
      ut("Möve"),        & ! (Umlaut Ö)
      ut("Bär"),         & ! (Umlaut Ä in the middle)
      ut("Zebra"),       & ! (Standard Z)
      ut("Mutter"),      & ! (Standard M)
      ut("Bussard"),     & ! (Standard double 's' comparison with ß)
      ut("Äpfel"),       & ! (Umlaut Ä )
      ut("Mond"),        & ! (Standard M)
      ut("Mütter"),      & ! (Umlaut Ü)
      ut("Übung"),       & ! (Umlaut Ü at the start)
      ut("F\xE4hnlein"), &   ! (Same word using Unicode codepoint
      ut("Fa\u0308hnlein") ] ! and Latin letter followed by
                             ! combining dieresis modifier U+0308.
   !
   rr=expand_backslash(rr) ! convert escape codes to UTF-8

   if(allocated(ii))deallocate(ii)  ! create index array
   allocate(ii(size(rr)))
   !
   print g,'original order'
   print g, ( trim(rr(i)%ch()),i=1,size(rr) )
   print g,
   !
   print g,  'Sort using default Unicode rule for comparison'
   call sort(rr, ii)
   call print_output()
   !
   print g,  'Sort using custom German dictionary order'
   call sort(rr, ii, comparison_func=german_dictionary)
   call print_output()

   print g,  'Correct Order'
   answer=[ &
      ut("Apfel"),     &
      ut("Äpfel"),     &
      ut("Band"),      &
      ut("Bär"),       &
      ut("Bus"),       &
      ut("Bussard"),   &
      ut("Buße"),      &
      ut("Fähnlein"),  &
      ut("Fähnlein"),  &
      ut("Mond"),      &
      ut("Möve"),      &
      ut("Mutter"),    &
      ut("Mütter"),    &
      ut("Oase"),      &
      ut("Öl"),        &
      ut("Ufer"),      &
      ut("Übung"),     &
      ut("Zebra")      ]
   print g, ( trim(answer(i)%ch()),i=1,size(answer) )
   print g, 'CHECK TEST WORDS:',rr(ii).eq.answer
   print g, merge('PAssED','FAILED', all(rr(ii).eq.answer))

contains

   subroutine print_output()
   character(len=:),allocatable :: output(:)
   integer                      :: i
      output=rr(ii)
      print g, ( trim(output(i)),i=1,size(output) )
      print g,
   end subroutine print_output

   function case_insensitive_compare(str1, str2) result (answer)
   ! case-insensitive comparison
   type(unicode_type), intent(in) :: str1, str2
   logical                        :: answer
      answer = lower(str1) < lower(str2)
   end function case_insensitive_compare
!
! German dictionaries follow specific guidelines
! for sorting umlauts (ä, ö, ü) and the eszett (β).
!
! Standard Dictionary Rules (DIN 5007-1, Variant 1)
!
! In traditional German dictionaries, words with umlauts are treated as
! their base vowels and the letter β is treated as ss. That is ...
!
!     Amlausts:
!         ä is sorted as a
!         ö is sorted as o
!         ü is sorted as u
!     Eszett:
!         β is sorted as ss
!
!  along with a similar exchange for their uppercase cases.
!
!  If normalized words are identical, the umlaut comes second.
!
!  The multibyte characters with an umlaut may be a single Unicode
!  codepoint or the associated Latin character followed by the
!  combining dieresis modifier U+0308.
!
! Note for reference that depending on the context (such as phone books
! or digital systems), other official standards apply:
!
!   + Phone Book Rules (DIN 5007-1, Variant 2): Umlauts are expanded by
!     adding an e after the vowel (ä = ae, ö = oe, ü = ue). As in Variant
!     1 the letter β is treated as ss.
!
!   + Strict / Character-by-Character Order: Some systems or older indexes
!     place umlauts right after their base letter as separate characters
!     (a, ä, b, c...).
!
function german_dictionary(lhs, rhs) result(is_lle)
!
! @(#) Lexically compare two strings using German dictionary order
!
type(ut), intent(in) :: lhs, rhs
logical              :: is_lle

   is_lle = nmi(lhs) < nmi(rhs)

end function german_dictionary

function nmi(in) result (out)
type(ut),intent(in)  :: in
type(ut)             :: out
integer,allocatable  :: codes(:)
integer              :: i, j, ikeep
integer,parameter    :: comb=2*int(z'308'), &
 &  um_lower_a    =  2*int(z'E4'),       &
 &  um_lower_o    =  2*int(z'F6'),       &
 &  um_lower_u    =  2*int(z'FC'),       &
 &  um_upper_a    =  2*int(z'C4'),       &
 &  um_upper_o    =  2*int(z'D6'),       &
 &  um_upper_u    =  2*int(z'DC'),       &
 &  lower_a       =  2*ichar('a')+1,     &
 &  lower_o       =  2*ichar('o')+1,     &
 &  lower_u       =  2*ichar('u')+1,     &
 &  upper_a       =  2*ichar('A')+1,     &
 &  upper_o       =  2*ichar('O')+1,     &
 &  upper_u       =  2*ichar('U')+1,     &
 &  lower_eszett  =  2*int(z'DF'),       &
 &  upper_eszett  =  2*int(z'1E9E'),     &
 &  lower_s       =  2*int(ichar('s')),  &
 &  upper_s       =  2*int(ichar('S'))

   ! if every letter is a eszett and first letter is COMB
   ! (which should not happen) longest index required would be
   allocate(codes(0:2*size(in%codes)))

   ikeep=0
   do concurrent ( i=1:size(in%codes) )
      if(in%codes(i).eq.comb)then
         codes(ikeep)=codes(ikeep)+1
      else
         ikeep=ikeep+1
         codes(ikeep)=in%codes(i)*2
         select case(codes(ikeep))
          case(:127*2)
          case(um_lower_a); codes(ikeep)=lower_a
          case(um_lower_o); codes(ikeep)=lower_o
          case(um_lower_u); codes(ikeep)=lower_u
          case(um_upper_a); codes(ikeep)=upper_a
          case(um_upper_o); codes(ikeep)=upper_o
          case(um_upper_u); codes(ikeep)=upper_u
          case(lower_eszett)
             codes(ikeep:ikeep+1)=lower_s
             ikeep=ikeep+1
          case(upper_eszett)
             codes(ikeep:ikeep+1)=upper_s
             ikeep=ikeep+1
         end select
      endif
   enddo
   ! codes are no longer valid Unicode codepoints for the input but
   ! can be used to compare lexically to another normalized value
   ! only because needed some method of telling if an A,O,U used to
   ! have an umlaut
   out=codes(1:ikeep)

end function nmi

end program demo_sort
