program demo_sort
   use iso_fortran_env, only : stdout => output_unit
   use M_unicode,       only : ut=>unicode_type, write(formatted)
   use M_unicode,       only : ch=>character
   use M_unicode,       only : lower, expand_backslash, sort, trim, len
   use M_unicode,       only : operator(<), assignment(=), operator(==)
   implicit none
   character(len=*),parameter :: g='(*(g0,1x))'
   type(ut),allocatable       :: rr(:)
   type(ut),allocatable       :: answer(:)
   integer,allocatable        :: ii(:)
   integer                    :: i
      !
      write(stdout,g)'sort array with sort(3f)'
      print g
    ! Word List for German Sort Test:
    !
    ! A list of German words designed to test a dictionary
    ! sorting algorithm. It includes standard letters alongside all three
    ! umlaut variations (ä, ö, ü) and the ligature ß (Eszett / "sharp S").
!
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
       ut("Bussard"),     & ! (Standard double 's' for comparison with ß)
       ut("Äpfel"),       & ! (Umlaut Ä — often sorted either right after 'A' or treated as 'Ae')
       ut("Mond"),        & ! (Standard M)
       ut("Mütter"),      & ! (Umlaut Ü)
       ut("Übung"),       & ! (Umlaut Ü at the start)
       ut("F\xE4hnlein"), & ! (Same word using Unicode codepoint and Latin letter followed by combining dieresis modifier U+0308.
       ut("Fa\u0308hnlein") ]
      !
      rr=expand_backslash(rr) ! convert escape codes to UTF-8

      if(allocated(ii))deallocate(ii)  ! create index array
      allocate(ii(size(rr)))
      !
      write(stdout,g)'original order'
      print g, ( trim(rr(i)%ch()),i=1,size(rr) )
      print g
      !
      print g,  'Sort using default Unicode rule for comparison'
      !1 000 000, : 12.43user 0.63system 0:13.57elapsed 96%CPU
      call sort(rr, ii)
      call print_output()
      !
      print g,  'Sort using custom rule'
      !do i=1,1000000
         call sort(rr, ii, comparison_func=german_dictionary)
      !enddo
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
      !
      ! (Note: In Variant 2, Äpfel sorts as "Aepfel", meaning it comes after
      ! Apfel but before Band. Bär sorts as "Baer", putting it right after Band).
      !
      ! Varient 2: Apfel → Äpfel → Band →
      ! Bär → Bus → Bussard → Buße →
      ! Fähnlein → Fähnlein →
      ! Mond → Möve → Mutter →
      ! Mütter → Oase → Öl → Ufer → Übung → Zebra
      !
      write(*,g) 'TEST WORDS:',rr(ii) == answer
      if(all(rr(ii) == answer))then
         write(*,g)'PASSED'
      else
         write(*,g)'FAILED'
         stop 1
      endif

   contains

   subroutine print_output()
   character(len=:),allocatable :: output(:)
   integer                      :: i
      output=rr(ii)
      print g, ( trim(output(i)),i=1,size(output) )
      print g
   end subroutine print_output

!
! German dictionaries and standard sorting rules follow specific guidelines
! for handling umlauts (ä, ö, ü) and the eszett (β).
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
!  codepoint or the associated Latin character followed by the combining dieresis
!  modifier U+0308.
!
!    \xE4   = ä (Latin small letter a with diaeresis): U+00E4
!    \xF6   = ö (Latin small letter o with diaeresis): U+00F6
!    \xFC   = ü (Latin small letter u with diaeresis): U+00FC
!    \DF    = ß (lower_eszett)
!    \xC4   = Ä (Latin capital letter A with diaeresis): U+00C4
!    \xD6   = Ö (Latin capital letter O with diaeresis): U+00D6
!    \xDC   = Ü (Latin capital letter U with diaeresis): U+00DC
!    \1E9E  = ẞ (upper_eszett)
!
! or equivalently
!
!    ä       = a, ö       = o, ü       = u
!    a\u0308 = a, o\u0308 = o, u\u0308 = u
!
!    Ä       = A, Ö       = O, Ü       = U
!    A\u0308 = A, O\u0308 = O, U\u0308 = U
!
! Alternative Sorting Rules
!
! Note for references that depending on the context (such as phone books
! or digital systems), other official standards apply:
!
!   + Phone Book Rules (DIN 5007-1, Variant 2): Umlauts are expanded by
!     adding an e after the vowel (ä = ae, ö = oe, ü = ue). As in Variant 1
!     the letter β is treated as ss.
!
!   + Strict / Character-by-Character Order: Some systems or older indexes
!     place umlauts right after their base letter as separate characters
!     (a, ä, b, c...).
!
function german_dictionary(lhs, rhs) result(is_lle)
!
! @(#) Lexically compare two strings being less than or equal using German dictionary order
!
type(ut), intent(in) :: lhs, rhs
logical              :: is_lle

   ! an in-place comparison would require less copying but for simplicity
   ! make and condition copies of the input and compare those
   ! RETURN 1.98user 1.11system 0:03.56elapsed 86%CPU (0avgtext+0avgdata 240324maxresident)k
   !is_lle = normalize(lhs) < normalize(rhs)
   is_lle = nmi(trim(lhs)) < nmi(trim(rhs))

end function german_dictionary

function normalize(in) result (out)
type(ut),intent(in)  :: in
type(ut)             :: out
integer,allocatable  :: codes(:)
integer              :: i, j
integer              :: multi
integer              :: sub
integer              :: sz
integer,parameter    :: COMB=2*int(z'308'), &
                        UMLA=2*int(z'E4'), UMLO=2*int(z'F6'), UMLU=2*int(z'FC'), &
                        UMUA=2*int(z'C4'), UMUO=2*int(z'D6'), UMUU=2*int(z'DC'), &
                        LA=2*ichar('a')+1, LO=2*ichar('o')+1, LU=2*ichar('u')+1, &
                        UA=2*ichar('A')+1, UO=2*ichar('O')+1, UU=2*ichar('U')+1
   ! RETURN 41.31user 1.41system 0:44.24elapsed 96%CPU (0avgtext+0avgdata 239980maxresident)k
   ! The code values in OUT are not valid UTF-8, they are used just
   ! for a comparison

   ! make codes all even (assuming doubling causes no overflow) to make room for umlaut values
   ! because if two strings are equal when normalized umlauted character should be last
   codes=in%codes*2
   !codes=in%codepoint()*2
   ! RETURN codes     61.72user 1.36system 1:05.50elapsed 96%CPU (0avgtext+0avgdata 240200maxresident)k
   ! RETURN codepoint 86.98user 1.55system 1:31.00elapsed 97%CPU (0avgtext+0avgdata 240096maxresident)k
   ! now if followed by the combining dieresis add one so can tell between a Latin letter and Latin letter that was umlauted
   do i=1,size(codes)-1
      if(codes(i+1) == COMB)then
         codes(i)=codes(i)+1
      endif
   enddo
   ! RETURN 89.06user 1.88system 1:33.57elapsed 97%CPU (0avgtext+0avgdata 240148maxresident)k

   ! Now remove the combining dieresis modifier U+0308. Note that assuming it only occurs following a,o,u so possible
   ! issue if not pure German
   codes=pack( codes, codes /= COMB )
   ! RETURN 144.46user 2.52system 2:33.78elapsed 95%CPU (0avgtext+0avgdata 129356maxresident)k
!=================================================================
   ! replace umlauted characters with associated Latin letters doubled and add one to mark as previously umlauted
   ! RETURN 182.90user 2.11system 3:11.02elapsed 96%CPU
!=================================================================
! 208.31user 1.75system 3:35.43elapsed 97%CPU (0avgtext+0avgdata 240860maxresident)k
!   DO I=1,size(codes)
!      if(codes(i) <= 2*127)then
!      elseif(codes(i) == UMLA)then
!         codes(i)=LA
!      elseif(codes(i) == UMLO)then
!         codes(i)=LO
!      elseif(codes(i) == UMLU)then
!         codes(i)=LU
!      elseif(codes(i) == UMUA)then
!         codes(i)=UA
!      elseif(codes(i) == UMUO)then
!         codes(i)=UO
!      elseif(codes(i) == UMUU)then
!         codes(i)=UU
!      endif
!   ENDDO
!=================================================================
! 205.77user 2.11system 3:34.21elapsed 97%CPU
!   DO CONCURRENT ( I=1:size(codes) )
!      if(codes(i) <= 2*127)then
!      elseif(codes(i) == UMLA)then
!         codes(i)=LA
!      elseif(codes(i) == UMLO)then
!         codes(i)=LO
!      elseif(codes(i) == UMLU)then
!         codes(i)=LU
!      elseif(codes(i) == UMUA)then
!         codes(i)=UA
!      elseif(codes(i) == UMUO)then
!         codes(i)=UO
!      elseif(codes(i) == UMUU)then
!         codes(i)=UU
!      endif
!   ENDDO
!=================================================================
! 182.90user 2.11system 3:11.02elapsed 96%CPU
   DO CONCURRENT ( I=1:size(codes) )
      if(codes(i) == UMLA)codes(i)=LA
      if(codes(i) == UMLO)codes(i)=LO
      if(codes(i) == UMLU)codes(i)=LU
      if(codes(i) == UMUA)codes(i)=UA
      if(codes(i) == UMUO)codes(i)=UO
      if(codes(i) == UMUU)codes(i)=UU
   ENDDO
!=================================================================
! 216.14user 1.54system 3:44.74elapsed 96%CPU
!    codes= merge( LA,codes, codes == UMLA )
!    codes= merge( LO,codes, codes == UMLO )
!    codes= merge( LU,codes, codes == UMLU )
!    codes= merge( UA,codes, codes == UMUA )
!    codes= merge( UO,codes, codes == UMUO )
!    codes= merge( UU,codes, codes == UMUU )
!=================================================================
! 212.39user 2.33system 3:53.28elapsed 92%CPU
!   do i=1,size(codes)
!      select case (codes(i))
!       case( :2*127)
!       case( UMLA); codes(i)=LA
!       case( UMLO); codes(i)=LO
!       case( UMLU); codes(i)=LU
!       case( UMUA); codes(i)=UA
!       case( UMUO); codes(i)=UO
!       case( UMUU); codes(i)=UU
!      end select
!   enddo
!=================================================================
! 276.06user 2.76system 4:54.66elapsed 94%CPU
!    WHERE ( codes == UMLA )
!       codes=LA
!    ELSEWHERE ( codes == UMLO )
!       codes=LO
!    ELSEWHERE ( codes == UMLU )
!       codes=LU
!    ELSEWHERE ( codes == UMUA )
!       codes=UA
!    ELSEWHERE ( codes == UMUO )
!       codes=UO
!    ELSEWHERE ( codes == UMUU )
!       codes=UU
!    END WHERE
!=================================================================
   ! substitute in 'ss' and 'SS'
   !    \DF    = ß (lower_eszett)
   !    \1E9E  = ẞ (upper_eszett)
   !
   do j=1,2

      select case(j)
       case(1)
         multi=2*int(z'DF')
         sub=2*int(ichar('s'))
       case(2)
         multi=2*int(z'1E9E')
         sub=2*int(ichar('S'))
       case default
         stop '*normalize* internal error: j out of range'
      end select

      sz=size(codes)
      do i=sz,1,-1
         if(codes(i) == multi)then
            if(sz == 1)then
               codes=[sub,sub]
            elseif(i == sz)then
               codes=[codes(:i-1),sub,sub]
            elseif(i == 1)then
               codes=[sub,sub,codes(i+1:)]
            else
               codes=[codes(:i-1),sub,sub,codes(i+1:)]
            endif
         endif
      enddo
   enddo

   out=codes

end function normalize

function nmi(in) result (out)
type(ut),intent(in)  :: in
type(ut)             :: out
integer,allocatable  :: codes(:)
integer              :: i, ikeep
integer,parameter    :: comb=2*int(z'308'), &
 &  um_lower_a=2*int(z'E4'),    um_lower_o=2*int(z'F6'),    um_lower_u=2*int(z'FC'),  &
 &  um_upper_a=2*int(z'C4'),    um_upper_o=2*int(z'D6'),    um_upper_u=2*int(z'DC'),  &
 &  lower_a=2*ichar('a')+1,     lower_o=2*ichar('o')+1,     lower_u=2*ichar('u')+1,   &
 &  upper_a=2*ichar('A')+1,     upper_o=2*ichar('O')+1,     upper_u=2*ichar('U')+1,   &
 &  lower_eszett=2*int(z'df'),  upper_eszett=2*int(z'1e9e'),                              &                         
 &  lower_s=2*int(ichar('s')),  upper_s=2*int(ichar('S'))                             

   ! if every letter is a beta and first letter is COMB (which should not happen)
   ! longest required would be
   allocate(codes(0:2*len(in)))

   ikeep=0
   do i=1,len(in)
      if(in%codes(i) == comb)then
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
   ! codes are no longer valid Unicode codepoints for the input
   ! but can be used to compare lexically to another normalized value only
   ! because needed some method of telling if an A,O,U used to have an umlaut
   out=codes(1:ikeep)

end function nmi

end program demo_sort
