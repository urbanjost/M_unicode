module M_unicode__convert

! UTF-8 <-> UTF-32 codepoint

use iso_fortran_env, only: int32
implicit none
private
public :: utoc, ctou

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!### NAME
!!   ctou(3f) - [M_unicode:CONVERSION] convert codepoints to UTF-8 CHARACTER string
!!   (LICENSE:MIT)
!!
!!### SYNOPSIS
!!
!!
!!    pure function ctou(codepoints) result(utf8)
!!
!!     integer,allocatable,intent(in) :: codepoints(:)
!!     character(len=*),intent(out)   :: utf8
!!
!!### CHARACTERISTICS
!!   + CODEPOINTS is of default INTEGER kind
!!   + UTF8 is a CHARACTER variable
!!### DESCRIPTION
!!
!!   ctou(3f) takes an integer array of Unicode codepoint
!!   values and generates a scalar CHARACTER variable
!!   containing bytes representing UTF-8-encoded data.
!!
!!### OPTIONS
!!
!!    + CODEPOINTS :  An INTEGER array of Unicode codepoint values representing
!!                    the glyphs to be encoded as UTF-8 data
!!
!!    + UTF8 :  Scalar CHARACTER variable
!!              containing a stream of bytes containing data encoded as
!!              UTF-8 text.
!!
!!### EXAMPLES
!!
!!   Sample program
!!
!!    program demo_ctou
!!    use m_unicode, only : ctou
!!    implicit none
!!    !'Noho me ka hau’oli' !(Be happy)
!!    integer,parameter :: codepoints(*)=[ &
!!       & 78,111,104,111,&
!!       & 32,109,101, &
!!       & 32,107,97, &
!!       & 32,104,97,117,8217,111,108,105]
!!    character(len=:),allocatable :: string
!!    character(len=1),allocatable :: bytes(:)
!!    character(len=*),parameter   :: space='(*(g0,1x))'
!!    character(len=*),parameter   :: z='(a,*(z0,1x))'
!!
!!      write(*,space)'CODEPOINTS:', codepoints
!!      write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
!!    !
!!      string = ctou(codepoints)
!!      write(*,space)'STRING:', string
!!    !
!!      write(*,space)'How long is this string in glyphs? '
!!      write(*,space)size(codepoints)
!!    !
!!      write(*,space)'How long is this string in bytes? '
!!      write(*,space)len(string)
!!    !
!!    end program demo_ctou
!!
!!  Results:
!!
!!     > CODEPOINTS: 78 111 104 111 32 109 101 32 107 97 32 104 97 117 ...
!!     > 8217 111 108 105
!!     > 48 4E 6F 68 6F 20 6D 65 20 6B 61 20 68 61 75 2019 6F 6C 69
!!     > STRING:Noho me ka hau’oli
!!     > How long is this string in glyphs?
!!     > 18
!!     > How long is this string in bytes?
!!     > 20
!!
!!### SEE ALSO
!!   functions that perform operations on character strings:
!!
!!   + elemental: adjustl(3), adjustr(3), index(3), scan(3), verify(3)
!!   + non-elemental: len_trim(3), repeat(3), trim(3),
!!     ctou(3), utf8_to_codepoints(3), codepoints_to_utf8(3)
!!
!!### AUTHOR
!!   + John S. Urban
!!
!!### LICENSE
!!     MIT
!       utoc(str) result(cp)
!
!           Decodes a UTF-8 encoded Fortran character variable into an array of
!           UTF-32 codepoints.
!
!           Arguments:
!               str  character(len=*), intent(in)
!                    Input UTF-8 byte sequence. Truncated or malformed UTF-8
!                    sequences are replaced with U+FFFD (Replacement Character).
!
!           Return Value:
!               cp   integer(int32), allocatable :: cp(:)
!                    Array of decoded scalar codepoints (size 0 if len(str) == 0).
!
!-----------------------------------------------------------------------------------------------------------------------------------
function utoc(str) result(cp)
   character(len=*), intent(in) :: str
   integer(int32), allocatable  :: cp(:)
   integer                      :: i, n, count, b1, b2, b3, b4
   n = len(str)
   allocate(cp(n))
   count = 0
   i = 1
   do while (i <= n)
      b1 = iachar(str(i:i))
      count = count + 1
      if (b1 < 128) then
         cp(count) = int(b1, int32)
         i = i + 1
      elseif (iand(b1, 224) == 192 .and. i + 1 <= n) then
         b2 = iachar(str(i+1:i+1))
         cp(count) = int(ior(ishft(iand(b1, 31), 6), iand(b2, 63)), int32)
         i = i + 2
      elseif (iand(b1, 240) == 224 .and. i + 2 <= n) then
         b2 = iachar(str(i+1:i+1))
         b3 = iachar(str(i+2:i+2))
         cp(count) = int(ior(ior(ishft(iand(b1, 15), 12), ishft(iand(b2, 63), 6)), iand(b3, 63)), int32)
         i = i + 3
      elseif (iand(b1, 248) == 240 .and. i + 3 <= n) then
         b2 = iachar(str(i+1:i+1))
         b3 = iachar(str(i+2:i+2))
         b4 = iachar(str(i+3:i+3))
         cp(count) = int(ior(ior(ior(ishft(iand(b1, 7), 18), ishft(iand(b2, 63), 12)), &
            ishft(iand(b3, 63), 6)), iand(b4, 63)), int32)
         i = i + 4
      else
         cp(count) = int(z'FFFD', int32)
         i = i + 1
      endif
   enddo
   cp = cp(1:count)
end function utoc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!### NAME
!!   utoc(3f) - [M_unicode:CONVERSION] convert UTF-8 CHARACTER string to Unicode codepoints
!!   (LICENSE:MIT)
!!
!!### SYNOPSIS
!!
!!
!!    pure function utoc(utf8) result(codepoints)
!!
!!     character(len=*),intent(in) :: utf8
!!     integer,allocatable         :: codepoints(:)
!!
!!### CHARACTERISTICS
!!   + UTF8 is a CHARACTER variable
!!   + CODEPOINTS is of default INTEGER kind
!!### DESCRIPTION
!!
!!   utoc(3f) takes a scalar CHARACTER variable containing a stream of
!!   bytes representing UTF-8-encoded data and generates an integer array
!!   of Unicode codepoint values.
!!
!!### OPTIONS
!!
!!    + UTF8 :  Scalar CHARACTER variable containing a stream of bytes
!!              containing data encoded as UTF-8 text.
!!
!!    + CODEPOINTS :  An INTEGER array of Unicode codepoint values representing
!!                    the glyphs to be encoded as UTF-8 data
!!
!!### EXAMPLES
!!
!!   Sample program
!!
!!    program demo_utoc
!!    use m_unicode, only : utoc
!!    implicit none
!!    !'Noho me ka hau’oli' !(Be happy)
!!    integer,parameter :: codepoints(*)=[ &
!!       & 78,111,104,111,&
!!       & 32,109,101, &
!!       & 32,107,97, &
!!       & 32,104,97,117,8217,111,108,105]
!!    character(len=:),allocatable :: string
!!    character(len=1),allocatable :: bytes(:)
!!    character(len=*),parameter   :: space='(*(g0,1x))'
!!    character(len=*),parameter   :: z='(a,*(z0,1x))'
!!
!!      write(*,space)'CODEPOINTS:', codepoints
!!      write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
!!    !
!!      string = utoc(codepoints)
!!      write(*,space)'STRING:', string
!!    !
!!      write(*,space)'How long is this string in glyphs? '
!!      write(*,space)size(codepoints)
!!    !
!!      write(*,space)'How long is this string in bytes? '
!!      write(*,space)len(string)
!!    !
!!    end program demo_utoc
!!
!!  Results:
!!
!!     > CODEPOINTS: 78 111 104 111 32 109 101 32 107 97 32 104 97 117 ...
!!     > 8217 111 108 105
!!     > 48 4E 6F 68 6F 20 6D 65 20 6B 61 20 68 61 75 2019 6F 6C 69
!!     > STRING:Noho me ka hau’oli
!!     > How long is this string in glyphs?
!!     > 18
!!     > How long is this string in bytes?
!!     > 20
!!
!!### SEE ALSO
!!   functions that perform operations on character strings:
!!
!!   + elemental: adjustl(3), adjustr(3), index(3), scan(3), verify(3)
!!   + non-elemental: len_trim(3), repeat(3), trim(3),
!!     utoc(3), utf8_to_codepoints(3), codepoints_to_utf8(3)
!!
!!### AUTHOR
!!   + John S. Urban
!!
!!### LICENSE
!!     MIT
!-----------------------------------------------------------------------------------------------------------------------------------
!       ctou(cp) result(str)
!
!           Encodes a UTF-32 codepoint array into a UTF-8 Fortran character string.
!
!           Arguments:
!               cp   integer(int32), intent(in) :: cp(:)
!                    Array of Unicode scalar values. Out-of-range values
!                    (< 0 or > U+10FFFF) are encoded as U+FFFD.
!
!           Return Value:
!               str  character(len=:), allocatable
!                    UTF-8 encoded string.
!-----------------------------------------------------------------------------------------------------------------------------------
function ctou(cp) result(str)
   integer(int32), intent(in)                   :: cp(:)
   character(len=:), allocatable                :: str
   integer                                      :: i, pos, c
   allocate(character(len=max(1, size(cp) * 4)) :: str)
   pos = 0
   do i = 1, size(cp)
      c = int(cp(i))
      if (c < 0 .or. c > int(z'10FFFF')) c = int(z'FFFD')
      if (c < 128) then
         pos = pos + 1
         str(pos:pos) = achar(c)
      elseif (c < 2048) then
         str(pos+1:pos+1) = achar(ior(192, ishft(c, -6)))
         str(pos+2:pos+2) = achar(ior(128, iand(c, 63)))
         pos = pos + 2
      elseif (c < 65536) then
         str(pos+1:pos+1) = achar(ior(224, ishft(c, -12)))
         str(pos+2:pos+2) = achar(ior(128, iand(ishft(c, -6), 63)))
         str(pos+3:pos+3) = achar(ior(128, iand(c, 63)))
         pos = pos + 3
      else
         str(pos+1:pos+1) = achar(ior(240, ishft(c, -18)))
         str(pos+2:pos+2) = achar(ior(128, iand(ishft(c, -12), 63)))
         str(pos+3:pos+3) = achar(ior(128, iand(ishft(c, -6), 63)))
         str(pos+4:pos+4) = achar(ior(128, iand(c, 63)))
         pos = pos + 4
      endif
   enddo
   str = str(1:pos)
end function ctou
!-----------------------------------------------------------------------------------------------------------------------------------
end module M_unicode__convert
