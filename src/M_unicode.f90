! TODO:
!  o globbing
!  o regex
!  o elemental functions?
module M_unicode
! Unicode-related procedures not requiring compiler support of ISO-10646
! first presented in https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949
! including enhancements and latin support from Francois Jacq, 2025-08
!
use,intrinsic :: iso_fortran_env, only : error_unit, stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none

private
public :: utf8_to_codepoints,  codepoints_to_utf8
public :: adjustl, adjustr, index, len, len_trim, repeat, trim
public :: sort
public :: split, tokenize
public :: upper, lower
public :: scan,  verify
public :: unicode_type
public :: assignment(=)
public :: character
public :: lle, llt, lne, leq, lgt, lge
public :: operator(<=), operator(<), operator(/=), operator(==), operator(>), operator(>=), operator(//)

private :: a2s, s2a

interface utf8_to_codepoints
   module procedure utf8_to_codepoints_str,utf8_to_codepoints_chars
end interface utf8_to_codepoints

interface codepoints_to_utf8
   module procedure codepoints_to_utf8_str,codepoints_to_utf8_chars
end interface codepoints_to_utf8

interface sort
   module procedure :: sort_quick_rx
end interface sort

interface tokenize
   module procedure :: split_first_last, split_pos, split_tokens
end interface tokenize

interface split
   module procedure :: split_first_last, split_pos, split_tokens
end interface split

! Assign a character sequence to a string.
interface assignment(=)
   module procedure :: assign_str_char
   module procedure :: assign_str_codes
end interface assignment(=)

interface character
   module procedure :: char_str,            char_strs
   module procedure :: char_str_range,      char_strs_range
   module procedure :: char_str_range_step, char_strs_range_step
end interface character

! INTRINSIC COMPATIBILITY
interface adjustl;      module procedure :: adjustl_str;  end interface adjustl
interface adjustr;      module procedure :: adjustr_str;  end interface adjustr
interface len;          module procedure :: len_str;      end interface len
interface len_trim;     module procedure :: len_trim_str; end interface len_trim
interface repeat;       module procedure :: repeat_str;   end interface repeat
interface trim;         module procedure :: trim_str;     end interface trim
interface index;        module procedure :: index_str_str,  index_str_char,  index_char_str;  end interface index

interface lle;          module procedure :: lle_str_str,    lle_str_char,    lle_char_str;    end interface lle
interface llt;          module procedure :: llt_str_str,    llt_str_char,    llt_char_str;    end interface llt
interface lne;          module procedure :: lne_char_str,   lne_str_char,    lne_str_str;     end interface lne
interface leq;          module procedure :: leq_char_str,   leq_str_char,    leq_str_str;     end interface leq
interface lgt;          module procedure :: lgt_str_str,    lgt_str_char,    lgt_char_str;    end interface lgt
interface lge;          module procedure :: lge_str_str,    lge_str_char,    lge_char_str;    end interface lge
interface operator(<=); module procedure :: lle_str_str,    lle_str_char,    lle_char_str;    end interface operator(<=)
interface operator(<);  module procedure :: llt_str_str,    llt_str_char,    llt_char_str;    end interface operator(<)
interface operator(/=); module procedure :: lne_char_str,   lne_str_char,    lne_str_str;     end interface operator(/=)
interface operator(==); module procedure :: leq_char_str,   leq_str_char,    leq_str_str;     end interface operator(==)
interface operator(>);  module procedure :: lgt_str_str,    lgt_str_char,    lgt_char_str;    end interface operator(>)
interface operator(>=); module procedure :: lge_str_str,    lge_str_char,    lge_char_str;    end interface operator(>=)
interface operator(//); module procedure :: concat_str_str, concat_str_char, concat_char_str; end interface operator(//)

type :: unicode_type ! Unicode string type holding an arbitrary sequence of integer codes.
   !sequence ! not used for storage association; a kludge to prevent extending this type.
   private
   integer, allocatable :: codes(:)
contains
   ! METHODS:
   procedure  ::  character      =>  oop_character
   procedure  ::  codepoint      =>  oop_codepoint
   procedure  ::  bytes          =>  oop_bytes    

   procedure  ::  adjustl        =>  oop_adjustl
   procedure  ::  adjustr        =>  oop_adjustr
   procedure  ::  index          =>  oop_index
   procedure  ::  len            =>  oop_len
   procedure  ::  len_trim       =>  oop_len_trim
   procedure  ::  trim           =>  oop_trim

   procedure  ::  split          =>  oop_split
   procedure  ::  tokenize       =>  oop_tokenize
   procedure  ::  scan           =>  oop_scan
   procedure  ::  verify         =>  oop_verify
!! procedure  ::  sort           =>  oop_sort
   procedure  ::  upper          =>  oop_upper
   procedure  ::  lower          =>  oop_lower

   !DECLARATION OF OVERLOADED OPERATORS FOR TYPE(UNICODE_TYPE)
   procedure,private :: eq => oop_eq
!   generic           :: operator(==) => eq
!   procedure,private :: ge => oop_ge
!   generic           :: operator(>=) => oop_ge
!   procedure,private :: lt => oop_lt
!   generic           :: operator(<)  => oop_lt
!   procedure,private :: gt => oop_gt
!   generic           :: operator(>)  => oop_gt
!   procedure,private :: le => oop_le
!   generic           :: operator(<=) => oop_le
!   procedure,private :: ne => oop_ne
!   generic           :: operator(/=) => oop_ne

!   procedure,private :: string_append_value
!   generic           :: operator(//) => string_append_value
end type unicode_type

! Constructor for new string instances
interface unicode_type
   elemental module function new_str(string) result(new)
      character(len=*), intent(in), optional :: string
      type(unicode_type)                     :: new
   end function new_str

   module function new_codes(codes) result(new)
      integer, intent(in)                    :: codes(:)
      type(unicode_type)                     :: new
   end function new_codes

end interface unicode_type

! space U+0020 32 Common Basic Latin Separator, Most common (normal
! ASCII space)
!
! no-break space U+00A0 160 Common Latin-1 Supplement Separator,
! Non-breaking space: identical to U+0020, but not a point at which a line
! may be broken.
!
! en quad U+2000 8192 General Punctuation Separator, Width of one en. U+2002
! is canonically equivalent to this character; U+2002 is preferred.
!
! em quad U+2001 8193   Common General Punctuation Separator,
! Also known as "mutton quad". Width of one em. U+2003 is
! canonically equivalent to this character; U+2003 is preferred.
!
! en space U+2002 8194   Common General Punctuation Separator,
! space Also known as "nut". Width of one en. U+2000 En Quad is
! canonically equivalent to this character; U+2002 is preferred.
!
! em space U+2003 8195  Common General Punctuation Separator,
! space Also known as "mutton". Width of one em. U+2001 Em Quad is
! canonically equivalent to this character; U+2003 is preferred.
!
! three-per-em space U+2004 8196 Common General Punctuation Separator,
! Also known as "thick space". One third of an em wide.
!
! four-per-em space U+2005 8197 Common General Punctuation Separator,
! space Also known as "mid space". One fourth of an em wide.
!
! six-per-em space U+2006 8198 Common General Punctuation Separator,
! space One sixth of an em wide. In computer typography, sometimes equated
! to U+2009.
!
! figure space U+2007 8199 Common General Punctuation Separator, In fonts
! with monospaced digits, equal to the width of one digit.
!
! punctuation space U+2008 8200 Common General Punctuation Separator,
! As wide as the narrow punctuation in a font, i.e. the advance width of
! the period or comma.
!
! thin space U+2009 8201 Common General Punctuation Separator, one-fifth
! (sometimes one-sixth) of an em wide.  Recommended for use as a thousands
! separator for measures made with SI units. Unlike U+2002 to U+2008,
! its width may get adjusted in typesetting.
!
! hair space U+200A 8202 Common General Punctuation Separator, space
! Thinner than a thin space.
!
! narrow no-break space U+202F 8239 Common General Punctuation Separator,
! Similar in function to U+00A0
!
! No-Break Space. When used with Mongolian, its width is usually one third
! of the normal space; in other context, its width sometimes resembles
! that of the Thin Space (U+2009).
!
! medium mathematical space U+205F 8287   Common General Punctuation
! Separator, space MMSP. Used in mathematical formulae. Four-eighteenths
! of an em. In mathematical typography, the widths of spaces are usually
! given in integral multiples of an eighteenth of an em, and 4/18 em
! may be used in several situations, for example between the a and the +
! and between the + and the b in the expression a + b.
!
! ideographic space U+3000 12288 　 Yes No Common CJK Symbols and
! Punctuation Separator, As wide as a CJK character cell (fullwidth). Used,
! for example, in tai tou.
integer,parameter :: G_SPACE=32
integer,parameter :: G_SPACES(*) = [ 32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288 ]
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine codepoints_to_utf8_chars(unicode,utf8,nerr)

integer,intent(in)                :: unicode(:)
character,allocatable,intent(out) :: utf8(:)
integer,intent(out)               :: nerr
integer                           :: i, n_unicode, n_utf8, cp
character, allocatable            :: temp_utf8(:)

   n_unicode = size(unicode)

   allocate(temp_utf8(4*n_unicode))
   n_utf8 = 0

   nerr=0
   do i = 1, n_unicode
      cp = unicode(i)

      select case (cp)
      case (0:127) ! 1 byte : 0xxxxxxx
         n_utf8 = n_utf8 + 1
         temp_utf8(n_utf8) = char(cp)

      case (128:2047) ! 2 bytes : 110xxxxx 10xxxxxx
         n_utf8 = n_utf8 + 2
         temp_utf8(n_utf8-1) = char(ior(192, ishft(cp, -6)))
         temp_utf8(n_utf8)   = char(ior(128, iand(cp, 63)))

      case (2048:65535) ! 3 bytes : 1110xxxx 10xxxxxx 10xxxxxx
         if (cp >= 55296 .and. cp <= 57343) then
            nerr=nerr+1
            n_utf8 = n_utf8 + 1
            temp_utf8(n_utf8) = '?'
            cycle
         endif
         n_utf8 = n_utf8 + 3
         temp_utf8(n_utf8-2) = char(ior(224, ishft(cp, -12)))
         temp_utf8(n_utf8-1) = char(ior(128, iand(ishft(cp, -6), 63)))
         temp_utf8(n_utf8)   = char(ior(128, iand(cp, 63)))

      case (65536:1114111) ! 4 bytes : 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
         n_utf8 = n_utf8 + 4
         temp_utf8(n_utf8-3) = char(ior(240, ishft(cp, -18)))
         temp_utf8(n_utf8-2) = char(ior(128, iand(ishft(cp, -12), 63)))
         temp_utf8(n_utf8-1) = char(ior(128, iand(ishft(cp, -6), 63)))
         temp_utf8(n_utf8)   = char(ior(128, iand(cp, 63)))

      case default
         nerr=nerr+1
         n_utf8 = n_utf8 + 1
         temp_utf8(n_utf8) = '?'
      end select
   enddo

   allocate(utf8(n_utf8))
   utf8 = temp_utf8(1:n_utf8)

end subroutine codepoints_to_utf8_chars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_codepoints_chars(utf8,unicode,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character            ,intent(in)  :: utf8(:)
integer  ,allocatable,intent(out) :: unicode(:)
integer,intent(out)               :: nerr
integer                           :: n_out
integer                           :: i, len8, b1, b2, b3, b4
integer                           :: cp, nbytes,nerr0
integer,allocatable               :: temp(:)

   nerr = 0

   len8 = size(utf8)
   i = 1
   n_out = 0
   allocate(temp(len8)) ! big enough to store all unicode values

   do while (i <= len8)

      nerr0=nerr

      b1 = ichar(utf8(i))
      if (b1 < 0) b1 = b1 + 256

      nbytes = 1

      select case (b1)

      case (0:127)
         cp = b1

      case (192:223)
         if (i+1 > len8) then
            nbytes=len8-i+1
            nerr = nerr+1
            cp=ICHAR('?')
         else
            nbytes=2
            b2 = ichar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            if (iand(b2, 192) /= 128) then
               nerr=nerr+1
               cp=ICHAR('?')
            else
               cp = iand(b1, 31)
               cp = ishft(cp,6) + iand(b2,63)
            endif
         endif

      case (224:239)
         if (i+2 > len8) then
            nbytes=len8-i+1
            nerr=nerr+1
            cp=ICHAR('?')
         else
            nbytes = 3
            b2 = ichar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            b3 = ichar(utf8(i+2)); if (b3 < 0) b3 = b3 + 256
            if (iand(b2, 192) /= 128 .or. iand(b3, 192) /= 128) then
               nerr =nerr+1
               cp=ICHAR('?')
            else
               cp = iand(b1, 15)
               cp = ishft(cp,6) + iand(b2,63)
               cp = ishft(cp,6) + iand(b3,63)
            endif
         endif

      case (240:247)
         if (i+3 > len8) then
            nbytes=len8-i+1
            nerr = nerr+1
            cp=ICHAR('?')
         else
            nbytes = 4
            b2 = ichar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            b3 = ichar(utf8(i+2)); if (b3 < 0) b3 = b3 + 256
            b4 = ichar(utf8(i+3)); if (b4 < 0) b4 = b4 + 256
            if (iand(b2,192)/=128 .or. iand(b3,192)/=128 .or. iand(b4,192)/=128) then
               nerr = nerr+1
               cp=ICHAR('?')
            else
               cp = iand(b1, 7)
               cp = ishft(cp,6) + iand(b2,63)
               cp = ishft(cp,6) + iand(b3,63)
               cp = ishft(cp,6) + iand(b4,63)
            endif
         endif

      case default
         nerr=nerr+1
         cp=ICHAR('?')

      end select

      if(nerr0 /= nerr) then
         ! This is an invalid UTF-8 start byte. We apply the heuristic
         ! and interpret it as an ISO-8859-15 character.
         select case (b1)
         case (164); cp = 8364 ! Euro
         case (166); cp = 352  ! S caron
         case (168); cp = 353  ! s caron
         case (180); cp = 381  ! Z caron
         case (184); cp = 382  ! z caron
         case (188); cp = 338  ! OE
         case (189); cp = 339  ! oe
         case (190); cp = 376  ! Y trema
         case default
            cp = b1 ! For all other chars, the codepoint is the byte value
         end select
         nbytes=1
      endif

      n_out = n_out + 1
      temp(n_out) = cp
      i = i + nbytes

   enddo

   allocate(unicode(n_out))
   unicode = temp(1:n_out)

end subroutine utf8_to_codepoints_chars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function a2s(array)  result (string)

! @(#) M_strs a2s(3fp) function to copy char array to string

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i

   forall( i = 1:size(array)) string(i:i) = array(i)
!  string=transfer(array,string)

end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

! @(#) M_strs s2a(3fp) function to copy string(1 Clen(string)) to char array

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i

   forall(i=1:len(string)) array(i) = string(i:i)
!  array=transfer(string,array)

end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine codepoints_to_utf8_str(unicode,utf8,nerr)

integer,intent(in)                       :: unicode(:)
character(len=:),allocatable,intent(out) :: utf8
integer,intent(out)                      :: nerr
character, allocatable                   :: utf8_chars(:)
   nerr=0
   call codepoints_to_utf8_chars(unicode,utf8_chars,nerr)
   utf8=a2s(utf8_chars)
end subroutine codepoints_to_utf8_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_codepoints_str(utf8,unicode,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character(len=*),intent(in)     :: utf8
integer,allocatable,intent(out) :: unicode(:)
integer,intent(out)             :: nerr
character,allocatable           :: temp(:)
   temp=s2a(utf8)
   call utf8_to_codepoints_chars(temp,unicode,nerr)
end subroutine utf8_to_codepoints_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Constructor for new string instances from a scalar character value.
elemental module function new_str(string) result(new)
character(len=*), intent(in), optional :: string
type(unicode_type)                     :: new
integer                                :: nerr
   if (present(string)) then
      call utf8_to_codepoints_str(string,new%codes,nerr)
   endif
end function new_str

! Constructor for new string instance from a vector integer value.
module function new_codes(codes) result(new)
integer,intent(in) :: codes(:)
type(unicode_type) :: new
   new%codes=codes
end function new_codes
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Assign a character sequence to a string.
elemental subroutine assign_str_char(lhs, rhs)
type(unicode_type), intent(inout) :: lhs
character(len=*), intent(in)      :: rhs
integer                           :: nerr
   call utf8_to_codepoints_str(rhs,lhs%codes,nerr)
end subroutine assign_str_char

! Assign a character sequence to a string.
subroutine assign_str_codes(lhs, rhs)
type(unicode_type), intent(inout) :: lhs
integer, intent(in)               :: rhs(:)
   lhs%codes=rhs
end subroutine assign_str_codes
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Returns the length of the character sequence represented by the string.
elemental function len_str(string) result(length)
type(unicode_type), intent(in) :: string
integer :: length

   if (allocated(string%codes)) then
      length = size(string%codes)
   else
      length = 0
   endif

end function len_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Return the character sequence represented by the string.
pure function char_str(string) result(aline)
type(unicode_type), intent(in)    :: string
character(len=:),allocatable      :: aline
integer                           :: nerr

   call codepoints_to_utf8_str(string%codes,aline,nerr)

end function char_str

pure function char_strs(string) result(lines)
type(unicode_type), intent(in)    :: string(:)
character(len=:),allocatable      :: lines(:)
character(len=:),allocatable      :: aline
integer                           :: i
integer                           :: mx
integer                           :: nerr

   mx=0
   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes,aline,nerr)
      mx=max(mx,len(aline))
   enddo

   allocate(character(len=mx) :: lines(size(string)) )

   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes,aline,nerr)
      lines(i)(:)=aline
   enddo

end function char_strs

pure function char_str_range(string, first, last) result(aline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: first
integer, intent(in)            :: last
character(len=:),allocatable   :: aline
integer                        :: nerr

   call codepoints_to_utf8_str(string%codes(first:last),aline,nerr)

end function char_str_range

pure function char_strs_range(string, first, last) result(lines)
type(unicode_type), intent(in) :: string(:)
integer, intent(in)            :: first
integer, intent(in)            :: last
character(len=:),allocatable   :: lines(:)
character(len=:),allocatable   :: aline
integer                        :: i
integer                        :: mx
integer                        :: nerr

   mx=0
   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes(first:last),aline,nerr)
      mx=max(mx,len(aline))
   enddo

   allocate(character(len=mx) :: lines(size(string)) )

   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes(first:last),aline,nerr)
      lines(i)(:)=aline
   enddo

end function char_strs_range

pure function char_str_range_step(string, first, last, step) result(aline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: first
integer, intent(in)            :: last
integer, intent(in)            :: step
character(len=:),allocatable   :: aline
integer                        :: nerr

   call codepoints_to_utf8_str(string%codes(first:last:step),aline,nerr)

end function char_str_range_step

pure function char_strs_range_step(string, first, last, step) result(lines)
type(unicode_type), intent(in) :: string(:)
integer, intent(in)            :: first
integer, intent(in)            :: last
integer, intent(in)            :: step
character(len=:),allocatable   :: lines(:)
character(len=:),allocatable   :: aline
integer                        :: i
integer                        :: mx
integer                        :: nerr

   mx=0
   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes(first:last:step),aline,nerr)
      mx=max(mx,len(aline))
   enddo

   allocate(character(len=mx) :: lines(size(string)) )

   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes(first:last:step),aline,nerr)
      lines(i)(:)=aline
   enddo

end function char_strs_range_step
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Repeats the character sequence held by the string by the number of specified copies.
! This method is elemental and returns a scalar character value.
elemental function repeat_str(string, ncopies) result(repeated_str)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: ncopies
type(unicode_type)             :: repeated_str
integer                        :: i

   repeated_str%codes=[(string%codes,i=1,ncopies)]

end function repeat_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Returns length of character sequence without trailing spaces represented by the string.
!
elemental function len_trim_str(string) result(length)
type(unicode_type), intent(in) :: string
integer                        :: length

   do length=size(string%codes),1,-1
      if(any(string%codes(length).eq.G_SPACES))cycle
      exit
   enddo

end function len_trim_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! This method is elemental and returns a scalar character value.
elemental function trim_str(string) result(trimmed_str)
type(unicode_type), intent(in) :: string
type(unicode_type)             :: trimmed_str
integer                        :: last

   last=len_trim_str(string)
   trimmed_str%codes=string%codes(:last)

end function trim_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! right-justify string by moving trailing spaces to beginning of string so length is retained even if spaces are of varied width
impure elemental function adjustr_str(string) result(adjusted)
type(unicode_type), intent(in) :: string
type(unicode_type)             :: adjusted
integer                        :: last

   last=len_trim_str(string)
   adjusted%codes=cshift(string%codes,-(size(string%codes)-last))

end function adjustr_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!left-justify string by  moving leading spaces to end of string so length is retained even if spaces are of varied width
elemental function adjustl_str(string) result(adjusted)
type(unicode_type), intent(in) :: string
type(unicode_type)             :: adjusted
integer                        :: first

   do first=1,size(string%codes),1
      if(any(string%codes(first).eq.G_SPACES))cycle
      exit
   enddo
   adjusted%codes=cshift(string%codes,first-1)

end function adjustl_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Concatenate two character sequences, LHS, RHS or both can be represented by a byte string or unicode_type.
!
elemental function concat_str_str(lhs, rhs) result(lhsrhs)
type(unicode_type), intent(in) :: lhs
type(unicode_type), intent(in) :: rhs
type(unicode_type)             :: lhsrhs
   lhsrhs%codes = [lhs%codes,rhs%codes]
end function concat_str_str

elemental function concat_str_char(lhs, rhs) result(lhsrhs)
type(unicode_type), intent(in) :: lhs
character(len=*), intent(in)   :: rhs
type(unicode_type)             :: lhsrhs
   lhsrhs = unicode_type(rhs)
   lhsrhs%codes = [lhs%codes, lhsrhs%codes]
end function concat_str_char

elemental function concat_char_str(lhs, rhs) result(lhsrhs)
character(len=*), intent(in)   :: lhs
type(unicode_type), intent(in) :: rhs
type(unicode_type)             :: lhsrhs
   lhsrhs = unicode_type(lhs)
   lhsrhs%codes = [lhsrhs%codes,rhs%codes]
end function concat_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Compare two character sequences for non-equality; LHS, RHS or both sequences can be a unicode string or character variable.
!
elemental function lne_str_str(lhs, rhs) result(is_equal)
type(unicode_type), intent(in) :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_equal
integer                        :: icount
   if(lhs%len_trim().eq.rhs%len_trim())then
      icount=lhs%len_trim()
      is_equal = .not.all( lhs%codes(:icount) .eq. rhs%codes(:icount) )
   else
      is_equal = .true.
   endif
end function lne_str_str

elemental function lne_str_char(lhs, rhs) result(is_equal)
type(unicode_type), intent(in) :: lhs
character(len=*), intent(in)   :: rhs
logical                        :: is_equal
   is_equal = lne_str_str(lhs, unicode_type(rhs))
end function lne_str_char

elemental function lne_char_str(lhs, rhs) result(is_equal)
character(len=*), intent(in)   :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_equal
   is_equal = lne_str_str(unicode_type(lhs), rhs)
end function lne_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Compare two character sequences for equality; LHS, RHS or both sequences can be a unicode string or character variable.
!
elemental function leq_str_str(lhs, rhs) result(is_equal)
type(unicode_type), intent(in) :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_equal
integer                        :: icount
   if(lhs%len_trim().eq.rhs%len_trim())then
      icount=lhs%len_trim()
      is_equal = all( lhs%codes(:icount) .eq. rhs%codes(:icount) )
   else
      is_equal = .false.
   endif
end function leq_str_str

elemental function leq_str_char(lhs, rhs) result(is_equal)
type(unicode_type), intent(in) :: lhs
character(len=*), intent(in)   :: rhs
logical                        :: is_equal
   is_equal = leq_str_str(lhs, unicode_type(rhs))
end function leq_str_char

elemental function leq_char_str(lhs, rhs) result(is_equal)
character(len=*), intent(in)   :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_equal
   is_equal = leq_str_str(unicode_type(lhs), rhs)
end function leq_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Lexically compare two character sequences for being greater or equal
elemental function lge_str_str(lhs, rhs) result(is_lge)
type(unicode_type), intent(in) :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_lge
integer                        :: i
integer                        :: llen
integer                        :: rlen

   llen=len_trim(lhs)
   rlen=len_trim(rhs)

   FOUND: block

   do i=1,min(llen,rlen)
      select case(lhs%codes(i)-rhs%codes(i))
      case(0);   cycle
      case(1:);  is_lge=.true.;  exit FOUND
      case(:-1); is_lge=.false.; exit FOUND
      end select
   enddo

   ! all equal, decide based on difference in length
   select case( llen - rlen )
   case(0);   is_lge=.true.
   case(1:);  is_lge=.true.
   case(:-1); is_lge=.false.
   end select

   endblock FOUND

end function lge_str_str

elemental function lge_str_char(lhs, rhs) result(is_lge)
type(unicode_type), intent(in) :: lhs
character(len=*), intent(in)   :: rhs
logical                        :: is_lge
   is_lge = lge_str_str(lhs, unicode_type(rhs))
end function lge_str_char

elemental function lge_char_str(lhs, rhs) result(is_lge)
character(len=*), intent(in)   :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_lge
   is_lge = lge_str_str(unicode_type(lhs), rhs )
end function lge_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Lexically compare two character sequences for being less than or equal
elemental function lle_str_str(lhs, rhs) result(is_lle)
type(unicode_type), intent(in) :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_lle
integer                        :: i
integer                        :: llen
integer                        :: rlen

   llen=len_trim(lhs)
   rlen=len_trim(rhs)

   FOUND: block

   do i=1,min(llen,rlen)
      select case( lhs%codes(i) - rhs%codes(i) )
      case(0);   cycle
      case(1:);  is_lle = .false.;  exit FOUND
      case(:-1); is_lle = .true.;   exit FOUND
      end select
   enddo

   ! all equal, decide based on difference in length
   select case( llen - rlen )
   case(:-1); is_lle = .true.
   case(0);   is_lle = .true.
   case(1:);  is_lle = .false.
   end select

   endblock FOUND

end function lle_str_str

elemental function lle_str_char(lhs, rhs) result(is_lle)
type(unicode_type), intent(in) :: lhs
character(len=*), intent(in)   :: rhs
logical                        :: is_lle
   is_lle = lle_str_str(lhs, unicode_type(rhs))
end function lle_str_char

elemental function lle_char_str(lhs, rhs) result(is_lle)
character(len=*), intent(in)   :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_lle
   is_lle = lle_str_str(unicode_type(lhs), rhs )
end function lle_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Lexically compare two character sequences for being less than
elemental function llt_str_str(lhs, rhs) result(is_llt)
type(unicode_type), intent(in) :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_llt
integer                        :: i
integer                        :: llen
integer                        :: rlen

   llen=len_trim(lhs)
   rlen=len_trim(rhs)

   FOUND: block

   do i=1,min(llen,rlen)
      select case(lhs%codes(i)-rhs%codes(i))
      case(0);   cycle;
      case(1:);  is_llt=.false.;  exit FOUND
      case(:-1); is_llt=.true.;   exit FOUND
      end select
   enddo

   ! all equal, decide based on difference in length
   select case( llen - rlen )
   case(0);   is_llt=.false.
   case(1:);  is_llt=.false.
   case(:-1); is_llt=.true.
   end select

   endblock FOUND

end function llt_str_str

elemental function llt_str_char(lhs, rhs) result(is_llt)
type(unicode_type), intent(in) :: lhs
character(len=*), intent(in)   :: rhs
logical                        :: is_llt
   is_llt = llt_str_str(lhs, unicode_type(rhs))
end function llt_str_char

elemental function llt_char_str(lhs, rhs) result(is_llt)
character(len=*), intent(in)   :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_llt
   is_llt = llt_str_str(unicode_type(lhs), rhs )
end function llt_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Lexically compare two character sequences for being greater than
elemental function lgt_str_str(lhs, rhs) result(is_lgt)
type(unicode_type), intent(in) :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_lgt
integer                        :: i
integer                        :: llen
integer                        :: rlen

   llen=len_trim(lhs)
   rlen=len_trim(rhs)

   FOUND: block

   do i=1,min(llen,rlen)
      select case(lhs%codes(i)-rhs%codes(i))
      case(0);   cycle;
      case(1:);  is_lgt=.true.;  exit FOUND
      case(:-1); is_lgt=.false.; exit FOUND
      end select
   enddo

   ! all equal, decide based on difference in length
   select case( llen - rlen )
   case(0);   is_lgt=.false.
   case(1:);  is_lgt=.true.
   case(:-1); is_lgt=.false.
   end select

   endblock FOUND

end function lgt_str_str

elemental function lgt_str_char(lhs, rhs) result(is_lgt)
type(unicode_type), intent(in) :: lhs
character(len=*), intent(in)   :: rhs
logical                        :: is_lgt
   is_lgt = lgt_str_str(lhs, unicode_type(rhs))
end function lgt_str_char

elemental function lgt_char_str(lhs, rhs) result(is_lgt)
character(len=*), intent(in)   :: lhs
type(unicode_type), intent(in) :: rhs
logical                        :: is_lgt
   is_lgt = lgt_str_str(unicode_type(lhs), rhs )
end function lgt_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! find location of substring within string

elemental function index_str_str(string, substring,back) result(foundat)
type(unicode_type), intent(in) :: string
type(unicode_type), intent(in) :: substring
logical,intent(in),optional    :: back
integer                        :: foundat
integer                        :: i
integer                        :: strlen
integer                        :: sublen
logical                        :: back_local

   back_local=.false.
   if(present(back))back_local=back

   strlen=string%len()
   sublen=substring%len()
   foundat=0

   if(back_local)then
      do i=strlen - sublen + 1,1,-1
         if ( all(string%codes(i:i+sublen-1) .eq. substring%codes) )then
            foundat=i
            exit
         endif
      enddo
   else
      do i=1,strlen - sublen + 1
         if ( all(string%codes(i:i+sublen-1) .eq. substring%codes) )then
            foundat=i
            exit
         endif
      enddo
   endif

end function index_str_str

elemental function index_str_char(string, substring,back) result(foundat)
type(unicode_type), intent(in) :: string
character(len=*), intent(in)   :: substring
logical,intent(in),optional    :: back
integer                        :: foundat
   foundat = index_str_str(string, unicode_type(substring))
end function index_str_char

elemental function index_char_str(string, substring,back) result(foundat)
character(len=*), intent(in)   :: string
type(unicode_type), intent(in) :: substring
logical,intent(in),optional    :: back
integer                        :: foundat
   foundat = index_str_str(unicode_type(string), substring )
end function index_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!!
!! In computing, Unicode characters are typically sorted using one of two methods:
!!
!! a simple binary code point sort or a more sophisticated,
!! language-sensitive collation. The correct approach depends on whether a
!! linguistically accurate "alphabetical" order is needed or if a simple,
!! fixed order is sufficient.
!!
!! Binary code point sort
!!
!! This is the simplest and fastest method, often used as a default by
!! programming languages and databases.
!!
!!     How it works: Strings are sorted based on the numeric value of their
!!     underlying Unicode code points. For example, a character with a code
!!     point of U+0061 (lowercase "a") will always be placed before U+0062
!!     (lowercase "b") because 97 is less than 98.
!!
!!     Limitations: While this works for the basic English alphabet, it
!!     produces non-intuitive results for other characters because the code
!!     point value does not correlate with linguistic sorting rules. For
!!     instance, it may place:
!!
!!         Uppercase letters before all lowercase letters (Z comes before a).
!!
!!         Accented letters in an order that is not linguistically correct
!!         for a given language (e.g., in German, an umlauted character
!!         like ö might be sorted differently than a plain o).
!!
!!         Characters from different scripts (like Latin, Greek, and
!!         Cyrillic) in an order determined solely by their assigned code
!!         point blocks.
!!
!! Unicode Collation Algorithm (UCA)
!!
!! This is the standard, more robust method for sorting that produces
!! correct, language-sensitive results. It is described in Unicode Technical
!! Standard #10.
!!
!!     How it works: Instead of sorting by a single numeric value, the
!!     UCA uses a multi-level approach to determine a sort key for each
!!     string. The algorithm takes into account the specific rules (or
!!     "tailorings") of a given language or locale, which are defined in
!!     the Common Locale Data Repository (CLDR).
!!
!!     Multi-level sorting: The UCA uses a hierarchy of weights for each
!!     character:
!!
!!         Primary: Compares the base letter, ignoring case and accents. This
!!         groups all versions of "a" (a, á, A, Á) together.
!!
!!         Secondary: Compares accents and diacritics. This establishes the
!!         order for different versions of the same base letter (e.g., o,
!!         ó, ô).
!!
!!         Tertiary: Compares case differences (uppercase vs. lowercase).
!!
!!         Quaternary: Deals with other special features, such as handling
!!         punctuation.
!!
!!     Locale-specific rules: The UCA can apply different rules based on
!!     a user's location. For example:
!!
!!         In German phonebooks, umlauted letters (ä) are often sorted as
!!         if they were ae. In other contexts, they are sorted with their
!!         base letter (a).
!!
!!         The correct sorting order for Chinese characters can be based
!!         on pronunciation (Pinyin) or stroke count, depending on the
!!         dictionary or region.
!!
!! How to choose a sorting method
!!
!!     Use binary sorting for performance when linguistic order doesn't
!!     matter. This is fine for internal data processing where you just
!!     need a consistent, quick sort.
!!
!!     Use the UCA for user-facing applications where culturally appropriate
!!     sorting is critical. If your application supports multiple languages,
!!     you must use a language-sensitive collator to provide the sorting
!!     users will expect. Most modern programming languages and databases
!!     have built-in libraries that implement the Unicode Collation
!!     Algorithm.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    sort_quick_rx(3f) - [M_unicode:sort:quicksort] indexed hybrid quicksort of an array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!      subroutine sort_quick_rx(data,index)
!!
!!          type(unicode_type),intent(in) :: data(:)
!!          integer,intent(out)           :: indx(size(data))
!!
!!##DESCRIPTION
!!    A rank hybrid quicksort. The data is not moved. An integer array is
!!    generated instead with values that are indices to the sorted order
!!    of the data. This requires a second array the size of the input
!!    array, which for large arrays would require a significant amount of
!!    memory. One major advantage of this method is that
!!    the indices can be used to access an entire user-defined type
!!    in sorted order. This makes this seemingly simple sort procedure
!!    usuable with the vast majority of user-defined types.  or other
!!    correlated data.
!!
!!##BACKGROUND
!!    From Leonard J. Moss of SLAC:
!!
!!    Here's a hybrid QuickSort I wrote a number of years ago. It's
!!    based on suggestions in Knuth, Volume 3, and performs much better
!!    than a pure QuickSort on short or partially ordered input arrays.
!!
!!    This routine performs an in-memory sort of the first N elements of
!!    array DATA, returning into array INDEX the indices of elements of
!!    DATA arranged in ascending order. Thus,
!!
!!       DATA(INDX(1)) will be the smallest number in array DATA;
!!       DATA(INDX(N)) will be the largest number in DATA.
!!
!!    The original data is not physically rearranged. The original order
!!    of equal input values is not necessarily preserved.
!!
!!    sort_quick_rx(3f) uses a hybrid QuickSort algorithm, based on several
!!    suggestions in Knuth, Volume 3, Section 5.2.2. In particular, the
!!    "pivot key" [my term] for dividing each subsequence is chosen to be
!!    the median of the first, last, and middle values of the subsequence;
!!    and the QuickSort is cut off when a subsequence has 9 or fewer
!!    elements, and a straight insertion sort of the entire array is done
!!    at the end. The result is comparable to a pure insertion sort for
!!    very short arrays, and very fast for very large arrays (of order 12
!!    micro-sec/element on the 3081K for arrays of 10K elements). It is
!!    also not subject to the poor performance of the pure QuickSort on
!!    partially ordered data.
!!
!!    Complex values are sorted by the magnitude of sqrt(r**2+i**2).
!!
!!    o Created: sortrx(3f): 15 Jul 1986, Len Moss
!!    o saved from url=(0044)http://www.fortran.com/fortran/quick_sort2.f
!!    o changed to update syntax from F77 style; John S. Urban 20161021
!!    o generalized from only real values to include other intrinsic types;
!!      John S. Urban 20210110
!!    o type(unicode_type) version JSU 2025-09-20. See M_sort for other types.
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program demo_sort_quick_rx
!!    use M_unicode, only : sort_quick_rx, unicode_type, assignment(=)
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0))'
!!    integer,parameter  :: isz=4
!!    type(unicode_type) :: rr(isz)
!!    integer            :: ii(isz)
!!    integer            :: i
!!       write(*,g)'sort array with sort_quick_rx(3f)'
!!       rr(1)="the"
!!       rr(2)="quick"
!!       rr(3)="brown"
!!       rr(4)="fox"
!!       call sort_quick_rx(rr,ii)
!!
!!       write(*,g)'original order'
!!       do i=1,size(rr)
!!          write(*,g)rr(i)%character()
!!       enddo
!!
!!       write(*,g)'sorted order'
!!       do i=1,size(rr)
!!          write(*,g)rr(ii(i))%character()
!!       enddo
!!
!!    end program demo_sort_quick_rx
!!
!!   Results:
!!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine sort_quick_rx(data,indx)

! ident_30="@(#) M_unicode sort_quick_rx(3f) indexed hybrid quicksort of a type(unicode_type) array"

type(unicode_type),intent(in)   :: data(:)
integer(kind=int32),intent(out) :: indx(:)
type(unicode_type)              :: datap

integer(kind=int32)             :: n
integer(kind=int32)             :: lstk(31),rstk(31),istk
integer(kind=int32)             :: l,r,i,j,p,indexp,indext

!  QuickSort Cutoff
!
!  Quit QuickSort-ing when a subsequence contains M or fewer elements and finish off at end with straight insertion sort.
!  According to Knuth, V.3, the optimum value of M is around 9.

integer,parameter :: M=9
!===================================================================================================================================
n=size(data)
if(size(indx).lt.n)then  ! if index is not big enough, only sort part of the data
  write(*,*)'*sort_quick_rx* ERROR: insufficient space to store index data'
  n=size(indx)
endif
!===================================================================================================================================
!  Make initial guess for INDEX

do i=1,n
   indx(i)=i
enddo

!  If array is short go directly to the straight insertion sort, else execute a QuickSort
if (N.gt.M)then
   !=============================================================================================================================
   !  QuickSort
   !
   !  The "Qn:"s correspond roughly to steps in Algorithm Q, Knuth, V.3, PP.116-117, modified to select the median
   !  of the first, last, and middle elements as the "pivot key" (in Knuth's notation, "K"). Also modified to leave
   !  data in place and produce an INDEX array. To simplify comments, let DATA[I]=DATA(INDX(I)).

   ! Q1: Initialize
   istk=0
   l=1
   r=n
   !=============================================================================================================================
   TOP: do

      ! Q2: Sort the subsequence DATA[L]..DATA[R].
      !
      !  At this point, DATA[l] <= DATA[m] <= DATA[r] for all l < L, r > R, and L <= m <= R.
      !  (First time through, there is no DATA for l < L or r > R.)

      i=l
      j=r

      ! Q2.5: Select pivot key
      !
      !  Let the pivot, P, be the midpoint of this subsequence, P=(L+R)/2; then rearrange INDX(L), INDX(P), and INDX(R)
      !  so the corresponding DATA values are in increasing order. The pivot key, DATAP, is then DATA[P].

      p=(l+r)/2
      indexp=indx(p)
      datap=data(indexp)

      if (data(indx(l)) .gt. datap) then
         indx(p)=indx(l)
         indx(l)=indexp
         indexp=indx(p)
         datap=data(indexp)
      endif

      if (datap .gt. data(indx(r))) then

         if (data(indx(l)) .gt. data(indx(r))) then
            indx(p)=indx(l)
            indx(l)=indx(r)
         else
            indx(p)=indx(r)
         endif

         indx(r)=indexp
         indexp=indx(p)
         datap=data(indexp)
      endif

      !  Now we swap values between the right and left sides and/or move DATAP until all smaller values are on the left and all
      !  larger values are on the right. Neither the left or right side will be internally ordered yet; however, DATAP will be
      !  in its final position.
      Q3: do
         ! Q3: Search for datum on left >= DATAP
         !   At this point, DATA[L] <= DATAP. We can therefore start scanning up from L, looking for a value >= DATAP
         !   (this scan is guaranteed to terminate since we initially placed DATAP near the middle of the subsequence).
         I=I+1
         if (data(indx(i)).lt.datap)then
            cycle Q3
         endif
         !-----------------------------------------------------------------------------------------------------------------------
         ! Q4: Search for datum on right <= DATAP
         !
         !   At this point, DATA[R] >= DATAP. We can therefore start scanning down from R, looking for a value <= DATAP
         !   (this scan is guaranteed to terminate since we initially placed DATAP near the middle of the subsequence).
         Q4: do
            j=j-1
            if (data(indx(j)).le.datap) then
               exit Q4
            endif
         enddo Q4
         !-----------------------------------------------------------------------------------------------------------------------
         ! Q5: Have the two scans collided?
         if (i.lt.j) then
            ! Q6: No, interchange DATA[I] <--> DATA[J] and continue
            indext=indx(i)
            indx(i)=indx(j)
            indx(j)=indext
            cycle Q3
         else
            ! Q7: Yes, select next subsequence to sort
            !   At this point, I >= J and DATA[l] <= DATA[I] == DATAP <= DATA[r], for all L <= l < I and J < r <= R.
         !   If both subsequences are more than M elements long, push the longer one on the stack
            !   and go back to QuickSort the shorter; if only one is more than M elements long, go back and QuickSort it;
         !   otherwise, pop a subsequence off the stack and QuickSort it.
            if (r-j .ge. i-l .and. i-l .gt. m) then
               istk=istk+1
               lstk(istk)=j+1
               rstk(istk)=r
               r=i-1
            elseif (i-l .gt. r-j .and. r-j .gt. m) then
               istk=istk+1
               lstk(istk)=l
               rstk(istk)=i-1
               l=j+1
            elseif (r-j .gt. m) then
               l=j+1
            elseif (i-l .gt. m) then
               r=i-1
            else
               ! Q8: Pop the stack, or terminate QuickSort if empty
               if (istk.lt.1) then
                  exit TOP
               endif
               l=lstk(istk)
               r=rstk(istk)
               istk=istk-1
            endif
            cycle TOP
         endif
         ! never get here, as cycle Q3 or cycle TOP
      enddo Q3
      exit TOP
   enddo TOP
endif
!===================================================================================================================================
! Q9: Straight Insertion sort
do i=2,n
   if (data(indx(i-1)) .gt. data(indx(i))) then
      indexp=indx(i)
      datap=data(indexp)
      p=i-1
      INNER: do
         indx(p+1) = indx(p)
         p=p-1
         if (p.le.0)then
            exit INNER
         endif
         if (data(indx(p)).le.datap)then
            exit INNER
         endif
      enddo INNER
      indx(p+1) = indexp
   endif
enddo
!===================================================================================================================================
!     All done
end subroutine sort_quick_rx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function upper(str) result (string)

! ident_25="@(#) M_unicode upper(3f) returns an uppercase string"

type(unicode_type), intent(in) :: str                 ! input string to convert to all uppercase
type(unicode_type)             :: string              ! output string that contains no miniscule letters
integer                        :: i                   ! loop counter
integer, parameter  :: ade_a = iachar('a'), ade_z = iachar('z')
integer, parameter  :: diff = iachar('A') - iachar('a')

   string=str
   do i=1,len(str)                           ! step thru each letter in the string in specified range
      select case(str%codes(i))
      case(ade_a:ade_z)
         string%codes(i) = str%codes(i) + diff
      end select
   enddo

   if(len(str).eq.0)string = str

end function upper

elemental pure function lower(str) result (string)

! ident_25="@(#) M_unicode lower(3f) returns an lowercase string"

type(unicode_type), intent(in) :: str                 ! input string to convert to all lowercase
type(unicode_type)             :: string              ! output string that contains no miniscule letters
integer                        :: i                   ! loop counter
integer, parameter  :: ade_a = iachar('A'), ade_z = iachar('Z')
integer, parameter  :: diff = iachar('A') - iachar('a')

   string=str
   do i=1,len(str)                           ! step thru each letter in the string in specified range
      select case(str%codes(i))
      case(ade_a:ade_z)
         string%codes(i) = str%codes(i) - diff
      end select
   enddo

   if(len(str).eq.0)string = str

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
impure subroutine split_tokens(string, set, tokens, separator)
! Splits a string into tokens using characters in set as token delimiters.
! If present, separator contains the array of token delimiters.
type(unicode_type), intent(in)                         :: string
type(unicode_type), intent(in)                         :: set
type(unicode_type), allocatable, intent(out)           :: tokens(:)
type(unicode_type), allocatable, intent(out), optional :: separator(:)

integer, allocatable                                   :: first(:), last(:)
integer                                                :: n
integer                                                :: imax
! AUTHOR  : Milan Curcic, "milancurcic@hey.com"
! LICENSE : MIT
! VERSION : version 0.1.0, copyright 2020, Milan Curcic

    call split_first_last(string, set, first, last)
    ! maxval() of a zero-size array is set to a flag value not zero or length of character string
    if(size(first).eq.0)then
       imax=0
    else
       imax=maxval(last-first)+1
    endif
    allocate(tokens(size(first)))

    do n = 1,size(tokens)
      tokens(n) = string%character(first(n),last(n),1)
    enddo

    if (present(separator)) then
      allocate(separator(size(tokens) - 1))
      do n = 1,size(tokens) - 1
        separator(n) = string%character(first(n+1)-1,first(n+1)-1,1)
      enddo
    endif

end subroutine split_tokens
!===================================================================================================================================
impure subroutine split_first_last(string, set, first, last)
! Computes the first and last indices of tokens in input string, delimited
! by the characters in set, and stores them into first and last output
! arrays.
type(unicode_type), intent(in)    :: string
type(unicode_type), intent(in)    :: set
integer, allocatable, intent(out) :: first(:)
integer, allocatable, intent(out) :: last(:)

type(unicode_type)                :: set_array(len(set))
logical, dimension(len(string))   :: is_first, is_last, is_separator
integer                           :: i
integer                           :: n
integer                           :: slen
! AUTHOR   : Milan Curcic, "milancurcic@hey.com"
! LICENSE  : MIT
! VERSION  : version 0.1.0, copyright 2020, Milan Curcic
! MODIFIED : 2025-09-21 JSU

    slen = len(string)

    do n = 1,len(set)
      set_array(n) = set%character(n,n)
    enddo

    FINDIT: do n = 1,slen
      do i=1,len(set)
         is_separator(n)=.false.
         if( string%character(n,n) == set_array(i)%character() )then
            is_separator(n) = .true.
            exit
         endif
      enddo
    enddo FINDIT

    is_first = .false.
    is_last = .false.

    if (.not. is_separator(1)) is_first(1) = .true.

    do concurrent (n = 2:slen-1)
      if (.not. is_separator(n)) then
        if (is_separator(n - 1)) is_first(n) = .true.
        if (is_separator(n + 1)) is_last(n) = .true.
      else
        if (is_separator(n - 1)) then
          is_first(n) = .true.
          is_last(n-1) = .true.
        endif
      endif
    enddo

    if (.not. is_separator(slen)) is_last(slen) = .true.

    first = pack([(n, n = 1, slen)], is_first)
    last = pack([(n, n = 1, slen)], is_last)

  end subroutine split_first_last
!===================================================================================================================================
impure subroutine split_pos(string, set, pos, back)
! If back is absent, computes the leftmost token delimiter in string whose
! position is > pos. If back is present and true, computes the rightmost
! token delimiter in string whose position is < pos. The result is stored
! in pos.
type(unicode_type), intent(in) :: string
type(unicode_type), intent(in) :: set
integer, intent(in out)        :: pos
logical, intent(in), optional  :: back

logical                        :: backward
type(unicode_type)             :: set_array(len(set))
integer                        :: i
integer                        :: result_pos
integer                        :: n
! AUTHOR   : Milan Curcic, "milancurcic@hey.com"
! LICENSE  : MIT
! VERSION  : version 0.1.0, copyright 2020, Milan Curcic
! MODIFIED : 2025-09-21 JSU

    backward = .false.
    if (present(back)) backward = back

    do n = 1,len(set)
      set_array(n) = set%character(n,n)
    enddo

    if (backward) then
      result_pos = 0
      FINDIT: do n = pos - 1, 1, -1
        do i=1,len(set)
           if (string%character(n,n) == set_array(i)%character() ) then
             result_pos = n
             exit FINDIT
           endif
        enddo
      enddo FINDIT
    else
      result_pos = len(string) + 1
      GETPOS: do n = pos + 1, len(string)
        do i=1,len(set)
           if (string%character(n,n) == set_array(i)%character() ) then
             result_pos = n
             exit GETPOS
           endif
        enddo
      enddo GETPOS
    endif

    pos = result_pos

end subroutine split_pos
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function scan(string,set,back) result(pos)
!@(#) M_unicode:scan(3f)  Scan a string for the presence of a set of characters
type(unicode_type),intent(in) :: string
type(unicode_type),intent(in) :: set
logical,intent(in),optional   :: back
logical                       :: back_local
integer                       :: pos
integer                       :: value
integer                       :: i
   back_local=.false.
   if(present(back))back_local=back
   pos=0
   do i=1,len(set)
      value=set%codes(i)
      pos = findloc(string%codes, value, dim=1, back=back_local)
      if(pos.ne.0)exit
   enddo

end function scan
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental impure function verify(string,set,back) result(result)
!@(#)  verify(3) ‐ [CHARACTER:SEARCH] Position of a character in a string that does not appear in a given set of characters.
type(unicode_type),intent(in) :: string
type(unicode_type),intent(in) :: set
type(unicode_type)            :: str
logical,intent(in),optional   :: back
integer                       :: result
integer                       :: pos
integer                       :: i
   result=0
   do i=1,len(string)
      str=string%character(i,i)
      pos=index(set,str,back)
      if(pos.eq.0)then
         result=i
         exit
      endif
   enddo
end function verify
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_upper(self) result (string_out)
class(unicode_type),intent(in)     :: self
type(unicode_type)                 :: string_out
   string_out=upper(self)
end function oop_upper
!===================================================================================================================================
function oop_lower(self) result (string_out)
class(unicode_type),intent(in)     :: self
type(unicode_type)                 :: string_out
   string_out=lower(self)
end function oop_lower
!===================================================================================================================================
function oop_adjustl(self) result (string_out)
class(unicode_type),intent(in)     :: self
type(unicode_type)                 :: string_out
   string_out=adjustl_str(self)
end function oop_adjustl
!===================================================================================================================================
function oop_adjustr(self) result (string_out)

! ident_13="@(#) M_strs oop_adjustr(3f) adjust string to right"

class(unicode_type),intent(in)     :: self
type(unicode_type)                 :: string_out
   string_out=adjustr_str(self)
end function oop_adjustr
!===================================================================================================================================
function oop_character(self,first,last,step) result(str_out)
class(unicode_type), intent(in) :: self
character(len=:),allocatable    :: str_out
integer,intent(in),optional     :: first, last, step
integer                         :: start, end, inc
type(unicode_type)              :: temp
   if(present(step))then;  inc=step;    else; inc=1;         endif
   if(present(first))then; start=first; else; start=1;       endif
   if(present(last))then;  end=last;    else; end=len(self); endif
   temp=self%codes(start:end:inc)
   str_out=char_str(temp)
end function oop_character
!===================================================================================================================================
function oop_bytes(self,first,last,step) result(bytes_out)
class(unicode_type), intent(in) :: self
integer,intent(in),optional     :: first, last, step
character(len=1),allocatable    :: bytes_out(:)
   bytes_out=s2a(oop_character(self,first,last,step))
end function oop_bytes
!===================================================================================================================================
function oop_codepoint(self,first,last,step) result(codes_out)
class(unicode_type), intent(in) :: self
integer,allocatable             :: codes_out(:)
integer,intent(in),optional     :: first, last, step
integer                         :: start, end, inc
type(unicode_type)              :: temp
   if(present(step))then;  inc=step;    else; inc=1;         endif
   if(present(first))then; start=first; else; start=1;       endif
   if(present(last))then;  end=last;    else; end=len(self); endif
   codes_out=self%codes(start:end:inc)
end function oop_codepoint
!===================================================================================================================================
impure function oop_verify(self,set,back) result(pos)
class(unicode_type),intent(in)  ::  self
class(unicode_type),intent(in)  ::  set
logical,intent(in),optional     ::  back
integer                         ::  pos
   pos=verify(self,set,back=back)
end function oop_verify
!===================================================================================================================================
pure function oop_scan(self,set,back) result(pos)
class(unicode_type),intent(in)  ::  self
class(unicode_type),intent(in)  ::  set
logical,optional,intent(in)     ::  back
integer                         ::  pos
   pos=scan(self,set,back=back)
end function oop_scan
!===================================================================================================================================
function oop_tokenize(self,set) result(tokens)
class(unicode_type),intent(in) :: self
type(unicode_type),intent(in)  :: set
type(unicode_type),allocatable :: tokens(:)
integer,allocatable            :: begin(:)
integer,allocatable            :: end(:)
integer                        :: i
   call split(self,set,begin,end)
   allocate(tokens(size(begin)))
   do i=1,size(begin)
      tokens(i)=self%character(begin(i),end(i))
   enddo
end function oop_tokenize
!===================================================================================================================================
function oop_split(self,set) result(tokens)
class(unicode_type),intent(in) :: self
type(unicode_type),intent(in)  :: set
type(unicode_type),allocatable :: tokens(:)
   call split(self,set,tokens)
end function oop_split
!===================================================================================================================================
function oop_sort(self) result(indx)
class(unicode_type), intent(in) :: self(:)
integer                         :: indx(size(self))
   call sort_quick_rx(self,indx)
end function oop_sort
!===================================================================================================================================
pure function oop_trim(self) result(string_out)
class(unicode_type), intent(in) :: self
type(unicode_type)              :: string_out
   string_out=trim(self)
end function oop_trim
!===================================================================================================================================
pure function oop_len_trim(self) result(len_trim_out)
class(unicode_type), intent(in) :: self
integer                         :: len_trim_out
   len_trim_out=len_trim(self)
end function oop_len_trim
!===================================================================================================================================
pure function oop_len(self) result(len_out)
class(unicode_type), intent(in) :: self
integer                         :: len_out
   len_out=len(self)
end function oop_len
!===================================================================================================================================
impure function oop_index(self,substring) result(index_out)
class(unicode_type), intent(in) :: self
class(*), intent(in)            :: substring
integer                         :: index_out
   select type(substring)
      type is (character(len=*))
         index_out=index(self,unicode_type(substring))
      type is (unicode_type)
         index_out=index(self,substring)
      class default
         stop '<ERROR>*oop_index* unknown type'
   end select
end function oop_index
!===================================================================================================================================
impure function oop_eq(self,string) result(is_eq)
class(unicode_type), intent(in) :: self
class(*), intent(in)            :: string
logical                         :: is_eq
   select type(string)
      type is (character(len=*))
         is_eq=leq_str_char(self,string)
      type is (unicode_type)
         is_eq=leq_str_str(self,string)
      class default
         stop '<ERROR>*oop_eq* unknown type'
   end select
end function oop_eq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_unicode
