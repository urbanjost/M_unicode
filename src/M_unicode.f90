module M_unicode
! Unicode-related procedures not requiring compiler support of ISO-10646
! first presented in https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949
! including enhancements and latin support from Francois Jacq, 2025-08
!
use iso_fortran_env, only: error_unit, stderr=>error_unit
implicit none

private
public  :: utf8_to_codepoints,  codepoints_to_utf8
public  :: isolatin_to_unicode, unicode_to_isolatin
public  :: utf8_to_isolatin,    isolatin_to_utf8

private :: a2s, s2a

public :: unicode_type
public :: assignment(=)

interface utf8_to_codepoints
   module procedure utf8_to_codepoints_str,utf8_to_codepoints_chars
end interface utf8_to_codepoints

interface codepoints_to_utf8
   module procedure codepoints_to_utf8_str,codepoints_to_utf8_chars
end interface codepoints_to_utf8

! Assign a character sequence to a string.
interface assignment(=)
   module procedure :: assign_str_char
   module procedure :: assign_str_codes
end interface assignment(=)

interface character
   module procedure :: char_str
   module procedure :: char_str_range
   module procedure :: char_str_range_step
end interface character
public :: character

! INTRINSIC COMPATIBILITY
interface adjustl;      module procedure :: adjustl_str;  end interface adjustl
interface adjustr;      module procedure :: adjustr_str;  end interface adjustr
interface len;          module procedure :: len_str;      end interface len
interface len_trim;     module procedure :: len_trim_str; end interface len_trim
interface repeat;       module procedure :: repeat_str;   end interface repeat
interface trim;         module procedure :: trim_str;     end interface trim
interface index;        module procedure :: index_str_str,  index_str_char,  index_char_str;  end interface index

public :: adjustl, adjustr, index, len, len_trim, repeat, trim

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

public :: lle, llt, lne, leq, lgt, lge
public :: operator(<=), operator(<), operator(/=), operator(==), operator(>), operator(>=), operator(//)

type :: unicode_type ! Unicode string type holding an arbitrary sequence of integer codes.
   !sequence ! not used for storage association; a kludge to prevent extending this type.
   private
   integer, allocatable :: codes(:)
contains
   ! METHODS:
   procedure  ::  character      =>  oop_character
   procedure  ::  bytes          =>  oop_bytes

   procedure  ::  adjustl        =>  oop_adjustl
   procedure  ::  adjustr        =>  oop_adjustr
   procedure  ::  index          =>  oop_index
   procedure  ::  len            =>  oop_len
   procedure  ::  len_trim       =>  oop_len_trim
   procedure  ::  trim           =>  oop_trim

!  procedure  ::  split          =>  oop_split
!  procedure  ::  tokenize       =>  oop_tokenize
!  procedure  ::  scan           =>  oop_scan
!  procedure  ::  verify         =>  oop_verify
!  procedure  ::  sort           =>  oop_sort
!  procedure  ::  upper          =>  oop_upper
!  procedure  ::  lower          =>  oop_lower

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
pure subroutine isolatin_to_unicode(isolatin,unicode,nerr)
character           ,intent(in)  :: isolatin(:)
integer, allocatable,intent(out) :: unicode(:)
integer             ,intent(out) :: nerr

integer :: i, n, char_code

   nerr = 0
   n = size(isolatin)
   allocate(unicode(n))

   do i = 1, n
      char_code = ichar(isolatin(i))
      ! Only 8 characters do not correspond to unicode
      select case (char_code)
      case (164); unicode(i) = 8364 ! Symbol Euro
      case (166); unicode(i) = 352  ! S caron
      case (168); unicode(i) = 353  ! s caron
      case (180); unicode(i) = 381  ! Z caron
      case (184); unicode(i) = 382  ! z caron
      case (188); unicode(i) = 338  ! OE majuscule
      case (189); unicode(i) = 339  ! oe minuscule
      case (190); unicode(i) = 376  ! Y trema
      case default
         unicode(i) = char_code
      end select
   enddo

end subroutine isolatin_to_unicode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine unicode_to_isolatin(unicode,isolatin,nerr)

integer              ,intent(in)  :: unicode(:)
character,allocatable,intent(out) :: isolatin(:)
integer              ,intent(out) :: nerr
integer                           :: i, n, cp

   nerr=0
   n = size(unicode)
   allocate(isolatin(n))

   do i = 1, n
      cp = unicode(i)
      select case (cp) ! 8 special characters
      case (8364); isolatin(i) = char(164) ! Euro
      case (352);  isolatin(i) = char(166) ! S caron
      case (353);  isolatin(i) = char(168) ! s caron
      case (381);  isolatin(i) = char(180) ! Z caron
      case (382);  isolatin(i) = char(184) ! z caron
      case (338);  isolatin(i) = char(188) ! OE majuscule
      case (339);  isolatin(i) = char(189) ! oe minuscule
      case (376);  isolatin(i) = char(190) ! Y trema
      case (0:163, 165, 167, 169:179, 181:183, 185:187, 191:255)
         isolatin(i) = char(cp)
      case default
         nerr=nerr+1
         isolatin(i) = '?' ! replacement character
      end select
   enddo

end subroutine unicode_to_isolatin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine isolatin_to_utf8(isolatin,utf8,nerr)
character            ,intent(in)  :: isolatin(:)
character,allocatable,intent(out) :: utf8(:)
integer              ,intent(out) :: nerr
integer,allocatable :: unicode(:)

   call isolatin_to_unicode(isolatin,unicode,nerr)
   call codepoints_to_utf8(unicode,utf8,nerr)

end subroutine isolatin_to_utf8
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_isolatin(utf8,isolatin,nerr)
character            ,intent(in)  :: utf8(:)
character,allocatable,intent(out) :: isolatin(:)
integer              ,intent(out) :: nerr
integer,allocatable :: unicode(:)

   call utf8_to_codepoints(utf8,unicode,nerr)
   call unicode_to_isolatin(unicode,isolatin,nerr)

end subroutine utf8_to_isolatin
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
type(unicode_type), intent(in) :: string
character(len=:),allocatable   :: aline
integer                        :: nerr

call codepoints_to_utf8_str(string%codes,aline,nerr)

end function char_str

pure function char_str_range(string, first, last) result(aline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: first
integer, intent(in)            :: last
!character(len=last-first+1)    :: aline
character(len=:),allocatable   :: aline
integer                        :: nerr

   call codepoints_to_utf8_str(string%codes(first:last),aline,nerr)

end function char_str_range

pure function char_str_range_step(string, first, last, step) result(aline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: first
integer, intent(in)            :: last
integer, intent(in)            :: step
!character(len=last-first+1)    :: aline
character(len=:),allocatable   :: aline
integer                        :: nerr

   call codepoints_to_utf8_str(string%codes(first:last:step),aline,nerr)

end function char_str_range_step
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
elemental function adjustr_str(string) result(adjusted)
type(unicode_type), intent(in) :: string
type(unicode_type)             :: adjusted
integer                        :: last

   last=len_trim_str(string)
   adjusted%codes=cshift(string%codes,-(size(adjusted%codes)-last-1))

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

elemental function index_str_str(string, substring) result(foundat)
type(unicode_type), intent(in) :: string
type(unicode_type), intent(in) :: substring
integer                        :: foundat
integer                        :: i
integer                        :: strlen
integer                        :: sublen

   strlen=string%len()
   sublen=substring%len()
   foundat=0

   do i=1,strlen - sublen + 1
      if ( all(string%codes(i:i+sublen-1) .eq. substring%codes) )then
         foundat=i
         exit
      endif
   enddo

end function index_str_str

elemental function index_str_char(string, substring) result(foundat)
type(unicode_type), intent(in) :: string
character(len=*), intent(in)   :: substring
integer                        :: foundat
   foundat = index_str_str(string, unicode_type(substring))
end function index_str_char

elemental function index_char_str(string, substring) result(foundat)
character(len=*), intent(in)   :: string
type(unicode_type), intent(in) :: substring
integer                        :: foundat
   foundat = index_str_str(unicode_type(string), substring )
end function index_char_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_adjustl(self) result (string_out)

! ident_12="@(#) M_strs oop_adjustl(3f) adjust string to left"

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
function oop_character(self,first,last,step) result(bytes_out)
class(unicode_type), intent(in) :: self
character(len=:),allocatable    :: bytes_out
integer,intent(in),optional     :: first, last, step
integer                         :: start, end, inc
type(unicode_type)              :: temp
   if(present(step))then;  inc=step;    else; inc=1;         endif
   if(present(first))then; start=first; else; start=1;       endif
   if(present(last))then;  end=last;    else; end=len(self); endif
   temp=self%codes(start:end:inc)
   bytes_out=char_str(temp)
end function oop_character
!===================================================================================================================================
pure function oop_trim(self) result(string_out)
class(unicode_type), intent(in) :: self
type(unicode_type)              :: string_out
   string_out=trim(self)
end function oop_trim
!===================================================================================================================================
pure function oop_bytes(self) result(bytes_out)
class(unicode_type), intent(in) :: self
character(len=:),allocatable    :: bytes_out(:)
   bytes_out=s2a(char_str(self))
end function oop_bytes
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
