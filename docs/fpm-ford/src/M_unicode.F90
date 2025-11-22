!-----------------------------------------------------------------------------------------------------------------------------------
#define  __INTEL_COMP        1
#define  __GFORTRAN_COMP     2
#define  __NVIDIA_COMP       3
#define  __NAG_COMP          4
#define  __flang__           5
#define  __UNKNOWN_COMP   9999

#define FLOAT128

#ifdef __INTEL_COMPILER
#   define __COMPILER__ __INTEL_COMP
#elif __GFORTRAN__ == 1
#   define __COMPILER__ __GFORTRAN_COMP
#elif __flang__
#   undef FLOAT128
#   define __COMPILER__ __LLVM_FLANG_COMP
#elif __NVCOMPILER
#   undef FLOAT128
#   define __COMPILER__ __NVIDIA_COMP
#else
#   define __COMPILER__ __UNKNOWN_COMP
#   warning  NOTE: UNKNOWN COMPILER
#endif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     M_unicode(3f) - [M_unicode::INTRO] Unicode string module
!     (LICENSE:MIT)
! 
! DESCRIPTION
!    The M_unicode(3f) module is a collection of Fortran string
!    methods that work with UTF-8-encoded as well as ASCII-7 data via the
!    user-defined type "unicode_type". The type supports allocatable ragged
!    arrays where each element may be of differing length.
! 
!    M_unicode(3f) includes routines for parsing, tokenizing, changing
!    case, substituting new strings for substrings, locating strings with
!    simple wildcard expressions, removing tabs and line terminators and
!    other string manipulations.
! 
!    The M_unicode(3fm) module also supplements the Fortran built-in
!    intrinsics with overloads of operators and intrinsics that allow
!    type(unicode_type) to be used with intrinsic names in much the same
!    manner the intrinsics operate on CHARACTER variables.
! 
!    Overloads of assignment, logical comparisons, and concatenation
!    using the // operator with strings (and other types) are included
!    as well as the intrinsics to make use of type(unicode_type) largely
!    consistent with standard CHARACTER string manipulations.
! 
!    Nearly all the methods are available using OOP syntax as well as
!    procedurally.
! 
!    The type components are not public to allow for use of the same
!    user code when using other modules such as M_utf8 which ultimately
!    will provide the same user interface but internally using ISO_10646
!    internal encoding instead of an array of integers containing codepoints
!    (which is what M_unicode uses). This has the drawback of not permitting
!    easy use of array syntax directly on the codepoint array. Perhaps this
!    decision will change but in the meantime several methods such as REPLACE
!    and CHARACTER and SUB
! 
! SYNOPSIS
! 
!   public methods:
! 
!    TOKENS
! 
!     split     subroutine parses string using specified delimiter
!               characters into tokens
!     tokenize  Parse a string into tokens.
! 
!    EDITING
! 
!     replace   function non-recursively globally replaces old
!               substring with new substring
! 
!     transliterate  replace characters from old set with new set
! 
!    CASE
! 
!     upper   function converts string to uppercase
!     lower   function converts string to miniscule
! 
!    STRING LENGTH
! 
!     len        return the length of a string in glyphs
!     len_trim   find location of last non-whitespace glyph
! 
!    PADDING
! 
!     pad        pad string to at least specified length with pattern string
! 
!    WHITE SPACE
! 
!     trim         Remove trailing blank characters of a string
!     expandtabs   expand tab characters
!     adjustl      Left adjust a string
!     adjustr      Right adjust a string
! 
!    ENCODING
! 
!     character(STRING,start,end,inc)  converts a string to type CHARACTER.
! 
!     escape                           expand C-like escape strings
! 
!     codepoints_to_utf8(codepoints,utf8,nerr)  subroutine to convert
!                                               codepoints to UTF-8 bytes
!     utf8_to_codepoints(utf8,codepoints,nerr)  subroutine to convert
!                                               UTF-8 bytes to codepoints
! 
!     STRING%character(start,end,inc)  OOP syntax for converting a string to
!                                      type CHARACTER.
!     STRING%bytes(start,end,inc)      Convert to an array of
!                                      CHARACTER(len=1) bytes.
!     STRING%codepoint(start,end,inc)  converts a string to an INTEGER
!                                      array of Unicode codepoints
! 
! 
!     char       converts an integer codepoint into a character
!     ichar      converts a type(unicode_type) glyph into an integer
!                codepoint
! 
!    NUMERIC STRINGS
! 
!     fmt       convert intrinsic to string using optional format
! 
!    CHARACTER TESTS
! 
!     ! based on Unicode codepoint, not dictionary order
! 
!     lgt       Lexical greater than
!     lge       Lexical greater than or equal
!     leq       Lexical equal
!     lne       Lexical not equal
!     lle       Lexical less than or equal
!     llt       Lexical less than
! 
! 
!    IO
! 
!     readline  read a text line from a file
! 
!    LOCATION
! 
!     index     Position of a substring within a string
!     scan      Scan a string for the presence of a set
!               of characters
!     verify    Scan a string for the absence of a set of characters
! 
!    CONCATENATION
! 
!    join              join elements of an array into a single string
!    operator(.cat.),
!    operator(//)      concatenate strings and/or convert intrinsics to
!                      strings and concatenate
! 
!    SYSTEM
! 
!     get_env    Get environment variable
!     get_arg    Get command line argument
! 
!    MISCELLANEOUS
! 
!     repeat        Repeated string concatenation
!     sort          Sort by Unicode codepoint value (not dictionary order)
! 
!    BASE CONVERSION
!    QUOTES
!    NONALPHA
! 
!    OOPS INTERFACE
! 
!     An OOP (Object-Oriented Programming) interface to the M_unicode(3fm)
!     module provides an alternative interface to all the same procedures
!     except for SORT(3f) and CHAR(3f).
! 
! SEE ALSO
!     All the procedure descriptions are conglomerated into the single file
!     "manual.txt" for simple access not requiring access to man-pages or
!     browsers.
! 
!     There are additional routines in other GPF modules for working with
!     expressions (M_calculator), time strings (M_time), random strings
!     (M_random, M_uuid), lists (M_list), and interfacing with the C regular
!     expression library (M_regex).
! 
! EXAMPLES
! 
!     Each of the procedures includes an example program in the example/
!     directory as well as a corresponding man(1) page for the procedure.
! 
!  Sample program:
! 
!    program demo_M_unicode
!    use,intrinsic :: iso_fortran_env, only : stdout=>output_unit
!    use M_unicode,only : tokenize, replace, character, upper, lower, len
!    use M_unicode,only : unicode_type, assignment(=), operator(//)
!    use M_unicode,only : ut => unicode_type, ch => character
!    use M_unicode,only : write(formatted)
!    type(unicode_type)             :: string
!    type(unicode_type)             :: numeric, uppercase, lowercase
!    type(unicode_type),allocatable :: array(:)
!    character(len=*),parameter     :: all='(g0)'
!    !character(len=*),parameter     :: uni='(DT)'
!    uppercase='АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ'
!    lowercase='абвгґдеєжзиіїйклмнопрстуфхцчшщьюя'
!    numeric='0123456789'
!     !
!     string=uppercase//numeric//lowercase
!     !
!     print all, 'Original string:'
!     print all, ch(string)
!     print all, 'length in bytes :',len(string%character())
!     print all, 'length in glyphs:',len(string)
!     print all
!     !
!     print all, 'convert to all uppercase:'
!     print all, ch( UPPER(string) )
!     print all
!     !
!     print all, 'convert to all lowercase:'
!     print all, ch( string%LOWER() )
!     print all
!     !
!     print all, 'tokenize on spaces ... '
!     call TOKENIZE(string,ut(' '),array)
!     print all, '... writing with A or G format:',character(array)
!     !print uni, ut('... writing with DT format'),array
!     print all
!     !
!     print all, 'case-insensitive replace:'
!     print all, ch( &
!     & REPLACE(string, &
!     & ut('клмнопрс'), &
!     & ut('--------'), &
!     & ignorecase=.true.) )
!     !
!     print all
!     !
!    end program demo_M_unicode
! 
!   Results:
! 
!    > Original string:
!    > АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ0123456789 ...
!    > абвгґдеєжзиіїйклмнопрстуфхцчшщьюя
!    > length in bytes :
!    > 144
!    > length in glyphs:
!    > 78
!    >
!    > convert to all uppercase:
!    > АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ0123456789 ...
!    > АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ
!    >
!    >
!    > tokenize on spaces ...
!    > ... writing with A or G format:
!    > АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ
!    > 0123456789
!    > абвгґдеєжзиіїйклмнопрстуфхцчшщьюя
!    > ... writing with DT format
!    > АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ
!    > 0123456789
!    > абвгґдеєжзиіїйклмнопрстуфхцчшщьюя
!    >
!    > case-insensitive replace:
!    > АБВГҐДЕЄЖЗИІЇЙ--------ТУФХЦЧШЩЬЮЯ0123456789 ...
!    > абвгґдеєжзиіїй--------туфхцчшщьюя
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_unicode__base
!
! Unicode-related procedures not requiring compiler support of ISO-10646
! first presented in https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949
! including enhancements and Latin support from Francois Jacq, 2025-08
!
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use,intrinsic :: iso_fortran_env, only : stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none

private
PUBLIC :: unicode_type
PUBLIC :: UTF8_TO_CODEPOINTS
PUBLIC :: CODEPOINTS_TO_UTF8
public :: character
public :: sort
public :: upper
public :: lower
public :: expandtabs
public :: escape
public :: fmt
PUBLIC :: AFMT
public :: replace
public :: transliterate
public :: pad
public :: join
public :: readline
public :: sub

public :: adjustl
public :: adjustr
public :: index
public :: len
public :: len_trim
public :: repeat
public :: trim
public :: split
public :: tokenize
public :: scan
public :: verify
public :: ichar
public :: get_env
public :: get_arg
PUBLIC :: LLE, LLT, LNE, LEQ, LGT, LGE
PUBLIC :: OPERATOR(<=), OPERATOR(<), OPERATOR(/=), OPERATOR(==), OPERATOR(>), OPERATOR(>=)
public :: assignment(=)

PUBLIC :: write(formatted)

! just for use in the parent module
public :: concat_g_g

private :: a2s, s2a
private :: binary_search
private :: section_uu
private :: section_ua
private :: section_au
private :: section_aa

interface utf8_to_codepoints
   module procedure utf8_to_codepoints_str,utf8_to_codepoints_chars
end interface utf8_to_codepoints

interface codepoints_to_utf8
   module procedure codepoints_to_utf8_str,codepoints_to_utf8_chars
end interface codepoints_to_utf8

interface sort
   module procedure :: sort_quick_rx
end interface sort

interface verify
   module procedure :: verify_uu
   module procedure :: verify_ua
   module procedure :: verify_au
end interface verify

interface get_arg
   module procedure :: get_arg_ia
   module procedure :: get_arg_iu
end interface get_arg

interface get_env
   module procedure :: get_env_uu
   module procedure :: get_env_ua
   module procedure :: get_env_au
   module procedure :: get_env_aa
end interface get_env

interface escape
   module procedure :: escape_uu
   module procedure :: escape_ua
   module procedure :: escape_au
   module procedure :: escape_aa
end interface escape

interface scan
   module procedure :: scan_uu
   module procedure :: scan_ua
end interface scan

interface fmt
   module procedure :: fmt_ga, fmt_gs
end interface fmt

interface tokenize
   module procedure :: split_first_last, split_first_last_uaii, split_pos, split_pos_uail, split_tokens, split_tokens_uauu
end interface tokenize

interface split
   module procedure :: split_first_last, split_first_last_uaii, split_pos, split_pos_uail, split_tokens, split_tokens_uauu
end interface split

! Assign a character sequence to a string.
interface assignment(=)
   module procedure :: assign_str_char
   module procedure :: assign_strs_char
   module procedure :: assign_str_code
   module procedure :: assign_str_codes
   module procedure :: assign_char_str
   module procedure :: assign_ints_str
end interface assignment(=)

interface character
   module procedure :: str_to_char,            strs_to_chars
   module procedure :: str_to_char_pos,        strs_to_chars_pos
   module procedure :: str_to_char_range,      strs_to_chars_range
   module procedure :: str_to_char_range_step, strs_to_chars_range_step
end interface character

interface replace
   module procedure :: replace_uuu, replace_uua, replace_uaa, replace_uau
   module procedure :: replace_aaa, replace_aua, replace_aau, replace_auu
   module procedure :: section_uu, section_ua, section_au, section_aa
end interface replace

interface transliterate
   module procedure :: transliterate_uuu, transliterate_uua, transliterate_uaa, transliterate_uau
   module procedure :: transliterate_aaa, transliterate_aua, transliterate_aau, transliterate_auu
end interface transliterate

! INTRINSIC COMPATIBILITY
interface adjustl;   module procedure :: adjustl_str;   end interface adjustl
interface adjustr;   module procedure :: adjustr_str;   end interface adjustr
interface len;       module procedure :: len_str;       end interface len
interface len_trim;  module procedure :: len_trim_str;  end interface len_trim
interface repeat;    module procedure :: repeat_str;    end interface repeat
interface trim;      module procedure :: trim_str;      end interface trim
interface ichar;     module procedure :: ichar_str;     end interface ichar
interface index;     module procedure :: index_str_str, index_str_char, index_char_str; end interface index

interface lle;          module procedure :: lle_str_str,   lle_str_char,  lle_char_str;  end interface lle
interface llt;          module procedure :: llt_str_str,   llt_str_char,  llt_char_str;  end interface llt
interface lne;          module procedure :: lne_char_str,  lne_str_char,  lne_str_str;   end interface lne
interface leq;          module procedure :: leq_char_str,  leq_str_char,  leq_str_str;   end interface leq
interface lgt;          module procedure :: lgt_str_str,   lgt_str_char,  lgt_char_str;  end interface lgt
interface lge;          module procedure :: lge_str_str,   lge_str_char,  lge_char_str;  end interface lge
interface operator(<=); module procedure :: lle_str_str,   lle_str_char,  lle_char_str;  end interface operator(<=)
interface operator(<);  module procedure :: llt_str_str,   llt_str_char,  llt_char_str;  end interface operator(<)
interface operator(/=); module procedure :: lne_char_str,  lne_str_char,  lne_str_str;   end interface operator(/=)
interface operator(==); module procedure :: leq_char_str,  leq_str_char,  leq_str_str;   end interface operator(==)
interface operator(>);  module procedure :: lgt_str_str,   lgt_str_char,  lgt_char_str;  end interface operator(>)
interface operator(>=); module procedure :: lge_str_str,   lge_str_char,  lge_char_str;  end interface operator(>=)

interface operator(.cat.)
   module procedure :: concat_uu_
end interface operator(.cat.)

type :: unicode_type ! Unicode string type holding an arbitrary sequence of integer codes.
   !sequence ! not used for storage association; a kludge to prevent extending this type.
   private
   integer, allocatable :: codes(:)
contains
   ! METHODS (type-bound procedures) :
   ! conversion
   procedure :: character  => oop_character ! a single variable in UTF-8 encoding
   procedure :: ch         => oop_character ! a single variable in UTF-8 encoding

   procedure :: codepoint  => oop_codepoint ! codes of each glyph
   procedure :: byte       => oop_byte      ! stream of bytes in UTF-8 encoding
   procedure :: ichar      => oop_ichar     ! code of a single character
   ! intrinsics
   procedure :: adjustl    => oop_adjustl
   procedure :: adjustr    => oop_adjustr
   procedure :: index      => oop_index
   procedure :: len        => oop_len
   procedure :: len_trim   => oop_len_trim
   procedure :: trim       => oop_trim
   procedure :: split      => oop_split
   procedure :: tokenize   => oop_tokenize
   procedure :: scan       => oop_scan
   procedure :: verify     => oop_verify

   ! transform
   procedure :: upper      => oop_upper
   procedure :: lower      => oop_lower
   procedure :: expandtabs => oop_expandtabs
   procedure :: escape     => oop_escape
   procedure :: fmt        => oop_fmt

   procedure :: sub        => oop_sub
   procedure :: pad        => oop_pad
   procedure :: join       => oop_join

   procedure :: get_env    => oop_get_env_uu, oop_get_env_ua
   procedure :: get_arg    => oop_get_arg_iu

   procedure,private :: oop_transliterate_uu, oop_transliterate_aa, oop_transliterate_au, oop_transliterate_ua
   generic, public   :: transliterate => oop_transliterate_uu, oop_transliterate_aa, oop_transliterate_au, oop_transliterate_ua

   procedure,private :: oop_replace_uuu
   procedure,private :: oop_replace_uaa
   procedure,private :: oop_replace_uau
   procedure,private :: oop_replace_uua
   procedure,private :: oop_section_uu
   procedure,private :: oop_section_ua
   generic,public    :: replace => oop_replace_uuu, oop_replace_uaa, oop_replace_uau, oop_replace_uua, &
                      & oop_section_uu, oop_section_ua

   !DECLARATION OF OVERLOADED OPERATORS FOR TYPE(UNICODE_TYPE)
   procedure,private :: eq => oop_eq
!   generic           :: operator(==) => oop_eq
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
!   procedure,private :: oop_g_g
!   generic           :: operator(//) => oop_g_g

end type unicode_type

! Constructor for new string instances
interface unicode_type
   elemental module function new_str(string) result(new)
      character(len=*), intent(in), optional :: string
      type(unicode_type)                     :: new
   end function new_str

   module function new_strs(strings) result(new)
      character(len=*), intent(in)           :: strings(:)
      type(unicode_type)                     :: new(size(strings))
   end function new_strs

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
! (sometimes one-sixth) of an em wide. Recommended for use as a thousands
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
integer,parameter :: G_SPACE = 32

integer,private,parameter :: spacescodes(*)= [32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288]

! Unicode lowercase to uppercase conversion mapping table
! The standard English lowercase "i" (U+0069) has a dot, which is called a "tittle".
! The uppercase dotted "İ" (U+0130) is a separate Unicode character that functions as the uppercase dotted "i".
! * U+0049 I LATIN CAPITAL LETTER I.
! * U+0130 İ LATIN CAPITAL LETTER I WITH DOT ABOVE.
! * U+0069 i LATIN SMALL LETTER I. (dotted)
! * U+0131 ı LATIN SMALL LETTER I DOTLESS
! So the problem is both "LATIN SMALL LETTER I DOTLESS" and "LATIN SMALL LETTER I" typically have uppercase "LATIN CAPITAL LETTER I"
! so a round trip will not put I back to a dotless I. Because doing ASCII outside of the table could have dotted as one set and
! undotted as another in the table but the routine would still have the same issue.

integer,parameter :: lowhigh=666
integer,parameter :: low_to_up(lowhigh,2)= reshape([ &
int(z'0061'), int(z'0041'), & ! LATIN SMALL LETTER A => LATIN CAPITAL LETTER A
int(z'0062'), int(z'0042'), & ! LATIN SMALL LETTER B => LATIN CAPITAL LETTER B
int(z'0063'), int(z'0043'), & ! LATIN SMALL LETTER C => LATIN CAPITAL LETTER C
int(z'0064'), int(z'0044'), & ! LATIN SMALL LETTER D => LATIN CAPITAL LETTER D
int(z'0065'), int(z'0045'), & ! LATIN SMALL LETTER E => LATIN CAPITAL LETTER E
int(z'0066'), int(z'0046'), & ! LATIN SMALL LETTER F => LATIN CAPITAL LETTER F
int(z'0067'), int(z'0047'), & ! LATIN SMALL LETTER G => LATIN CAPITAL LETTER G
int(z'0068'), int(z'0048'), & ! LATIN SMALL LETTER H => LATIN CAPITAL LETTER H
int(z'0069'), int(z'0049'), & ! LATIN SMALL LETTER I => LATIN CAPITAL LETTER I
int(z'006A'), int(z'004A'), & ! LATIN SMALL LETTER J => LATIN CAPITAL LETTER J
int(z'006B'), int(z'004B'), & ! LATIN SMALL LETTER K => LATIN CAPITAL LETTER K
int(z'006C'), int(z'004C'), & ! LATIN SMALL LETTER L => LATIN CAPITAL LETTER L
int(z'006D'), int(z'004D'), & ! LATIN SMALL LETTER M => LATIN CAPITAL LETTER M
int(z'006E'), int(z'004E'), & ! LATIN SMALL LETTER N => LATIN CAPITAL LETTER N
int(z'006F'), int(z'004F'), & ! LATIN SMALL LETTER O => LATIN CAPITAL LETTER O
int(z'0070'), int(z'0050'), & ! LATIN SMALL LETTER P => LATIN CAPITAL LETTER P
int(z'0071'), int(z'0051'), & ! LATIN SMALL LETTER Q => LATIN CAPITAL LETTER Q
int(z'0072'), int(z'0052'), & ! LATIN SMALL LETTER R => LATIN CAPITAL LETTER R
int(z'0073'), int(z'0053'), & ! LATIN SMALL LETTER S => LATIN CAPITAL LETTER S
int(z'0074'), int(z'0054'), & ! LATIN SMALL LETTER T => LATIN CAPITAL LETTER T
int(z'0075'), int(z'0055'), & ! LATIN SMALL LETTER U => LATIN CAPITAL LETTER U
int(z'0076'), int(z'0056'), & ! LATIN SMALL LETTER V => LATIN CAPITAL LETTER V
int(z'0077'), int(z'0057'), & ! LATIN SMALL LETTER W => LATIN CAPITAL LETTER W
int(z'0078'), int(z'0058'), & ! LATIN SMALL LETTER X => LATIN CAPITAL LETTER X
int(z'0079'), int(z'0059'), & ! LATIN SMALL LETTER Y => LATIN CAPITAL LETTER Y
int(z'007A'), int(z'005A'), & ! LATIN SMALL LETTER Z => LATIN CAPITAL LETTER Z
int(z'00E0'), int(z'00C0'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER A GRAVE
int(z'00E1'), int(z'00C1'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER A ACUTE
int(z'00E2'), int(z'00C2'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER A CIRCUMFLEX
int(z'00E3'), int(z'00C3'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER A TILDE
int(z'00E4'), int(z'00C4'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER A DIAERESIS
int(z'00E5'), int(z'00C5'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER A RING
int(z'00E6'), int(z'00C6'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER A E
int(z'00E7'), int(z'00C7'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER C CEDILLA
int(z'00E8'), int(z'00C8'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER E GRAVE
int(z'00E9'), int(z'00C9'), & ! LATIN SMALL LETTER A GRAVE => LATIN CAPITAL LETTER E ACUTE
int(z'00EA'), int(z'00CA'), & ! LATIN SMALL LETTER E CIRCUMFLEX => LATIN CAPITAL LETTER E CIRCUMFLEX
int(z'00EB'), int(z'00CB'), & ! LATIN SMALL LETTER E DIAERESIS => LATIN CAPITAL LETTER E DIAERESIS
int(z'00EC'), int(z'00CC'), & ! LATIN SMALL LETTER I GRAVE => LATIN CAPITAL LETTER I GRAVE
int(z'00ED'), int(z'00CD'), & ! LATIN SMALL LETTER I ACUTE => LATIN CAPITAL LETTER I ACUTE
int(z'00EE'), int(z'00CE'), & ! LATIN SMALL LETTER I CIRCUMFLEX => LATIN CAPITAL LETTER I CIRCUMFLEX
int(z'00EF'), int(z'00CF'), & ! LATIN SMALL LETTER I DIAERESIS => LATIN CAPITAL LETTER I DIAERESIS
int(z'00F0'), int(z'00D0'), & ! LATIN SMALL LETTER ETH => LATIN CAPITAL LETTER ETH
int(z'00F1'), int(z'00D1'), & ! LATIN SMALL LETTER N TILDE => LATIN CAPITAL LETTER N TILDE
int(z'00F2'), int(z'00D2'), & ! LATIN SMALL LETTER O GRAVE => LATIN CAPITAL LETTER O GRAVE
int(z'00F3'), int(z'00D3'), & ! LATIN SMALL LETTER O ACUTE => LATIN CAPITAL LETTER O ACUTE
int(z'00F4'), int(z'00D4'), & ! LATIN SMALL LETTER O CIRCUMFLEX => LATIN CAPITAL LETTER O CIRCUMFLEX
int(z'00F5'), int(z'00D5'), & ! LATIN SMALL LETTER O TILDE => LATIN CAPITAL LETTER O TILDE
int(z'00F6'), int(z'00D6'), & ! LATIN SMALL LETTER O DIAERESIS => LATIN CAPITAL LETTER O DIAERESIS
int(z'00F8'), int(z'00D8'), & ! LATIN SMALL LETTER O SLASH => LATIN CAPITAL LETTER O SLASH
int(z'00F9'), int(z'00D9'), & ! LATIN SMALL LETTER U GRAVE => LATIN CAPITAL LETTER U GRAVE
int(z'00FA'), int(z'00DA'), & ! LATIN SMALL LETTER U ACUTE => LATIN CAPITAL LETTER U ACUTE
int(z'00FB'), int(z'00DB'), & ! LATIN SMALL LETTER U CIRCUMFLEX => LATIN CAPITAL LETTER U CIRCUMFLEX
int(z'00FC'), int(z'00DC'), & ! LATIN SMALL LETTER U DIAERESIS => LATIN CAPITAL LETTER U DIAERESIS
int(z'00FD'), int(z'00DD'), & ! LATIN SMALL LETTER Y ACUTE => LATIN CAPITAL LETTER Y ACUTE
int(z'00FE'), int(z'00DE'), & ! LATIN SMALL LETTER THORN => LATIN CAPITAL LETTER THORN
int(z'00FF'), int(z'0178'), & ! LATIN SMALL LETTER Y DIAERESIS => LATIN CAPITAL LETTER Y WITH DIAERESIS
int(z'0101'), int(z'0100'), & ! LATIN SMALL LETTER A WITH MACRON => LATIN CAPITAL LETTER A WITH MACRON
int(z'0103'), int(z'0102'), & ! LATIN SMALL LETTER A WITH BREVE => LATIN CAPITAL LETTER A WITH BREVE
int(z'0105'), int(z'0104'), & ! LATIN SMALL LETTER A WITH OGONEK => LATIN CAPITAL LETTER A WITH OGONEK
int(z'0107'), int(z'0106'), & ! LATIN SMALL LETTER C WITH ACUTE => LATIN CAPITAL LETTER C WITH ACUTE
int(z'0109'), int(z'0108'), & ! LATIN SMALL LETTER C WITH CIRCUMFLEX => LATIN CAPITAL LETTER C WITH CIRCUMFLEX
int(z'010B'), int(z'010A'), & ! LATIN SMALL LETTER C WITH DOT ABOVE => LATIN CAPITAL LETTER C WITH DOT ABOVE
int(z'010D'), int(z'010C'), & ! LATIN SMALL LETTER C WITH CARON => LATIN CAPITAL LETTER C WITH CARON
int(z'010F'), int(z'010E'), & ! LATIN SMALL LETTER D WITH CARON => LATIN CAPITAL LETTER D WITH CARON
int(z'0111'), int(z'0110'), & ! LATIN SMALL LETTER D WITH STROKE => LATIN CAPITAL LETTER D WITH STROKE
int(z'0113'), int(z'0112'), & ! LATIN SMALL LETTER E WITH MACRON => LATIN CAPITAL LETTER E WITH MACRON
int(z'0115'), int(z'0114'), & ! LATIN SMALL LETTER E WITH BREVE => LATIN CAPITAL LETTER E WITH BREVE
int(z'0117'), int(z'0116'), & ! LATIN SMALL LETTER E WITH DOT ABOVE => LATIN CAPITAL LETTER E WITH DOT ABOVE
int(z'0119'), int(z'0118'), & ! LATIN SMALL LETTER E WITH OGONEK => LATIN CAPITAL LETTER E WITH OGONEK
int(z'011B'), int(z'011A'), & ! LATIN SMALL LETTER E WITH CARON => LATIN CAPITAL LETTER E WITH CARON
int(z'011D'), int(z'011C'), & ! LATIN SMALL LETTER G WITH CIRCUMFLEX => LATIN CAPITAL LETTER G WITH CIRCUMFLEX
int(z'011F'), int(z'011E'), & ! LATIN SMALL LETTER G WITH BREVE => LATIN CAPITAL LETTER G WITH BREVE
int(z'0121'), int(z'0120'), & ! LATIN SMALL LETTER G WITH DOT ABOVE => LATIN CAPITAL LETTER G WITH DOT ABOVE
int(z'0123'), int(z'0122'), & ! LATIN SMALL LETTER G WITH CEDILLA => LATIN CAPITAL LETTER G WITH CEDILLA
int(z'0125'), int(z'0124'), & ! LATIN SMALL LETTER H WITH CIRCUMFLEX => LATIN CAPITAL LETTER H WITH CIRCUMFLEX
int(z'0127'), int(z'0126'), & ! LATIN SMALL LETTER H WITH STROKE => LATIN CAPITAL LETTER H WITH STROKE
int(z'0129'), int(z'0128'), & ! LATIN SMALL LETTER I WITH TILDE => LATIN CAPITAL LETTER I WITH TILDE
int(z'012B'), int(z'012A'), & ! LATIN SMALL LETTER I WITH MACRON => LATIN CAPITAL LETTER I WITH MACRON
int(z'012D'), int(z'012C'), & ! LATIN SMALL LETTER I WITH BREVE => LATIN CAPITAL LETTER I WITH BREVE
int(z'012F'), int(z'012E'), & ! LATIN SMALL LETTER I WITH OGONEK => LATIN CAPITAL LETTER I WITH OGONEK
int(z'0131'), int(z'0049'), & ! LATIN SMALL LETTER DOTLESS I => LATIN CAPITAL LETTER I
int(z'0133'), int(z'0132'), & ! LATIN SMALL LIGATURE IJ => LATIN CAPITAL LIGATURE IJ
int(z'0135'), int(z'0134'), & ! LATIN SMALL LETTER J WITH CIRCUMFLEX => LATIN CAPITAL LETTER J WITH CIRCUMFLEX
int(z'0137'), int(z'0136'), & ! LATIN SMALL LETTER K WITH CEDILLA => LATIN CAPITAL LETTER K WITH CEDILLA
int(z'013A'), int(z'0139'), & ! LATIN SMALL LETTER L WITH ACUTE => LATIN CAPITAL LETTER L WITH ACUTE
int(z'013C'), int(z'013B'), & ! LATIN SMALL LETTER L WITH CEDILLA => LATIN CAPITAL LETTER L WITH CEDILLA
int(z'013E'), int(z'013D'), & ! LATIN SMALL LETTER L WITH CARON => LATIN CAPITAL LETTER L WITH CARON
int(z'0140'), int(z'013F'), & ! LATIN SMALL LETTER L WITH MIDDLE DOT => LATIN CAPITAL LETTER L WITH MIDDLE DOT
int(z'0142'), int(z'0141'), & ! LATIN SMALL LETTER L WITH STROKE => LATIN CAPITAL LETTER L WITH STROKE
int(z'0144'), int(z'0143'), & ! LATIN SMALL LETTER N WITH ACUTE => LATIN CAPITAL LETTER N WITH ACUTE
int(z'0146'), int(z'0145'), & ! LATIN SMALL LETTER N WITH CEDILLA => LATIN CAPITAL LETTER N WITH CEDILLA
int(z'0148'), int(z'0147'), & ! LATIN SMALL LETTER N WITH CARON => LATIN CAPITAL LETTER N WITH CARON
int(z'014B'), int(z'014A'), & ! LATIN SMALL LETTER ENG (SAMI) => LATIN CAPITAL LETTER ENG (SAMI)
int(z'014D'), int(z'014C'), & ! LATIN SMALL LETTER O WITH MACRON => LATIN CAPITAL LETTER O WITH MACRON
int(z'014F'), int(z'014E'), & ! LATIN SMALL LETTER O WITH BREVE => LATIN CAPITAL LETTER O WITH BREVE
int(z'0151'), int(z'0150'), & ! LATIN SMALL LETTER O WITH DOUBLE ACUTE => LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
int(z'0153'), int(z'0152'), & ! LATIN SMALL LIGATURE OE => LATIN CAPITAL LIGATURE OE
int(z'0155'), int(z'0154'), & ! LATIN SMALL LETTER R WITH ACUTE => LATIN CAPITAL LETTER R WITH ACUTE
int(z'0157'), int(z'0156'), & ! LATIN SMALL LETTER R WITH CEDILLA => LATIN CAPITAL LETTER R WITH CEDILLA
int(z'0159'), int(z'0158'), & ! LATIN SMALL LETTER R WITH CARON => LATIN CAPITAL LETTER R WITH CARON
int(z'015B'), int(z'015A'), & ! LATIN SMALL LETTER S WITH ACUTE => LATIN CAPITAL LETTER S WITH ACUTE
int(z'015D'), int(z'015C'), & ! LATIN SMALL LETTER S WITH CIRCUMFLEX => LATIN CAPITAL LETTER S WITH CIRCUMFLEX
int(z'015F'), int(z'015E'), & ! LATIN SMALL LETTER S WITH CEDILLA => LATIN CAPITAL LETTER S WITH CEDILLA
int(z'0161'), int(z'0160'), & ! LATIN SMALL LETTER S WITH CARON => LATIN CAPITAL LETTER S WITH CARON
int(z'0163'), int(z'0162'), & ! LATIN SMALL LETTER T WITH CEDILLA => LATIN CAPITAL LETTER T WITH CEDILLA
int(z'0165'), int(z'0164'), & ! LATIN SMALL LETTER T WITH CARON => LATIN CAPITAL LETTER T WITH CARON
int(z'0167'), int(z'0166'), & ! LATIN SMALL LETTER T WITH STROKE => LATIN CAPITAL LETTER T WITH STROKE
int(z'0169'), int(z'0168'), & ! LATIN SMALL LETTER U WITH TILDE => LATIN CAPITAL LETTER U WITH TILDE
int(z'016B'), int(z'016A'), & ! LATIN SMALL LETTER U WITH MACRON => LATIN CAPITAL LETTER U WITH MACRON
int(z'016D'), int(z'016C'), & ! LATIN SMALL LETTER U WITH BREVE => LATIN CAPITAL LETTER U WITH BREVE
int(z'016F'), int(z'016E'), & ! LATIN SMALL LETTER U WITH RING ABOVE => LATIN CAPITAL LETTER U WITH RING ABOVE
int(z'0171'), int(z'0170'), & ! LATIN SMALL LETTER U WITH DOUBLE ACUTE => LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
int(z'0173'), int(z'0172'), & ! LATIN SMALL LETTER U WITH OGONEK => LATIN CAPITAL LETTER U WITH OGONEK
int(z'0175'), int(z'0174'), & ! LATIN SMALL LETTER W WITH CIRCUMFLEX => LATIN CAPITAL LETTER W WITH CIRCUMFLEX
int(z'0177'), int(z'0176'), & ! LATIN SMALL LETTER Y WITH CIRCUMFLEX => LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
int(z'017A'), int(z'0179'), & ! LATIN SMALL LETTER Z WITH ACUTE => LATIN CAPITAL LETTER Z WITH ACUTE
int(z'017C'), int(z'017B'), & ! LATIN SMALL LETTER Z WITH DOT ABOVE => LATIN CAPITAL LETTER Z WITH DOT ABOVE
int(z'017E'), int(z'017D'), & ! LATIN SMALL LETTER Z WITH CARON => LATIN CAPITAL LETTER Z WITH CARON
int(z'0183'), int(z'0182'), & ! LATIN SMALL LETTER B WITH TOPBAR => LATIN CAPITAL LETTER B WITH TOPBAR
int(z'0185'), int(z'0184'), & ! LATIN SMALL LETTER TONE SIX => LATIN CAPITAL LETTER TONE SIX
int(z'0188'), int(z'0187'), & ! LATIN SMALL LETTER C WITH HOOK => LATIN CAPITAL LETTER C WITH HOOK
int(z'018C'), int(z'018B'), & ! LATIN SMALL LETTER D WITH TOPBAR => LATIN CAPITAL LETTER D WITH TOPBAR
int(z'0192'), int(z'0191'), & ! LATIN SMALL LETTER F WITH HOOK => LATIN CAPITAL LETTER F WITH HOOK
int(z'0199'), int(z'0198'), & ! LATIN SMALL LETTER K WITH HOOK => LATIN CAPITAL LETTER K WITH HOOK
int(z'01A1'), int(z'01A0'), & ! LATIN SMALL LETTER O WITH HORN => LATIN CAPITAL LETTER O WITH HORN
int(z'01A3'), int(z'01A2'), & ! LATIN SMALL LETTER OI => LATIN CAPITAL LETTER OI
int(z'01A5'), int(z'01A4'), & ! LATIN SMALL LETTER P WITH HOOK => LATIN CAPITAL LETTER P WITH HOOK
int(z'01A8'), int(z'01A7'), & ! LATIN SMALL LETTER TONE TWO => LATIN CAPITAL LETTER TONE TWO
int(z'01AD'), int(z'01AC'), & ! LATIN SMALL LETTER T WITH HOOK => LATIN CAPITAL LETTER T WITH HOOK
int(z'01B0'), int(z'01AF'), & ! LATIN SMALL LETTER U WITH HORN => LATIN CAPITAL LETTER U WITH HORN
int(z'01B4'), int(z'01B3'), & ! LATIN SMALL LETTER Y WITH HOOK => LATIN CAPITAL LETTER Y WITH HOOK
int(z'01B6'), int(z'01B5'), & ! LATIN SMALL LETTER Z WITH STROKE => LATIN CAPITAL LETTER Z WITH STROKE
int(z'01B9'), int(z'01B8'), & ! LATIN SMALL LETTER EZH REVERSED => LATIN CAPITAL LETTER EZH REVERSED
int(z'01BD'), int(z'01BC'), & ! LATIN SMALL LETTER TONE FIVE => LATIN CAPITAL LETTER TONE FIVE
int(z'01C6'), int(z'01C4'), & ! LATIN SMALL LETTER DZ WITH CARON => LATIN CAPITAL LETTER DZ WITH CARON
int(z'01C9'), int(z'01C7'), & ! LATIN SMALL LETTER LJ => LATIN CAPITAL LETTER LJ
int(z'01CC'), int(z'01CA'), & ! LATIN SMALL LETTER NJ => LATIN CAPITAL LETTER NJ
int(z'01CE'), int(z'01CD'), & ! LATIN SMALL LETTER A WITH CARON => LATIN CAPITAL LETTER A WITH CARON
int(z'01D0'), int(z'01CF'), & ! LATIN SMALL LETTER I WITH CARON => LATIN CAPITAL LETTER I WITH CARON
int(z'01D2'), int(z'01D1'), & ! LATIN SMALL LETTER O WITH CARON => LATIN CAPITAL LETTER O WITH CARON
int(z'01D4'), int(z'01D3'), & ! LATIN SMALL LETTER U WITH CARON => LATIN CAPITAL LETTER U WITH CARON
int(z'01D6'), int(z'01D5'), & ! LATIN SMALL LETTER U WITH DIAERESIS AND MACRON => LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
int(z'01D8'), int(z'01D7'), & ! LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE => LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
int(z'01DA'), int(z'01D9'), & ! LATIN SMALL LETTER U WITH DIAERESIS AND CARON => LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
int(z'01DC'), int(z'01DB'), & ! LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE => LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
int(z'01DF'), int(z'01DE'), & ! LATIN SMALL LETTER A WITH DIAERESIS AND MACRON => LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
int(z'01E1'), int(z'01E0'), & ! LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON => LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
int(z'01E3'), int(z'01E2'), & ! LATIN SMALL LIGATURE AE WITH MACRON => LATIN CAPITAL LIGATURE AE MTH MACRON
int(z'01E5'), int(z'01E4'), & ! LATIN SMALL LETTER G WITH STROKE => LATIN CAPITAL LETTER G WITH STROKE
int(z'01E7'), int(z'01E6'), & ! LATIN SMALL LETTER G WITH CARON => LATIN CAPITAL LETTER G WITH CARON
int(z'01E9'), int(z'01E8'), & ! LATIN SMALL LETTER K WITH CARON => LATIN CAPITAL LETTER K WITH CARON
int(z'01EB'), int(z'01EA'), & ! LATIN SMALL LETTER O WITH OGONEK => LATIN CAPITAL LETTER O WITH OGONEK
int(z'01ED'), int(z'01EC'), & ! LATIN SMALL LETTER O WITH OGONEK AND MACRON => LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
int(z'01EF'), int(z'01EE'), & ! LATIN SMALL LETTER EZH WITH CARON => LATIN CAPITAL LETTER EZH WITH CARON
int(z'01F3'), int(z'01F1'), & ! LATIN SMALL LETTER DZ => LATIN CAPITAL LETTER DZ
int(z'01F5'), int(z'01F4'), & ! LATIN SMALL LETTER G WITH ACUTE => LATIN CAPITAL LETTER G WITH ACUTE
int(z'01FB'), int(z'01FA'), & ! LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE => LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
int(z'01FD'), int(z'01FC'), & ! LATIN SMALL LIGATURE AE WITH ACUTE => LATIN CAPITAL LIGATURE AE WITH ACUTE
int(z'01FF'), int(z'01FE'), & ! LATIN SMALL LETTER O WITH STROKE AND ACUTE => LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
int(z'0201'), int(z'0200'), & ! LATIN SMALL LETTER A WITH DOUBLE GRAVE => LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
int(z'0203'), int(z'0202'), & ! LATIN SMALL LETTER A WITH INVERTED BREVE => LATIN CAPITAL LETTER A WITH INVERTED BREVE
int(z'0205'), int(z'0204'), & ! LATIN SMALL LETTER E WITH DOUBLE GRAVE => LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
int(z'0207'), int(z'0206'), & ! LATIN SMALL LETTER E WITH INVERTED BREVE => LATIN CAPITAL LETTER E WITH INVERTED BREVE
int(z'0209'), int(z'0208'), & ! LATIN SMALL LETTER I WITH DOUBLE GRAVE => LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
int(z'020B'), int(z'020A'), & ! LATIN SMALL LETTER I WITH INVERTED BREVE => LATIN CAPITAL LETTER I WITH INVERTED BREVE
int(z'020D'), int(z'020C'), & ! LATIN SMALL LETTER O WITH DOUBLE GRAVE => LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
int(z'020F'), int(z'020E'), & ! LATIN SMALL LETTER O WITH INVERTED BREVE => LATIN CAPITAL LETTER O WITH INVERTED BREVE
int(z'0211'), int(z'0210'), & ! LATIN SMALL LETTER R WITH DOUBLE GRAVE => LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
int(z'0213'), int(z'0212'), & ! LATIN SMALL LETTER R WITH INVERTED BREVE => LATIN CAPITAL LETTER R WITH INVERTED BREVE
int(z'0215'), int(z'0214'), & ! LATIN SMALL LETTER U WITH DOUBLE GRAVE => LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
int(z'0217'), int(z'0216'), & ! LATIN SMALL LETTER U WITH INVERTED BREVE => LATIN CAPITAL LETTER U WITH INVERTED BREVE
int(z'0253'), int(z'0181'), & ! LATIN SMALL LETTER B WITH HOOK => LATIN CAPITAL LETTER B WITH HOOK
int(z'0254'), int(z'0186'), & ! LATIN SMALL LETTER OPEN O => LATIN CAPITAL LETTER OPEN O
int(z'0257'), int(z'018A'), & ! LATIN SMALL LETTER D WITH HOOK => LATIN CAPITAL LETTER D WITH HOOK
int(z'0258'), int(z'018E'), & ! LATIN SMALL LETTER REVERSED E => LATIN CAPITAL LETTER REVERSED E
int(z'0259'), int(z'018F'), & ! LATIN SMALL LETTER SCHWA => LATIN CAPITAL LETTER SCHWA
int(z'025B'), int(z'0190'), & ! LATIN SMALL LETTER OPEN E => LATIN CAPITAL LETTER OPEN E
int(z'0260'), int(z'0193'), & ! LATIN SMALL LETTER G WITH HOOK => LATIN CAPITAL LETTER G WITH HOOK
int(z'0263'), int(z'0194'), & ! LATIN SMALL LETTER GAMMA => LATIN CAPITAL LETTER GAMMA
int(z'0268'), int(z'0197'), & ! LATIN SMALL LETTER I WITH STROKE => LATIN CAPITAL LETTER I WITH STROKE
int(z'0269'), int(z'0196'), & ! LATIN SMALL LETTER IOTA => LATIN CAPITAL LETTER IOTA
int(z'026F'), int(z'019C'), & ! LATIN SMALL LETTER TURNED M => LATIN CAPITAL LETTER TURNED M
int(z'0272'), int(z'019D'), & ! LATIN SMALL LETTER N WITH LEFT HOOK => LATIN CAPITAL LETTER N WITH LEFT HOOK
int(z'0275'), int(z'019F'), & ! LATIN SMALL LETTER BARRED O => LATIN CAPITAL LETTER O WITH MIDDLE TILDE
int(z'0283'), int(z'01A9'), & ! LATIN SMALL LETTER ESH => LATIN CAPITAL LETTER ESH
int(z'0288'), int(z'01AE'), & ! LATIN SMALL LETTER T WITH RETROFLEX HOOK => LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
int(z'028A'), int(z'01B1'), & ! LATIN SMALL LETTER UPSILON => LATIN CAPITAL LETTER UPSILON
int(z'028B'), int(z'01B2'), & ! LATIN SMALL LETTER V WITH HOOK => LATIN CAPITAL LETTER V WITH HOOK
int(z'0292'), int(z'01B7'), & ! LATIN SMALL LETTER EZH => LATIN CAPITAL LETTER EZH
int(z'03AC'), int(z'0386'), & ! GREEK SMALL LETTER ALPHA WITH TONOS => GREEK CAPITAL LETTER ALPHA WITH TONOS
int(z'03AD'), int(z'0388'), & ! GREEK SMALL LETTER EPSILON WITH TONOS => GREEK CAPITAL LETTER EPSILON WITH TONOS
int(z'03AE'), int(z'0389'), & ! GREEK SMALL LETTER ETA WITH TONOS => GREEK CAPITAL LETTER ETA WITH TONOS
int(z'03AF'), int(z'038A'), & ! GREEK SMALL LETTER IOTA WITH TONOS => GREEK CAPITAL LETTER IOTA WITH TONOS
int(z'03B1'), int(z'0391'), & ! GREEK SMALL LETTER ALPHA => GREEK CAPITAL LETTER ALPHA
int(z'03B2'), int(z'0392'), & ! GREEK SMALL LETTER BETA => GREEK CAPITAL LETTER BETA
int(z'03B3'), int(z'0393'), & ! GREEK SMALL LETTER GAMMA => GREEK CAPITAL LETTER GAMMA
int(z'03B4'), int(z'0394'), & ! GREEK SMALL LETTER DELTA => GREEK CAPITAL LETTER DELTA
int(z'03B5'), int(z'0395'), & ! GREEK SMALL LETTER EPSILON => GREEK CAPITAL LETTER EPSILON
int(z'03B6'), int(z'0396'), & ! GREEK SMALL LETTER ZETA => GREEK CAPITAL LETTER ZETA
int(z'03B7'), int(z'0397'), & ! GREEK SMALL LETTER ETA => GREEK CAPITAL LETTER ETA
int(z'03B8'), int(z'0398'), & ! GREEK SMALL LETTER THETA => GREEK CAPITAL LETTER THETA
int(z'03B9'), int(z'0399'), & ! GREEK SMALL LETTER IOTA => GREEK CAPITAL LETTER IOTA
int(z'03BA'), int(z'039A'), & ! GREEK SMALL LETTER KAPPA => GREEK CAPITAL LETTER KAPPA
int(z'03BB'), int(z'039B'), & ! GREEK SMALL LETTER LAMDA => GREEK CAPITAL LETTER LAMDA
int(z'03BC'), int(z'039C'), & ! GREEK SMALL LETTER MU => GREEK CAPITAL LETTER MU
int(z'03BD'), int(z'039D'), & ! GREEK SMALL LETTER NU => GREEK CAPITAL LETTER NU
int(z'03BE'), int(z'039E'), & ! GREEK SMALL LETTER XI => GREEK CAPITAL LETTER XI
int(z'03BF'), int(z'039F'), & ! GREEK SMALL LETTER OMICRON => GREEK CAPITAL LETTER OMICRON
int(z'03C0'), int(z'03A0'), & ! GREEK SMALL LETTER PI => GREEK CAPITAL LETTER PI
int(z'03C1'), int(z'03A1'), & ! GREEK SMALL LETTER RHO => GREEK CAPITAL LETTER RHO
int(z'03C3'), int(z'03A3'), & ! GREEK SMALL LETTER SIGMA => GREEK CAPITAL LETTER SIGMA
int(z'03C4'), int(z'03A4'), & ! GREEK SMALL LETTER TAU => GREEK CAPITAL LETTER TAU
int(z'03C5'), int(z'03A5'), & ! GREEK SMALL LETTER UPSILON => GREEK CAPITAL LETTER UPSILON
int(z'03C6'), int(z'03A6'), & ! GREEK SMALL LETTER PHI => GREEK CAPITAL LETTER PHI
int(z'03C7'), int(z'03A7'), & ! GREEK SMALL LETTER CHI => GREEK CAPITAL LETTER CHI
int(z'03C8'), int(z'03A8'), & ! GREEK SMALL LETTER PSI => GREEK CAPITAL LETTER PSI
int(z'03C9'), int(z'03A9'), & ! GREEK SMALL LETTER OMEGA => GREEK CAPITAL LETTER OMEGA
int(z'03CA'), int(z'03AA'), & ! GREEK SMALL LETTER IOTA WITH DIALYTIKA => GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
int(z'03CB'), int(z'03AB'), & ! GREEK SMALL LETTER UPSILON WITH DIALYTIKA => GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
int(z'03CC'), int(z'038C'), & ! GREEK SMALL LETTER OMICRON WITH TONOS => GREEK CAPITAL LETTER OMICRON WITH TONOS
int(z'03CD'), int(z'038E'), & ! GREEK SMALL LETTER UPSILON WITH TONOS => GREEK CAPITAL LETTER UPSILON WITH TONOS
int(z'03CE'), int(z'038F'), & ! GREEK SMALL LETTER OMEGA WITH TONOS => GREEK CAPITAL LETTER OMEGA WITH TONOS
int(z'03E3'), int(z'03E2'), & ! COPTIC SMALL LETTER SHEI => COPTIC CAPITAL LETTER SHEI
int(z'03E5'), int(z'03E4'), & ! COPTIC SMALL LETTER FEI => COPTIC CAPITAL LETTER FEI
int(z'03E7'), int(z'03E6'), & ! COPTIC SMALL LETTER KHEI => COPTIC CAPITAL LETTER KHEI
int(z'03E9'), int(z'03E8'), & ! COPTIC SMALL LETTER HORI => COPTIC CAPITAL LETTER HORI
int(z'03EB'), int(z'03EA'), & ! COPTIC SMALL LETTER GANGIA => COPTIC CAPITAL LETTER GANGIA
int(z'03ED'), int(z'03EC'), & ! COPTIC SMALL LETTER SHIMA => COPTIC CAPITAL LETTER SHIMA
int(z'03EF'), int(z'03EE'), & ! COPTIC SMALL LETTER DEI => COPTIC CAPITAL LETTER DEI
int(z'0430'), int(z'0410'), & ! CYRILLIC SMALL LETTER A => CYRILLIC CAPITAL LETTER A
int(z'0431'), int(z'0411'), & ! CYRILLIC SMALL LETTER BE => CYRILLIC CAPITAL LETTER BE
int(z'0432'), int(z'0412'), & ! CYRILLIC SMALL LETTER VE => CYRILLIC CAPITAL LETTER VE
int(z'0433'), int(z'0413'), & ! CYRILLIC SMALL LETTER GHE => CYRILLIC CAPITAL LETTER GHE
int(z'0434'), int(z'0414'), & ! CYRILLIC SMALL LETTER DE => CYRILLIC CAPITAL LETTER DE
int(z'0435'), int(z'0415'), & ! CYRILLIC SMALL LETTER IE => CYRILLIC CAPITAL LETTER IE
int(z'0436'), int(z'0416'), & ! CYRILLIC SMALL LETTER ZHE => CYRILLIC CAPITAL LETTER ZHE
int(z'0437'), int(z'0417'), & ! CYRILLIC SMALL LETTER ZE => CYRILLIC CAPITAL LETTER ZE
int(z'0438'), int(z'0418'), & ! CYRILLIC SMALL LETTER I => CYRILLIC CAPITAL LETTER I
int(z'0439'), int(z'0419'), & ! CYRILLIC SMALL LETTER SHORT I => CYRILLIC CAPITAL LETTER SHORT I
int(z'043A'), int(z'041A'), & ! CYRILLIC SMALL LETTER KA => CYRILLIC CAPITAL LETTER KA
int(z'043B'), int(z'041B'), & ! CYRILLIC SMALL LETTER EL => CYRILLIC CAPITAL LETTER EL
int(z'043C'), int(z'041C'), & ! CYRILLIC SMALL LETTER EM => CYRILLIC CAPITAL LETTER EM
int(z'043D'), int(z'041D'), & ! CYRILLIC SMALL LETTER EN => CYRILLIC CAPITAL LETTER EN
int(z'043E'), int(z'041E'), & ! CYRILLIC SMALL LETTER O => CYRILLIC CAPITAL LETTER O
int(z'043F'), int(z'041F'), & ! CYRILLIC SMALL LETTER PE => CYRILLIC CAPITAL LETTER PE
int(z'0440'), int(z'0420'), & ! CYRILLIC SMALL LETTER ER => CYRILLIC CAPITAL LETTER ER
int(z'0441'), int(z'0421'), & ! CYRILLIC SMALL LETTER ES => CYRILLIC CAPITAL LETTER ES
int(z'0442'), int(z'0422'), & ! CYRILLIC SMALL LETTER TE => CYRILLIC CAPITAL LETTER TE
int(z'0443'), int(z'0423'), & ! CYRILLIC SMALL LETTER U => CYRILLIC CAPITAL LETTER U
int(z'0444'), int(z'0424'), & ! CYRILLIC SMALL LETTER EF => CYRILLIC CAPITAL LETTER EF
int(z'0445'), int(z'0425'), & ! CYRILLIC SMALL LETTER HA => CYRILLIC CAPITAL LETTER HA
int(z'0446'), int(z'0426'), & ! CYRILLIC SMALL LETTER TSE => CYRILLIC CAPITAL LETTER TSE
int(z'0447'), int(z'0427'), & ! CYRILLIC SMALL LETTER CHE => CYRILLIC CAPITAL LETTER CHE
int(z'0448'), int(z'0428'), & ! CYRILLIC SMALL LETTER SHA => CYRILLIC CAPITAL LETTER SHA
int(z'0449'), int(z'0429'), & ! CYRILLIC SMALL LETTER SHCHA => CYRILLIC CAPITAL LETTER SHCHA
int(z'044A'), int(z'042A'), & ! CYRILLIC SMALL LETTER HARD SIGN => CYRILLIC CAPITAL LETTER HARD SIGN
int(z'044B'), int(z'042B'), & ! CYRILLIC SMALL LETTER YERU => CYRILLIC CAPITAL LETTER YERU
int(z'044C'), int(z'042C'), & ! CYRILLIC SMALL LETTER SOFT SIGN => CYRILLIC CAPITAL LETTER SOFT SIGN
int(z'044D'), int(z'042D'), & ! CYRILLIC SMALL LETTER E => CYRILLIC CAPITAL LETTER E
int(z'044E'), int(z'042E'), & ! CYRILLIC SMALL LETTER YU => CYRILLIC CAPITAL LETTER YU
int(z'044F'), int(z'042F'), & ! CYRILLIC SMALL LETTER YA => CYRILLIC CAPITAL LETTER YA
int(z'0451'), int(z'0401'), & ! CYRILLIC SMALL LETTER IO => CYRILLIC CAPITAL LETTER IO
int(z'0452'), int(z'0402'), & ! CYRILLIC SMALL LETTER DJE (SERBOCROATIAN) => CYRILLIC CAPITAL LETTER DJE (SERBOCROATIAN)
int(z'0453'), int(z'0403'), & ! CYRILLIC SMALL LETTER GJE => CYRILLIC CAPITAL LETTER GJE
int(z'0454'), int(z'0404'), & ! CYRILLIC SMALL LETTER UKRAINIAN IE => CYRILLIC CAPITAL LETTER UKRAINIAN IE
int(z'0455'), int(z'0405'), & ! CYRILLIC SMALL LETTER DZE => CYRILLIC CAPITAL LETTER DZE
int(z'0456'), int(z'0406'), & ! CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I => CYRILLIC CAPITAL LETTER BYELORUSSIAN_UKRAINIAN I
int(z'0457'), int(z'0407'), & ! CYRILLIC SMALL LETTER YI (UKRAINIAN) => CYRILLIC CAPITAL LETTER YI (UKRAINIAN)
int(z'0458'), int(z'0408'), & ! CYRILLIC SMALL LETTER JE => CYRILLIC CAPITAL LETTER JE
int(z'0459'), int(z'0409'), & ! CYRILLIC SMALL LETTER LJE => CYRILLIC CAPITAL LETTER LJE
int(z'045A'), int(z'040A'), & ! CYRILLIC SMALL LETTER NJE => CYRILLIC CAPITAL LETTER NJE
int(z'045B'), int(z'040B'), & ! CYRILLIC SMALL LETTER TSHE (SERBOCROATIAN) => CYRILLIC CAPITAL LETTER TSHE (SERBOCROATIAN)
int(z'045C'), int(z'040C'), & ! CYRILLIC SMALL LETTER KJE => CYRILLIC CAPITAL LETTER KJE
int(z'045E'), int(z'040E'), & ! CYRILLIC SMALL LETTER SHORT U (BYELORUSSIAN) => CYRILLIC CAPITAL LETTER SHORT U (BYELORUSSIAN)
int(z'045F'), int(z'040F'), & ! CYRILLIC SMALL LETTER DZHE => CYRILLIC CAPITAL LETTER DZHE
int(z'0461'), int(z'0460'), & ! CYRILLIC SMALL LETTER OMEGA => CYRILLIC CAPITAL LETTER OMEGA
int(z'0463'), int(z'0462'), & ! CYRILLIC SMALL LETTER YAT => CYRILLIC CAPITAL LETTER YAT
int(z'0465'), int(z'0464'), & ! CYRILLIC SMALL LETTER IOTIFIED E => CYRILLIC CAPITAL LETTER IOTIFIED E
int(z'0467'), int(z'0466'), & ! CYRILLIC SMALL LETTER LITTLE YUS => CYRILLIC CAPITAL LETTER LITTLE YUS
int(z'0469'), int(z'0468'), & ! CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS => CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
int(z'046B'), int(z'046A'), & ! CYRILLIC SMALL LETTER BIG YUS => CYRILLIC CAPITAL LETTER BIG YUS
int(z'046D'), int(z'046C'), & ! CYRILLIC SMALL LETTER IOTIFIED BIG YUS => CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
int(z'046F'), int(z'046E'), & ! CYRILLIC SMALL LETTER KSI => CYRILLIC CAPITAL LETTER KSI
int(z'0471'), int(z'0470'), & ! CYRILLIC SMALL LETTER PSI => CYRILLIC CAPITAL LETTER PSI
int(z'0473'), int(z'0472'), & ! CYRILLIC SMALL LETTER FITA => CYRILLIC CAPITAL LETTER FITA
int(z'0475'), int(z'0474'), & ! CYRILLIC SMALL LETTER IZHITSA => CYRILLIC CAPITAL LETTER IZHITSA
int(z'0477'), int(z'0476'), & ! CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT => CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
int(z'0479'), int(z'0478'), & ! CYRILLIC SMALL LETTER UK => CYRILLIC CAPITAL LETTER UK
int(z'047B'), int(z'047A'), & ! CYRILLIC SMALL LETTER ROUND OMEGA => CYRILLIC CAPITAL LETTER ROUND OMEGA
int(z'047D'), int(z'047C'), & ! CYRILLIC SMALL LETTER OMEGA WITH TITLO => CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
int(z'047F'), int(z'047E'), & ! CYRILLIC SMALL LETTER OT => CYRILLIC CAPITAL LETTER OT
int(z'0481'), int(z'0480'), & ! CYRILLIC SMALL LETTER KOPPA => CYRILLIC CAPITAL LETTER KOPPA
int(z'0491'), int(z'0490'), & ! CYRILLIC SMALL LETTER GHE WITH UPTURN => CYRILLIC CAPITAL LETTER GHE WITH UPTURN
int(z'0493'), int(z'0492'), & ! CYRILLIC SMALL LETTER GHE WITH STROKE => CYRILLIC CAPITAL LETTER GHE WITH STROKE
int(z'0495'), int(z'0494'), & ! CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK => CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
int(z'0497'), int(z'0496'), & ! CYRILLIC SMALL LETTER ZHE WITH DESCENDER => CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
int(z'0499'), int(z'0498'), & ! CYRILLIC SMALL LETTER ZE WITH DESCENDER => CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
int(z'049B'), int(z'049A'), & ! CYRILLIC SMALL LETTER KA WITH DESCENDER => CYRILLIC CAPITAL LETTER KA WITH DESCENDER
int(z'049D'), int(z'049C'), & ! CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE => CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
int(z'049F'), int(z'049E'), & ! CYRILLIC SMALL LETTER KA WITH STROKE => CYRILLIC CAPITAL LETTER KA WITH STROKE
int(z'04A1'), int(z'04A0'), & ! CYRILLIC SMALL LETTER EASHKIR KA => CYRILLIC CAPITAL LETTER BASHKIR KA
int(z'04A3'), int(z'04A2'), & ! CYRILLIC SMALL LETTER EN WITH DESCENOER => CYRILLIC CAPITAL LETTER EN WITH DESCENDER
int(z'04A5'), int(z'04A4'), & ! CYRILLIC SMALL LIGATURE EN GHE => CYRILLIC CAPITAL LIGATURE EN GHF
int(z'04A7'), int(z'04A6'), & ! CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK (ABKHASIAN) => CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK (ABKHASIAN)
int(z'04A9'), int(z'04A8'), & ! CYRILLIC SMALL LETTER ABKHASIAN HA => CYRILLIC CAPITAL LETTER ABKHASIAN HA
int(z'04AB'), int(z'04AA'), & ! CYRILLIC SMALL LETTER ES WITH DESCENDER => CYRILLIC CAPITAL LETTER ES WITH DESCENDER
int(z'04AD'), int(z'04AC'), & ! CYRILLIC SMALL LETTER TE WITH DESCENDER => CYRILLIC CAPITAL LETTER TE WITH DESCENDER
int(z'04AF'), int(z'04AE'), & ! CYRILLIC SMALL LETTER STRAIGHT U => CYRILLIC CAPITAL LETTER STRAIGHT U
int(z'04B1'), int(z'04B0'), & ! CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE => CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
int(z'04B3'), int(z'04B2'), & ! CYRILLIC SMALL LETTER HA WITH DESCENDER => CYRILLIC CAPITAL LETTER HA WITH DESCENDER
int(z'04B5'), int(z'04B4'), & ! CYRILLIC SMALL LIGATURE TE TSE (ABKHASIAN) => CYRILLIC CAPITAL LIGATURE TE TSE (ABKHASIAN)
int(z'04B7'), int(z'04B6'), & ! CYRILLIC SMALL LETTER CHE WITH DESCENDER => CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
int(z'04B9'), int(z'04B8'), & ! CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE => CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
int(z'04BB'), int(z'04BA'), & ! CYRILLIC SMALL LETTER SHHA => CYRILLIC CAPITAL LETTER SHHA
int(z'04BD'), int(z'04BC'), & ! CYRILLIC SMALL LETTER ABKHASIAN CHE => CYRILLIC CAPITAL LETTER ABKHASIAN CHE
int(z'04BF'), int(z'04BE'), & ! CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER => CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
int(z'04C2'), int(z'04C1'), & ! CYRILLIC SMALL LETTER ZHE WITH BREVE => CYRILLIC CAPITAL LETTER ZHE WITH BREVE
int(z'04C4'), int(z'04C3'), & ! CYRILLIC SMALL LETTER KA WITH HOOK => CYRILLIC CAPITAL LETTER KA WITH HOOK
int(z'04C8'), int(z'04C7'), & ! CYRILLIC SMALL LETTER EN WITH HOOK => CYRILLIC CAPITAL LETTER EN WITH HOOK
int(z'04CC'), int(z'04CB'), & ! CYRILLIC SMALL LETTER KHAKASSIAN CHE => CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
int(z'04D1'), int(z'04D0'), & ! CYRILLIC SMALL LETTER A WITH BREVE => CYRILLIC CAPITAL LETTER A WITH BREVE
int(z'04D3'), int(z'04D2'), & ! CYRILLIC SMALL LETTER A WITH DIAERESIS => CYRILLIC CAPITAL LETTER A WITH DIAERESIS
int(z'04D5'), int(z'04D4'), & ! CYRILLIC SMALL LIGATURE A IE => CYRILLIC CAPITAL LIGATURE A IE
int(z'04D7'), int(z'04D6'), & ! CYRILLIC SMALL LETTER IE WITH BREVE => CYRILLIC CAPITAL LETTER IE WITH BREVE
int(z'04D9'), int(z'04D8'), & ! CYRILLIC SMALL LETTER SCHWA => CYRILLIC CAPITAL LETTER SCHWA
int(z'04DB'), int(z'04DA'), & ! CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS => CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
int(z'04DD'), int(z'04DC'), & ! CYRILLIC SMALL LETTER ZHE WITH DIAERESIS => CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
int(z'04DF'), int(z'04DE'), & ! CYRILLIC SMALL LETTER ZE WITH DIAERESIS => CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
int(z'04E1'), int(z'04E0'), & ! CYRILLIC SMALL LETTER ABKHASIAN DZE => CYRILLIC CAPITAL LETTER ABKHASIAN DZE
int(z'04E3'), int(z'04E2'), & ! CYRILLIC SMALL LETTER I WITH MACRON => CYRILLIC CAPITAL LETTER I WITH MACRON
int(z'04E5'), int(z'04E4'), & ! CYRILLIC SMALL LETTER I WITH DIAERESIS => CYRILLIC CAPITAL LETTER I WITH DIAERESIS
int(z'04E7'), int(z'04E6'), & ! CYRILLIC SMALL LETTER O WITH DIAERESIS => CYRILLIC CAPITAL LETTER O WITH DIAERESIS
int(z'04E9'), int(z'04E8'), & ! CYRILLIC SMALL LETTER BARRED O => CYRILLIC CAPITAL LETTER BARRED O
int(z'04EB'), int(z'04EA'), & ! CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS => CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
int(z'04EF'), int(z'04EE'), & ! CYRILLIC SMALL LETTER U WITH MACRON => CYRILLIC CAPITAL LETTER U WITH MACRON
int(z'04F1'), int(z'04F0'), & ! CYRILLIC SMALL LETTER U WITH DIAERESIS => CYRILLIC CAPITAL LETTER U WITH DIAERESIS
int(z'04F3'), int(z'04F2'), & ! CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE => CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
int(z'04F5'), int(z'04F4'), & ! CYRILLIC SMALL LETTER CHE AITH DIAERESIS => CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
int(z'04F9'), int(z'04F8'), & ! CYRILLIC SMALL LETTER YERU WITH DIAERESIS => CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
int(z'0561'), int(z'0531'), & ! ARMENIAN SMALL LETTER AYB => ARMENIAN CAPITAL LETTER AYB
int(z'0562'), int(z'0532'), & ! ARMENIAN SMALL LETTER BEN => ARMENIAN CAPITAL LETTER BEN
int(z'0563'), int(z'0533'), & ! ARMENIAN SMALL LETTER GIM => ARMENIAN CAPITAL LETTER GIM
int(z'0564'), int(z'0534'), & ! ARMENIAN SMALL LETTER DA => ARMENIAN CAPITAL LETTER DA
int(z'0565'), int(z'0535'), & ! ARMENIAN SMALL LETTER ECH => ARMENIAN CAPITAL LETTER ECH
int(z'0566'), int(z'0536'), & ! ARMENIAN SMALL LETTER ZA => ARMENIAN CAPITAL LETTER ZA
int(z'0567'), int(z'0537'), & ! ARMENIAN SMALL LETTER EH => ARMENIAN CAPITAL LETTER EH
int(z'0568'), int(z'0538'), & ! ARMENIAN SMALL LETTER ET => ARMENIAN CAPITAL LETTER ET
int(z'0569'), int(z'0539'), & ! ARMENIAN SMALL LETTER TO => ARMENIAN CAPITAL LETTER TO
int(z'056A'), int(z'053A'), & ! ARMENIAN SMALL LETTER ZHE => ARMENIAN CAPITAL LETTER ZHE
int(z'056B'), int(z'053B'), & ! ARMENIAN SMALL LETTER INI => ARMENIAN CAPITAL LETTER INI
int(z'056C'), int(z'053C'), & ! ARMENIAN SMALL LETTER LIWN => ARMENIAN CAPITAL LETTER LIWN
int(z'056D'), int(z'053D'), & ! ARMENIAN SMALL LETTER XEH => ARMENIAN CAPITAL LETTER XEH
int(z'056E'), int(z'053E'), & ! ARMENIAN SMALL LETTER CA => ARMENIAN CAPITAL LETTER CA
int(z'056F'), int(z'053F'), & ! ARMENIAN SMALL LETTER KEN => ARMENIAN CAPITAL LETTER KEN
int(z'0570'), int(z'0540'), & ! ARMENIAN SMALL LETTER HO => ARMENIAN CAPITAL LETTER HO
int(z'0571'), int(z'0541'), & ! ARMENIAN SMALL LETTER JA => ARMENIAN CAPITAL LETTER JA
int(z'0572'), int(z'0542'), & ! ARMENIAN SMALL LETTER GHAD => ARMENIAN CAPITAL LETTER GHAD
int(z'0573'), int(z'0543'), & ! ARMENIAN SMALL LETTER CHEH => ARMENIAN CAPITAL LETTER CHEH
int(z'0574'), int(z'0544'), & ! ARMENIAN SMALL LETTER MEN => ARMENIAN CAPITAL LETTER MEN
int(z'0575'), int(z'0545'), & ! ARMENIAN SMALL LETTER YI => ARMENIAN CAPITAL LETTER YI
int(z'0576'), int(z'0546'), & ! ARMENIAN SMALL LETTER NOW => ARMENIAN CAPITAL LETTER NOW
int(z'0577'), int(z'0547'), & ! ARMENIAN SMALL LETTER SNA => ARMENIAN CAPITAL LETTER SHA
int(z'0578'), int(z'0548'), & ! ARMENIAN SMALL LETTER VO => ARMENIAN CAPITAL LETTER VO
int(z'0579'), int(z'0549'), & ! ARMENIAN SMALL LETTER CHA => ARMENIAN CAPITAL LETTER CHA
int(z'057A'), int(z'054A'), & ! ARMENIAN SMALL LETTER PEH => ARMENIAN CAPITAL LETTER PEH
int(z'057B'), int(z'054B'), & ! ARMENIAN SMALL LETTER JHEH => ARMENIAN CAPITAL LETTER JHEH
int(z'057C'), int(z'054C'), & ! ARMENIAN SMALL LETTER RA => ARMENIAN CAPITAL LETTER RA
int(z'057D'), int(z'054D'), & ! ARMENIAN SMALL LETTER SEH => ARMENIAN CAPITAL LETTER SEH
int(z'057E'), int(z'054E'), & ! ARMENIAN SMALL LETTER VEW => ARMENIAN CAPITAL LETTER VEW
int(z'057F'), int(z'054F'), & ! ARMENIAN SMALL LETTER TIWN => ARMENIAN CAPITAL LETTER TIWN
int(z'0580'), int(z'0550'), & ! ARMENIAN SMALL LETTER REH => ARMENIAN CAPITAL LETTER REH
int(z'0581'), int(z'0551'), & ! ARMENIAN SMALL LETTER CO => ARMENIAN CAPITAL LETTER CO
int(z'0582'), int(z'0552'), & ! ARMENIAN SMALL LETTER YIWN => ARMENIAN CAPITAL LETTER YIWN
int(z'0583'), int(z'0553'), & ! ARMENIAN SMALL LETTER PIWP => ARMENIAN CAPITAL LETTER PIWR
int(z'0584'), int(z'0554'), & ! ARMENIAN SMALL LETTER KEH => ARMENIAN CAPITAL LETTER KEH
int(z'0585'), int(z'0555'), & ! ARMENIAN SMALL LETTER OH => ARMENIAN CAPITAL LETTER OH
int(z'0586'), int(z'0556'), & ! ARMENIAN SMALL LETTER FEH => ARMENIAN CAPITAL LETTER FEH
int(z'10D0'), int(z'10A0'), & ! GEORGIAN LETTER AN => GEORGIAN CAPITAL LETTER AN (KHUTSURI)
int(z'10D1'), int(z'10A1'), & ! GEORGIAN LETTER BAN => GEORGIAN CAPITAL LETTER BAN (KHUTSURI)
int(z'10D2'), int(z'10A2'), & ! GEORGIAN LETTER GAN => GEORGIAN CAPITAL LETTER GAN (KHUTSURI)
int(z'10D3'), int(z'10A3'), & ! GEORGIAN LETTER DON => GEORGIAN CAPITAL LETTER DON (KHUTSURI)
int(z'10D4'), int(z'10A4'), & ! GEORGIAN LETTER EN => GEORGIAN CAPITAL LETTER EN (KHUTSURI)
int(z'10D5'), int(z'10A5'), & ! GEORGIAN LETTER VIN => GEORGIAN CAPITAL LETTER VIN (KHUTSURI)
int(z'10D6'), int(z'10A6'), & ! GEORGIAN LETTER ZEN => GEORGIAN CAPITAL LETTER ZEN (KHUTSURI)
int(z'10D7'), int(z'10A7'), & ! GEORGIAN LETTER TAN => GEORGIAN CAPITAL LETTER TAN (KHUTSURI)
int(z'10D8'), int(z'10A8'), & ! GEORGIAN LETTER IN => GEORGIAN CAPITAL LETTER IN (KHUTSURI)
int(z'10D9'), int(z'10A9'), & ! GEORGIAN LETTER KAN => GEORGIAN CAPITAL LETTER KAN (KHUTSURI)
int(z'10DA'), int(z'10AA'), & ! GEORGIAN LETTER LAS => GEORGIAN CAPITAL LETTER LAS (KHUTSURI)
int(z'10DB'), int(z'10AB'), & ! GEORGIAN LETTER MAN => GEORGIAN CAPITAL LETTER MAN (KHUTSURI)
int(z'10DC'), int(z'10AC'), & ! GEORGIAN LETTER NAR => GEORGIAN CAPITAL LETTER NAR (KHUTSURI)
int(z'10DD'), int(z'10AD'), & ! GEORGIAN LETTER ON => GEORGIAN CAPITAL LETTER ON (KHUTSURI)
int(z'10DE'), int(z'10AE'), & ! GEORGIAN LETTER PAR => GEORGIAN CAPITAL LETTER PAR (KHUTSURI)
int(z'10DF'), int(z'10AF'), & ! GEORGIAN LETTER ZHAR => GEORGIAN CAPITAL LETTER ZHAR (KHUTSURI)
int(z'10E0'), int(z'10B0'), & ! GEORGIAN LETTER RAE => GEORGIAN CAPITAL LETTER RAE (KHUTSURI)
int(z'10E1'), int(z'10B1'), & ! GEORGIAN LETTER SAN => GEORGIAN CAPITAL LETTER SAN (KHUTSURI)
int(z'10E2'), int(z'10B2'), & ! GEORGIAN LETTER TAR => GEORGIAN CAPITAL LETTER TAR (KHUTSURI)
int(z'10E3'), int(z'10B3'), & ! GEORGIAN LETTER UN => GEORGIAN CAPITAL LETTER UN (KHUTSURI)
int(z'10E4'), int(z'10B4'), & ! GEORGIAN LETTER PHAR => GEORGIAN CAPITAL LETTER PHAR (KHUTSURI)
int(z'10E5'), int(z'10B5'), & ! GEORGIAN LETTER KHAR => GEORGIAN CAPITAL LETTER KHAR (KHUTSURI)
int(z'10E6'), int(z'10B6'), & ! GEORGIAN LETTER GHAN => GEORGIAN CAPITAL LETTER GHAN (KHUTSURI)
int(z'10E7'), int(z'10B7'), & ! GEORGIAN LETTER QAR => GEORGIAN CAPITAL LETTER QAR (KHUTSURI)
int(z'10E8'), int(z'10B8'), & ! GEORGIAN LETTER SHIN => GEORGIAN CAPITAL LETTER SHIN (KHUTSURI)
int(z'10E9'), int(z'10B9'), & ! GEORGIAN LETTER CHIN => GEORGIAN CAPITAL LETTER CHIN (KHUTSURI)
int(z'10EA'), int(z'10BA'), & ! GEORGIAN LETTER CAN => GEORGIAN CAPITAL LETTER CAN (KHUTSURI)
int(z'10EB'), int(z'10BB'), & ! GEORGIAN LETTER JIL => GEORGIAN CAPITAL LETTER JIL (KHUTSURI)
int(z'10EC'), int(z'10BC'), & ! GEORGIAN LETTER CIL => GEORGIAN CAPITAL LETTER CIL (KHUTSURI)
int(z'10ED'), int(z'10BD'), & ! GEORGIAN LETTER CHAR => GEORGIAN CAPITAL LETTER CHAR (KHUTSURI)
int(z'10EE'), int(z'10BE'), & ! GEORGIAN LETTER XAN => GEORGIAN CAPITAL LETTER XAN (KHUTSURI)
int(z'10EF'), int(z'10BF'), & ! GEORGIAN LETTER JHAN => GEORGIAN CAPITAL LETTER JHAN (KHUTSURI)
int(z'10F0'), int(z'10C0'), & ! GEORGIAN LETTER HAE => GEORGIAN CAPITAL LETTER HAE (KHUTSURI)
int(z'10F1'), int(z'10C1'), & ! GEORGIAN LETTER HE => GEORGIAN CAPITAL LETTER HE (KHUTSURI)
int(z'10F2'), int(z'10C2'), & ! GEORGIAN LETTER HIE => GEORGIAN CAPITAL LETTER HIE (KHUTSURI)
int(z'10F3'), int(z'10C3'), & ! GEORGIAN LETTER WE => GEORGIAN CAPITAL LETTER WE (KHUTSURI)
int(z'10F4'), int(z'10C4'), & ! GEORGIAN LETTER HAR => GEORGIAN CAPITAL LETTER HAR (KHUTSURI)
int(z'10F5'), int(z'10C5'), & ! GEORGIAN LETTER HOE => GEORGIAN CAPITAL LETTER HOE (KHUTSURI)
int(z'1E01'), int(z'1E00'), & ! LATIN SMALL LETTER A WITH RING BELOW => LATIN CAPITAL LETTER A WITH RING BELOW
int(z'1E03'), int(z'1E02'), & ! LATIN SMALL LETTER B WITH DOT ABOVE => LATIN CAPITAL LETTER B WITH DOT ABOVE
int(z'1E05'), int(z'1E04'), & ! LATIN SMALL LETTER B WITH DOT BELOW => LATIN CAPITAL LETTER B WITH DOT BELOW
int(z'1E07'), int(z'1E06'), & ! LATIN SMALL LETTER B WITH LINE BELOW => LATIN CAPITAL LETTER B WITH LINE BELOW
int(z'1E09'), int(z'1E08'), & ! LATIN SMALL LETTER C WITH CEDILLA AND ACUTE => LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
int(z'1E0B'), int(z'1E0A'), & ! LATIN SMALL LETTER D WITH DOT ABOVE => LATIN CAPITAL LETTER D WITH DOT ABOVE
int(z'1E0D'), int(z'1E0C'), & ! LATIN SMALL LETTER D WITH DOT BELOW => LATIN CAPITAL LETTER D WITH DOT BELOW
int(z'1E0F'), int(z'1E0E'), & ! LATIN SMALL LETTER D WITH LINE BELOW => LATIN CAPITAL LETTER D WITH LINE BELOW
int(z'1E11'), int(z'1E10'), & ! LATIN SMALL LETTER D WITH CEDILLA => LATIN CAPITAL LETTER D WITH CEDILLA
int(z'1E13'), int(z'1E12'), & ! LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW => LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
int(z'1E15'), int(z'1E14'), & ! LATIN SMALL LETTER E WITH MACRON AND GRAVE => LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
int(z'1E17'), int(z'1E16'), & ! LATIN SMALL LETTER E WITH MACRON AND ACUTE => LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
int(z'1E19'), int(z'1E18'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW => LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
int(z'1E1B'), int(z'1E1A'), & ! LATIN SMALL LETTER E WITH TILDE BELOW => LATIN CAPITAL LETTER E WITH TILDE BELOW
int(z'1E1D'), int(z'1E1C'), & ! LATIN SMALL LETTER E WITH CEDILLA AND BREVE => LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
int(z'1E1F'), int(z'1E1E'), & ! LATIN SMALL LETTER F WITH DOT ABOVE => LATIN CAPITAL LETTER F WITH DOT ABOVE
int(z'1E21'), int(z'1E20'), & ! LATIN SMALL LETTER G WITH MACRON => LATIN CAPITAL LETTER G WITH MACRON
int(z'1E23'), int(z'1E22'), & ! LATIN SMALL LETTER H WITH DOT ABOVE => LATIN CAPITAL LETTER H WITH DOT ABOVE
int(z'1E25'), int(z'1E24'), & ! LATIN SMALL LETTER H WITH DOT BELOW => LATIN CAPITAL LETTER H WITH DOT BELOW
int(z'1E27'), int(z'1E26'), & ! LATIN SMALL LETTER H WITH DIAERESIS => LATIN CAPITAL LETTER H WITH DIAERESIS
int(z'1E29'), int(z'1E28'), & ! LATIN SMALL LETTER H WITH CEDILLA => LATIN CAPITAL LETTER H WITH CEDILLA
int(z'1E2B'), int(z'1E2A'), & ! LATIN SMALL LETTER H WITH BREVE BELOW => LATIN CAPITAL LETTER H WITH BREVE BELOW
int(z'1E2D'), int(z'1E2C'), & ! LATIN SMALL LETTER I WITH TILDE BELOW => LATIN CAPITAL LETTER I WITH TILDE BELOW
int(z'1E2F'), int(z'1E2E'), & ! LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE => LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
int(z'1E31'), int(z'1E30'), & ! LATIN SMALL LETTER K WITH ACUTE => LATIN CAPITAL LETTER K WITH ACUTE
int(z'1E33'), int(z'1E32'), & ! LATIN SMALL LETTER K WITH DOT BELOW => LATIN CAPITAL LETTER K WITH DOT BELOW
int(z'1E35'), int(z'1E34'), & ! LATIN SMALL LETTER K WITH LINE BELOW => LATIN CAPITAL LETTER K WITH LINE BELOW
int(z'1E37'), int(z'1E36'), & ! LATIN SMALL LETTER L WITH DOT BELOW => LATIN CAPITAL LETTER L WITH DOT BELOW
int(z'1E39'), int(z'1E38'), & ! LATIN SMALL LETTER L WITH DOT BELOW AND MACRON => LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
int(z'1E3B'), int(z'1E3A'), & ! LATIN SMALL LETTER L WITH LINE BELOW => LATIN CAPITAL LETTER L WITH LINE BELOW
int(z'1E3D'), int(z'1E3C'), & ! LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW => LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
int(z'1E3F'), int(z'1E3E'), & ! LATIN SMALL LETTER M WITH ACUTE => LATIN CAPITAL LETTER M WITH ACUTE
int(z'1E41'), int(z'1E40'), & ! LATIN SMALL LETTER M WITH DOT ABOVE => LATIN CAPITAL LETTER M WITH DOT ABOVE
int(z'1E43'), int(z'1E42'), & ! LATIN SMALL LETTER M WITH DOT BELOW => LATIN CAPITAL LETTER M WITH DOT BELOW
int(z'1E45'), int(z'1E44'), & ! LATIN SMALL LETTER N WITH DOT ABOVE => LATIN CAPITAL LETTER N WITH DOT ABOVE
int(z'1E47'), int(z'1E46'), & ! LATIN SMALL LETTER N WITH DOT BELOW => LATIN CAPITAL LETTER N WITH DOT BELOW
int(z'1E49'), int(z'1E48'), & ! LATIN SMALL LETTER N WITH LINE BELOW => LATIN CAPITAL LETTER N WITH LINE BELOW
int(z'1E4B'), int(z'1E4A'), & ! LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW => LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
int(z'1E4D'), int(z'1E4C'), & ! LATIN SMALL LETTER O WITH TILDE AND ACUTE => LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
int(z'1E4F'), int(z'1E4E'), & ! LATIN SMALL LETTER O WITH TlLDE AND DIAERESIS => LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
int(z'1E51'), int(z'1E50'), & ! LATIN SMALL LETTER O WITH MACRON AND GRAVE => LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
int(z'1E53'), int(z'1E52'), & ! LATIN SMALL LETTER O WITH MACRON AND ACUTE => LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
int(z'1E55'), int(z'1E54'), & ! LATIN SMALL LETTER P WITH ACUTE => LATIN CAPITAL LETTER P WITH ACUTE
int(z'1E57'), int(z'1E56'), & ! LATIN SMALL LETTER P WITH DOT ABOVE => LATIN CAPITAL LETTER P WITH DOT ABOVE
int(z'1E59'), int(z'1E58'), & ! LATIN SMALL LETTER R WITH DOT ABOVE => LATIN CAPITAL LETTER R WITH DOT ABOVE
int(z'1E5B'), int(z'1E5A'), & ! LATIN SMALL LETTER R WITH DOT BELOW => LATIN CAPITAL LETTER R WITH DOT BELOW
int(z'1E5D'), int(z'1E5C'), & ! LATIN SMALL LETTER R WITH DOT BELOW AND MACRON => LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
int(z'1E5F'), int(z'1E5E'), & ! LATIN SMALL LETTER R WITH LINE BELOW => LATIN CAPITAL LETTER R WITH LINE BELOW
int(z'1E61'), int(z'1E60'), & ! LATIN SMALL LETTER S WITH DOT ABOVE => LATIN CAPITAL LETTER S WITH DOT ABOVE
int(z'1E63'), int(z'1E62'), & ! LATIN SMALL LETTER S WITH DOT BELOW => LATIN CAPITAL LETTER S WITH DOT BELOW
int(z'1E65'), int(z'1E64'), & ! LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE => LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
int(z'1E67'), int(z'1E66'), & ! LATIN SMALL LETTER S WITH CARON AND DOT ABOVE => LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
int(z'1E69'), int(z'1E68'), & ! LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE => LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
int(z'1E6B'), int(z'1E6A'), & ! LATIN SMALL LETTER T WITH DOT ABOVE => LATIN CAPITAL LETTER T WITH DOT ABOVE
int(z'1E6D'), int(z'1E6C'), & ! LATIN SMALL LETTER T WITH DOT BELOW => LATIN CAPITAL LETTER T WITH DOT BELOW
int(z'1E6F'), int(z'1E6E'), & ! LATIN SMALL LETTER T WITH LINE BELOW => LATIN CAPITAL LETTER T WITH LINE BELOW
int(z'1E71'), int(z'1E70'), & ! LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW => LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
int(z'1E73'), int(z'1E72'), & ! LATIN SMALL LETTER U WITH DIAERESIS BELOW => LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
int(z'1E75'), int(z'1E74'), & ! LATIN SMALL LETTER U WITH TILDE BELOW => LATIN CAPITAL LETTER U WITH TILDE BELOW
int(z'1E77'), int(z'1E76'), & ! LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW => LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
int(z'1E79'), int(z'1E78'), & ! LATIN SMALL LETTER U WITH TILDE AND ACUTE => LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
int(z'1E7B'), int(z'1E7A'), & ! LATIN SMALL LETTER U WITH MACRON AND DIAERESIS => LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
int(z'1E7D'), int(z'1E7C'), & ! LATIN SMALL LETTER V WITH TILDE => LATIN CAPITAL LETTER V WITH TILDE
int(z'1E7F'), int(z'1E7E'), & ! LATIN SMALL LETTER V WITH DOT BELOW => LATIN CAPITAL LETTER V WITH DOT BELOW
int(z'1E81'), int(z'1E80'), & ! LATIN SMALL LETTER W WITH GRAVE => LATIN CAPITAL LETTER W WITH GRAVE
int(z'1E83'), int(z'1E82'), & ! LATIN SMALL LETTER W WITH ACUTE => LATIN CAPITAL LETTER W WITH ACUTE
int(z'1E85'), int(z'1E84'), & ! LATIN SMALL LETTER W WITH DIAERESIS => LATIN CAPITAL LETTER W WITH DIAERESIS
int(z'1E87'), int(z'1E86'), & ! LATIN SMALL LETTER W WITH DOT ABOVE => LATIN CAPITAL LETTER W WITH DOT ABOVE
int(z'1E89'), int(z'1E88'), & ! LATIN SMALL LETTER W WITH DOT BELOW => LATIN CAPITAL LETTER W WITH DOT BELOW
int(z'1E8B'), int(z'1E8A'), & ! LATIN SMALL LETTER X WITH DOT ABOVE => LATIN CAPITAL LETTER X WITH DOT ABOVE
int(z'1E8D'), int(z'1E8C'), & ! LATIN SMALL LETTER X WITH DIAERESIS => LATIN CAPITAL LETTER X5 WITH DIAERESIS
int(z'1E8F'), int(z'1E8E'), & ! LATIN SMALL LETTER Y WITH DOT ABOVE => LATIN CAPITAL LETTER Y WITH DOT ABOVE
int(z'1E91'), int(z'1E90'), & ! LATIN SMALL LETTER Z WITH CIRCUMFLEX => LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
int(z'1E93'), int(z'1E92'), & ! LATIN SMALL LETTER Z WITH DOT BELOW => LATIN CAPITAL LETTER Z WITH DOT BELOW
int(z'1E95'), int(z'1E94'), & ! LATIN SMALL LETTER Z WITH LINE BELOW => LATIN CAPITAL LETTER Z WITH LINE BELOW
int(z'1EA1'), int(z'1EA0'), & ! LATIN SMALL LETTER A WITH DOT BELOW => LATIN CAPITAL LETTER A WITH DOT BELOW
int(z'1EA3'), int(z'1EA2'), & ! LATIN SMALL LETTER A WITH HOOK ABOVE => LATIN CAPITAL LETTER A WITH HOOK ABOVE
int(z'1EA5'), int(z'1EA4'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE => LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
int(z'1EA7'), int(z'1EA6'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE => LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
int(z'1EA9'), int(z'1EA8'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE => LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
int(z'1EAB'), int(z'1EAA'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE => LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
int(z'1EAD'), int(z'1EAC'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW => LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
int(z'1EAF'), int(z'1EAE'), & ! LATIN SMALL LETTER A WITH BREVE AND ACUTE => LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
int(z'1EB1'), int(z'1EB0'), & ! LATIN SMALL LETTER A WITH BREVE AND GRAVE => LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
int(z'1EB3'), int(z'1EB2'), & ! LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE => LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
int(z'1EB5'), int(z'1EB4'), & ! LATIN SMALL LETTER A WITH BREVE AND TILDE => LATIN CAPITAL LETTER A WITH BREVE AND TILDE
int(z'1EB7'), int(z'1EB6'), & ! LATIN SMALL LETTER A WITH BREVE AND DOT BELOW => LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
int(z'1EB9'), int(z'1EB8'), & ! LATIN SMALL LETTER E WITH DOT BELOW => LATIN CAPITAL LETTER E WITH DOT BELOW
int(z'1EBB'), int(z'1EBA'), & ! LATIN SMALL LETTER E WITH HOOK ABOVE => LATIN CAPITAL LETTER E WITH HOOK ABOVE
int(z'1EBD'), int(z'1EBC'), & ! LATIN SMALL LETTER E WITH TILDE => LATIN CAPITAL LETTER E WITH TILDE
int(z'1EBF'), int(z'1EBE'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE => LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
int(z'1EC1'), int(z'1EC0'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE => LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
int(z'1EC3'), int(z'1EC2'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE => LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
int(z'1EC5'), int(z'1EC4'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE => LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
int(z'1EC7'), int(z'1EC6'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW => LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
int(z'1EC9'), int(z'1EC8'), & ! LATIN SMALL LETTER I WITH HOOK ABOVE => LATIN CAPITAL LETTER I WITH HOOK ABOVE
int(z'1ECB'), int(z'1ECA'), & ! LATIN SMALL LETTER I WITH DOT BELOW => LATIN CAPITAL LETTER I WITH DOT BELOW
int(z'1ECD'), int(z'1ECC'), & ! LATIN SMALL LETTER O WITH DOT BELOW => LATIN CAPITAL LETTER O WITH DOT BELOW
int(z'1ECF'), int(z'1ECE'), & ! LATIN SMALL LETTER O WITH HOOK ABOVE => LATIN CAPITAL LETTER O WITH HOOK ABOVE
int(z'1ED1'), int(z'1ED0'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE => LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
int(z'1ED3'), int(z'1ED2'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE => LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
int(z'1ED5'), int(z'1ED4'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE => LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
int(z'1ED7'), int(z'1ED6'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE => LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
int(z'1ED9'), int(z'1ED8'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW => LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
int(z'1EDB'), int(z'1EDA'), & ! LATIN SMALL LETTER O WITH HORN AND ACUTE => LATIN CAPITAL LETTER O WITH HORN AND ACUTE
int(z'1EDD'), int(z'1EDC'), & ! LATIN SMALL LETTER O WITH HORN AND GRAVE => LATIN CAPITAL LETTER O WITH HORN AND GRAVE
int(z'1EDF'), int(z'1EDE'), & ! LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE => LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
int(z'1EE1'), int(z'1EE0'), & ! LATIN SMALL LETTER O WITH HORN AND TILDE => LATIN CAPITAL LETTER O WITH HORN AND TILDE
int(z'1EE3'), int(z'1EE2'), & ! LATIN SMALL LETTER O WITH HORN AND DOT BELOW => LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
int(z'1EE5'), int(z'1EE4'), & ! LATIN SMALL LETTER U WITH DOT BELOW => LATIN CAPITAL LETTER U WITH DOT BELOW
int(z'1EE7'), int(z'1EE6'), & ! LATIN SMALL LETTER U WITH HOOK ABOVE => LATIN CAPITAL LETTER U WITH HOOK ABOVE
int(z'1EE9'), int(z'1EE8'), & ! LATIN SMALL LETTER U WITH HORN AND ACUTE => LATIN CAPITAL LETTER U WITH HORN AND ACUTE
int(z'1EEB'), int(z'1EEA'), & ! LATIN SMALL LETTER U WITH HORN AND GRAVE => LATIN CAPITAL LETTER U WITH HORN AND GRAVE
int(z'1EED'), int(z'1EEC'), & ! LATIN SMALL LETTER U WITH HORN AND HOCK ABOVE => LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
int(z'1EEF'), int(z'1EEE'), & ! LATIN SMALL LETTER U WITH HORN AND TILDE => LATIN CAPITAL LETTER U WITH HORN AND TILDE
int(z'1EF1'), int(z'1EF0'), & ! LATIN SMALL LETTER U WITH HORN AND DOT BELOW => LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
int(z'1EF3'), int(z'1EF2'), & ! LATIN SMALL LETTER Y WITH GRAVE => LATIN CAPITAL LETTER Y WITH GRAVE
int(z'1EF5'), int(z'1EF4'), & ! LATIN SMALL LETTER Y WITH DOT BELOW => LATIN CAPITAL LETTER Y WITH DOT BELOW
int(z'1EF7'), int(z'1EF6'), & ! LATIN SMALL LETTER Y WITH HOOK ABOVE => LATIN CAPITAL LETTER Y WITH HOOK ABOVE
int(z'1EF9'), int(z'1EF8'), & ! LATIN SMALL LETTER Y WITH TILDE => LATIN CAPITAL LETTER Y WITH TILDE
int(z'1F00'), int(z'1F08'), & ! GREEK SMALL LETTER ALPHA WITH PSILI => GREEK CAPITAL LETTER ALPHA WITH PSILI
int(z'1F01'), int(z'1F09'), & ! GREEK SMALL LETTER ALPHA WITH DASIA => GREEK CAPITAL LETTER ALPHA WITH DASIA
int(z'1F02'), int(z'1F0A'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA => GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
int(z'1F03'), int(z'1F0B'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA => GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
int(z'1F04'), int(z'1F0C'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA => GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
int(z'1F05'), int(z'1F0D'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA => GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
int(z'1F06'), int(z'1F0E'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI => GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
int(z'1F07'), int(z'1F0F'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI => GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
int(z'1F10'), int(z'1F18'), & ! GREEK SMALL LETTER EPSILON WITH PSILI => GREEK CAPITAL LETTER EPSILON WITH PSILI
int(z'1F11'), int(z'1F19'), & ! GREEK SMALL LETTER EPSILON WITH DASIA => GREEK CAPITAL LETTER EPSILON WITH DASIA
int(z'1F12'), int(z'1F1A'), & ! GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA => GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
int(z'1F13'), int(z'1F1B'), & ! GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA => GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
int(z'1F14'), int(z'1F1C'), & ! GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA => GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
int(z'1F15'), int(z'1F1D'), & ! GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA => GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
int(z'1F20'), int(z'1F28'), & ! GREEK SMALL LETTER ETA WITH PSILI => GREEK CAPITAL LETTER ETA WITH PSILI
int(z'1F21'), int(z'1F29'), & ! GREEK SMALL LETTER ETA WITH DASIA => GREEK CAPITAL LETTER ETA WITH DASIA
int(z'1F22'), int(z'1F2A'), & ! GREEK SMALL LETTER ETA WITH PSILI AND VARIA => GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
int(z'1F23'), int(z'1F2B'), & ! GREEK SMALL LETTER ETA WITH DASIA AND VARIA => GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
int(z'1F24'), int(z'1F2C'), & ! GREEK SMALL LETTER ETA WITH PSILI AND OXIA => GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
int(z'1F25'), int(z'1F2D'), & ! GREEK SMALL LETTER ETA WITH DASIA AND OXIA => GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
int(z'1F26'), int(z'1F2E'), & ! GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI => GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
int(z'1F27'), int(z'1F2F'), & ! GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI => GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
int(z'1F30'), int(z'1F38'), & ! GREEK SMALL LETTER IOTA WITH PSILI => GREEK CAPITAL LETTER IOTA WITH PSILI
int(z'1F31'), int(z'1F39'), & ! GREEK SMALL LETTER IOTA WITH DASIA => GREEK CAPITAL LETTER IOTA WITH DASIA
int(z'1F32'), int(z'1F3A'), & ! GREEK SMALL LETTER IOTA WITH PSILI AND VARIA => GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
int(z'1F33'), int(z'1F3B'), & ! GREEK SMALL LETTER IOTA WITH DASIA AND VARIA => GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
int(z'1F34'), int(z'1F3C'), & ! GREEK SMALL LETTER IOTA WITH PSILI AND OXIA => GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
int(z'1F35'), int(z'1F3D'), & ! GREEK SMALL LETTER IOTA WITH DASIA AND OXIA => GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
int(z'1F36'), int(z'1F3E'), & ! GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI => GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
int(z'1F37'), int(z'1F3F'), & ! GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI => GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
int(z'1F40'), int(z'1F48'), & ! GREEK SMALL LETTER OMICRON WITH PSILI => GREEK CAPITAL LETTER OMICRON WITH PSILI
int(z'1F41'), int(z'1F49'), & ! GREEK SMALL LETTER OMICRON WITH DASIA => GREEK CAPITAL LETTER OMICRON WITH DASIA
int(z'1F42'), int(z'1F4A'), & ! GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA => GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
int(z'1F43'), int(z'1F4B'), & ! GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA => GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
int(z'1F44'), int(z'1F4C'), & ! GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA => GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
int(z'1F45'), int(z'1F4D'), & ! GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA => GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
int(z'1F51'), int(z'1F59'), & ! GREEK SMALL LETTER UPSILON WITH DASIA => GREEK CAPITAL LETTER UPSILON WITH OASIS
int(z'1F53'), int(z'1F5B'), & ! GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA => GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
int(z'1F55'), int(z'1F5D'), & ! GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA => GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
int(z'1F57'), int(z'1F5F'), & ! GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI => GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
int(z'1F60'), int(z'1F68'), & ! GREEK SMALL LETTER OMEGA WITh PSILI => GREEK CAPITAL LETTER OMEGA WITH PSILI
int(z'1F61'), int(z'1F69'), & ! GREEK SMALL LETTER OMEGA WITH DASIA => GREEK CAPITAL LETTER OMEGA WITH DASIA
int(z'1F62'), int(z'1F6A'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA => GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
int(z'1F63'), int(z'1F6B'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA => GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
int(z'1F64'), int(z'1F6C'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA => GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
int(z'1F65'), int(z'1F6D'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA => GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
int(z'1F66'), int(z'1F6E'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI => GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
int(z'1F67'), int(z'1F6F'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI => GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
int(z'1F80'), int(z'1F88'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ALPHA WITh PSILI AND PROSGEGRAMMENI
int(z'1F81'), int(z'1F89'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
int(z'1F82'), int(z'1F8A'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
int(z'1F83'), int(z'1F8B'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
int(z'1F84'), int(z'1F8C'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMEN
int(z'1F85'), int(z'1F8D'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMEN
int(z'1F86'), int(z'1F8E'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1F87'), int(z'1F8F'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1F90'), int(z'1F98'), & ! GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
int(z'1F91'), int(z'1F99'), & ! GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
int(z'1F92'), int(z'1F9A'), & ! GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
int(z'1F93'), int(z'1F9B'), & ! GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
int(z'1F94'), int(z'1F9C'), & ! GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
int(z'1F95'), int(z'1F9D'), & ! GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
int(z'1F96'), int(z'1F9E'), & ! GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1F97'), int(z'1F9F'), & ! GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1FA0'), int(z'1FA8'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
int(z'1FA1'), int(z'1FA9'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
int(z'1FA2'), int(z'1FAA'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
int(z'1FA3'), int(z'1FAB'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
int(z'1FA4'), int(z'1FAC'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
int(z'1FA5'), int(z'1FAD'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI => GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
int(z'1FA6'), int(z'1FAE'), & ! GREEK SMALL LETTER OMEGA WITh PSILI AND PERISPOMENI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1FA7'), int(z'1FAF'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND PEPISPOMENI AND YPOGEGRAMMENI => GREEK CAPITAL LETTER OMECA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1FB0'), int(z'1FB8'), & ! GREEK SMALL LETTER ALPHA WITH VRACHY => GREEK CAPITAL LETTER ALPHA WITH VRACHY
int(z'1FB1'), int(z'1FB9'), & ! GREEK SMALL LETTER ALPHA WITH MACRON => GREEK CAPITAL LETTER ALPHA WITH MACRON
int(z'1FD0'), int(z'1FD8'), & ! GREEK SMALL LETTER IOTA WITH VRACHY => GREEK CAPITAL LETTER IOTA WITH VRACHY
int(z'1FD1'), int(z'1FD9'), & ! GREEK SMALL LETTER IOTA WITH MACRON => GREEK CAPITAL LETTER IOTA WITH MACRON
int(z'1FE0'), int(z'1FE8'), & ! GREEK SMALL LETTER UPSILON WITH VRACHY => GREEK CAPITAL LETTER UPSILON WITH VRACHY
int(z'1FE1'), int(z'1FE9'), & ! GREEK SMALL LETTER UPSILON WITH MACRON => GREEK CAPITAL LETTER UPSILON WITH MACRON
int(z'24D0'), int(z'24B6'), & ! CIRCLED LATIN SMALL LETTER A => CIRCLED LATIN CAPITAL LETTER A
int(z'24D1'), int(z'24B7'), & ! CIRCLED LATIN SMALL LETTER B => CIRCLED LATIN CAPITAL LETTER B
int(z'24D2'), int(z'24B8'), & ! CIRCLED LATIN SMALL LETTER C => CIRCLED LATIN CAPITAL LETTER C
int(z'24D3'), int(z'24B9'), & ! CIRCLED LATIN SMALL LETTER D => CIRCLED LATIN CAPITAL LETTER D
int(z'24D4'), int(z'24BA'), & ! CIRCLED LATIN SMALL LETTER E => CIRCLED LATIN CAPITAL LETTER E
int(z'24D5'), int(z'24BB'), & ! CIRCLED LATIN SMALL LETTER F => CIRCLED LATIN CAPITAL LETTER F
int(z'24D6'), int(z'24BC'), & ! CIRCLED LATIN SMALL LETTER G => CIRCLED LATIN CAPITAL LETTER G
int(z'24D7'), int(z'24BD'), & ! CIRCLED LATIN SMALL LETTER H => CIRCLED LATIN CAPITAL LETTER H
int(z'24D8'), int(z'24BE'), & ! CIRCLED LATIN SMALL LETTER I => CIRCLED LATIN CAPITAL LETTER I
int(z'24D9'), int(z'24BF'), & ! CIRCLED LATIN SMALL LETTER J => CIRCLED LATIN CAPITAL LETTER J
int(z'24DA'), int(z'24C0'), & ! CIRCLED LATIN SMALL LETTER K => CIRCLED LATIN CAPITAL LETTER K
int(z'24DB'), int(z'24C1'), & ! CIRCLED LATIN SMALL LETTER L => CIRCLED LATIN CAPITAL LETTER L
int(z'24DC'), int(z'24C2'), & ! CIRCLED LATIN SMALL LETTER M => CIRCLED LATIN CAPITAL LETTER M
int(z'24DD'), int(z'24C3'), & ! CIRCLED LATIN SMALL LETTER N => CIRCLED LATIN CAPITAL LETTER N
int(z'24DE'), int(z'24C4'), & ! CIRCLED LATIN SMALL LETTER O => CIRCLED LATIN CAPITAL LETTER O
int(z'24DF'), int(z'24C5'), & ! CIRCLED LATIN SMALL LETTER P => CIRCLED LATIN CAPITAL LETTER P
int(z'24E0'), int(z'24C6'), & ! CIRCLED LATIN SMALL LETTER Q => CIRCLED LATIN CAPITAL LETTER Q
int(z'24E1'), int(z'24C7'), & ! CIRCLED LATIN SMALL LETTER R => CIRCLED LATIN CAPITAL LETTER R
int(z'24E2'), int(z'24C8'), & ! CIRCLED LATIN SMALL LETTER S => CIRCLED LATIN CAPITAL LETTER S
int(z'24E3'), int(z'24C9'), & ! CIRCLED LATIN SMALL LETTER T => CIRCLED LATIN CAPITAL LETTER T
int(z'24E4'), int(z'24CA'), & ! CIRCLED LATIN SMALL LETTER U => CIRCLED LATIN CAPITAL LETTER U
int(z'24E5'), int(z'24CB'), & ! CIRCLED LATIN SMALL LETTER V => CIRCLED LATIN CAPITAL LETTER V
int(z'24E6'), int(z'24CC'), & ! CIRCLED LATIN SMALL LETTER W => CIRCLED LATIN CAPITAL LETTER W
int(z'24E7'), int(z'24CD'), & ! CIRCLED LATIN SMALL LETTER X => CIRCLED LATIN CAPITAL LETTER X
int(z'24E8'), int(z'24CE'), & ! CIRCLED LATIN SMALL LETTER Y => CIRCLED LATIN CAPITAL LETTER Y
int(z'24E9'), int(z'24CF'), & ! CIRCLED LATIN SMALL LETTER Z => CIRCLED LATIN CAPITAL LETTER Z
int(z'FF41'), int(z'FF21'), & ! FULLWIDTH LATIN SMALL LETTER A => FULLWIDTH LATIN CAPITAL LETTER A
int(z'FF42'), int(z'FF22'), & ! FULLWIDTH LATIN SMALL LETTER B => FULLWIDTH LATIN CAPITAL LETTER B
int(z'FF43'), int(z'FF23'), & ! FULLWIDTH LATIN SMALL LETTER C => FULLWIDTH LATIN CAPITAL LETTER C
int(z'FF44'), int(z'FF24'), & ! FULLWIDTH LATIN SMALL LETTER D => FULLWIDTH LATIN CAPITAL LETTER D
int(z'FF45'), int(z'FF25'), & ! FULLWIDTH LATIN SMALL LETTER E => FULLWIDTH LATIN CAPITAL LETTER E
int(z'FF46'), int(z'FF26'), & ! FULLWIDTH LATIN SMALL LETTER F => FULLWIDTH LATIN CAPITAL LETTER F
int(z'FF47'), int(z'FF27'), & ! FULLWIDTH LATIN SMALL LETTER G => FULLWIDTH LATIN CAPITAL LETTER G
int(z'FF48'), int(z'FF28'), & ! FULLWIDTH LATIN SMALL LETTER H => FULLWIDTH LATIN CAPITAL LETTER H
int(z'FF49'), int(z'FF29'), & ! FULLWIDTH LATIN SMALL LETTER I => FULLWIDTH LATIN CAPITAL LETTER I
int(z'FF4A'), int(z'FF2A'), & ! FULLWIDTH LATIN SMALL LETTER J => FULLWIDTH LATIN CAPITAL LETTER J
int(z'FF4B'), int(z'FF2B'), & ! FULLWIDTH LATIN SMALL LETTER K => FULLWIDTH LATIN CAPITAL LETTER K
int(z'FF4C'), int(z'FF2C'), & ! FULLWIDTH LATIN SMALL LETTER L => FULLWIDTH LATIN CAPITAL LETTER L
int(z'FF4D'), int(z'FF2D'), & ! FULLWIDTH LATIN SMALL LETTER M => FULLWIDTH LATIN CAPITAL LETTER M
int(z'FF4E'), int(z'FF2E'), & ! FULLWIDTH LATIN SMALL LETTER N => FULLWIDTH LATIN CAPITAL LETTER N
int(z'FF4F'), int(z'FF2F'), & ! FULLWIDTH LATIN SMALL LETTER O => FULLWIDTH LATIN CAPITAL LETTER O
int(z'FF50'), int(z'FF30'), & ! FULLWIDTH LATIN SMALL LETTER P => FULLWIDTH LATIN CAPITAL LETTER P
int(z'FF51'), int(z'FF31'), & ! FULLWIDTH LATIN SMALL LETTER Q => FULLWIDTH LATIN CAPITAL LETTER Q
int(z'FF52'), int(z'FF32'), & ! FULLWIDTH LATIN SMALL LETTER R => FULLWIDTH LATIN CAPITAL LETTER R
int(z'FF53'), int(z'FF33'), & ! FULLWIDTH LATIN SMALL LETTER S => FULLWIDTH LATIN CAPITAL LETTER S
int(z'FF54'), int(z'FF34'), & ! FULLWIDTH LATIN SMALL LETTER T => FULLWIDTH LATIN CAPITAL LETTER T
int(z'FF55'), int(z'FF35'), & ! FULLWIDTH LATIN SMALL LETTER U => FULLWIDTH LATIN CAPITAL LETTER U
int(z'FF56'), int(z'FF36'), & ! FULLWIDTH LATIN SMALL LETTER V => FULLWIDTH LATIN CAPITAL LETTER V
int(z'FF57'), int(z'FF37'), & ! FULLWIDTH LATIN SMALL LETTER W => FULLWIDTH LATIN CAPITAL LETTER W
int(z'FF58'), int(z'FF38'), & ! FULLWIDTH LATIN SMALL LETTER X => FULLWIDTH LATIN CAPITAL LETTER X
int(z'FF59'), int(z'FF39'), & ! FULLWIDTH LATIN SMALL LETTER Y => FULLWIDTH LATIN CAPITAL LETTER Y
int(z'FF5A'), int(z'FF3A')] & ! FULLWIDTH LATIN SMALL LETTER Z => FULLWIDTH LATIN CAPITAL LETTER Z
,shape(low_to_up),order=[2,1])

integer,parameter :: highlow=666
integer,parameter :: up_to_low(highlow,2)= reshape([ &
int(z'0041'), int(z'0061'), & ! LATIN SMALL LETTER A <= LATIN CAPITAL LETTER A
int(z'0042'), int(z'0062'), & ! LATIN SMALL LETTER B <= LATIN CAPITAL LETTER B
int(z'0043'), int(z'0063'), & ! LATIN SMALL LETTER C <= LATIN CAPITAL LETTER C
int(z'0044'), int(z'0064'), & ! LATIN SMALL LETTER D <= LATIN CAPITAL LETTER D
int(z'0045'), int(z'0065'), & ! LATIN SMALL LETTER E <= LATIN CAPITAL LETTER E
int(z'0046'), int(z'0066'), & ! LATIN SMALL LETTER F <= LATIN CAPITAL LETTER F
int(z'0047'), int(z'0067'), & ! LATIN SMALL LETTER G <= LATIN CAPITAL LETTER G
int(z'0048'), int(z'0068'), & ! LATIN SMALL LETTER H <= LATIN CAPITAL LETTER H
int(z'0049'), int(z'0131'), & ! LATIN SMALL LETTER DOTLESS I <= LATIN CAPITAL LETTER I
int(z'004A'), int(z'006A'), & ! LATIN SMALL LETTER J <= LATIN CAPITAL LETTER J
int(z'004B'), int(z'006B'), & ! LATIN SMALL LETTER K <= LATIN CAPITAL LETTER K
int(z'004C'), int(z'006C'), & ! LATIN SMALL LETTER L <= LATIN CAPITAL LETTER L
int(z'004D'), int(z'006D'), & ! LATIN SMALL LETTER M <= LATIN CAPITAL LETTER M
int(z'004E'), int(z'006E'), & ! LATIN SMALL LETTER N <= LATIN CAPITAL LETTER N
int(z'004F'), int(z'006F'), & ! LATIN SMALL LETTER O <= LATIN CAPITAL LETTER O
int(z'0050'), int(z'0070'), & ! LATIN SMALL LETTER P <= LATIN CAPITAL LETTER P
int(z'0051'), int(z'0071'), & ! LATIN SMALL LETTER Q <= LATIN CAPITAL LETTER Q
int(z'0052'), int(z'0072'), & ! LATIN SMALL LETTER R <= LATIN CAPITAL LETTER R
int(z'0053'), int(z'0073'), & ! LATIN SMALL LETTER S <= LATIN CAPITAL LETTER S
int(z'0054'), int(z'0074'), & ! LATIN SMALL LETTER T <= LATIN CAPITAL LETTER T
int(z'0055'), int(z'0075'), & ! LATIN SMALL LETTER U <= LATIN CAPITAL LETTER U
int(z'0056'), int(z'0076'), & ! LATIN SMALL LETTER V <= LATIN CAPITAL LETTER V
int(z'0057'), int(z'0077'), & ! LATIN SMALL LETTER W <= LATIN CAPITAL LETTER W
int(z'0058'), int(z'0078'), & ! LATIN SMALL LETTER X <= LATIN CAPITAL LETTER X
int(z'0059'), int(z'0079'), & ! LATIN SMALL LETTER Y <= LATIN CAPITAL LETTER Y
int(z'005A'), int(z'007A'), & ! LATIN SMALL LETTER Z <= LATIN CAPITAL LETTER Z
int(z'00C0'), int(z'00E0'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER A GRAVE
int(z'00C1'), int(z'00E1'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER A ACUTE
int(z'00C2'), int(z'00E2'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER A CIRCUMFLEX
int(z'00C3'), int(z'00E3'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER A TILDE
int(z'00C4'), int(z'00E4'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER A DIAERESIS
int(z'00C5'), int(z'00E5'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER A RING
int(z'00C6'), int(z'00E6'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER A E
int(z'00C7'), int(z'00E7'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER C CEDILLA
int(z'00C8'), int(z'00E8'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER E GRAVE
int(z'00C9'), int(z'00E9'), & ! LATIN SMALL LETTER A GRAVE <= LATIN CAPITAL LETTER E ACUTE
int(z'00CA'), int(z'00EA'), & ! LATIN SMALL LETTER E CIRCUMFLEX <= LATIN CAPITAL LETTER E CIRCUMFLEX
int(z'00CB'), int(z'00EB'), & ! LATIN SMALL LETTER E DIAERESIS <= LATIN CAPITAL LETTER E DIAERESIS
int(z'00CC'), int(z'00EC'), & ! LATIN SMALL LETTER I GRAVE <= LATIN CAPITAL LETTER I GRAVE
int(z'00CD'), int(z'00ED'), & ! LATIN SMALL LETTER I ACUTE <= LATIN CAPITAL LETTER I ACUTE
int(z'00CE'), int(z'00EE'), & ! LATIN SMALL LETTER I CIRCUMFLEX <= LATIN CAPITAL LETTER I CIRCUMFLEX
int(z'00CF'), int(z'00EF'), & ! LATIN SMALL LETTER I DIAERESIS <= LATIN CAPITAL LETTER I DIAERESIS
int(z'00D0'), int(z'00F0'), & ! LATIN SMALL LETTER ETH <= LATIN CAPITAL LETTER ETH
int(z'00D1'), int(z'00F1'), & ! LATIN SMALL LETTER N TILDE <= LATIN CAPITAL LETTER N TILDE
int(z'00D2'), int(z'00F2'), & ! LATIN SMALL LETTER O GRAVE <= LATIN CAPITAL LETTER O GRAVE
int(z'00D3'), int(z'00F3'), & ! LATIN SMALL LETTER O ACUTE <= LATIN CAPITAL LETTER O ACUTE
int(z'00D4'), int(z'00F4'), & ! LATIN SMALL LETTER O CIRCUMFLEX <= LATIN CAPITAL LETTER O CIRCUMFLEX
int(z'00D5'), int(z'00F5'), & ! LATIN SMALL LETTER O TILDE <= LATIN CAPITAL LETTER O TILDE
int(z'00D6'), int(z'00F6'), & ! LATIN SMALL LETTER O DIAERESIS <= LATIN CAPITAL LETTER O DIAERESIS
int(z'00D8'), int(z'00F8'), & ! LATIN SMALL LETTER O SLASH <= LATIN CAPITAL LETTER O SLASH
int(z'00D9'), int(z'00F9'), & ! LATIN SMALL LETTER U GRAVE <= LATIN CAPITAL LETTER U GRAVE
int(z'00DA'), int(z'00FA'), & ! LATIN SMALL LETTER U ACUTE <= LATIN CAPITAL LETTER U ACUTE
int(z'00DB'), int(z'00FB'), & ! LATIN SMALL LETTER U CIRCUMFLEX <= LATIN CAPITAL LETTER U CIRCUMFLEX
int(z'00DC'), int(z'00FC'), & ! LATIN SMALL LETTER U DIAERESIS <= LATIN CAPITAL LETTER U DIAERESIS
int(z'00DD'), int(z'00FD'), & ! LATIN SMALL LETTER Y ACUTE <= LATIN CAPITAL LETTER Y ACUTE
int(z'00DE'), int(z'00FE'), & ! LATIN SMALL LETTER THORN <= LATIN CAPITAL LETTER THORN
int(z'0100'), int(z'0101'), & ! LATIN SMALL LETTER A WITH MACRON <= LATIN CAPITAL LETTER A WITH MACRON
int(z'0102'), int(z'0103'), & ! LATIN SMALL LETTER A WITH BREVE <= LATIN CAPITAL LETTER A WITH BREVE
int(z'0104'), int(z'0105'), & ! LATIN SMALL LETTER A WITH OGONEK <= LATIN CAPITAL LETTER A WITH OGONEK
int(z'0106'), int(z'0107'), & ! LATIN SMALL LETTER C WITH ACUTE <= LATIN CAPITAL LETTER C WITH ACUTE
int(z'0108'), int(z'0109'), & ! LATIN SMALL LETTER C WITH CIRCUMFLEX <= LATIN CAPITAL LETTER C WITH CIRCUMFLEX
int(z'010A'), int(z'010B'), & ! LATIN SMALL LETTER C WITH DOT ABOVE <= LATIN CAPITAL LETTER C WITH DOT ABOVE
int(z'010C'), int(z'010D'), & ! LATIN SMALL LETTER C WITH CARON <= LATIN CAPITAL LETTER C WITH CARON
int(z'010E'), int(z'010F'), & ! LATIN SMALL LETTER D WITH CARON <= LATIN CAPITAL LETTER D WITH CARON
int(z'0110'), int(z'0111'), & ! LATIN SMALL LETTER D WITH STROKE <= LATIN CAPITAL LETTER D WITH STROKE
int(z'0112'), int(z'0113'), & ! LATIN SMALL LETTER E WITH MACRON <= LATIN CAPITAL LETTER E WITH MACRON
int(z'0114'), int(z'0115'), & ! LATIN SMALL LETTER E WITH BREVE <= LATIN CAPITAL LETTER E WITH BREVE
int(z'0116'), int(z'0117'), & ! LATIN SMALL LETTER E WITH DOT ABOVE <= LATIN CAPITAL LETTER E WITH DOT ABOVE
int(z'0118'), int(z'0119'), & ! LATIN SMALL LETTER E WITH OGONEK <= LATIN CAPITAL LETTER E WITH OGONEK
int(z'011A'), int(z'011B'), & ! LATIN SMALL LETTER E WITH CARON <= LATIN CAPITAL LETTER E WITH CARON
int(z'011C'), int(z'011D'), & ! LATIN SMALL LETTER G WITH CIRCUMFLEX <= LATIN CAPITAL LETTER G WITH CIRCUMFLEX
int(z'011E'), int(z'011F'), & ! LATIN SMALL LETTER G WITH BREVE <= LATIN CAPITAL LETTER G WITH BREVE
int(z'0120'), int(z'0121'), & ! LATIN SMALL LETTER G WITH DOT ABOVE <= LATIN CAPITAL LETTER G WITH DOT ABOVE
int(z'0122'), int(z'0123'), & ! LATIN SMALL LETTER G WITH CEDILLA <= LATIN CAPITAL LETTER G WITH CEDILLA
int(z'0124'), int(z'0125'), & ! LATIN SMALL LETTER H WITH CIRCUMFLEX <= LATIN CAPITAL LETTER H WITH CIRCUMFLEX
int(z'0126'), int(z'0127'), & ! LATIN SMALL LETTER H WITH STROKE <= LATIN CAPITAL LETTER H WITH STROKE
int(z'0128'), int(z'0129'), & ! LATIN SMALL LETTER I WITH TILDE <= LATIN CAPITAL LETTER I WITH TILDE
int(z'012A'), int(z'012B'), & ! LATIN SMALL LETTER I WITH MACRON <= LATIN CAPITAL LETTER I WITH MACRON
int(z'012C'), int(z'012D'), & ! LATIN SMALL LETTER I WITH BREVE <= LATIN CAPITAL LETTER I WITH BREVE
int(z'012E'), int(z'012F'), & ! LATIN SMALL LETTER I WITH OGONEK <= LATIN CAPITAL LETTER I WITH OGONEK
int(z'0130'), int(z'0069'), & ! LATIN sMALL LETTER I <= LATIN CAPITAL LETTER I WITH DOT ABOVE
int(z'0132'), int(z'0133'), & ! LATIN SMALL LIGATURE IJ <= LATIN CAPITAL LIGATURE IJ
int(z'0134'), int(z'0135'), & ! LATIN SMALL LETTER J WITH CIRCUMFLEX <= LATIN CAPITAL LETTER J WITH CIRCUMFLEX
int(z'0136'), int(z'0137'), & ! LATIN SMALL LETTER K WITH CEDILLA <= LATIN CAPITAL LETTER K WITH CEDILLA
int(z'0139'), int(z'013A'), & ! LATIN SMALL LETTER L WITH ACUTE <= LATIN CAPITAL LETTER L WITH ACUTE
int(z'013B'), int(z'013C'), & ! LATIN SMALL LETTER L WITH CEDILLA <= LATIN CAPITAL LETTER L WITH CEDILLA
int(z'013D'), int(z'013E'), & ! LATIN SMALL LETTER L WITH CARON <= LATIN CAPITAL LETTER L WITH CARON
int(z'013F'), int(z'0140'), & ! LATIN SMALL LETTER L WITH MIDDLE DOT <= LATIN CAPITAL LETTER L WITH MIDDLE DOT
int(z'0141'), int(z'0142'), & ! LATIN SMALL LETTER L WITH STROKE <= LATIN CAPITAL LETTER L WITH STROKE
int(z'0143'), int(z'0144'), & ! LATIN SMALL LETTER N WITH ACUTE <= LATIN CAPITAL LETTER N WITH ACUTE
int(z'0145'), int(z'0146'), & ! LATIN SMALL LETTER N WITH CEDILLA <= LATIN CAPITAL LETTER N WITH CEDILLA
int(z'0147'), int(z'0148'), & ! LATIN SMALL LETTER N WITH CARON <= LATIN CAPITAL LETTER N WITH CARON
int(z'014A'), int(z'014B'), & ! LATIN SMALL LETTER ENG (SAMI) <= LATIN CAPITAL LETTER ENG (SAMI)
int(z'014C'), int(z'014D'), & ! LATIN SMALL LETTER O WITH MACRON <= LATIN CAPITAL LETTER O WITH MACRON
int(z'014E'), int(z'014F'), & ! LATIN SMALL LETTER O WITH BREVE <= LATIN CAPITAL LETTER O WITH BREVE
int(z'0150'), int(z'0151'), & ! LATIN SMALL LETTER O WITH DOUBLE ACUTE <= LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
int(z'0152'), int(z'0153'), & ! LATIN SMALL LIGATURE OE <= LATIN CAPITAL LIGATURE OE
int(z'0154'), int(z'0155'), & ! LATIN SMALL LETTER R WITH ACUTE <= LATIN CAPITAL LETTER R WITH ACUTE
int(z'0156'), int(z'0157'), & ! LATIN SMALL LETTER R WITH CEDILLA <= LATIN CAPITAL LETTER R WITH CEDILLA
int(z'0158'), int(z'0159'), & ! LATIN SMALL LETTER R WITH CARON <= LATIN CAPITAL LETTER R WITH CARON
int(z'015A'), int(z'015B'), & ! LATIN SMALL LETTER S WITH ACUTE <= LATIN CAPITAL LETTER S WITH ACUTE
int(z'015C'), int(z'015D'), & ! LATIN SMALL LETTER S WITH CIRCUMFLEX <= LATIN CAPITAL LETTER S WITH CIRCUMFLEX
int(z'015E'), int(z'015F'), & ! LATIN SMALL LETTER S WITH CEDILLA <= LATIN CAPITAL LETTER S WITH CEDILLA
int(z'0160'), int(z'0161'), & ! LATIN SMALL LETTER S WITH CARON <= LATIN CAPITAL LETTER S WITH CARON
int(z'0162'), int(z'0163'), & ! LATIN SMALL LETTER T WITH CEDILLA <= LATIN CAPITAL LETTER T WITH CEDILLA
int(z'0164'), int(z'0165'), & ! LATIN SMALL LETTER T WITH CARON <= LATIN CAPITAL LETTER T WITH CARON
int(z'0166'), int(z'0167'), & ! LATIN SMALL LETTER T WITH STROKE <= LATIN CAPITAL LETTER T WITH STROKE
int(z'0168'), int(z'0169'), & ! LATIN SMALL LETTER U WITH TILDE <= LATIN CAPITAL LETTER U WITH TILDE
int(z'016A'), int(z'016B'), & ! LATIN SMALL LETTER U WITH MACRON <= LATIN CAPITAL LETTER U WITH MACRON
int(z'016C'), int(z'016D'), & ! LATIN SMALL LETTER U WITH BREVE <= LATIN CAPITAL LETTER U WITH BREVE
int(z'016E'), int(z'016F'), & ! LATIN SMALL LETTER U WITH RING ABOVE <= LATIN CAPITAL LETTER U WITH RING ABOVE
int(z'0170'), int(z'0171'), & ! LATIN SMALL LETTER U WITH DOUBLE ACUTE <= LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
int(z'0172'), int(z'0173'), & ! LATIN SMALL LETTER U WITH OGONEK <= LATIN CAPITAL LETTER U WITH OGONEK
int(z'0174'), int(z'0175'), & ! LATIN SMALL LETTER W WITH CIRCUMFLEX <= LATIN CAPITAL LETTER W WITH CIRCUMFLEX
int(z'0176'), int(z'0177'), & ! LATIN SMALL LETTER Y WITH CIRCUMFLEX <= LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
int(z'0178'), int(z'00FF'), & ! LATIN SMALL LETTER Y DIAERESIS <= LATIN CAPITAL LETTER Y WITH DIAERESIS
int(z'0179'), int(z'017A'), & ! LATIN SMALL LETTER Z WITH ACUTE <= LATIN CAPITAL LETTER Z WITH ACUTE
int(z'017B'), int(z'017C'), & ! LATIN SMALL LETTER Z WITH DOT ABOVE <= LATIN CAPITAL LETTER Z WITH DOT ABOVE
int(z'017D'), int(z'017E'), & ! LATIN SMALL LETTER Z WITH CARON <= LATIN CAPITAL LETTER Z WITH CARON
int(z'0181'), int(z'0253'), & ! LATIN SMALL LETTER B WITH HOOK <= LATIN CAPITAL LETTER B WITH HOOK
int(z'0182'), int(z'0183'), & ! LATIN SMALL LETTER B WITH TOPBAR <= LATIN CAPITAL LETTER B WITH TOPBAR
int(z'0184'), int(z'0185'), & ! LATIN SMALL LETTER TONE SIX <= LATIN CAPITAL LETTER TONE SIX
int(z'0186'), int(z'0254'), & ! LATIN SMALL LETTER OPEN O <= LATIN CAPITAL LETTER OPEN O
int(z'0187'), int(z'0188'), & ! LATIN SMALL LETTER C WITH HOOK <= LATIN CAPITAL LETTER C WITH HOOK
int(z'018A'), int(z'0257'), & ! LATIN SMALL LETTER D WITH HOOK <= LATIN CAPITAL LETTER D WITH HOOK
int(z'018B'), int(z'018C'), & ! LATIN SMALL LETTER D WITH TOPBAR <= LATIN CAPITAL LETTER D WITH TOPBAR
int(z'018E'), int(z'0258'), & ! LATIN SMALL LETTER REVERSED E <= LATIN CAPITAL LETTER REVERSED E
int(z'018F'), int(z'0259'), & ! LATIN SMALL LETTER SCHWA <= LATIN CAPITAL LETTER SCHWA
int(z'0190'), int(z'025B'), & ! LATIN SMALL LETTER OPEN E <= LATIN CAPITAL LETTER OPEN E
int(z'0191'), int(z'0192'), & ! LATIN SMALL LETTER F WITH HOOK <= LATIN CAPITAL LETTER F WITH HOOK
int(z'0193'), int(z'0260'), & ! LATIN SMALL LETTER G WITH HOOK <= LATIN CAPITAL LETTER G WITH HOOK
int(z'0194'), int(z'0263'), & ! LATIN SMALL LETTER GAMMA <= LATIN CAPITAL LETTER GAMMA
int(z'0196'), int(z'0269'), & ! LATIN SMALL LETTER IOTA <= LATIN CAPITAL LETTER IOTA
int(z'0197'), int(z'0268'), & ! LATIN SMALL LETTER I WITH STROKE <= LATIN CAPITAL LETTER I WITH STROKE
int(z'0198'), int(z'0199'), & ! LATIN SMALL LETTER K WITH HOOK <= LATIN CAPITAL LETTER K WITH HOOK
int(z'019C'), int(z'026F'), & ! LATIN SMALL LETTER TURNED M <= LATIN CAPITAL LETTER TURNED M
int(z'019D'), int(z'0272'), & ! LATIN SMALL LETTER N WITH LEFT HOOK <= LATIN CAPITAL LETTER N WITH LEFT HOOK
int(z'019F'), int(z'0275'), & ! LATIN SMALL LETTER BARRED O <= LATIN CAPITAL LETTER O WITH MIDDLE TILDE
int(z'01A0'), int(z'01A1'), & ! LATIN SMALL LETTER O WITH HORN <= LATIN CAPITAL LETTER O WITH HORN
int(z'01A2'), int(z'01A3'), & ! LATIN SMALL LETTER OI <= LATIN CAPITAL LETTER OI
int(z'01A4'), int(z'01A5'), & ! LATIN SMALL LETTER P WITH HOOK <= LATIN CAPITAL LETTER P WITH HOOK
int(z'01A7'), int(z'01A8'), & ! LATIN SMALL LETTER TONE TWO <= LATIN CAPITAL LETTER TONE TWO
int(z'01A9'), int(z'0283'), & ! LATIN SMALL LETTER ESH <= LATIN CAPITAL LETTER ESH
int(z'01AC'), int(z'01AD'), & ! LATIN SMALL LETTER T WITH HOOK <= LATIN CAPITAL LETTER T WITH HOOK
int(z'01AE'), int(z'0288'), & ! LATIN SMALL LETTER T WITH RETROFLEX HOOK <= LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
int(z'01AF'), int(z'01B0'), & ! LATIN SMALL LETTER U WITH HORN <= LATIN CAPITAL LETTER U WITH HORN
int(z'01B1'), int(z'028A'), & ! LATIN SMALL LETTER UPSILON <= LATIN CAPITAL LETTER UPSILON
int(z'01B2'), int(z'028B'), & ! LATIN SMALL LETTER V WITH HOOK <= LATIN CAPITAL LETTER V WITH HOOK
int(z'01B3'), int(z'01B4'), & ! LATIN SMALL LETTER Y WITH HOOK <= LATIN CAPITAL LETTER Y WITH HOOK
int(z'01B5'), int(z'01B6'), & ! LATIN SMALL LETTER Z WITH STROKE <= LATIN CAPITAL LETTER Z WITH STROKE
int(z'01B7'), int(z'0292'), & ! LATIN SMALL LETTER EZH <= LATIN CAPITAL LETTER EZH
int(z'01B8'), int(z'01B9'), & ! LATIN SMALL LETTER EZH REVERSED <= LATIN CAPITAL LETTER EZH REVERSED
int(z'01BC'), int(z'01BD'), & ! LATIN SMALL LETTER TONE FIVE <= LATIN CAPITAL LETTER TONE FIVE
int(z'01C4'), int(z'01C6'), & ! LATIN SMALL LETTER DZ WITH CARON <= LATIN CAPITAL LETTER DZ WITH CARON
int(z'01C7'), int(z'01C9'), & ! LATIN SMALL LETTER LJ <= LATIN CAPITAL LETTER LJ
int(z'01CA'), int(z'01CC'), & ! LATIN SMALL LETTER NJ <= LATIN CAPITAL LETTER NJ
int(z'01CD'), int(z'01CE'), & ! LATIN SMALL LETTER A WITH CARON <= LATIN CAPITAL LETTER A WITH CARON
int(z'01CF'), int(z'01D0'), & ! LATIN SMALL LETTER I WITH CARON <= LATIN CAPITAL LETTER I WITH CARON
int(z'01D1'), int(z'01D2'), & ! LATIN SMALL LETTER O WITH CARON <= LATIN CAPITAL LETTER O WITH CARON
int(z'01D3'), int(z'01D4'), & ! LATIN SMALL LETTER U WITH CARON <= LATIN CAPITAL LETTER U WITH CARON
int(z'01D5'), int(z'01D6'), & ! LATIN SMALL LETTER U WITH DIAERESIS AND MACRON <= LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
int(z'01D7'), int(z'01D8'), & ! LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE <= LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
int(z'01D9'), int(z'01DA'), & ! LATIN SMALL LETTER U WITH DIAERESIS AND CARON <= LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
int(z'01DB'), int(z'01DC'), & ! LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE <= LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
int(z'01DE'), int(z'01DF'), & ! LATIN SMALL LETTER A WITH DIAERESIS AND MACRON <= LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
int(z'01E0'), int(z'01E1'), & ! LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON <= LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
int(z'01E2'), int(z'01E3'), & ! LATIN SMALL LIGATURE AE WITH MACRON <= LATIN CAPITAL LIGATURE AE MTH MACRON
int(z'01E4'), int(z'01E5'), & ! LATIN SMALL LETTER G WITH STROKE <= LATIN CAPITAL LETTER G WITH STROKE
int(z'01E6'), int(z'01E7'), & ! LATIN SMALL LETTER G WITH CARON <= LATIN CAPITAL LETTER G WITH CARON
int(z'01E8'), int(z'01E9'), & ! LATIN SMALL LETTER K WITH CARON <= LATIN CAPITAL LETTER K WITH CARON
int(z'01EA'), int(z'01EB'), & ! LATIN SMALL LETTER O WITH OGONEK <= LATIN CAPITAL LETTER O WITH OGONEK
int(z'01EC'), int(z'01ED'), & ! LATIN SMALL LETTER O WITH OGONEK AND MACRON <= LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
int(z'01EE'), int(z'01EF'), & ! LATIN SMALL LETTER EZH WITH CARON <= LATIN CAPITAL LETTER EZH WITH CARON
int(z'01F1'), int(z'01F3'), & ! LATIN SMALL LETTER DZ <= LATIN CAPITAL LETTER DZ
int(z'01F4'), int(z'01F5'), & ! LATIN SMALL LETTER G WITH ACUTE <= LATIN CAPITAL LETTER G WITH ACUTE
int(z'01FA'), int(z'01FB'), & ! LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE <= LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
int(z'01FC'), int(z'01FD'), & ! LATIN SMALL LIGATURE AE WITH ACUTE <= LATIN CAPITAL LIGATURE AE WITH ACUTE
int(z'01FE'), int(z'01FF'), & ! LATIN SMALL LETTER O WITH STROKE AND ACUTE <= LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
int(z'0200'), int(z'0201'), & ! LATIN SMALL LETTER A WITH DOUBLE GRAVE <= LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
int(z'0202'), int(z'0203'), & ! LATIN SMALL LETTER A WITH INVERTED BREVE <= LATIN CAPITAL LETTER A WITH INVERTED BREVE
int(z'0204'), int(z'0205'), & ! LATIN SMALL LETTER E WITH DOUBLE GRAVE <= LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
int(z'0206'), int(z'0207'), & ! LATIN SMALL LETTER E WITH INVERTED BREVE <= LATIN CAPITAL LETTER E WITH INVERTED BREVE
int(z'0208'), int(z'0209'), & ! LATIN SMALL LETTER I WITH DOUBLE GRAVE <= LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
int(z'020A'), int(z'020B'), & ! LATIN SMALL LETTER I WITH INVERTED BREVE <= LATIN CAPITAL LETTER I WITH INVERTED BREVE
int(z'020C'), int(z'020D'), & ! LATIN SMALL LETTER O WITH DOUBLE GRAVE <= LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
int(z'020E'), int(z'020F'), & ! LATIN SMALL LETTER O WITH INVERTED BREVE <= LATIN CAPITAL LETTER O WITH INVERTED BREVE
int(z'0210'), int(z'0211'), & ! LATIN SMALL LETTER R WITH DOUBLE GRAVE <= LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
int(z'0212'), int(z'0213'), & ! LATIN SMALL LETTER R WITH INVERTED BREVE <= LATIN CAPITAL LETTER R WITH INVERTED BREVE
int(z'0214'), int(z'0215'), & ! LATIN SMALL LETTER U WITH DOUBLE GRAVE <= LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
int(z'0216'), int(z'0217'), & ! LATIN SMALL LETTER U WITH INVERTED BREVE <= LATIN CAPITAL LETTER U WITH INVERTED BREVE
int(z'0386'), int(z'03AC'), & ! GREEK SMALL LETTER ALPHA WITH TONOS <= GREEK CAPITAL LETTER ALPHA WITH TONOS
int(z'0388'), int(z'03AD'), & ! GREEK SMALL LETTER EPSILON WITH TONOS <= GREEK CAPITAL LETTER EPSILON WITH TONOS
int(z'0389'), int(z'03AE'), & ! GREEK SMALL LETTER ETA WITH TONOS <= GREEK CAPITAL LETTER ETA WITH TONOS
int(z'038A'), int(z'03AF'), & ! GREEK SMALL LETTER IOTA WITH TONOS <= GREEK CAPITAL LETTER IOTA WITH TONOS
int(z'038C'), int(z'03CC'), & ! GREEK SMALL LETTER OMICRON WITH TONOS <= GREEK CAPITAL LETTER OMICRON WITH TONOS
int(z'038E'), int(z'03CD'), & ! GREEK SMALL LETTER UPSILON WITH TONOS <= GREEK CAPITAL LETTER UPSILON WITH TONOS
int(z'038F'), int(z'03CE'), & ! GREEK SMALL LETTER OMEGA WITH TONOS <= GREEK CAPITAL LETTER OMEGA WITH TONOS
int(z'0391'), int(z'03B1'), & ! GREEK SMALL LETTER ALPHA <= GREEK CAPITAL LETTER ALPHA
int(z'0392'), int(z'03B2'), & ! GREEK SMALL LETTER BETA <= GREEK CAPITAL LETTER BETA
int(z'0393'), int(z'03B3'), & ! GREEK SMALL LETTER GAMMA <= GREEK CAPITAL LETTER GAMMA
int(z'0394'), int(z'03B4'), & ! GREEK SMALL LETTER DELTA <= GREEK CAPITAL LETTER DELTA
int(z'0395'), int(z'03B5'), & ! GREEK SMALL LETTER EPSILON <= GREEK CAPITAL LETTER EPSILON
int(z'0396'), int(z'03B6'), & ! GREEK SMALL LETTER ZETA <= GREEK CAPITAL LETTER ZETA
int(z'0397'), int(z'03B7'), & ! GREEK SMALL LETTER ETA <= GREEK CAPITAL LETTER ETA
int(z'0398'), int(z'03B8'), & ! GREEK SMALL LETTER THETA <= GREEK CAPITAL LETTER THETA
int(z'0399'), int(z'03B9'), & ! GREEK SMALL LETTER IOTA <= GREEK CAPITAL LETTER IOTA
int(z'039A'), int(z'03BA'), & ! GREEK SMALL LETTER KAPPA <= GREEK CAPITAL LETTER KAPPA
int(z'039B'), int(z'03BB'), & ! GREEK SMALL LETTER LAMDA <= GREEK CAPITAL LETTER LAMDA
int(z'039C'), int(z'03BC'), & ! GREEK SMALL LETTER MU <= GREEK CAPITAL LETTER MU
int(z'039D'), int(z'03BD'), & ! GREEK SMALL LETTER NU <= GREEK CAPITAL LETTER NU
int(z'039E'), int(z'03BE'), & ! GREEK SMALL LETTER XI <= GREEK CAPITAL LETTER XI
int(z'039F'), int(z'03BF'), & ! GREEK SMALL LETTER OMICRON <= GREEK CAPITAL LETTER OMICRON
int(z'03A0'), int(z'03C0'), & ! GREEK SMALL LETTER PI <= GREEK CAPITAL LETTER PI
int(z'03A1'), int(z'03C1'), & ! GREEK SMALL LETTER RHO <= GREEK CAPITAL LETTER RHO
int(z'03A3'), int(z'03C3'), & ! GREEK SMALL LETTER SIGMA <= GREEK CAPITAL LETTER SIGMA
int(z'03A4'), int(z'03C4'), & ! GREEK SMALL LETTER TAU <= GREEK CAPITAL LETTER TAU
int(z'03A5'), int(z'03C5'), & ! GREEK SMALL LETTER UPSILON <= GREEK CAPITAL LETTER UPSILON
int(z'03A6'), int(z'03C6'), & ! GREEK SMALL LETTER PHI <= GREEK CAPITAL LETTER PHI
int(z'03A7'), int(z'03C7'), & ! GREEK SMALL LETTER CHI <= GREEK CAPITAL LETTER CHI
int(z'03A8'), int(z'03C8'), & ! GREEK SMALL LETTER PSI <= GREEK CAPITAL LETTER PSI
int(z'03A9'), int(z'03C9'), & ! GREEK SMALL LETTER OMEGA <= GREEK CAPITAL LETTER OMEGA
int(z'03AA'), int(z'03CA'), & ! GREEK SMALL LETTER IOTA WITH DIALYTIKA <= GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
int(z'03AB'), int(z'03CB'), & ! GREEK SMALL LETTER UPSILON WITH DIALYTIKA <= GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
int(z'03E2'), int(z'03E3'), & ! COPTIC SMALL LETTER SHEI <= COPTIC CAPITAL LETTER SHEI
int(z'03E4'), int(z'03E5'), & ! COPTIC SMALL LETTER FEI <= COPTIC CAPITAL LETTER FEI
int(z'03E6'), int(z'03E7'), & ! COPTIC SMALL LETTER KHEI <= COPTIC CAPITAL LETTER KHEI
int(z'03E8'), int(z'03E9'), & ! COPTIC SMALL LETTER HORI <= COPTIC CAPITAL LETTER HORI
int(z'03EA'), int(z'03EB'), & ! COPTIC SMALL LETTER GANGIA <= COPTIC CAPITAL LETTER GANGIA
int(z'03EC'), int(z'03ED'), & ! COPTIC SMALL LETTER SHIMA <= COPTIC CAPITAL LETTER SHIMA
int(z'03EE'), int(z'03EF'), & ! COPTIC SMALL LETTER DEI <= COPTIC CAPITAL LETTER DEI
int(z'0401'), int(z'0451'), & ! CYRILLIC SMALL LETTER IO <= CYRILLIC CAPITAL LETTER IO
int(z'0402'), int(z'0452'), & ! CYRILLIC SMALL LETTER DJE (SERBOCROATIAN) <= CYRILLIC CAPITAL LETTER DJE (SERBOCROATIAN)
int(z'0403'), int(z'0453'), & ! CYRILLIC SMALL LETTER GJE <= CYRILLIC CAPITAL LETTER GJE
int(z'0404'), int(z'0454'), & ! CYRILLIC SMALL LETTER UKRAINIAN IE <= CYRILLIC CAPITAL LETTER UKRAINIAN IE
int(z'0405'), int(z'0455'), & ! CYRILLIC SMALL LETTER DZE <= CYRILLIC CAPITAL LETTER DZE
int(z'0406'), int(z'0456'), & ! CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I <= CYRILLIC CAPITAL LETTER BYELORUSSIAN_UKRAINIAN I
int(z'0407'), int(z'0457'), & ! CYRILLIC SMALL LETTER YI (UKRAINIAN) <= CYRILLIC CAPITAL LETTER YI (UKRAINIAN)
int(z'0408'), int(z'0458'), & ! CYRILLIC SMALL LETTER JE <= CYRILLIC CAPITAL LETTER JE
int(z'0409'), int(z'0459'), & ! CYRILLIC SMALL LETTER LJE <= CYRILLIC CAPITAL LETTER LJE
int(z'040A'), int(z'045A'), & ! CYRILLIC SMALL LETTER NJE <= CYRILLIC CAPITAL LETTER NJE
int(z'040B'), int(z'045B'), & ! CYRILLIC SMALL LETTER TSHE (SERBOCROATIAN) <= CYRILLIC CAPITAL LETTER TSHE (SERBOCROATIAN)
int(z'040C'), int(z'045C'), & ! CYRILLIC SMALL LETTER KJE <= CYRILLIC CAPITAL LETTER KJE
int(z'040E'), int(z'045E'), & ! CYRILLIC SMALL LETTER SHORT U (BYELORUSSIAN) <= CYRILLIC CAPITAL LETTER SHORT U (BYELORUSSIAN)
int(z'040F'), int(z'045F'), & ! CYRILLIC SMALL LETTER DZHE <= CYRILLIC CAPITAL LETTER DZHE
int(z'0410'), int(z'0430'), & ! CYRILLIC SMALL LETTER A <= CYRILLIC CAPITAL LETTER A
int(z'0411'), int(z'0431'), & ! CYRILLIC SMALL LETTER BE <= CYRILLIC CAPITAL LETTER BE
int(z'0412'), int(z'0432'), & ! CYRILLIC SMALL LETTER VE <= CYRILLIC CAPITAL LETTER VE
int(z'0413'), int(z'0433'), & ! CYRILLIC SMALL LETTER GHE <= CYRILLIC CAPITAL LETTER GHE
int(z'0414'), int(z'0434'), & ! CYRILLIC SMALL LETTER DE <= CYRILLIC CAPITAL LETTER DE
int(z'0415'), int(z'0435'), & ! CYRILLIC SMALL LETTER IE <= CYRILLIC CAPITAL LETTER IE
int(z'0416'), int(z'0436'), & ! CYRILLIC SMALL LETTER ZHE <= CYRILLIC CAPITAL LETTER ZHE
int(z'0417'), int(z'0437'), & ! CYRILLIC SMALL LETTER ZE <= CYRILLIC CAPITAL LETTER ZE
int(z'0418'), int(z'0438'), & ! CYRILLIC SMALL LETTER I <= CYRILLIC CAPITAL LETTER I
int(z'0419'), int(z'0439'), & ! CYRILLIC SMALL LETTER SHORT I <= CYRILLIC CAPITAL LETTER SHORT I
int(z'041A'), int(z'043A'), & ! CYRILLIC SMALL LETTER KA <= CYRILLIC CAPITAL LETTER KA
int(z'041B'), int(z'043B'), & ! CYRILLIC SMALL LETTER EL <= CYRILLIC CAPITAL LETTER EL
int(z'041C'), int(z'043C'), & ! CYRILLIC SMALL LETTER EM <= CYRILLIC CAPITAL LETTER EM
int(z'041D'), int(z'043D'), & ! CYRILLIC SMALL LETTER EN <= CYRILLIC CAPITAL LETTER EN
int(z'041E'), int(z'043E'), & ! CYRILLIC SMALL LETTER O <= CYRILLIC CAPITAL LETTER O
int(z'041F'), int(z'043F'), & ! CYRILLIC SMALL LETTER PE <= CYRILLIC CAPITAL LETTER PE
int(z'0420'), int(z'0440'), & ! CYRILLIC SMALL LETTER ER <= CYRILLIC CAPITAL LETTER ER
int(z'0421'), int(z'0441'), & ! CYRILLIC SMALL LETTER ES <= CYRILLIC CAPITAL LETTER ES
int(z'0422'), int(z'0442'), & ! CYRILLIC SMALL LETTER TE <= CYRILLIC CAPITAL LETTER TE
int(z'0423'), int(z'0443'), & ! CYRILLIC SMALL LETTER U <= CYRILLIC CAPITAL LETTER U
int(z'0424'), int(z'0444'), & ! CYRILLIC SMALL LETTER EF <= CYRILLIC CAPITAL LETTER EF
int(z'0425'), int(z'0445'), & ! CYRILLIC SMALL LETTER HA <= CYRILLIC CAPITAL LETTER HA
int(z'0426'), int(z'0446'), & ! CYRILLIC SMALL LETTER TSE <= CYRILLIC CAPITAL LETTER TSE
int(z'0427'), int(z'0447'), & ! CYRILLIC SMALL LETTER CHE <= CYRILLIC CAPITAL LETTER CHE
int(z'0428'), int(z'0448'), & ! CYRILLIC SMALL LETTER SHA <= CYRILLIC CAPITAL LETTER SHA
int(z'0429'), int(z'0449'), & ! CYRILLIC SMALL LETTER SHCHA <= CYRILLIC CAPITAL LETTER SHCHA
int(z'042A'), int(z'044A'), & ! CYRILLIC SMALL LETTER HARD SIGN <= CYRILLIC CAPITAL LETTER HARD SIGN
int(z'042B'), int(z'044B'), & ! CYRILLIC SMALL LETTER YERU <= CYRILLIC CAPITAL LETTER YERU
int(z'042C'), int(z'044C'), & ! CYRILLIC SMALL LETTER SOFT SIGN <= CYRILLIC CAPITAL LETTER SOFT SIGN
int(z'042D'), int(z'044D'), & ! CYRILLIC SMALL LETTER E <= CYRILLIC CAPITAL LETTER E
int(z'042E'), int(z'044E'), & ! CYRILLIC SMALL LETTER YU <= CYRILLIC CAPITAL LETTER YU
int(z'042F'), int(z'044F'), & ! CYRILLIC SMALL LETTER YA <= CYRILLIC CAPITAL LETTER YA
int(z'0460'), int(z'0461'), & ! CYRILLIC SMALL LETTER OMEGA <= CYRILLIC CAPITAL LETTER OMEGA
int(z'0462'), int(z'0463'), & ! CYRILLIC SMALL LETTER YAT <= CYRILLIC CAPITAL LETTER YAT
int(z'0464'), int(z'0465'), & ! CYRILLIC SMALL LETTER IOTIFIED E <= CYRILLIC CAPITAL LETTER IOTIFIED E
int(z'0466'), int(z'0467'), & ! CYRILLIC SMALL LETTER LITTLE YUS <= CYRILLIC CAPITAL LETTER LITTLE YUS
int(z'0468'), int(z'0469'), & ! CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS <= CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
int(z'046A'), int(z'046B'), & ! CYRILLIC SMALL LETTER BIG YUS <= CYRILLIC CAPITAL LETTER BIG YUS
int(z'046C'), int(z'046D'), & ! CYRILLIC SMALL LETTER IOTIFIED BIG YUS <= CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
int(z'046E'), int(z'046F'), & ! CYRILLIC SMALL LETTER KSI <= CYRILLIC CAPITAL LETTER KSI
int(z'0470'), int(z'0471'), & ! CYRILLIC SMALL LETTER PSI <= CYRILLIC CAPITAL LETTER PSI
int(z'0472'), int(z'0473'), & ! CYRILLIC SMALL LETTER FITA <= CYRILLIC CAPITAL LETTER FITA
int(z'0474'), int(z'0475'), & ! CYRILLIC SMALL LETTER IZHITSA <= CYRILLIC CAPITAL LETTER IZHITSA
int(z'0476'), int(z'0477'), & ! CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT <= CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
int(z'0478'), int(z'0479'), & ! CYRILLIC SMALL LETTER UK <= CYRILLIC CAPITAL LETTER UK
int(z'047A'), int(z'047B'), & ! CYRILLIC SMALL LETTER ROUND OMEGA <= CYRILLIC CAPITAL LETTER ROUND OMEGA
int(z'047C'), int(z'047D'), & ! CYRILLIC SMALL LETTER OMEGA WITH TITLO <= CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
int(z'047E'), int(z'047F'), & ! CYRILLIC SMALL LETTER OT <= CYRILLIC CAPITAL LETTER OT
int(z'0480'), int(z'0481'), & ! CYRILLIC SMALL LETTER KOPPA <= CYRILLIC CAPITAL LETTER KOPPA
int(z'0490'), int(z'0491'), & ! CYRILLIC SMALL LETTER GHE WITH UPTURN <= CYRILLIC CAPITAL LETTER GHE WITH UPTURN
int(z'0492'), int(z'0493'), & ! CYRILLIC SMALL LETTER GHE WITH STROKE <= CYRILLIC CAPITAL LETTER GHE WITH STROKE
int(z'0494'), int(z'0495'), & ! CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK <= CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
int(z'0496'), int(z'0497'), & ! CYRILLIC SMALL LETTER ZHE WITH DESCENDER <= CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
int(z'0498'), int(z'0499'), & ! CYRILLIC SMALL LETTER ZE WITH DESCENDER <= CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
int(z'049A'), int(z'049B'), & ! CYRILLIC SMALL LETTER KA WITH DESCENDER <= CYRILLIC CAPITAL LETTER KA WITH DESCENDER
int(z'049C'), int(z'049D'), & ! CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE <= CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
int(z'049E'), int(z'049F'), & ! CYRILLIC SMALL LETTER KA WITH STROKE <= CYRILLIC CAPITAL LETTER KA WITH STROKE
int(z'04A0'), int(z'04A1'), & ! CYRILLIC SMALL LETTER EASHKIR KA <= CYRILLIC CAPITAL LETTER BASHKIR KA
int(z'04A2'), int(z'04A3'), & ! CYRILLIC SMALL LETTER EN WITH DESCENOER <= CYRILLIC CAPITAL LETTER EN WITH DESCENDER
int(z'04A4'), int(z'04A5'), & ! CYRILLIC SMALL LIGATURE EN GHE <= CYRILLIC CAPITAL LIGATURE EN GHF
int(z'04A6'), int(z'04A7'), & ! CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK (ABKHASIAN) <= CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK (ABKHASIAN)
int(z'04A8'), int(z'04A9'), & ! CYRILLIC SMALL LETTER ABKHASIAN HA <= CYRILLIC CAPITAL LETTER ABKHASIAN HA
int(z'04AA'), int(z'04AB'), & ! CYRILLIC SMALL LETTER ES WITH DESCENDER <= CYRILLIC CAPITAL LETTER ES WITH DESCENDER
int(z'04AC'), int(z'04AD'), & ! CYRILLIC SMALL LETTER TE WITH DESCENDER <= CYRILLIC CAPITAL LETTER TE WITH DESCENDER
int(z'04AE'), int(z'04AF'), & ! CYRILLIC SMALL LETTER STRAIGHT U <= CYRILLIC CAPITAL LETTER STRAIGHT U
int(z'04B0'), int(z'04B1'), & ! CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE <= CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
int(z'04B2'), int(z'04B3'), & ! CYRILLIC SMALL LETTER HA WITH DESCENDER <= CYRILLIC CAPITAL LETTER HA WITH DESCENDER
int(z'04B4'), int(z'04B5'), & ! CYRILLIC SMALL LIGATURE TE TSE (ABKHASIAN) <= CYRILLIC CAPITAL LIGATURE TE TSE (ABKHASIAN)
int(z'04B6'), int(z'04B7'), & ! CYRILLIC SMALL LETTER CHE WITH DESCENDER <= CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
int(z'04B8'), int(z'04B9'), & ! CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE <= CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
int(z'04BA'), int(z'04BB'), & ! CYRILLIC SMALL LETTER SHHA <= CYRILLIC CAPITAL LETTER SHHA
int(z'04BC'), int(z'04BD'), & ! CYRILLIC SMALL LETTER ABKHASIAN CHE <= CYRILLIC CAPITAL LETTER ABKHASIAN CHE
int(z'04BE'), int(z'04BF'), & ! CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER <= CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
int(z'04C1'), int(z'04C2'), & ! CYRILLIC SMALL LETTER ZHE WITH BREVE <= CYRILLIC CAPITAL LETTER ZHE WITH BREVE
int(z'04C3'), int(z'04C4'), & ! CYRILLIC SMALL LETTER KA WITH HOOK <= CYRILLIC CAPITAL LETTER KA WITH HOOK
int(z'04C7'), int(z'04C8'), & ! CYRILLIC SMALL LETTER EN WITH HOOK <= CYRILLIC CAPITAL LETTER EN WITH HOOK
int(z'04CB'), int(z'04CC'), & ! CYRILLIC SMALL LETTER KHAKASSIAN CHE <= CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
int(z'04D0'), int(z'04D1'), & ! CYRILLIC SMALL LETTER A WITH BREVE <= CYRILLIC CAPITAL LETTER A WITH BREVE
int(z'04D2'), int(z'04D3'), & ! CYRILLIC SMALL LETTER A WITH DIAERESIS <= CYRILLIC CAPITAL LETTER A WITH DIAERESIS
int(z'04D4'), int(z'04D5'), & ! CYRILLIC SMALL LIGATURE A IE <= CYRILLIC CAPITAL LIGATURE A IE
int(z'04D6'), int(z'04D7'), & ! CYRILLIC SMALL LETTER IE WITH BREVE <= CYRILLIC CAPITAL LETTER IE WITH BREVE
int(z'04D8'), int(z'04D9'), & ! CYRILLIC SMALL LETTER SCHWA <= CYRILLIC CAPITAL LETTER SCHWA
int(z'04DA'), int(z'04DB'), & ! CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS <= CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
int(z'04DC'), int(z'04DD'), & ! CYRILLIC SMALL LETTER ZHE WITH DIAERESIS <= CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
int(z'04DE'), int(z'04DF'), & ! CYRILLIC SMALL LETTER ZE WITH DIAERESIS <= CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
int(z'04E0'), int(z'04E1'), & ! CYRILLIC SMALL LETTER ABKHASIAN DZE <= CYRILLIC CAPITAL LETTER ABKHASIAN DZE
int(z'04E2'), int(z'04E3'), & ! CYRILLIC SMALL LETTER I WITH MACRON <= CYRILLIC CAPITAL LETTER I WITH MACRON
int(z'04E4'), int(z'04E5'), & ! CYRILLIC SMALL LETTER I WITH DIAERESIS <= CYRILLIC CAPITAL LETTER I WITH DIAERESIS
int(z'04E6'), int(z'04E7'), & ! CYRILLIC SMALL LETTER O WITH DIAERESIS <= CYRILLIC CAPITAL LETTER O WITH DIAERESIS
int(z'04E8'), int(z'04E9'), & ! CYRILLIC SMALL LETTER BARRED O <= CYRILLIC CAPITAL LETTER BARRED O
int(z'04EA'), int(z'04EB'), & ! CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS <= CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
int(z'04EE'), int(z'04EF'), & ! CYRILLIC SMALL LETTER U WITH MACRON <= CYRILLIC CAPITAL LETTER U WITH MACRON
int(z'04F0'), int(z'04F1'), & ! CYRILLIC SMALL LETTER U WITH DIAERESIS <= CYRILLIC CAPITAL LETTER U WITH DIAERESIS
int(z'04F2'), int(z'04F3'), & ! CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE <= CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
int(z'04F4'), int(z'04F5'), & ! CYRILLIC SMALL LETTER CHE AITH DIAERESIS <= CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
int(z'04F8'), int(z'04F9'), & ! CYRILLIC SMALL LETTER YERU WITH DIAERESIS <= CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
int(z'0531'), int(z'0561'), & ! ARMENIAN SMALL LETTER AYB <= ARMENIAN CAPITAL LETTER AYB
int(z'0532'), int(z'0562'), & ! ARMENIAN SMALL LETTER BEN <= ARMENIAN CAPITAL LETTER BEN
int(z'0533'), int(z'0563'), & ! ARMENIAN SMALL LETTER GIM <= ARMENIAN CAPITAL LETTER GIM
int(z'0534'), int(z'0564'), & ! ARMENIAN SMALL LETTER DA <= ARMENIAN CAPITAL LETTER DA
int(z'0535'), int(z'0565'), & ! ARMENIAN SMALL LETTER ECH <= ARMENIAN CAPITAL LETTER ECH
int(z'0536'), int(z'0566'), & ! ARMENIAN SMALL LETTER ZA <= ARMENIAN CAPITAL LETTER ZA
int(z'0537'), int(z'0567'), & ! ARMENIAN SMALL LETTER EH <= ARMENIAN CAPITAL LETTER EH
int(z'0538'), int(z'0568'), & ! ARMENIAN SMALL LETTER ET <= ARMENIAN CAPITAL LETTER ET
int(z'0539'), int(z'0569'), & ! ARMENIAN SMALL LETTER TO <= ARMENIAN CAPITAL LETTER TO
int(z'053A'), int(z'056A'), & ! ARMENIAN SMALL LETTER ZHE <= ARMENIAN CAPITAL LETTER ZHE
int(z'053B'), int(z'056B'), & ! ARMENIAN SMALL LETTER INI <= ARMENIAN CAPITAL LETTER INI
int(z'053C'), int(z'056C'), & ! ARMENIAN SMALL LETTER LIWN <= ARMENIAN CAPITAL LETTER LIWN
int(z'053D'), int(z'056D'), & ! ARMENIAN SMALL LETTER XEH <= ARMENIAN CAPITAL LETTER XEH
int(z'053E'), int(z'056E'), & ! ARMENIAN SMALL LETTER CA <= ARMENIAN CAPITAL LETTER CA
int(z'053F'), int(z'056F'), & ! ARMENIAN SMALL LETTER KEN <= ARMENIAN CAPITAL LETTER KEN
int(z'0540'), int(z'0570'), & ! ARMENIAN SMALL LETTER HO <= ARMENIAN CAPITAL LETTER HO
int(z'0541'), int(z'0571'), & ! ARMENIAN SMALL LETTER JA <= ARMENIAN CAPITAL LETTER JA
int(z'0542'), int(z'0572'), & ! ARMENIAN SMALL LETTER GHAD <= ARMENIAN CAPITAL LETTER GHAD
int(z'0543'), int(z'0573'), & ! ARMENIAN SMALL LETTER CHEH <= ARMENIAN CAPITAL LETTER CHEH
int(z'0544'), int(z'0574'), & ! ARMENIAN SMALL LETTER MEN <= ARMENIAN CAPITAL LETTER MEN
int(z'0545'), int(z'0575'), & ! ARMENIAN SMALL LETTER YI <= ARMENIAN CAPITAL LETTER YI
int(z'0546'), int(z'0576'), & ! ARMENIAN SMALL LETTER NOW <= ARMENIAN CAPITAL LETTER NOW
int(z'0547'), int(z'0577'), & ! ARMENIAN SMALL LETTER SNA <= ARMENIAN CAPITAL LETTER SHA
int(z'0548'), int(z'0578'), & ! ARMENIAN SMALL LETTER VO <= ARMENIAN CAPITAL LETTER VO
int(z'0549'), int(z'0579'), & ! ARMENIAN SMALL LETTER CHA <= ARMENIAN CAPITAL LETTER CHA
int(z'054A'), int(z'057A'), & ! ARMENIAN SMALL LETTER PEH <= ARMENIAN CAPITAL LETTER PEH
int(z'054B'), int(z'057B'), & ! ARMENIAN SMALL LETTER JHEH <= ARMENIAN CAPITAL LETTER JHEH
int(z'054C'), int(z'057C'), & ! ARMENIAN SMALL LETTER RA <= ARMENIAN CAPITAL LETTER RA
int(z'054D'), int(z'057D'), & ! ARMENIAN SMALL LETTER SEH <= ARMENIAN CAPITAL LETTER SEH
int(z'054E'), int(z'057E'), & ! ARMENIAN SMALL LETTER VEW <= ARMENIAN CAPITAL LETTER VEW
int(z'054F'), int(z'057F'), & ! ARMENIAN SMALL LETTER TIWN <= ARMENIAN CAPITAL LETTER TIWN
int(z'0550'), int(z'0580'), & ! ARMENIAN SMALL LETTER REH <= ARMENIAN CAPITAL LETTER REH
int(z'0551'), int(z'0581'), & ! ARMENIAN SMALL LETTER CO <= ARMENIAN CAPITAL LETTER CO
int(z'0552'), int(z'0582'), & ! ARMENIAN SMALL LETTER YIWN <= ARMENIAN CAPITAL LETTER YIWN
int(z'0553'), int(z'0583'), & ! ARMENIAN SMALL LETTER PIWP <= ARMENIAN CAPITAL LETTER PIWR
int(z'0554'), int(z'0584'), & ! ARMENIAN SMALL LETTER KEH <= ARMENIAN CAPITAL LETTER KEH
int(z'0555'), int(z'0585'), & ! ARMENIAN SMALL LETTER OH <= ARMENIAN CAPITAL LETTER OH
int(z'0556'), int(z'0586'), & ! ARMENIAN SMALL LETTER FEH <= ARMENIAN CAPITAL LETTER FEH
int(z'10A0'), int(z'10D0'), & ! GEORGIAN LETTER AN <= GEORGIAN CAPITAL LETTER AN (KHUTSURI)
int(z'10A1'), int(z'10D1'), & ! GEORGIAN LETTER BAN <= GEORGIAN CAPITAL LETTER BAN (KHUTSURI)
int(z'10A2'), int(z'10D2'), & ! GEORGIAN LETTER GAN <= GEORGIAN CAPITAL LETTER GAN (KHUTSURI)
int(z'10A3'), int(z'10D3'), & ! GEORGIAN LETTER DON <= GEORGIAN CAPITAL LETTER DON (KHUTSURI)
int(z'10A4'), int(z'10D4'), & ! GEORGIAN LETTER EN <= GEORGIAN CAPITAL LETTER EN (KHUTSURI)
int(z'10A5'), int(z'10D5'), & ! GEORGIAN LETTER VIN <= GEORGIAN CAPITAL LETTER VIN (KHUTSURI)
int(z'10A6'), int(z'10D6'), & ! GEORGIAN LETTER ZEN <= GEORGIAN CAPITAL LETTER ZEN (KHUTSURI)
int(z'10A7'), int(z'10D7'), & ! GEORGIAN LETTER TAN <= GEORGIAN CAPITAL LETTER TAN (KHUTSURI)
int(z'10A8'), int(z'10D8'), & ! GEORGIAN LETTER IN <= GEORGIAN CAPITAL LETTER IN (KHUTSURI)
int(z'10A9'), int(z'10D9'), & ! GEORGIAN LETTER KAN <= GEORGIAN CAPITAL LETTER KAN (KHUTSURI)
int(z'10AA'), int(z'10DA'), & ! GEORGIAN LETTER LAS <= GEORGIAN CAPITAL LETTER LAS (KHUTSURI)
int(z'10AB'), int(z'10DB'), & ! GEORGIAN LETTER MAN <= GEORGIAN CAPITAL LETTER MAN (KHUTSURI)
int(z'10AC'), int(z'10DC'), & ! GEORGIAN LETTER NAR <= GEORGIAN CAPITAL LETTER NAR (KHUTSURI)
int(z'10AD'), int(z'10DD'), & ! GEORGIAN LETTER ON <= GEORGIAN CAPITAL LETTER ON (KHUTSURI)
int(z'10AE'), int(z'10DE'), & ! GEORGIAN LETTER PAR <= GEORGIAN CAPITAL LETTER PAR (KHUTSURI)
int(z'10AF'), int(z'10DF'), & ! GEORGIAN LETTER ZHAR <= GEORGIAN CAPITAL LETTER ZHAR (KHUTSURI)
int(z'10B0'), int(z'10E0'), & ! GEORGIAN LETTER RAE <= GEORGIAN CAPITAL LETTER RAE (KHUTSURI)
int(z'10B1'), int(z'10E1'), & ! GEORGIAN LETTER SAN <= GEORGIAN CAPITAL LETTER SAN (KHUTSURI)
int(z'10B2'), int(z'10E2'), & ! GEORGIAN LETTER TAR <= GEORGIAN CAPITAL LETTER TAR (KHUTSURI)
int(z'10B3'), int(z'10E3'), & ! GEORGIAN LETTER UN <= GEORGIAN CAPITAL LETTER UN (KHUTSURI)
int(z'10B4'), int(z'10E4'), & ! GEORGIAN LETTER PHAR <= GEORGIAN CAPITAL LETTER PHAR (KHUTSURI)
int(z'10B5'), int(z'10E5'), & ! GEORGIAN LETTER KHAR <= GEORGIAN CAPITAL LETTER KHAR (KHUTSURI)
int(z'10B6'), int(z'10E6'), & ! GEORGIAN LETTER GHAN <= GEORGIAN CAPITAL LETTER GHAN (KHUTSURI)
int(z'10B7'), int(z'10E7'), & ! GEORGIAN LETTER QAR <= GEORGIAN CAPITAL LETTER QAR (KHUTSURI)
int(z'10B8'), int(z'10E8'), & ! GEORGIAN LETTER SHIN <= GEORGIAN CAPITAL LETTER SHIN (KHUTSURI)
int(z'10B9'), int(z'10E9'), & ! GEORGIAN LETTER CHIN <= GEORGIAN CAPITAL LETTER CHIN (KHUTSURI)
int(z'10BA'), int(z'10EA'), & ! GEORGIAN LETTER CAN <= GEORGIAN CAPITAL LETTER CAN (KHUTSURI)
int(z'10BB'), int(z'10EB'), & ! GEORGIAN LETTER JIL <= GEORGIAN CAPITAL LETTER JIL (KHUTSURI)
int(z'10BC'), int(z'10EC'), & ! GEORGIAN LETTER CIL <= GEORGIAN CAPITAL LETTER CIL (KHUTSURI)
int(z'10BD'), int(z'10ED'), & ! GEORGIAN LETTER CHAR <= GEORGIAN CAPITAL LETTER CHAR (KHUTSURI)
int(z'10BE'), int(z'10EE'), & ! GEORGIAN LETTER XAN <= GEORGIAN CAPITAL LETTER XAN (KHUTSURI)
int(z'10BF'), int(z'10EF'), & ! GEORGIAN LETTER JHAN <= GEORGIAN CAPITAL LETTER JHAN (KHUTSURI)
int(z'10C0'), int(z'10F0'), & ! GEORGIAN LETTER HAE <= GEORGIAN CAPITAL LETTER HAE (KHUTSURI)
int(z'10C1'), int(z'10F1'), & ! GEORGIAN LETTER HE <= GEORGIAN CAPITAL LETTER HE (KHUTSURI)
int(z'10C2'), int(z'10F2'), & ! GEORGIAN LETTER HIE <= GEORGIAN CAPITAL LETTER HIE (KHUTSURI)
int(z'10C3'), int(z'10F3'), & ! GEORGIAN LETTER WE <= GEORGIAN CAPITAL LETTER WE (KHUTSURI)
int(z'10C4'), int(z'10F4'), & ! GEORGIAN LETTER HAR <= GEORGIAN CAPITAL LETTER HAR (KHUTSURI)
int(z'10C5'), int(z'10F5'), & ! GEORGIAN LETTER HOE <= GEORGIAN CAPITAL LETTER HOE (KHUTSURI)
int(z'1E00'), int(z'1E01'), & ! LATIN SMALL LETTER A WITH RING BELOW <= LATIN CAPITAL LETTER A WITH RING BELOW
int(z'1E02'), int(z'1E03'), & ! LATIN SMALL LETTER B WITH DOT ABOVE <= LATIN CAPITAL LETTER B WITH DOT ABOVE
int(z'1E04'), int(z'1E05'), & ! LATIN SMALL LETTER B WITH DOT BELOW <= LATIN CAPITAL LETTER B WITH DOT BELOW
int(z'1E06'), int(z'1E07'), & ! LATIN SMALL LETTER B WITH LINE BELOW <= LATIN CAPITAL LETTER B WITH LINE BELOW
int(z'1E08'), int(z'1E09'), & ! LATIN SMALL LETTER C WITH CEDILLA AND ACUTE <= LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
int(z'1E0A'), int(z'1E0B'), & ! LATIN SMALL LETTER D WITH DOT ABOVE <= LATIN CAPITAL LETTER D WITH DOT ABOVE
int(z'1E0C'), int(z'1E0D'), & ! LATIN SMALL LETTER D WITH DOT BELOW <= LATIN CAPITAL LETTER D WITH DOT BELOW
int(z'1E0E'), int(z'1E0F'), & ! LATIN SMALL LETTER D WITH LINE BELOW <= LATIN CAPITAL LETTER D WITH LINE BELOW
int(z'1E10'), int(z'1E11'), & ! LATIN SMALL LETTER D WITH CEDILLA <= LATIN CAPITAL LETTER D WITH CEDILLA
int(z'1E12'), int(z'1E13'), & ! LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW <= LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
int(z'1E14'), int(z'1E15'), & ! LATIN SMALL LETTER E WITH MACRON AND GRAVE <= LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
int(z'1E16'), int(z'1E17'), & ! LATIN SMALL LETTER E WITH MACRON AND ACUTE <= LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
int(z'1E18'), int(z'1E19'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW <= LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
int(z'1E1A'), int(z'1E1B'), & ! LATIN SMALL LETTER E WITH TILDE BELOW <= LATIN CAPITAL LETTER E WITH TILDE BELOW
int(z'1E1C'), int(z'1E1D'), & ! LATIN SMALL LETTER E WITH CEDILLA AND BREVE <= LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
int(z'1E1E'), int(z'1E1F'), & ! LATIN SMALL LETTER F WITH DOT ABOVE <= LATIN CAPITAL LETTER F WITH DOT ABOVE
int(z'1E20'), int(z'1E21'), & ! LATIN SMALL LETTER G WITH MACRON <= LATIN CAPITAL LETTER G WITH MACRON
int(z'1E22'), int(z'1E23'), & ! LATIN SMALL LETTER H WITH DOT ABOVE <= LATIN CAPITAL LETTER H WITH DOT ABOVE
int(z'1E24'), int(z'1E25'), & ! LATIN SMALL LETTER H WITH DOT BELOW <= LATIN CAPITAL LETTER H WITH DOT BELOW
int(z'1E26'), int(z'1E27'), & ! LATIN SMALL LETTER H WITH DIAERESIS <= LATIN CAPITAL LETTER H WITH DIAERESIS
int(z'1E28'), int(z'1E29'), & ! LATIN SMALL LETTER H WITH CEDILLA <= LATIN CAPITAL LETTER H WITH CEDILLA
int(z'1E2A'), int(z'1E2B'), & ! LATIN SMALL LETTER H WITH BREVE BELOW <= LATIN CAPITAL LETTER H WITH BREVE BELOW
int(z'1E2C'), int(z'1E2D'), & ! LATIN SMALL LETTER I WITH TILDE BELOW <= LATIN CAPITAL LETTER I WITH TILDE BELOW
int(z'1E2E'), int(z'1E2F'), & ! LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE <= LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
int(z'1E30'), int(z'1E31'), & ! LATIN SMALL LETTER K WITH ACUTE <= LATIN CAPITAL LETTER K WITH ACUTE
int(z'1E32'), int(z'1E33'), & ! LATIN SMALL LETTER K WITH DOT BELOW <= LATIN CAPITAL LETTER K WITH DOT BELOW
int(z'1E34'), int(z'1E35'), & ! LATIN SMALL LETTER K WITH LINE BELOW <= LATIN CAPITAL LETTER K WITH LINE BELOW
int(z'1E36'), int(z'1E37'), & ! LATIN SMALL LETTER L WITH DOT BELOW <= LATIN CAPITAL LETTER L WITH DOT BELOW
int(z'1E38'), int(z'1E39'), & ! LATIN SMALL LETTER L WITH DOT BELOW AND MACRON <= LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
int(z'1E3A'), int(z'1E3B'), & ! LATIN SMALL LETTER L WITH LINE BELOW <= LATIN CAPITAL LETTER L WITH LINE BELOW
int(z'1E3C'), int(z'1E3D'), & ! LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW <= LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
int(z'1E3E'), int(z'1E3F'), & ! LATIN SMALL LETTER M WITH ACUTE <= LATIN CAPITAL LETTER M WITH ACUTE
int(z'1E40'), int(z'1E41'), & ! LATIN SMALL LETTER M WITH DOT ABOVE <= LATIN CAPITAL LETTER M WITH DOT ABOVE
int(z'1E42'), int(z'1E43'), & ! LATIN SMALL LETTER M WITH DOT BELOW <= LATIN CAPITAL LETTER M WITH DOT BELOW
int(z'1E44'), int(z'1E45'), & ! LATIN SMALL LETTER N WITH DOT ABOVE <= LATIN CAPITAL LETTER N WITH DOT ABOVE
int(z'1E46'), int(z'1E47'), & ! LATIN SMALL LETTER N WITH DOT BELOW <= LATIN CAPITAL LETTER N WITH DOT BELOW
int(z'1E48'), int(z'1E49'), & ! LATIN SMALL LETTER N WITH LINE BELOW <= LATIN CAPITAL LETTER N WITH LINE BELOW
int(z'1E4A'), int(z'1E4B'), & ! LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW <= LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
int(z'1E4C'), int(z'1E4D'), & ! LATIN SMALL LETTER O WITH TILDE AND ACUTE <= LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
int(z'1E4E'), int(z'1E4F'), & ! LATIN SMALL LETTER O WITH TlLDE AND DIAERESIS <= LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
int(z'1E50'), int(z'1E51'), & ! LATIN SMALL LETTER O WITH MACRON AND GRAVE <= LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
int(z'1E52'), int(z'1E53'), & ! LATIN SMALL LETTER O WITH MACRON AND ACUTE <= LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
int(z'1E54'), int(z'1E55'), & ! LATIN SMALL LETTER P WITH ACUTE <= LATIN CAPITAL LETTER P WITH ACUTE
int(z'1E56'), int(z'1E57'), & ! LATIN SMALL LETTER P WITH DOT ABOVE <= LATIN CAPITAL LETTER P WITH DOT ABOVE
int(z'1E58'), int(z'1E59'), & ! LATIN SMALL LETTER R WITH DOT ABOVE <= LATIN CAPITAL LETTER R WITH DOT ABOVE
int(z'1E5A'), int(z'1E5B'), & ! LATIN SMALL LETTER R WITH DOT BELOW <= LATIN CAPITAL LETTER R WITH DOT BELOW
int(z'1E5C'), int(z'1E5D'), & ! LATIN SMALL LETTER R WITH DOT BELOW AND MACRON <= LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
int(z'1E5E'), int(z'1E5F'), & ! LATIN SMALL LETTER R WITH LINE BELOW <= LATIN CAPITAL LETTER R WITH LINE BELOW
int(z'1E60'), int(z'1E61'), & ! LATIN SMALL LETTER S WITH DOT ABOVE <= LATIN CAPITAL LETTER S WITH DOT ABOVE
int(z'1E62'), int(z'1E63'), & ! LATIN SMALL LETTER S WITH DOT BELOW <= LATIN CAPITAL LETTER S WITH DOT BELOW
int(z'1E64'), int(z'1E65'), & ! LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE <= LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
int(z'1E66'), int(z'1E67'), & ! LATIN SMALL LETTER S WITH CARON AND DOT ABOVE <= LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
int(z'1E68'), int(z'1E69'), & ! LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE <= LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
int(z'1E6A'), int(z'1E6B'), & ! LATIN SMALL LETTER T WITH DOT ABOVE <= LATIN CAPITAL LETTER T WITH DOT ABOVE
int(z'1E6C'), int(z'1E6D'), & ! LATIN SMALL LETTER T WITH DOT BELOW <= LATIN CAPITAL LETTER T WITH DOT BELOW
int(z'1E6E'), int(z'1E6F'), & ! LATIN SMALL LETTER T WITH LINE BELOW <= LATIN CAPITAL LETTER T WITH LINE BELOW
int(z'1E70'), int(z'1E71'), & ! LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW <= LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
int(z'1E72'), int(z'1E73'), & ! LATIN SMALL LETTER U WITH DIAERESIS BELOW <= LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
int(z'1E74'), int(z'1E75'), & ! LATIN SMALL LETTER U WITH TILDE BELOW <= LATIN CAPITAL LETTER U WITH TILDE BELOW
int(z'1E76'), int(z'1E77'), & ! LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW <= LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
int(z'1E78'), int(z'1E79'), & ! LATIN SMALL LETTER U WITH TILDE AND ACUTE <= LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
int(z'1E7A'), int(z'1E7B'), & ! LATIN SMALL LETTER U WITH MACRON AND DIAERESIS <= LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
int(z'1E7C'), int(z'1E7D'), & ! LATIN SMALL LETTER V WITH TILDE <= LATIN CAPITAL LETTER V WITH TILDE
int(z'1E7E'), int(z'1E7F'), & ! LATIN SMALL LETTER V WITH DOT BELOW <= LATIN CAPITAL LETTER V WITH DOT BELOW
int(z'1E80'), int(z'1E81'), & ! LATIN SMALL LETTER W WITH GRAVE <= LATIN CAPITAL LETTER W WITH GRAVE
int(z'1E82'), int(z'1E83'), & ! LATIN SMALL LETTER W WITH ACUTE <= LATIN CAPITAL LETTER W WITH ACUTE
int(z'1E84'), int(z'1E85'), & ! LATIN SMALL LETTER W WITH DIAERESIS <= LATIN CAPITAL LETTER W WITH DIAERESIS
int(z'1E86'), int(z'1E87'), & ! LATIN SMALL LETTER W WITH DOT ABOVE <= LATIN CAPITAL LETTER W WITH DOT ABOVE
int(z'1E88'), int(z'1E89'), & ! LATIN SMALL LETTER W WITH DOT BELOW <= LATIN CAPITAL LETTER W WITH DOT BELOW
int(z'1E8A'), int(z'1E8B'), & ! LATIN SMALL LETTER X WITH DOT ABOVE <= LATIN CAPITAL LETTER X WITH DOT ABOVE
int(z'1E8C'), int(z'1E8D'), & ! LATIN SMALL LETTER X WITH DIAERESIS <= LATIN CAPITAL LETTER X5 WITH DIAERESIS
int(z'1E8E'), int(z'1E8F'), & ! LATIN SMALL LETTER Y WITH DOT ABOVE <= LATIN CAPITAL LETTER Y WITH DOT ABOVE
int(z'1E90'), int(z'1E91'), & ! LATIN SMALL LETTER Z WITH CIRCUMFLEX <= LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
int(z'1E92'), int(z'1E93'), & ! LATIN SMALL LETTER Z WITH DOT BELOW <= LATIN CAPITAL LETTER Z WITH DOT BELOW
int(z'1E94'), int(z'1E95'), & ! LATIN SMALL LETTER Z WITH LINE BELOW <= LATIN CAPITAL LETTER Z WITH LINE BELOW
int(z'1EA0'), int(z'1EA1'), & ! LATIN SMALL LETTER A WITH DOT BELOW <= LATIN CAPITAL LETTER A WITH DOT BELOW
int(z'1EA2'), int(z'1EA3'), & ! LATIN SMALL LETTER A WITH HOOK ABOVE <= LATIN CAPITAL LETTER A WITH HOOK ABOVE
int(z'1EA4'), int(z'1EA5'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE <= LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
int(z'1EA6'), int(z'1EA7'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE <= LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
int(z'1EA8'), int(z'1EA9'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE <= LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
int(z'1EAA'), int(z'1EAB'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE <= LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
int(z'1EAC'), int(z'1EAD'), & ! LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW <= LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
int(z'1EAE'), int(z'1EAF'), & ! LATIN SMALL LETTER A WITH BREVE AND ACUTE <= LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
int(z'1EB0'), int(z'1EB1'), & ! LATIN SMALL LETTER A WITH BREVE AND GRAVE <= LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
int(z'1EB2'), int(z'1EB3'), & ! LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE <= LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
int(z'1EB4'), int(z'1EB5'), & ! LATIN SMALL LETTER A WITH BREVE AND TILDE <= LATIN CAPITAL LETTER A WITH BREVE AND TILDE
int(z'1EB6'), int(z'1EB7'), & ! LATIN SMALL LETTER A WITH BREVE AND DOT BELOW <= LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
int(z'1EB8'), int(z'1EB9'), & ! LATIN SMALL LETTER E WITH DOT BELOW <= LATIN CAPITAL LETTER E WITH DOT BELOW
int(z'1EBA'), int(z'1EBB'), & ! LATIN SMALL LETTER E WITH HOOK ABOVE <= LATIN CAPITAL LETTER E WITH HOOK ABOVE
int(z'1EBC'), int(z'1EBD'), & ! LATIN SMALL LETTER E WITH TILDE <= LATIN CAPITAL LETTER E WITH TILDE
int(z'1EBE'), int(z'1EBF'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE <= LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
int(z'1EC0'), int(z'1EC1'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE <= LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
int(z'1EC2'), int(z'1EC3'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE <= LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
int(z'1EC4'), int(z'1EC5'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE <= LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
int(z'1EC6'), int(z'1EC7'), & ! LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW <= LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
int(z'1EC8'), int(z'1EC9'), & ! LATIN SMALL LETTER I WITH HOOK ABOVE <= LATIN CAPITAL LETTER I WITH HOOK ABOVE
int(z'1ECA'), int(z'1ECB'), & ! LATIN SMALL LETTER I WITH DOT BELOW <= LATIN CAPITAL LETTER I WITH DOT BELOW
int(z'1ECC'), int(z'1ECD'), & ! LATIN SMALL LETTER O WITH DOT BELOW <= LATIN CAPITAL LETTER O WITH DOT BELOW
int(z'1ECE'), int(z'1ECF'), & ! LATIN SMALL LETTER O WITH HOOK ABOVE <= LATIN CAPITAL LETTER O WITH HOOK ABOVE
int(z'1ED0'), int(z'1ED1'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE <= LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
int(z'1ED2'), int(z'1ED3'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE <= LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
int(z'1ED4'), int(z'1ED5'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE <= LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
int(z'1ED6'), int(z'1ED7'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE <= LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
int(z'1ED8'), int(z'1ED9'), & ! LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW <= LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
int(z'1EDA'), int(z'1EDB'), & ! LATIN SMALL LETTER O WITH HORN AND ACUTE <= LATIN CAPITAL LETTER O WITH HORN AND ACUTE
int(z'1EDC'), int(z'1EDD'), & ! LATIN SMALL LETTER O WITH HORN AND GRAVE <= LATIN CAPITAL LETTER O WITH HORN AND GRAVE
int(z'1EDE'), int(z'1EDF'), & ! LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE <= LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
int(z'1EE0'), int(z'1EE1'), & ! LATIN SMALL LETTER O WITH HORN AND TILDE <= LATIN CAPITAL LETTER O WITH HORN AND TILDE
int(z'1EE2'), int(z'1EE3'), & ! LATIN SMALL LETTER O WITH HORN AND DOT BELOW <= LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
int(z'1EE4'), int(z'1EE5'), & ! LATIN SMALL LETTER U WITH DOT BELOW <= LATIN CAPITAL LETTER U WITH DOT BELOW
int(z'1EE6'), int(z'1EE7'), & ! LATIN SMALL LETTER U WITH HOOK ABOVE <= LATIN CAPITAL LETTER U WITH HOOK ABOVE
int(z'1EE8'), int(z'1EE9'), & ! LATIN SMALL LETTER U WITH HORN AND ACUTE <= LATIN CAPITAL LETTER U WITH HORN AND ACUTE
int(z'1EEA'), int(z'1EEB'), & ! LATIN SMALL LETTER U WITH HORN AND GRAVE <= LATIN CAPITAL LETTER U WITH HORN AND GRAVE
int(z'1EEC'), int(z'1EED'), & ! LATIN SMALL LETTER U WITH HORN AND HOCK ABOVE <= LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
int(z'1EEE'), int(z'1EEF'), & ! LATIN SMALL LETTER U WITH HORN AND TILDE <= LATIN CAPITAL LETTER U WITH HORN AND TILDE
int(z'1EF0'), int(z'1EF1'), & ! LATIN SMALL LETTER U WITH HORN AND DOT BELOW <= LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
int(z'1EF2'), int(z'1EF3'), & ! LATIN SMALL LETTER Y WITH GRAVE <= LATIN CAPITAL LETTER Y WITH GRAVE
int(z'1EF4'), int(z'1EF5'), & ! LATIN SMALL LETTER Y WITH DOT BELOW <= LATIN CAPITAL LETTER Y WITH DOT BELOW
int(z'1EF6'), int(z'1EF7'), & ! LATIN SMALL LETTER Y WITH HOOK ABOVE <= LATIN CAPITAL LETTER Y WITH HOOK ABOVE
int(z'1EF8'), int(z'1EF9'), & ! LATIN SMALL LETTER Y WITH TILDE <= LATIN CAPITAL LETTER Y WITH TILDE
int(z'1F08'), int(z'1F00'), & ! GREEK SMALL LETTER ALPHA WITH PSILI <= GREEK CAPITAL LETTER ALPHA WITH PSILI
int(z'1F09'), int(z'1F01'), & ! GREEK SMALL LETTER ALPHA WITH DASIA <= GREEK CAPITAL LETTER ALPHA WITH DASIA
int(z'1F0A'), int(z'1F02'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA <= GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
int(z'1F0B'), int(z'1F03'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA <= GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
int(z'1F0C'), int(z'1F04'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA <= GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
int(z'1F0D'), int(z'1F05'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA <= GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
int(z'1F0E'), int(z'1F06'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI <= GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
int(z'1F0F'), int(z'1F07'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI <= GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
int(z'1F18'), int(z'1F10'), & ! GREEK SMALL LETTER EPSILON WITH PSILI <= GREEK CAPITAL LETTER EPSILON WITH PSILI
int(z'1F19'), int(z'1F11'), & ! GREEK SMALL LETTER EPSILON WITH DASIA <= GREEK CAPITAL LETTER EPSILON WITH DASIA
int(z'1F1A'), int(z'1F12'), & ! GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA <= GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
int(z'1F1B'), int(z'1F13'), & ! GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA <= GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
int(z'1F1C'), int(z'1F14'), & ! GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA <= GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
int(z'1F1D'), int(z'1F15'), & ! GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA <= GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
int(z'1F28'), int(z'1F20'), & ! GREEK SMALL LETTER ETA WITH PSILI <= GREEK CAPITAL LETTER ETA WITH PSILI
int(z'1F29'), int(z'1F21'), & ! GREEK SMALL LETTER ETA WITH DASIA <= GREEK CAPITAL LETTER ETA WITH DASIA
int(z'1F2A'), int(z'1F22'), & ! GREEK SMALL LETTER ETA WITH PSILI AND VARIA <= GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
int(z'1F2B'), int(z'1F23'), & ! GREEK SMALL LETTER ETA WITH DASIA AND VARIA <= GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
int(z'1F2C'), int(z'1F24'), & ! GREEK SMALL LETTER ETA WITH PSILI AND OXIA <= GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
int(z'1F2D'), int(z'1F25'), & ! GREEK SMALL LETTER ETA WITH DASIA AND OXIA <= GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
int(z'1F2E'), int(z'1F26'), & ! GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI <= GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
int(z'1F2F'), int(z'1F27'), & ! GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI <= GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
int(z'1F38'), int(z'1F30'), & ! GREEK SMALL LETTER IOTA WITH PSILI <= GREEK CAPITAL LETTER IOTA WITH PSILI
int(z'1F39'), int(z'1F31'), & ! GREEK SMALL LETTER IOTA WITH DASIA <= GREEK CAPITAL LETTER IOTA WITH DASIA
int(z'1F3A'), int(z'1F32'), & ! GREEK SMALL LETTER IOTA WITH PSILI AND VARIA <= GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
int(z'1F3B'), int(z'1F33'), & ! GREEK SMALL LETTER IOTA WITH DASIA AND VARIA <= GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
int(z'1F3C'), int(z'1F34'), & ! GREEK SMALL LETTER IOTA WITH PSILI AND OXIA <= GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
int(z'1F3D'), int(z'1F35'), & ! GREEK SMALL LETTER IOTA WITH DASIA AND OXIA <= GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
int(z'1F3E'), int(z'1F36'), & ! GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI <= GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
int(z'1F3F'), int(z'1F37'), & ! GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI <= GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
int(z'1F48'), int(z'1F40'), & ! GREEK SMALL LETTER OMICRON WITH PSILI <= GREEK CAPITAL LETTER OMICRON WITH PSILI
int(z'1F49'), int(z'1F41'), & ! GREEK SMALL LETTER OMICRON WITH DASIA <= GREEK CAPITAL LETTER OMICRON WITH DASIA
int(z'1F4A'), int(z'1F42'), & ! GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA <= GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
int(z'1F4B'), int(z'1F43'), & ! GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA <= GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
int(z'1F4C'), int(z'1F44'), & ! GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA <= GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
int(z'1F4D'), int(z'1F45'), & ! GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA <= GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
int(z'1F59'), int(z'1F51'), & ! GREEK SMALL LETTER UPSILON WITH DASIA <= GREEK CAPITAL LETTER UPSILON WITH OASIS
int(z'1F5B'), int(z'1F53'), & ! GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA <= GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
int(z'1F5D'), int(z'1F55'), & ! GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA <= GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
int(z'1F5F'), int(z'1F57'), & ! GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI <= GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
int(z'1F68'), int(z'1F60'), & ! GREEK SMALL LETTER OMEGA WITh PSILI <= GREEK CAPITAL LETTER OMEGA WITH PSILI
int(z'1F69'), int(z'1F61'), & ! GREEK SMALL LETTER OMEGA WITH DASIA <= GREEK CAPITAL LETTER OMEGA WITH DASIA
int(z'1F6A'), int(z'1F62'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA <= GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
int(z'1F6B'), int(z'1F63'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA <= GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
int(z'1F6C'), int(z'1F64'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA <= GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
int(z'1F6D'), int(z'1F65'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA <= GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
int(z'1F6E'), int(z'1F66'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI <= GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
int(z'1F6F'), int(z'1F67'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI <= GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
int(z'1F88'), int(z'1F80'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ALPHA WITh PSILI AND PROSGEGRAMMENI
int(z'1F89'), int(z'1F81'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
int(z'1F8A'), int(z'1F82'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
int(z'1F8B'), int(z'1F83'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
int(z'1F8C'), int(z'1F84'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMEN
int(z'1F8D'), int(z'1F85'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMEN
int(z'1F8E'), int(z'1F86'), & ! GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1F8F'), int(z'1F87'), & ! GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1F98'), int(z'1F90'), & ! GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
int(z'1F99'), int(z'1F91'), & ! GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
int(z'1F9A'), int(z'1F92'), & ! GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
int(z'1F9B'), int(z'1F93'), & ! GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
int(z'1F9C'), int(z'1F94'), & ! GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
int(z'1F9D'), int(z'1F95'), & ! GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
int(z'1F9E'), int(z'1F96'), & ! GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1F9F'), int(z'1F97'), & ! GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1FA8'), int(z'1FA0'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
int(z'1FA9'), int(z'1FA1'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
int(z'1FAA'), int(z'1FA2'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
int(z'1FAB'), int(z'1FA3'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
int(z'1FAC'), int(z'1FA4'), & ! GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
int(z'1FAD'), int(z'1FA5'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
int(z'1FAE'), int(z'1FA6'), & ! GREEK SMALL LETTER OMEGA WITh PSILI AND PERISPOMENI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1FAF'), int(z'1FA7'), & ! GREEK SMALL LETTER OMEGA WITH DASIA AND PEPISPOMENI AND YPOGEGRAMMENI <= GREEK CAPITAL LETTER OMECA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
int(z'1FB8'), int(z'1FB0'), & ! GREEK SMALL LETTER ALPHA WITH VRACHY <= GREEK CAPITAL LETTER ALPHA WITH VRACHY
int(z'1FB9'), int(z'1FB1'), & ! GREEK SMALL LETTER ALPHA WITH MACRON <= GREEK CAPITAL LETTER ALPHA WITH MACRON
int(z'1FD8'), int(z'1FD0'), & ! GREEK SMALL LETTER IOTA WITH VRACHY <= GREEK CAPITAL LETTER IOTA WITH VRACHY
int(z'1FD9'), int(z'1FD1'), & ! GREEK SMALL LETTER IOTA WITH MACRON <= GREEK CAPITAL LETTER IOTA WITH MACRON
int(z'1FE8'), int(z'1FE0'), & ! GREEK SMALL LETTER UPSILON WITH VRACHY <= GREEK CAPITAL LETTER UPSILON WITH VRACHY
int(z'1FE9'), int(z'1FE1'), & ! GREEK SMALL LETTER UPSILON WITH MACRON <= GREEK CAPITAL LETTER UPSILON WITH MACRON
int(z'24B6'), int(z'24D0'), & ! CIRCLED LATIN SMALL LETTER A <= CIRCLED LATIN CAPITAL LETTER A
int(z'24B7'), int(z'24D1'), & ! CIRCLED LATIN SMALL LETTER B <= CIRCLED LATIN CAPITAL LETTER B
int(z'24B8'), int(z'24D2'), & ! CIRCLED LATIN SMALL LETTER C <= CIRCLED LATIN CAPITAL LETTER C
int(z'24B9'), int(z'24D3'), & ! CIRCLED LATIN SMALL LETTER D <= CIRCLED LATIN CAPITAL LETTER D
int(z'24BA'), int(z'24D4'), & ! CIRCLED LATIN SMALL LETTER E <= CIRCLED LATIN CAPITAL LETTER E
int(z'24BB'), int(z'24D5'), & ! CIRCLED LATIN SMALL LETTER F <= CIRCLED LATIN CAPITAL LETTER F
int(z'24BC'), int(z'24D6'), & ! CIRCLED LATIN SMALL LETTER G <= CIRCLED LATIN CAPITAL LETTER G
int(z'24BD'), int(z'24D7'), & ! CIRCLED LATIN SMALL LETTER H <= CIRCLED LATIN CAPITAL LETTER H
int(z'24BE'), int(z'24D8'), & ! CIRCLED LATIN SMALL LETTER I <= CIRCLED LATIN CAPITAL LETTER I
int(z'24BF'), int(z'24D9'), & ! CIRCLED LATIN SMALL LETTER J <= CIRCLED LATIN CAPITAL LETTER J
int(z'24C0'), int(z'24DA'), & ! CIRCLED LATIN SMALL LETTER K <= CIRCLED LATIN CAPITAL LETTER K
int(z'24C1'), int(z'24DB'), & ! CIRCLED LATIN SMALL LETTER L <= CIRCLED LATIN CAPITAL LETTER L
int(z'24C2'), int(z'24DC'), & ! CIRCLED LATIN SMALL LETTER M <= CIRCLED LATIN CAPITAL LETTER M
int(z'24C3'), int(z'24DD'), & ! CIRCLED LATIN SMALL LETTER N <= CIRCLED LATIN CAPITAL LETTER N
int(z'24C4'), int(z'24DE'), & ! CIRCLED LATIN SMALL LETTER O <= CIRCLED LATIN CAPITAL LETTER O
int(z'24C5'), int(z'24DF'), & ! CIRCLED LATIN SMALL LETTER P <= CIRCLED LATIN CAPITAL LETTER P
int(z'24C6'), int(z'24E0'), & ! CIRCLED LATIN SMALL LETTER Q <= CIRCLED LATIN CAPITAL LETTER Q
int(z'24C7'), int(z'24E1'), & ! CIRCLED LATIN SMALL LETTER R <= CIRCLED LATIN CAPITAL LETTER R
int(z'24C8'), int(z'24E2'), & ! CIRCLED LATIN SMALL LETTER S <= CIRCLED LATIN CAPITAL LETTER S
int(z'24C9'), int(z'24E3'), & ! CIRCLED LATIN SMALL LETTER T <= CIRCLED LATIN CAPITAL LETTER T
int(z'24CA'), int(z'24E4'), & ! CIRCLED LATIN SMALL LETTER U <= CIRCLED LATIN CAPITAL LETTER U
int(z'24CB'), int(z'24E5'), & ! CIRCLED LATIN SMALL LETTER V <= CIRCLED LATIN CAPITAL LETTER V
int(z'24CC'), int(z'24E6'), & ! CIRCLED LATIN SMALL LETTER W <= CIRCLED LATIN CAPITAL LETTER W
int(z'24CD'), int(z'24E7'), & ! CIRCLED LATIN SMALL LETTER X <= CIRCLED LATIN CAPITAL LETTER X
int(z'24CE'), int(z'24E8'), & ! CIRCLED LATIN SMALL LETTER Y <= CIRCLED LATIN CAPITAL LETTER Y
int(z'24CF'), int(z'24E9'), & ! CIRCLED LATIN SMALL LETTER Z <= CIRCLED LATIN CAPITAL LETTER Z
int(z'FF21'), int(z'FF41'), & ! FULLWIDTH LATIN SMALL LETTER A <= FULLWIDTH LATIN CAPITAL LETTER A
int(z'FF22'), int(z'FF42'), & ! FULLWIDTH LATIN SMALL LETTER B <= FULLWIDTH LATIN CAPITAL LETTER B
int(z'FF23'), int(z'FF43'), & ! FULLWIDTH LATIN SMALL LETTER C <= FULLWIDTH LATIN CAPITAL LETTER C
int(z'FF24'), int(z'FF44'), & ! FULLWIDTH LATIN SMALL LETTER D <= FULLWIDTH LATIN CAPITAL LETTER D
int(z'FF25'), int(z'FF45'), & ! FULLWIDTH LATIN SMALL LETTER E <= FULLWIDTH LATIN CAPITAL LETTER E
int(z'FF26'), int(z'FF46'), & ! FULLWIDTH LATIN SMALL LETTER F <= FULLWIDTH LATIN CAPITAL LETTER F
int(z'FF27'), int(z'FF47'), & ! FULLWIDTH LATIN SMALL LETTER G <= FULLWIDTH LATIN CAPITAL LETTER G
int(z'FF28'), int(z'FF48'), & ! FULLWIDTH LATIN SMALL LETTER H <= FULLWIDTH LATIN CAPITAL LETTER H
int(z'FF29'), int(z'FF49'), & ! FULLWIDTH LATIN SMALL LETTER I <= FULLWIDTH LATIN CAPITAL LETTER I
int(z'FF2A'), int(z'FF4A'), & ! FULLWIDTH LATIN SMALL LETTER J <= FULLWIDTH LATIN CAPITAL LETTER J
int(z'FF2B'), int(z'FF4B'), & ! FULLWIDTH LATIN SMALL LETTER K <= FULLWIDTH LATIN CAPITAL LETTER K
int(z'FF2C'), int(z'FF4C'), & ! FULLWIDTH LATIN SMALL LETTER L <= FULLWIDTH LATIN CAPITAL LETTER L
int(z'FF2D'), int(z'FF4D'), & ! FULLWIDTH LATIN SMALL LETTER M <= FULLWIDTH LATIN CAPITAL LETTER M
int(z'FF2E'), int(z'FF4E'), & ! FULLWIDTH LATIN SMALL LETTER N <= FULLWIDTH LATIN CAPITAL LETTER N
int(z'FF2F'), int(z'FF4F'), & ! FULLWIDTH LATIN SMALL LETTER O <= FULLWIDTH LATIN CAPITAL LETTER O
int(z'FF30'), int(z'FF50'), & ! FULLWIDTH LATIN SMALL LETTER P <= FULLWIDTH LATIN CAPITAL LETTER P
int(z'FF31'), int(z'FF51'), & ! FULLWIDTH LATIN SMALL LETTER Q <= FULLWIDTH LATIN CAPITAL LETTER Q
int(z'FF32'), int(z'FF52'), & ! FULLWIDTH LATIN SMALL LETTER R <= FULLWIDTH LATIN CAPITAL LETTER R
int(z'FF33'), int(z'FF53'), & ! FULLWIDTH LATIN SMALL LETTER S <= FULLWIDTH LATIN CAPITAL LETTER S
int(z'FF34'), int(z'FF54'), & ! FULLWIDTH LATIN SMALL LETTER T <= FULLWIDTH LATIN CAPITAL LETTER T
int(z'FF35'), int(z'FF55'), & ! FULLWIDTH LATIN SMALL LETTER U <= FULLWIDTH LATIN CAPITAL LETTER U
int(z'FF36'), int(z'FF56'), & ! FULLWIDTH LATIN SMALL LETTER V <= FULLWIDTH LATIN CAPITAL LETTER V
int(z'FF37'), int(z'FF57'), & ! FULLWIDTH LATIN SMALL LETTER W <= FULLWIDTH LATIN CAPITAL LETTER W
int(z'FF38'), int(z'FF58'), & ! FULLWIDTH LATIN SMALL LETTER X <= FULLWIDTH LATIN CAPITAL LETTER X
int(z'FF39'), int(z'FF59'), & ! FULLWIDTH LATIN SMALL LETTER Y <= FULLWIDTH LATIN CAPITAL LETTER Y
int(z'FF3A'), int(z'FF5A')] & ! FULLWIDTH LATIN SMALL LETTER Z <= FULLWIDTH LATIN CAPITAL LETTER Z
,shape(up_to_low),order=[2,1])

! hits but in alpha lfortran
!integer,parameter :: hexchars(*) = iachar(['a','b','c','d','e','f', &
!                                         & '0','1','2','3','4','5','6','7','8','9', &
!                                         & 'A','B','C','D','E','F' ])
integer,parameter :: hexchars(*) = [97,98,99,100,101,102,48,49,50,51,52,53,54,55,56,57,65,66,67,68,69,70]

type unicode_codepoints
   integer :: SPACES(size(spacescodes))
   integer :: LOWER(size(low_to_up,dim=1))
   integer :: UPPER(size(up_to_low,dim=1))
   integer :: hexadecimal(size(hexchars))
   integer :: bom(1)=[int(z'FEFF')]

end type unicode_codepoints

type(unicode_codepoints),parameter,public :: unicode= unicode_codepoints( &
   upper=up_to_low(:,2), &
   lower=low_to_up(:,2), &
   hexadecimal=[hexchars], &
   bom=[int(z'FEFF')], &
   spaces=spacescodes )

type :: force_keywords ! force keywords, using @awvwgk method
end type force_keywords
! so then any argument that comes after "force_kwargs" is a compile time error
! if not done with a keyword unless someone "breaks" it by passing something
! of this type:
!    type(force_keywords), optional, intent(in) :: force_kwargs

!> Write string to connected formatted unit.
interface write(formatted);   module procedure :: write_formatted;   end interface
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   CODEPOINTS_TO_UTF8(3f) - [M_unicode:CONVERSION] convert codepoints
!   to CHARACTER
!   (LICENSE:MIT)
! 
! SYNOPSIS
! 
!    pure subroutine codepoints_to_utf8(codepoints,utf8,nerr)
! 
!     integer,allocatable,intent(in) :: codepoints(:)
!     !
!     character(len=1),intent(out)   :: utf8(:)
!     !  or
!     character(len=*),intent(out)   :: utf8
!     !
!     integer,intent(out)            :: nerr
! 
! CHARACTERISTICS
!   + UTF8 is a scalar or array CHARACTER variable
!   + CODEPOINTS is of default INTEGER kind
!   + NERR is of default INTEGER kind
! 
! DESCRIPTION
!   CODEPOINTS_TO_UTF8(3f) takes an integer array of Unicode codepoint
!   values and generates either a scalar CHARACTER variable or an array
!   of bytes (AKA. CHARACTER(LEN=1)) which are assumed to contain a stream
!   of bytes representing UTF-8-encoded data.
! 
! OPTIONS
! 
!    + CODEPOINTS :  An INTEGER array of Unicode codepoint values representing
!                    the glyphs to be encoded at UTF-8 data
! 
!    + UTF8 :  Scalar or single-character array CHARACTER variables
!              to contain a stream of bytes containing data encoded at
!              UTF-8 text.
! 
!    + NERR :  Zero if no error occurred. If not zero the stream of bytes
!              could not be completely converted to UTF-8 characters.
! 
! EXAMPLES
!   Sample program
! 
!    program demo_codepoints_to_utf8
!    use m_unicode, only : codepoints_to_utf8
!    implicit none
!    !'Noho me ka hau’oli' !(Be happy)
!    integer,parameter :: codepoints(*)=[ &
!       & 78,111,104,111,&
!       & 32,109,101, &
!       & 32,107,97, &
!       & 32,104,97,117,8217,111,108,105]
!    character(len=:),allocatable :: string
!    character(len=1),allocatable :: bytes(:)
!    character(len=*),parameter   :: solid='(*(g0))'
!    character(len=*),parameter   :: space='(*(g0,1x))'
!    character(len=*),parameter   :: z='(a,*(z0,1x))'
!    integer                      :: nerr
!    ! BASIC USAGE: SCALAR CHARACTER VARIABLE
!      write(*,space)'CODEPOINTS:', codepoints
!      write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
!      call codepoints_to_utf8(codepoints,string,nerr)
!      write(*,solid)'STRING:',string
!    !
!      write(*,space)'How long is this string in glyphs? '
!      write(*,space)size(codepoints)
!      write(*,space)'How long is this string in bytes? '
!      write(*,space)len(string)
!    !
!    ! BASIC USAGE: ARRAY OF BYTES
!      call codepoints_to_utf8(codepoints,bytes,nerr)
!      write(*,solid)'STRING:',bytes
!    !
!      write(*,space)'How long is this string in glyphs? '
!      write(*,space)size(codepoints)
!      write(*,space)'How long is this string in bytes? '
!      write(*,space)size(bytes)
!    !
!    end program demo_codepoints_to_utf8
! 
!  Results:
! 
!     > CODEPOINTS: 78 111 104 111 32 109 101 32 107 97 32 104 97 117 ...
!     > 8217 111 108 105
!     > 48 4E 6F 68 6F 20 6D 65 20 6B 61 20 68 61 75 2019 6F 6C 69
!     > STRING:Noho me ka hau’oli
!     > How long is this string in glyphs?
!     > 18
!     > How long is this string in bytes?
!     > 20
!     > STRING:Noho me ka hau’oli
!     > How long is this string in glyphs?
!     > 18
!     > How long is this string in bytes?
!     > 20
! 
! SEE ALSO
!   functions that perform operations on character strings:
! 
!   + elemental: adjustl(3), adjustr(3), index(3), scan(3), verify(3)
!   + non-elemental: len_trim(3), repeat(3), trim(3),
!                    codepoints_to_utf8(3), utf8_to_codepoints(3)
! 
! AUTHOR
!   + John S. Urban
!   + Francois Jacq - enhancements and Latin support from Francois Jacq, 2025-08
! 
! LICENSE
!     MIT
!===================================================================================================================================
pure subroutine codepoints_to_utf8_chars(codepoints,utf8,nerr)
intrinsic char
integer,intent(in)                :: codepoints(:)
character,allocatable,intent(out) :: utf8(:)
integer,intent(out)               :: nerr
integer                           :: i, n_unicode, n_utf8, cp
character, allocatable            :: temp_utf8(:)

   n_unicode = size(codepoints)

   if(allocated(temp_utf8))deallocate(temp_utf8)
   allocate(temp_utf8(4*n_unicode))
   n_utf8 = 0

   nerr=0
   do i = 1, n_unicode
      cp = codepoints(i)

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

   if(allocated(utf8))deallocate(utf8)
   allocate(utf8(n_utf8))
   utf8 = temp_utf8(1:n_utf8)

end subroutine codepoints_to_utf8_chars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   UTF8_TO_CODEPOINTS(3f) - [M_unicode:CONVERSION] Convert UTF-8-encoded
!   data to Unicode codepoints
!   (LICENSE:MIT)
! 
! SYNOPSIS
! 
!    pure subroutine utf8_to_codepoints(utf8,codepoints,nerr)
! 
!     character(len=1),intent(in)     :: utf8(:)
!     !  or
!     character(len=*),intent(in)     :: utf8
!     !
!     integer,allocatable,intent(out) :: codepoints(:)
!     integer,intent(out)             :: nerr
! 
! CHARACTERISTICS
!   + UTF8 is a scalar CHARACTER variable or array of single-byte
!     CHARACTER values
!   + the returned values in CODEPOINTS are of default INTEGER kind
!   + the error flag NERR is default integer kind
! 
! DESCRIPTION
!   UTF8_TO_CODEPOINTS(3f) takes either a scalar CHARACTER variable or
!   an array of CHARACTER(LEN=1) bytes which are treated as a stream of
!   bytes representing UTF-8-encoded data and converted to an INTEGER
!   array containing Unicode codepoint values for each glyph.
! 
!   In fact, this routine is also able to decode an ISOLATIN string
! 
! OPTIONS
!   + UTF8 :  Scalar CHARACTER string or single-character array of CHARACTER
!             variables assumed to represent a stream of bytes containing
!             data encoded at UTF-8 text.
! 
!   + CODEPOINTS :  An INTEGER array of Unicode codepoint values
!                   representing the glyphs found in STRING
!   + NERR :  Zero if no error occurred. If not zero the stream of bytes
!             could not be completely converted to UTF-8 characters.
! 
! EXAMPLES
!   Sample program
! 
!    program demo_utf8_to_codepoints
!    use m_unicode, only : utf8_to_codepoints
!    implicit none
!    character(len=*),parameter   :: string ='Noho me ka hau’oli' !(Be happy)
!    character(len=1),allocatable :: bytes(:)
!    character(len=*),parameter   :: solid='(*(g0))'
!    character(len=*),parameter   :: space='(*(g0,1x))'
!    character(len=*),parameter   :: z='(a,*(z0,1x))'
!    integer,allocatable          :: codepoints(:)
!    integer                      :: nerr
!    integer                      :: i
!    ! BASIC USAGE: SCALAR CHARACTER VARIABLE
!      write(*,solid)'STRING:',string
!      call utf8_to_codepoints(string,codepoints,nerr)
!      write(*,space)'CODEPOINTS:', codepoints
!      write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
!    !
!      write(*,space)'How long is this string in glyphs? '
!      write(*,space)size(codepoints)
!      write(*,space)'How long is this string in bytes? '
!      write(*,space)len(string)
!    !
!    ! BASIC USAGE: ARRAY OF BYTES
!      bytes=[(string(i:i),i=1,len(string))]
!      write(*,solid)'STRING:',bytes
!      call utf8_to_codepoints(bytes,codepoints,nerr)
!      write(*,space)'CODEPOINTS:', codepoints
!      write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
!    !
!      write(*,space)'How long is this string in glyphs? '
!      write(*,space)size(codepoints)
!      write(*,space)'How long is this string in bytes? '
!      write(*,space)size(bytes)
!    !
!    end program demo_utf8_to_codepoints
! 
!  Results:
! 
!     > STRING:Noho me ka hau’oli
!     > CODEPOINTS: 78 111 104 111 32 109 101 32 107 97 32 104 97 117 ...
!     > 8217 111 108 105
!     > 48 4E 6F 68 6F 20 6D 65 20 6B 61 20 68 61 75 2019 6F 6C 69
!     > How long is this string in glyphs?
!     > 18
!     > How long is this string in bytes?
!     > 20
!     > STRING:Noho me ka hau’oli
!     > CODEPOINTS: 78 111 104 111 32 109 101 32 107 97 32 104 97 117 ...
!     > 8217 111 108 105
!     > 48 4E 6F 68 6F 20 6D 65 20 6B 61 20 68 61 75 2019 6F 6C 69
!     > How long is this string in glyphs?
!     > 18
!     > How long is this string in bytes?
!     > 20
! 
! SEE ALSO
!   functions that perform operations on character strings:
! 
!   + elemental: adjustl(3), adjustr(3), index(3), scan(3), verify(3)
!   + non-elemental: len_trim(3), repeat(3), trim(3), codepoints_to_utf8(3)
! 
! AUTHOR
!   + John S. Urban
!   + Francois Jacq - enhancements and Latin support from Francois Jacq, 2025-08
! 
! LICENSE
!     MIT
!===================================================================================================================================
pure subroutine utf8_to_codepoints_chars(utf8,codepoints,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character(len=1),intent(in)     :: utf8(:)
integer,allocatable,intent(out) :: codepoints(:)
integer,intent(out)             :: nerr
integer                         :: n_out
integer                         :: i, len8, b1, b2, b3, b4
integer                         :: cp, nbytes,nerr0
integer,allocatable             :: temp(:)

   nerr = 0

   len8 = size(utf8)
   i = 1
   n_out = 0
   if(allocated(temp))deallocate(temp)
   allocate(temp(len8)) ! big enough to store all Unicode codepoint values

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

   if(allocated(codepoints))deallocate(codepoints)
   allocate(codepoints(n_out))
   codepoints = temp(1:n_out)

end subroutine utf8_to_codepoints_chars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function a2s(array) result (string)

! ident_1="@(#) M_unicode a2s(3fp) function to copy char array to string"

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i

   forall( i = 1:size(array)) string(i:i) = array(i)
!  string=transfer(array,string)

end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string) result (array)

! ident_2="@(#) M_unicode s2a(3fp) function to copy string(1 Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i

   forall(i=1:len(string)) array(i) = string(i:i)
!  array=transfer(string,array)

end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function binary_search(arr, target) result(index)
implicit none
integer,intent(in) :: arr(:)         ! The sorted array to search
integer,intent(in) :: target         ! The value to find
integer            :: index          ! The returned index of the target, or -1 if not found
integer            :: low, high, mid

  low = 1
  high = size(arr)
  index = -1 ! Initialize to -1 (not found)

  do while (low <= high)
    mid = low + (high - low) / 2 ! Calculate middle index to prevent overflow
    if (arr(mid) == target) then
      index = mid
      exit
    elseif (arr(mid) < target) then
      low = mid + 1
    else
      high = mid - 1
    endif
  enddo

end function binary_search
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine codepoints_to_utf8_str(codepoints,utf8,nerr)

integer,intent(in)                       :: codepoints(:)
character(len=:),allocatable,intent(out) :: utf8
integer,intent(out)                      :: nerr
character, allocatable                   :: utf8_chars(:)
   nerr=0
   call codepoints_to_utf8_chars(codepoints,utf8_chars,nerr)
   utf8=a2s(utf8_chars)
end subroutine codepoints_to_utf8_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_codepoints_str(utf8,codepoints,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character(len=*),intent(in)     :: utf8
integer,allocatable,intent(out) :: codepoints(:)
integer,intent(out)             :: nerr
character,allocatable           :: temp(:)
   temp=s2a(utf8)
   call utf8_to_codepoints_chars(temp,codepoints,nerr)
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

! Constructor for new string instances from a vector character value.
module function new_strs(strings) result(new)
character(len=*), intent(in)           :: strings(:)
type(unicode_type)                     :: new(size(strings))
integer                                :: nerr
integer                                :: i
   do i=1,size(strings)
      call utf8_to_codepoints_str(strings(i),new(i)%codes,nerr)
   enddo
end function new_strs

! Constructor for new string instance from a vector integer value.
module function new_codes(codes) result(new)
integer,intent(in) :: codes(:)
type(unicode_type) :: new
   new%codes=codes
end function new_codes
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

!> Assign a string to a character sequence
subroutine assign_ints_str(lhs, rhs)
integer,allocatable,intent(inout) :: lhs(:)
type(unicode_type),intent(in)     :: rhs
   lhs=rhs%codes
end subroutine assign_ints_str

!> Assign a string to a character sequence
subroutine assign_char_str(lhs, rhs)
character(len=:),allocatable,intent(inout) :: lhs
type(unicode_type),intent(in)              :: rhs
integer                                    :: nerr
   call codepoints_to_utf8_str(rhs%codes,lhs,nerr)
end subroutine assign_char_str

!> Assign a character sequence to a string.
elemental subroutine assign_str_char(lhs, rhs)
type(unicode_type), intent(inout) :: lhs
character(len=*), intent(in)      :: rhs
integer                           :: nerr
   call utf8_to_codepoints_str(rhs,lhs%codes,nerr)
end subroutine assign_str_char

subroutine assign_strs_char(lhs, rhs)
type(unicode_type),intent(inout) :: lhs
character(len=*),intent(in)      :: rhs(:)
integer                          :: nerr
integer                          :: i
integer,allocatable              :: temp(:)
   if(allocated(lhs%codes))deallocate(lhs%codes)
   allocate(lhs%codes(0))
   do i=1,size(rhs)
      call utf8_to_codepoints_str(rhs(i),temp,nerr)
      lhs%codes=[lhs%codes,temp]
   enddo
end subroutine assign_strs_char

! Assign a sequence of codepoints to a string.
subroutine assign_str_codes(lhs, rhs)
type(unicode_type), intent(inout) :: lhs
integer, intent(in)               :: rhs(:)
   lhs%codes=rhs
end subroutine assign_str_codes

elemental subroutine assign_str_code(lhs, rhs)
type(unicode_type), intent(inout) :: lhs
integer, intent(in)               :: rhs
   lhs%codes=[rhs]
end subroutine assign_str_code
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   LEN(3f) - [M_unicode:WHITESPACE] Length of a string
!     (LICENSE:MIT)
! 
! SYNOPSIS
!   result = len(string)
! 
!    elemental integer function len(string)
! 
!     type(unicode_type),intent(in) :: string
! 
! CHARACTERISTICS
!   + STRING is a scalar or array string variable
!   + the returned value is of default INTEGER kind
! 
! DESCRIPTION
!   LEN(3) returns the length of a type(unicode_type) string.
! 
!   Note that unlike the intrinsic of the same name STRING needs to be
!   defined; as the length of each element is not defined until allocated;
!   and the KIND parameter is not available for specifying the kind of the
!   integer returned.
! 
! OPTIONS
!   + STRING : A scalar or array string to return the length(s) of in glyph
!     counts. If it is an unallocated allocatable variable or a pointer that
!     is not associated, its length type parameter shall not be deferred.
! 
! RESULT
!   The result has a value equal to the number of glyphs in STRING if
!   it is scalar or the elements of STRING if it is an array.
! 
! EXAMPLES
!   Sample program
! 
!    program demo_len
!    use M_unicode, only : assignment(=), ut=>unicode_type, len
!    use M_unicode, only : write(formatted)
!    implicit none
!    type(ut)             :: string
!    type(ut),allocatable :: many_strings(:)
!    integer                        :: ii
!    ! BASIC USAGE
!      string='Noho me ka hau’oli' ! (Be happy.)
!      ii=len(string)
!      write(*,'(DT,*(g0))')string, ' LEN=', ii
!    !
!      string=' How long is this allocatable string? '
!      write(*,'(DT,*(g0))')string, ' LEN=', len(string)
!    !
!    ! STRINGS IN AN ARRAY MAY BE OF DIFFERENT LENGTHS
!      many_strings = [ ut('Tom'), ut('Dick'), ut('Harry') ]
!      write(*,'(*(g0,1x))')'length of elements of array=',len(many_strings)
!    !
!      write(*,'(*(g0))')'length from type parameter inquiry=',string%len()
!    !
!    ! LOOK AT HOW A PASSED STRING CAN BE USED ...
!      call passed(ut(' how long? '))
!    !
!    contains
!    !
!    subroutine passed(str)
!    type(ut),intent(in) :: str
!       ! you can query the length of the passed variable
!       ! when an interface is present
!       write(*,'(*(g0))')'length of passed value is ', len(str)
!    end subroutine passed
!    !
!    end program demo_len
! 
!   Results:
! 
!    > Noho me ka hau’oli LEN=18
!    >  How long is this allocatable string?  LEN=38
!    > length of elements of array= 3 4 5
!    > length from type parameter inquiry=38
!    > length of passed value is 11
! 
! SEE ALSO
!   functions that perform operations on character strings:
! 
!   + elemental: adjustl(3), adjustr(3), index(3), scan(3), verify(3)
!   + non-elemental: len_trim(3), len(3), repeat(3), trim(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
! Returns the length of the character sequence represented by the string.
elemental function len_str(string) result(length)
type(unicode_type), intent(in) :: string
integer                        :: length

   if (allocated(string%codes)) then
      length = size(string%codes)
   else
      length = 0
   endif

end function len_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    CHARACTER(3f) - [M_unicode:WHITESPACE] convert type(unicode_type)
!    string  to a CHARACTER variable
!    (LICENSE:MIT)
! 
! SYNOPSIS
!     result = character(STRING,start,end,inc)
!      or
!     result = STRING%character(start,end,inc)
! 
!      elemental function character(string,start,end,inc)
! 
!       type(unicode_type),intent(in) :: string
!       integer,intent(in)            :: start
!       integer,intent(in)            :: end
!       integer,intent(in)            :: inc
! 
! CHARACTERISTICS
!   + STRING is a scalar or array string variable
!   + the returned value is a CHARACTER scalar or array
! 
! DESCRIPTION
!   CHARACTER(3f) returns a CHARACTER variable given a string variable
!   of type type(unicode_type).
! 
! OPTIONS
!   + STRING : A scalar or array string to convert to intrinsic CHARACTER
!              type.
! RESULT
!   The result converts each string to bytes stored in CHARACTER variables.
!   All elements will be padded to the same length of the longest element;
!   as all elements of a CHARACTER array are required to be of the same length.
! 
!   Commonly used to pass data to procedures requiring CHARACTER variables
!   or for printing when the DT format is not used..
! 
! EXAMPLES
!   Sample program
! 
!    program demo_character
!    use M_unicode, only : ut=>unicode_type, ch=>character, trim, len, pad
!    use M_unicode, only : write(formatted), assignment(=)
!    type(ut)             :: ustr
!    type(ut),allocatable :: array(:)
!    integer              :: i
!    character(len=*),parameter :: all='(*(g0))'
! 
!       ustr=[949, 8021, 961, 951, 954, 945, 33] ! eureka in codepoints
!       ! when doing I/O using DT might be the most intuitive
!       ! but sometimes converting to intrinsic character variables
!       ! is preferred
!       write (*,all)  ch(ustr)      ! convert to CHARACTER variable
!       write (*,all)  ustr%character()      ! convert to CHARACTER variable
!       ! you can select a range of glyphs
!       write (*,all)  ustr%character(3,4) ! similar to LINE(3:4) for
!                                          ! CHARACTER variables
!       ! and even reverse a string
!       write (*,all)  ustr%character(len(ustr),1,-1) ! reverse string
!       ! note that OOP syntax provides a few other options
!       write (*,all)  ustr%byte() ! convert to CHARACTER(LEN=1) type
! 
!       ! arrays
!       !
!       ! using this syntax make sure to make the LEN value large enough
!       ! that glyphs can take up to four bytes
!       array= ut([ character(len=60) :: &
!       'Confucius never claimed to be a prophet, '       ,&
!       'but I think he foresaw AI! He said '             ,&
!       ''                                                ,&
!       ' "学而不思则罔，思而不学则殆"'                   ,&
!       'or'                                              ,&
!       ' (xué ér bù sī zé wǎng, sī ér bù xué zé dài),'   ,&
!       'which is also'                                   ,&
!       ' "To learn without thinking is to be lost, '     ,&
!       ' to think without learning is to be in danger".'])
!       !
!       write(*,'(*(:,"[",g0,"]",/))')ch(array)
!       ! all elements will be the same length in bytes but not necessarily
!       !in glyphs
!       write(*,'(a,*(i0,1x))')'all elements the same length in BYTES:', &
!               & len(ch(array))
!       write(*,'(a,*(i0,1x))')'lengths (in glyphs):',len(array)
!       array=trim(array)
!       write(*,'(a,*(i0,1x))')'lengths after trimming (in glyphs):', &
!               & len(array)
!       write(*,'(:*(:,"[",g0,"]",/))')ch(array)
!       write(*,*)
!       !
!       ! using this syntax the elements will be of different lengths
!       array= [ &
!       ut('Confucius never claimed to be a prophet,')      ,&
!       ut('but I think he foresaw AI! He said')            ,&
!       ut('')                                              ,&
!       ut(' "学而不思则罔，思而不学则殆"')                    ,&
!       ut('or')                                            ,&
!       ut(' (xué ér bù sī zé wǎng, sī ér bù xué zé dài),') ,&
!       ut('which is also')                                 ,&
!       ut(' "To learn without thinking is to be lost,')    ,&
!       ut(' to think without learning is to be in danger".')]
!       ! but using the CHARACTER function will still make them the same
!       ! length in bytes so you might want to print them individually
!       ! for certain effects, subject to font properties such as varying
!       ! glyph widths.
!       write(*,'(*("[",g0,"]",/))')(ch(array(i)),i=1,size(array))
!       write(*,'(*("[",g0,"]",/))')(ch(pad(array(i),60)),i=1,size(array))
!       !
!    end program demo_character
! 
!  Results:
! 
!     > εὕρηκα!
!     > εὕρηκα!
!     > ρη
!     > !ακηρὕε
!     > εὕρηκα!
!     > [Confucius never claimed to be a prophet,                    ]
!     > [but I think he foresaw AI! He said                          ]
!     > [                                                            ]
!     > [ "学而不思则罔，思而不学则殆"                  ]
!     > [or                                                          ]
!     > [ (xué ér bù sī zé wǎng, sī ér bù xué zé dài),   ]
!     > [which is also                                               ]
!     > [ "To learn without thinking is to be lost,                  ]
!     > [ to think without learning is to be in danger".             ]
!     >
!     > all elements the same length in BYTES:60
!     > lengths (in glyphs):60 60 60 34 60 48 60 60 60
!     > lengths after trimming (in glyphs):40 34 0 16 2 45 13 42 47
!     > [Confucius never claimed to be a prophet,                 ]
!     > [but I think he foresaw AI! He said                       ]
!     > [                                                         ]
!     > [ "学而不思则罔，思而不学则殆"               ]
!     > [or                                                       ]
!     > [ (xué ér bù sī zé wǎng, sī ér bù xué zé dài),]
!     > [which is also                                            ]
!     > [ "To learn without thinking is to be lost,               ]
!     > [ to think without learning is to be in danger".          ]
!     >
!     >
!     > [Confucius never claimed to be a prophet,]
!     > [but I think he foresaw AI! He said]
!     > []
!     > [ "学而不思则罔，思而不学则殆"]
!     > [or]
!     > [ (xué ér bù sī zé wǎng, sī ér bù xué zé dài),]
!     > [which is also]
!     > [ "To learn without thinking is to be lost,]
!     > [ to think without learning is to be in danger".]
!     > [
!     > [Confucius never claimed to be a prophet,                    ]
!     > [but I think he foresaw AI! He said                          ]
!     > [                                                            ]
!     > ["学而不思则罔，思而不学则殆"                                      ]
!     > [or                                                          ]
!     > [(xué ér bù sī zé wǎng, sī ér bù xué zé dài),                ]
!     > [which is also                                               ]
!     > ["To learn without thinking is to be lost,                   ]
!     > [to think without learning is to be in danger".              ]
!     > [
! 
! SEE ALSO
!   functions that perform operations on character strings:
! 
!   + elemental: adjustl(3), adjustr(3), index(3), scan(3), verify(3)
!   + non-elemental: len_trim(3), len(3), repeat(3), trim(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
! Return the character sequence represented by the string.
pure function str_to_char(string) result(aline)
type(unicode_type), intent(in) :: string
character(len=:),allocatable   :: aline
integer                        :: nerr

   call codepoints_to_utf8_str(string%codes,aline,nerr)

end function str_to_char

pure function strs_to_chars(string) result(lines)
type(unicode_type), intent(in) :: string(:)
character(len=:),allocatable   :: lines(:)
character(len=:),allocatable   :: aline
integer                        :: i
integer                        :: mx
integer                        :: nerr

   mx=0
   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes,aline,nerr)
      mx=max(mx,len(aline))
   enddo

   if(allocated(lines))deallocate(lines)
   allocate(character(len=mx) :: lines(size(string)) )

   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes,aline,nerr)
      lines(i)(:)=aline
   enddo

end function strs_to_chars

pure function str_to_char_pos(string, pos ) result(aline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: pos
character(len=:),allocatable   :: aline
integer                        :: nerr

   call codepoints_to_utf8_str(string%codes(pos:pos),aline,nerr)

end function str_to_char_pos

pure function strs_to_chars_pos(string, pos ) result(aline)
type(unicode_type), intent(in) :: string(:)
integer, intent(in)            :: pos
character(len=1),allocatable   :: aline(:)
character(len=:),allocatable   :: line
integer                        :: nerr
integer                        :: i

   if(allocated(aline))deallocate(aline)
   allocate(character(len=1) :: aline(size(string)) )

   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes(pos:pos),line,nerr)
      aline(i)=line
   enddo

end function strs_to_chars_pos

pure function str_to_char_range(string, first, last) result(aline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: first
integer, intent(in)            :: last
character(len=:),allocatable   :: aline
integer                        :: nerr
integer                        :: last_local

   last_local=last
   if(last_local.le.0)last_local=len(string)
   call codepoints_to_utf8_str(string%codes(first:last_local),aline,nerr)

end function str_to_char_range

pure function strs_to_chars_range(string, first, last) result(lines)
type(unicode_type), intent(in) :: string(:)
integer, intent(in)            :: first
integer, intent(in)            :: last
character(len=:),allocatable   :: lines(:)
character(len=:),allocatable   :: aline
integer                        :: i
integer                        :: mx
integer                        :: last_local
integer                        :: nerr

   mx=0

   do i=1,size(string)
      last_local=last
      if(last_local.le.0)last_local=len(string(i))
      call codepoints_to_utf8_str(string(i)%codes(first:last_local),aline,nerr)
      mx=max(mx,len(aline))
   enddo

   if(allocated(lines))deallocate(lines)
   allocate(character(len=mx) :: lines(size(string)) )

   do i=1,size(string)
      call codepoints_to_utf8_str(string(i)%codes(first:last_local),aline,nerr)
      lines(i)(:)=aline
   enddo

end function strs_to_chars_range

pure function str_to_char_range_step(string, first, last, step) result(aline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: first
integer, intent(in)            :: last
integer, intent(in)            :: step
character(len=:),allocatable   :: aline
integer                        :: nerr
integer                        :: last_local

   last_local=last
   if(last_local.le.0)last_local=len(string)
   call codepoints_to_utf8_str(string%codes(first:last_local:step),aline,nerr)

end function str_to_char_range_step

pure function strs_to_chars_range_step(string, first, last, step) result(lines)
type(unicode_type), intent(in) :: string(:)
integer, intent(in)            :: first
integer, intent(in)            :: last
integer, intent(in)            :: step
character(len=:),allocatable   :: lines(:)
character(len=:),allocatable   :: aline
integer                        :: i
integer                        :: mx
integer                        :: nerr
integer                        :: last_local

   mx=0
   do i=1,size(string)
      last_local=last
      if(last_local.le.0)last_local=len(string(i))
      call codepoints_to_utf8_str(string(i)%codes(first:last_local:step),aline,nerr)
      mx=max(mx,len(aline))
   enddo

   if(allocated(lines))deallocate(lines)
   allocate(character(len=mx) :: lines(size(string)) )

   do i=1,size(string)
      last_local=last
      if(last_local.le.0)last_local=len(string(i))
      call codepoints_to_utf8_str(string(i)%codes(first:last_local:step),aline,nerr)
      lines(i)(:)=aline
   enddo

end function strs_to_chars_range_step
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   REPEAT(3) - [M_unicode:CHARACTER] Repeated string concatenation
! 
! SYNOPSIS
!   result = repeat(string, ncopies)
! 
!    type(unicode_type) function repeat(string, ncopies)
! 
!     type(unicode_type),intent(in)   :: string
!     integer(kind=**),intent(in)   :: ncopies
! 
! CHARACTERISTICS
! 
!   + STRING is a scalar string of type(unicode_type).
!   + NCOPIES is a scalar integer.
!   + the result is a new scalar string of type type(unicode_type)
! 
! DESCRIPTION
!   REPEAT(3) concatenates copies of a string.
! 
! OPTIONS
!   +  STRING : The input string to repeat
!   +  NCOPIES : Number of copies to make of STRING, greater than or equal to
!      zero (0).
! 
! RESULT
!   A new string built up from NCOPIES copies of STRING.
! 
! EXAMPLES
!   Sample program:
! 
!     program demo_repeat
!     use M_unicode, only : ut=>unicode_type,repeat,escape,write(formatted)
!     implicit none
!        write(*,'(DT)') repeat(escape("\u2025*"), 35)
!        write(*,'(DT)') repeat(ut("_"), 70)          ! line break
!        write(*,'(DT)') repeat(ut("1234567890"), 7)  ! number line
!        write(*,'(DT)') repeat(ut("         |"), 7)  !
!     end program demo_repeat
! 
! STANDARD
!   Fortran 95
! 
! SEE ALSO
!   Functions that perform operations on character strings:
! 
!   + ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)
!   + NON-ELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)
! 
!   Fortran descriptions (license: MIT) @urbanjost
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
! NAME
!   LEN_TRIM(3f) - [M_unicode:WHITESPACE] string length without trailing blank
!   characters
!   (LICENSE:MIT)
! 
! SYNOPSIS
!   result = len_trim(string)
! 
!          elemental integer(kind=kind) function len_trim(string)
! 
!           character(len=*),intent(in) :: string
! 
! CHARACTERISTICS
!   + string is of type type(unicode_type)
!   + the return value is of type default integer.
! 
! DESCRIPTION
!   len_trim(3) returns the length of a string, ignoring any trailing
!   blanks.
! 
! OPTIONS
!   + string : the input string whose length is to be measured.
! 
! RESULT
!   the result equals the number of glyphs remaining after any trailing
!   blanks in string are removed.
! 
!   if the input argument is of zero length or all blanks the result is zero.
! 
! EXAMPLES
!   sample program
! 
!    program demo_len_trim
!    use M_unicode, only : ut=>unicode_type, assignment(=)
!    use M_unicode, only : len,len_trim
!    use M_unicode, only : write(formatted)
!    implicit none
!    type(ut) :: string
!    integer  :: i
!    ! basic usage
!       string=" how long is this string?     "
!       print '(DT)',  string
!       print *, 'untrimmed length=',len(string)
!       print *, 'trimmed length=',len_trim(string)
!       !
!       ! print string, then print substring of string
!       string='xxxxx   '
!       write(*,'(*(DT))')string,string,string
!       i=len_trim(string)
!       print '(*(DT))',string%sub(1,i),string%sub(1,i),string%sub(1,i)
!       !
!       ! elemental example
!       ele:block
!       ! an array of strings may be used
!       type(ut),allocatable :: tablet(:)
!       tablet=[ &
!       & ut(' how long is this string?     '),&
!       & ut('and this one?')]
!          write(*,*)'untrimmed length=  ',len(tablet)
!          write(*,*)'trimmed length=    ',len_trim(tablet)
!          write(*,*)'sum trimmed length=',sum(len_trim(tablet))
!       endblock ele
!       !
!    end program demo_len_trim
! 
!   results:
! 
!    >  how long is this string?
!    >  untrimmed length=          30
!    >  trimmed length=          25
!    > xxxxx   xxxxx   xxxxx
!    > xxxxxxxxxxxxxxx
!    >  untrimmed length=            30          13
!    >  trimmed length=              25          13
!    >  sum trimmed length=          38
! 
! SEE ALSO
!   functions that perform operations on character strings, return lengths of
!   arguments, and search for certain arguments:
! 
!   + elemental: adjustl(3), adjustr(3), index(3), scan(3), verify(3)
! 
!   + nonelemental: repeat(3), len(3), trim(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
! Returns length of character sequence without trailing spaces represented by the string.
!
elemental function len_trim_str(string) result(length)
type(unicode_type), intent(in) :: string
integer                        :: length

   if(allocated(string%codes))then
      do length=size(string%codes),1,-1
         if(any(string%codes(length).eq.unicode%SPACES))cycle
         exit
      enddo
   else
      length=0
   endif

end function len_trim_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   ICHAR(3f) - [M_unicode:CONVERSION] character-to-integer code conversion
!   function
!   (LICENSE:MIT)
! 
! SYNOPSIS
!   result = ichar(c)
! 
!     elemental integer function ichar(c,kind)
! 
!      type(unicode_type),intent(in) :: c
! 
! CHARACTERISTICS
!   •  c is a scalar character
! 
!   •  the return value is of default integer kind.
! 
! DESCRIPTION
!   ichar(3) returns the code for the character in the system's native
!   character set. the correspondence between characters and their codes is
!   not necessarily the same across different Fortran implementations. For
!   example, a platform using EBCDIC would return different values than
!   an ASCII platform.
! 
!   See IACHAR(3) for specifically working with the ASCII character set.
! 
! OPTIONS
!   +  C : The input character to determine the decimal code of.
! 
! RESULT
!    The codepoint in the Unicode character set for the character being
!    queried is returned.
! 
!    The result is the position of C in the Unicode collating sequence,
!    which is generally not the dictionary order in a particular language.
! 
!    It is nonnegative and less than n, where n is the number of characters
!    in the collating sequence.
! 
!    For any characters C and D capable of representation in the processor,
!    C <= D is true if and only if ICHAR(C) <= ICHAR(D) is true and C ==
!    D is true if and only if ICHAR(C) == ICHAR(D) is true.
! 
! EXAMPLES
!   sample program:
! 
!    program demo_ichar
!    use M_unicode, only : assignment(=),ch=>character
!    use M_unicode, only : ut=>unicode_type, write(formatted)
!    use M_unicode, only : ichar, escape, len
!    implicit none
!    type(ut)             :: string
!    type(ut),allocatable :: lets(:)
!    integer,allocatable  :: ilets(:)
!    integer              :: i
!       !
!       ! create a string containing multibyte characters
!       string=[949, 8021, 961, 951, 954, 945, 33] ! eureka
!       write(*,'(*(DT,1x,"(AKA. eureka!)"))')string
!       !
!       ! call ichar(3) on each glyph of the string to convert
!       ! the string to an array of integer codepoints
!       ilets=[(ichar(string%sub(i,i)),i=1,len(string))]
!       write(*,'(*(z0,1x))')ilets
!       !
!       ! note that the %codepoint method is commonly used to
!       ! convert a string to an integer array of codepoints
!       write(*,'(*(z0,1x))')string%codepoint()
! 
!       ! elemental
!       write(*,'("WRITING ISSUES:")')
!       !
!       ! define an array LETS with escape codes with one glyph per element
!       lets=[ut('\U03B5'),ut('\U1F55'),ut('\U03C1'),ut('\U03B7'), &
!           & ut('\U03BA'),ut('\U03B1'),ut('\U0021')]
!       lets=escape(lets) ! convert escape codes to glyphs
!       !
!       ! look at issues with converting to CHARACTER for simple printing
!       !
!       write(*,'("each element is a single glyph ",*(g0,1x))')len(lets)
!       !
!       ! notice if you convert to an array of intrinsic CHARACTER type the
!       ! strings are all the same length in bytes; but unicode characters
!       ! can take various numbers of bytes
!       write(*,'(*(g0,":"))')'CHARACTER array elements have same length',&
!          & len(ch(lets))
!       ! this will not appear correctly because all elements are padded to
!       ! the same length in bytes
!       write(*,'(*(a,":"))')ch(lets)
!       ! one element at a time will retain the size of each element
!       write(*,'(*(a,":"))')(ch(lets(i:i)),i=1,size(lets))
!       !
!       ! the FIRST LETTER of each element is converted to a codepoint so
!       ! for the special case where each string element is a single glyph
!       ! an elemental approach works
!       write(*,'("ELEMENTAL:",*(z0,1x))')ichar(lets)
! 
!       ! OOPS
!       write(*,'("OOPS:",*(z0,1x))')lets%ichar()
!    end program demo_ichar
! 
!   results:
! 
!    > Project is up to date
!    > εὕρηκα! (AKA. eureka!)
!    > 3B5 1F55 3C1 3B7 3BA 3B1 21
!    > 3B5 1F55 3C1 3B7 3BA 3B1 21
!    > WRITING ISSUES:
!    > each element is a single glyph 1 1 1 1 1 1 1
!    > CHARACTER array elements have same length:3:
!    > ε :ὕ:ρ :η :κ :α :!  :
!    > ε:ὕ:ρ:η:κ:α:!:
!    > ELEMENTAL:3B5 1F55 3C1 3B7 3BA 3B1 21
!    > OOPS:3B5 1F55 3C1 3B7 3BA 3B1 21
! 
! SEE ALSO
!   achar(3), char(3), iachar(3)
! 
!   functions that perform operations on character strings, return
!   lengths of arguments, and search for certain arguments:
! 
!   +  elemental: adjustl(3), adjustr(3), index(3),
!      scan(3), verify(3)
! 
!   +  nonelemental: len_trim(3), len(3), repeat(3), trim(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
!
! Return code value of first character of string like intrinsic ichar()
!
elemental function ichar_str(string) result(code)
type(unicode_type), intent(in) :: string
integer                        :: code

   if(size(string%codes) == 0)then
      code=0
   else
      code=string%codes(1)
   endif

end function ichar_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   TRIM(3f) - [M_unicode:WHITESPACE] remove trailing blank characters from
!              a string
!              (LICENSE:MIT)
! 
! SYNOPSIS
!   result = trim(string)
! 
!    type(unicode_type) function trim(string)
! 
!     type(unicode_type),intent(in) :: string
! 
! CHARACTERISTICS
! 
!   + the result is a string.
! 
! DESCRIPTION
!   trim(3) removes trailing blank characters from a string.
! 
! OPTIONS
!   + string : a string to trim
! 
! RESULT
!   the result is the same as string except trailing blanks are removed.
! 
!   if string is composed entirely of blanks or has zero length, the
!   result has zero length.
! 
! EXAMPLES
!   sample program:
! 
!    program demo_trim
!    use M_unicode, only : ut=>unicode_type, assignment(=)
!    use M_unicode, only : trim, len
!    use M_unicode, only : write(formatted)
!    implicit none
!    type(ut)                   :: str
!    type(ut), allocatable      :: strs(:)
!    character(len=*),parameter :: brackets='( *("[",DT,"]":,1x) )'
!    integer                    :: i
!       !
!       str='   trailing    '
!       print brackets, str,trim(str) ! trims it
!       !
!       str='   leading'
!       print brackets, str,trim(str) ! no effect
!       !
!       str='            '
!       print brackets, str,trim(str) ! becomes zero length
!       print *,  len(str), len(trim('               '))
!       !
!       strs=[ut("Z "),ut(" a b c"),ut("ABC   "),ut("")]
!       !
!       write(*,*)'untrimmed:'
!       print brackets, (strs(i), i=1,size(strs))
!       print brackets, strs
!       !
!       write(*,*)'trimmed:'
!       ! everything prints trimmed
!       print brackets, (trim(strs(i)), i=1,size(strs))
!       print brackets, trim(strs)
!       !
!    end program demo_trim
! 
!   results:
! 
!    > [   trailing    ] [   trailing]
!    > [   leading] [   leading]
!    > [            ] []
!    >           12           0
!    >  untrimmed:
!    > [Z ] [ a b c] [ABC   ] []
!    > [Z ] [ a b c] [ABC   ] []
!    >  trimmed:
!    > [Z] [ a b c] [ABC] []
!    > [Z] [ a b c] [ABC] []
! 
! SEE ALSO
!   Functions that perform operations on character strings, return
!   lengths of arguments, and search for certain arguments:
! 
!   + elemental: adjustl(3), adjustr(3), index(3), scan(3), verify(3)
! 
!   + nonelemental: len_trim(3), len(3), repeat(3), trim(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
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
! NAME
!   ADJUSTR(3f) - [M_unicode:WHITESPACE] right-justify a string
!                 (LICENSE:MIT)
! 
! SYNOPSIS
!   result = adjustr(string,glyphs)
! 
!    elemental function adjustr(string)
! 
!     type(unicode_type)            :: adjustr
!     type(unicode_type),intent(in) :: string
!     integer,intent(in),optional   :: glyphs
! 
! CHARACTERISTICS
!   + STRING is a string variable
!   + GLYPHS is a default integer
!   + the return value is a string variable
! 
! DESCRIPTION
!   ADJUSTR(3) right-justifies a string by removing trailing spaces. Spaces
!   are inserted at the start of the string as needed to retain the
!   original length unless an explicit return length is specified by the
!   GLYPHS parameter.
! 
! OPTIONS
!   + STRING : the string to right-justify
!   + GLYPHS : length in glyphs to extend to or truncate to
! 
! RESULT
!   trailing spaces are removed and the same number of spaces are then
!   inserted at the start of string.
! 
! EXAMPLES
! 
!  sample program:
! 
!   program demo_adjustr
!   use M_unicode, only : ut=>unicode_type
!   use M_unicode, only : adjustr, len
!   use M_unicode, only : write(formatted)
!   use M_unicode, only : assignment(=)
!   implicit none
!   type(ut)                   :: str
!   type(ut),allocatable       :: array(:)
!   integer                    :: i
!   character(len=*),parameter :: bracket='("[",DT,"]")'
!       !
!       call numberline(2)
!       !
!       ! basic usage
!       str = '  sample string     '
!       write(*,bracket) str
!       str = adjustr(str)
!       write(*,bracket) str
!       !
!       call numberline(5)
!       !
!       ! elemental
!       array=ut([character(len=50) :: &
!       '    एक (ek) ', &
!       '       दो (do) ', &
!       '          तीन(teen) ' ])
!       !
!       ! print array unadjusted
!       write(*,bracket)array
!       !do i=1,size(array)
!       !   write(*,'(*(g0,1x))')array(i)%codepoint()
!       !enddo
!       ! note 50 bytes is not necessarily 50 glyphs
!       write(*,'(*(g0,1x))')'length in glyphs=',len(array)
!       write(*,'(*(g0,1x))')'length in bytes=',(len(array(i)%character()),i=1,size(array))
!       !
!       call numberline(5)
!       !
!       ! print array right-justified
!       write(*,bracket)adjustr(array)
!       !
!       call numberline(5)
!       !
!       ! print array right-justified specifying number of glyphs
!       write(*,*)'set to 50'
!       write(*,bracket)adjustr(array,50)
!       !
!       write(*,*)'set to 60'
!       call numberline(6)
!       write(*,bracket)adjustr(array,60)
!       write(*,*)'set to 40'
!       call numberline(4)
!       write(*,bracket)adjustr(array,40)
!       write(*,*)'set to 10'
!       call numberline(1)
!       write(*,bracket)adjustr(array,10)
!       write(*,*)'set to 5'
!       write(*,bracket)adjustr(array,5)
!       write(*,*)'set to 4'
!       write(*,bracket)adjustr(array,4)
!       write(*,*)'set to 1'
!       write(*,bracket)adjustr(array,1)
!    contains
!       !
!       subroutine numberline(ireps)
!       integer,intent(in) :: ireps
!          write(*,'(1x,a)')repeat('1234567890',ireps)
!       end subroutine numberline
!    end program demo_adjustr
! 
!   Results:
! 
!    >  12345678901234567890
!    > [  sample string     ]
!    > [       sample string]
!    >  12345678901234567890123456789012345678901234567890
!    > [    एक (ek)                                   ]
!    > [       दो (do)                                ]
!    > [          तीन(teen)                         ]
!    > length in glyphs= 46 46 44
!    > length in bytes= 50 50 50
!    >  12345678901234567890123456789012345678901234567890
!    > [                                       एक (ek)]
!    > [                                       दो (do)]
!    > [                                   तीन(teen)]
!    >  12345678901234567890123456789012345678901234567890
!    >  set to 50
!    > [                                           एक (ek)]
!    > [                                           दो (do)]
!    > [                                         तीन(teen)]
!    >  set to 60
!    >  123456789012345678901234567890123456789012345678901234567890
!    > [                                                     एक (ek)]
!    > [                                                     दो (do)]
!    > [                                                   तीन(teen)]
!    >  set to 40
!    >  1234567890123456789012345678901234567890
!    > [                                 एक (ek)]
!    > [                                 दो (do)]
!    > [                               तीन(teen)]
!    >  set to 10
!    >  1234567890
!    > [   एक (ek)]
!    > [   दो (do)]
!    > [ तीन(teen)]
!    >  set to 5
!    > [ (ek)]
!    > [ (do)]
!    > [teen)]
!    >  set to 4
!    > [(ek)]
!    > [(do)]
!    > [een)]
!    >  set to 1
!    > [)]
!    > [)]
!    > [)]
! 
! SEE ALSO
!   ADJUSTL(3), TRIM(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
impure elemental function adjustr_str(string,glyphs) result(adjusted)

! ident_3="@(#) M_unicode adjustr(3f) adjust string to right"

! right-justify string by moving trailing spaces to beginning of string so length is retained even if spaces are of varied width

type(unicode_type), intent(in) :: string
integer,intent(in),optional    :: glyphs
type(unicode_type)             :: adjusted
integer                        :: last
integer                        :: i
   if(present(glyphs))then
      if(glyphs.le.0)then
         if(allocated(adjusted%codes))deallocate(adjusted%codes)
         allocate(adjusted%codes(0))
      elseif(glyphs.lt.size(string%codes))then ! shorter
         adjusted%codes=adjustl(string)
         adjusted=trim_str(adjusted)
         if(size(adjusted%codes).lt.glyphs)then
            adjusted%codes=[(32,i=1,glyphs-size(adjusted%codes)),adjusted%codes]
         else
            adjusted%codes=adjusted%codes(size(adjusted%codes)-glyphs+1:)
         endif
      elseif(glyphs.eq.size(string%codes))then
         last=len_trim_str(string)
         adjusted%codes=cshift(string%codes,-(size(string%codes)-last))
      else ! longer than string length
         adjusted%codes=[(32,i=1,glyphs-size(string%codes)),string%codes]
         last=len_trim_str(adjusted)
         adjusted%codes=cshift(adjusted%codes,-(size(adjusted%codes)-last))
      endif
   else
      last=len_trim_str(string)
      adjusted%codes=cshift(string%codes,-(size(string%codes)-last))
   endif

end function adjustr_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   ADJUSTL(3f) - [M_unicode:WHITESPACE] Left-justified a string
!                 (LICENSE:MIT)
! 
! SYNOPSIS
!   result = adjustl(string,glyphs)
! 
!    function adjustl(string,glyphs) result(out)
! 
!     type(unicode_type),intent(in) :: string
!     integer,intent(in),optional   :: glyphs
!     type(unicode_type)            :: out
! 
! CHARACTERISTICS
!   + STRING is a string variable of type(unicode_type)
!   + GLYPHS is a default integer
!   + The return value is a string variable of type(unicode_type)
! 
! DESCRIPTION
!   adjustl(3) will left-justify a string by removing leading spaces. Spaces
!   are inserted at the end of the string as needed to keep the number of
!   glyphs on output the same as the number on input unless overridden by
!   the GLYPHS parameter.
! 
! OPTIONS
!   +  STRING : the string to left-justify
!   +  GLYPHS : the length of the output in glyphs
! 
! RESULT
!   A copy of STRING where leading spaces are removed and the same
!   number of spaces are inserted on the end of STRING unless GLYPHS is
!   specified. Note using GLYPHS can cause in string truncation.
! 
! EXAMPLES
!   Sample program:
! 
!    program demo_adjustl
!    use M_unicode, only : ut=>unicode_type
!    use M_unicode, only : ch=>character
!    use M_unicode, only : adjustl, trim, len_trim, verify
!    use M_unicode, only : write(formatted)
!    use M_unicode, only : assignment(=)
!    implicit none
!    type(ut)                   :: usample, uout
!    integer                    :: istart, iend
!    character(len=*),parameter :: adt = '(a,"[",DT,"]")'
!     !
!     ! basic use
!       usample='   sample string   '
!       write(*,adt) 'original: ',usample
!     !
!     ! note a string stays the same length
!     ! and is not trimmed by just an adjustl(3) call.
!       write(*,adt) 'adjusted: ',adjustl(usample)
!     !
!     ! a fixed‐length string can be trimmed using trim(3)
!       uout=trim(adjustl(usample))
!       write(*,adt) 'trimmed:  ',uout
!     !
!     ! or alternatively you can select a substring without adjusting
!       istart= max(1,verify(usample, ' ')) ! first non‐blank character
!       iend = len_trim(usample)
!       write(*,adt) 'substring:',usample%sub(istart,iend)
!     !
!       write(*,adt) 'substring:',adjustl(usample,30)
!       write(*,adt) 'substring:',adjustl(usample,20)
!       write(*,adt) 'substring:',adjustl(usample,10)
!       write(*,adt) 'substring:',adjustl(usample,0)
!    end program demo_adjustl
! 
!   Results:
! 
!    > original: [   sample string   ]
!    > adjusted: [sample string      ]
!    > trimmed:  [sample string]
!    > substring:[sample string]
! 
! SEE ALSO
!   ADJUSTR(3), TRIM(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
!left-justify string by  moving leading spaces to end of string so length is retained even if spaces are of varied width
elemental function adjustl_str(string,glyphs) result(adjusted)
type(unicode_type),intent(in) :: string
integer,intent(in),optional   :: glyphs
type(unicode_type)            :: adjusted
integer                       :: first
integer                       :: i

   do first=1,size(string%codes),1
      if(any(string%codes(first).eq.unicode%SPACES))cycle
      exit
   enddo
   adjusted%codes=cshift(string%codes,first-1)
   if(present(glyphs))then
      if(glyphs.le.0)then
         deallocate(adjusted%codes)
         allocate(adjusted%codes(0))
      elseif(glyphs.le.size(adjusted%codes))then
         adjusted%codes=adjusted%codes(1:glyphs)
      else
         adjusted%codes=[adjusted%codes,(32,i=1,glyphs-size(adjusted%codes)+1)]
      endif
   endif

end function adjustl_str
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
! NAME
!   INDEX(3f) - [M_unicode:SEARCH] Position of a substring within a string
!               (LICENSE:MIT)
! 
! SYNOPSIS
!   result = index( string, substring [,back] [,kind] )
! 
!    elemental integer(kind=KIND) function index(string,substring,back,kind)
! 
!     character(len=*,kind=KIND),intent(in) :: string
!     character(len=*,kind=KIND),intent(in) :: substring
!     logical(kind=**),intent(in),optional :: back
!     integer(kind=**),intent(in),optional :: kind
! 
! CHARACTERISTICS
!   + STRING     is a character variable of any kind
! 
!   + SUBSTRING  is a character variable of the same kind as STRING
! 
!   + BACK       is a logical variable of any supported kind
! 
!   + KIND       is a scalar integer constant expression.
! 
! DESCRIPTION
!   INDEX(3) returns the position of the start of the leftmost or
!   rightmost occurrence of string SUBSTRING in STRING, counting from
!   one. If SUBSTRING is not present in STRING, zero is returned.
! 
! OPTIONS
!   + STRING : string to be searched for a match
! 
!   + SUBSTRING : string to attempt to locate in STRING
! 
!   + BACK : If the BACK argument is present and true, the return value
!     is the start of the rightmost occurrence rather than the
!     leftmost.
! 
!   + KIND : if KIND is present, the kind type parameter is that specified
!     by the value of KIND; otherwise the kind type parameter is
!     that of default integer type.
! 
! RESULT
!   The result is the starting position of the first substring SUBSTRING
!   found in STRING.
! 
!   If the length of SUBSTRING is longer than STRING the result is zero.
! 
!   If the substring is not found the result is zero.
! 
!   If BACK is .true. the greatest starting position is returned (that is,
!   the position of the right‐most match). Otherwise, the smallest
!   position starting a match (ie. the left‐most match) is returned.
! 
!   The position returned is measured from the left with the first character
!   of STRING being position one.
! 
!   Otherwise, if no match is found zero is returned.
! 
! EXAMPLES
!   Example program
! 
!    program demo_index
!    use M_unicode, only : ut=>unicode_type
!    use M_unicode, only : assignment(=)
!    use M_unicode, only : index
!    implicit none
!    type(ut)                   :: str
!    character(len=*),parameter :: all='(*(g0))'
!    integer                    :: ii
!       !
!       str='Huli i kēia kaula no kēia ʻōlelo'
!       !bug!print all, index(str,'kēia').eq.8
!       ii=index(str,'kēia'); print all, ii.eq.8
!       !
!       ! return value is counted from the left end even if BACK=.TRUE.
!       !bug!print all, index(str,'kēia',back=.true.).eq.22
!       ii=index(str,'kēia',back=.true.); print all, ii.eq.22
!       !
!       ! INDEX is case-sensitive
!       !bug!print all, index(str,'Kēia').eq.0
!       ii=index(str,'Kēia'); print all, ii.eq.0
!       !<<<<<<<<<<
!       !ifx bug: ifx (IFX) 2024.1.0 20240308
!       !
!       !example/demo_index.f90(17): error #6766: A binary defined OPERATOR
!       !definition is missing or incorrect.   [EQ]
!       !        print all, index(str,'k  ia',back=.true.).eq.22
!       !--------------------------------------------------^
!       !Original works with gfortran and flang_new and this works with ifx
!       !        ii=ndex(str,'k  ia',back=.true.)
!       !    print all, ii.eq.22
!       !>>>>>>>>>>
!    end program demo_index
! 
!   Expected Results:
! 
!    > T
!    > T
!    > T
!    > T
!    > T
!    > T
! 
! SEE ALSO
!   Functions that perform operations on character strings, return lengths
!   of arguments, and search for certain arguments:
! 
!   +  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)
! 
!   +  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
! find location of substring within string

elemental function index_str_str(string, substring, back) result(foundat)
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
   foundat = index_str_str(string, unicode_type(substring), back )
end function index_str_char

elemental function index_char_str(string, substring,back) result(foundat)
character(len=*), intent(in)   :: string
type(unicode_type), intent(in) :: substring
logical,intent(in),optional    :: back
integer                        :: foundat
   foundat = index_str_str(unicode_type(string), substring , back )
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
! NAME
!     SORT(3f) - [M_unicode:SORT] indexed hybrid quicksort of
!     an array
!     (LICENSE:MIT)
! 
! SYNOPSIS
! 
!       subroutine sort(data,index)
! 
!           type(unicode_type),intent(in) :: data(:)
!           integer,intent(out)           :: indx(size(data))
! 
! DESCRIPTION
!    A rank hybrid quicksort. The data is not moved. An integer array is
!    generated instead with values that are indices to the sorted order
!    of the data. This requires a second array the size of the input
!    array, which for large arrays would require a significant amount of
!    memory. One major advantage of this method is that the indices can
!    be used to access an entire user-defined type in sorted order. This
!    makes this seemingly simple sort procedure usable with the vast
!    majority of user-defined types. or other correlated data.
! 
! BACKGROUND
!     From Leonard J. Moss of SLAC:
! 
!     Here's a hybrid QuickSort I wrote a number of years ago. It's based
!     on suggestions in Knuth, Volume 3, and performs much better than a
!     pure QuickSort on short or partially ordered input arrays.
! 
!     This routine performs an in-memory sort of the first N elements of
!     array DATA, returning into array INDEX the indices of elements of
!     DATA arranged in ascending order. Thus,
! 
!        DATA(INDX(1)) will be the smallest number in array DATA;
!        DATA(INDX(N)) will be the largest number in DATA.
! 
!     The original data is not physically rearranged. The original order
!     of equal input values is not necessarily preserved.
! 
!     sort(3f) uses a hybrid QuickSort algorithm, based on several
!     suggestions in Knuth, Volume 3, Section 5.2.2. In particular, the
!     "pivot key" [my term] for dividing each subsequence is chosen to be
!     the median of the first, last, and middle values of the subsequence;
!     and the QuickSort is cut off when a subsequence has 9 or fewer
!     elements, and a straight insertion sort of the entire array is done
!     at the end. The result is comparable to a pure insertion sort for
!     very short arrays, and very fast for very large arrays (of order 12
!     micro-sec/element on the 3081K for arrays of 10K elements). It is
!     also not subject to the poor performance of the pure QuickSort on
!     partially ordered data.
! 
!     Complex values are sorted by the magnitude of sqrt(r**2+i**2).
! 
!     o Created: sortrx(3f): 15 Jul 1986, Len Moss
!     o saved from url=(0044)http://www.fortran.com/fortran/quick_sort2.f
!     o changed to update syntax from F77 style; John S. Urban 20161021
!     o generalized from only real values to include other intrinsic types;
!       John S. Urban 20210110
!     o type(unicode_type) version JSU 2025-09-20. See M_sort for other types.
! 
! EXAMPLES
! 
!   Sample usage:
! 
!    program demo_sort
!    use iso_fortran_env, only : stdout => output_unit
!    use M_unicode,       only : sort, unicode_type, assignment(=)
!    use M_unicode,       only : ut=>unicode_type, write(formatted)
!    use M_unicode,       only : ch=>character
!    implicit none
!    character(len=*),parameter :: g='(*(g0,1x))'
!    integer,parameter          :: isz=4
!    type(unicode_type)         :: rr(isz)
!    integer                    :: ii(isz)
!    integer                    :: i
!       !
!       write(stdout,g)'sort array with sort(3f)'
!       rr=[ &
!        ut("the"),   &
!        ut("quick"), &
!        ut("brown"), &
!        ut("fox") ]
!       !
!       write(stdout,g)'original order'
!       write(stdout,g)ch(rr)
!       !
!       call sort(rr,ii)
!       !
!       write(stdout,g)'sorted order'
!       ! convert to character
!       do i=1,size(rr)
!          write(stdout,'(i3.3,1x,a)')i,rr(ii(i))%character()
!       enddo
!       !
!       write(stdout,g)'reorder original'
!       rr=rr(ii)
!       write(stdout,g)ch(rr)
!    end program demo_sort
! 
!   Results:
! 
!    > sort array with sort(3f)
!    > original order
!    > the quick brown fox
!    > sorted order
!    > 001 brown
!    > 002 fox
!    > 003 quick
!    > 004 the
!    > reorder original
!    > brown fox quick the
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
subroutine sort_quick_rx(data,indx)

! ident_4="@(#) M_unicode sort_quick_rx(3f) indexed hybrid quicksort of a type(unicode_type) array"

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
impure elemental function reverse(string) result (rev)

! ident_5="@(#) M_unicode reverse(3f) Return a string reversed"

type(unicode_type),intent(in)  :: string   ! string to reverse
type(unicode_type)             :: rev      ! return value (reversed string)
   rev=string%sub(len(string),1,-1)
end function reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     REPLACE(3f) - [M_unicode:EDITING] function replaces one
!     substring for another in string
!     (LICENSE:MIT)
! 
! SYNOPSIS
! 
!  syntax:
! 
!       function replace(target,old,new, &
!        & occurrence, &
!        & repeat, &
!        & ignorecase, &
!        & ierr,back) result (newline)
!          or
!       function replace(target,start,end,new) result newline
! 
!       type(unicode_type)|character(len=*),intent(in) :: target
! 
!       type(unicode_type)|character(len=*),intent(in) :: old
!       type(unicode_type)|character(len=*),intent(in) :: new
!           or
!       type(unicode_type)|character(len=*),intent(in) :: new
!       integer, intent(in) :: start
!       integer, intent(in) :: end
! 
!       integer,intent(in),optional            :: occurrence
!       integer,intent(in),optional            :: repeat
!       logical,intent(in),optional            :: ignorecase
!       integer,intent(out),optional           :: changes
!       logical,intent(in),optional            :: back
!       character(len=:),allocatable           :: newline
! 
! CHARACTERISTICS
!   + TARGET,OLD and NEW may be a string or a character variable.
! 
! DESCRIPTION
!     Replace old substring with new value in string. Either a
!     old and new string is specified, or a new string and a
!     column range indicating the position of the text to replace
!     is specified.
! 
! OPTIONS
!      target      input line to be changed
!      old         old substring to replace
!      new         new substring
!      start       starting column of text to replace
!      end         ending column of text to replace
! 
!     KEYWORD REQUIRED
!      occurrence  if present, start changing at the Nth occurrence of the
!                  OLD string.
!      repeat      number of replacements to perform. Defaults to a global
!                  replacement.
!      ignorecase  whether to ignore ASCII case or not. Defaults
!                  to .false. .
!      back        if true start replacing moving from the right end of the
!                  string moving left instead of from the left to the right.
! RETURNS
!      newline     allocatable string returned
!      changes     count of changes made.
! 
! EXAMPLES
! 
!   Sample Program:
! 
!    program demo_replace
!    use M_unicode, only : ut=>unicode_type
!    use M_unicode, only : unicode_type
!    use M_unicode, only : character, replace
!    use M_unicode, only : write(formatted)
!    implicit none
!    type(unicode_type) :: line
!    !
!    write(*,'(DT)') &
!    & replace(ut('Xis is Xe string'),ut('X'),ut('th') )
!    write(*,'(DT)') &
!    & replace(ut('Xis is xe string'),ut('x'),ut('th'),ignorecase=.true.)
!    write(*,'(DT)') &
!    & replace(ut('Xis is xe string'),ut('X'),ut('th'),ignorecase=.false.)
!    !
!    ! a null old substring means "at beginning of line"
!    write(*,'(DT)') &
!    & replace(ut('my line of text'),ut(''),ut('BEFORE:'))
!    !
!    ! a null new string deletes occurrences of the old substring
!    write(*,'(DT)') replace(ut('I wonder i ii iii'),ut('i'),ut(''))
!    !
!    ! Examples of the use of RANGE
!    !
!    line=replace(ut('aaaaaaaaa'),ut('a'),ut('A'),occurrence=1,repeat=1)
!    write(*,*)'replace first a with A ['//line%character()//']'
!    !
!    line=replace(ut('aaaaaaaaa'),ut('a'),ut('A'),occurrence=3,repeat=3)
!    write(*,*)'replace a with A for 3rd to 5th occurrence [' &
!    & //line%character()//']'
!    !
!    line=replace(ut('ababababa'),ut('a'),ut(''),occurrence=3,repeat=3)
!    write(*,*)'replace a with null instances 3 to 5 ['// &
!    & line%character()//']'
!    !
!    line=replace( &
!     & ut('a b ab baaa aaaa aa aa a a a aa aaaaaa'),&
!     & ut('aa'),ut('CCCC'),occurrence=-1,repeat=1)
!    write(*,*)'replace lastaa with CCCC ['//line%character()//']'
!    !
!    write(*,'(DT)')replace(ut('myf90stuff.f90.f90'),&
!    & ut('f90'),ut('for'),occurrence=-1,repeat=1)
!    write(*,'(DT)')replace(ut('myf90stuff.f90.f90'),&
!    & ut('f90'),ut('for'),occurrence=-2,repeat=2)
!    !
!    end program demo_replace
! 
!   Results:
! 
!    > this is the string
!    > this is the string
!    > this is xe string
!    > BEFORE:my line of text
!    > I wonder
!    >  replace first a with A [Aaaaaaaaa]
!    >  replace a with A for 3rd to 5th occurrence [aaAAAaaaa]
!    >  replace a with null instances 3 to 5 [ababbb]
!    >  replace lastaa with CCCC [a b ab baaa aaaa aa aa a a a aa aaaaCCCC]
!    > myf90stuff.f90.for
!    > myforstuff.for.f90
! 
! AUTHOR
!     John S. Urban
! LICENSE
!    MIT
function replace_uuu(target,old,new,force_,occurrence,repeat,ignorecase,changes,back) result (newline)

! ident_6="@(#) M_unicode replace(3f) replace one substring for another in string"

! parameters
type(unicode_type),intent(in)            :: target     ! input line to be changed
type(unicode_type),intent(in)            :: old        ! old substring to replace
type(unicode_type),intent(in)            :: new        ! new substring
type(force_keywords),optional,intent(in) :: force_
integer,intent(in),optional              :: occurrence ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional              :: repeat     ! how many replacements
logical,intent(in),optional              :: ignorecase
integer,intent(out),optional             :: changes    ! number of changes made
logical,intent(in),optional              :: back

! returns
type(unicode_type) :: newline               ! output string

! local
type(unicode_type) :: new_local, old_local, old_local_for_comparison
integer            :: icount,ichange
integer            :: original_input_length
integer            :: len_old, len_new
integer            :: ladd
integer            :: left_margin, right_margin
integer            :: ind
integer            :: ic
integer            :: ichr
integer            :: range_local(2)
integer            :: ilen_temp
type(unicode_type) :: target_for_comparison   ! input line to be changed
logical            :: ignorecase_local
logical            :: flip
type(unicode_type) :: target_local   ! input line to be changed

   flip=.false.
   ignorecase_local=.false.
   original_input_length=len_trim(target)          ! get non-blank length of input line

   old_local=old
   new_local=new

   if(present(ignorecase))then
      ignorecase_local=ignorecase
   else
      ignorecase_local=.false.
   endif
   if(present(occurrence))then
      range_local(1)=abs(occurrence)
   else
      range_local(1)=1
   endif
   if(present(repeat))then
      range_local(2)=range_local(1)+repeat-1
   else
      range_local(2)=original_input_length
   endif
   if(ignorecase_local)then
      target_for_comparison=lower(target)
      old_local_for_comparison=lower(old_local)
   else
      target_for_comparison=target
      old_local_for_comparison=old_local
   endif
   if(present(back))then
      flip=back
   endif
   if(present(occurrence))then
      if(occurrence < 0)then
         flip=.true.
         target_for_comparison=reverse(target_for_comparison)
         target_local=reverse(target)
         old_local_for_comparison=reverse(old_local_for_comparison)
         old_local=reverse(old_local)
         new_local=reverse(new_local)
      else
         target_local=target
      endif
   else
      target_local=target
   endif

   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   len_old=len(old_local)                              ! length of old substring to be replaced
   len_new=len(new_local)                              ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(target)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string

   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichr=len_new + original_input_length
      if(len_new > 0)then
         newline=new_local%sub(1,len_new).cat.target_local%sub(left_margin,original_input_length)
      else
         newline=target_local%sub(left_margin,original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(changes))changes=ichange
      if(flip) newline=reverse(newline)
      return
   endif

   ichr=left_margin                                   ! place to put characters into output string
   ic=left_margin                                     ! place looking at in input string
   loop: do
                                                      ! try finding start of OLD in remaining part of input in change window
      ilen_temp=len(target_for_comparison)
      ind=index(target_for_comparison%sub(ic,ilen_temp),old_local_for_comparison%sub(1,len_old))+ic-1
      if(ind == ic-1.or.ind > right_margin)then       ! did not find old string or found old string past edit window
         exit loop                                    ! no more changes left to make
      endif
      icount=icount+1                                 ! found an old string to change, so increment count of change candidates
      if(ind > ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                  ! find length of character range to copy as-is from input to output
         newline=newline%sub(1,ichr-1).cat.target_local%sub(ic,ind-1)
         ichr=ichr+ladd
      endif
      if(icount >= range_local(1).and.icount <= range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new /= 0)then                                          ! put in new string
            newline=newline%sub(1,ichr-1).cat.new_local%sub(1,len_new)
            ichr=ichr+len_new
         endif
      else
         if(len_old /= 0)then                                          ! put in copy of old string
            newline=newline%sub(1,ichr-1).cat.old_local%sub(1,len_old)
            ichr=ichr+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop

   select case (ichange)
   case (0)                                        ! there were no changes made to the window
      newline=target_local                         ! if no changes made output should be input
   case default
      if(ic <= len(target))then                    ! if there is more after last change on original line add it
         newline=newline%sub(1,ichr-1).cat.target_local%sub(ic,max(ic,original_input_length))
      endif
   end select
   if(present(changes))changes=ichange
   if(flip) newline=reverse(newline)
end function replace_uuu
!===================================================================================================================================
function replace_uua(target,old,new,force_,occurrence,repeat,ignorecase,changes,back) result (newline)
type(unicode_type),intent(in)            :: target
type(unicode_type),intent(in)            :: old
character(len=*),intent(in)              :: new
type(force_keywords),optional,intent(in) :: force_
integer,intent(in),optional              :: occurrence ,repeat
logical,intent(in),optional              :: ignorecase
integer,intent(out),optional             :: changes
logical,intent(in),optional              :: back
type(unicode_type)                       :: newline
newline=replace_uuu(target,old,unicode_type(new),force_,occurrence,repeat,ignorecase,changes,back)
end function replace_uua
!===================================================================================================================================
function replace_uau(target,old,new,force_,occurrence,repeat,ignorecase,changes,back) result (newline)
type(unicode_type),intent(in)            :: target
character(len=*),intent(in)              :: old
type(unicode_type),intent(in)            :: new
type(force_keywords),optional,intent(in) :: force_
integer,intent(in),optional              :: occurrence ,repeat
logical,intent(in),optional              :: ignorecase
integer,intent(out),optional             :: changes
logical,intent(in),optional              :: back
type(unicode_type)                       :: newline
newline=replace_uuu(target,unicode_type(old),new,force_,occurrence,repeat,ignorecase,changes,back)
end function replace_uau
!===================================================================================================================================
function replace_uaa(target,old,new,force_,occurrence,repeat,ignorecase,changes,back) result (newline)
type(unicode_type),intent(in)            :: target
character(len=*),intent(in)              :: old
character(len=*),intent(in)              :: new
type(force_keywords),optional,intent(in) :: force_
integer,intent(in),optional              :: occurrence ,repeat
logical,intent(in),optional              :: ignorecase
integer,intent(out),optional             :: changes
logical,intent(in),optional              :: back
type(unicode_type)                       :: newline
newline=replace_uuu(target,unicode_type(old),unicode_type(new),force_,occurrence,repeat,ignorecase,changes,back)
end function replace_uaa
!===================================================================================================================================
function replace_aaa(target,old,new,force_,occurrence,repeat,ignorecase,changes,back) result (newline)
character(len=*),intent(in)              :: target
character(len=*),intent(in)              :: old
character(len=*),intent(in)              :: new
type(force_keywords),optional,intent(in) :: force_
integer,intent(in),optional              :: occurrence ,repeat
logical,intent(in),optional              :: ignorecase
integer,intent(out),optional             :: changes
logical,intent(in),optional              :: back
type(unicode_type)                       :: newline
newline=replace_uuu(unicode_type(target),unicode_type(old),unicode_type(new),force_,occurrence,repeat,ignorecase,changes,back)
end function replace_aaa
!===================================================================================================================================
function replace_aua(target,old,new,force_,occurrence,repeat,ignorecase,changes,back) result (newline)
character(len=*),intent(in)              :: target
type(unicode_type),intent(in)            :: old
character(len=*),intent(in)              :: new
type(force_keywords),optional,intent(in) :: force_
integer,intent(in),optional              :: occurrence ,repeat
logical,intent(in),optional              :: ignorecase
integer,intent(out),optional             :: changes
logical,intent(in),optional              :: back
type(unicode_type)                       :: newline
newline=replace_uuu(unicode_type(target),old,unicode_type(new),force_,occurrence,repeat,ignorecase,changes,back)
end function replace_aua
!===================================================================================================================================
function replace_aau(target,old,new,force_,occurrence,repeat,ignorecase,changes,back) result (newline)
character(len=*),intent(in)              :: target
character(len=*),intent(in)              :: old
type(unicode_type),intent(in)            :: new
type(force_keywords),optional,intent(in) :: force_
integer,intent(in),optional              :: occurrence ,repeat
logical,intent(in),optional              :: ignorecase
integer,intent(out),optional             :: changes
logical,intent(in),optional              :: back
type(unicode_type)                       :: newline
newline=replace_uuu(unicode_type(target),unicode_type(old),new,force_,occurrence,repeat,ignorecase,changes,back)
end function replace_aau
!===================================================================================================================================
function replace_auu(target,old,new,force_,occurrence,repeat,ignorecase,changes,back) result (newline)
character(len=*),intent(in)              :: target
type(unicode_type),intent(in)            :: old
type(unicode_type),intent(in)            :: new
type(force_keywords),optional,intent(in) :: force_
integer,intent(in),optional              :: occurrence ,repeat
logical,intent(in),optional              :: ignorecase
integer,intent(out),optional             :: changes
logical,intent(in),optional              :: back
type(unicode_type)                       :: newline
newline=replace_uuu(unicode_type(target),old,new,force_,occurrence,repeat,ignorecase,changes,back)
end function replace_auu
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     JOIN(3f) - [M_unicode:EDITING] append CHARACTER variable array into
!     a single CHARACTER variable with specified separator
!     (LICENSE:MIT)
! 
! SYNOPSIS
! 
!     impure function join(str,sep,clip) result (string)
! 
!      type(unicode_type),intent(in)          :: str(:)
!      type(unicode_type),intent(in),optional :: sep
!      logical,intent(in),optional            :: clip
!      type(unicode_type),allocatable         :: string
! 
! DESCRIPTION
!    JOIN(3f) appends the elements of a CHARACTER array into a single
!    CHARACTER variable, with elements 1 to N joined from left to right.
!    By default each element is trimmed of trailing spaces and the
!    default separator is a null string.
! 
! OPTIONS
!       STR     array of variables to be joined
!       SEP     separator string to place between each variable. defaults
!               to a null string.
!       CLIP    option to trim each element of STR of trailing and leading
!               spaces. Defaults to .TRUE.
! 
! RETURNS
!       STRING  CHARACTER variable composed of all of the elements of STR()
!               appended together with the optional separator SEP placed
!               between the elements.
! 
! EXAMPLES
! 
!   Sample program:
! 
!    program demo_join
!    use M_unicode,  only : join, ut=>unicode_type, ch=>character, assignment(=)
!    !use M_unicode, only : write(formatted)
!    implicit none
!    character(len=*),parameter    :: w='((g0,/,g0))'
!    !character(len=*),parameter   :: v='((g0,/,DT))'
!    character(len=20),allocatable :: proverb(:)
!    type(ut),allocatable          :: s(:)
!    type(ut),allocatable          :: sep
!      !
!      proverb=[ character(len=13) :: &
!        & ' United'       ,&
!        & '  we'          ,&
!        & '   stand,'     ,&
!        & '    divided'   ,&
!        & '     we fall.' ]
!      !
!      if(allocated(s))deallocate(s)
!      allocate(s(size(proverb))) ! avoid GNU Fortran (GCC) 16.0.0 bug
!      s=proverb
!      write(*,w) 'SIMPLE JOIN:         ', ch( join(s)                )
!      write(*,w) 'JOIN WITH SEPARATOR: ', ch( join(s,sep=ut(' '))    )
!      write(*,w) 'CUSTOM SEPARATOR:    ', ch( join(s,sep=ut('<-->')) )
!      write(*,w) 'NO TRIMMING:         ', ch( join(s,clip=.false.)   )
!      !
!      sep=ut()
!      write(*,w) 'SIMPLE JOIN:         ', ch(sep%join(s) )
!      sep=' '
!      write(*,w) 'JOIN WITH SEPARATOR: ', ch(sep%join(s) )
!      sep='<-->'
!      write(*,w) 'CUSTOM SEPARATOR:    ', ch(sep%join(s) )
!      sep=''
!      write(*,w) 'NO TRIMMING:         ', ch(sep%join(s,clip=.false.) )
!    end program demo_join
! 
!  Results:
! 
!   > SIMPLE JOIN:
!   > Unitedwestand,dividedwe fall.
!   > JOIN WITH SEPARATOR:
!   > United we stand, divided we fall.
!   > CUSTOM SEPARATOR:
!   > United==>we==>stand,==>divided==>we fall.
!   > NO TRIMMING:
!   >  United         we             stand,         divided        we fall.
!   > SIMPLE JOIN:
!   > Unitedwestand,dividedwe fall.
!   > JOIN WITH SEPARATOR:
!   > United we stand, divided we fall.
!   > CUSTOM SEPARATOR:
!   > United==>we==>stand,==>divided==>we fall.
!   > NO TRIMMING:
!   >  United         we             stand,         divided        we fall.
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
impure function join(str,sep,clip) result (string)

! ident_7="@(#) M_unicode join(3f) merge string array into a single string value adding specified separator"

type(unicode_type),intent(in)          :: str(:)
type(unicode_type),intent(in),optional :: sep
logical,intent(in),optional            :: clip
type(unicode_type)                     :: temp
type(unicode_type)                     :: sep_local
type(unicode_type)                     :: string
logical                                :: clip_local
integer                                :: i
   if(present(sep))then  ; sep_local=sep   ; else ; sep_local=''      ; endif
   if(present(clip))then ; clip_local=clip ; else ; clip_local=.true. ; endif
   string=''
   if(size(str) /= 0)then
      do i = 1,size(str)-1
         if(clip_local)then
            temp=adjustl(str(i)) ! avoid gfortran GNU Fortran (GCC) 16.0.0 20250727 (experimental) bug
            !gfortran!string=string//adjustl(trim(str))//sep_local ! produces no left adjust in gfortran as the moment
            !ifx!string=string//trim(temp)//sep_local
            temp=trim(temp)
            string=[string%codes,temp%codes,sep_local%codes]
         else
            !string=string//str(i)//sep_local
            string=[string%codes,str(i)%codes,sep_local%codes]
         endif
      enddo
      if(clip_local)then
         temp=adjustl(str(i))
         !string=[string//trim(temp)
         temp=trim(temp)
         string=[string%codes,temp%codes]
      else
         !string=string//str(i)
         string=[string%codes,str(i)%codes]
      endif
   endif
end function join
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!  UPPER(3f) - [M_unicode:CASE] changes a string to uppercase
!  (LICENSE:MIT)
! 
! SYNOPSIS
! 
!     elemental pure function upper(str) result (string)
! 
!      character(*), intent(in)    :: str
!      character(len(str))         :: string  ! output string
! 
! DESCRIPTION
!    upper(string) returns a copy of the input string with all characters
!    converted to uppercase, assuming Unicode character sets are being used.
! 
! OPTIONS
!     str    string to convert to uppercase
! 
! RETURNS
!     upper  copy of the input string with all characters converted to
!            uppercase.
! 
! TRIVIA
!     The terms "uppercase" and "lowercase" date back to the early days of
!     the mechanical printing press. Individual metal alloy casts of each
!     needed letter, or punctuation symbol, were meticulously added to a
!     press block, by hand, before rolling out copies of a page. These
!     metal casts were stored and organized in wooden cases. The more
!     often needed miniscule letters were placed closer to hand, in the
!     lower cases of the work bench. The less often needed, capitalized,
!     majuscule letters, ended up in the harder to reach upper cases.
! 
! EXAMPLES
! 
!   Sample program:
! 
!    program demo_upper
!    use iso_fortran_env, only : stdout => output_unit
!    use M_unicode,       only : upper, unicode_type, assignment(=)
!    use M_unicode,       only : ut => unicode_type, operator(==)
!    implicit none
!    character(len=*),parameter :: g='(*(g0))'
!    type(unicode_type)         :: pangram
!    type(unicode_type)         :: diacritics
!    type(unicode_type)         :: expected
!       !
!       ! a sentence containing every letter of the English alphabet
!       ! often used to test telegraphs since the advent of the 19th century
!       ! and as an exercise repetitively generated in typing classes
!       pangram  = "The quick brown fox jumps over the lazy dog."
!       expected = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG."
!       call test(pangram,expected)
!       !
!       ! Slovak pangram
!       pangram    = 'Vypätá dcéra grófa Maxwella s IQ nižším ako &
!       &kôň núti čeľaď hrýzť hŕbu jabĺk.'
!       expected   = 'VYPÄTÁ DCÉRA GRÓFA MAXWELLA S IQ NIŽŠÍM AKO &
!       &KÔŇ NÚTI ČEĽAĎ HRÝZŤ HŔBU JABĹK.'
!       call test(pangram,expected)
!       !
!       ! contains each special Czech letter with diacritics exactly once
!       print g,'("A horse that was too yellow-ish moaned devilish odes")'
!       diacritics = 'Příliš žluťoučký kůň úpěl ďábelské ódy.'
!       expected   = 'PŘÍLIŠ ŽLUŤOUČKÝ KŮŇ ÚPĚL ĎÁBELSKÉ ÓDY.'
!       call test(diacritics,expected)
!    contains
!    subroutine test(in,expected)
!    type(unicode_type),intent(in) :: in
!    type(unicode_type),intent(in) :: expected
!    type(unicode_type)            :: uppercase
!    character(len=*),parameter    :: nl=new_line('A')
!       write(stdout,g)in%character()
!       uppercase=upper(in)
!       write(stdout,g)uppercase%character()
!       write(stdout,g)merge('PASSED','FAILED',uppercase == expected ),nl
!    end subroutine test
!    end program demo_upper
! 
!  Expected output
! 
!   > The quick brown fox jumps over the lazy dog.
!   > THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG.
!   > PASSED
!   >
!   > Vypätá dcéra grófa Maxwella s IQ nižším ako kôň núti ...
!   > čeľaď hrýzť hŕbu jabĺk.
!   > VYPÄTÁ DCÉRA GRÓFA MAXWELLA S IQ NIŽŠÍM AKO KÔŇ NÚTI ...
!   > ČEĽAĎ HRÝZŤ HŔBU JABĹK.
!   > PASSED
!   >
!   > ("A horse that was too yellow-ish moaned devilish odes")
!   > Příliš žluťoučký kůň úpěl ďábelské ódy.
!   > PŘÍLIŠ ŽLUŤOUČKÝ KŮŇ ÚPĚL ĎÁBELSKÉ ÓDY.
!   > PASSED
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
elemental pure function upper(str) result (string)

! ident_8="@(#) M_unicode upper(3f) returns an uppercase string"

type(unicode_type),intent(in) :: str                 ! input string to convert to all uppercase
type(unicode_type)            :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: pos
integer,parameter             :: ade_a = iachar('a'), ade_z = iachar('z')
integer,parameter             :: diff = iachar('A') - iachar('a')

   string=str
   do i=1,len(str)                           ! step thru each letter in the string in specified range
      select case(str%codes(i))
      case(ade_a:ade_z)
         string%codes(i) = str%codes(i) + diff
      case default
         pos=binary_search(low_to_up(:,1),str%codes(i))
         if(pos > 0)then
            string%codes(i) = low_to_up(pos,2)
         endif
      end select
   enddo

   if(len(str).eq.0)string = str

end function upper

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     LOWER(3f) - [M_unicode:CASE] changes a string to lowercase over
!     specified range
!     (LICENSE:MIT)
! 
! SYNOPSIS
! 
!     elemental pure function lower(str) result (string)
! 
!      character(*), intent(in) :: str
!      character(len(str))      :: string  ! output string
! 
! DESCRIPTION
!       lower(str) returns a copy of the input string with all
!       characters converted to miniscule (ie. "lowercase").
! 
! OPTIONS
!     str    string to convert to miniscule
! 
! RETURNS
!     lower  copy of the entire input string with all characters converted
!            to miniscule.
! 
! TRIVIA
!    The terms "uppercase" and "lowercase" date back to the early days
!    of the mechanical printing press. Individual metal alloy casts of
!    each needed letter or punctuation symbol were meticulously added to a
!    press block, by hand, before rolling out copies of a page. These metal
!    casts were stored and organized in wooden cases. The more-often-needed
!    miniscule letters were placed closer to hand, in the lower cases of
!    the work bench. The less often needed, capitalized, majuscule letters,
!    ended up in the harder to reach upper cases.
! 
! EXAMPLES
! 
!  Sample program:
! 
!    program demo_lower
!    use iso_fortran_env, only : stdout => output_unit
!    use M_unicode,       only : lower, unicode_type, assignment(=), trim
!    use M_unicode,       only : ut => unicode_type, operator(==)
!    implicit none
!    character(len=*),parameter :: g='(*(g0))'
!    type(unicode_type) :: pangram
!    type(unicode_type) :: diacritics
!    type(unicode_type) :: expected
!      !
!      ! a sentence containing every letter of the English alphabet
!      pangram="THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
!      expected="the quick brown fox jumps over the lazy dog"
!      call test(pangram,expected)
!      !
!      ! Slovak pangram
!      PANGRAM    = 'VYPÄTÁ DCÉRA GRÓFA MAXWELLA S IQ NIŽŠÍM AKO &
!      &KÔŇ NÚTI ČEĽAĎ HRÝZŤ HŔBU JABĹK.'
!      expected   = 'vypätá dcéra grófa maxwella s iq nižším ako &
!      &kôň núti čeľaď hrýzť hŕbu jabĺk.'
!      call test(pangram,expected)
!      !
!      ! contains each special Czech letter with diacritics exactly once
!      DIACRITICS='PŘÍLIŠ ŽLUŤOUČKÝ KŮŇ ÚPĚL ĎÁBELSKÉ ÓDY.'
!      expected ='příliš žluťoučký kůň úpěl ďábelské ódy.'
!      print g,'("A horse that was too yellow-ish moaned devilish odes")'
!      call test(diacritics,expected)
!    contains
!    subroutine test(in,expected)
!    type(unicode_type),intent(in) :: in
!    type(unicode_type),intent(in) :: expected
!    type(unicode_type)            :: lowercase
!    character(len=*),parameter    :: nl=new_line('A')
!        write(stdout,g)in%character()
!        lowercase=lower(in)
!        write(stdout,g)lowercase%character()
!        write(stdout,g)merge('PASSED','FAILED',lowercase == expected ),nl
!    end subroutine test
!    end program demo_lower
! 
!   Expected output
! 
!    > THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG
!    > the quick brown fox jumps over the lazy dog
!    > PASSED
!    >
!    > VYPÄTÁ DCÉRA GRÓFA MAXWELLA S IQ NIŽŠÍM AKO KÔŇ NÚTI ...
!    > ČEĽAĎ HRÝZŤ HŔBU JABĹK.
!    > vypätá dcéra grófa maxwella s iq nižším ako kôň núti ...
!    > čeľaď hrýzť hŕbu jabĺk.
!    > PASSED
!    >
!    > ("A horse that was too yellow-ish moaned devilish odes")
!    > PŘÍLIŠ ŽLUŤOUČKÝ KŮŇ ÚPĚL ĎÁBELSKÉ ÓDY.
!    > příliš žluťoučký kůň úpěl ďábelské ódy.
!    > PASSED
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
elemental pure function lower(str) result (string)

! ident_9="@(#) M_unicode lower(3f) returns a lowercase string"

type(unicode_type), intent(in) :: str                 ! input string to convert to all lowercase
type(unicode_type)             :: string              ! output string that contains no miniscule letters
integer                        :: i                   ! loop counter
integer                        :: pos
integer, parameter             :: ade_a = iachar('A'), ade_z = iachar('Z')
integer, parameter             :: diff = iachar('A') - iachar('a')

   string=str
   do i=1,len(str)                           ! step thru each letter in the string in specified range
      select case(str%codes(i))
      case(ade_a:ade_z)
         string%codes(i) = str%codes(i) - diff
      case default
         pos=binary_search(up_to_low(:,1),str%codes(i))
         if(pos > 0)then
            string%codes(i) = up_to_low(pos,2)
         endif
      end select
   enddo

   if(len(str).eq.0)string = str

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   SPLIT(3f) - [M_unicode:PARSE] parse a string into tokens, one at a time.
!   (LICENSE:MIT)
! 
! SYNOPSIS
!   call split (string, set, pos [, back])
! 
!    type(unicode_type),intent(in) :: string
!    type(unicode_type),intent(in) :: set
!    integer,intent(inout)         :: pos
!    logical,intent(in),optional   :: back
! 
! CHARACTERISTICS
!   + STRING is a scalar character variable
!   + SET is a scalar string variable
! 
! DESCRIPTION
!   Find the extent of consecutive tokens in a string. given a string and
!   a position to start looking for a token return the position of the
!   end of the token. a set of separator characters may be specified as
!   well as the direction of parsing.
! 
!   typically consecutive calls are used to parse a string into a set of
!   tokens by stepping through the start and end positions of each token.
! 
! OPTIONS
!   + STRING : the string to search for tokens in.
! 
!   + SET : Each character in set is a token delimiter. a sequence of
!     zero or more characters in string delimited by any token delimiter,
!     or the beginning or end of string, comprise a token. thus, two
!     consecutive token delimiters in STRING, or a token delimiter in the
!     first or last character of STRING, indicate a token with zero length.
! 
!   + POS : on input, the position from which to start looking for the next
!     separator from. This is typically the first character or the last
!     returned value of POS if searching from left to right (ie. back is
!     absent or .true.) or the last character or the last returned value
!     of POS when searching from right to left (ie. when back is .FALSE.).
! 
!     If BACK is present with the value .TRUE., the value of pos shall be
!     in the range 0 < POS <= len(STRING)+1; otherwise it shall be in the
!     range 0 <= POS <= len(STRING).
! 
!     So POS on input is typically an end of the string or the position
!     of a separator, probably from a previous call to split but POS on
!     input can be any position in the range 1 <= POS <= len(STRING). if
!     POS points to a non-separator character in the string the call is
!     still valid but it will start searching from the specified position
!     and that will result (somewhat obviously) in the string from POS on
!     input to the returned POS being a partial token.
! 
!   + BACK : If BACK is absent or is present with the value .FALSE., POS is
!     assigned the position of the leftmost token delimiter in string
!     whose position is greater than POS, or if there is no such character,
!     it is assigned a value one greater than the length of string. this
!     identifies a token with starting position one greater than the value
!     of POS on invocation, and ending position one less than the value
!     of POS on return.
! 
!     If BACK is present with the value .TRUE., POS is assigned the
!     position of the rightmost token delimiter in string whose position
!     is less than POS, or if there is no such character, it is assigned
!     the value zero. This identifies a token with ending position one
!     less than the value of POS on invocation, and starting position one
!     greater than the value of POS  on return.
! 
! EXAMPLE
!   sample program:
! 
!    program demo_split
!    use iso_fortran_env, only : stdout => output_unit
!    use M_unicode,       only : unicode_type, assignment(=)
!    use M_unicode,       only : split, len, character
!    use M_unicode,       only : ut=>unicode_type
!    implicit none
!    character(len=*),parameter :: g='(*(g0,1x))'
!    type(ut)                   :: proverb
!    type(ut)                   :: delims
!    type(ut),allocatable       :: array(:)
!    integer                    :: first
!    integer                    :: last
!    integer                    :: pos
!    integer                    :: i
!       !
!       delims= '=|; '
!       !
!       proverb="Más vale pájaro en mano, que ciento volando."
!       call printwords(proverb)
! 
!       ! there really are not spaces between these glyphs
!       array=[ &
!        ut("七転び八起き。"), &
!        ut("転んでもまた立ち上がる。"), &
!        ut("くじけずに前を向いて歩いていこう。")]
!       call printwords(array)
!       !
!       write(stdout,g)'OOP'
!       array=proverb%split(ut(' '))
!       write(stdout,'(*(:"[",a,"]"))')(character(array(i)),i=1,size(array))
!    contains
!    impure elemental subroutine printwords(line)
!    type(ut),intent(in) :: line
!       pos = 0
!       write(stdout,g)line%character(),len(line)
!       do while (pos < len(line))
!           first = pos + 1
!           call split (line, delims, pos)
!           last = pos - 1
!           print g, line%character(first,last),first,last,pos
!       end do
!    end subroutine printwords
!    end program demo_split
! 
!   Results:
! 
!    > Project is up to date
!    > Más vale pájaro en mano, que ciento volando. 44
!    > Más 1 3 4
!    > vale 5 8 9
!    > pájaro 10 15 16
!    > en 17 18 19
!    > mano, 20 24 25
!    > que 26 28 29
!    > ciento 30 35 36
!    > volando. 37 44 45
!    > 七転び八起き。 7
!    > 七転び八起き。 1 7 8
!    > 転んでもまた立ち上がる。 12
!    > 転んでもまた立ち上がる。 1 12 13
!    > くじけずに前を向いて歩いていこう。 17
!    > くじけずに前を向いて歩いていこう。 1 17 18
!    > OOP
!    > [Más][vale][pájaro][en][mano,][que][ciento][volando.]
! 
! SEE ALSO
!   + tokenize(3) - parse a string into tokens
!   + index(3) - position of a substring within a string
!   + scan(3) - scan a string for the presence of a set of characters
!   + verify(3)  -  position  of a character in a string of characters that does
!     not appear in a given set of characters.
! 
! AUTHOR
!     Milan Curcic, "milancurcic@hey.com"
!     John S. Urban -- UTF-8 version
! 
! LICENSE
!     MIT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   TOKENIZE(3f) - [M_unicode:PARSE] Parse a string into tokens.
!   (LICENSE:MIT)
! 
! SYNOPSIS
!   TOKEN form (returns array of strings)
! 
!    subroutine tokenize(string, set, tokens [, separator])
! 
!     type(unicode_type),intent(in) :: string
!     type(unicode_type),intent(in) :: set
!     type(unicode_type),allocatable,intent(out) :: tokens(:)
!     type(unicode_type),allocatable,intent(out),optional :: separator(:)
! 
!   ARRAY BOUNDS form (returns arrays defining token positions)
! 
!    subroutine tokenize (string, set, first, last)
! 
!     type(unicode_type),intent(in) :: string
!     type(unicode_type),intent(in) :: set
!     integer,allocatable,intent(out) :: first(:)
!     integer,allocatable,intent(out) :: last(:)
! 
! CHARACTERISTICS
!   +  STRING ‐ a scalar of type string. It is an INTENT(IN)
!      argument.
! 
!   +  SET ‐ a scalar of type string with the same kind type
!      parameter as STRING. It is an INTENT(IN) argument.
! 
!   +  SEPARATOR ‐ (optional) shall be of type string. It is an
!      INTENT(OUT)argument. It shall not be a coarray or a coindexed object.
! 
!   +  TOKENS ‐ of type string. It is an INTENT(OUT) argument. It shall
!      not be a coarray or a coindexed object.
! 
!   +  FIRST,LAST ‐ an allocatable array of type integer and rank
!      one. It is an INTENT(OUT) argument. It shall not be a coarray or a
!      coindexed object.
! 
! DESCRIPTION
!   TOKENIZE(3) parses a string into tokens. There are two forms of the
!   subroutine TOKENIZE(3).
! 
!   +  The token form returns an array with one token per element,
!      all of the same length as the longest token.
! 
!   +  The array bounds form returns two integer arrays. One
!      contains the beginning position of the tokens and the other the end
!      positions.
! 
!   Since the token form pads all the tokens to the same length the
!   original number of trailing spaces of each token accept for the
!   longest is lost.
! 
!   The array bounds form retains information regarding the exact token
!   length even when padded by spaces.
! 
! OPTIONS
!   •  STRING : The string to parse into tokens.
! 
!   +  SET :  Each character in SET is a token delimiter. A
!      sequence of zero or more characters in STRING delimited by any token
!      delimiter, or the beginning or end of STRING, comprise a token. Thus,
!      two consecutive token delimiters in STRING, or a token delimiter
!      in the first or last character of STRING, indicate a token with
!      zero length.
! 
!   +  TOKENS : It shall be an allocatable array of rank one with
!      deferred length. It is allocated with the lower bound equal to one
!      and the upper bound equal to the number of tokens in STRING, and
!      with character length equal to the length of the longest token.
! 
!      The tokens in STRING are assigned in the order found, as if by
!      intrinsic assignment, to the elements of TOKENS, in array element
!      order.
! 
!   +  FIRST : shall be an allocatable array of type integer and rank one.
!      It is an INTENT(OUT) argument. It shall not be a coarray or
!      a coindexed object.
! 
!      It is allocated with the lower bound equal to one and the upper
!      bound equal to the number of tokens in STRING. Each element is
!      assigned, in array element order, the starting position of each
!      token in STRING, in the order found.
! 
!      If a token has zero length, the starting position is equal to
!      one if the token is at the beginning of STRING, and one greater
!      than the position of the preceding delimiter otherwise.
! 
!   +  LAST : It is allocated with the lower bound equal to one and the
!      upper bound equal to the number of tokens in STRING. Each
!      element is assigned, in array element order, the ending position
!      of each token in STRING, in the order found.
! 
!      If a token has zero length, the ending position is one less than
!      the starting position.
! 
! EXAMPLES
! 
!   Sample of uses
! 
!    program demo_tokenize
!    use M_unicode, only : tokenize, ut=>unicode_type,ch=>character
!    use M_unicode, only : assignment(=),operator(/=)
!    implicit none
!    !
!    ! some useful formats
!    character(len=*),parameter ::       &
!     & brackets='(*("[",g0,"]":,","))' ,&
!     & a_commas='(a,*(g0:,","))'       ,&
!     & gen='(*(g0))'
!    !
!    ! Execution of TOKEN form (return array of tokens)
!    !
!       block
!       type(ut)             :: string
!       type(ut),allocatable :: tokens(:)
!       integer              :: i
!          string = '  first,second ,third       '
!          call tokenize(string, set=';,', tokens=tokens )
!          write(*,brackets)ch(tokens)
! 
!          string = '  first , second ,third       '
!          call tokenize(string, set=' ,', tokens=tokens )
!          write(*,brackets)(tokens(i)%character(),i=1,size(tokens))
!          ! remove blank tokens
!          tokens=pack(tokens, tokens /= '' )
!          write(*,brackets)ch(tokens)
!    !
!       endblock
!    !
!    ! Execution of BOUNDS form (return position of tokens)
!    !
!       block
!       type(ut)                   :: string
!       character(len=*),parameter :: set = " ,"
!       integer,allocatable        :: first(:), last(:)
!          write(*,gen)repeat('1234567890',6)
!          string = 'first,second,,fourth'
!          write(*,gen)ch(string)
!          call tokenize (string, set, first, last)
!          write(*,a_commas)'FIRST=',first
!          write(*,a_commas)'LAST=',last
!          write(*,a_commas)'HAS LENGTH=',last-first.gt.0
!       endblock
!    !
!    end program demo_tokenize
! 
!   Results:
! 
!    > [  first     ],[second      ],[third       ]
!    > [],[first],[],[],[second],[],[third],[],[],[],[],[]
!    > [first ],[second],[third ]
!    > 123456789012345678901234567890123456789012345678901234567890
!    > first,second,,fourth
!    > FIRST=1,7,14,15
!    > LAST=5,12,13,20
!    > HAS LENGTH=T,T,F,T
! 
! SEE ALSO
!   +  SPLIT(3) ‐ return tokens from a string, one at a time
! 
!   +  INDEX(3) ‐ Position of a substring within a string
! 
!   +  SCAN(3) ‐ Scan a string for the presence of a set of characters
! 
!   +  VERIFY(3) ‐ Position of a character in a string of characters
!                  that does not appear in a given set of characters.
! 
! AUTHOR
!     Milan Curcic, "milancurcic@hey.com"
!     John S. Urban -- UTF-8 version
! 
! LICENSE
!     MIT
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
! AUTHOR   : Milan Curcic, "milancurcic@hey.com"
! LICENSE  : MIT
! VERSION  : version 0.1.0, copyright 2020, Milan Curcic
! MODIFIED : 2025-10-15 UTF-8 version, urbanjost

    call split_first_last(string, set, first, last)
    ! maxval() of a zero-size array is set to a flag value not zero or length of character string
    if(size(first).eq.0)then
       imax=0
    else
       imax=maxval(last-first)+1
    endif
    if(allocated(tokens))deallocate(tokens)
    allocate(tokens(size(first)))
    !
    do n = 1,size(tokens)
      tokens(n) = string%character(first(n),last(n),1)
    enddo
    !
    if (present(separator)) then
      if(allocated(separator))deallocate(separator)
      allocate(separator(size(tokens) - 1))
      do n = 1,size(tokens) - 1
        separator(n) = string%character(first(n+1)-1,first(n+1)-1,1)
      enddo
    endif

end subroutine split_tokens
!===================================================================================================================================
impure subroutine split_tokens_uauu(string, set, tokens, separator)
! Splits a string into tokens using characters in set as token delimiters.
! If present, separator contains the array of token delimiters.
type(unicode_type),intent(in)                       :: string
character(len=*),intent(in)                         :: set
type(unicode_type),allocatable,intent(out)          :: tokens(:)
type(unicode_type),allocatable,intent(out),optional :: separator(:)
   call split_tokens(string,unicode_type(set),tokens,separator)
end subroutine split_tokens_uauu
!===================================================================================================================================
impure subroutine split_first_last(string, set, first, last)
! Computes the first and last indices of tokens in input string, delimited
! by the characters in set, and stores them into first and last output
! arrays.
type(unicode_type), intent(in)         :: string
type(unicode_type), intent(in)         :: set
integer, allocatable, intent(out)      :: first(:)
integer, allocatable, intent(out)      :: last(:)

type(unicode_type)                     :: set_array(size(set%codes))
logical, dimension(size(string%codes)) :: is_first, is_last, is_separator
integer                                :: i
integer                                :: n
integer                                :: slen
! AUTHOR   : Milan Curcic, "milancurcic@hey.com"
! LICENSE  : MIT
! VERSION  : version 0.1.0, copyright 2020, Milan Curcic
! MODIFIED : 2025-09-21 JSU
    !
    slen = len(string)
    !
    do n = 1,len(set)
      set_array(n) = set%character(n,n)
    enddo
    !
    FINDIT: do n = 1,slen
      do i=1,len(set)
         is_separator(n)=.false.
         if( string%character(n,n) == set_array(i)%character() )then
            is_separator(n) = .true.
            exit
         endif
      enddo
    enddo FINDIT
    !
    is_first = .false.
    is_last = .false.
    !
    if (.not. is_separator(1)) is_first(1) = .true.
    !
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
    !
    if (.not. is_separator(slen)) is_last(slen) = .true.
    !
    first = pack([(n, n = 1, slen)], is_first)
    last = pack([(n, n = 1, slen)], is_last)
    !
  end subroutine split_first_last
!===================================================================================================================================
impure subroutine split_first_last_uaii(string, set, first, last)
type(unicode_type),intent(in)            :: string
character(len=*),intent(in)              :: set
integer,allocatable,intent(out)          :: first(:)
integer,allocatable,intent(out),optional :: last(:)
   call split_first_last(string,unicode_type(set),first,last)
end subroutine split_first_last_uaii
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
type(unicode_type)             :: set_array(size(set%codes))
integer                        :: i
integer                        :: result_pos
integer                        :: n
! AUTHOR   : Milan Curcic, "milancurcic@hey.com"
! LICENSE  : MIT
! VERSION  : version 0.1.0, copyright 2020, Milan Curcic
! MODIFIED : 2025-09-21 JSU

    backward = .false.
    if (present(back)) backward = back
    !
    do n = 1,len(set)
      set_array(n) = set%character(n,n)
    enddo
    !
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
    !
    pos = result_pos
    !
end subroutine split_pos
!===================================================================================================================================
impure subroutine split_pos_uail(string, set, pos, back)
type(unicode_type),intent(in) :: string
character(len=*),intent(in)   :: set
integer,intent(in out)        :: pos
logical,intent(in),optional   :: back
   call split_pos(string,unicode_type(set),pos,back)
end subroutine split_pos_uail
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    PAD(3f) - [M_unicode:WHITESPACE] return string padded to at least
!    specified length
!    (LICENSE:MIT)
! 
! SYNOPSIS
! 
!    function pad(str,length,pattern,right,clip) result(out)
! 
!     type(unicode_type)                         :: str
!     integer,intent(in)                         :: length
!     type(unicode_type)                         :: out
!     type(unicode_type),intent(in),optional     :: pattern
!     logical,intent(in),optional                :: right
!     logical,intent(in),optional                :: clip
! 
! DESCRIPTION
!    pad(3f) pads a string with a pattern to at least the specified
!    length. If the trimmed input string is longer than the requested
!    length the trimmed string is returned.
! 
! OPTIONS
!    str      the input string to return trimmed, but then padded to
!             the specified length if shorter than length
!    length   The minimum string length to return
!    pattern  optional string to use as padding. Defaults to a space.
!    right    if true pads string on the right, else on the left. Defaults
!             to true.
!    clip     trim spaces from input string ends. Defaults to .true.
! 
! RETURNS
!    out  The input string padded to the requested length or
!         the trimmed input string if the input string is
!         longer than the requested length.
! 
! EXAMPLES
! 
!  Sample Program:
! 
!   program demo_pad
!   use M_unicode, only  : pad, assignment(=)
!   !use M_unicode, only : write(formatted)
!   use M_unicode, only  : len
!   use M_unicode, only  : ch=> character
!   use M_unicode, only  : ut=> unicode_type
!   implicit none
!   type(ut)                   :: string
!   type(ut)                   :: answer
!   integer                    :: i
!   !character(len=*),parameter :: u='(*(DT))'
!   character(len=*),parameter :: u='(*(g0))'
!     !
!     string='abcdefghij'
!     !
!     write(*,*)'pad on right till 20 characters long'
!     answer=pad(string,20)
!     write(*,'("[",g0,"]",/)') answer%character()
!     !
!     write(*,*)'original is not trimmed for short length requests'
!     answer=pad(string,5)
!     write(*,'("[",g0,"]",/)') answer%character()
!     !
!     i=30
!     write(*,*)'pad with specified string and left-justified integers'
!     write(*,'(1x,g0,1x,i0)') &
!      & ch(pad(ut('CHAPTER 1 : The beginning '),i,ut('.') )), 1   , &
!      & ch(pad(ut('CHAPTER 2 : The end '),i,ut('.') )),       1234, &
!      & ch(pad(ut('APPENDIX '),i,ut('.') )),                  1235
!     !
!     write(*,*)'pad with specified string and right-justified integers'
!     write(*,'(1x,g0,i7)') &
!      & ch(pad(ut('CHAPTER 1 : The beginning '),i,ut('.') )), 1   , &
!      & ch(pad(ut('CHAPTER 2 : The end '),i,ut('.') )),       1234, &
!      & ch(pad(ut('APPENDIX '),i,ut('.') )),                  1235
!     !
!     write(*,*)'pad on left with zeros'
!     write(*,u)ch(pad(ut('12'),5,ut('0'),right=.false.))
!     !
!     write(*,*)'various lengths with clip .true. and .false.'
!     write(*,u)ch(pad(ut('12345 '),30,ut('_'),right=.false.))
!     write(*,u)ch(pad(ut('12345 '),30,ut('_'),right=.false.,clip=.true.))
!     write(*,u)ch(pad(ut('12345 '), 7,ut('_'),right=.false.))
!     write(*,u)ch(pad(ut('12345 '), 7,ut('_'),right=.false.,clip=.true.))
!     write(*,u)ch(pad(ut('12345 '), 6,ut('_'),right=.false.))
!     write(*,u)ch(pad(ut('12345 '), 6,ut('_'),right=.false.,clip=.true.))
!     write(*,u)ch(pad(ut('12345 '), 5,ut('_'),right=.false.))
!     write(*,u)ch(pad(ut('12345 '), 5,ut('_'),right=.false.,clip=.true.))
!     write(*,u)ch(pad(ut('12345 '), 4,ut('_'),right=.false.))
!     write(*,u)ch(pad(ut('12345 '), 4,ut('_'),right=.false.,clip=.true.))
!  end program demo_pad
! 
!   Results:
! 
!    >  pad on right till 20 characters long
!    > [abcdefghij          ]
!    >
!    >  original is not trimmed for short length requests
!    > [abcdefghij]
!    >
!    >  pad with specified string and left-justified integers
!    >  CHAPTER 1 : The beginning .... 1
!    >  CHAPTER 2 : The end .......... 1234
!    >  APPENDIX ..................... 1235
!    >  pad with specified string and right-justified integers
!    >  CHAPTER 1 : The beginning ....      1
!    >  CHAPTER 2 : The end ..........   1234
!    >  APPENDIX .....................   1235
!    >  pad on left with zeros
!    > 00012
!    >  various lengths with clip .true. and .false.
!    > ________________________12345
!    > _________________________12345
!    > _12345
!    > __12345
!    > 12345
!    > _12345
!    > 12345
!    > 12345
!    > 12345
!    > 2345
! 
! SEE ALSO
!      adjustl(3f), adjustr(3f), repeat(3f), trim(3f), len_trim(3f), len(3f)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
!===================================================================================================================================
impure elemental function pad(line,length,pattern,right,clip) result(out)

! ident_10="@(#) M_unicode pad(3f) return string padded to at least specified length"

type(unicode_type),intent(in)          :: line
integer,intent(in)                     :: length
type(unicode_type),intent(in),optional :: pattern
logical,optional,intent(in)            :: right
logical,optional,intent(in)            :: clip
type(unicode_type)                     :: out
type(unicode_type)                     :: temp
logical                                :: local_right
logical                                :: local_clip
type(unicode_type)                     :: local_pattern
type(unicode_type)                     :: local_line
integer                                :: newlen

if(  present(right)    )then;  local_right=right;      else;  local_right=.true.;  endif
if(  present(clip)     )then;  local_clip=clip;        else;  local_clip=.true. ;  endif
if(  present(pattern)  )then;  local_pattern=pattern;  else;  local_pattern=' ' ;  endif

if(len(local_pattern) == 0)then
   out=line
else

   if(local_clip)then
      local_line=trim(adjustl(line))
      newlen=max(length,len(local_line))
   else
      local_line=line
      newlen=max( length,len(line) )
   endif

   if(local_right)then
      !out=[local_line//repeat(local_pattern,newlen/len(local_pattern)+1)
      temp=repeat(local_pattern,newlen/len(local_pattern)+1)
      out=[local_line%codes,temp%codes]
   else
      ! make a line of pattern
      out=repeat(local_pattern, ceiling(real(newlen)/len(local_pattern)))

      !out=out%sub(1,newlen-len(local_line))//local_line
      out=out%sub(1,newlen-len(local_line))
      out=[out%codes,local_line%codes]
   endif

   out=out%sub(1,newlen)

endif
end function pad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!   SCAN(3f) - [M_unicode:SEARCH] Scan a string for the presence of a
!   set of characters
!   (LICENSE:MIT)
! 
! SYNOPSIS
!   result = scan( string, set, [,back] )
! 
!    elemental integer(kind=KIND) function scan(string,set,back)
! 
!     type(unicode_type),intent(in) :: string
! 
!     type(unicode_type),intent(in) :: set
!        or
!     character(len=*),intent(in)   :: set
! 
!     logical,intent(in),optional   :: back
! 
! CHARACTERISTICS
!   +  STRING is a string of type unicode_type
! 
!   +  SET must be a string of type unicode_type or character
! 
!   +  BACK is a logical of default kind
! 
!   +  the result is an integer of default kind.
! 
! DESCRIPTION
!   SCAN(3) scans a STRING for any of the characters in a SET of characters.
! 
!   If BACK is either absent or equals .false., this function returns the
!   position of the leftmost character of STRING that is in SET. If BACK
!   equals .true., the rightmost position is returned. If no character of
!   SET is found in STRING, the result is zero.
! 
! OPTIONS
!   +  STRING : the string to be scanned
! 
!   +  SET : the set of characters which will be matched
! 
!   +  BACK : if .true. the position of the rightmost character matched
!      is returned, instead of the leftmost.
! 
! RESULT
!   If BACK is absent or is present with the value false and if STRING
!   contains at least one character that is in SET, the value of the result
!   is the position of the leftmost character of STRING that is in SET.
! 
!   If BACK is present with the value true and if STRING contains at least
!   one character that is in SET, the value of the result is the position
!   of the rightmost character of STRING that is in SET.
! 
!   The value of the result is zero if no character of STRING is in SET
!   or if the length of STRING or SET is zero.
! 
! EXAMPLES
!   Sample program:
! 
!    program demo_scan
!    use iso_fortran_env, only : stdout => output_unit
!    use M_unicode,       only : scan, unicode_type, assignment(=)
!    use M_unicode,       only : ut=>unicode_type
!    implicit none
!    character(len=*),parameter :: g='(*(g0,1x))'
!    type(ut)                   :: line
!    type(ut)                   :: set
!       !
!       write(*,*) scan("fortran", "ao")          ! 2, found ’o’
!       write(*,*) scan("fortran", "ao", .true.)  ! 6, found ’a’
!       write(*,*) scan("fortran", "c++")         ! 0, found none
!       !
!       line='parsley😃sage😃rosemary😃😃thyme'
!       set='😃'
!       write(stdout,g) '12345678901234567890123456789012345678901234567890'
!       write(stdout,g) line%character()
!       write(stdout,g) scan(line, set)
!       write(stdout,g) scan(line, set, back=.true.)
!       write(stdout,g) scan(line, set, back=.false.)
!       write(stdout,g) scan(line, unicode_type("NOT"))
!       write(stdout,g) 'OOP'
!       write(stdout,g) line%scan(set)
!       write(stdout,g) line%scan(ut("o"))
!    end program demo_scan
! 
!   Results:
! 
!     >            2
!     >            6
!     >            0
!     > 12345678901234567890123456789012345678901234567890
!     > parsley😃sage😃rosemary😃😃thyme
!     > 8
!     > 23
!     > 8
!     > 0
!     > OOP
!     > 8
!     > 15
! 
! SEE ALSO
!   Functions that perform operations on character strings, return lengths
!   of arguments, and search for certain arguments:
! 
!   +  ADJUSTL(3), ADJUSTR(3), INDEX(3), VERIFY(3)
! 
!   +  LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
elemental pure function scan_uu(string,set,back) result(pos)

! ident_11="@(#) M_unicode scan(3f) Scan a string for the presence of a set of characters"

type(unicode_type),intent(in) :: string
type(unicode_type),intent(in) :: set
logical,intent(in),optional   :: back
logical                       :: back_local
integer                       :: pos
integer                       :: i
   back_local=.false.
   if(present(back))back_local=back
   pos=0
   if(back_local)then
      pos = maxval( [ (findloc(string%codes, set%codes(i), dim=1, back=back_local) ,i=1,size(set%codes) )])
   else
      pos = minval( [ (findloc(string%codes, set%codes(i), dim=1, back=back_local), i=1,size(set%codes) )])
   endif

end function scan_uu
!===================================================================================================================================
elemental pure function scan_ua(string,set,back) result(pos)
! allow SET to be CHARACTER and not just TYPE(UNICODE_TYPE)
type(unicode_type),intent(in) :: string
character(len=*),intent(in)   :: set
type(unicode_type)            :: set_u
logical,intent(in),optional   :: back
integer                       :: pos
   set_u=set
   pos = scan_uu(string,set_u,back)
end function scan_ua
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! 
! NAME
!   VERIFY(3f) - [M_unicode:SEARCH] Position of a character in a string of
!   characters that does not appear in a given set of characters.
!   (LICENSE:MIT)
! 
! SYNOPSIS
!   result = verify(string, set [,back] [,kind] )
! 
!            elemental integer function verify(string,set,back,KIND)
! 
!             type(unicode_type),intent(in) :: string
! 
!             type(unicode_type),intent(in) :: set
!                or
!             character(len=*),intent(in)   :: set
! 
!             logical,intent(in),optional   :: back
! 
! CHARACTERISTICS
! 
!   +  STRING  must be of type string
!   +  SET  must be of type string or character.
!   +  BACK shall be of type logical.
!   +  A default integer kind is returned.
! 
! DESCRIPTION
!   VERIFY(3) verifies that all the characters in STRING belong to the set of
!   characters in SET by identifying the position of the first character in the
!   string that is not in the set.
! 
!   This makes it easy to verify strings are all uppercase or lowercase, follow a
!   basic syntax, only contain printable characters, and many of the conditions
!   tested for with the C routines ISALNUM(3c), ISALPHA(3c), ISASCII(3c),
!   ISBLANK(3c), ISCNTRL(3c), ISDIGIT(3c), ISGRAPH(3c), ISLOWER(3c), ISPRINT(3c),
!   ISPUNCT(3c), ISSPACE(3c), ISUPPER(3c), and ISXDIGIT(3c); but for a string as
!   well as an array of strings.
! 
! OPTIONS
!   +  STRING : The string to search in for an unmatched character.
! 
!   +  SET : The set of characters that must be matched.
! 
!   +  BACK : The direction to look for an unmatched character. The left‐most
!      unmatched character position isreturned unless BACK is present and
!      .false., which causes the position of the right‐most unmatched character
!      to be returned instead of the left‐most unmatched character.
! 
! RESULT
!   If all characters of STRING are found in SET, the result is zero.
! 
!   If STRING is of zero length a zero (0) is always returned.
! 
!   Otherwise, if an unmatched character is found The position of the first or
!   last (if BACK is .false.) unmatched character in STRING is returned, starting
!   with position one on the left end of the string.
! 
! EXAMPLES
!   Sample program I:
! 
!    program demo_verify
!    ! general examples
!    use M_unicode, only : assignment(=)
!    use M_unicode, only : ut=>unicode_type, ch=>character
!    use M_unicode, only : write(formatted)
!    use M_unicode, only : operator(==)
!    use M_unicode, only : verify, replace
!    use M_unicode, only : operator(//)
!    implicit none
!    ! some useful character sets
!    character,parameter          :: &
!     & int*(*)   = "1234567890", &
!     & low*(*)   = "abcdefghijklmnopqrstuvwxyz", &
!     & upp*(*)   = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
!     & punc*(*)  = "!""#$%&'()*+,‐./:;<=>?@[\]'_‘{|}˜", &
!     & blank*(*) = " ", &
!     & tab       = char(11), &
!     & prnt*(*) = int//low//upp//blank//punc
!    !
!    type(ut)                     :: stru
!    integer                      :: i
!        print *, "basics:"
!        print *, VERIFY ("ABBA", "A")                ! has the value 2.
!        print *, VERIFY ("ABBA", "A", BACK = .TRUE.) ! has the value 3.
!        print *, VERIFY ("ABBA", "AB")               ! has the value 0.
!       !
!       print *,"find first non‐uppercase letter"
!       ! will produce the location of "d", because there is no match in UPP
!       write(*,*) "something unmatched",verify(ut("ABCdEFG"), upp)
!       !
!       print *,"if everything is matched return zero"
!       ! will produce 0 as all letters have a match
!       write(*,*) &
!       & "everything matched",verify(ut("ffoorrttrraann"), "nartrof")
!       !
!       print *,"easily categorize strings as uppercase, lowercase, ..."
!       ! C-like functionality but does entire strings not just characters
!       write(*,*)"isdigit 123?",verify(ut("123"), int) == 0
!       write(*,*)"islower abc?",verify(ut("abc"), low) == 0
!       write(*,*)"isalpha aBc?",verify(ut("aBc"), low//upp) == 0
!       write(*,*)"isblank aBc dEf?",verify(ut("aBc dEf"), blank//tab ) /= 0
!       ! check if all printable characters
!       stru="aB;cde,fgHI!Jklmno PQRSTU vwxyz"
!       write(*,*)"isprint?",verify(stru,prnt) == 0
!       !
!       ! this now has a nonprintable tab character in it
!       stru=replace(stru,10,10,ut(char(11)))
!       write(*,*)"isprint?",verify(stru,prnt) == 0
!       !
!       print *,"VERIFY(3) is very powerful using expressions as masks"
!       ! verify(3) is often used in a logical expression
!       stru=" This is NOT all UPPERCASE "
!       write(*,*)"all uppercase/spaces?",verify(stru, blank//upp) == 0
!       stru=" This IS all uppercase "
!       write(*,*) "stru=["//stru//"]"
!       write(*,*)"all uppercase/spaces?",verify(stru, blank//upp) == 0
!       !
!       ! set and show complex stru to be tested
!       stru="  Check this out. Let me know  "
!       ! show the stru being examined
!       write(*,*) "stru=["//stru//"]"
!       write(*,*) "        "//repeat(int,4) ! number line
!       !
!       ! function returns a position just not a logical like C
!       print *, "returning a position not just a logical is useful"
!       ! which can be very useful for parsing strings
!       write(*,*)"first non‐blank character",verify(stru, blank)
!       write(*,*)"last non‐blank character",verify(stru, blank,back=.true.)
!       write(*,*)"first non‐letter non‐blank",verify(stru,low//upp//blank)
!       !
!      !VERIFY(3) is elemental (can check an array of strings in one call)
!       print *, "elemental"
!       ! are strings all letters (or blanks)?
!       write(*,*) "array of strings",verify( &
!       ! strings must all be same length, so force to length 10
!       & [character(len=10) :: "YES","ok","000","good one","Nope!"], &
!       & low//upp//blank) == 0
!       !
!       ! rarer, but the set can be an array, not just the strings to test
!       ! you could do ISPRINT() this (harder) way :>
!       write(*,*)"isprint?", &
!       & .not.all(verify(ut("aBc"), [(char(i),i=32,126)])==1)
!       ! instead of this way
!       write(*,*)"isprint?",verify(ut("aBc"),prnt) == 0
!       !
!    end program demo_verify
! 
!   Results:
! 
!        >  basics:
!        >            2
!        >            3
!        >            0
!        >  find first non‐uppercase letter
!        >  something unmatched           4
!        >  if everything is matched return zero
!        >  everything matched           0
!        >  easily categorize strings as uppercase, lowercase, ...
!        >  isdigit 123? T
!        >  islower abc? T
!        >  isalpha aBc? T
!        >  isblank aBc dEf? T
!        >  isprint? T
!        >  isprint? F
!        >  VERIFY(3) is very powerful using expressions as masks
!        >  all uppercase/spaces? F
!        >  string=[ This IS all uppercase ]
!        >  all uppercase/spaces? F
!        >  string=[  Check this out. Let me know  ]
!        >          1234567890123456789012345678901234567890
!        >  returning a position not just a logical is useful
!        >  first non‐blank character           3
!        >  last non‐blank character          29
!        >  first non‐letter non‐blank          17
!        >  elemental
!        >  array of strings T T F T F
!        >  isprint? T
!        >  isprint? T
! 
!   Sample program II:
! 
!   Determine if strings are valid integer representations
! 
!    program fortran_ints
!    use M_unicode, only : ut=>unicode_type,assignment(=)
!    use M_unicode, only : adjustr, verify, trim, len
!    use M_unicode, only : write(formatted)
!    use M_unicode, only : operator(.cat.)
!    use M_unicode, only : operator(==)
!    implicit none
!    integer :: i
!    character(len=*),parameter :: asciiints(*)=[character(len=10) :: &
!     "+1 ", &
!     "3044848 ", &
!     "30.40 ", &
!     "September ", &
!     "1 2 3", &
!     "  -3000 ", &
!     " "]
!     type(ut),allocatable :: ints(:)
!     if(allocated(ints))deallocate(ints)
!     allocate(ints(size(asciiints))) ! gfortran bug
!     ints=asciiints
!     ints=trim(ints)
!     ! show if strings pass or fail the test done by isint(3)
!     write(*,"('is integer?')")
!     do i=1,size(ints)
!       write(*,'("|",DT,T14,"|",l1,"|")') ints(i), isint(ints(i))
!     enddo
!     ! elemental
!     write(*,"(*(g0,1x))") isint(ints)
! 
!    contains
! 
!    impure elemental function isint(line) result (lout)
!    use M_unicode, only : adjustl, verify, trim
!    !
!    ! determine if string is a valid integer representation
!    ! ignoring trailing spaces and leading spaces
!    !
!    character(len=*),parameter :: digits="0123456789"
!    type(ut),intent(in)        :: line
!    type(ut)                   :: name
!    logical                    :: lout
!       lout=.false.
!       ! make sure at least two characters long to simplify tests
!       name=adjustl(line).cat.'  '
!       ! blank string
!       if( name == '' )return
!       ! allow one leading sign
!       if( verify(name%sub(1,1),ut('+‐-')) == 0 ) name=name%sub(2,len(name))
!       ! was just a sign
!       if( name == '' )return
!       lout=verify(trim(name), digits)  == 0
!    end function isint
! 
!    end program fortran_ints
! 
!   Results:
! 
!     > is integer?
!     > |+1          |T|
!     > |3044848     |T|
!     > |30.40       |F|
!     > |September   |F|
!     > |1 2 3       |F|
!     > |  ‐3000     |T|
!     > |            |F|
!     > T T F F F T F
! 
!   Sample program III:
! 
!   Determine if strings represent valid Fortran symbol names
! 
!    program fortran_symbol_name
!    use M_unicode, only : ut=>unicode_type, trim, verify, len
!    use M_unicode, only : ch=>character
!    use M_unicode, only : write(formatted)
!    implicit none
!    integer :: i
!    type(ut),allocatable :: symbols(:)
!       symbols=[ &
!        ut('A_'), ut('10'), ut('a10'), ut('September'), ut('A B'), &
!        ut('_A'), ut(' ')]
! 
!       do i=1,size(symbols)
!          write(*,'(1x,DT,T11,"|",l2)')symbols(i),fortran_name(symbols(i))
!       enddo
! 
!    contains
! 
!    impure elemental function fortran_name(line) result (lout)
!    !
!    ! determine if a string is a valid Fortran name
!    ! ignoring trailing spaces (but not leading spaces)
!    !
!    character(len=*),parameter :: int="0123456789"
!    character(len=*),parameter :: lower="abcdefghijklmnopqrstuvwxyz"
!    character(len=*),parameter :: upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
!    character(len=*),parameter :: allowed=upper//lower//int//"_"
! 
!    type(ut),intent(in)        :: line
!    type(ut)                   :: name
!    logical                    :: lout
!       name=trim(line)
!       if(len(name).ne.0)then
!          ! first character is alphameric
!          lout = verify(name%sub(1,1), lower//upper) == 0  &
!           ! other characters are allowed in a symbol name
!           & .and. verify(name,allowed) == 0           &
!           ! allowable length
!           & .and. len(name) <= 63
!       else
!          lout = .false.
!       endif
!    end function fortran_name
! 
!    end program fortran_symbol_name
! 
!   Results:
! 
!    >  A_       | T
!    >  10       | F
!    >  a10      | T
!    >  September| T
!    >  A B      | F
!    >  _A       | F
!    >           | F
! 
!   Sample program IV:
! 
!   check if string is of form NN‐HHHHH
! 
!    program form
!    !
!    ! check if string is of form NN‐HHHHH
!    !
!    use iso_fortran_env, only : stdout => output_unit
!    use M_unicode,       only : verify, unicode_type, assignment(=)
!    use M_unicode,       only : ut=>unicode_type
!    implicit none
!    character(len=*),parameter :: g='(*(g0,1x))'
!    !
!    character(len=*),parameter :: int='1234567890'
!    character(len=*),parameter :: hex='abcdefABCDEF0123456789'
!    logical                    :: lout
!    type(unicode_type)         :: chars
!    type(unicode_type)         :: str
!       !
!       chars='32‐af43d'
!       lout=.true.
!       !
!       ! are the first two characters integer characters?
!       str = chars%character(1,2)
!       lout = (verify( str, ut(int) ) == 0) .and.lout
!       !
!       ! is the third character a dash?
!       str = chars%character(3,3)
!       lout = (verify( str, ut('‐-') ) == 0) .and.lout
!       !
!       ! is remaining string a valid representation of a hex value?
!       str = chars%character(4,8)
!       lout = (verify( str, ut(hex) ) == 0) .and.lout
!       !
!       if(lout)then
!          write(stdout,g)trim(chars%character()),' passed'
!       else
!          write(stdout,g)trim(chars%character()),' failed'
!       endif
!    end program form
! 
!   Results:
! 
!           32‐af43d passed
! 
!   Sample program V:
! 
!   exploring uses of elemental functionality and dusty corners
! 
!    program more_verify
!    use M_unicode, only : ut=>unicode_type, verify
!    use M_unicode, only : assignment(=)
!    use M_unicode, only : ch=>character
!    implicit none
!    character(len=*),parameter :: &
!      & low="abcdefghijklmnopqrstuvwxyz", &
!      & upp="ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
!      & blank=" "
!    ! note character variables in an array have to be of the same length
!    type(ut),allocatable :: strings(:)
!    type(ut),allocatable :: sets(:)
! 
!       strings=[ut("Go"),ut("right"),ut("home!")]
!       sets=[ut("do"),ut("re"),ut("me")]
! 
!      ! elemental ‐‐ you can use arrays for both strings and for sets
! 
!       ! check each string from right to left for non‐letter/non‐blank
!       write(*,*)"last non‐letter",verify(strings,upp//low//blank,back=.true.)
! 
!       ! even BACK can be an array
!       ! find last non‐uppercase character in "Go"
!       ! and first non‐lowercase in "right"
!       write(*,*) verify(strings(1:2),[upp,low],back=[.true.,.false.])
! 
!       ! using a null string for a set is not well defined. Avoid it
!       write(*,*) "null",verify("for tran ", "", .true.) ! 8,length of string?
!       ! probably what you expected
!       write(*,*) "blank",verify("for tran ", " ", .true.) ! 7,found ’n’
! 
!       ! first character in  "Go    " not in "do",
!       ! and first letter in "right " not in "ri"
!       ! and first letter in "home! " not in "me"
!       write(*,*) verify(strings,sets)
! 
!    end program more_verify
! 
!   Results:
! 
!    >  last non‐letter 0 0 5
!    >  2 0
!    >  null 9
!    >  blank 8
!    >  1 2 1
! 
! SEE ALSO
!   Functions that perform operations on character strings, return
!   lengths of arguments, and search for certain arguments:
! 
!   +  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3),
! 
!   +  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
!
elemental impure function verify_uu(string,set,back) result(result)

! ident_12="@(#) M_unicode verify(3f) determine position of a character in a string that does not appear in a given set of characters."

type(unicode_type),intent(in) :: string
type(unicode_type),intent(in) :: set
type(unicode_type)            :: str
logical,intent(in),optional   :: back
integer                       :: result
integer                       :: pos
integer                       :: i
   result=0
   do i=1,len(string)
      str=string%sub(i,i)
      pos=index(set,str,back)
      if(pos.eq.0)then
         result=i
         exit
      endif
   enddo
end function verify_uu
!===================================================================================================================================
elemental impure function verify_ua(string,set,back) result(result)
type(unicode_type),intent(in) :: string
character(len=*),intent(in)   :: set
type(unicode_type)            :: set_u
logical,intent(in),optional   :: back
integer                       :: result
   set_u=set
   result=verify_uu(string,set_u,back)
end function verify_ua
!===================================================================================================================================
elemental impure function verify_au(string,set,back) result(result)
character(len=*),intent(in)   :: string
type(unicode_type),intent(in) :: set
logical,intent(in),optional   :: back
type(unicode_type)            :: ustring
integer                       :: result
   ustring=string
   result=verify_uu(ustring,set,back)
end function verify_au
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     EXPANDTABS(3f) - [M_unicode:WHITESPACE] function to expand tab characters
!     (LICENSE:MIT)
! 
! SYNOPSIS
! 
!     elemental function expandtabs(INSTR,TABSIZE) result(OUT)
! 
!      type(unicode_type),intent=(in)  :: INSTR
!      integer,intent(in),optional     :: TAB_SIZE
!      type(unicode_type)              :: OUT
! 
! DESCRIPTION
!    EXPANDTABS(3) expands tabs in INSTR to spaces in OUT. It assumes a
!    tab is set every 8 characters by default. Trailing spaces are removed.
! 
! OPTIONS
!    instr     Input line to remove tabs from
!    tab_size  spacing between tab stops.
! 
! RETURNS
!    out       Output string with tabs expanded.
! 
! EXAMPLES
! 
!  Sample program:
! 
!     program demo_expandtabs
!     use M_unicode, only : expandtabs, ch=>character, replace
!     use M_unicode, only : assignment(=), ut=> unicode_type
!     implicit none
!     type(ut)                     :: in
!     type(ut)                     :: inexpanded
!     character(len=:),allocatable :: dat
!     integer                      :: i
!        dat='  this is my string  '
!        ! change spaces to tabs to make a sample input
!        do i=1,len(dat)
!           if(dat(i:i) == ' ')dat(i:i)=char(9)
!        enddo
!        in=dat
!        !
!        inexpanded=expandtabs(in)
!        write(*,'("[",a,"]")')ch(inexpanded)
!        inexpanded=replace(inexpanded,ut(' '),ut('_'))
!        write(*,'("[",a,"]")')ch(inexpanded)
!        !
!        write(*,'("[",a,"]")')ch(in%expandtabs())
!        write(*,'("[",a,"]")')ch(in%expandtabs(tab_size=8))
!        write(*,'("[",a,"]")')ch(in%expandtabs(tab_size=1))
!        write(*,'("[",a,"]")')ch(in%expandtabs(tab_size=0))
!        !
!     end program demo_expandtabs
! 
!    Results:
! 
!     > [                this    is      my      string]
!     > [________________this____is______my______string]
!     > [                this    is      my      string]
!     > [                this    is      my      string]
!     > [  this is my string]
!     > [thisismystring]
! 
! AUTHOR
!      John S. Urban
! 
! LICENSE
!     MIT
elemental function expandtabs(instr,tab_size) result(out)

! ident_13="@(#) M_unicode expandtabs(3f) convert tabs to spaces and trim line removing CRLF chars"

type(unicode_type),intent(in) :: instr     ! input line to scan for tab characters
type(unicode_type)            :: out       ! tab-expanded version of INSTR produced
integer,intent(in),optional   :: tab_size
integer                       :: ipos      ! position in OUT to put next character of INSTR
integer                       :: istep     ! counter advances thru string INSTR
integer                       :: icount    ! number of tab characters in input
integer                       :: i
integer                       :: tab_size_local
   tab_size_local=8                        ! assume a tab stop is set every 8th column
   if(present(tab_size))tab_size_local=tab_size
   ! count number of tab characters in input
   icount=0
   do i=1,size(instr%codes)
      if(instr%codes(i)==9)icount=icount+1
   enddo
   ! initially set length of output to the maxiumum length that might result
   if(allocated(out%codes))deallocate(out%codes)
   allocate( out%codes(size(instr%codes)+8*icount) )
   out%codes=32                         ! blank-fill string
   ipos=1                                  ! where to put next character in output string OUT
   SCAN_LINE: do istep=1,len_trim(instr)   ! look through input string one character at a time
      EXPAND_TABS : select case (instr%codes(istep)) ! take actions based on character found
      case(9)        ! character is a horizontal tab so move pointer out to appropriate column
         if(tab_size_local.gt.0)then
            ipos = ipos + (tab_size_local - (mod(ipos-1,tab_size_local)))
         endif
      case default   ! character is anything else other than a tab
         out%codes(ipos)=instr%codes(istep)
         ipos=ipos+1
      end select EXPAND_TABS
   enddo SCAN_LINE
   out=trim(out)
end function expandtabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     ESCAPE(3f) - [M_unicode:CONVERSION] expand C-like escape sequences
!     (LICENSE:MIT)
! 
! SYNOPSIS
! 
!    function escape(line,utf8) result(out)
! 
!     type(unicode_type)                    :: line
!     character(len=1),intent(in),optional  :: protect
!     type(unicode_type)                    :: out
! 
! DESCRIPTION
!    ESCAPE(3) expands commonly used escape sequences that
!    represent glyphs or control characters. By default ...
! 
!    Escape sequences
! 
!     \      backslash
!     a      alert (BEL) -- g is an alias for a
!     b      backspace
!     c      suppress further output
!     e      escape
!     f      form feed
!     n      new line
!     r      carriage return
!     t      horizontal tab
!     v      vertical tab
! 
!     oNNN   byte with octal value NNN (3 digits)
!     0-9    digits will be assumed an octal value till a
!            non-octal value character is encountered
!     dNNN   byte with decimal value NNN (3 digits)
! 
!     xHH        byte with hexadecimal value HH (2 digits);
!                h is an alias for x
!     uZZZZ      translate Unicode codepoint value to bytes
!     UZZZZZZZZ  translate Unicode codepoint value to bytes
! 
!   The default escape character is the backslash, but this may be
!   changed using the optional parameter ESCAPE.
! 
! EXAMPLES
! 
!   Sample Program:
! 
!    program demo_escape
!    ! demonstrate filter to expand C-like escape sequences in input lines
!    use iso_fortran_env, only : stdout => output_unit
!    use M_unicode,       only : ut=>unicode_type,ch=>character,len,escape
!    use M_unicode,       only : assignment(=), trim
!    implicit none
!    type(ut),allocatable  :: poem(:)
!    type(ut)              :: test(5)
!    integer               :: i
!       !
!       ! “The Crow and the Fox” by Jean de la Fontaine
!       write(stdout,'(a,/)') &
!       'Le Corbeau et le Renard -- Jean de la Fontaine'
!       !
!       poem=[&
!       ut( 'Le Corbeau et le Renard'                                   ),&
!       ut( ''                                                          ),&
!       ut( 'Ma\u00EEtre Corbeau, sur un arbre perch\u00E9,'            ),&
!       ut( 'Tenait en son bec un fromage.'                             ),&
!       ut( 'Ma\u00EEtre Renard, par l\u2019odeur all\u00E9ch\u00E9,'   ),&
!       ut( 'Lui tint \U000000E0 peu pr\U000000E8s ce langage :'        ),&
!       ut( '\U000000ABH\U000000E9 ! bonjour, Monsieur du Corbeau.'     ),&
!       ut( 'Que vous \U000000EAtes joli ! que vous me semblez beau !'  ),&
!       ut( 'Sans mentir, si votre ramage'                              ),&
!       ut( 'Se rapporte \U000000E0 votre plumage,'                     ),&
!       ut( 'Vous \xEAtes le Ph\xE9nix des h\xF4tes de ces bois.\xBB'   ),&
!       ut( 'A ces mots le Corbeau ne se sent pas de joie ;'            ),&
!       ut( 'Et pour montrer sa belle voix,'                            ),&
!       ut( 'Il ouvre un large bec, laisse tomber sa proie.'            ),&
!       ut( 'Le Renard s\u2019en saisit, et dit : \xABMon bon Monsieur,'),&
!       ut( 'Apprenez que tout flatteur'                                ),&
!       ut( 'Vit aux d\xE9pens de celui qui l\U00002019\u00E9coute :'   ),&
!       ut( 'Cette le\xE7on vaut bien un fromage, sans doute.\xBB'      ),&
!       ut( 'Le Corbeau, honteux et confus,'                            ),&
!       ut( &
!       'Jura, mais un peu tard, qu\u2019on ne l\u2019y prendrait plus.'),&
!       ut( ' -- Jean de la Fontaine')]
!       !
!       poem=escape(poem)
!       write(stdout,'(g0)')ch(poem)
!       !
!       test=[ &
!        '\e[H\e[2J           ',& ! home cursor and clear screen
!                                 ! on ANSI terminals
!        '\tABC\tabc          ',& ! write some tabs in the output
!        '\tA\a               ',& ! ring bell at end if supported
!        '\nONE\nTWO\nTHREE   ',& ! place one word per line
!        '\\                  ']
!       test=trim(escape(test))
!       write(*,'(a)')(test(i)%character(),i=1,size(test))
!       !
!    end program demo_escape
! 
!  Results (with nonprintable characters shown visible):
! 
!      > ^[[H^[[2J
!      > ^IABC^Iabc
!      > ^IA^G
!      >
!      > ONE
!      > TWO
!      > THREE
!      > \
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
impure elemental function escape_uu(line,protect) result(out)

! ident_14="@(#) M_unicode escape(3f) return string with escape sequences expanded"

type(unicode_type),intent(in)          :: line
type(unicode_type),intent(in),optional :: protect ! default is backslash
type(unicode_type)                     :: out

character(len=:),allocatable :: buffer
character(len=:),allocatable :: format
character(len=:),allocatable :: temp

integer            :: esc    ! Default is backslash
integer            :: i
integer            :: lgth
character(len=3)   :: thr
character(len=4)   :: four
character(len=8)   :: eight
integer            :: nnn
integer            :: iostat
integer            :: icount
integer,parameter  :: alert=7
integer,parameter  :: backspace=8
integer,parameter  :: horizontal_tab=9
integer,parameter  :: newline=10
integer,parameter  :: vertical_tab=11
integer,parameter  :: form_feed=12
integer,parameter  :: carriage_return=13
integer,parameter  :: eskape=27
integer,parameter  :: a=ichar('a'),AA=ichar('A'),g=ichar('g'),GG=ichar('G')
integer,parameter  :: b=ichar('b'),BB=ichar('B')
integer,parameter  :: c=ichar('c'),CC=ichar('C')
integer,parameter  :: d=ichar('d'),DD=ichar('D')
integer,parameter  :: e=ichar('e'),EE=ichar('E')
integer,parameter  :: f=ichar('f'),FF=ichar('F')
integer,parameter  :: n=ichar('n'),NN=ichar('N')
integer,parameter  :: o=ichar('o'),OO=ichar('O')
integer,parameter  :: r=ichar('r'),RR=ichar('R')
integer,parameter  :: t=ichar('t'),TT=ichar('T')
integer,parameter  :: u=ichar('u'),UU=ichar('U')
integer,parameter  :: v=ichar('v'),VV=ichar('V')
integer,parameter  :: x=ichar('x'),XX=ichar('X'),h=ichar('h'),HH=ichar('H')
   i=0 ! pointer into input

   lgth=len_trim(line)
   out=''

   if(lgth == 0)return

   if( present(protect) )then
      if(size(protect%codes).lt.1)then
         esc=0
      else
         esc=ichar(protect%sub(1,1))
      endif
   else
      esc=92
   endif

   EXP: do
      i=i+1
      if(i > lgth)exit
      if(line%codes(i) == esc)then
         i=i+1
         if(i > lgth)then  ! esc at end of line
            out%codes=[out%codes,esc]
            exit
         endif
         if(line%codes(i) /= esc)then
            BACKSLASH: select case(line%codes(i))
            case(a,AA,g,GG);out%codes=[out%codes,alert]
            case(b,BB);out%codes=[out%codes,backspace]
            case(c,CC);exit EXP                         ! suppress further output
            case(d,DD) ! %d     Dnnn decimal value
                   thr=character(line%sub(i+1,i+3))
                   read(thr,'(i3)',iostat=iostat)nnn
                   out=[out%codes,nnn]
                   i=i+3
            case(e,EE);out%codes=[out%codes, eskape ]
            case(f,FF);out%codes=[out%codes, form_feed ]
            case(n,NN);out%codes=[out%codes, newline]
            case(o,OO)
                   thr=character(line%sub(i+1,i+3))
                   read(thr,'(o3)',iostat=iostat)nnn
                   out%codes=[out%codes,nnn]
                   i=i+3
            case(r,RR);out%codes=[out%codes,carriage_return]
            case(t,TT);out%codes=[out%codes,horizontal_tab]
            case(v,VV);out%codes=[out%codes,vertical_tab]
            case(x,XX,h,HH) ! %x xHH  byte with hexadecimal value HH (1 to 2 digits)
                   thr=character(line%sub(i+1,i+2))
                   read(thr,'(z2)',iostat=iostat)nnn
                   out%codes=[out%codes,nnn]
                   i=i+2
            case(u) ! %uZZZZ  translate Unicode codepoint to bytes
                   four=character(line%sub(i+1,i+4))
                   read(four,'(z4)',iostat=iostat)nnn
                   out%codes=[out%codes,nnn]
                   i=i+4
            case(UU) ! %UZZZZZZZZ  translate Unicode codepoint to bytes
                   eight=character(line%sub(i+1,i+8))
                   read(eight,'(z8)',iostat=iostat)nnn
                   out%codes=[out%codes,nnn]
                   i=i+8
            case(ichar('0'):ichar('7'));
                buffer=line%sub(i,step=1)
                icount=verify(buffer,'01234567')
                if(icount.eq.0)icount=len(buffer)+1
                icount=icount-1
                buffer=character(line%sub(i,i+icount-1))
                temp=fmt(icount)
                format='(o'//temp//')'
                read(buffer,format,iostat=iostat)nnn
                out%codes=[out%codes,nnn]
                i=i+icount-1
            case default ! no match so just copy character, could produce warning
                out%codes=[out%codes,line%codes(i)]
            end select BACKSLASH
         else
            out%codes=[out%codes,esc] ! escape character, defaults to backslash
         endif
      else
         out%codes=[out%codes,line%codes(i)]
      endif
      if(i >= lgth)exit EXP
   enddo EXP

end function escape_uu
!===================================================================================================================================
impure elemental function escape_aa(line,protect) result(out)
character(len=*),intent(in)          :: line
character(len=1),intent(in)          :: protect
type(unicode_type)                   :: uline
type(unicode_type)                   :: uprotect
type(unicode_type)                   :: out
   uline=line
   uprotect=protect
   out=escape(uline,uprotect)
end function escape_aa
!===================================================================================================================================
impure elemental function escape_au(line,protect) result(out)
character(len=*),intent(in)            :: line
type(unicode_type),intent(in),optional :: protect
type(unicode_type)                     :: uline
type(unicode_type)                     :: out
   uline=line
   out=escape(uline,protect)
end function escape_au
!===================================================================================================================================
impure elemental function escape_ua(line,protect) result(out)
type(unicode_type),intent(in)        :: line
character(len=1),intent(in)          :: protect
type(unicode_type)                   :: uprotect
type(unicode_type)                   :: out
   uprotect=protect
   out=escape(line,uprotect)
end function escape_ua
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    SUB(3f) - [M_unicode:EDITING] Return substring
!    (LICENSE:MIT)
! 
! SYNOPSIS
! 
!    function sub(str,left,right,step) result(section)
! 
!     type(unicode_type)          :: str
!     integer,intent(in),optional :: left
!     integer,intent(in),optional :: right
!     integer,intent(in),optional :: step
! 
! DESCRIPTION
!    sub(3f) returns a substring from one column to another.
! 
! OPTIONS
!    str           the input string to return a section of
!    left          column number of str starting section of str to return.
!                  Defaults to 1 when STEP is positive, or right end of
!                  STR when STEP is negative.
!    right         column number of str ending section of str to return.
!                  Defaults to right end of STR when STEP is positive, or
!                  1 when STEP is negative.
!    step          step to take from left column to right column.
!                  Defaults to 1.
! 
! RETURNS
!    out  The specified subsection of the input string
! 
! EXAMPLES
! 
!   Sample Program:
! 
!     program demo_sub
!      use M_unicode, only : sub, assignment(=)
!      use M_unicode, only : len
!      use M_unicode, only : ut=> unicode_type
!      implicit none
!      type(ut)                   :: string
!      type(ut)                   :: piece
!         !
!         string='abcdefghij'
!         !
!         piece=sub(string,3,5)
!         call printme('selected range:')
!         piece=sub(string,6)
!         call printme('from character to end:')
!         piece=sub(string,5,5)
!         call printme('single character:')
!         piece=sub(string,step=-1)
!         call printme('reverse string:')
!      contains
!      subroutine printme(label)
!      character(len=*),intent(in) :: label
!         write(*,'(a,"[",g0,"]",/)') label, piece%character()
!      end subroutine printme
!      end program demo_sub
! 
!   Results:
! 
! SEE ALSO
!      adjustl(3f), adjustr(3f), repeat(3f), trim(3f), len_trim(3f), len(3f)
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
function sub(str,start,end,step) result(section)
type(unicode_type),intent(in) :: str
type(unicode_type)            :: section
integer,intent(in),optional   :: start, end, step
integer                       :: start_, end_, step_
integer                       :: which
integer                       :: sgn
   sgn=1
   if(present(step))sgn=sign(1,step)
   which=4*merge(1,0,present(start))+ 2*merge(1,0,present(end))+ 1*merge(1,0,present(step))
   select case(which*sgn)
   case(int(b'000'))      ; start_=1        ; end_=len(str) ; step_=1
   case(int(b'001'))      ; start_=1        ; end_=len(str) ; step_=step
   case(int(b'010'))      ; start_=1        ; end_=end      ; step_=1
   case(int(b'011'))      ; start_=1        ; end_=end      ; step_=step
   case(int(b'100'))      ; start_=start    ; end_=len(str) ; step_=1
   case(int(b'101'))      ; start_=start    ; end_=len(str) ; step_=step
   case(int(b'110'))      ; start_=start    ; end_=end      ; step_=1
   case(int(b'111'))      ; start_=start    ; end_=end      ; step_=step
   case(int(b'001')*(-1)) ; start_=len(str) ; end_=1        ; step_=step
   case(int(b'011')*(-1)) ; start_=1        ; end_=end      ; step_=step
   case(int(b'101')*(-1)) ; start_=start    ; end_=1        ; step_=step
   case(int(b'111')*(-1)) ; start_=start    ; end_=end      ; step_=step
   case default
      write(0,*)'*sub* unknown case ',which,'sign',sgn
      stop 1
   end select
   if(step_.gt.0)then
      end_=min(size(str%codes),end_)
      start_=max(1,start_)
   else
      start_=min(size(str%codes),start_)
      end_=max(1,end_)
   endif
   section=str%codes(start_:end_:step_)
end function sub
function section_uu(str,first,last,new) result(out)
type(unicode_type),intent(in) :: str
integer,intent(in),optional   :: first, last
type(unicode_type),intent(in) :: new
type(unicode_type)            :: out

integer                       :: start, end
integer                       :: which
   which=2*merge(1,0,present(first))+ 1*merge(1,0,present(last))
   select case(which)
   case(int(b'00')) ; start=1     ; end=len(str)
   case(int(b'01')) ; start=1     ; end=last
   case(int(b'10')) ; start=first ; end=len(str)
   case(int(b'11')) ; start=first ; end=last
   end select
   out=[str%codes(1:start-1),new%codes,str%codes(end+1:len(str))]
end function section_uu
!===================================================================================================================================
function section_ua(str,first,last,new) result(out)
type(unicode_type),intent(in) :: str
integer,intent(in)            :: first, last
character(len=*),intent(in)   :: new
type(unicode_type)            :: out

   out=section_uu(str,first,last,unicode_type(new))
end function section_ua
!===================================================================================================================================
function section_au(str,first,last,new) result(out)
character(len=*),intent(in)   :: str
integer,intent(in)            :: first, last
type(unicode_type),intent(in) :: new
type(unicode_type)            :: out

   out=section_uu(unicode_type(str),first,last,new)
end function section_au
!===================================================================================================================================
function section_aa(str,first,last,new) result(out)
character(len=*),intent(in) :: str
integer,intent(in)          :: first, last
character(len=*),intent(in) :: new
type(unicode_type)          :: out

   out=section_uu(unicode_type(str),first,last,unicode_type(new))
end function section_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     transliterate(3f) - [M_unicode:EDITING] replace characters from old
!                         set with new set
!     (LICENSE:PD)
! 
! SYNOPSIS
! 
!     impure function transliterate(instr,old_set,new_set) result(outstr)
! 
!      type(unicode_type),intent(in)  :: instr
!      type(unicode_type),intent(in)  :: old_set
!      type(unicode_type),intent(in)  :: new_set
!      type(unicode_type)             :: outstr
! 
! CHARACTERISTICS
! 
!    Although a conversion might occur on each call, the input values
!    may be CHARACTER as well as TYPE(UNICODE_TYPE).
! 
! DESCRIPTION
!    Translate or delete characters from an input string.
! 
! 
! OPTIONS
!   instr    input string to change
!   old_set  list of glyphs to change in INSTR if found
! 
!            Each glyph in the input string that matches a glyph
!            in the old set is replaced.
!   new_set  list of glyphs to replace glyphs in OLD_SET with.
! 
!            If NEW_SET is the empty set glyphs in INSTR that
!            match any in OLD_SET are deleted.
! 
!            If NEW_SET is shorter than OLD_SET the last glyph
!            in NEW_SET is used to replace the remaining glyphs
!            in NEW_SET.
! 
! RETURNS
!    outstr  INSTR with substitutions applied
! 
! EXAMPLES
! 
!   Sample Program:
! 
!    program demo_transliterate
! 
!     use M_unicode, only : transliterate,ut=>unicode_type
!     use M_unicode, only : write(formatted),ch=>character
!     use M_unicode, only : assignment(=)
!     implicit none
!     character(len=*),parameter :: u='(DT)'
!     type(ut)  :: STRING, UPPER, LOWER
!     type(ut)  :: MIDDLE_DOT
! 
!        STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
!        LOWER='abcdefghijklmnopqrstuvwxyz'
!        UPPER='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!        call callit()
! 
!        print u
!        print u,ut('Greek')
!        !
!        ! | Α α | Β β | Γ γ | Δ δ | Ε ε | Ζ ζ   |
!        ! | Η η | Θ θ | Ι ι | Κ κ | Λ λ | Μ μ   |
!        ! | Ν ν | Ξ ξ | Ο ο | Π π | Ρ ρ | Σ σ ς |
!        ! | Τ τ | Υ υ | Φ φ | Χ χ | Ψ ψ | Ω ω   |
!        !
!        STRING='ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω'
!        ! ignoring ς for simplicity
!        UPPER='ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ'
!        LOWER='αβγδεζηθικλμνξοπρστυφχψω'
!        call callit()
! 
!        ! OOP
!        print u
!        print u,ut('OOP!')
!        print u,STRING%TRANSLITERATE(UPPER,'_')
! 
!        ! U+00B7 Middle Dot Unicode Character
!        print u,STRING%TRANSLITERATE(LOWER,'·') ! ASCII bytes
!        print u,STRING%TRANSLITERATE(LOWER,ut('·')) ! cast
!        MIDDLE_DOT=int(z'00B7')
!        print u,STRING%TRANSLITERATE(LOWER,MIDDLE_DOT) ! hexadecimal
! 
!     contains
!     subroutine callit()
!          print u, STRING
! 
!          ! convert -7 string to uppercase:
!          print u, TRANSLITERATE(STRING , LOWER, UPPER )
! 
!          ! change all miniscule letters to a colon (":"):
!          print u, TRANSLITERATE(STRING, LOWER, ':')
! 
!          ! delete all miniscule letters
!          print u, TRANSLITERATE(STRING, LOWER, '')
! 
!          end subroutine callit
! 
!     end program demo_transliterate
! 
!   Results:
! 
!    > aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ
!    > AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ
!    > :A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:T:U:V:W:X:Y:Z
!    > ABCDEFGHIJKLMNOPQRSTUVWXYZ
!    >
!    > Greek
!    > ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω
!    > ΑΑΒΒΓΓΔΔΕΕΖΖΗΗΘΘΙΙΚΚΛΛΜΜΝΝΞΞΟΟΠΠΡΡΣΣςΤΤΥΥΦΦΧΧΨΨΩΩ
!    > Α:Β:Γ:Δ:Ε:Ζ:Η:Θ:Ι:Κ:Λ:Μ:Ν:Ξ:Ο:Π:Ρ:Σ:ςΤ:Υ:Φ:Χ:Ψ:Ω:
!    > ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣςΤΥΦΧΨΩ
!    >
!    > OOP!
!    > _α_β_γ_δ_ε_ζ_η_θ_ι_κ_λ_μ_ν_ξ_ο_π_ρ_σς_τ_υ_φ_χ_ψ_ω
!    > Α·Β·Γ·Δ·Ε·Ζ·Η·Θ·Ι·Κ·Λ·Μ·Ν·Ξ·Ο·Π·Ρ·Σ·ςΤ·Υ·Φ·Χ·Ψ·Ω·
!    > Α·Β·Γ·Δ·Ε·Ζ·Η·Θ·Ι·Κ·Λ·Μ·Ν·Ξ·Ο·Π·Ρ·Σ·ςΤ·Υ·Φ·Χ·Ψ·Ω·
!    > Α·Β·Γ·Δ·Ε·Ζ·Η·Θ·Ι·Κ·Λ·Μ·Ν·Ξ·Ο·Π·Ρ·Σ·ςΤ·Υ·Φ·Χ·Ψ·Ω·
! 
! AUTHOR
!    John S. Urban
! 
! LICENSE
!     MIT
impure function transliterate_uuu(instr,old_set,new_set) result(outstr)

! ident_15="@(#) M_unicode transliterate(3f) replace characters from old set with new set"

type(unicode_type),intent(in) :: instr                  ! input string to change
type(unicode_type),intent(in) :: old_set                ! set of characters to replace
type(unicode_type),intent(in) :: new_set                ! new characters to replace old characters
type(unicode_type)            :: outstr                 ! output string to generate
integer                       :: i, ii, jj

   jj=size(new_set%codes)
   if(jj /= 0)then
      outstr=instr                                      ! initially assume output string equals input string
      stepthru: do i = 1, size(instr%codes)
         ii=findloc(old_set%codes,instr%codes(i),dim=1) ! see if current character is in old_set
         if (ii /= 0)then
            if(ii <= jj)then                            ! use corresponding character in new_set
               outstr%codes(i) = new_set%codes(ii)
            else
               outstr%codes(i) = new_set%codes(jj)      ! new_set not as long as old_set; use last character in new_set
            endif
         endif
      enddo stepthru
   else                                                 ! new_set is null string so delete characters in old_set
      outstr%codes=[(32,i=1,size(instr%codes))]
      hopthru: do i = 1, size(instr%codes)
         ii=findloc(old_set%codes,instr%codes(i),dim=1) ! see if current character is in old_set
         if (ii == 0)then                               ! only keep characters not in old_set
            jj=jj+1
            outstr%codes(jj) = instr%codes(i)
         endif
      enddo hopthru
   endif
end function transliterate_uuu
!-----------------------------------------------------------------------------------------------------------------------------------
function transliterate_uua(instr,old_set,new_set) result (outstr)
type(unicode_type),intent(in)  :: instr
type(unicode_type),intent(in)  :: old_set
character(len=*),intent(in)    :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uuu(instr,old_set,unicode_type(new_set))
end function transliterate_uua
!-----------------------------------------------------------------------------------------------------------------------------------
function transliterate_uaa(instr,old_set,new_set) result (outstr)
type(unicode_type),intent(in)  :: instr
character(len=*),intent(in)    :: old_set
character(len=*),intent(in)    :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uuu(instr,unicode_type(old_set),unicode_type(new_set))
end function transliterate_uaa
!-----------------------------------------------------------------------------------------------------------------------------------
function transliterate_aaa(instr,old_set,new_set) result (outstr)
character(len=*),intent(in)    :: instr
character(len=*),intent(in)    :: old_set
character(len=*),intent(in)    :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uuu(unicode_type(instr),unicode_type(old_set),unicode_type(new_set))
end function transliterate_aaa
!-----------------------------------------------------------------------------------------------------------------------------------
function transliterate_uau(instr,old_set,new_set) result (outstr)
type(unicode_type),intent(in)  :: instr
character(len=*),intent(in)    :: old_set
type(unicode_type),intent(in)  :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uuu(instr,unicode_type(old_set),new_set)
end function transliterate_uau
!-----------------------------------------------------------------------------------------------------------------------------------
function transliterate_aau(instr,old_set,new_set) result (outstr)
character(len=*),intent(in)    :: instr
character(len=*),intent(in)    :: old_set
type(unicode_type),intent(in)  :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uuu(unicode_type(instr),unicode_type(old_set),new_set)
end function transliterate_aau
!-----------------------------------------------------------------------------------------------------------------------------------
function transliterate_aua(instr,old_set,new_set) result (outstr)
character(len=*),intent(in)    :: instr
type(unicode_type),intent(in)  :: old_set
character(len=*),intent(in)    :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uuu(unicode_type(instr),old_set,unicode_type(new_set))
end function transliterate_aua
!-----------------------------------------------------------------------------------------------------------------------------------
function transliterate_auu(instr,old_set,new_set) result (outstr)
character(len=*),intent(in)    :: instr
type(unicode_type),intent(in)  :: old_set
type(unicode_type),intent(in)  :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uuu(unicode_type(instr),old_set,new_set)
end function transliterate_auu
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    get_env(3f) - [M_unicode:SYSTEM] return value of environment variable
!    (LICENSE:MIT)
! 
! SYNOPSIS
! 
!     impure elemental function get_env(name,default) result(out)
! 
!      type(unicode_type),intent=(in) :: name
!      type(unicode_type),optional    :: default
!      type(unicode_type)             :: out
! 
! CHARACTERISTICS
!    NAME and DEFAULT may be default CHARACTER type as well.
! 
! DESCRIPTION
!    get_env(3) gets the value of the requested environment variable
!    as TYPE(UNICODE_TYPE) .
! 
! OPTIONS
!    name     name of environment variable to return the value of.
!             Typically the name may only contain the characters
!             A-Z,a-z,0-9 and underscore; but allowed values are
!             system-dependent.
!    default  value to return if environment variable NAME is not set
!             or set to a blank value
! 
! RETURNS
!    out      value assigned based on value of environment variable NAME
! 
! EXAMPLES
! 
!  Sample program:
! 
!     program demo_get_env
!     use M_unicode, only : get_env, ut=> unicode_type
!     use M_unicode, only : assignment(=), operator(//)
!     implicit none
!     type(ut) :: name
!     type(ut) :: default
!     type(ut) :: value
!     type(ut) :: smiley
!     integer  :: i
!     character(len=*),parameter :: bracket= '(1x,*("[",a,"]",:))'
!        !
!        smiley=128515 ! set with Unicode code point
!        name='UTF8'   ! set with ASCII
!        default='Have a nice day '//smiley//'!' ! set with unicode_type
!        !
!        ! arguments can be type(unicode_type) or character
!        ! but type(unicode_type) is always returned
!        value=get_env(name,             default             )
!        value=get_env(name%character(), default%character() )
!        value=get_env(name,             default%character() )
!        value=get_env(name%character(), default             )
!        !
!        write(*,*)value%character()
!        !
!        ! print each glyph surrounded by brackets
!        write(*,bracket)(value%character(i,i),i=1,value%len())
!        !
!     end program demo_get_env
! 
!   Results:
! 
!     > Have a nice day 😃!
!     > [H][a][v][e][ ][a][ ][n][i][c][e][ ][d][a][y][ ][😃][!]
! 
! AUTHOR
!      John S. Urban
! 
! LICENSE
!     MIT
impure elemental function get_env_aa(name,default) result(uvalue)
! a function that makes calling get_environment_variable(3) simple
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
implicit none
character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: default
character(len=:),allocatable         :: value
type(unicode_type)                   :: uvalue
integer                              :: howbig
integer                              :: stat
integer                              :: length
   length=0
   value=""
   if(name.ne."")then
      call get_environment_variable( name, &
      & length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
       if(.not.present(default))then
          write(stderr,*) &
          & name, " is not defined in the environment"
          value=""
       endif
      case (2)
       write(stderr,*) &
       & "This processor does not support environment variables. Boooh!"
       value=""
      case default
       ! make string of sufficient size to hold value
       if(allocated(value))deallocate(value)
       allocate(character(len=max(howbig,1)) :: value)
       ! get value
       call get_environment_variable( &
       & name,value,status=stat,trim_name=.true.)
       if(stat.ne.0)value=""
      end select
   endif
   if(value.eq."".and.present(default))value=default
   uvalue=value
end function get_env_aa

impure elemental function get_env_uu(name,default) result(value)
! a function that makes calling get_environment_variable(3) simple
type(unicode_type),intent(in)          :: name
type(unicode_type),intent(in),optional :: default
type(unicode_type)                     :: value
character(len=:),allocatable           :: temp1
character(len=:),allocatable           :: temp2
integer                                :: nerr
   call codepoints_to_utf8_str(name%codes,temp1,nerr)
   if(present(default))then
      call codepoints_to_utf8_str(default%codes,temp2,nerr)
      value=get_env_aa(temp1,temp2)
   else
      value=get_env_aa(temp1)
   endif
end function get_env_uu

impure elemental function get_env_au(name,default) result(value)
! a function that makes calling get_environment_variable(3) simple
character(len=*),intent(in)   :: name
type(unicode_type),intent(in) :: default
type(unicode_type)            :: value
character(len=:),allocatable  :: temp
integer                       :: nerr
   call codepoints_to_utf8_str(default%codes,temp,nerr)
   value=get_env_aa(name,temp)
end function get_env_au

impure elemental function get_env_ua(name,default) result(value)
! a function that makes calling get_environment_variable(3) simple
type(unicode_type),intent(in)        :: name
character(len=*),intent(in)          :: default
type(unicode_type)                   :: value
character(len=:),allocatable         :: temp
integer                              :: nerr
   call codepoints_to_utf8_str(name%codes,temp,nerr)
   value=get_env_aa(temp,default)
end function get_env_ua
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    get_arg(3f) - [M_unicode:SYSTEM] get command line argument
!    (LICENSE:MIT)
! 
! SYNOPSIS
! 
!     impure elemental function get_arg(position,default) result(out)
! 
!      integer,intent=(in)         :: position
!      type(unicode_type),optional :: default
!      type(unicode_type)          :: out
! 
! CHARACTERISTICS
!    DEFAULT may be default CHARACTER type as well.
! 
! DESCRIPTION
!    get_arg(3) gets the value of the requested command line argument
!    as TYPE(UNICODE_TYPE) .
! 
! OPTIONS
!    position  Position on command line of requested argument. Zero returns
!              the name of the command executed. Non-existent positions
!              default to returning a null string.
! 
!    default  value to return if argument is not set or set to a blank value
! 
! RETURNS
!    out      value assigned based on value of environment variable NAME
! 
! EXAMPLES
! 
!  Sample program:
! 
!     program demo_get_arg
!     use M_unicode, only : get_arg, ut=> unicode_type, ch=>character
!     use M_unicode, only : assignment(=), operator(//), write(formatted)
!     implicit none
!     integer  :: position
!     type(ut) :: default
!     type(ut) :: value
!     type(ut) :: smiley
!     integer  :: i
!     character(len=*),parameter :: bracket= '(1x,*("[",a,"]",:))'
!        !
!        smiley=128515 ! set with Unicode code point
!        default='Wish I was first '//smiley//'!' ! set with unicode_type
!        !
!        ! arguments can be type(unicode_type) or character
!        ! but type(unicode_type) is always returned
!        do position=0,command_argument_count()
!           value=get_arg(position, default             )
!           value=get_arg(position, default%character() )
!           !
!           write(*,*)value%character()
!           write(*,'(DT)')default%get_arg(position)
!           !
!           ! print each glyph surrounded by brackets
!           write(*,bracket)(value%character(i,i),i=1,value%len())
!        enddo
!        !
!     end program demo_get_arg
! 
!   Results:
! 
!    > demo_get_arg
!    > demo_get_arg
!    > [d][e][m][o][_][g][e][t][_][a][r][g]
! 
! AUTHOR
!      John S. Urban
! 
! LICENSE
!     MIT
impure elemental function get_arg_ia(position,default) result(value)
! get nth argument from command line
integer,intent(in)                   :: position
character(len=*),intent(in),optional :: default
character(len=:),allocatable         :: temp
type(unicode_type)                   :: value
integer                              :: argument_length, istat
   call get_command_argument(number=position,length=argument_length)
   if(allocated(temp))deallocate(temp)
   allocate(character(len=argument_length) :: temp)
   temp(:)=''
   call get_command_argument(position, temp, status=istat)
   if(present(default))then
      if(temp.eq.'')temp=default
   endif
   value=temp
end function get_arg_ia

impure elemental function get_arg_iu(position,default) result(value)
integer,intent(in)            :: position
type(unicode_type),intent(in) :: default
character(len=:),allocatable  :: temp
type(unicode_type)            :: value
integer                       :: nerr
   call codepoints_to_utf8_str(default%codes,temp,nerr)
   value=get_arg_ia(position,temp)
end function get_arg_iu
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_transliterate_uu(self,old_set,new_set) result (outstr)
class(unicode_type),intent(in) :: self
type(unicode_type),intent(in)  :: old_set
type(unicode_type),intent(in)  :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uuu(self,old_set,new_set)
end function oop_transliterate_uu
!===================================================================================================================================
function oop_transliterate_ua(self,old_set,new_set) result (outstr)
class(unicode_type),intent(in) :: self
type(unicode_type),intent(in)  :: old_set
character(len=*),intent(in)    :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uua(self,old_set,new_set)
end function oop_transliterate_ua
!===================================================================================================================================
function oop_transliterate_aa(self,old_set,new_set) result (outstr)
class(unicode_type),intent(in) :: self
character(len=*),intent(in)    :: old_set
character(len=*),intent(in)    :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uaa(self,old_set,new_set)
end function oop_transliterate_aa
!===================================================================================================================================
function oop_transliterate_au(self,old_set,new_set) result (outstr)
class(unicode_type),intent(in) :: self
character(len=*),intent(in)    :: old_set
type(unicode_type),intent(in)  :: new_set
type(unicode_type)             :: outstr
   outstr=transliterate_uau(self,old_set,new_set)
end function oop_transliterate_au
!===================================================================================================================================
function oop_replace_uuu(self,old,new,occurrence,repeat,ignorecase,changes,back) result (newline)

class(unicode_type),intent(in) :: self       ! input line to be changed
type(unicode_type),intent(in)  :: old        ! old substring to replace
type(unicode_type),intent(in)  :: new        ! new substring
integer,intent(in),optional    :: occurrence ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional    :: repeat     ! how many replacements
logical,intent(in),optional    :: ignorecase
logical,intent(in),optional    :: back
integer,intent(out),optional   :: changes    ! number of changes made
! returns
type(unicode_type)             :: newline    ! output string

   newline=replace(self,old,new,occurrence=occurrence,repeat=repeat,ignorecase=ignorecase,changes=changes,back=back)
end function oop_replace_uuu
!===================================================================================================================================
function oop_replace_uaa(self,old,new,occurrence,repeat,ignorecase,changes,back) result (newline)

class(unicode_type),intent(in) :: self       ! input line to be changed
character(len=*),intent(in)    :: old        ! old substring to replace
character(len=*),intent(in)    :: new        ! new substring
integer,intent(in),optional    :: occurrence ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional    :: repeat     ! how many replacements
logical,intent(in),optional    :: ignorecase
logical,intent(in),optional    :: back
integer,intent(out),optional   :: changes    ! number of changes made
! returns
type(unicode_type)             :: newline    ! output string

   newline=replace(self,unicode_type(old),unicode_type(new), &
   & occurrence=occurrence,repeat=repeat,ignorecase=ignorecase,changes=changes,back=back)
end function oop_replace_uaa
!===================================================================================================================================
function oop_replace_uau(self,old,new,occurrence,repeat,ignorecase,changes,back) result (newline)

class(unicode_type),intent(in) :: self       ! input line to be changed
character(len=*),intent(in)    :: old        ! old substring to replace
type(unicode_type),intent(in)  :: new        ! new substring
integer,intent(in),optional    :: occurrence ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional    :: repeat     ! how many replacements
logical,intent(in),optional    :: ignorecase
logical,intent(in),optional    :: back
integer,intent(out),optional   :: changes    ! number of changes made
! returns
type(unicode_type)             :: newline    ! output string

   newline=replace(self,unicode_type(old),new, &
   & occurrence=occurrence,repeat=repeat,ignorecase=ignorecase,changes=changes,back=back)
end function oop_replace_uau
!===================================================================================================================================
function oop_replace_uua(self,old,new,occurrence,repeat,ignorecase,changes,back) result (newline)

class(unicode_type),intent(in) :: self       ! input line to be changed
type(unicode_type),intent(in)  :: old        ! old substring to replace
character(len=*),intent(in)    :: new        ! new substring
integer,intent(in),optional    :: occurrence ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional    :: repeat     ! how many replacements
logical,intent(in),optional    :: ignorecase
logical,intent(in),optional    :: back
integer,intent(out),optional   :: changes    ! number of changes made
! returns
type(unicode_type) :: newline                ! output string

   newline=replace(self,old,unicode_type(new), &
   & occurrence=occurrence,repeat=repeat,ignorecase=ignorecase,changes=changes,back=back)
end function oop_replace_uua
!===================================================================================================================================
function oop_section_uu(self,start,end,new) result (newline)
class(unicode_type),intent(in) :: self       ! input line to be changed
integer,intent(in)             :: start
integer,intent(in)             :: end
type(unicode_type),intent(in)  :: new        ! new substring
type(unicode_type)             :: newline    ! output string
   newline=section_uu(self,start,end,new)
end function oop_section_uu
!===================================================================================================================================
function oop_section_ua(self,start,end,new) result (newline)
class(unicode_type),intent(in) :: self       ! input line to be changed
integer,intent(in)             :: start
integer,intent(in)             :: end
character(len=*),intent(in)    :: new        ! new substring
type(unicode_type)             :: newline    ! output string
   newline=section_ua(self,start,end,new)
end function oop_section_ua
!===================================================================================================================================
function oop_fmt(self,format) result (string_out)
class(unicode_type),intent(in) :: self
character(len=*),optional      :: format
type(unicode_type)             :: string_out
   string_out=fmt(self,format)
end function oop_fmt
!===================================================================================================================================
function oop_expandtabs(self,tab_size) result (string_out)
class(unicode_type),intent(in) :: self
integer,intent(in),optional    :: tab_size
type(unicode_type)             :: string_out
   string_out=expandtabs(self,tab_size)
end function oop_expandtabs
!===================================================================================================================================
function oop_escape(self,protect) result (string_out)
class(unicode_type),intent(in)       :: self
character(len=1),intent(in),optional :: protect
type(unicode_type)                   :: string_out
   string_out=escape(self,protect)
end function oop_escape
!===================================================================================================================================
function oop_upper(self) result (string_out)
class(unicode_type),intent(in) :: self
type(unicode_type)             :: string_out
   string_out=upper(self)
end function oop_upper
!===================================================================================================================================
function oop_lower(self) result (string_out)
class(unicode_type),intent(in) :: self
type(unicode_type)             :: string_out
   string_out=lower(self)
end function oop_lower
!===================================================================================================================================
function oop_adjustl(self,glyphs) result (string_out)
class(unicode_type),intent(in) :: self
integer,intent(in),optional    :: glyphs
type(unicode_type)             :: string_out
   string_out=adjustl_str(self,glyphs)
end function oop_adjustl
!===================================================================================================================================
function oop_adjustr(self,glyphs) result (string_out)
class(unicode_type),intent(in) :: self
integer,intent(in),optional    :: glyphs
type(unicode_type)             :: string_out
   string_out=adjustr_str(self,glyphs)
end function oop_adjustr
!===================================================================================================================================
function oop_sub(self,first,last,step) result(str_out)
class(unicode_type),intent(in) :: self
integer,intent(in),optional    :: first, last, step
type(unicode_type)             :: str_out
   str_out=sub(self,first,last,step)
end function oop_sub
!===================================================================================================================================
function oop_get_arg_iu(self,position) result (value)
class(unicode_type),intent(in)       :: self
integer,intent(in)                   :: position
type(unicode_type)                   :: value
   value=get_arg_iu(position,self)
end function oop_get_arg_iu
!===================================================================================================================================
function oop_get_env_ua(self,default) result (value)
class(unicode_type),intent(in) :: self
character(len=*),intent(in)    :: default
type(unicode_type)             :: value
   value=get_env_ua(self,default)
end function oop_get_env_ua
!===================================================================================================================================
function oop_get_env_uu(self,default) result (value)
class(unicode_type),intent(in) :: self
type(unicode_type),intent(in)  :: default
type(unicode_type)             :: value
   value=get_env_uu(self,default)
end function oop_get_env_uu
!===================================================================================================================================
function oop_join(self,array,clip) result (out)

! ident_16="@(#) M_unicode oop_join(3f) merge string array into a single string value adding specified separator"

class(unicode_type),intent(in) :: self
type(unicode_type),intent(in)  :: array(:)
logical,intent(in),optional    :: clip
type(unicode_type)             :: out

   if(allocated(self%codes))then
      out=join(array,self,clip)
   else
      out=join(array,unicode_type(''),clip)
   endif

end function oop_join
!===================================================================================================================================
function oop_pad(self,length,pattern,right,clip) result (out)

! ident_17="@(#) M_unicode pad(3f) pad string with repeating pattern to at least specified length"

class(unicode_type),intent(in)         :: self       ! input line to be changed
integer,intent(in)                     :: length
type(unicode_type),intent(in),optional :: pattern
logical,optional,intent(in)            :: right
logical,optional,intent(in)            :: clip
! returns
type(unicode_type)                     :: out

   out=pad(self,length,pattern,right,clip)

end function oop_pad
!===================================================================================================================================
function oop_character(self,first,last,step) result(str_out)
class(unicode_type), intent(in) :: self
integer,intent(in),optional     :: first, last, step
character(len=:),allocatable    :: str_out
integer                         :: start, end, inc
type(unicode_type)              :: temp
integer                         :: which
   which=4*merge(1,0,present(first))+ 2*merge(1,0,present(last))+ 1*merge(1,0,present(step))
   select case(which)
   case(int(b'000')) ; start=1     ; end=len(self) ; inc=1
   case(int(b'001')) ; start=1     ; end=len(self) ; inc=step
   case(int(b'010')) ; start=1     ; end=last      ; inc=1
   case(int(b'011')) ; start=1     ; end=last      ; inc=step
   case(int(b'100')) ; start=first ; end=first     ; inc=1
   case(int(b'101')) ; start=first ; end=len(self) ; inc=step
   case(int(b'110')) ; start=first ; end=last      ; inc=1
   case(int(b'111')) ; start=first ; end=last      ; inc=step
   end select
   temp=self%codes(start:end:inc)
   str_out=str_to_char(temp)
end function oop_character
!===================================================================================================================================
function oop_byte(self,first,last,step) result(bytes_out)
class(unicode_type), intent(in) :: self
integer,intent(in),optional     :: first, last, step
character(len=1),allocatable    :: bytes_out(:)
   bytes_out=s2a(oop_character(self,first,last,step))
end function oop_byte
!===================================================================================================================================
! return codepoint value of first character as is done by intrinsic ichar()
elemental function oop_ichar(self) result(code)
class(unicode_type), intent(in) :: self
integer                         :: code
   if(size(self%codes) == 0 )then
      code=0
   else
      code=self%codes(1)
   endif
end function oop_ichar
!===================================================================================================================================
function oop_codepoint(self,first,last,step) result(codes_out)
class(unicode_type), intent(in) :: self
integer,allocatable             :: codes_out(:)
integer,intent(in),optional     :: first, last, step
integer                         :: start, end, inc
integer                         :: which
   which=4*merge(1,0,present(first))+ 2*merge(1,0,present(last))+ 1*merge(1,0,present(step))
   select case(which)
   case(int(b'000')) ; start=1     ; end=len(self) ; inc=1
   case(int(b'001')) ; start=1     ; end=len(self) ; inc=step
   case(int(b'010')) ; start=1     ; end=last      ; inc=1
   case(int(b'011')) ; start=1     ; end=last      ; inc=step
   case(int(b'100')) ; start=first ; end=first     ; inc=1
   case(int(b'101')) ; start=first ; end=len(self) ; inc=step
   case(int(b'110')) ; start=first ; end=last      ; inc=1
   case(int(b'111')) ; start=first ; end=last      ; inc=step
   end select
   codes_out=self%codes(start:end:inc)
end function oop_codepoint
!===================================================================================================================================
impure function oop_verify(self,set,back) result(pos)
class(unicode_type),intent(in) :: self
class(unicode_type),intent(in) :: set
logical,intent(in),optional    :: back
integer                        :: pos
   pos=verify_uu(self,set,back=back)
end function oop_verify
!===================================================================================================================================
pure function oop_scan(self,set,back) result(pos)
class(unicode_type),intent(in) :: self
class(unicode_type),intent(in) :: set
logical,optional,intent(in)    :: back
integer                        :: pos
   pos=scan_uu(self,set,back=back)
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
   if(allocated(tokens))deallocate(tokens)
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
! NAME
!     READLINE(3f) - [M_unicode:READ] read a line from specified LUN into
!                    string up to line length limit
!                    (LICENSE:MIT)
! 
! SYNTAX
!    function readline(lun,iostat) result(line)
! 
!     integer,intent(in),optional  :: lun
!     integer,intent(out),optional :: iostat
!     type(unicode_type)           :: line
! 
! DESCRIPTION
!    Read a line of any length up to programming environment maximum
!    line length. Requires Fortran 2003+.
! 
!    It is primarily expected to be used when reading input which will
!    then be parsed.
! 
!    The input file must have a PAD attribute of YES for the function
!    to work properly, which is typically true.
! 
!    The simple use of a loop that repeatedly re-allocates a character
!    variable in addition to reading the input file one buffer at a
!    time could (depending on the programming environment used) be
!    inefficient, as it could reallocate and allocate memory used for
!    the output string with each buffer read.
! 
! OPTIONS
!     LUN     optional LUN (Fortran logical I/O unit) number. Defaults
!             to stdin.
!     IOSTAT  status returned by READ(IOSTAT=IOS). If not zero, an error
!             occurred or an end-of-file or end-of-record was encountered.
! RETURNS
!     LINE    line read.
!             if IOSTAT is not zero, LINE returns the I/O error message.
! 
! EXAMPLE
! 
!    Sample program:
! 
!     program demo_readline
!     use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
!     use,intrinsic :: iso_fortran_env, only : iostat_end
!     use M_unicode, only : readline, len, trim
!     use M_unicode, only : assignment(=), ch=>character, ut=>unicode_type
!     implicit none
!     type(ut)                     :: line
!     character(len=:),allocatable :: aline
!     integer,allocatable          :: ints(:)
!     integer                      :: iostat
!        open(unit=stdin,pad='yes')
!        !
!        INFINITE: do
!           line=readline(iostat=iostat)
!           if(iostat.ne.0)exit
!           ! write the length, line in brackets and its Unicode codepoints
!           write(*,'(*(g0,1x))')len(line),'['//ch(line)//']',line%codepoint()
!           ! or assign the string to an allocatable array of integers
!           ints=line
!           ! and the string to a character variable
!           aline=line
!           write(*,'(*(g0,1x))')len(line),'['//ch(line)//']',ints
!        enddo INFINITE
!        !
!        if(iostat /= iostat_end)then
!           write(*,*)'error reading input:',ch(trim(line))
!        endif
!        !
!     end program demo_readline
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
function readline(lun,iostat) result(line)
implicit none

! ident_18="@(#) M_unicode readline(3f) read a line from specified LUN into string up to line length limit"

type(unicode_type)           :: line
integer,intent(in),optional  :: lun
integer,intent(out),optional :: iostat
integer                      :: iostat_local
character(len=4096)          :: message

integer,parameter            :: buflen=1024
character(len=:),allocatable :: line_local
character(len=buflen)        :: buffer
integer                      :: isize
integer                      :: lun_local

   line_local=''
   iostat_local=0
   if(present(lun))then
      lun_local=lun
   else
      lun_local=stdin
   endif

   INFINITE: do                               ! read characters from line and append to result
      read(lun_local,pad='yes',iostat=iostat_local,fmt='(a)',advance='no', &
      & size=isize,iomsg=message) buffer      ! read next buffer (might use stream I/O for files
                                              ! other than stdin so system line limit is not limiting
      if(isize > 0)line_local=line_local//buffer(:isize)    ! append what was read to result
      if(is_iostat_eor(iostat_local))then     ! if hit EOR reading is complete unless backslash ends the line
         iostat_local=0                       ! hitting end of record is not an error for this routine
         exit INFINITE                        ! end of reading line
     elseif(iostat_local /= 0)then            ! end of file or error
        line=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   line=line_local                            ! trim line
   if(present(iostat))iostat=iostat_local
end function readline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     FMT(3f) - [M_unicode:CONVERSION] convert any intrinsic to a string using specified format
!     (LICENSE:MIT)
! SYNOPSIS
! 
!     function fmt(value,format) result(string)
! 
!      class(*),intent(in),optional           :: value
! 
!      character(len=*),intent(in),optional   :: format
!         or
!      type(unicode_type),intent(in),optional :: format
! 
!      type(unicode_type)                     :: string
! DESCRIPTION
!     FMT(3f) converts any standard intrinsic value to a string using the specified
!     format.
! OPTIONS
!     value    value to print the value of. May be of type INTEGER, LOGICAL,
!              REAL, DOUBLEPRECISION, COMPLEX, or CHARACTER as well as
!              TYPE(UNICODE_TYPE).
!     format   format to use to print value. It is up to the user to use an
!              appropriate format. The format does not require being
!              surrounded by parenthesis. If not present a default is selected
!              similar to what would be produced with free format, with
!              trailing zeros removed.
! RETURNS
!     string   A string value
! EXAMPLES
! 
!    Sample program:
! 
!      program demo_fmt
!      use :: M_unicode, only : fmt, assignment(=)
!      use :: M_unicode, only : ut=>unicode_type, ch=>character
!      implicit none
!      character(len=:),allocatable :: Astr, Aformat
!      type(ut) :: Ustr
!         ! format can be CHARACTER
!         Aformat="('[',i0,']')"
!         Astr=fmt(10,Aformat)
!         write(*,*)'result is ',Astr
!         ! format can be string
!         Astr=fmt(10.0/3.0,ut("'[',g0.5,']'"))
!         write(*,*)'result is ',Astr
!         ! Output is a string, so use ch()
!         write(*,*)'result is ', ch(fmt(.true.,"'The answer is [',g0,']'"))
!         ! OOP
!         Ustr='A B C'
!         Ustr=Ustr%fmt("'[',g0,']'")
!         write(*,*)'result is ',ch(Ustr)
!      end program demo_fmt
! 
!    Results:
! 
!      result is [10]
!      result is [3.3333]
!      result is The final answer is [T]
!      result is [A B C]
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
recursive function afmt(generic,format) result (line)

! ident_19="@(#) M_unicode afmt(3f) convert any intrinsic to a CHARACTER variable using specified format"

class(*),intent(in)                  :: generic
character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: line
character(len=:),allocatable         :: fmt_local
character(len=:),allocatable         :: re,im
integer                              :: iostat
character(len=255)                   :: iomsg
character(len=1),parameter           :: null=char(0)
integer                              :: iilen
logical                              :: trimit
   if(present(format))then
      fmt_local=format
      trimit=.false.
   else
      fmt_local=''
      trimit=.true.
   endif
   ! add ",a" and print null and use position of null to find length of output
   ! add cannot use SIZE= or POS= or ADVANCE='NO' on WRITE() on INTERNAL READ,
   ! and do not want to trim as trailing spaces can be significant
   if(fmt_local == '')then
      select type(generic)
         type is (integer(kind=int8));     fmt_local='(i0,a)'
         type is (integer(kind=int16));    fmt_local='(i0,a)'
         type is (integer(kind=int32));    fmt_local='(i0,a)'
         type is (integer(kind=int64));    fmt_local='(i0,a)'
         type is (real(kind=real32));      fmt_local='(1pg0,a)'
         type is (real(kind=real64));      fmt_local='(1pg0,a)'
#ifdef FLOAT128
         type is (real(kind=real128));     fmt_local='(1pg0,a)'
#endif
         type is (logical);                fmt_local='(l1,a)'
         type is (character(len=*));       fmt_local='(a,a)'
                 trimit=.false.
         type is (unicode_type);           fmt_local='(a,a)'
                 trimit=.false.
         type is (complex);                fmt_local='("(",1pg0,",",1pg0,")",a)'
         type is (complex(kind=real64));   fmt_local='("(",1pg0,",",1pg0,")",a)'
         class default
          fmt_local='(*(g0,1x)'
          stop '<ERROR>*afmt* unknown type.'
      end select
   else
      if(format(1:1) == '(')then
         fmt_local=format(:len_trim(format)-1)//',a)'
      else
         fmt_local='('//fmt_local//',a)'
      endif
   endif
   if(allocated(line))deallocate(line)
   allocate(character(len=256) :: line) ! cannot currently write into allocatable variable
   iostat=0
   select type(generic)
     type is (integer(kind=int8));  write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
     type is (integer(kind=int16)); write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
     type is (integer(kind=int32)); write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
     type is (integer(kind=int64)); write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
     type is (real(kind=real32));   write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
     type is (real(kind=real64));   write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
#ifdef FLOAT128
     type is (real(kind=real128));  write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
#endif
     type is (logical);             write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
     type is (character(len=*));    write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
     type is (unicode_type);        write(line,fmt_local,iostat=iostat,iomsg=iomsg) character(generic),null
     type is (complex);
        if(trimit)then
           re=afmt(real(generic))
           im=afmt(aimag(generic))
           call trimzeros_(re)
           call trimzeros_(im)
           fmt_local='("(",g0,",",g0,")",a)'
           write(line,fmt_local,iostat=iostat,iomsg=iomsg) trim(re),trim(im),null
           trimit=.false.
        else
           write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
        endif
     type is (complex(kind=real64));
        if(trimit)then
           re=afmt(real(generic))
           im=afmt(aimag(generic))
           call trimzeros_(re)
           call trimzeros_(im)
           fmt_local='("(",g0,",",g0,")",a)'
           write(line,fmt_local,iostat=iostat,iomsg=iomsg) trim(re),trim(im),null
           trimit=.false.
        else
           write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
        endif
     class default
        stop '<ERROR>*afmt* unknown type'
   end select
   if(iostat /= 0)then
      line='<ERROR>'//trim(iomsg)
   else
      iilen=index(line,null,back=.true.)
      if(iilen == 0)iilen=len(line)
      line=line(:iilen-1)
   endif

   if(index(line,'.') /= 0 .and. trimit) call trimzeros_(line)

end function afmt
!===================================================================================================================================
recursive function fmt_ga(generic,format) result (line)

! ident_20="@(#) M_unicode afmt(3f) convert any intrinsic to a CHARACTER variable using specified format"

class(*),intent(in)                  :: generic
character(len=*),intent(in),optional :: format
type(unicode_type)                   :: line
line=afmt(generic,format)
end function fmt_ga
recursive function fmt_gs(generic,format) result (line)

! ident_21="@(#) M_unicode afmt(3f) convert any intrinsic to a CHARACTER variable using specified format"

class(*),intent(in)           :: generic
type(unicode_type),intent(in) :: format
type(unicode_type)            :: line
character(len=:),allocatable  :: aformat
   aformat=format
   line=afmt(generic,aformat)
end function fmt_gs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     TRIMZEROS_(3fp) - [M_unicode:EDITING] Delete trailing zeros from
!     numeric decimal string
!     (LICENSE:MIT)
! 
! SYNOPSIS
! 
!     subroutine trimzeros_(str)
! 
!      character(len=*)  :: str
! 
! DESCRIPTION
!     TRIMZEROS_(3f) deletes trailing zeros from a string representing a
!     number. If the resulting string would end in a decimal point, one
!     trailing zero is added.
! 
! OPTIONS
!     str   input string will be assumed to be a numeric value and have
!           trailing zeros removed
! EXAMPLES
! 
!     Sample program:
! 
!        program demo_trimzeros_
!        use M_unicode, only : trimzeros_
!        character(len=:),allocatable :: string
!           string= '123.450000000000'
!           call trimzeros_(string)
!           write(*,*)string
!           string='12345'
!           call trimzeros_(string)
!           write(*,*)string
!           string='12345.'
!           call trimzeros_(string)
!           write(*,*)string
!           string='12345.00e3'
!           call trimzeros_(string)
!           write(*,*)string
!        end program demo_trimzeros_
! 
!    Results:
! 
!      > 123.45
!      > 12345
!      > 12345
!      > 12345e3
! 
! AUTHOR
!     John S. Urban
! 
! LICENSE
!     MIT
subroutine trimzeros_(string)

! ident_22="@(#) M_unicode trimzeros_(3fp) Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)               :: string
character(len=len(string) + 2) :: str
character(len=len(string))     :: eexp       ! the exponent string if present
integer                        :: ipos       ! where exponent letter appears if present
integer                        :: i, ii
intrinsic scan, index, len_trim
!intrinsic operator(//)  ! any way to do something like this?
   str = string                              ! working copy of string
   ipos = scan(str, 'eEdD')                  ! find end of real number if string uses exponent notation
   if (ipos > 0) then                        ! letter was found
      eexp = str(ipos:)                      ! keep exponent string so it can be added back as a suffix
      str = str(1:ipos - 1)                  ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if (index(str, '.') == 0) then            ! if no decimal character in original string add one to end of string
      ii = len_trim(str)
      str(ii + 1:ii + 1) = '.'               ! add decimal to end of string
   endif
   do i = len_trim(str), 1, -1               ! scanning from end find a non-zero character
      select case (str(i:i))
      case ('0')                             ! found a trailing zero so keep trimming
         cycle
      case ('.')                             ! found a decimal character at end of remaining string
         if (i <= 1) then
            str = '0'
         else
            str = str(1:i - 1)
         endif
         exit
      case default
         str = str(1:i)                      ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if (ipos > 0) then                        ! if originally had an exponent place it back on
      string = trim(str)//trim(eexp)
   else
      string = str
   endif
end subroutine trimzeros_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function concat_uu_(lhs,rhs) result (string)
type(unicode_type),intent(in) :: lhs
type(unicode_type),intent(in) :: rhs
type(unicode_type)            :: string1, string2, string
   string1 = fmt(lhs)
   string2 = fmt(rhs)
   string%codes=[string1%codes,string2%codes]
end function concat_uu_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! worked fine with gfortran, ifx produced an error
! ././src/M_unicode.F90(5530): error #9186: The dummy arguments of the
! specific procedure defining a defined assignment or defined operator
! cannot both be unlimited polymorphic. [CONCAT_G_G]
!
function concat_g_g(lhs,rhs) result (string)

! ident_23="@(#) M_overload g_g(3f) convert two single intrinsic values or strings to a string"
!
! use this instead of str() so character variables are not trimmed and/or spaces are not added
class(*),intent(in) :: lhs, rhs
type(unicode_type)  :: string1, string2, string
   string1 = fmt(lhs)
   string2 = fmt(rhs)
   string%codes=[string1%codes,string2%codes]
end function concat_g_g
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!> Write string to connected formatted unit.
subroutine write_formatted(string, unit, iotype, v_list, iostat, iomsg)
class(unicode_type), intent(in) :: string
integer, intent(in)             :: unit
character(len=*), intent(in)    :: iotype
integer, intent(in)             :: v_list(:)
integer, intent(out)            :: iostat
character(len=*), intent(inout) :: iomsg

   select case(iotype)
   case("LISTDIRECTED")
      write(unit, '(a)', iostat=iostat, iomsg=iomsg) character(string)
   case("NAMELIST")
      error stop "[Fatal] This implementation does not support namelist output"
   case default ! DT*
      select case(size(v_list))
      case(0) ! DT
         if(allocated(string%codes))then
            write(unit, '(a)', iostat=iostat, iomsg=iomsg) character(string)
         endif
      case default
         error stop "[Fatal] This implementation does not support v_list formatters"
      end select
   end select

end subroutine write_formatted
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_unicode__base

module M_unicode
use M_unicode__base
public :: operator(//)
public :: operator(.cat.)
interface operator(//); module procedure :: concat_g_g; end interface operator(//)
interface operator(.cat.); module procedure :: concat_g_g; end interface operator(.cat.)
end module M_unicode
