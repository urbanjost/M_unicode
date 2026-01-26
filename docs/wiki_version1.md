<a name="Unicode"></a>
#### Introduction to Fortran Unicode support
## Reading, writing and processing UTF-8 data using Fortran, Unicode codepoints, and their encodings

Unicode is an international standard for encoding text that assigns a
unique whole numeric value, called a **code point**, to every character,
symbol, and emoji from virtually all written languages and scripts in
the world. This allows computers to process, store, and display text
correctly across different platforms by providing a universal mapping
for characters.

There are several standardized ways to encode the code points. The interest
here is in two of them -- UTF-8 and UCS-4 encoding.

**UTF-8 encoding** has emerged as the de-facto standard format for
representing Unicode in text files on all major operating systems.

Not all code points are stored with the same number of bytes in UTF-8. The
characters represented in single-byte ASCII-7 characters are represented
by the same single byte in UTF-8 as well, but other characters require
from two to four bytes of storage. This means ASCII-7 is a subset of
UTF-8 but UTF-8 can represent far more characters. This compatibility
with ASCII is a very large advantage of UTF-8 encoding over other code
point encodings as a file format, contributing to it becoming a de-facto
standard.

**UCS-4 encoding** is simpler and homogeneous. Each code point is stored
as a 32-bit value, thus using the same amount of bytes for each character
(unlike UTF-8). This format is often used to internally encode Unicode
code points in various computing languages.

UCS-4 encoding shares the trait of constant storage size per element with
all Fortran intrinsic types, making it a natural fit for the internal
representation of code points in the Fortran language.

Since the release of the 2003 standard, Fortran does indeed __optionally__
support processing of Unicode UTF-8-encoded files in this manner. Data
is internally stored using UCS-4 encoding but translated to and from
UCS-8 encoding during formatted I/O. This option will be referred to as
the **Fortran ISO_10646 standard**.

### Glyphs

A character encoded using UCS-4 or UTF-8 is often referred to as a
**"glyph"** to differentiate it from ASCII characters. "Glyph" more
technically is actually the name for the appearance of the rendering of
the character via a font. But it will be used here as well as representing
a Unicode "character".

### The Guides

The following guides describe using UTF-8 files from Fortran
codes. They not only include examples using the standard-specified
ISO_10646 extension, but describe how to process UTF-8 encoded data
without the extension. They include discussions concerning what is
standardized and what is not, what commonly-used extensions compilers
provide to address some of the current gaps in Unicode support, and
what is known to be potentially non-portable but useful behavior from
various compilers/processors.

The resulting methods are incorporated into Fortran Modules available
via github repositories.

The selection of methods to employ breaks down along these major divides:

 + [using the optional Fortran ISO_10646 standard](#ucs4).

   The first guide set assumes you want to use the ISO_10646 extension
   and would prefer to conform as portably as reasonable to the Fortran
   standard; and probably avoid using UTF-8-encoded constant strings.

 + [processing UTF-8 data without using the ISO_10646 extension](#no_iso_10646).

 + [using UTF-8-encoded source files](#utf8_source_ext) versus using
   only Fortran source files strictly adhering to the Fortran character set

 + using [Common Unicode-related processor-dependent extensions](#extensions_ext)

 + [M_ucs4](https://github.com/urbanjost/M_ucs4) - A Module supporting
   using the ISO_10646 extension

   A module supplementing the ISO_10646 extension, including

     * low-level procedures for converting UTF-8 encoded byte streams to
       and from UCS-4 character variables.
     * additional conversion routines to bridge the gaps in Fortran
     * Unicode processing
     * related utility programs

 + [M_unicode](https://github.com/urbanjost/M_unicode) - Processing
   UTF-8 data without depending on the ISO_10646 extension

   A module defining a user-defined type that allows for ragged arrays
   of Unicode data and overlays of intrinsic functions along with many
   common character methods allowing additional functions such as case
   conversion, sorting, and padding.  This is a very complete inteface
   for processing UTF-8 encoded data that does not require the optional
   ISO_10646 extension. It provides both a functional and OOP interface.

-------------------------------------------------------------
<a name="ucs4"></a>
## The optional Fortran 2003 ISO_10646 standard

The Fortran 2003 standard first defines support for processing of
Unicode UTF-8-encoded files. There are three simple main points:

1. The option ENCODING="UTF8" on OPEN() statements indicates to
   automatically encode and decode formatted data from UTF-8 files to
   binary UCS-4 internal values.

2. This new UCS-4 type is expressed as a CHARACTER variable declared
   to have KIND="ISO_10646",

3. The functionality provided includes overloading the ASCII
   character-related intrinsics, comparitive operators, and
   assigment with support for processing of UCS-4
   encoded data.

These three simple additions combined conveniently make it so the
processing of UCS-4 encoded data can be coded with the same methods as
used for the ASCII-7 encoded data historically supported by the default
intrinsic CHARACTER type.

That is, these features make interacting wth UTF-8 files virtually
effortless.


### The default CHARACTER kind is still required

One cannot quite completely quit using ASCII yet. Even though the Fortran
standard allows processor-dependent characters to appear in comments
and character constants (e.g. quoted strings) it is not mandated what
encoding of characters are allowed there. UTF-8 is largely a de-facto
standard for file encoding so it is extremely likely you can compile
UTF-8 Fortran source files as long as the multi-byte characters are
restricted to comments and constant strings, but it is not guaranteed
by the Fortran Standard.

Indeed, the Fortran standard defines most interactions with operating
systems such as filenames and command line character encoding as
implementation-dependent and requiring the use of a default CHARACTER kind.

So in practice almost all processors require data passed to and from the
system to be encoded as byte streams of UTF-8 characters, not as UCS-4
data. This includes arguments passed in from command lines and environment
variables, filenames on INQUIRE and OPEN statements and string constants.

However, since that is all processor-dependent as far as the standard is
concerned Fortran does not support intrinsics that convert to and from
the internal UCS-4 representation and UTF-8 byte streams other than the
afore-mentioned automatic conversion on READ and WRITE statements where
the ENCODING="UTF-8" option has been used on an OPEN() statement.

Further details follow:

### Introduction to Fortran ISO_10646 (UCS-4-encoded Unicode) support
   + **[Lesson I:](#lesson1_ucs4)** reading and writing UTF-8 Unicode files
   + **[Lesson II:](#lesson2_ucs4)** creating Unicode strings in ASCII Fortran source files
   + **[Lesson III:](#lesson3_ucs4)** mixing ASCII and UCS4 kinds as regards
      + assignments
      + concatenation
      + passing arguments to external ASCII libraries
      + mixing kinds on I/O argument lists
   + **[Lesson IV:](#lesson4_ucs4)** what is and is not supported with internal READ and WRITE statements
   + **[Lesson V:](#lesson5_ucs4)** processing Unicode file names on OPEN() statements
   + **[Lesson VI:](#lesson6_ucs4)** reading UTF-8 strings from command lines and environment variables.
   + **[Lesson VII:](#lesson7_ucs4)** passing Unicode strings to and from C
   + **[Summary](#summary_ucs4)** putting it all together

-------------------------------------------------------------
<a name="lesson1_ucs4"></a>
#### Introduction to Fortran Unicode support
### Lesson I: reading and writing UTF-8 Unicode files

Not all Fortran compilers provide high-level ISO-10646 (ie. "Unicode")
support. To determine if a compiler provides support, one can attempt
to compile and execute the following program:
```fortran
   program test_for_iso_10646
   implicit none
   integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')
   write(*,*) trim(merge('ISO-10646 SUPPORTED    ', &
                         'ISO-10646 NOT SUPPORTED', &
                          ucs4>0))
   end program test_for_iso_10646
```
If the supplemental ISO-10646 standard is supported, you want to
select a terminal emulator and font and system locale so this
next program prints an emoji to the screen:

```fortran
   program test_for_iso_10646
   use iso_fortran_env, only : output_unit
   implicit none
   intrinsic selected_char_kind
   integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')
      open(output_unit,encoding='utf-8')
      write(output_unit,'(*(g0,1x))') & ! 😃
      & 'Smiling face with open mouth',char(int(z'1F603'),kind=ucs4)
   end program test_for_iso_10646
```
If that is not done, the extensions will work with files but not with
standard input and output to the screen. It is likely to work by default,
but if not you will generally find out that how to use UTF-8 data to the screen
on your system is well documented but very system-dependent.

If the ISO-10646 supplement is not supported Unicode usage will require
lower-level knowledge of byte-level Fortran processing and I/O and the
hosting operating system, which is covered in a different guide. This
introduction only applies to compilers providing ISO-10646 support.

Fortran Unicode support is straight-forward when just reading and writing
UTF-8-encoded files. There is very little different from when processing
ASCII files.

In many cases all that is required is to

  1. declare any character variables to be used with multi-byte UTF-8
     characters (ie. basically any character other than the ASCII 7-bit
     characters) to be kind "iso_10646"
  2. open/reopen your files with UTF-8 encoding.

Fortran will then convert the data from UTF-8 files to whichever
Unicode encoding it uses internally (UTF-8, UTF-32, UTF-16, ...) on input,
and convert back to UTF-8 on output.

If standard-conforming, the internal representation will be UCS-4, as the
standard description of the intrinsic SELECTED_CHAR_KIND() states:

   If NAME has the value "ISO_10646", then the result has a value equal
   to that of the kind type parameter of the ISO_10646 character kind
   (corresponding to UCS-4 as specified in ISO/IEC 10646) if the processor
   supports such a kind; otherwise the result has the value −1.

This automatic conversion between UCS-4 (aka.UTF-32) and UTF-8 encoding is
not so different from what occurs when reading and writing numeric values
from ASCII files. The binary representation of the numbers (REAL, INTEGER,
COMPLEX, ..) used internally by the program is very different from the
human-readable ASCII representations, but Fortran makes this conversion
automatically for the user also, when asked to provide formatted I/O.

Why not just use UTF-8 encoding directly? UCS-4 encoding represents
each glyph or character using four bytes. That is, each character is
basically represented as a 32-bit value. This makes it much easier
to provide arrays and optimized intrinsics than when using UTF-8 encoding,
which requires supporting multi-byte characters from one to four bytes.

So it is assumed here that "ISO_10646" implies standard-conforming UCS-4
encoding internally, but the same rules apply if your compiler supports a
UTF-2 encoding extension and you select it instead (except UTF-2 requires
less storage, but cannot represent as wide a range of Unicode glyphs).

For the purposes of this tutorial what matters is that you know the
internal representation is encoded differently than in the UTF-8 files,
and that one kind cannot be converted to the other simply by copying
bytes from one representation to the other.

Also note that the memory required to hold UCS-4 characters is four times
greater than if they were ASCII characters, as all UCS-4 characters are
4-byte values and all ASCII characters are 1-byte.

Many useful programs can adhere to these restrictions.

A simplistic example that reads a UTF-8 file with lines up to 4096
glyphs and outputs the file prefixing each line with a glyph/character
count demonstrates that very little differs from a similar program which
processes ASCII files:

```fortran
program count_glyphs
! @(#) read utf-8 file and write it back out prefixed with line glyph counts
use, intrinsic :: iso_fortran_env,only : stdout=>output_unit, stdin=>input_unit
implicit none
intrinsic selected_char_kind
intrinsic is_iostat_end
intrinsic len_trim
!------
! DIFFERENCE: we will be using the kind name "ucs4" for Unicode variables
integer, parameter            :: ucs4 = selected_char_kind ('ISO_10646')
!------
character(len=*),parameter    :: g= '(*(g0))'
integer                       :: length
integer                       :: i
integer                       :: iostat
!------
! DIFFERENCE: string declared with KIND=UCS4. This statement
! specifies a maximum line length of 4096 glyphs not bytes
! as this character variable is Unicode ISO_10646, not ASCII
character(len=4096,kind=ucs4) :: uline
!------
character(len=255)            :: iomsg

   !------
   ! DIFFERENCE: you can change the encoding used for a file dynamically,
   ! even on pre-assigned files so make sure stdin and stdout are set to
   ! expect to format UCS4-encoded internal data as UTF-8 encoded files:
   open (stdin, encoding='UTF-8')
   open (stdout, encoding='UTF-8')
   !------

   ! copy file to stdout, prefixing each line with a glyph/character count
   do
      read(stdin,'(a)',iostat=iostat,iomsg=iomsg)uline
      if(iostat.eq.0)then
         !------
         ! NOTE: LEN_TRIM() works with UCS-4 just as with ASCII
         length=len_trim(uline)
         !------
         !------
         ! NOTE: String substrings work just as with ASCII
         write(stdout,'(i9,": ",a)')length,uline(:length)
         !------
      elseif(is_iostat_end(iostat))then
         exit
      else
         !------
         ! NOTE:
         ! does the ASCII message have to be converted to UCS-4?
         ! This will be discussed in detail later, but for now
         ! remember you can change the encoding of a file dynamically
         ! anyway
         open (stdout, encoding='DEFAULT')
         !------
         write(stdout,g)'<ERROR>',trim(iomsg)
         stop
      endif
      ! and the answer is that unless you are going to output a series
      ! of bytes in the message that do not represent an ASCII-7 or
      ! UCS-4 character (which would be a very unusual thing to be
      ! doing) you can leave the encoding set to UTF-8 and output
      ! traditional CHARACTER(kind=DEFAULT) variables just fine.
   enddo

end program count_glyphs
```

So if we create a file called "upagain.utf"
```text
七転び八起き。
転んでもまた立ち上がる。
くじけずに前を向いて歩いていこう。

Romanization:

   Nanakorobi yaoki.
   Koronde mo mata tachiagaru.
   Kujikezu ni mae o muite aruite ikou.

English translation:

   "Fall seven times, stand up eight.
   Even if you fall down, you will get up again.
   Don't be discouraged, just keep walking forward."
```
and make sure that our terminal displays UTF-8 files properly by
displaying that file to the screen, then running the program
```bash
./count_glyphs < upagain.utf
```
should produce
```text
        7: 七転び八起き。
       12: 転んでもまた立ち上がる。
       17: くじけずに前を向いて歩いていこう。
        0:
       13: Romanization:
        0:
       20:    Nanakorobi yaoki.
       30:    Koronde mo mata tachiagaru.
       39:    Kujikezu ni mae o muite aruite ikou.
        0:
       19: English translation:
        0:
       37:    "Fall seven times, stand up eight.
       48:    Even if you fall down, you will get up again.
       52:    Don't be discouraged, just keep walking forward."
```
### Summary

That is how simple basic Unicode usage is in Fortran. The data will be
converted from UTF-8 files to UCS-4 internal representation and back
again transparently. __The CHARACTER substring indexing and intrinsic
functions such as LEN(), TRIM(), VERIFY(), INDEX(), and SCAN() are
generic, and will work with Unicode as simply as with ASCII__.

#### Introduction to Fortran Unicode support
### Lesson I: reading and writing UTF-8 Unicode files

Not all Fortran compilers provide high-level ISO-10646 (ie. "Unicode")
support. To determine if a compiler provides support, one can attempt
to compile and execute the following program:
```fortran
   program test_for_iso_10646
   implicit none
   integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')
   write(*,*) trim(merge('ISO-10646 SUPPORTED    ', &
                         'ISO-10646 NOT SUPPORTED', &
                          ucs4>0))
   end program test_for_iso_10646
```
If the supplemental ISO-10646 standard is supported, you want to
select a terminal emulator and font and system locale so this
next program prints an emoji to the screen:

```fortran
   program test_for_iso_10646
   use iso_fortran_env, only : output_unit
   implicit none
   intrinsic selected_char_kind
   integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')
      open(output_unit,encoding='utf-8')
      write(output_unit,'(*(g0,1x))') & ! 😃
      & 'Smiling face with open mouth',char(int(z'1F603'),kind=ucs4)
   end program test_for_iso_10646
```
If that is not done, the extensions will work with files but not with
standard input and output to the screen. It is likely to work by default,
but if not you will generally find out that how to use UTF-8 data to the screen
on your system is well documented but very system-dependent.

If the ISO-10646 supplement is not supported Unicode usage will require
lower-level knowledge of byte-level Fortran processing and I/O and the
hosting operating system, which is covered in a different guide. This
introduction only applies to compilers providing ISO-10646 support.

Fortran Unicode support is straight-forward when just reading and writing
UTF-8-encoded files. There is very little different from when processing
ASCII files.

In many cases all that is required is to

  1. declare any character variables to be used with multi-byte UTF-8
     characters (ie. basically any character other than the ASCII 7-bit
     characters) to be kind "iso_10646"
  2. open/reopen your files with UTF-8 encoding.

Fortran will then convert the data from UTF-8 files to whichever
Unicode encoding it uses internally (UTF-8, UTF-32, UTF-16, ...) on input,
and convert back to UTF-8 on output.

If standard-conforming, the internal representation will be UCS-4, as the
standard description of the intrinsic SELECTED_CHAR_KIND() states:

   If NAME has the value "ISO_10646", then the result has a value equal
   to that of the kind type parameter of the ISO_10646 character kind
   (corresponding to UCS-4 as specified in ISO/IEC 10646) if the processor
   supports such a kind; otherwise the result has the value −1.

This automatic conversion between UCS-4 (aka.UTF-32) and UTF-8 encoding is
not so different from what occurs when reading and writing numeric values
from ASCII files. The binary representation of the numbers (REAL, INTEGER,
COMPLEX, ..) used internally by the program is very different from the
human-readable ASCII representations, but Fortran makes this conversion
automatically for the user also, when asked to provide formatted I/O.

Why not just use UTF-8 encoding directly? UCS-4 encoding represents
each glyph or character using four bytes. That is, each character is
basically represented as a 32-bit value. This makes it much easier
to provide arrays and optimized intrinsics than when using UTF-8 encoding,
which requires supporting multi-byte characters from one to four bytes.

So it is assumed here that "ISO_10646" implies standard-conforming UCS-4
encoding internally, but the same rules apply if your compiler supports a
UTF-2 encoding extension and you select it instead (except UTF-2 requires
less storage, but cannot represent as wide a range of Unicode glyphs).

For the purposes of this tutorial what matters is that you know the
internal representation is encoded differently than in the UTF-8 files,
and that one kind cannot be converted to the other simply by copying
bytes from one representation to the other.

Also note that the memory required to hold UCS-4 characters is four times
greater than if they were ASCII characters, as all UCS-4 characters are
4-byte values and all ASCII characters are 1-byte.

Many useful programs can adhere to these restrictions.

A simplistic example that reads a UTF-8 file with lines up to 4096
glyphs and outputs the file prefixing each line with a glyph/character
count demonstrates that very little differs from a similar program which
processes ASCII files:

```fortran
program count_glyphs
! @(#) read utf-8 file and write it back out prefixed with line glyph counts
use, intrinsic :: iso_fortran_env,only : stdout=>output_unit, stdin=>input_unit
implicit none
intrinsic selected_char_kind
intrinsic is_iostat_end
intrinsic len_trim
!------
! DIFFERENCE: we will be using the kind name "ucs4" for Unicode variables
integer, parameter            :: ucs4 = selected_char_kind ('ISO_10646')
!------
character(len=*),parameter    :: g= '(*(g0))'
integer                       :: length
integer                       :: i
integer                       :: iostat
!------
! DIFFERENCE: string declared with KIND=UCS4. This statement
! specifies a maximum line length of 4096 glyphs not bytes
! as this character variable is Unicode ISO_10646, not ASCII
character(len=4096,kind=ucs4) :: uline
!------
character(len=255)            :: iomsg

   !------
   ! DIFFERENCE: you can change the encoding used for a file dynamically,
   ! even on pre-assigned files so make sure stdin and stdout are set to
   ! expect to format UCS4-encoded internal data as UTF-8 encoded files:
   open (stdin, encoding='UTF-8')
   open (stdout, encoding='UTF-8')
   !------

   ! copy file to stdout, prefixing each line with a glyph/character count
   do
      read(stdin,'(a)',iostat=iostat,iomsg=iomsg)uline
      if(iostat.eq.0)then
         !------
         ! NOTE: LEN_TRIM() works with UCS-4 just as with ASCII
         length=len_trim(uline)
         !------
         !------
         ! NOTE: String substrings work just as with ASCII
         write(stdout,'(i9,": ",a)')length,uline(:length)
         !------
      elseif(is_iostat_end(iostat))then
         exit
      else
         !------
         ! NOTE:
         ! does the ASCII message have to be converted to UCS-4?
         ! This will be discussed in detail later, but for now
         ! remember you can change the encoding of a file dynamically
         ! anyway
         open (stdout, encoding='DEFAULT')
         !------
         write(stdout,g)'<ERROR>',trim(iomsg)
         stop
      endif
      ! and the answer is that unless you are going to output a series
      ! of bytes in the message that do not represent an ASCII-7 or
      ! UCS-4 character (which would be a very unusual thing to be
      ! doing) you can leave the encoding set to UTF-8 and output
      ! traditional CHARACTER(kind=DEFAULT) variables just fine.
   enddo

end program count_glyphs
```

So if we create a file called "upagain.utf"
```text
七転び八起き。
転んでもまた立ち上がる。
くじけずに前を向いて歩いていこう。

Romanization:

   Nanakorobi yaoki.
   Koronde mo mata tachiagaru.
   Kujikezu ni mae o muite aruite ikou.

English translation:

   "Fall seven times, stand up eight.
   Even if you fall down, you will get up again.
   Don't be discouraged, just keep walking forward."
```
and make sure that our terminal displays UTF-8 files properly by
displaying that file to the screen, then running the program
```bash
./count_glyphs < upagain.utf
```
should produce
```text
        7: 七転び八起き。
       12: 転んでもまた立ち上がる。
       17: くじけずに前を向いて歩いていこう。
        0:
       13: Romanization:
        0:
       20:    Nanakorobi yaoki.
       30:    Koronde mo mata tachiagaru.
       39:    Kujikezu ni mae o muite aruite ikou.
        0:
       19: English translation:
        0:
       37:    "Fall seven times, stand up eight.
       48:    Even if you fall down, you will get up again.
       52:    Don't be discouraged, just keep walking forward."
```
### Summary

That is how simple basic Unicode usage is in Fortran. The data will be
converted from UTF-8 files to UCS-4 internal representation and back
again transparently. __The CHARACTER substring indexing and intrinsic
functions such as LEN(), TRIM(), VERIFY(), INDEX(), and SCAN() are
generic, and will work with Unicode as simply as with ASCII__.

<a name="lesson2_ucs4"></a>
#### Introduction to Fortran Unicode support
### Lesson II: creating Unicode strings in ASCII Fortran source files

Lesson I showed reading and writing UTF-8 files is easy, but
can multi-byte characters be defined directly in Fortran source
code? Yes, but to be strictly portable the multi-byte characters
need defined with numeric Unicode code point values instead of as
what-you-see-is-what-you-get UTF-8 characters.

The Fortran code instructions must be written only using the Fortran
character set, which is basically ASCII-7 characters sans the control
characters other than newline (ie. backspace, tab, bell, ...).

Since ASCII is a subset of Unicode the line is a bit blurry
as to what encoding source files may use for constant strings and
comments, however.

In particular, can constant strings and comments
be composed in UTF-8 or must the entire file be ASCII? What about
extended ASCII, which uses all 256 values representable in one byte,
versus strict adherence to the defined 128 ASCII characters or even the
Fortran character set, which is a subset of the ASCII characters?

Section 6.1(Processor character set) and 7.4.4(Character constants)
of the Fortran 2023 Standard provide guidance on this. A lot is left
up to the processor. A conservative interpretation implies that for
ASCII input files a quoted constant string is only guaranteed portable
when composed of one-byte ASCII-7bit characters.

What is left in question is what encoding ensues when this criteria is
not met.

For example, assume a UTF-8 encoded source file has been created. If
multi-byte characters are to be represented in the code there are
several pitfalls to avoid. The following example shows which syntax
results in properly encoded data, and several that do not. The intent
is to print a Euro symbol:

```fortran
program euro
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none
integer, parameter         :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter :: g5='(*(a9,3x,g0,t18,g0,t25,g0,t32,g0))'
! TRY A: NO
!     LHS holds one byte, but three are required for the RHS which is
!     a stream of bytes representing a Euro character in UTF-8;
!     not a UCS-4 character. The result is a truncated UTF8-encoded string.
character(len=1)           :: euro0 = '€'
!
! TRY B: NO
! If the intent was to use ISO_10646 encoding this fails. This will hold
! a multi-byte UTF-8 character as 3 ASCII bytes, not a UCS-4 "character".
character(len=*),parameter :: euro1 = '€'
!
! TRY C: NO
! The RHS is 3 bytes in UTF-8 encoding, not a single 4-byte character,
character(len=1,kind=ucs4) :: euro2 = '€'

! TRY D: NO
! a prefix assumes the quoted string is ASCII, not UTF-8
! although it could be argued it would be nice if it worked.
character(len=1,kind=ucs4) :: euro3 = ucs4_'€'

! TRY E: YES
! this defines Unicode character U+20AC properly as a UCS-4 character
character(len=1,kind=ucs4) :: euro4 = char(int(z'20AC'), kind=ucs4)

   write(stdout,g5) 'VARIABLE', 'LEN', 'BYTES', 'KIND', 'OUTPUT'
   open(stdout,encoding='utf-8')
   write(stdout,g5)'euro0',len(euro0),storage_size(euro0)/8,kind(euro0),euro0
   write(stdout,g5)'euro1',len(euro1),storage_size(euro1)/8,kind(euro1),euro1
   write(stdout,g5)'euro2',len(euro2),storage_size(euro2)/8,kind(euro2),euro2
   write(stdout,g5)'euro3',len(euro3),storage_size(euro3)/8,kind(euro3),euro3
   write(stdout,g5)'euro4',len(euro4),storage_size(euro4)/8,kind(euro4),euro4
end program euro
```
## Output
```text
 VARIABLE   LEN  BYTES  KIND   OUTPUT
    euro0   1    1      1      ?
    euro1   3    3      1      €
    euro2   1    4      4      â
    euro3   1    4      4      â
    euro4   1    4      4      €
```
We want to see a Euro character, have a string with a length of 1 that is
stored in four bytes, and be of kind UCS-4. So only `euro4` is a correctly
generated value.

In this exercise we are just demonstrating there are a lot of ways to
specify a string constant that will _not_ end up creating a proper UCS-4
string, but one (admittedly verbose and obfusticated) syntax that should
always succeed.

The lesson learned is the CHAR() intrinsic function can be reliably used
to directly construct UCS-4 multi-byte characters from their Unicode
code points.

# Mixing CHAR() and quoted constants

A quoted string literal can be used to define UCS-4 strings as long
as the quoted characters are one byte characters (ie. ASCII).

For instance,
```fortran
   :
integer, parameter         :: ucs4 = selected_char_kind ('ISO_10646')
character(len=:,kind=ucs4),allocatable :: string
string = ucs4_'Unicode character: ' // char(9787, kind=ucs4)
   :
```
mixes a quoted UCS-4 constant string and the CHAR() function. As long as the
quoted string is composed of ASCII7 one-byte characters there is no
ambiguity -- so the above line will work.

## a program to convert a utf-8 file to Fortran CHAR() declarations

Since typing all those code point values can get tedious, lets construct
a program that reads a UTF-8 file and converts it to a program that
defines all the input lines as UCS-4 variables using CHAR():

```fortran
program unifile_to_ftn
! @(#) convert UTF-8 text on command line to char(3f) calls
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none
integer, parameter            :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter    :: &
   & form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter    :: g= '(*(g0))'
character(len=80)             :: count
integer                       :: i, j, iostat
character(len=4096,kind=ucs4) :: uline
   open (stdin, encoding='UTF-8')
   open (stdout, encoding='UTF-8')
   write(stdout,g) 'program testit'
   write(stdout,g) 'use,intrinsic :: iso_fortran_env, only : output_unit'
   write(stdout,g) "integer,parameter :: ucs4=selected_char_kind ('ISO_10646')"
   write(stdout,g) "   open (output_unit, encoding='utf-8')"
   do j=1,huge(0)-1
      read(stdin,'(a)',iostat=iostat)uline
      if(iostat.ne.0)exit
      write(count,g) "variable_",j,"= &"
      write(stdout,g) 'block'
      write(stdout,g) '! Unicode code points for ',trim(uline)
      write(stdout,g) 'character(len=*,kind=ucs4),parameter :: '//trim(count)
      write(stdout,form)(uline(i:i),i=1,len_trim(uline))
      write(stdout,g) "   write(output_unit,'(a)' )variable_",j
      write(stdout,g) 'endblock'
   enddo
   write(stdout,g) "end program testit"
end program unifile_to_ftn
```
### Input

Given an example input file
```text
七転び八起き。
転んでもまた立ち上がる。
くじけずに前を向いて歩いていこう。
```
### Output

The following program source file will be generated:
```fortran
program testit
use, intrinsic :: iso_fortran_env, only : output_unit
integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')
   open (output_unit, encoding='utf-8')
block
! Unicode code points for 七転び八起き。
character(len=*,kind=ucs4),parameter :: variable_1= &
char(int(z'4E03'),kind=ucs4)// &
char(int(z'8EE2'),kind=ucs4)// &
char(int(z'3073'),kind=ucs4)// &
char(int(z'516B'),kind=ucs4)// &
char(int(z'8D77'),kind=ucs4)// &
char(int(z'304D'),kind=ucs4)// &
char(int(z'3002'),kind=ucs4)
   write(output_unit,'(a)' )variable_1
endblock
block
! Unicode code points for 転んでもまた立ち上がる。
character(len=*,kind=ucs4),parameter :: variable_2= &
char(int(z'8EE2'),kind=ucs4)// &
char(int(z'3093'),kind=ucs4)// &
char(int(z'3067'),kind=ucs4)// &
char(int(z'3082'),kind=ucs4)// &
char(int(z'307E'),kind=ucs4)// &
char(int(z'305F'),kind=ucs4)// &
char(int(z'7ACB'),kind=ucs4)// &
char(int(z'3061'),kind=ucs4)// &
char(int(z'4E0A'),kind=ucs4)// &
char(int(z'304C'),kind=ucs4)// &
char(int(z'308B'),kind=ucs4)// &
char(int(z'3002'),kind=ucs4)
   write(output_unit,'(a)' )variable_2
endblock
block
! Unicode code points for くじけずに前を向いて歩いていこう。
character(len=*,kind=ucs4),parameter :: variable_3= &
char(int(z'304F'),kind=ucs4)// &
char(int(z'3058'),kind=ucs4)// &
char(int(z'3051'),kind=ucs4)// &
char(int(z'305A'),kind=ucs4)// &
char(int(z'306B'),kind=ucs4)// &
char(int(z'524D'),kind=ucs4)// &
char(int(z'3092'),kind=ucs4)// &
char(int(z'5411'),kind=ucs4)// &
char(int(z'3044'),kind=ucs4)// &
char(int(z'3066'),kind=ucs4)// &
char(int(z'6B69'),kind=ucs4)// &
char(int(z'3044'),kind=ucs4)// &
char(int(z'3066'),kind=ucs4)// &
char(int(z'3044'),kind=ucs4)// &
char(int(z'3053'),kind=ucs4)// &
char(int(z'3046'),kind=ucs4)// &
char(int(z'3002'),kind=ucs4)
   write(output_unit,'(a)' )variable_3
endblock
end program testit
```
It should be relatively easy to copy and paste and edit the resulting
variable declarations into source files where it is needed. The output
is generated as a complete program that should reproduce the input file
(sans any trailing spaces) when executed.

CHAR() is elemental and decimal values work as well as hexidecimal, so
this alternative syntax works as well:
```fortran
program unifile_to_ftn
! @(#) convert UTF-8 text on command line to char(3f) calls
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none
integer, parameter            :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter    :: g= '(*(g0))'
character(len=80)             :: count
integer                       :: i, j, iostat
character(len=4096,kind=ucs4) :: uline
   open (stdin, encoding='UTF-8')
   open (stdout, encoding='UTF-8')
   write(stdout,g) 'program testit'
   write(stdout,g) 'use,intrinsic :: iso_fortran_env, only : output_unit'
   write(stdout,g) "integer,parameter :: ucs4=selected_char_kind ('ISO_10646')"
   write(stdout,g) "   open (output_unit, encoding='utf-8')"
   do j=1,huge(0)-1
      read(stdin,'(a)',iostat=iostat)uline
      if(iostat.ne.0)exit
      write(count,g) "variable_",j,"(*)= char([ &"
      write(stdout,g) 'block'
      write(stdout,g) '! Unicode code points for ',trim(uline)
      write(stdout,g) 'character(len=*,kind=ucs4),parameter :: '//trim(count)
      write(stdout,'("",*(i0,:,","))',advance="no")(ichar(uline(i:i)),i=1,len_trim(uline))
      write(stdout,g) '],kind=ucs4)'
      write(stdout,g) "   write(output_unit,'(*(a))' )variable_",j
      write(stdout,g) 'endblock'
   enddo
   write(stdout,g) "end program testit"
end program unifile_to_ftn
```
where arrays of single characters are constructed instead of multi-character variables,
and for simplicity it is assumed source code line length is assumed unlimited.

# Output
```fortran
program testit
use,intrinsic :: iso_fortran_env, only : output_unit
integer,parameter :: ucs4=selected_char_kind ('ISO_10646')
   open (output_unit, encoding='utf-8')
block
! Unicode code points for 七転び八起き。
character(len=*,kind=ucs4),parameter :: variable_1(*)= char([ &
19971,36578,12403,20843,36215,12365,12290],kind=ucs4)
   write(output_unit,'(*(a))' )variable_1
endblock
block
! Unicode code points for 転んでもまた立ち上がる。
character(len=*,kind=ucs4),parameter :: variable_2(*)= char([ &
36578,12435,12391,12418,12414,12383,31435,12385,19978,12364,12427,12290],kind=ucs4)
   write(output_unit,'(*(a))' )variable_2
endblock
block
! Unicode code points for くじけずに前を向いて歩いていこう。
character(len=*,kind=ucs4),parameter :: variable_3(*)= char([ &
12367,12376,12369,12378,12395,21069,12434,21521,12356,12390,27497,12356,12390,12356,12371,12358,12290],kind=ucs4)
   write(output_unit,'(*(a))' )variable_3
endblock
end program testit
```

## Summary

As tempting as it may be to place Unicode multi-byte characters in quoted
constant strings in code source, the guaranteed-portable standard method
is to use the CHAR() function and integer Unicode code point values to
construct UCS-4 variables directly from the source code.

Placing messages in an external file and opening the file as UTF-8
encoded is an easy alternative that lets you maintain the messages as
Unicode directly, but this will require always making the message file
accessible when the program is being used.

We will look at alternatives that allow for what-you-see-is-what-you-get
string declarations as well, at the cost of assuming UTF-8 source files
are acceptable.

<a name="lesson3_ucs4"></a>
#### Introduction to Fortran Unicode support
### Lesson III: mixing ASCII and UCS4 kinds as regards concatenation and assignments

### Concatenation, Assignment, and automatic conversion

### Assignment
Concerning assignment -- the Fortran standard states

   if the variable is of type character and of ISO 10646, ASCII, or default
   character kind, expr shall be of ISO 10646, ASCII, or default character
   kind, otherwise if the variable is of type character expr shall have
   the same kind type parameter,

   For an intrinsic assignment statement where the variable is of type
   character, if expr has a different kind type parameter, each character "c"
   in expr is converted to the kind type parameter of the variable by

       ACHAR(IACHAR(c),KIND(variable)).

   NOTES

   For nondefault character kinds, the blank padding character is
   processor dependent

   When assigning a character expression to a variable of a different kind,
   each character of the expression that is not representable in the kind
   of the variable is replaced by a processor-dependent character.

Unfortunately that means UTF-8 data is not recognized as such, and if you
have a constant string encoded as UTF-8 in a default CHARACTER string,
assigning it to a UCS-4 string will not produce proper conversion.
Assigning a UCS-4 value to a ASCII variable will cause all the non-ASCII
characters to be replaced with a "not represented" character.
```fortran
program assignment
use iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none

intrinsic selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')

character(len=:),allocatable           :: aline, a1, a2
character(len=:,kind=ucs4),allocatable :: uline, u1, u2
character(len=1),allocatable           :: ch(:), ch2(:)
character(len=1,kind=ucs4),allocatable :: glyph(:)
integer                                :: i
integer                                :: iostat
integer                                :: nerr
character(len=1)                       :: paws
character(len=1,kind=ucs4)             :: smiley=char(int(z'1F603'),kind=ucs4) ! 😃 Smiling face with open mouth

   open (stdout, encoding='DEFAULT')
   open (stdout, encoding='UTF-8')
   !
   ! only characters defined in the other encoding are copied on an assign

   write(stdout,'(A)')repeat(' ',80)
   write(stdout,'(A)')'assign RHS ucs4 to LHS ascii'
   uline=char(int(z'261B'),ucs4) // ucs4_'UCS-4 string' // char(int(z'261A'),ucs4)
   write(stdout,'(a)')trim(uline)
   aline=uline ! only the ASCII 7-bit characters are copied
   write(stdout,'(a)')trim(aline) // ' assigned to ASCII'

   write(stdout,'(A)')repeat(' ',80)
   write(stdout,'(A)')'assign LHS ascii to RHS ucs4'
   aline=ascii_'ASCII string'
   write(stdout,'(a)')trim(aline)
   uline=aline ! all ASCII 7-bit characters can be represented in UCS-4
   write(stdout,'(a)')trim(uline)//ucs4_' assigned to UCS4'

   write(stdout,'(A)')'round trip for all ASCII bytes'

   write(stdout,'(A)')repeat(ucs4_'=',80)
   ch=[(char(i),i=0,255)]
   open (stdout, encoding='DEFAULT')
   write(stdout,'(10(g0,1x,g0,1x))')(ch(i),i=0,255)
   open (stdout, encoding='UTF-8')
   write(stdout,'(10(g0,1x,g0,1x))')(ch(i),i=0,255)
   read(stdin,'(a)',iostat=iostat)paws

   write(stdout,'(A)')repeat(ucs4_'=',80)
   glyph=ch
   write(stdout,'(10(g0,1x,g0,1x))')(glyph(i),i=0,255)
   read(stdin,'(a)',iostat=iostat)paws

   write(stdout,'(A)')repeat(ucs4_'=',80)
   ch2=glyph
   write(stdout,'(10(g0,1x,g0,1x))')(ch2(i),i=0,255)
   read(stdin,'(a)',iostat=iostat)paws

   write(stdout,'(A)')repeat(ucs4_'=',80)

   write(stdout,'(a,L0)') 'roundrobin returned all values unchanged?',all( ch .eq. ch2)

end program assignment
```

The output of this example takes some study but the main lesson is
simple. Basically assignment of a constant quoted string to a UCS-4
encoded variable only really works if the constant string is composed
only of ASCII characters.

Fortran instructions other than READ and WRITE
are unaware of any possible non-ASCII encoding of constant strings.

### Concatenation

A limitation of concatenation is that all the strings have to be of the
same KIND, so you cannot simply append UCS-4 and ASCII-7 strings.

And we have already seen assignment between the kinds only assigns
representable characters.

But the definition of assignment includes an equivalent conversion
defined in terms of ACHAR(3) and IACHAR(3):

       ACHAR(IACHAR(c),KIND(variable)).

So we can make functions that do what an assignment does to overcome
the first limitation where everything concatenated must be the same kind.

We will do that in the following concatenation example; but that function
will still not transfer UTF-8 encoded data properly.

```fortran
program concatenate
use iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none

intrinsic selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')

character(len=*),parameter             :: g='(*(g0))'
character(len=:),allocatable           :: aline, a1, a2
character(len=:,kind=ucs4),allocatable :: uline, u1, u2
character(len=1),allocatable           :: ch(:), ch2(:)
character(len=1,kind=ucs4),allocatable :: glyph(:)
integer                                :: i
integer                                :: iostat
integer                                :: nerr
character(len=1)                       :: paws
                                       !  😃 Smiling face with open mouth
character(len=1,kind=ucs4)             :: smiley=char(int(z'1F603'),kind=ucs4)

   open (stdout, encoding='DEFAULT')
   open (stdout, encoding='UTF-8')
   !
   ! Concatenation:
   !
   write(stdout,'(A)')repeat('=',80)
   write(stdout,'(a)')'strings of different kinds cannot be concatenated.'
   !uline='ascii string'// smiley // 'ascii string' ! NO. Kinds must match

   write(stdout,'(a)') 'Of course constants can have their KIND specified.'
   uline=ucs4_'first UCS4 string' // smiley // ucs4_'another UCS4 string '
   write(stdout,'(A)') uline
   !
   write(stdout,'(A)')repeat('=',80)
   write(stdout,'(a)') 'you can use simple assigns to do conversions'
   ! so if I have a UCS4 string
   u1=smiley // ucs4_'UCS4 strings' // smiley // ucs4_'appended together' // smiley
   ! and an ASCII string
   a1='ascii strings' // 'appended together'
   ! the ASCII string can be converted to UCS4 with an assign
   u2=a1 ! use allocation to convert ASCII to UCS4
   ! now with a copy of everything as UCS4 the append will work
   uline=u1//u2 ! now append together the two strings which are now of the same kind
   write(stdout,'(a)') uline
   !
   write(stdout,'(A)')repeat('=',80)
   write(stdout,'(a)') 'we can make functions to convert to and from ASCII and UCS4'
   ! using the same conversions as used by an assign.
   uline=smiley // ascii_to_ucs4('ascii string') // smiley // ucs4_'unicode string' // smiley
   write(stdout,'(a)') uline
   !
   write(stdout,'(A)')'unrepresentable characters:'
   write(stdout,'(a)')'what about characters that have no equivalent in the other kind?'
   write(stdout,'(A)')'conversion by assignment'
   aline=uline
   write(stdout,g) aline,' ',len(aline),' ',len(uline)
   write(stdout,'(a)') 'conversion by ACHAR/ICHAR:'
   aline=ucs4_to_ascii(uline) ! is "smiley" replaced with a character used for errors?
   write(stdout,g) aline,' ',len(aline),' ',len(uline)
   write(stdout,'(a)') 'which character replaces the unrepresentable characters is processor-dependent'
   write(stdout,'(a)') 'and might be unprintable'
   aline=smiley
   write(stdout,'(a,i0,a)') 'ADE:',ichar(aline),' CHARACTER:',aline
   write(stdout,'(A)')repeat('=',80)

contains

function ascii_to_ucs4(astr) result(ustr)
! @(#) make the same conversion as an assignment statement from ASCII to UCS4
character(len=*,kind=ascii),intent(in) :: astr
character(len=len(astr),kind=ucs4)     :: ustr
integer                                :: i
   do i=1,len(astr)
      ustr(i:i)=achar(iachar(astr(i:i)),kind=ucs4)
   enddo
end function ascii_to_ucs4

function ucs4_to_ascii(ustr) result(astr)
! @(#) make the same conversion as an assignment statement from UCS4 to ASCII
character(len=*,kind=ucs4),intent(in)  :: ustr
character(len=len(ustr),kind=ascii)    :: astr
integer                                :: i
   do i=1,len(ustr)
      astr(i:i)=achar(iachar(ustr(i:i)),kind=ascii)
   enddo
end function ucs4_to_ascii

end program concatenate
```
## Expected Output
```text
================================================================================
strings of different kinds cannot be concatenated.
Of course constants can have their KIND specified.
first UCS4 string😃another UCS4 string
================================================================================
you can use simple assigns to do conversions
😃UCS4 strings😃appended together😃ascii stringsappended together
================================================================================
we can make functions to convert to and from ASCII and UCS4
😃ascii string😃unicode string😃
unrepresentable characters:
what about characters that have no equivalent in the other kind?
conversion by assignment
?ascii string?unicode string? 29 29
conversion by ACHAR/ICHAR:
?ascii string?unicode string? 29 29
which character replaces the unrepresentable characters is processor-dependent
and might be unprintable
ADE:3 CHARACTER:
?
================================================================================
```
## Summary

Assignment allows for easily converting ASCII-7 to UCS-4; and
allows extracting ASCII-7 from UCS-4 strings. But assignment does not properly
account for UTF-8 coding in any way.

Concatenation is only allowed between strings of the same KIND.

It is easy to make functions that do the same conversion as assignment
performs, which can make it easier to pass INTENT(IN) values on procedure
calls and statements as ASCII, which is a common need. Even if all the characters
are in the ASCII-7 set, a UCS-4 encoded variable cannot be used as a filename on
an OPEN(3) for example. You can assign the string to an ASCII scratch variable
or call such a function as the above UCS4_TO_ASCII(3) function to resolve that
and similiar issues where you are encoding your data as UCS-4 but some other
procedure only expects ASCII.

<a name="lesson4_ucs4"></a>
#### Introduction to Fortran Unicode support
### Lesson IV: what is and is not supported with internal READ and WRITE statements

   Per the Fortran standard there are not many new facets to Fortran
   provided by Unicode support regarding internal I/O, but there are a
   few nuances that need heeded.

   From the 2023 Fortran standard, regarding internal I/O:

      Fortran 90 permitted defined assignment between character strings
      of the same rank and different kinds.

      This document does not permit that if both of the different kinds are
      ASCII, ISO 10646, or default kind. An input/output list shall
      not contain an effective item that is non-default character except
      for ISO_10646 or ASCII character if the data transfer statement
      specifies an internal file of ISO_10646 character kind.

      An input/output list shall not contain an effective item of type
      character of any kind other than ASCII if the data transfer statement
      speciﬁes an ASCII character internal file.

      An output list shall not contain an effective item that is a
      boz-literal-constant.


The following program explores various combinations of internal READ(3)
and WRITE(3) statements involving UCS-4 encoded characters.
```fortran
program internal_io
use iso_fortran_env, only : stdout=>output_unit
implicit none
integer,parameter                       :: ascii=selected_char_kind ("ascii")
integer,parameter                       :: ucs4=selected_char_kind("ISO_10646")
character(len=1,kind=ucs4)              :: glyph
character(len=*),parameter              :: all='(*(g0))'
character(len=:,kind=ascii),allocatable :: aline
character(len=:,kind=ascii),allocatable :: astr
character(len=:,kind=ucs4),allocatable  :: uline
character(len=:,kind=ucs4),allocatable  :: ustr
integer                                 :: i

   open (stdout, encoding='UTF-8')

   print all, 'unicode UCS-4 string'
   ustr  = ucs4_'Hello World and Ni Hao -- ' &
      & // char(int(z'4F60'),ucs4) // char(int(z'597D'),ucs4)
   write (*,*) ustr
   print all, 'length  :',len(ustr)
   print all, 'bytes   :',storage_size(ustr)/8

   print all, 'ASCII bytes'
   astr= 'Hello World and Ni Hao -- 你好'
   write (*,*) astr
   print all, 'length  :',len(astr)
   print all, 'bytes   :',storage_size(astr)/8

   ! SHOULD WRITE INTO ASCII CREATE RAW UTF8 OR WHAT?
   print all, 'UCS4 characters written to ASCII internal file'
   aline=repeat(' ',len(ustr))
   write(aline,all)ustr
   aline=trim(aline)
   print all, 'length        :',len(aline)
   print all, 'bytes         :',storage_size(aline)/8
   print all,aline

   ! SHOULD WRITE INTO UCS4 DO ANYTHING DIFFERENT?
   print all
   print all, 'UCS4 characters written to UCS4 internal file'
   uline=repeat(' ',len(ustr))
   write(uline,all)ustr
   uline=trim(uline)
   print all, 'length        :',len(uline)
   print all, 'bytes         :',storage_size(uline)/8
   print all,uline

   ! SHOULD WRITE INTO ASCII CREATE RAW UTF8 OR WHAT?
   print all, 'ASCII characters into ASCII internal file'
   aline=repeat(' ',len(astr))
   write(aline,all)astr
   aline=trim(aline)
   print all, 'length        :',len(aline)
   print all, 'bytes         :',storage_size(aline)/8
   print all,aline

   ! SHOULD WRITE INTO UCS4 DO ANYTHING DIFFERENT?
   print all
   print all, 'ASCII characters into UCS4 internal file'
   uline=repeat(' ',len(astr))
   write(uline,all)astr
   uline=trim(uline)
   print all, 'length        :',len(uline)
   print all, 'bytes         :',storage_size(uline)/8
   print all,uline
   write(stdout,all)(ichar(uline(i:i)),",",i=1,len(uline))
   print all, 'And back again'
   write(stdout,all)'before:',astr
   write(stdout,all)(ichar(astr(i:i)),",",i=1,len(astr))
   read(uline,'(a)')astr
   write(stdout,all)'after:',astr
   write(stdout,all)(ichar(astr(i:i)),",",i=1,len(astr))

end program internal_io
```
### Output
```text
unicode UCS-4 string
 Hello World and Ni Hao -- ä½ å¥½
length  :28
bytes   :112
ASCII bytes
 Hello World and Ni Hao -- ä½ å¥½
length  :32
bytes   :32
UCS4 characters written to ASCII internal file
length        :28
bytes         :28
Hello World and Ni Hao -- ??

UCS4 characters written to UCS4 internal file
length        :28
bytes         :112
Hello World and Ni Hao -- ä½ å¥½
ASCII characters into ASCII internal file
length        :32
bytes         :32
Hello World and Ni Hao -- ä½ å¥½

ASCII characters into UCS4 internal file
length        :32
bytes         :128
Hello World and Ni Hao -- ¿¿¿¿¿¿¿¿¿¿¿¿¿¿
72,101,108,108,111,32,87,111,114,108,100,32,97,110,100,32,78,105,32,72,97,111,32,45,45,32,-28,-67,-96,-27,-91,-67,
And back again
before:Hello World and Ni Hao -- ä½ å¥½
72,101,108,108,111,32,87,111,114,108,100,32,97,110,100,32,78,105,32,72,97,111,32,45,45,32,228,189,160,229,165,189,
after:Hello World and Ni Hao -- ??????
72,101,108,108,111,32,87,111,114,108,100,32,97,110,100,32,78,105,32,72,97,111,32,45,45,32,63,63,63,63,63,63,
```
## Summary

  Internal READ and WRITE statements are not a mechanism for converting
  between UTF-8 and UCS-4 character encodings like external files
  can be. There is no equivalent of the OPEN() statement option
  "ENCODING='UTF-8" for internal files.

  This might seem surprising because internal I/O is used to encode and
  decode numeric values to and from CHARACTER variables, and external
  files are the singular standard method for converting between internal
  UCS-4 representation and UTF-8 files. So it would seem like internal
  I/O is the natural forum for character kind conversion, but that is
  __not__ the case.

  The best practice is **like-with-like**. That is

    + Do not read or write UCS-4 variables from an internal file of
      DEFAULT or ASCII kind.

    + To use internal I/O with UCS-4 characters read and write from a
      character variable of UCS-4 kind. You may also read and write ASCII
      characters with ADE(ASCII Decimal Equivalent) values from 0 to 128,
      but unlike when using a default or ASCII internal file use of the
      unsigned values 129 to 256 (aka. integer byte values -128 to -1)
      is undefined.

  Note in a related manner that the "A" file descriptor produces a
  binary transfer of bytes for integers and reals and everything else
  until Unicode was introduced. UCS-4-kind characters are converted to
  and from UTF-8 when using external files open with ENCODING='utf-8',
  which breaks with the use of the "A" format field descriptor to transfer
  data of all types byte-by-byte to formatted files.

  The second paragraph quoted from the standard above state you can
  write ASCII bytes into a ISO 10646 variable, but it is perhaps unclear
  whether that would be a byte-per-byte transfer or whether only the 128
  ASCII characters would be allowed or whether it might treat the data as
  UTF-8 and encode it into UCS4. In all compilers I have tried the ASCII
  writes into a UCS4 variable fail accept for ADE values from 0 o 128.

  So basically with Unicode support added you cannot do much new that
  is useful with internal I/O accept read and write UCS-4 values in and
  out of UCS-4 internal files. This allows for common operations like
  converting strings to numeric variables but provides no functionality
  that is aware of UTF-8 encoded streams of bytes.



<a name="lesson5_ucs4"></a>
#### Introduction to Fortran Unicode support
### Lesson V: processing Unicode file names on OPEN() statements

If your OS supports utf-8 as the default encoding it is likely you will
at some point encounter a filename containing multi-byte Unicode characters.

The definition of the OPEN() statement specifies that the
filename expression is a "scalar-default-char-expr". But it
also states

    A file may have a name; a file that has a name is called a named
    file. The name of a named file is represented by a character
    string value. The set of allowable names for a file is processor
    dependent.

And the description of the FILE= specifier states

    12.5.6.10 FILE= specifier in the OPEN statement

    The value of the FILE= specifier is the name of the file to
    be connected to the specified unit. Any trailing blanks are
    ignored. The file-name-expr shall be a name that is allowed by
    the processor. The interpretation of case is processor dependent.

So what filenames are allowed is processor-dependent -- but probably
is restricted to a default character expression, which currently is
typically ASCII or extended ASCII.

So if your Fortran compiler allows Unicode filenames the filename is
likely to require being specified as a stream of bytes of the default
CHARACTER kind representing utf-8 characters.

But what if you have the filename in UCS-4 internal representation?
Fortran does not currently provide an intrinsic procedure for converting
ucs-4 to utf-8 Unicode.

Fortran does the conversion needed when writing ucs-4 internal data
to utf-8-encoded files. We can use that functionality to create a
simple conversion routine.

```fortran
program read_filename
! @(#) convert ucs-4 filename to utc-8 for OPEN() statement
use, intrinsic :: iso_fortran_env, only : output_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=:),allocatable           :: afilename
character(len=:,kind=ucs4),allocatable :: ufilename
integer                                :: lun

   ! we have a UCS-4 filename from somewhere ...
   ufilename = & ! ENCODING:môj_obľúbený_súbor "my_favorite_file"
   char(int(z'6D'),kind=ucs4)  // char(int(z'F4'),kind=ucs4) // char(int(z'6A'),kind=ucs4)// &
   char(int(z'5F'),kind=ucs4)  // char(int(z'6F'),kind=ucs4) // char(int(z'62'),kind=ucs4)// &
   char(int(z'13E'),kind=ucs4) // char(int(z'FA'),kind=ucs4) // char(int(z'62'),kind=ucs4)// &
   char(int(z'65'),kind=ucs4)  // char(int(z'6E'),kind=ucs4) // char(int(z'FD'),kind=ucs4)// &
   char(int(z'5F'),kind=ucs4)  // char(int(z'73'),kind=ucs4) // char(int(z'FA'),kind=ucs4)// &
   char(int(z'62'),kind=ucs4)  // char(int(z'6F'),kind=ucs4) // char(int(z'72'),kind=ucs4)

   open (output_unit, encoding='utf-8')
   write(output_unit,*)'FILENAME:',ufilename

   afilename=ucs4_to_utf8(ufilename)

   open (newunit=lun, file=afilename, encoding='utf-8')
   !CLOSE(unit=lun, status='delete')

contains

function ucs4_to_utf8(ucs4_string) result(ascii_string)
character(len=*,kind=ucs4),intent(in) :: ucs4_string
character(len=:),allocatable          :: ascii_string
character(len=(len(ucs4_string)*4))   :: line
integer                               :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')ucs4_string
   rewind(lun)
   open(unit=lun,encoding='default')
   read(lun,'(A)')line
   close(lun)
   ascii_string=trim(line)
end function ucs4_to_utf8

end program read_filename
```
## Summary

If your processor supports Unicode filenames you probably need to convert
any filename in UCS-4 encoding to UTF-8 encoding to use the name on an
OPEN() statement.

Using Fortran's ability to encode UCS-4 data as UTF-8 when writing
external files it is easy to create a function for converting between
the two encodings.

Note that modules of related functions can be found at
[github.com/urbanjost/M_ucs4](github.com/urbanjost/M_ucs4) and
[github.com/urbanjost/M_unicode](github.com/urbanjost/M_unicode) that
use methods more efficient than using scratch files.

If your processor does not support Unicode filenames your operating
system may support links. So an alternative might be to make an ASCII
filename that is an alias for the unusable filename. This can typically
be done with system commands using the intrinsic EXECUTE_COMMAND_LINE(3) if
you do not have procedures for creating (and removing) links.

<a name="lesson6_ucs4"></a>
#### Introduction to Fortran Unicode support
### Lesson VI: reading UTF-8 strings from command lines

If your OS supports utf-8 as the default encoding it is likely you
will at some point want to pass a parameter from the command line or an
environment variable value that contains multi-byte Unicode characters
to your program.

But the GET_COMMAND_ARGUMENT() intrinsic specifies that the VALUE argument
is a "scalar character variable of default kind".

Perhaps you are going to use the value as a stream of bytes representing
utf-8 characters. In that case you may be able to use the string
without converting it to the supported UCS-4 internal representation
used by Fortran.

But if that is not the case, how will you convert the UTF-8 bytes
to UCS-4?  Fortran does not provide a procedure for such conversions.

But Fortran does the conversion needed when reading UTF-8-encoded files
into UCS-4 variables. So we use that functionality to create a simple
conversion function:

```fortran
program read_commandline
! @(#) take command line argument utf-8 text and generate Fortran statement that represents the string
use, intrinsic :: iso_fortran_env, only : output_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter             :: form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter             :: g= '(*(g0))'
integer                                :: i
character(len=:),allocatable           :: aline
character(len=:),allocatable           :: command_line
character(len=:,kind=ucs4),allocatable :: ustr
   command_line=getargs()          ! get string containing all command arguments as CHARACTER bytes
   ustr=utf8_to_ucs4(command_line) ! convert bytes to internal Fortran Unicode representation

   ! write the command line out as a Fortran variable expression using the CHAR() function
   open (output_unit, encoding='UTF-8')
   write(*,g) '! ENCODING:',command_line
   write(*,g) 'character(len=*,kind=ucs4),parameter :: variable= &'
   write(*,form)(ustr(i:i),i=1,len(ustr))
contains

function getargs() result(command_line)
integer                                :: length
character(len=:),allocatable           :: command_line
   call get_command(length=length)                 ! get command line length
   allocate(character(len=length) :: command_line) ! allocate string big enough to hold command line
   call get_command(command=command_line)          ! get command line as a string
   call get_command_argument(0,length=length)      ! remove argument 0
   command_line=adjustl(command_line(length+2:))
end function getargs

function utf8_to_ucs4(string) result(corrected)
character(len=*),intent(in)            :: string
character(len=:,kind=ucs4),allocatable :: corrected
character(len=(len(string)),kind=ucs4) :: line
integer                                :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')string
   rewind(lun)
   read(lun,'(A)')line
   close(lun)
   corrected=trim(line)
end function utf8_to_ucs4

end program read_commandline
```
An example run; using the famous Confucian expression
"己所不欲，勿施於人" (jǐ suǒ bù yù, wù shī yú rén) or
"What you do not want done to yourself, do not do to others":

```bash
read_commandline "己所不欲，勿施於人"
```

```text
! ENCODING:己所不欲，勿施於人
character(len=*,kind=ucs4),parameter :: variable= &
char(int(z'5DF1'),kind=ucs4)// &
char(int(z'6240'),kind=ucs4)// &
char(int(z'4E0D'),kind=ucs4)// &
char(int(z'6B32'),kind=ucs4)// &
char(int(z'FF0C'),kind=ucs4)// &
char(int(z'52FF'),kind=ucs4)// &
char(int(z'65BD'),kind=ucs4)// &
char(int(z'65BC'),kind=ucs4)// &
char(int(z'4EBA'),kind=ucs4)
```
## Summary

Command line arguments are typically returned as single-byte characters
by the GET_COMMAND_ARGUMENT() and GET_COMMAND() procedures. But using
Fortran's ability to encode UTF-8 as UCS-4 when reading and writing
external files it is easy to create a function for converting between
the two encodings to make up for the lack of any equivalent intrinsics.

The same applies to values contained in environment variables; albeit
most systems restrict the names of environment variables to ASCII-7
characters among other possible restrictions.

<a name="lesson7_ucs4"></a>
#### Introduction to Fortran Unicode support
### **Lesson VII:** passing Unicode strings to and from C

Passing Unicode strings from Fortran to C involves careful handling of
character encoding and memory management due to the differences in how
Fortran and C handle strings.

### Encoding Considerations:

Fortran character variables can be declared with
selected_char_kind('ISO_10646') to explicitly indicate a Unicode encoding
(typically UCS-4/UTF-32, depending on the compiler and system).

### C Unicode Handling:
.
C can handle Unicode through wchar_t for wide characters or by treating
UTF-8 encoded strings as arrays of char. The <uchar.h> header and
functions like mbrtoc32 or c32rtomb can be used for conversion between
encodings if necessary.

### Passing the String Data:

#### Using iso_c_binding:

The iso_c_binding module in Fortran is crucial for interoperability. It
provides types like c_char and c_ptr and functions like c_loc for
obtaining C-compatible pointers.

The most common approach is to pass the Fortran character variable as a
c_ptr (pointing to the beginning of the character data) and also pass
its length as an integer(c_int) to the C function. This allows the C
function to know the exact length of the string, which is essential for
handling non-null-terminated Fortran strings.

#### Null Termination (Optional but Recommended for C):
.
If the C function expects a null-terminated string, the Fortran code can
explicitly add a null terminator (char(0)) at the end of the character
string or array before passing it to C. However, ensure sufficient memory is
allocated in Fortran to accommodate the null terminator.

### Example Structure (Conceptual):

Fortran Side:

```fortran
module my_fortran_module
    use iso_c_binding
    implicit none

    interface
        subroutine process_unicode_string(c_string_ptr, string_length) &
        & bind(C, name='process_unicode_string')
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: c_string_ptr
            integer(c_int), intent(in), value :: string_length
        end subroutine process_unicode_string
    end interface

contains

    subroutine send_unicode_to_c()
        character(len=20, kind=selected_char_kind('ISO_10646')) :: unicode_str
        integer(c_int) :: str_len

        unicode_str = 'Hello, Unicode!😊'
        str_len = len(unicode_str, kind=c_int)

        call process_unicode_string(c_loc(unicode_str), str_len)
    end subroutine send_unicode_to_c

end module my_fortran_module
```
C Side:

```C
#include <stdio.h>
#include <string.h> // For strlen if you add null termination

// Function signature must match Fortran's bind(C)
void process_unicode_string(char *c_string_ptr, int string_length) {
    // Treat c_string_ptr as a pointer to a sequence of bytes representing the Unicode string
    // string_length provides the length of the string in bytes

    // Example: Print the string (assuming UTF-8)
    printf("Received Unicode string from Fortran: %.*s\n", string_length, c_string_ptr);

    // If Fortran adds a null terminator, you could use strlen:
    // printf("Received Unicode string from Fortran: %s\n", c_string_ptr);
}
```
---
### Key Points:

**Memory Ownership**:

Decide whether Fortran or C is responsible for allocating and
deallocating the memory for the string. If Fortran allocates, C should
not free it. If C allocates (e.g., using malloc), C must free it.

**Character Kind**:

Using selected_char_kind('ISO_10646') in Fortran is crucial for
simple proper Unicode handling.

**Length Parameter**:

Always pass the string length explicitly, as Fortran strings are
not inherently null-terminated like C strings.

<a name="summary_ucs4"></a>
#### Introduction to Fortran Unicode support
### Summary: Lessons Learned

The main lessons discussed here are

1. Compilers that support CHARACTER kind ISO_10646  make reading and
   writing UTF-8 encoded files as easy as doing the same with ASCII
   files.

2. All the CHARACTER intrinsics work with UCS4 variables.

3. Fortran does not supply functions to convert between UTF-8 encoded
   byte streams and UCS-4 encoded data. Fortran encodes Unicode data
   internally as UCS-4, but modern operating systems typically support
   UTF-8 encoded data. So this generally causes problems with converting
   values from command lines and environment variables to UCS-4.
   Problems typically arise for using UCS-4 encoded variables when
   opening files and doing file inquiry by name.

   Creating source files using UTF-8 encoding makes it nearly universally
   easy to write multi-byte files in Fortran constant strings, but
   remember Fortran instructions other than comments and constant
   strings must be composed only of characters in the Fortran character
   set (which amounts to ASCII-7 characters sans control characters).

If you care about the shortfallings in item 3 create a few conversion
routines and you can solve those problems on any system supporting
UTF-8 encoded files.

Remember that if you use UTF-8 constants in your code files that this is
not disallowed by the Standard, but neither is it required to be supported.

Taking that all into account
the following example program shows how to read an environment
variable into a UCS-4 variable, open files with UTF-8 encoded names, and
use intrinsic CHARACTER methods with Unicode data, circumventing the
issues raised in item 3.

```fortran
module M_encode
implicit none
private
integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')
integer, parameter :: ascii = selected_char_kind ("ascii")

public :: ascii_to_ucs4
public :: ucs4_to_ascii
public :: ucs4_to_utf8
public :: utf8_to_ucs4
public :: get_env
public :: get_arg

contains

pure function ascii_to_ucs4(astr) result(ustr)
! @(#) make the same conversion as an assignment statement from ASCII to UCS4
character(len=*,kind=ascii),intent(in) :: astr
character(len=len(astr),kind=ucs4)     :: ustr
integer                                :: i
   do i=1,len(astr)
      ustr(i:i)=achar(iachar(astr(i:i)),kind=ucs4)
   enddo
end function ascii_to_ucs4

pure function ucs4_to_ascii(ustr) result(astr)
! @(#) make the same conversion as an assignment statement from UCS4 to ASCII
character(len=*,kind=ucs4),intent(in)  :: ustr
character(len=len(ustr),kind=ascii)    :: astr
integer                                :: i
   do i=1,len(ustr)
      astr(i:i)=achar(iachar(ustr(i:i)),kind=ascii)
   enddo
end function ucs4_to_ascii

impure function ucs4_to_utf8(ucs4_string) result(ascii_string)
! @(#) use I/O to convert ucs4 to utf8 encoding
character(len=*,kind=ucs4),intent(in) :: ucs4_string
character(len=:),allocatable          :: ascii_string
character(len=(len(ucs4_string)*4))   :: line
integer                               :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')ucs4_string
   rewind(lun)
   open(unit=lun,encoding='default')
   read(lun,'(A)')line
   close(lun)
   ascii_string=trim(line)
end function ucs4_to_utf8

impure function utf8_to_ucs4(string) result(corrected)
! @(#) use I/O to convert utf8 to ucs4 encoding
character(len=*),intent(in)            :: string
character(len=:,kind=ucs4),allocatable :: corrected
character(len=(len(string)),kind=ucs4) :: line
integer                                :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')string
   rewind(lun)
   read(lun,'(A)')line
   close(lun)
   corrected=trim(line)
end function utf8_to_ucs4

function get_env_bytes(name,default) result(value)
! a function that makes calling get_environment_variable(3) simple
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
implicit none
character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: default
character(len=:),allocatable         :: value
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
end function get_env_bytes

function get_env(name,default) result(value)
! a function that makes calling get_environment_variable(3) simple
character(len=*,kind=ucs4),intent(in)          :: name
character(len=*,kind=ucs4),intent(in),optional :: default
character(len=:,kind=ucs4),allocatable         :: value
character(len=:),allocatable                   :: temp
   if(present(default))then
      temp=get_env_bytes(ucs4_to_utf8(name),ucs4_to_utf8(default))
   else
      temp=get_env_bytes(ucs4_to_utf8(name))
   endif
   value=utf8_to_ucs4(temp)
end function get_env

function get_arg_bytes(pos) result(arg)
integer                      :: argument_length, istat, pos
character(len=:),allocatable :: arg
   !
   ! allocate arg array big enough to hold command line argument
   !
   call get_command_argument(number=pos,length=argument_length)
   if(allocated(arg))deallocate(arg)
   allocate(character(len=argument_length) :: arg)
   call get_command_argument( pos, arg, status=istat )
   if(istat.ne.0)arg=''
end function get_arg_bytes

function get_arg(pos) result(arg)
integer                                :: pos
character(len=:,kind=ucs4),allocatable :: arg
   arg=utf8_to_ucs4(get_arg_bytes(pos))
end function get_arg

end module M_encode

program try_module
! @(#) convert environment variable to ucs-4 and show hexadecimal and decimal code point of characters
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
use M_encode, only : get_env, utf8_to_ucs4, ucs4_to_utf8, get_arg
implicit none
integer, parameter                        :: ucs4 = selected_char_kind ('ISO_10646')
character(len=1,kind=ucs4)                :: smiley=char(int(z'1F603'),kind=ucs4) ! 😃 Smiling face with open mouth
character(len=:,kind=ucs4),allocatable    :: string, env, arg
character(len=1,kind=ucs4)                :: glyph
character(len=80*4,kind=ucs4)             :: ufilename ! hold at least 80 UTF-8 glyphs
integer                                   :: i
integer                                   :: lun
   open(unit=stdout,encoding='UTF-8')
   !
   ! environment variable
   !
   env=get_env(ucs4_'UTF8_VARIABLE',smiley)
   write(*,*)'UTF8_VARIABLE=',env
   do i=1,len(env)
      glyph=env(i:i)
      write(*,'(z0,t8,i0,t16,a)')ichar(glyph),ichar(glyph),glyph
   enddo
   !
   ! command line arguments
   !
   do i=1, command_argument_count() ! get number of arguments
      arg=get_arg(i)
      write(*,*)'ARGUMENT:',i,ucs4_'['//arg//ucs4_']'
   enddo
   !
   ! utf-8 encoded constant
   !
   string=utf8_to_ucs4('七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。')
   write(*,*)'STRING:',string
   write(*,*)'REVERSED:',[(string(i:i),i=len(string),1,-1)]
   !
   ! convert UCS4 to UTF8 for use as a filename
   !
   ! môj_obľúbený_súbor, "my_favorite_file" in decimal codepoints
   write(ufilename,'(*(a))')char([109,244,106,95,111,98,318,250,98,101,110,253,95,115,250,98,111,114],kind=ucs4)
   write(*,*)'FILENAME:',trim(ufilename)
   open(newunit=lun,file=ucs4_to_utf8(ufilename))

end program try_module
```
## Expected Default Output:
```fortran
 UTF8_VARIABLE=😃
1F603  128515  😃
 STRING:七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。
 REVERSED:。うこいてい歩てい向を前にずけじく。るが上ち立たまもでん転。き起八び転七
 FILENAME:môj_obľúbený_súbor
```
<a name="no_iso_10646"></a>
## Processing Unicode when ISO-10646 is not supported
### Lesson I: converting UTF-8 codes to and from INTEGER values

If a Fortran compiler does not provide the optional ISO-10646 support
you can still do more than just copy UTF-8 byte streams to and from files.

The most general approach is to convert the utf-8 data into Unicode
integer code points.

Fortran also lacks an intrinsic string type of variable length.

This all points to creating a user-defined type that contains Unicode
strings as an integer array, with functions similar to the Fortran
CHARACTER intrinsics.

## UTF-8 bytes to codes

The first thing to do with the UTF-8 encoded data is to convert it to
Unicode code values; that is to find the integer value that identifies
that glyph using Unicode encoding. Fortran is not aware of UTF-8
encoding except via I/O routines when the optional ISO_10646 supplement
is supported. So routines need created that do the conversion of UTF-8
encoded data to and from Unicode code point values. These procedures
are available in the [M_unicode](https://github.com/urbanjost/M_unicode)
module as

   + utf8_to_codepoints()
   + codepoints_to_utf8()

They are public, but generally not expected to be called directly by user
code.

To encapsulate this data a user-defined type called UNICODE_TYPE is defined.
This allows for creating ragged arrays of character data where each element
may be a different length.

Assignment is defined such that UNICODE_TYPE variables can be defined by
being assigned to UTF-8 encoded streams of bytes or even an integer array
containing Unicode codepoint values.

A function called CHARACTER is needed to convert the type back to a stream
of bytes, for passing to other procedures or for printing as ASCII data.

Now with this type defined we can overload all the character-related intrinsics
to provide a familar interface, add an OOP interface to the type and add
additional functions for sorting, advanced string manipulation, and case
conversion.

The result is an interface arguably simpler to use than the ISO-10646
supplement that is considerably more powerful.

# Summary

 + [M_unicode](https://github.com/urbanjost/M_unicode)

   The M_unicode github repository contains not only the module code but
   build methods using fpm(1), make(1), and cmake(1); a unit test;
   example programs for each method provided; and documentation in
   HTML, man-page, and flat-text formats.

+ [TOP](Unicode)

<a name="extensions_ext"></a>
### off the beaten path:

## Common Unicode-related extensions

 * **[Extension I:](#backslash_ext)** the backslash escape code extension
 * **[Extension II:](#bom_ext)** embedding BOM characters at the beginning of files

<a name="backslash_ext"></a>
### off the beaten path: common Unicode extensions
## Extension I: backslash extension

A common Fortran extension is to support Unicode escape sequences which
specify characters by their hexadecimal code points.  This allows for
building UCS-4 strings more easily than using BOZ literals and the CHAR()
function. Usually the form is

    \xnn:        8-bit hexadecimal code nn
    \unnnn:     16-bit hexadecimal code nnnn
    \Unnnnnnnn: 32-bit hexadecimal code nnnnnnnn

To enable this generally requires a compiler switch such as -fbackslash or
-Mbackslash. Without an option, backslashes
within string literals are typically treated as literal backslash
characters. However, in at least one case the default is to enable backslash
escape sequences and a switch is required to cause standard-conforming
behavior. Other C-style escape sequences such as "\n" for a newline and
"\t" for a tab character are also typically supported.

The following example prints the Unicode symbol ☻ (black smiling face)
of code point U+263B. The compiled binary must be executed in a terminal
with Unicode support, like XTerm or sakura.

```fortran
program backslash_escape
use,intrinsic :: iso_fortran_env, only: output_unit
implicit none
integer,parameter :: ucs4 = selected_char_kind('ISO_10646')
character(kind=ucs4,len=:),allocatable :: str

   ! EXTENSION:
   str = ucs4_'Unicode character: \u263B'

   open (output_unit, encoding='utf-8')
   print '(a)', str
   print '(a)', ucs4_'Unicode character: \U0000263B'
end program backslash_escape
```

When using gfortran(1) build and run the executable with:
```bash
$ gfortran -fbackslash -o unicode unicode.f90
$ ./unicode
Unicode character: ☻
```

This is equivalent to BOZ literals, for instance:
```text
str = ucs4_'Unicode character: ' // char(int(z'263B'), kind=ucs4)
```
Or, simply by using the decimal character code point:

```text
str = ucs4_'Unicode character: ' // char(9787,ucs4)
```

Since these strings require an extension and may require specific compiler
options using a standard method is preferred but it is important to be
aware that code might be using C-like escape sequences, as building
such code without the extension active can produce incorrect strings
that can initially go unnoticed.

## Summary

Several compilers allow for quoted strings to contain code point
escape sequences. This is not standard and the syntax may therefore vary
from processor to processor.

Note that if the code point values are above 255 decimal that the string being created must be of type ISO_10646, not ASCII.

### Compiler support

#### **gfortran**
gfortran(1) has the -fbackslash compiler option:
```text
    -

        "\x"nn, "\u"nnnn and "\U"nnnnnnnn (where each n is a hexadecimal
        digit) are translated into the Unicode characters corresponding to
        the specified code points.
```

#### **flang new** (the LLVM version)

C-style backslash escape sequences in quoted CHARACTER literals (but not Hollerith) [-fbackslash], including Unicode escapes with \U.

#### **NAG Fortran**

Compiler supports UCS-4 beginning in release 5.3 (as well as UCS-2 and JIS X 0213)
but does not support Unicode escape sequences.

#### **ifx**

Intel Fortran does not support ISO_10646.

<a name="bom_ext"></a>
## off the beaten path: common Unicode extensions
### Extension II: embedding BOM characters at the beginning of files

## Byte Order Mark (BOM)

At its simplest, Unicode basically assigns a unique integer code to
each glyph or character. Basically ASCII files are just that -- composed
of one-byte unsigned integer values. But this limits you to 255 characters.
Unicode codes go far beyond 255.

So the ISO/IEC-10646 specification defines several ways of encoding
each Unicode character as a set of computer bytes (ie., UTF-8, UTF-2/UTF-16, and
UCS-4/UTF-32).  Each method has pros and cons, primarily being trade-offs
between the amount of data required versus how simply or efficiently the
data can be processed; but including other factors such as how well it
integrates and how little it conflicts with existing common text encoding
such as ASCII.

To make it simple to identify which encoding a text file is using,  a
byte sequence was designed called the Byte Order Mark (BOM) that would
not act as a character itself, but could be used to determine what
encoding the file was using when placed at the start of the file.

That is why some Unicode files, particularly those originating from Windows
systems, begin with a Byte Order Mark (BOM). This Unicode "noncharacter"
is code point value U+FEFF. It provides a strong indicator
of the encoding and byte order (endianness).

   + UTF-8: The BOM is encoded as bytes EF BB BF.
   + UTF-16 Big Endian: The BOM is encoded as bytes FE FF.
   + UTF-16 Little Endian: The BOM is encoded as bytes FF FE.
   + UTF-32 Big Endian: The BOM is encoded as bytes 00 00 FE FF.
   + UTF-32 Little Endian: The BOM is encoded as bytes FF FE 00 00.

If a BOM is absent, one can attempt to decode the file using each encoding
and check for validity. For instance, a sequence of bytes that is valid
UTF-8 might be invalid or produce nonsensical characters when interpreted
as UTF-16 or UTF-32. Because of Unicodes' design this can be done with a
high degree of reliability, decreasing the need for the BOM "Unicode Signature"
defined by the "magic string" of bytes placed at the beginning of the file.

In practice UTF-8 has many advantages over the other encodings when
used for file data. It is not effected by endianness, contains
ASCII 8-bit characters as a subset, can avoid being misconstrued as
an extended ASCII character set such as LATIN1 or LATIN2 encoding
(commonly used with modern European languages), and can represent all
Unicode characters but remain as compact as ASCII files when the files
predominantly are composed of ASCII characters, which is still often
the case.

#### Note:
   _All the Unicode encodings are sensitive to byte order except
   UTF-8_. That alone might make UTF-8 the preferred text file format.

UTF-8 has become so dominant as the Unicode file encoding scheme the use
of a BOM character is no longer even recommended unless required to work
properly with particular applications. Even when a file is started with
encoding='UTF-8', a Byte Order Mark (BOM) is not generated automatically
by any (current) Fortran compiler by default.

However, note that the NAG fortran compiler has a
__-bom=Asis|Remove|Insert__ option.

BOM characters are found most often in MicroSoft Windows environments.
The BOM character as a "magic string" was used in virtually all Unicode
files on MSWindows when initially introduced, partly because Microsoft
supported multiple Unicode text file formats early on, before UTF-8 was
seen as the de-facto text file encoding.

Note that to qualify as a BOM the string must appear at the beginning of
the file, not the middle of a data stream. Unicode says it should be
interpreted as a normal codepoint (namely a word joiner), not as a BOM
if it does not appear first.

### BOM as it applies to Fortran source files

The Unicode Standard permits the BOM in UTF-8 files, but does not
require or recommend its use.

There are references that state that if it is encountered
"its presence interferes with the use of UTF-8 by software that does not
expect non-ASCII bytes at the start of a file but that could otherwise
handle the text stream".

One such place might be a Fortran source file!  Multi-byte characters are
non-standard as part of the coding instructions, but are often handled
when appearing in comments and literal quoted character strings.

Some applications may require it. The most relevant issue is that the
NAG Fortran compiler has an extension where it formally supports UTF-8
source files, which are supposed to require starting with a BOM character
to distinguish them from ASCII files.

The GNU/Linux or Unix command file(1) will usually identify a file
starting with a BOM as UTF-8 encoded; but often determining whether a
text is encoded in UTF-8, UTF-16, or UTF-32, especially without explicit
metadata, often relies on analyzing the byte sequence for patterns
specific to each encoding.

The Unicode standard also does not recommend removing a BOM when it is
there, so that round-tripping between encodings does not lose information,
and so that code that relies on it continues to work.

Not using a BOM allows text to be backwards-compatible with software
designed for extended ASCII. For instance many non-Fortran programming
languages permit non-ASCII bytes in string literals but not
at the start of the file.

---
### prefix code with a BOM Unicode signature using a stream of bytes

This program creates a Fortran source file starting with a UTF-8 BOM. Try
to compile the output program to see if your compiler will compile it.

It could fail because a character is not in the Fortran character set
outside of a comment or literal string
```fortran
program bom_bytes
use iso_fortran_env, only : stdout => output_unit
implicit none
character(len=*),parameter :: &
   & A_bom = char(int(z'EF'))// char(int(z'BB'))// char(int(z'BF'))
   write(stdout,'(a)') &
    'program testit ! Unicode BOM as utf-8 bytes'               ,&
    '   write(*,*)"File starts with BOM from ""bytes"" write!"' ,&
    'end program testit'

end program bom_bytes
---
```
### prefix code with a BOM using standard ISO-10646

This program also generates another program source with the first
character the BOM character, but requires the compiler to support
the optional ISO-10646 supplemental standard.
```fortran
program bom_ucs4
use iso_fortran_env, only : stdout => output_unit
implicit none
intrinsic selected_char_kind
integer,parameter :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*,kind=ucs4),parameter :: U_bom=char(int(z'FEFF'),kind=ucs4)

   open(stdout,encoding='UTF-8')
   write(stdout,'(a)',advance='no')U_bom
   write(stdout,'(a)') &

    ucs4_'program testit ! Unicode BOM encoded to utf-8 bytes by Fortran' ,&
    ucs4_'   write(*,*)"File starts with BOM from UCS-4 write!"'          ,&
    ucs4_'end program testit'

end program bom_ucs4
```
### References

See [Wikipedia](https://en.wikipedia.org/wiki/Byte_order_mark) entry for
more information on the BOM Unicode character code, U+FEFF
(aka. ZERO WIDTH NO-BREAK SPACE),

