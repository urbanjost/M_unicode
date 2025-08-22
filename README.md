# M_unicode

### A WIP (Work In Progress)

## Unicode usage from Fortran when UTF-8 source files are supported

Fortran 2003 and later standards describe internal representation of
Unicode using 4-byte-per-character UCS-4 encoding for characters/glyphs,
including an ability to automatically encode and decode data read and
written to UTF-8 files.

But Unicode support is optional.

The M_utf8 repository deals with using Unicode from Fortran when UCS-4
(ie. CHARACTER(KIND="ISO_10646") is supported, emphasizing adherence to
the standard to promote portability.

When the compiler does __not__ support CHARACTER(KIND="ISO_10646") it
still may support UTF-8 source files and permit entering multi-byte
Unicode characters in comments and (more importantly) in constant
strings. This is now very common, as nearly all current operating systems
and many applications support UTF-8 text files.

But whether in input and output files, or as what-you-see-is-what-you-get
character constants the compiler will see this text as byte streams, and
will be unaware of how many glyphs/characters are represented.

So it may often be easy to use Unicode characters in fixed messages, but
if the text needs manipulated or processed in any way dealing with Unicode
as a raw series of 8-bit-bytes becomes complex and non-intuitive.

To keep processing of Unicode as simple as processing ASCII-7 characters
the **M_unicode** module provides a user-defined type named **UNICODE_TYPE**
and a number of procedures for converting byte streams that represent
UTF8-encoded text into Unicode code points (ie, 32-bit integer values
that generally identify one specific Unicode character).

Additionally the most common character-related intrinsics and operators
are overloaded to work with the UNICODE_TYPE variables; and the type is
extended to include the procedures and operators as type-bound procedures
for programmers that prefer OOP (Object-Oriented Programming) capabilities.


### M_unicose module

 + utf8_to_codepoints, codepoints_to_utf8

 + utf8_to_ucs4, ucs4_to_utf8
 + utf8_to_ucs4_via_io, ucs4_to_utf8_via_io

 + ascii_to_ucs4, ucs4_to_ascii

 + isolatin_to_unicode, unicode_to_isolatin
 + utf8_to_isolatin, isolatin_to_utf8
 + isolatin_to_utf32, utf32_to_isolatin

### UTF-8 source files -- just in comments and constants

The Fortran character set is the set of characters used in constructing
Fortran code. It is now the same as the ASCII 7-bit character set sans
the unprintable control characters. The letters a-z,A-Z and digits 0-9
and underscore are the only characters allowed in operator symbols. These
same characters and the "special" characters (the remaining printable
ASCII 7-bit characters) are used for bracketing, and various forms of
separating and delimiting other lexical tokens.

But what about other non-ASCII-7 characters representable by the
processor?  Possibilities include the extended ASCII characters or
multi-byte characters as defined for UTF-8 text files. The standard
states that, whatever those "additional characters" are, they may appear
in character constants:

    6.1.6   Other characters

    Additional characters may be representable in the processor, but
    shall appear only in comments (6.3.2.3, 6.3.3.2), character constants
    (7.4.4), input/output records (12.2.2), and character string edit
    descriptors (13.3.2).

Since ASCII-7-bit is a subset of UTF-8 it is very likely that if your
system supports UTF-8 files that it therefore will allow multi-byte
characters to be represented in comments and character constants.

But it is up to the processor whether it supports or even allows
UTF-8 files.  In the past when UTF-8 file support was rare this meant
using UTF-8 multi-byte characters in source files was likely to be very
non-portable.  Now that UTF-8 files are supported on most systems this
extension can be useful, particularly with compilers that do not support
UCS-4 yet.

Without using any Fortran Unicode support features, if the output
is redirected to a file does it appear correctly?
```fortran
program multibyte
character(len=*),parameter :: all='(*(g0))'
   print all,'Confucius never claimed to be a prophet, '
   print all,'but I think he foresaw AI! He said '
   print all
   print all,' "学而不思则罔，思而不学则殆"'
   print all,'or '
   print all,' (xué ér bù sī zé wǎng, sī ér bù xué zé dài),'
   print all,'or '
   print all,' "To learn without thinking is to be lost, '
   print all,' to think without learning is to be in danger".'
end program multibyte
```
Does it display properly when written to the screen? You might have
a terminal that does not support UTF-8 characters, or might have to
set your locale properly. If it is not the terminal and your compiler
does support ISO-10646 specifying the encoding of the output file
as UTF-8 will likely correct any output issues even though the strings
are a series of bytes and not UCS-4 encoded:
```fortran
program multibyte_encoded
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
implicit none
   open(stdout,encoding='utf-8')
   write(stdout,'(a)') &
   'Confucius never claimed to be a prophet, '       ,&
   'but I think he foresaw AI! He said '             ,&
   ''                                                ,&
   ' "学而不思则罔，思而不学则殆"'                   ,&
   'or'                                              ,&
   ' (xué ér bù sī zé wǎng, sī ér bù xué zé dài),'   ,&
   'or'                                              ,&
   ' "To learn without thinking is to be lost, '     ,&
   ' to think without learning is to be in danger".' 
end program multibyte_encoded
```

Remember that unless the compiler directly supports UTF-8 representation
( so far I have identified no compilers that do so) these strings are seen
by the compiler as a string of bytes, and is otherwise unaware they
represent Unicode characters. If they are just to be read and written as-is
as in the previous example program that is not a major concern; but if you
are manipulating or processing the strings in significant ways you probably
want to convert the strings to TYPE(UNICODE_TYPE).

## Summary

Yes, a Fortran source file can contain multibyte Unicode characters, but
the level of support and how they are handled depends on the specific
Fortran compiler and operating system and is not otherwise defined by
the Fortran standard.

The appearance of multi-byte characters in comments and character
constants is typically all that is allowed and compiler errors occur
when unsupported characters outside the Fortran character set are used
in the body of the code -- unless the vendor extends Fortran beyond the
requirements of the Fortran standard. It is assumed here that is not
the case; so be careful that outside of quoted strings and comments that
only ASCII-7-bit characters are used to write the actual coding instructions.

Some editors might try to be "helpful" and change ASCII quote and dash
characters to other characters when editing UTF-8 files, so be aware
you might need to normalize your source files into the allowed Fortran
character set.

Avoid list-directed output. It does not know which bytes are composing
a glyph and may split lines at inappropriate points.

### Compiler Support:

   Even with standard features, compiler support for Unicode in source
   files can vary. Some compilers might require specific flags (e.g.,
   -qmbcs for XL Fortran) or might offer better integration with
   system-level Unicode functionalities.

### Source File Encoding:

   The encoding of the Fortran source file itself is crucial. If the
    file is saved in a Unicode encoding like UTF-8, the compiler needs
   to be able to correctly interpret these characters during compilation.

### Limitations:

While modern Fortran can handle Unicode characters, there might still be
limitations compared to languages like C++ regarding the ease of use with
complex Unicode features (e.g., surrogate pairs, text directionality, normalization).

### In summary: 

While it is possible to include multibyte Unicode characters in Fortran
source files, especially with modern Fortran standards and supportive
compilers, it requires careful consideration of the compiler's
capabilities and the source file's encoding.


 ## References

Initially based on a discussion begun in
https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949, 2025-08;
including features and enhancements from Francois Jacq.

The improvements include procedures for handling ASCII encoding extensions
often used for internationalization that pre-date Unicode, such as the
Latin encodings.


The intent is for the M_unicode module to be useful on many platforms, but currently
it is primarily tested with GCC/gfortran and INTEL/ifx on Linux and Cygwin.

```fortran
program testit
use iso_fortran_env, only : stdout=>output_unit
use M_unicode 
implicit none
type(unicode_type) :: uline

   !open (stdout, encoding='UTF-8') ! preferred, but not required if not supported

   uline= 'Hello World and Ni Hao -- 你好  ')
   write (stdout,*) character(str) ! convert to intrinsic CHARACTER variable
   write (stdout,*) len(str)
   write (stdout,*) len_trim(str)
   write (stdout,*) str(27:28)
   write (stdout,*) index(uline,'你')

   ! OOPS
   write (stdout,*) str%character(len(str),1,-1) ! reverse string
   write (stdout,*) str%codes ! Unicode code point values

end program testit
```
-------------------------------------------------------------
## See Also

 + [https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949](https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949)
 + [https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618](https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618)
 + [https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764](https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764)
<!--
 + [UTF-8 Everywhere Manifesto](http://utf8everywhere.org/)
-->
