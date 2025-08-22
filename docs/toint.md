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

The first thing to do with the UTF-8 file is to convert it to Unicode
code values; that is to find the integer value that identifies that glyph
using Unicode encoding. We have placed procedures that do that conversion
in M_unicode.f90:

   + utf8_to_codepoints()
   + codepoints_to_utf8()


+ [TOP](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson0.md)
+ [PREVIOUS](https://github.com/lockstockandbarrel/earth/blob/main/docs/aaaaa.md)
+ [NEXT](https://github.com/lockstockandbarrel/earth/blob/main/docs/bom_ext.md)

