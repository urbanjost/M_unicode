# M_unicode

## Release version 1.0.0; 2025-09-28

**M_unicode** provides support for Unicode encoded as UTF-8 data even
when the optional Fortran ISO_10646 extension is not provided.

A user-defined type named **unicode_type** allows for creating ragged
arrays of ASCII or UTF-8 encoded data that otherwise can be treated much
like a CHARACTER kind.

**M_unicode** has overloading for all the basic operators and character
intrinsics with both a procedural and OOP interface. The intrinsic
overloads include **TOKENIZE()** and **SPLIT()**.

In addition the **UPPER()** and **LOWER()** funtions support the concept
of case for the Unicode Latin characters not just the ASCII subset,
and a basic SORT() function provides for ordering the data by Unicode
codepoint values.

The distribution provides a __Make__ file and easily builds with __fpm__.

Documentation and examples provide a guide for basic usage.

**M_unicode** should be useful for anyone working with UTF-8 data,
particularly if the compiler does not support the UCS-4 extensions
of Fortran.
