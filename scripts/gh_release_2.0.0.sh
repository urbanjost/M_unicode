#!/bin/bash
################################################################################
gh release delete v2.0.0
gh release list
gh release create v2.0.0 --title "Using UTF-8 data from Fortran" --notes-file - <<\EOF
# The M_unicode module

Major new features supported in version v2.0.0 include support for
 + C-style escape codes (**ESCAPE(3),REMOVE_BACKSLASH(3),ADD_BACKSLASH(3)**),
 + globbing (**GLOB(3)**) 
 + simplification of assignment of **CHARACTER** arrays to **TYPE(UNICODE_TYPE)**.

## DESCRIPTION
  The M_unicode(3f) module is a collection of Fortran string methods that work
  with __UTF-8__ encoded text not just __ASCII-7__ data.

  Strings are declared using the user-defined type "UNICODE_TYPE". The type
  supports allocatable ragged arrays where each element may be of differing
  length.

  Compiler support of the optional Fortran ISO_10646 extension is not
  required.

  Nearly all the methods are available using both OOP and procedural syntax.

  Complete User Documentation is included as flat text, HTML and man-pages.

| Topic             | Category | Description |
|-------------------|----------|-------------|
|M_unicode          | INTRO | Unicode string module |
|lower              | CASE | changes a string to lowercase over specified range |
|upper              | CASE | changes a string to uppercase |
|glob               | COMPARE | compare given string for match to a pattern which may contain globbing wildcard characters |
|isblank            | COMPARE | returns .true. if character is a Unicode or ASCII-7 blank character (space or horizontal tab) . |
|isspace            | COMPARE | returns .true. if character is a null, space, tab, carriage return, new line, vertical tab, or formfeed |
|add_backslash      | CONVERSION | Convert UTF-8 encoded data to ASCII-7 C-style backslash escape sequences |
|utf8_to_codepoints | CONVERSION | Convert UTF-8-encoded data to Unicode codepoints |
|ichar              | CONVERSION | character-to-integer code conversion function |
|fmt                | CONVERSION | convert any intrinsic to a string using specified format |
|codepoints_to_utf8 | CONVERSION | convert codepoints to CHARACTER |
|character          | CONVERSION | convert type(unicode_type) string to a CHARACTER variable |
|escape             | CONVERSION | expand C-like escape sequences |
|remove_backslash   | CONVERSION | expand C-like escape sequences |
|sub                | EDITING | Return substring |
|join               | EDITING | append CHARACTER variable array into a single CHARACTER variable with specified separator |
|replace            | EDITING | function replaces one substring for another in string |
|transliterate      | EDITING | replace characters from old set with new set |
|readline           | IO | read a line from specified LUN into string up to line length limit |
|repeat             | PAD | Repeated string concatenation |
|pad                | PAD | return string padded to at least specified length |
|tokenize           | PARSE | Parse a string into tokens. |
|split              | PARSE | parse a string into tokens, one at a time. |
|isascii            | QUERY | returns .true. if all the characters of a string are in the set from CHAR(0) to CHAR(127). |
|slurp              | READ | read formatted UTF-8 file into a TYPE(UNICODE_TYPE) string array |
|verify             | SEARCH | Position of a character in a string of characters that does not appear in a given set of characters. |
|index              | SEARCH | Position of a substring within a string |
|scan               | SEARCH | Scan a string for the presence of a set of characters |
|sort               | SORT | indexed hybrid quicksort of an array |
|get_arg            | SYSTEM | get command line argument |
|get_env            | SYSTEM | return value of environment variable |
|adjustl            | WHITESPACE | Left-justified a string |
|len                | WHITESPACE | Length of a string |
|expandtabs         | WHITESPACE | function to expand tab characters |
|trim               | WHITESPACE | remove trailing blank characters from a string |
|adjustr            | WHITESPACE | right-justify a string |
|len_trim           | WHITESPACE | string length without trailing blank characters |

EOF
################################################################################
gh release list
cygstart https://github.com/urbanjost/M_unicode
################################################################################
exit
################################################################################
