      program demo_M_unicode
      use,intrinsic :: iso_fortran_env, only : stdout=>output_unit
      use M_unicode,only : TOKENIZE, REPLACE, CHARACTER, UPPER, LOWER
      use M_unicode,only : unicode_type, assignment(=), operator(//)
      use M_unicode,only : read(formatted), write(formatted)
      use M_unicode,only : ut => unicode_type, ch => character
      type(unicode_type)             :: string, numeric, uppercase, lowercase
      type(unicode_type),allocatable :: array(:)
      character(len=*),parameter     :: gen='(g0)'
      character(len=*),parameter     :: uni='(DT)'
      uppercase='АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ'
      lowercase='абвгґдеєжзиіїйклмнопрстуфхцчшщьюя'
      numeric='0123456789'

       string=uppercase//' '//numeric//' '//lowercase
       
       write(stdout,gen)'Original string:'
       write(stdout,uni) string
       write(stdout,gen)
       write(stdout,gen)'convert to all uppercase:'
       write(stdout,uni) UPPER(string)
       write(stdout,gen)
       write(stdout,gen)'convert to all lowercase:'
       write(stdout,uni) LOWER(string)
       write(stdout,gen)
       write(stdout,gen)'tokenize on spaces:'
       call TOKENIZE(string,ut(' '),array)
       write(stdout,gen) character(array)
       write(stdout,uni) array
       write(stdout,gen)
       write(stdout,gen)'case-insensitive replace:'
       write(stdout,uni) REPLACE(string, &
       & ut('клмнопрс'), &
       & ut('--------'), &
       & ignorecase=.true.)
       write(stdout,gen)

      end program demo_M_unicode
