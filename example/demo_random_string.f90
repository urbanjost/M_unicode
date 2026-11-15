    program demo_random_string
    use iso_fortran_env, only : stdout => output_unit
    use M_unicode,       only : random_string, ut=>unicode_type
    use M_unicode,       only : assignment(=), ch=>character
    character(len=:),allocatable :: aset
    type(ut)                     :: uset, ulets
    integer,allocatable          :: iset(:), ilets(:)
    integer                      :: i
    character(len=*),parameter   :: g='("[",g0,"]")'

     ! ensure getting an initially pseudo-random sequence
       call random_init(.false., .true.)

     ! ASCII-7 CHARACTER variable
       ! random hexadecimal values are often used as authorization keys
       aset='0123456789abcdef'
       write(stdout,g)aset, random_string(aset,64)

     ! using type(unicode_type)
       ! pick letters from Greek uppercase alphabet
       uset='ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ'
       write(stdout,g) uset%ch(), ch(random_string(uset,16))

     ! using codepoints
       ! ADE values for ASCII-7 uppercase
       iset=[(i,i=65,90)]   
       ilets=random_string(iset,32) 
       ! convert the codepoint values to a string
       ulets=ilets         
       write(stdout,g) ulets%ch() 

    end program demo_random_string
