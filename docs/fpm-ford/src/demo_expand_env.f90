    program demo_expand_env
    use M_unicode, only : expand_env, ut=>unicode_type
    use M_unicode, only : assignment(=), trim, ch=>character
    use M_unicode, only : operator(//)
    use M_unicode, only : keyword_expand
    implicit none
    integer                       :: i
    character(len=:), allocatable :: expanded
    character(len=:), allocatable :: testsa(:)
    type(ut),allocatable :: tests(:)

    testsa = [ character(len=80) :: &
    "-------------- Testing Pure Fortran Word Expansion --------------",&
    " scratch space is $TMP for $USER with home directory  ${HOME}",    &
    " DATE is $DATE TIME is ${TIME} ZONE is $ZONE TIMESTAMP $TIMESTAMP",&
    " FILE is $FILE ARGS: 0: $0 1: $1 2: $2 3: $3",                     &
    " COMPILER is $COMPILER COMPILER_OPTIONS are ${COMPILER_OPTIONS}",  &
    " ${0} versus ${FILE}  $0 versus $FILE",                            &
    " All arguments is ${0} ${*}",                                      &
    " unknown $UNKNOWN incorrect ${ A B C} but syntax OK",              &
    " Bad $  ${  and stuff $UNKNOWN ${ A B C}",                         &
    ""]

       tests=testsa
       tests=trim(tests)
       do i = 1,size(tests)
          expanded=expand_env(tests(i))
          print '(A)', "Input:  "//tests(i)%ch()
          print '(A)', "Result: "//expanded
          print '(A)', ch( "Result: "//tests(i)%expand_env() )
          print '(A)', ""
       enddo

    call version()

    contains
    subroutine version()
    use M_unicode, only : expand_env, ut=>unicode_type
    use M_unicode, only : assignment(=), trim, ch=>character
    use M_unicode, only : operator(//)
    use M_unicode, only : keyword_expand, replace
    implicit none
    character(len=1),parameter    :: nl=new_line('a')
    character(len=:), allocatable :: testsa(:)
    integer                       :: i
    integer                       :: ilast
    type(ut),allocatable          :: tests(:)

    testsa = [ character(len=80) :: &
    " <g>PROGRAM</g>         : pretender(1)", &
    " <g>VERSION</g>         : 1.0.0", &
    " <g>DESCRIPTION</g>     : <ul>pretend to be a program</ul>", &
    " <g>COPYRIGHT</g>       : <copy> Mouse and Company, 1957", &
    " <g>LICENSE</g>         : Public Domain", &
    " <g>AUTHOR</g>          : Micky Mouse", &
    " <g>COMPILED</g>        : 2023-02-17T19:30:55-0300", &
    " <g>TIMESTAMP</g>       : $TIMESTAMP", &
    " <g>COMMAND</g>         : ${0} ${*}", &
    " <g>USER</g>            : $USER",    &
    " <g>PWD</g>             : $PWD",    &
    " <g>COMPILER</g>        : $COMPILER",  &
    " <g>COMPILER_OPTIONS</g>: ${COMPILER_OPTIONS}" ]

       tests=keyword_expand(testsa)
       tests=trim(expand_env(tests))
       ilast=size(tests)
       tests(ilast)=replace(tests(ilast),' -',nl//'                   -')
       tests(ilast)=replace(tests(ilast),' /',nl//'                   /')
       print '(A)', (tests(i)%ch(),i=1,size(tests))
    end subroutine version

    end program demo_expand_env
