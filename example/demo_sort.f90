     program demo_sort
     use iso_fortran_env, only : stdout => output_unit
     use M_unicode,       only : write(formatted)
     use M_unicode,       only : sort, assignment(=)
     use M_unicode,       only : ut=>unicode_type, ch=>character
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     character(len=*),parameter :: dt='(*(dt,1x))'
     integer,parameter          :: isz=4
     type(ut)                   :: rr(isz)
     integer                    :: ii(isz)
     integer                    :: i

        write(stdout,g)'sort array with sort(3f)'
        rr=[ &
         ut("the"),   &
         ut("quick"), &
         ut("brown"), &
         ut("fox") ]

        write(stdout,g)'original order'
        !ifx bug!write(stdout,dt)rr
        write(stdout,g)ch(rr)

        call sort(rr,ii)

        write(stdout,g)'sorted order'
        ! convert to character
        do i=1,size(rr)
           write(stdout,'(i3.3,1x,a)')i,rr(ii(i))%character()
        enddo

        write(stdout,g)'reorder original'
        rr=rr(ii)
        !ifx bug!write(stdout,dt)rr
        write(stdout,g)ch(rr)

     end program demo_sort
