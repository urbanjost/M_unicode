     program demo_fmt
     use :: M_unicode, only : fmt, assignment(=)
     use :: M_unicode, only : ut=>unicode_type, ch=>character
     implicit none
     character(len=:),allocatable :: Aoutput
     type(ut) :: Uoutput

        ! format can be CHARACTER
        Aoutput=fmt(10,"'[',i0,']'")
        write(*,*)'result is ',Aoutput

        ! format can be string
        Aoutput=fmt(10.0/3.0,ut("'[',g0.5,']'"))
        write(*,*)'result is ',Aoutput

        Uoutput=fmt(.true.,"'The final answer is [',g0,']'")
        write(*,*)'result is ',ch(Uoutput)

     end program demo_fmt
