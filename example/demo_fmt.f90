     program demo_fmt
     use :: M_unicode, only : fmt, unicode_type, assignment(=), ch=>character
     implicit none
     character(len=:),allocatable :: Aoutput
     type(unicode_type) :: Uoutput

        Aoutput=fmt(10,"'[',i0,']'")
        write(*,*)'result is ',Aoutput

        Aoutput=fmt(10.0/3.0,"'[',g0.5,']'")
        write(*,*)'result is ',Aoutput

        Uoutput=fmt(.true.,"'The final answer is [',g0,']'")
        write(*,*)'result is ',ch(Uoutput)

     end program demo_fmt
