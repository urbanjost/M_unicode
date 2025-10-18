   program demo_index
   use M_unicode, only : ut=>unicode_type
   use M_unicode, only : assignment(=)
   use M_unicode, only : index
   implicit none
   type(ut) :: str
   character(len=*),parameter :: all='(*(g0))'

      str='Huli i kēia kaula no kēia ʻōlelo'
      print all, index(str,'kēia').eq.8

      ! return value is counted from the left end even if BACK=.TRUE.
      print all, index(str,'kēia',back=.true.).eq.22

      ! INDEX is case‐sensitive
      print all, index(str,'Kēia').eq.0

      str='Search this string for this expression'
      print all, index(str,'this').eq.8
      print all, index(str,'this',back=.true.).eq.24
      print all, index(str,'This').eq.0

   end program demo_index
