program demo_issensitive
use iso_fortran_env, only : stdout => output_unit
use M_unicode,       only : issensitive, len, assignment(=)
use M_unicode,       only : ut => unicode_type, lower, upper
use M_unicode,       only : ch => character
implicit none
type(ut)                   :: lines(4), line, low, upp, diff
character(len=*),parameter :: g = '(*(g0))'
integer                    :: i
integer                    :: j

   lines(1) = 'P{äg15🍐 🌦6^c0ßßẞ)🌛🌢;4279OTRNiü!zx🌴|'
   lines(2) = 'ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ'
   lines(3) = 'abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   lines(4) = 'v:Hk~dqyUBm🌟🌬lEwöJ🌀A,f[]8🌷(S}🍍3'

   do j=1,size(lines)
      ! given a line
      line=lines(j)
      ! generate a lowercase and uppercase copy
      low=lower(line)
      upp=upper(line)

      ! determining which characters are different in the 
      ! lowercase and uppercase versions lets us determine
      ! which characters are case-sensitive, at least as
      ! far as the upper(3f) and lower(3f) functions
      print g, 'ORIGINAL      :',line%ch()
      print g, 'LOWERCASE COPY:',low%ch()
      print g, 'UPPERCASE COPY:',upp%ch()
      diff=pack(line%codes,low%codes == upp%codes)
      print g, 'NO CASE CHANGE:',diff%ch()
      diff=pack(line%codes,low%codes /= upp%codes)
      print g, 'CASE CHANGED  :',diff%ch()

      ! the same characters should be identified by the
      ! issensitive(3f) function 
      write(*,g,advance='no')'SENSITIVE     :'
      do i = 1,len(line)
         if (issensitive(line%sub(i,i))) then
            write(*,g,advance='no')line%ch(i,i)
         endif
      enddo
      write(*,g)

      write(*,g,advance='no')'NOT SENSITIVE :'
      do i = 1,len(line)
         if (.not.issensitive(line%sub(i,i))) then
            write(*,g,advance='no')line%ch(i,i)
         endif
      enddo
      write(*,g)
      write(*,g)
   enddo

end program demo_issensitive
