program test_M_unicode
use iso_fortran_env, only : output_unit
use M_unicode, only : adjustl, adjustr, index
use M_unicode, only : trim, len, len_trim
use M_unicode, only : character 
use M_unicode, only : assignment(=), unicode_type, operator(//)
use M_unicode, only : operator(<=), lle
use M_unicode, only : operator(<),  llt
use M_unicode, only : operator(/=), lne
use M_unicode, only : operator(==), leq
use M_unicode, only : operator(>),  lgt
use M_unicode, only : operator(>=), lge

use M_unicode, only : utf8_to_codepoints,  codepoints_to_utf8

use M_unicode, only : scan,  verify
use M_unicode, only : split, tokenize
use M_unicode, only : repeat
use M_unicode, only : upper, lower
use M_unicode, only : sort

implicit none
character(len=*),parameter :: g0='(*(g0))'
logical,parameter          :: T=.true.
logical,parameter          :: F=.false.
!
character(len=*),parameter :: upagain="七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。"
! Romanization:
! Nanakorobi yaoki. Koronde mo mata tachiagaru. Kujikezu ni mae o muite aruite ikou.
! or English translation
! "Fall seven times, stand up eight. Even if you fall down, you will get up again. Don't be discouraged, just keep walking forward."
!
character(len=:),allocatable   :: astr
type(unicode_type)             :: ut_str
type(unicode_type)             :: smiley
integer                        :: total
integer                        :: i
type(unicode_type)             :: lhs, rhs
type(unicode_type)             :: string, substring

   smiley='😃'
   total = 0

   write(*,g0)'encoding can be altered on an open file'
   open (output_unit, encoding='UTF-8')

   write(*,g0)
   astr='Hello World and Ni Hao -- 你好'
   ut_str=astr
   call checkits('convert to ASCII bytes',astr,ut_str%bytes(),transfer('Hello World and Ni Hao -- 你好',['A']))

   astr="  this is a string    "
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'this is a string      ')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'      this is a string')
   call checkit('trim',astr,character(trim(ut_str%trim())),'  this is a string')
   call check('len_trim',ut_str%len_trim().eq.18)
   call check('len_trim',len_trim(ut_str).eq.18)
   call check('len',ut_str%len().eq.22)
   call check('len',len(ut_str).eq.22)

   astr="  "
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'  ')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'  ')
   call checkit('trim',astr,character(trim(ut_str%trim())),'')
   call check('len_trim',ut_str%len_trim().eq.0)
   call check('len',ut_str%len().eq.2)

   astr=""
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'')
   call checkit('trim',astr,character(trim(ut_str%trim())),'')
   call check('len_trim',ut_str%len_trim().eq.0)
   call check('len',ut_str%len().eq.0)

   astr="ALLFULL"
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'ALLFULL')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'ALLFULL')
   call checkit('trim',astr,character(trim(ut_str%trim())),'ALLFULL')
   call check('len_trim',ut_str%len_trim().eq.7)
   call check('len',ut_str%len().eq.7)

   ut_str=[32,32,int(z'1F603'),32,32,32]
   astr=character(ut_str)
   call checkit('adjustl',astr,character(ut_str%adjustl()),'😃     ')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'     😃')
   call checkit('trim',astr,character(trim(ut_str%trim())),'  😃')
   call check('len_trim',ut_str%len_trim().eq.3)
   call check('len',ut_str%len().eq.6)

   if(total.ne.0)then
      write(*,g0)total,'failures'
      stop 1
   endif

   ut_str=smiley // ' and ' // smiley // 'and' // smiley // smiley // 'is it'
   astr='😃 and 😃and😃😃is it'
   call checkit('concatenation',astr,character(ut_str), '😃 and 😃and😃😃is it')

   lhs='this is the left'
   rhs='this is the right'
   call checkits_l('LLE',' ', [ lle(lhs,rhs),lhs <= rhs,lle(rhs,lhs),rhs <= lhs ] , [T,T,F,F] )
   call checkits_l('LLT',' ', [ llt(lhs,rhs),lhs <  rhs,llt(rhs,lhs),rhs <  lhs ] , [T,T,F,F] )
   call checkits_l('LNE',' ', [ lne(lhs,rhs),lhs /= rhs,lne(rhs,lhs),rhs /= lhs ] , [T,T,T,T] )
   call checkits_l('LEQ',' ', [ leq(lhs,rhs),lhs == rhs,leq(rhs,lhs),rhs == lhs ] , [F,F,F,F] )
   call checkits_l('LGT',' ', [ lgt(lhs,rhs),lhs >  rhs,lgt(rhs,lhs),rhs >  lhs ] , [F,F,T,T] )
   call checkits_l('LGE',' ', [ lge(lhs,rhs),lhs >= rhs,lge(rhs,lhs),rhs >= lhs ] , [F,F,T,T] )
   lhs='abc'
   rhs='abc '
   call checkits_l('LLE',' ', [ lle(lhs,rhs),lhs <= rhs,lle(rhs,lhs),rhs <= lhs ] , [T,T,T,T] )
   call checkits_l('LLT',' ', [ llt(lhs,rhs),lhs <  rhs,llt(rhs,lhs),rhs <  lhs ] , [F,F,F,F] )
   call checkits_l('LNE',' ', [ lne(lhs,rhs),lhs /= rhs,lne(rhs,lhs),rhs /= lhs ] , [F,F,F,F] )
   call checkits_l('LEQ',' ', [ leq(lhs,rhs),lhs == rhs,leq(rhs,lhs),rhs == lhs ] , [T,T,T,T] )
   call checkits_l('LGT',' ', [ lgt(lhs,rhs),lhs >  rhs,lgt(rhs,lhs),rhs >  lhs ] , [F,F,F,F] )
   call checkits_l('LGE',' ', [ lge(lhs,rhs),lhs >= rhs,lge(rhs,lhs),rhs >= lhs ] , [T,T,T,T] )

   string=" can you find me here? "
   substring="find me"
   astr=character(substring)
   call check('index '//string%character()//':'//substring%character(),index(string,substring).eq.10)
   call check('index '//string%character()//':'//astr,index(string,astr).eq.10)

   string=" can you find me here? "
   substring="not there"
   astr=character(substring)
   call check('index '//string%character()//':'//substring%character(),index(string,substring).eq.0)
   call check('index '//string%character()//':'//astr,index(string,astr).eq.0)

   string="short"
   substring="shortnot"
   astr=character(substring)
   call check('index '//string%character()//':'//substring%character(),index(string,substring).eq.0)
   call check('index '//string%character()//':'//astr,index(string,astr).eq.0)

   string=upagain
   write(*,g0)'original bytes  :', upagain
   write(*,g0)'bytes reversed  :', (upagain(i:i),i=len(upagain),1,-1)
   write(*,g0)'original string :', string%character()
   write(*,g0)'string reversed :', string%character(string%len(),1,-1)

contains

subroutine checkit(label,aline,answer,expected)
character(len=*),intent(in) :: label
character(len=*),intent(in) :: aline
character(len=*),intent(in) :: answer
character(len=*),intent(in) :: expected
   write(*,g0)merge('PASSED','FAILED',answer.eq.expected),' ',label,':[',aline,'][',answer,'][',expected,']'
   if(answer.ne.expected)total=total+1
end subroutine checkit

subroutine checkits(label,aline,answer,expected)
character(len=*),intent(in) :: label
character(len=*),intent(in) :: aline
character(len=*),intent(in) :: answer(:)
character(len=*),intent(in) :: expected(:)
   write(*,g0)merge('PASSED','FAILED',all(answer.eq.expected)),' ',label,':[',aline,'][',answer,'][',expected,']'
   if(all(answer.ne.expected))total=total+1
end subroutine checkits

subroutine checkits_l(label,aline,answer,expected)
character(len=*),intent(in) :: label
character(len=*),intent(in) :: aline
logical,intent(in) :: answer(:)
logical,intent(in) :: expected(:)
   write(*,g0)merge('PASSED','FAILED',all(answer.eqv.expected)),' ',label,':[',aline,'][',answer,'][',expected,']'
   if(all(answer.neqv.expected))total=total+1
end subroutine checkits_l

subroutine check(label,test)
character(len=*),intent(in) :: label
logical,intent(in)          :: test
   write(*,g0)merge('PASSED','FAILED',test),' ',label
   if(.not.test)total=total+1
end subroutine check

end program test_M_unicode
