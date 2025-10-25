module testsuite_M_unicode
use iso_fortran_env, only : output_unit
! overload intrinsics
use M_unicode, only : adjustl, adjustr, index
use M_unicode, only : trim, len, len_trim
use M_unicode, only : repeat
use M_unicode, only : upper, lower
use M_unicode, only : expandtabs
use M_unicode, only : escape
use M_unicode, only : sort
use M_unicode, only : scan, verify
use M_unicode, only : tokenize, split
use M_unicode, only : ichar
use M_unicode, only : replace
use M_unicode, only : sub
use M_unicode, only : pad
use M_unicode, only : join
use M_unicode, only : fmt, afmt

use M_unicode, only : assignment(=), unicode_type
use M_unicode, only : operator(.cat.)
!use M_unicode, only : operator(//)
use M_unicode, only : operator(<=), lle
use M_unicode, only : operator(<),  llt
use M_unicode, only : operator(/=), lne
use M_unicode, only : operator(==), leq
use M_unicode, only : operator(>),  lgt
use M_unicode, only : operator(>=), lge

use M_unicode, only : character
use M_unicode, only : utf8_to_codepoints,  codepoints_to_utf8

use M_unicode, only : ut => unicode_type
use M_unicode, only : ch => character

implicit none
character(len=*),parameter :: g0='(*(g0))'
character(len=*),parameter :: g1='(*(g0,1x))'
logical,parameter          :: T=.true.
logical,parameter          :: F=.false.
integer                    :: total

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
   if(size(answer).eq.size(expected) )then
      write(*,g0)merge('PASSED','FAILED',all(answer.eq.expected)),' ',label,':[',aline,'][',answer,'][',expected,']'
      if(all(answer.ne.expected))total=total+1
   else
      write(*,g0)'FAILED',' ',label,':[',aline,'][',answer,'][',expected,']'
      total=total+1
   endif
end subroutine checkits

subroutine checkits_l(label,aline,answer,expected)
character(len=*),intent(in) :: label
character(len=*),intent(in) :: aline
logical,intent(in) :: answer(:)
logical,intent(in) :: expected(:)
   write(*,g0)merge('PASSED','FAILED',all(answer.eqv.expected)),' ',label,':[',aline,'][',answer,'][',expected,']'
   if(all(answer.neqv.expected))total=total+1
end subroutine checkits_l

subroutine check(label,test,description)
character(len=*),intent(in)          :: label
logical,intent(in)                   :: test
character(len=*),intent(in),optional :: description
   if( present(description) )then
      write(*,g0)merge('PASSED','FAILED',test),' ',label,':',description
   else
      write(*,g0)merge('PASSED','FAILED',test),' ',label
   endif
   if(.not.test)total=total+1
end subroutine check

subroutine programming_environment()
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
implicit none
character(len=:),allocatable :: version, options
character(len=*),parameter   :: nl=new_line('a')
integer                      :: where, start, break
   version=compiler_version()
   options=' '//compiler_options()
   start=1
   do 
      where=index(options(start:),' -')
      if(where.eq.0)exit
      break=where+start-1
      options(break:break)=nl
      start=where
   enddo
   if(start.eq.1)then
      do 
         where=index(options(start:),' /')
         if(where.eq.0)exit
         break=where+start-1
         options(break:break)=nl
         start=where
      enddo
   endif
   print '(*(1x,a))', &
    'This file was compiled by ', &
    version,nl,        &
    'using the options ',         &
    options
end subroutine programming_environment

subroutine test_index()
type(unicode_type)             :: string, substring
character(len=:),allocatable   :: astr
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
end subroutine test_index

subroutine test_repeat()
character(len=:),allocatable   :: astr
type(unicode_type)             :: ut_str
integer                        :: i
   ut_str='💣💥💣💥💣💥'
   astr=ut_str%character()
   ut_str=repeat(ut_str,5)
   call checkit('repeat',astr,ut_str%character(),&
   '💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥')
   ut_str=[(i,i=48,57)]
   ut_str=ut_str%character(2,10)//ut_str%character(1,1)
   call checkit('repeat',ut_str%character(),character(repeat(ut_str, 3)),'123456789012345678901234567890')
end subroutine test_repeat

subroutine test_adjustl()
character(len=:),allocatable   :: astr
type(unicode_type)             :: ut_str

   astr="  this is a string    "
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'this is a string      ')

   astr="  "
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'  ')

   astr=""
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'')

   astr="ALLFULL"
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'ALLFULL')

   ut_str=[32,32,int(z'1F603'),32,32,32]
   astr=character(ut_str)
   call checkit('adjustl',astr,character(ut_str%adjustl()),'😃     ')

   astr = "this is a string              "
   ut_str =   "  this is a string    "
   call checkit('adjustl',astr,character(ut_str%adjustl(30)),astr)

end subroutine test_adjustl

subroutine test_adjustr()
character(len=:),allocatable   :: astr
type(unicode_type)             :: ut_str
   astr="  this is a string    "
   ut_str=astr
   call checkit('adjustr',astr,character(ut_str%adjustr()),'      this is a string')

   astr="  "
   ut_str=astr
   call checkit('adjustr',astr,character(ut_str%adjustr()),'  ')

   astr=""
   ut_str=astr
   call checkit('adjustr',astr,character(ut_str%adjustr()),'')

   astr="ALLFULL"
   ut_str=astr
   call checkit('adjustr',astr,character(ut_str%adjustr()),'ALLFULL')

   ut_str=[32,32,int(z'1F603'),32,32,32]
   astr=character(ut_str)
   call checkit('adjustr',astr,character(ut_str%adjustr()),'     😃')

   astr = "              this is a string"
   ut_str =   "  this is a string    "
   call checkit('adjustr',astr,character(ut_str%adjustr(30)),astr)

end subroutine test_adjustr

subroutine test_len()
character(len=:),allocatable   :: astr
type(unicode_type)             :: ut_str
   astr="  this is a string    "
   ut_str=astr
   call check('len',ut_str%len().eq.22)
   call check('len',len(ut_str).eq.22)

   astr="  "
   ut_str=astr
   call check('len',ut_str%len().eq.2)

   astr=""
   ut_str=astr
   call check('len',ut_str%len().eq.0)

   astr="ALLFULL"
   ut_str=astr
   call check('len',ut_str%len().eq.7)

   ut_str=[32,32,int(z'1F603'),32,32,32]
   astr=character(ut_str)
   call check('len',ut_str%len().eq.6)

end subroutine test_len

subroutine test_len_trim()
character(len=:),allocatable   :: astr
type(unicode_type)             :: ut_str
   astr="  this is a string    "
   ut_str=astr
   call check('len_trim',ut_str%len_trim().eq.18)
   call check('len_trim',len_trim(ut_str).eq.18)

   astr="  "
   ut_str=astr
   call check('len_trim',ut_str%len_trim().eq.0)

   astr=""
   ut_str=astr
   call check('len_trim',ut_str%len_trim().eq.0)

   astr="ALLFULL"
   ut_str=astr
   call check('len_trim',ut_str%len_trim().eq.7)

   ut_str=[32,32,int(z'1F603'),32,32,32]
   astr=character(ut_str)
   call check('len_trim',ut_str%len_trim().eq.3)

end subroutine test_len_trim

subroutine test_trim()
character(len=:),allocatable   :: astr
type(unicode_type)             :: ut_str
   astr="  this is a string    "
   ut_str=astr
   call checkit('trim',astr,character(trim(ut_str%trim())),'  this is a string')

   astr="  "
   ut_str=astr
   call checkit('trim',astr,character(trim(ut_str%trim())),'')

   astr=""
   ut_str=astr
   call checkit('trim',astr,character(trim(ut_str%trim())),'')

   astr="ALLFULL"
   ut_str=astr
   call checkit('trim',astr,character(trim(ut_str%trim())),'ALLFULL')

   ut_str=[32,32,int(z'1F603'),32,32,32]
   astr=character(ut_str)
   call checkit('trim',astr,character(trim(ut_str%trim())),'  😃')

end subroutine test_trim

subroutine test_concatenate()
type(ut) :: str
integer  :: ten=10,twenty=20
   !need clarification! () required by ifx, not flang_new or gfortran
   !str='so '//ten//'+'//twenty//'='//(ten+twenty)//' 😃'
   str='so '.cat.ten.cat.'+'.cat.twenty.cat.'='.cat.(ten+twenty).cat.' 😃'
   ! ifx cannot print when // overloaded
   call check('//',str == 'so 10+20=30 😃','concatenate got '//str%character())
end subroutine test_concatenate

subroutine test_expandtabs()
character(len=:),allocatable :: str
type(unicode_type)           :: in
type(unicode_type)           :: expected
integer                      :: i
   str='  this is my string  '
   ! change spaces to tabs to make a sample input
   do i=1,len(str)
      if(str(i:i) == ' ')str(i:i)=char(9)
   enddo
   in=str
   expected="                this    is      my      string"
   call check('expandtabs',expandtabs(in).eq.expected,character(expandtabs(in)))
   call check('expandtabs',in%expandtabs().eq.expected,character(expandtabs(in)))
   expected="thisismystring"
   call check('expandtabs',in%expandtabs(tab_size=0).eq.expected,character(in%expandtabs(tab_size=0)))
end subroutine test_expandtabs

subroutine test_fmt()

  call  add('INTEGER',  fmt(10),            '10'       )
  call  add('LOGICAL',  fmt(.false.),       'F'        )
  call  add('LOGICAL',  fmt(.true.),        'T'        )
  call  add('REAL',     fmt(100.0),         '100'      )
  call  add('COMPLEX',  fmt((11.0,22.0)),   '(11,22)'  )
  call  add('REAL',     fmt(100.0,'f0.2'),  '100.00'   )

contains

subroutine add(message,question,answer)
character(len=*),intent(in)   :: message
type(ut),intent(in)           :: question
character(len=*),intent(in)   :: answer
  call check('fmt',question.eq.answer,'testing '//message//' expected '//answer//' got '//ch(question))
end subroutine add

end subroutine test_fmt

subroutine test_upper()
type(unicode_type) :: upp, low, temp
integer            :: i
character(len=128) :: ascii7
!
! remember unicode characters are multi-byte so be careful
! with older compilers to not exceed 132 bytes per line
!
   low='&
   &abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëì&
   &íîïðñòóôõöøùúûüýþÿāăąćĉċčďđēĕėęěĝğġģĥħĩīĭ&
   &įıĳĵķĺļľŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷźżž&
   &ƃƅƈƌƒƙơƣƥƨƭưƴƶƹƽǆǉǌǎǐǒǔǖǘǚǜǟǡǣǥǧǩǫǭǯǳǵǻǽǿ&
   &ȁȃȅȇȉȋȍȏȑȓȕȗɓɔɗɘəɛɠɣɨɩɯɲɵʃʈʊʋʒάέήί&
   &αβγδεζηθικλμνξοπρστυφχψωϊϋόύώϣϥϧϩϫϭϯабвгдежзий&
   &клмнопрстуфхцчшщъыьэюяёђѓєѕіїјљњћќўџ&
   &ѡѣѥѧѩѫѭѯѱѳѵѷѹѻѽѿҁґғҕҗҙқҝҟҡңҥҧҩҫҭүұҳҵҷҹһҽҿӂӄӈ&
   &ӌӑӓӕӗәӛӝӟӡӣӥӧөӫӯӱӳӵӹաբգդեզէըթժիլխծկհձղ&
   &ճմյնշոչպջռսվտրցւփքօֆაბგდევზთიკლმნოპჟრსტუფქ&
   &ღყშჩცძწჭხჯჰჱჲჳჴჵḁḃḅḇḉḋḍḏḑḓḕḗḙḛḝḟḡḣḥḧ&
   &ḩḫḭḯḱḳḵḷḹḻḽḿṁṃṅṇṉṋṍṏṑṓṕṗṙ&
   &ṛṝṟṡṣṥṧṩṫṭṯṱṳṵṷṹṻṽṿ&
   &ẁẃẅẇẉẋẍẏẑẓẕạảấầẩẫậắằẳẵặẹẻẽếềểễệỉịọỏốồổỗ&
   &ộớờởỡợụủứừửữựỳỵỷỹἀἁἂἃἄἅἆἇἐἑἒἓἔἕἠἡἢἣἤἥἦἧἰἱ&
   &ἲἳἴἵἶἷὀὁὂὃὄὅὑὓὕὗὠὡὢὣὤὥὦὧᾀᾁᾂᾃᾄᾅᾆᾇᾐᾑᾒᾓᾔᾕᾖᾗ&
   &ᾠᾡᾢᾣᾤᾥᾦᾧᾰᾱῐῑῠῡⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ&
   &ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ'
   upp='&
   &ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊË&
   &ÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞŸĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬ&
   &ĮIĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŹŻ&
   &ŽƂƄƇƋƑƘƠƢƤƧƬƯƳƵƸƼǄǇǊǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮǱǴǺǼǾ&
   &ȀȂȄȆȈȊȌȎȐȒȔȖƁƆƊƎƏƐƓƔƗƖƜƝƟƩƮƱƲƷΆΈΉΊΑΒΓΔΕΖΗΘ&
   &ΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫΌΎΏϢϤϦϨϪϬϮАБВГДЕЖЗИЙ&
   &КЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯЁЂЃЄЅІЇЈЉЊЋЌЎЏѠѢѤѦѨѪѬ&
   &ѮѰѲѴѶѸѺѼѾҀҐҒҔҖҘҚҜҞҠҢҤҦҨҪҬҮҰҲҴҶҸҺҼҾӁӃӇ&
   &ӋӐӒӔӖӘӚӜӞӠӢӤӦӨӪӮӰӲӴӸԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿՀՁՂՃՄ&
   &ՅՆՇՈՉՊՋՌՍՎՏՐՑՒՓՔՕՖႠႡႢႣႤႥႦႧႨႩႪႫႬႭႮႯႰႱႲႳႴႵ&
   &ႶႷႸႹႺႻႼႽႾႿჀჁჂჃჄჅḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮ&
   &ḰḲḴḶḸḺḼḾṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾ&
   &ẀẂẄẆẈẊẌẎẐẒẔẠẢẤẦẨẪẬẮẰẲẴẶẸẺẼẾỀỂỄỆỈỊ&
   &ỌỎỐỒỔỖỘỚỜỞỠỢỤỦỨỪỬỮỰỲỴỶỸ&
   &ἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹ&
   &ἺἻἼἽἾἿὈὉὊὋὌὍὙὛὝὟὨὩὪὫὬὭὮὯ&
   &ᾈᾉᾊᾋᾌᾍᾎᾏᾘᾙᾚᾛᾜᾝᾞᾟᾨᾩᾪᾫᾬᾭᾮᾯᾸᾹ&
   &ῘῙῨῩⒶⒷⒸⒹⒺⒻⒼⒽⒾⒿⓀⓁⓂⓃⓄⓅⓆⓇⓈⓉⓊⓋⓌⓍⓎⓏ&
   &ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ'

   call check('upper', upper(low)==upp )
   call check('upper', character(upper(low))==character(upp) )

   write(ascii7,g0)(achar(i),i=0,127)
   ascii7( ichar('a')+1:ichar('z')+1 ) = ' '
   ascii7( ichar('A')+1:ichar('Z')+1 ) = ' '
   temp=ascii7
   call check('upper',temp%character()==ascii7,'check non-alphameric like'//ascii7(ichar(' ')+1:len(ascii7)-1) )
   call check('upper',upper(temp)==lower(temp),'expect no difference')
   call check('upper',temp==upper(temp),'expect no change')

end subroutine test_upper

subroutine test_lower()
type(unicode_type)  :: upp, low, lowkludge, temp, letter1, letter2
integer             :: i
integer,allocatable :: codes(:)
character(len=128)  :: ascii7
!
! remember unicode characters are multi-byte so be careful
! with older compilers to not exceed 132 bytes per line
!
   low='&
   &abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëì&
   &íîïðñòóôõöøùúûüýþÿāăąćĉċčďđēĕėęěĝğġģĥħĩīĭ&
   &įıĳĵķĺļľŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷźżž&
   &ƃƅƈƌƒƙơƣƥƨƭưƴƶƹƽǆǉǌǎǐǒǔǖǘǚǜǟǡǣǥǧǩǫǭǯǳǵǻǽǿ&
   &ȁȃȅȇȉȋȍȏȑȓȕȗɓɔɗɘəɛɠɣɨɩɯɲɵʃʈʊʋʒάέήί&
   &αβγδεζηθικλμνξοπρστυφχψωϊϋόύώϣϥϧϩϫϭϯабвгдежзий&
   &клмнопрстуфхцчшщъыьэюяёђѓєѕіїјљњћќўџ&
   &ѡѣѥѧѩѫѭѯѱѳѵѷѹѻѽѿҁґғҕҗҙқҝҟҡңҥҧҩҫҭүұҳҵҷҹһҽҿӂӄӈ&
   &ӌӑӓӕӗәӛӝӟӡӣӥӧөӫӯӱӳӵӹաբգդեզէըթժիլխծկհձղ&
   &ճմյնշոչպջռսվտրցւփքօֆაბგდევზთიკლმნოპჟრსტუფქ&
   &ღყშჩცძწჭხჯჰჱჲჳჴჵḁḃḅḇḉḋḍḏḑḓḕḗḙḛḝḟḡḣḥḧ&
   &ḩḫḭḯḱḳḵḷḹḻḽḿṁṃṅṇṉṋṍṏṑṓṕṗṙ&
   &ṛṝṟṡṣṥṧṩṫṭṯṱṳṵṷṹṻṽṿ&
   &ẁẃẅẇẉẋẍẏẑẓẕạảấầẩẫậắằẳẵặẹẻẽếềểễệỉịọỏốồổỗ&
   &ộớờởỡợụủứừửữựỳỵỷỹἀἁἂἃἄἅἆἇἐἑἒἓἔἕἠἡἢἣἤἥἦἧἰἱ&
   &ἲἳἴἵἶἷὀὁὂὃὄὅὑὓὕὗὠὡὢὣὤὥὦὧᾀᾁᾂᾃᾄᾅᾆᾇᾐᾑᾒᾓᾔᾕᾖᾗ&
   &ᾠᾡᾢᾣᾤᾥᾦᾧᾰᾱῐῑῠῡⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ&
   &ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ'
   upp='&
   &ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊË&
   &ÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞŸĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬ&
   &ĮIĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŹŻ&
   &ŽƂƄƇƋƑƘƠƢƤƧƬƯƳƵƸƼǄǇǊǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮǱǴǺǼǾ&
   &ȀȂȄȆȈȊȌȎȐȒȔȖƁƆƊƎƏƐƓƔƗƖƜƝƟƩƮƱƲƷΆΈΉΊΑΒΓΔΕΖΗΘ&
   &ΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫΌΎΏϢϤϦϨϪϬϮАБВГДЕЖЗИЙ&
   &КЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯЁЂЃЄЅІЇЈЉЊЋЌЎЏѠѢѤѦѨѪѬ&
   &ѮѰѲѴѶѸѺѼѾҀҐҒҔҖҘҚҜҞҠҢҤҦҨҪҬҮҰҲҴҶҸҺҼҾӁӃӇ&
   &ӋӐӒӔӖӘӚӜӞӠӢӤӦӨӪӮӰӲӴӸԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿՀՁՂՃՄ&
   &ՅՆՇՈՉՊՋՌՍՎՏՐՑՒՓՔՕՖႠႡႢႣႤႥႦႧႨႩႪႫႬႭႮႯႰႱႲႳႴႵ&
   &ႶႷႸႹႺႻႼႽႾႿჀჁჂჃჄჅḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮ&
   &ḰḲḴḶḸḺḼḾṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾ&
   &ẀẂẄẆẈẊẌẎẐẒẔẠẢẤẦẨẪẬẮẰẲẴẶẸẺẼẾỀỂỄỆỈỊ&
   &ỌỎỐỒỔỖỘỚỜỞỠỢỤỦỨỪỬỮỰỲỴỶỸ&
   &ἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹ&
   &ἺἻἼἽἾἿὈὉὊὋὌὍὙὛὝὟὨὩὪὫὬὭὮὯ&
   &ᾈᾉᾊᾋᾌᾍᾎᾏᾘᾙᾚᾛᾜᾝᾞᾟᾨᾩᾪᾫᾬᾭᾮᾯᾸᾹ&
   &ῘῙῨῩⒶⒷⒸⒹⒺⒻⒼⒽⒾⒿⓀⓁⓂⓃⓄⓅⓆⓇⓈⓉⓊⓋⓌⓍⓎⓏ&
   &ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ'

   temp=lower(upp)
   ! known conundrum at 82 i ı
   do i=1,len(temp)
      letter1=temp%character(i)
      letter2=low%character(i,i)
      if ( letter1 /= letter2 )then
         write(*,g1)i,letter1%character(), letter2%character()
      endif
   enddo
   codes=low%codepoint()
   codes(82)=ichar('i')
   lowkludge=codes
   !call check('lower', temp==low )
   !call check('lower', character(temp)==character(low) )
   call check('lower', temp==lowkludge )
   call check('lower', character(temp)==character(lowkludge) )

   write(ascii7,g0)(achar(i),i=0,127)
   ascii7( ichar('a')+1:ichar('z')+1 ) = ' '
   ascii7( ichar('A')+1:ichar('Z')+1 ) = ' '
   temp=ascii7
   !call check('lower',temp%character()==ascii7,'check non-alphameric like'//ascii7(ichar(' ')+1:len(ascii7)-1) )
   call check('lower',upper(temp)==lower(temp),'expect no difference')
   call check('lower',temp==lower(temp),'expect no change')

end subroutine test_lower

subroutine test_tokenize()
type(unicode_type),allocatable   :: tokens(:), expected(:)
type(unicode_type),allocatable   :: separators(:)
type(unicode_type)               :: delims
type(unicode_type)               :: herbs
character(len=:),allocatable     :: line
integer,allocatable,dimension(:) :: begins
integer,allocatable,dimension(:) :: ends
integer                          :: i

   delims = ' ,&'
   herbs  = 'parsley,sage,rosemary&thyme'
   expected=[ut('parsley'),ut('sage'),ut('rosemary'),ut('thyme') ]

   CALL TOKENIZE (herbs, delims, tokens, separators)
   line='tokens:'
   do i=1,size(tokens)
      line=line//'['//tokens(i)%character()//']'
   enddo
   call check('tokenize', all(tokens == expected), line )

   expected=[ut(','),ut(','),ut('&') ]
   line='separators:'
   do i=1,size(separators)
      line=line//'['//separators(i)%character()//']'
   enddo
   call check('tokenize', all(separators == expected), line )

   CALL TOKENIZE (herbs, delims, begins, ends)
   call check('tokenize',size(begins) == 4 ,'size of begins')
   call check('tokenize',size(ends) == 4 ,'size of ends')
   if(size(begins).eq.4 .and. size(ends).eq.4 )then
      call check('tokenize',all(begins == [1,9,14,23]) ,'begins')
      call check('tokenize',all(ends == [7,12,21,27]) ,'ends')
   endif

   ! OOP

   herbs='parsley😃sage😃rosemary😃😃thyme'
   delims='😃'
   expected=[ut('parsley'),ut('sage'),ut('rosemary'),ut(''),ut('thyme') ]
   tokens=herbs%tokenize(delims)
   line='tokens:'
   do i=1,size(tokens)
      line=line//'['//tokens(i)%character()//']'
   enddo
   call check('tokenize', all(tokens == expected), line )


end subroutine test_tokenize

subroutine test_sort()
type(unicode_type),allocatable :: array(:)

! create using ASCII array
! --------------------------------
! bug?
! array= [ 'red    ','green  ','blue   ','yellow ','orange ','black  ','white  ','brown  ','gray   ','cyan   ','magenta','purple ']
! Fortran runtime error: Array bound mismatch for dimension 1 of array 'array' (25769804402/12)
! --------------------------------
allocate(array(12))  ! gfortran requires this
array(:)= [ 'red    ','green  ','blue   ','yellow ','orange ','black  ','white  ','brown  ','gray   ','cyan   ','magenta','purple ']
call chk()

! create using UNICODE_TYPE array
array= [ ut('RED'),ut('GREEN'),ut('BLUE'),ut('YELLOW'),ut('ORANGE'),ut('BLACK'), &
& ut('WHITE'),ut('BROWN'),ut('GRAY'),ut('CYAN'),ut('MAGENTA'),ut('PURPLE')]
call chk()

contains
subroutine chk()
integer                        :: temp
integer                        :: csz
integer                        :: i
integer,allocatable            :: ints(:)
   if (allocated(ints))deallocate(ints)
   allocate(ints(size(array)))
   call sort(array,ints)
   csz=size(array)
   array=array(ints)

   temp=total
   call check('sort',all(array(1:csz-1) .le. array(2:csz)),'sort array') ! verify in ascending order
   if(total.ne.temp)then
      do i=1,size(array)
         write(*,g0)array(i)%character()
      enddo
   endif
end subroutine chk

end subroutine test_sort

subroutine test_other()
type(unicode_type)         :: string
character(len=*),parameter :: upagain=&
"七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。"
! Romanization:
! Nanakorobi yaoki. Koronde mo mata tachiagaru. Kujikezu ni mae o muite aruite ikou.
! or English translation
! "Fall seven times, stand up eight. Even if you fall down, you will get up again. Don't be discouraged, just keep walking forward."
!
   string=upagain
   write(*,g0)'original bytes  :', upagain
   !-------------------
   ! doing this generates non-unicode byte sequences in UTF-8, which means
   ! the OS may not longer recognize the output as UTF-8
   !write(*,g0)'bytes reversed  :', (upagain(i:i),i=len(upagain),1,-1)
   !-------------------
   write(*,g0)'original string :', string%character()
   write(*,g0)'string reversed :', string%character(string%len(),1,-1)
end subroutine test_other

subroutine test_operators()
type(unicode_type)             :: lhs, rhs
character(len=:),allocatable   :: astr
type(unicode_type)             :: smiley
type(unicode_type)             :: ut_str
   smiley='😃'
   write(*,g0)
   astr='Hello World and Ni Hao -- 你好'
   ut_str=astr
   call checkits('convert to ASCII bytes',astr,ut_str%byte(),transfer('Hello World and Ni Hao -- 你好',['A']))

   ut_str=smiley .cat. ' and ' .cat. smiley .cat. 'and' .cat. smiley .cat. smiley .cat. 'is it'
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

end subroutine test_operators

function random_ascii_string(chars,length) result(out)

!$@(#) M_random::random_string(3f): create random string composed of provided characters of specified length

character(len=*),intent(in)  :: chars
integer,intent(in)           :: length
character(len=:),allocatable :: out
real                         :: x
integer                      :: ilen   ! length of list of characters
integer                      :: which
integer                      :: i
   ilen=len(chars)
   out=''
   if(ilen.gt.0)then
      do i=1,length
         call random_number(x)
         which=nint(real(ilen-1)*x)+1
         out=out//chars(which:which)
      enddo
   endif
end function random_ascii_string

subroutine test_split()
type(ut)                   :: proverb
type(ut)                   :: delims
type(ut),allocatable       :: expected(:)
type(ut),allocatable       :: answer(:)
integer                    :: first
integer                    :: last
integer                    :: pos
integer                    :: i

   delims= '=|; '

   proverb="Más vale pájaro en mano, que ciento volando."
   expected=[ ut("Más"), ut("vale"), ut("pájaro"), ut("en"), ut("mano,"), ut("que"), ut("ciento"), ut("volando.") ]

   pos = 0
   i=0
   do while (pos < len(proverb))
      first = pos + 1
      call split (proverb, delims, pos)
      last = pos - 1
      i=i+1
      call check('split', proverb%character(first,last) == expected(i)%character() ,expected(i)%character() )
   enddo

   ! OOP
   answer=proverb%split(ut(' '))
   call check('split', size(answer) == size(expected) ,'size')
   if ( size(answer) == size(expected) )then
      call check('split', all(answer == expected) ,' oop all at once')
   endif

end subroutine test_split

subroutine test_scan()
type(ut) :: line
type(ut) :: set
   !     1234567890123456789012345678901234567890
   line='parsley?|sage?|rosemary?|?|thyme'
   line='parsley😃|sage😃|rosemary😃|😃|thyme'
   set='😃|'
   call check('scan', scan(line, set) == 8, 'default')
   call check('scan', scan(line, set, back=.true.) == 27,'back=true')
   call check('scan', scan(line, set, back=.false.) == 8 ,'back=false')
   call check('scan', scan(line, unicode_type("NOT")) == 0,'no match')
   ! OOP
   call check('scan', line%scan(set) == 8,'oop test')
   call check('scan', line%scan(ut("o")) == 17,'oop test')
end subroutine test_scan

subroutine test_verify()
character(len=*),parameter :: int='1234567890'
character(len=*),parameter :: hex='abcdefABCDEF0123456789'
logical                    :: lout
type(unicode_type)         :: chars
type(unicode_type)         :: str

   chars='32‐af43d'
   lout=.true.

   ! are the first two characters integer characters?
   str = chars%character(1,2)
   lout = (verify( str, ut(int) ) == 0) .and.lout 

   ! is the third character a dash?
   str = chars%character(3,3)
   lout = (verify( str, ut('‐-') ) == 0) .and.lout 

   ! is remaining string a valid representation of a hex value?
   str = chars%character(4,8)
   lout = (verify( str, ut(hex) ) == 0) .and.lout 

   call check( 'verify', lout, chars%character() )

end subroutine test_verify

subroutine test_ichar()
type(unicode_type)             :: ut_str
   ut_str='ABC'
   call check('ichar',ut_str%ichar().eq.ichar('A'),'string%ichar()')
   call check('ichar',ichar(ut('A')).eq.ichar('A'),'ichar(ut("A")')
   call check('ichar',ichar(ut_str%sub(2,3)).eq.ichar('B'),'ichar(ut_str%sub(2,3))')
end subroutine test_ichar

subroutine test_escape()
type(unicode_type)  :: ut_str
integer,allocatable :: ints(:)
!    \      backslash
!    a      alert (BEL) -- g is an alias for a
!    b      backspace
!    c      suppress further output
!    e      escape
!    f      form feed
!    n      new line
!    r      carriage return
!    t      horizontal tab
!    v      vertical tab
!
!    oNNN   byte with octal value NNN (3 digits)
!    0-9    up to three digits following will be treated
!           as an octal value

!    dNNN   byte with decimal value NNN (3 digits)

!    xHH        byte with hexadecimal value HH (2 digits);
!               h is an alias for x
!    uZZZZ      translate Unicode codepoint value to bytes
!    UZZZZZZZZ  translate Unicode codepoint value to bytes
   ut_str='\\\a\b\e\f\n\r\t\v\c'
   ints=[92,7,8,27,12,10,13,9,11]
   ut_str=escape(ut_str)
   call check('escape',len(ut_str).eq.9,'size')
   if( len(ut_str).eq.9 )then
      call check('escape',all(ut_str%codepoint().eq.ints),'codes')
   endif
   call check('escape',escape(ut('\')).eq.'\','backslash at end of line')
   call check('escape',escape(ut('text\0')).eq.'text'//char(0),'null at end')
   call check('escape',escape(ut('\122\123A')).eq.'RSA','two')

   ! (kaufii hai?) [Literal Meaning: “Is there coffee?”] “Do you have coffee?” (Informal)
   ut_str='\u0915\u0949\u092B\U0000093C\U00000940\x20\u0939\u0948\x3F'
   call check('escape',escape(ut_str).eq.'कॉफ़ी है?','hexadecimal')
end subroutine test_escape

subroutine test_join()
character(len=20),allocatable :: proverb(:)
type(ut),allocatable       :: s(:)
type(ut),allocatable       :: sep
type(ut)                   :: expected
   proverb=[ character(len=13) :: &
     & ' United'       ,&
     & '  we'          ,&
     & '   stand,'     ,&
     & '    divided'   ,&
     & '     we fall.' ]
   allocate(s(size(proverb))) ! avoid GNU Fortran (GCC) 16.0.0 bug
   s=proverb

   expected='Unitedwestand,dividedwe fall.'
   call check('join', join(s) == expected, 'SIMPLE JOIN' )

   expected='United we stand, divided we fall.'
   call check('join', join(s,sep=ut(' ') ) == expected, 'JOIN WITH SEPARATOR' )

   expected='United<-->we<-->stand,<-->divided<-->we fall.'
   call check('join', join(s,sep=ut('<-->')) == expected, 'CUSTOM SEPARATOR' )

   expected=' United               we                   stand,               divided              we fall.'
   call check('join', join(s,clip=.false.)  == expected, 'NO TRIMMING' )

   sep=ut()
   expected='Unitedwestand,dividedwe fall.'
   call check('join', sep%join(s)==expected, 'OOP SIMPLE JOIN')
   sep=' '
   expected='United we stand, divided we fall.'
   call check('join', sep%join(s)==expected, 'OOP JOIN WITH SEPARATOR')
   sep='<-->'
   expected='United<-->we<-->stand,<-->divided<-->we fall.'
   call check('join', sep%join(s)==expected, 'OOP CUSTOM SEPARATOR')
   sep=''
   expected=' United               we                   stand,               divided              we fall.'
   call check('join', sep%join(s,clip=.false.)==expected, 'OOP NO TRIMMING')

end subroutine test_join

subroutine test_pad()
type(ut)                   :: string
type(ut)                   :: answer
! 
   string='abcdefghij'
   answer='[abcdefghij          ]'
   call check('pad',bracket(pad(string,20)) == answer,'pad on right till 20 characters long')
   answer='[abcdefghij]'
   call check('pad',bracket(pad(string,5)) == answer,'original is not truncated for short specified length')

   ! non-blank pattern
   ! pad on left
   call check('pad', pad(ut('12'),5,ut('0'),right=.false.) == '00012', 'pad on left with zeros')

   string='12345 '

   call check('pad', bracket(pad(string,15,ut('_'),right=.false.,clip=.true.)) == '[__________12345]',&
   &character(bracket(pad(string,15,ut('_'),right=.false.,clip=.true.))) )
   call check('pad', bracket(pad(string,15,ut('_'),right=.false.,clip=.false.)) == '[_________12345 ]',&
   &character(bracket(pad(string,15,ut('_'),right=.false.,clip=.false.))) )

   call check('pad', bracket(pad(string, 7,ut('_'),right=.false.,clip=.true.)) == '[__12345]',&
   &character(bracket(pad(string,7,ut('_'),right=.false.,clip=.true.))) )
   call check('pad', bracket(pad(string, 7,ut('_'),right=.false.,clip=.false.)) == '[_12345 ]',&
   &character(bracket(pad(string,7,ut('_'),right=.false.,clip=.false.))) )

   call check('pad', bracket(pad(ut('12345 '), 6,ut('_'),right=.false.,clip=.true.)) == '[_12345]',&
   &character(bracket(pad(string,6,ut('_'),right=.false.,clip=.true.))) )
   call check('pad', bracket(pad(ut('12345 '), 6,ut('_'),right=.false.,clip=.false.)) == '[12345 ]',&
   &character(bracket(pad(string,6,ut('_'),right=.false.,clip=.false.))) )

   call check('pad', bracket(pad(ut('12345 '), 5,ut('_'),right=.false.,clip=.true.)) == '[12345]',&
   &character(bracket(pad(string,5,ut('_'),right=.false.,clip=.true.))) )
   call check('pad', bracket(pad(ut('12345 '), 5,ut('_'),right=.false.,clip=.false.)) == '[12345 ]',&
   &character(bracket(pad(string,5,ut('_'),right=.false.,clip=.false.))) )

   call check('pad', bracket(pad(ut('12345 '), 4,ut('_'),right=.false.,clip=.true.)) == '[12345]',&
   &character(bracket(pad(string,4,ut('_'),right=.false.,clip=.true.))) )
   call check('pad', bracket(pad(ut('12345 '), 4,ut('_'),right=.false.,clip=.false.)) == '[12345 ]',&
   &character(bracket(pad(string,4,ut('_'),right=.false.,clip=.false.))) )

contains 
function bracket(line) result (bracketed)
type(unicode_type),intent(in) :: line
type(unicode_type)            :: bracketed
   bracketed='['.cat.line.cat.']'
end function bracket
end subroutine test_pad

subroutine test_sub()
type(unicode_type)           :: line
line='this is the string'
call check('sub', 'this is the string'  == character( sub(line    ) ) )
call check('sub', 'the string'          == character( sub(line, 9 ) ) ) 
call check('sub', 'this is '  == character( sub(line,  1,        8) ) )
call check('sub', 'the string'== character( sub(line,  9,len(line)) ) ) 
call check('sub', 'is the'    == character( sub(line,  6,       11) ) )

call check('sub', 'this is the string'  == character( line%sub(            ) ) )
call check('sub', 'the string'          == character( line%sub( 9          ) ) )
call check('sub', 'this is '            == character( line%sub( 1,        8) ) )
call check('sub', 'the string'          == character( line%sub( 9,len(line)) ) )
call check('sub', 'is the'              == character( line%sub( 6,       11) ) )
!
end subroutine test_sub

subroutine test_replace()
type(unicode_type)           :: line
character(len=:),allocatable :: aline
! 
call check('replace',&
 'this is the string' == character( replace(ut('Xis is Xe string'),ut('X'),ut('th') ) ) )
call check('replace',&
 'this is the string'==character( replace(ut('Xis is xe string'),ut('x'),ut('th'),ignorecase=.true.) ) )
call check('replace',&
 'this is xe string'==character( replace(ut('Xis is xe string'),ut('X'),ut('th'),ignorecase=.false.) ) )

call check('replace',&
 'BEFORE:my line of text'==character(replace(ut('my line of text'),ut(''),ut('BEFORE:'))),&
 'a null new substring means "at beginning of line"' )
! 
call check('replace',&
 'I wonder'== character(replace(ut('I wonder i ii iii'),ut('i'),ut('')) ),&
 'a null new string deletes occurrences of the old substring' )
! 
! Examples of the use of RANGE
! 
line=replace(ut('aaaaaaaaa'),ut('a'),ut('A'),occurrence=1,repeat=1)
call check('replace', line == ut('Aaaaaaaaa'), 'replace first a with A ['//line%character()//']' )
! 
line=replace(ut('aaaaaaaaa'),ut('a'),ut('A'),occurrence=3,repeat=3)
call check('replace', line==ut('aaAAAaaaa'),'replace a with A for 3rd to 5th occurrence ['//line%character()//']')
! 
line=replace(ut('ababababa'),ut('a'),ut(''),occurrence=3,repeat=3)
call check('replace',line==ut('ababbb'),'replace a with null instances 3 to 5 ['//line%character()//']' )
! 
line=replace( &
 & ut('a b ab baaa aaaa aa aa a a a aa aaaaaa'),&
 & ut('aa'),ut('CCCC'),occurrence=-1,repeat=1)
call check('replace', line == ut('a b ab baaa aaaa aa aa a a a aa aaaaCCCC'),'replace last aa with CCCC ['//line%character()//']')
! 
line=replace(ut('myf90stuff.f90.f90'),ut('f90'),ut('for'),occurrence=-1,repeat=1)
call check('replace',line== 'myf90stuff.f90.for')

line=replace(ut('myf90stuff.f90.f90'),ut('f90'),ut('for'),occurrence=-2,repeat=2)
call check('replace',line=='myforstuff.for.f90')
! 
line='ABCDEFGHIJ'
call check('replace',ut('ABCdEFGHIJ')   == replace(line,4,4,'d'),    'replace a column ')
call check('replace',ut('ABCGHIJ')      == replace(line,4,6,''),     'remove columns in middle')
call check('replace',ut('ABCDE')        == replace(line,6,10,''),    'remove columns at end')
call check('replace',ut('ABCDEend')     == replace(line,6,10,'end'), 'replace end')
call check('replace',ut('startDEFGHIJ') == replace(line,1,3,'start'),'replace start')
call check('replace',ut('FGHIJ')        == replace(line,1,5,''),     'remove start '//ch(replace(line,1,5,'')))
!
! combinations of character and string parameters
call check('replace',ut('ABC[def]GHIJ') == replace( line,   'DEF',    '[def]'), 'replace string')
call check('replace',ut('ABC[def]GHIJ') == replace( line,ut('DEF'),ut('[def]')),'replace string')
call check('replace',ut('ABC[def]GHIJ') == replace( line,   'DEF', ut('[def]')),'replace string')
call check('replace',ut('ABC[def]GHIJ') == replace( line,ut('DEF'),   '[def]'), 'replace string')
     aline=line
call check('replace',ut('ABC[def]GHIJ') == replace(aline,   'DEF',    '[def]'), 'replace string')
call check('replace',ut('ABC[def]GHIJ') == replace(aline,ut('DEF'),ut('[def]')),'replace string')
call check('replace',ut('ABC[def]GHIJ') == replace(aline,   'DEF', ut('[def]')),'replace string')
call check('replace',ut('ABC[def]GHIJ') == replace(aline,ut('DEF'),   '[def]'), 'replace string')

!ifx bug!call check('replace',ut('ABC[def]GHIJ') == line%replace(   'DEF',    '[def]'), 'replace string')
!ifx bug!call check('replace',ut('ABC[def]GHIJ') == line%replace(ut('DEF'),ut('[def]')),'replace string')
!ifx bug!call check('replace',ut('ABC[def]GHIJ') == line%replace(   'DEF', ut('[def]')),'replace string')
!ifx bug!call check('replace',ut('ABC[def]GHIJ') == line%replace(ut('DEF'),   '[def]'), 'replace string')

!ifx bug!call check('replace',ut('ABCdEFGHIJ')  == line%replace(4,4,'d'),     'oop replace a column')
!ifx bug!call check('replace',ut('ABCDE')       == line%replace(6,10,''),     'oop remove columns')
!ifx bug!call check('replace',ut('ABCDEend')    == line%replace(6,10,'end'),  'oop replace end')
!ifx bug!call check('replace',ut('startDEFGHIJ')== line%replace(1,3,'start'), 'oop replace start')

!ifx bug!call check('range',ut('ABCdEFGHIJ')  == line%range(4,4,'d'),     'oop remove a column')
!ifx bug!call check('range',ut('ABCDE')       == line%range(6,10,''),     'oop remove columns')
!ifx bug!call check('range',ut('ABCDEend')    == line%range(6,10,'end'),  'oop remove end')
!ifx bug!call check('range',ut('startDEFGHIJ')== line%range(1,3,'start'), 'oop remove start')

end subroutine test_replace

end module testsuite_M_unicode

program test_M_unicode
use testsuite_M_unicode
   total = 0

   !write(*,g0)'encoding can be altered on an open file'
   !open (output_unit, encoding='UTF-8')

   call programming_environment()
   call test_adjustl()
   call test_adjustr()
   call test_trim()
   call test_len_trim()
   call test_len()
   call test_index()
   call test_repeat()
   call test_upper()
   call test_lower()
   call test_tokenize()
   call test_sort()
   call test_operators()
   call test_split()
   call test_scan()
   call test_verify()
   call test_ichar()
   call test_replace()
   call test_sub()
   call test_pad()
   call test_join()
   call test_expandtabs()
   call test_fmt()
   call test_escape()
   call test_concatenate()
   call test_other()

   write(*,g0)
   if(total.ne.0)then
      write(*,g0)total,' failures'
      stop 1
   else
      write(*,g0)'all passed'
   endif

end program test_M_unicode
