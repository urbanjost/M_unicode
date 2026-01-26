program upside_down
use M_unicode
!use M_unicode, only : assignment(=), ch=>character, ut=>unicode_type
use M_unicode, only : ch=>character, ut=>unicode_type
type(ut)                     :: uline
integer                      :: i
character(len=128)           :: ascii7
character(len=:),allocatable :: cmdline
   cmdline=getargs()
   if(cmdline.eq.'')then
      do i=0,127
         ascii7(i+1:i+1)=char(i)
      enddo
      uline=downside_up(ascii7(32:126))
      write(*,*)uline%codepoint()
      write(*,*)ch(uline)
   else
      uline=downside_up(cmdline)
      write(*,*)ch(uline)
   endif
!   do i=int(z'A4d0'),int(z'A4FF') ! Lisu (Fraser script) Unicode block 
!      uline = i
!      write(*,'(i0,1x,z0,1x,a)')i,i,ch(uline)
!   enddo
contains
function downside_up(string) result(uline)
character(len=*),intent(in) :: string
type(ut)                    :: uline
character(len=1)            :: letter
integer                     :: i
integer                     :: which
integer                     :: ichr
integer                     :: ichrs(len(string))
   which=1
   do i=1,len(string)
      letter=string(i:i)
      select case(letter)
      case(' '); ichr=iachar(' ')
      case('!'); ichr=int(z'00A1')
      case('"'); ichr=int(z'201E')
      case('#'); ichr=iachar('#')
      case('$'); ichr=iachar('$')
      case('%'); ichr=iachar('%')
      case('&'); ichr=int(z'214B')
      case(''''); ichr=int(z'002C')
      case('('); ichr=int(z'0029')
      case(')'); ichr=iachar('(')
      case('*'); ichr=iachar('*')
      case('+'); ichr=iachar('+')
      case(','); ichr=iachar('''')
      case('-'); ichr=iachar('-')
      case('.'); ichr=int(z'02D9')
      case('/'); ichr=iachar('/')
      case('0'); ichr=iachar('0')
      case('1'); ichr=int(z'0196')
      case('2'); ichr=int(z'1105')
      case('3'); ichr=int(z'0190')
      case('4'); ichr=int(z'152D')
      if(which.eq.2)ichr=int(z'3123')
      if(which.eq.3)ichr=iachar('h')
      case('5'); ichr=int(z'03DB')
      case('6'); ichr=iachar('9')
      case('7'); ichr=int(z'2C62')
      if(which.eq.2)ichr=int(z'3125')
      case('8'); ichr=iachar('8')
      case('9'); ichr=iachar('6')
      case(':'); ichr=iachar(':')
      case(';'); ichr=int(z'061B')
      case('<'); ichr=iachar('>')
      case('='); ichr=iachar('=')
      case('>'); ichr=iachar('<')
      case('?'); ichr=int(z'00BF')
      case('@'); ichr=iachar('@')
      ! Default ∀𐐒Ↄ◖ƎℲ⅁HIſ⋊⅂WᴎOԀΌᴚS⊥∩ᴧMX⅄Z
      case('A'); ichr=int(z'2200')
         if(which.eq.4)ichr=int(z'A4EF') ! ꓯ
      case('B'); ichr=int(z'00010412')
         if(which.eq.4)ichr=int(z'A4ED') ! ꓭ
      case('C'); ichr=int(z'2183')
         if(which.eq.2)ichr=int(z'0186')
         ichr=int(z'A4DB') ! ꓛ
      case('D'); ichr=int(z'25D6')
         ichr=int(z'A4F7') ! ꓷ
      case('E'); ichr=int(z'018E')
         ichr=int(z'A4F1') ! ꓱ
      case('F'); ichr=int(z'2132')
         if(which.eq.4)ichr=int(z'A4DE') ! ꓞ
      case('G'); ichr=int(z'2141')
         ichr=int(z'A4E8') ! ꓨ
         if(which.eq.2)ichr=int(z'05E4')
      case('H'); ichr=iachar('H')
         ichr=int(z'A4E7') ! ꓧ
      case('I'); ichr=iachar('I')
         ichr=int(z'A4F2') ! ꓲ
      case('J'); ichr=int(z'017F')
         ichr=int(z'A4E9') ! ꓩ
      case('K'); ichr=int(z'22CA')
         ichr=int(z'A4D8') ! ꓘ
      case('L'); ichr=int(z'2142')
         if(which.eq.4)ichr=int(z'A4F6') ! ꓶ
         if(which.eq.2)ichr=int(z'02E5')
      case('M'); ichr=ichar('W')
         ichr=int(z'A4EA') ! ꓪ
      case('N'); ichr=int(z'1D0E')
         ichr=int(z'A4E0') ! ꓠ
      case('O'); ichr=iachar('O')
         ichr=int(z'A4F3') ! ꓳ
      case('P'); ichr=int(z'0500')
         ichr=int(z'A4D2') ! ꓒ
      case('Q'); ichr=int(z'038C')
      case('R'); ichr=int(z'1D1A')
         ichr=int(z'A4E4') ! ꓤ
      case('S'); ichr=iachar('S')
         ichr=int(z'A4E2') ! ꓢ
      case('T'); ichr=int(z'22A5')
         ichr=int(z'A4D5') ! ꓕ
         if(which.eq.2)ichr=int(z'2534')
      case('U'); ichr=int(z'2229')
         ichr=int(z'A4F5') ! ꓵ
      case('V'); ichr=int(z'1D27')
         ichr=int(z'A4E5') ! ꓥ
         if(which.eq.2)ichr=int(z'039B')
      case('W'); ichr=iachar('M')
         ichr=int(z'A4DF') ! ꓟ
      case('X'); ichr=iachar('X')
         ichr=int(z'A4EB') ! ꓫ
      case('Y'); ichr=int(z'2144')
      case('Z'); ichr=iachar('Z')
         ichr=int(z'A4DC') ! ꓜ
      case('['); ichr=iachar(']')
      case('\'); ichr=iachar('\')
      case(']'); ichr=iachar('[')
      case('^'); ichr=iachar('v')
      case('_'); ichr=int(z'203E')
      case('`'); ichr=iachar(',')
      case('a'); ichr=int(z'0250')
      case('b'); ichr=iachar('q')
      case('c'); ichr=int(z'0254')
      case('d'); ichr=iachar('p')
      case('e'); ichr=int(z'01DD')
      case('f'); ichr=int(z'025F')
      case('g'); ichr=int(z'0183')
      case('h'); ichr=int(z'0265')
      case('i'); ichr=int(z'0131')
      if(which.eq.2)ichr=int(z'1D09')
      case('j'); ichr=int(z'027E')
      case('k'); ichr=int(z'029E')
      case('l'); ichr=int(z'0283')
      case('m'); ichr=int(z'026F')
      case('n'); ichr=iachar('u')
      case('o'); ichr=iachar('o')
      case('p'); ichr=iachar('d')
      case('q'); ichr=iachar('b')
      case('r'); ichr=int(z'0279')
      case('s'); ichr=iachar('s')
      case('t'); ichr=int(z'0287')
      case('u'); ichr=iachar('n')
      case('v'); ichr=int(z'028C')
      case('w'); ichr=int(z'028D')
      case('x'); ichr=iachar('x')
      case('y'); ichr=int(z'028E')
      case('z'); ichr=iachar('z')
      case('{'); ichr=iachar('}')
      case('|'); ichr=iachar('|')
      case('}'); ichr=iachar('{')
      case('~'); ichr=iachar('~')
      case default
                 ichr=iachar(letter)
      end select
      ichrs( size(ichrs)-i+1 )=ichr
   enddo
   uline = ichrs
end function downside_up
function getargs() result(command_line)
integer                                :: length
character(len=:),allocatable           :: command_line
   call get_command(length=length)                 ! get command line length
   allocate(character(len=length) :: command_line) ! allocate string big enough to hold command line
   call get_command(command=command_line)          ! get command line as a string
   call get_command_argument(0,length=length)      ! remove argument 0
   command_line=adjustl(command_line(length+2:))
end function getargs
end program upside_down
