module M_unicode__get
use,intrinsic :: iso_fortran_env, only : stdout => output_unit
use,intrinsic :: iso_fortran_env, only : stdin  => input_unit
use,intrinsic :: iso_fortran_env, only : stderr => error_unit
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
private

public :: get_env
public :: get_arg
public :: get_env_character, get_env_integer, get_env_real, get_env_double, get_env_logical
public :: get_arg_character, get_arg_integer, get_arg_real, get_arg_double, get_arg_logical

interface get_env
   module procedure get_env_character
   module procedure get_env_integer
   module procedure get_env_real
   module procedure get_env_double
   module procedure get_env_logical
end interface get_env

interface get_arg
   module procedure get_arg_character
   module procedure get_arg_integer
   module procedure get_arg_real
   module procedure get_arg_double
   module procedure get_arg_logical
end interface get_arg

type :: force_keywords ! force keywords, using @awvwgk method
end type force_keywords
!
! so then any argument that comes after "force_kwargs" is a compile time error
! if not done with a keyword unless someone "breaks" it by passing something
! of this type:
!    type(force_keywords), optional, intent(in) :: force_kwargs
!
! kludge: block
! type(force_keywords),volatile :: quiet_
!    if( present(force_) )quiet_=force_  ! so compiler does not complain about force_ being unused
! endblock kludge
!
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!### NAME
!!    get_arg(3f) - [M_unicode:QUERY] acquire value from command line
!!    (LICENSE:PD)
!!
!!### SYNOPSIS
!!
!!    function get_arg(n,default,ierr) result(arg)
!!
!!      integer,intent(in)           :: n
!!
!!      ! ONE OF THE FOLLOWING:
!!      character(len=*)             :: default
!!      type(unicode_type)           :: default
!!      integer                      :: default
!!      real                         :: default
!!      doubleprecision              :: default
!!      logical                      :: default
!!
!!      integer,intent(out),optional :: ierr
!!
!!      ! ONE OF THE FOLLOWING, MATCHING TYPE OF DEFAULT
!!      character(len=:),allocatable :: arg
!!      type(unicode_type)           :: arg
!!      integer                      :: arg
!!      real                         :: arg
!!      doubleprecision              :: arg
!!      logical                      :: arg
!!
!!### DESCRIPTION
!!
!! get_arg(3f) returns the Nth command line argument.
!!
!!### OPTIONS
!!    n        The number of the argument, from 0 to the count of the
!!             total number of arguments.
!!    default  The value to return if the argument is not present. The
!!             type of the value determines the type of the returned
!!             value. It may be REAL, DOUBLE_PRECISION, INTEGER, CHARACTER,
!!             LOGICAL or TYPE(UNICODE_TYPE).  The default type returned
!!             is CHARACTER.
!!
!!### RETURNS
!!    arg      The value of the Nth command line argument after attempting
!!             to convert it to the same type as DEFAULT.
!!
!!    ierr     zero if no error occurred. Keyword syntax is required.
!!
!!### EXAMPLES
!!
!!
!!  Sample program
!!
!!    program demo_get_arg
!!    use M_unicode, only : get_arg, ut=> unicode_type, ch=>character
!!    use M_unicode, only : assignment(=), operator(//), write(formatted)
!!    implicit none
!!    integer                      :: position
!!    type(ut)                     :: udefault
!!    type(ut)                     :: uvalue
!!    type(ut)                     :: smiley
!!    integer                      :: ierr
!!    integer                      :: j
!!    character(len=*),parameter   :: g='(*(g0))'
!!
!!    integer                      :: i
!!    real                         :: r
!!    doubleprecision              :: d
!!    logical                      :: tf
!!    character(len=:),allocatable :: c
!!
!!    ! basics
!!    do i=1,command_argument_count()
!!       write(*,g)i,' ',get_arg(i)
!!    enddo
!!    !
!!    smiley=128515 ! smiley emoji set with Unicode code point
!!    udefault='Wish I was there '//smiley//'!'
!!    !
!!    if(command_argument_count().eq.0)then
!!       call execute_command_line(get_arg(0)//' one T 100 100.2 300.0e3 ')
!!    else
!!       do position=0,command_argument_count()
!!
!!          write(*,g) repeat('=',50)
!!
!!          ! returns type(unicode_type)
!!          uvalue=get_arg(position, udefault)
!!          write(*,g)uvalue%character()
!!          ! oop
!!          write(*,'("OOP:",DT)')udefault%get_arg(position)
!!
!!          ! returns character which is assigned to type(unicode_type)
!!          uvalue=get_arg(position, udefault%character() )
!!          write(*,g)uvalue%character()
!!
!!          write(*,g)get_arg(position,' character default')
!!
!!          c=get_arg(position)
!!
!!          tf=get_arg(position,.false.,ierr=ierr)
!!          if(ierr.eq.0)write(*,g)position,' ',c,' is a good logical: ',tf
!!
!!          r=get_arg(position,0.0,ierr=ierr)
!!          if(ierr.eq.0)write(*,g)position,' ',c,' is a good real: ',r
!!
!!          d=get_arg(position,0.0d0,ierr=ierr)
!!          if(ierr.eq.0)write(*,g)position,' ',c,' is a good double: ',d
!!
!!          i=get_arg(position,0,ierr=ierr)
!!          if(ierr.eq.0)write(*,g)position,' ',c,' is a good integer: ',i
!!
!!       enddo
!!    endif
!!    end program demo_get_arg
!!
!!  Results
!!   > 1 one
!!   > 2 T
!!   > 3 100
!!   > 4 100.2
!!   > 5 300.0e3
!!   > ==================================================
!!   > build/gfortran_F560B09219607C26/example/demo_get_arg
!!   > OOP:build/gfortran_F560B09219607C26/example/demo_get_arg
!!   > build/gfortran_F560B09219607C26/example/demo_get_arg
!!   > build/gfortran_F560B09219607C26/example/demo_get_arg
!!   > ==================================================
!!   > one
!!   > OOP:one
!!   > one
!!   > one
!!   > ==================================================
!!   > T
!!   > OOP:T
!!   > T
!!   > T
!!   > 2 T is a good logical: T
!!   > ==================================================
!!   > 100
!!   > OOP:100
!!   > 100
!!   > 100
!!   > 3 100 is a good real: 100.000000
!!   > 3 100 is a good double: 100.00000000000000
!!   > 3 100 is a good integer: 100
!!   > ==================================================
!!   > 100.2
!!   > OOP:100.2
!!   > 100.2
!!   > 100.2
!!   > 4 100.2 is a good real: 100.199997
!!   > 4 100.2 is a good double: 100.20000000000000
!!   > ==================================================
!!   > 300.0e3
!!   > OOP:300.0e3
!!   > 300.0e3
!!   > 300.0e3
!!   > 5 300.0e3 is a good real: 300000.000
!!   > 5 300.0e3 is a good double: 300000.00000000000
!!
!!### SEE ALSO
!!     get_command_argument(3f), command_argument_count(3f)
!!
!!### AUTHOR
!!    John S. Urban
!!
!!### LICENSE
!!    MIT
function get_arg_character(n,default,force_,ierr) result(arg)
integer,intent(in)                         :: n
character(len=*),intent(in),optional       :: default
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
integer                                    :: argument_length, istat
character(len=:),allocatable               :: arg
   !
   ! allocate string big enough to hold command line argument
   !
   call get_command_argument( number=n, length=argument_length )
   if (allocated(arg))deallocate( arg )
   allocate(character(len=argument_length) :: arg )
   call get_command_argument(n, arg, status=istat )
   if (arg=='')arg = default
   if (present(ierr)) ierr = istat
end function get_arg_character

function get_arg_real(n,DEFAULT,force_,ierr) result(VALUE)
integer,intent(in)                         :: n
real,intent(in)                            :: DEFAULT
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
real                                       :: VALUE
integer                                    :: iostat
character(len=255)                         :: iomsg, fmt
character(len=:),allocatable               :: string
   STRING = get_arg_character(n,'')
   iostat = 0
   iomsg = ''
   if (STRING=='') then
      VALUE = DEFAULT
   else
      write(fmt,'(*(g0))')'(g',max(1,len(string)),'.0)'
      string = string//' '
      read(STRING,fmt,iostat=iostat,iomsg=iomsg)value
      if (iostat/=0) then
         value = -huge(0.0)
      endif
   endif
   if (present(ierr)) then
      ierr = iostat
   elseif (iostat/=0) then
      write(stderr,'(a)')'<ERROR>*get_arg* n=',n,' STRING='//STRING//':'//trim(iomsg)
   endif
end function get_arg_real

function get_arg_double(n,DEFAULT,force_,ierr) result(VALUE)
integer,intent(in)                         :: n
doubleprecision,intent(in)                 :: DEFAULT
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
doubleprecision                            :: VALUE
character(len=:),allocatable               :: STRING
integer                                    :: iostat
character(len=255)                         :: iomsg, fmt
   STRING = get_arg_character(n,'')
   iostat = 0
   iomsg = ''
   if (STRING=='') then
      VALUE = DEFAULT
   else
      write(fmt,'(*(g0))')'(g',max(1,len(string)),'.0)'
      string = string//' '
      read(STRING,fmt,iostat=iostat,iomsg=iomsg)value
      if (iostat/=0) then
         value = -huge(0.0d0)
      endif
   endif
   if (present(ierr)) then
      ierr = iostat
   elseif (iostat/=0) then
      write(stderr,'(a)')'<ERROR>*get_arg* n=',n,' STRING='//STRING//':'//trim(iomsg)
   endif
end function get_arg_double

function get_arg_integer(n,DEFAULT,force_,ierr) result(VALUE)
integer,intent(in)                         :: n
integer,intent(in)                         :: DEFAULT
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
integer                                    :: VALUE
character(len=:),allocatable               :: STRING
integer                                    :: iostat
character(len=255)                         :: iomsg, fmt
   STRING = get_arg_character(n,'')
   iostat = 0
   iomsg = ''
   if (STRING=='') then
      VALUE = DEFAULT
      iostat = 0
   else
      write(fmt,'(*(g0))')'(i',max(1,len(string)),')'
      string = string//' '
      read(STRING,fmt,iostat=iostat,iomsg=iomsg)value
      if (iostat/=0) then
         value = -huge(0)
      endif
   endif
   if (present(ierr)) then
      ierr = iostat
   elseif (iostat/=0) then
      write(stderr,'(a)')'<ERROR>*get_arg* n=',n,' STRING='//STRING//':'//trim(iomsg)
   endif
end function get_arg_integer

function get_arg_logical(n,DEFAULT,force_,ierr) result(VALUE)
integer,intent(in)                         :: n
logical,intent(in)                         :: DEFAULT
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
logical                                    :: VALUE
character(len=:),allocatable               :: STRING
integer                                    :: iostat
character(len=255)                         :: iomsg, fmt
character(len=1)                           :: ch
   VALUE = DEFAULT
   STRING = get_arg_character(n,'',ierr=iostat)
   if (iostat==0) then
      if (STRING=='') then
         VALUE = DEFAULT
      else
         string = string//'  '
         ch = string(1:1)
         if (ch=='.')ch = string(2:2)
         select case (ch)
          case ('t','T','y','Y')
            value = .true.
          case ('f','F','n','N')
            value = .false.
          case default
            iostat=1
            value = .false.
         end select
      endif
   endif

   if (present(ierr)) then
      ierr = iostat
   elseif (iostat/=0) then
      write(stderr,'(a)')'<ERROR>*get_arg* n=',n,' STRING='//STRING//':'//trim(iomsg)
      !1error stop
   endif
end function get_arg_logical
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!### NAME
!!     get_env(3f) - [M_unicode:QUERY] a function returning the value of
!!                   an environment variable
!!     (LICENSE:PD)
!!
!!### SYNTAX
!!    function get_env(NAME,DEFAULT,IERR=IERR) result(VALUE)
!!
!!     character(len=*),intent(in)            :: NAME
!!
!!     ! ONE OF ...
!!     character(len=*),intent(in),optional :: DEFAULT
!!     real,intent(in),optional             :: DEFAULT
!!     integer,intent(in),optional          :: DEFAULT
!!     doubleprecision,intent(in),optional  :: DEFAULT
!!     logical,intent(in),optional          :: DEFAULT
!!     type(unicode_type),optional          :: DEFAULT
!!
!!     integer,intent(out),optional           :: IERR
!!
!!     ! ONE OF THE FOLLOWING, MATCHING TYPE OF DEFAULT
!!     character(len=:),allocatable         :: VALUE
!!     integer                              :: VALUE
!!     real                                 :: VALUE
!!     doubleprecision                      :: VALUE
!!     logical                              :: VALUE
!!     type(unicode_type)                   :: VALUE
!!
!!### DESCRIPTION
!!     Get the value of an environment variable or optionally return a
!!     default value when the environment variable is not set or is set
!!     to a blank string.
!!
!!     The type returned is the same as the type of the default value.
!!
!!### OPTIONS
!!    NAME     name of environment variable.
!!    DEFAULT  value to return if environment variable is not set or set
!!             to an empty string. May be CHARACTER, REAL, INTEGER,
!!             LOGICAL, DOUBLEPRECISION or TYPE(UNICODE_TYPE.
!!             Defaults to a null CHARACTER value.
!!### RETURNS
!!    VALUE    the value of the environment variable or the default.
!!             The type is the same as DEFAULT unless NAME
!!             is CHARACTER but VALUE is DEFAULT, in which case
!!             TYPE(UNICODE_TYPE) is returned. That is, if either NAME
!!             or VALUE is TYPE(UNICODE_TYPE) the returned value is
!!             TYPE(UNICODE_TYPE). If an error occurs and it is numeric,
!!             huge(0|0.0|0.0d0) is returned.
!!
!!             For a LOGICAL type, Any environment variable value starting
!!             with F,f,N or n is .FALSE. and any value starting with
!!             Y,y,T or t is true. A leading period (".") is ignored.
!!             Anything else returns .false. .
!!
!!    IERR     return error code. Must be specified with a keyword.
!!             It is zero if no error occurred.
!!
!!### EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_get_env
!!    use M_unicode, only : get_env, get_arg
!!    use M_unicode, only : ut => unicode_type
!!    use M_unicode, only : assignment(=), operator(//)
!!    implicit none
!!    character(len=*),parameter   :: g='(*(g0))'
!!    integer                      :: ierr
!!    character(len=:),allocatable :: HOME
!!    type(ut)                     :: uname
!!    type(ut)                     :: udefault
!!    type(ut)                     :: uvalue
!!    type(ut)                     :: smiley
!!    integer                      :: i
!!
!!      smiley = 128515 ! set with Unicode code point
!!      udefault = 'Have a nice day '//smiley//'!'
!!
!!      !Basics
!!       HOME=get_env('HOME','UNKNOWN')
!!       write(*,'(a)')HOME
!!       write(*,'(a)')Get_env('PATH')
!!
!!      ! arguments can be type(unicode_type) or character
!!      ! but type(unicode_type) is always returned if either is
!!      ! type(unicode_type)
!!       do i = 1,2
!!          select case (i)
!!          case (1); uname = 'HOME'
!!          case (2); uname = 'UNKNOWN'
!!          end select
!!
!!            uvalue = get_env(uname,             udefault             )
!!            write(*,*)uvalue%character()
!!
!!            uvalue = get_env(uname%character(), udefault%character() )
!!            write(*,*)uvalue%character()
!!
!!            uvalue = get_env(uname,             udefault%character() )
!!            write(*,*)uvalue%character()
!!
!!            uvalue = get_env(uname%character(), udefault             )
!!            write(*,*)uvalue%character()
!!
!!            uvalue = get_env(uname)
!!            write(*,*)uvalue%character()
!!
!!            uvalue = get_env(uname%character())
!!            write(*,*)uvalue%character()
!!
!!         enddo
!!
!!      !call this program setting enviroment variable
!!      ! STOP=RUN unless STOP=RUN is already set
!!
!!      !otherwise print various environment variable values
!!      !converted to various types
!!
!!       if(get_env('STOP').eq.'RUN')then
!!          write(*,g)repeat('-',80)
!!          write(*,g)get_env('CHARACTER','string')
!!          write(*,g)get_env('INTEGER',100)
!!          write(*,g)get_env('REAL',200.0)
!!          write(*,g)get_env('DOUBLE',300.0d0)
!!          write(*,g)get_env('LOGICAL',.true.)
!!
!!          write(*,g)repeat('-',80)
!!          write(*,g)get_env('CHARACTER','string',ierr=ierr)
!!          write(*,g)'ierr=',ierr
!!          write(*,g)get_env('INTEGER',100,ierr=ierr)
!!          write(*,g)'ierr=',ierr
!!          write(*,g)get_env('REAL',200.0,ierr=ierr)
!!          write(*,g)'ierr=',ierr
!!          write(*,g)get_env('DOUBLE',300.0d0,ierr=ierr)
!!          write(*,g)'ierr=',ierr
!!          write(*,g)get_env('LOGICAL',.true.)
!!          write(*,g)'ierr=',ierr
!!
!!          write(*,g)repeat('-',80)
!!          write(*,g)get_env('CHARACTER')
!!          write(*,g)get_env('HOME')
!!        else
!!          write(*,g)repeat('-',80)
!!          call execute_command_line('env STOP=RUN '//get_arg(0))
!!          call execute_command_line('env STOP=RUN CHARACTER=aaaa &
!!          & INTEGER=1 REAL=2.3 DOUBLE=444444444444 '//get_arg(0))
!!          call execute_command_line('env STOP=RUN CHARACTER=bbbb &
!!          & INTEGER=1 REAL=2.3 DOUBLE=44.555 '//get_arg(0))
!!          call execute_command_line('env STOP=RUN CHARACTER=cccc &
!!          & INTEGER=asdf REAL=asdf DOUBLE=adsf '//get_arg(0))
!!          write(*,g)repeat('-',80)
!!          stop
!!       endif
!!
!!    end program demo_get_env
!!
!!  Results
!!
!!    >  /home/johns
!!    >  /home/johns/.local/bin:/usr/local/bin:/usr/bin:
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >
!!    >
!!    > ------------------------------------------------------------
!!    >  /home/johns
!!    >  /home/johns/.local/bin:/usr/local/bin:/usr/bin:
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >
!!    >
!!    > ------------------------------------------------------------
!!    > string
!!    > 100
!!    > 200.000000
!!    > 300.00000000000000
!!    > T
!!    > ------------------------------------------------------------
!!    > string
!!    >  ierr=           0
!!    > 100
!!    >  ierr=           0
!!    > 200.000000
!!    >  ierr=           0
!!    > 300.00000000000000
!!    >  ierr=           0
!!    > T
!!    >  ierr=           0
!!    > ------------------------------------------------------------
!!    >
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns/.local/bin:/usr/local/bin:/usr/bin:
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >
!!    >
!!    > ------------------------------------------------------------
!!    > aaaa
!!    > 1
!!    > 2.29999995
!!    > 444444444444.00000
!!    > T
!!    > ------------------------------------------------------------
!!    > aaaa
!!    >  ierr=           0
!!    > 1
!!    >  ierr=           0
!!    > 2.29999995
!!    >  ierr=           0
!!    > 444444444444.00000
!!    >  ierr=           0
!!    > T
!!    >  ierr=           0
!!    > ------------------------------------------------------------
!!    > aaaa
!!    > /home/johns
!!    > /home/johns
!!    >  /home/johns/.local/bin:/usr/local/bin:/usr/bin:
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >
!!    >
!!    > ------------------------------------------------------------
!!    > bbbb
!!    > 1
!!    > 2.29999995
!!    > 44.555000000000000
!!    > T
!!    > ------------------------------------------------------------
!!    > bbbb
!!    >  ierr=           0
!!    > 1
!!    >  ierr=           0
!!    > 2.29999995
!!    >  ierr=           0
!!    > 44.555000000000000
!!    >  ierr=           0
!!    > T
!!    >  ierr=           0
!!    > ------------------------------------------------------------
!!    > bbbb
!!    > /home/johns
!!    > /home/johns
!!    >  /home/johns/.local/bin:/usr/local/bin:/usr/bin:
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  /home/johns
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >  Have a nice day 😃!
!!    >
!!    >
!!    > ------------------------------------------------------------
!!    > cccc
!!    > -2147483647
!!    > -0.340282347E+39
!!    > -0.17976931348623157E+309
!!    > T
!!    > ------------------------------------------------------------
!!    > cccc
!!    >  ierr=           0
!!    > -2147483647
!!    >  ierr=        5010
!!    > -0.340282347E+39
!!    >  ierr=        5010
!!    > -0.17976931348623157E+309
!!    >  ierr=        5010
!!    > T
!!    >  ierr=        5010
!!    > ------------------------------------------------------------
!!    > cccc
!!    > /home/johns
!!    > ------------------------------------------------------------
!!
!!### SEE ALSO
!!
!!    get_environment_variable(3fortran), system_getenv(3m_system),
!!    set_environment_variable(3m_system), system_putenv(3m_system),
!!    system_clearenv(3m_system), system_initenv(3m_system),
!!    system_getenv(3m_system), system_unsetenv(3m_system)
!!
!!### AUTHOR
!!    John S. Urban
!!
!!### LICENSE
!!    MIT
function get_env_character(NAME,DEFAULT,force_,ierr) result(VALUE)
implicit none
character(len=*),intent(in)              :: NAME
character(len=*),intent(in),optional     :: DEFAULT
type(force_keywords),optional,intent(in) :: force_
integer,intent(out),optional             :: ierr
character(len=:),allocatable             :: VALUE

character(len=255)                       :: errmsg
integer                                  :: howbig
integer                                  :: stat
integer                                  :: length
   ! get length required to hold value
   length=0
   errmsg=''
   stat=0
   if(NAME.ne.'')then
      call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
         !*!print *, NAME, " is not defined in the environment. Strange..."
         VALUE=''
         stat=0
      case (2)
         !*!print *, "This processor doesn't support environment variables. Boooh!"
         VALUE=''
      case default
         ! make string to hold value of sufficient size
         allocate(character(len=max(howbig,1)) :: VALUE)
         ! get value
         call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
         if(stat.ne.0)VALUE=''
      end select
   else
      VALUE=''
   endif
   if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
   if(present(ierr))then
      ierr=stat
   elseif(stat.ne.0)then
      !write(stderr,'(a)')trim(errmsg)
   endif
end function get_env_character

function get_env_real(NAME,DEFAULT,force_,ierr) result(VALUE)
character(len=*),intent(in)                :: NAME
real,intent(in)                            :: DEFAULT
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
real                                       :: VALUE
character(len=:),allocatable               :: STRING
integer                                    :: iostat
character(len=255)                         :: iomsg, fmt
   STRING=get_env_character(NAME,'')
   iostat=0
   iomsg=''
   if(STRING.eq.'')then
      VALUE=DEFAULT
   else
      write(fmt,'(*(g0))')'(g',max(1,len(string)),'.0)'
      string=string//' '
      read(STRING,fmt,iostat=iostat,iomsg=iomsg)value
      if(iostat.ne.0)then
         value=-huge(0.0)
      endif
   endif
   if(present(ierr))then
      ierr=iostat
   elseif(iostat.ne.0)then
      write(stderr,'(a)')'<ERROR>*get_env* NAME='//NAME//' STRING='//STRING//':'//trim(iomsg)
   endif
end function get_env_real

function get_env_double(NAME,DEFAULT,force_,ierr) result(VALUE)
character(len=*),intent(in)                :: NAME
doubleprecision,intent(in)                 :: DEFAULT
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
doubleprecision                            :: VALUE
character(len=:),allocatable               :: STRING
integer                                    :: iostat
character(len=255)                         :: iomsg, fmt
   STRING=get_env_character(NAME,'')
   iostat=0
   iomsg=''
   if(STRING.eq.'')then
      VALUE=DEFAULT
   else
      write(fmt,'(*(g0))')'(g',max(1,len(string)),'.0)'
      string=string//' '
      read(STRING,fmt,iostat=iostat,iomsg=iomsg)value
      if(iostat.ne.0)then
         value=-huge(0.0d0)
      endif
   endif
   if(present(ierr))then
      ierr=iostat
   elseif(iostat.ne.0)then
      write(stderr,'(a)')'<ERROR>*get_env* NAME='//NAME//' STRING='//STRING//':'//trim(iomsg)
   endif
end function get_env_double

function get_env_integer(NAME,DEFAULT,force_,ierr) result(VALUE)
character(len=*),intent(in)                :: NAME
integer,intent(in)                         :: DEFAULT
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
integer                                    :: VALUE
character(len=:),allocatable               :: STRING
integer                                    :: iostat
character(len=255)                         :: iomsg, fmt
   STRING=get_env_character(NAME,'')
   iostat=0
   iomsg=''
   if(STRING.eq.'')then
      VALUE=DEFAULT
      iostat=0
   else
      write(fmt,'(*(g0))')'(i',max(1,len(string)),')'
      string=string//' '
      read(STRING,fmt,iostat=iostat,iomsg=iomsg)value
      if(iostat.ne.0)then
         value=-huge(0)
      endif
   endif
   if(present(ierr))then
      ierr=iostat
   elseif(iostat.ne.0)then
      write(stderr,'(a)')'<ERROR>*get_env* NAME='//NAME//' STRING='//STRING//':'//trim(iomsg)
   endif
end function get_env_integer

function get_env_logical(NAME,DEFAULT,force_,ierr) result(VALUE)
character(len=*),intent(in)                :: NAME
logical,intent(in)                         :: DEFAULT
type(force_keywords), optional, intent(in) :: force_
integer,intent(out),optional               :: ierr
logical                                    :: VALUE
character(len=:),allocatable               :: STRING
integer                                    :: iostat
character(len=255)                         :: iomsg, fmt
character(len=1)                           :: ch
   STRING=get_env_character(NAME,'',ierr=iostat)
   if(iostat.ne.0)then
      VALUE=.false.
   elseif(STRING.eq.'')then
      VALUE=DEFAULT
      iostat=0
   else
      string=string//'  '
      ch=string(1:1)
      if(ch.eq.'.')ch=string(2:2)
      select case(ch)
      case('t','T','y','Y')
         value=.true.
      case('f','F','n','N')
         value=.false.
       case default
         value=.false.
      end select
   endif
   if(present(ierr))then
      ierr=iostat
   elseif(iostat.ne.0)then
      write(stderr,'(a)')'<ERROR>*get_env* NAME='//NAME//' STRING='//STRING//':'//trim(iomsg)
   endif
end function get_env_logical

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_unicode__get
