program demo_add_backslash

use iso_fortran_env, only : stdout => output_unit
use m_unicode,       only : add_backslash, expand_backslash
use m_unicode,       only : add_html, expand_html
use m_unicode,       only : keyword_expand
use m_unicode,       only : upper, lower
use m_unicode,       only : assignment(=)
use m_unicode,       only : ut => unicode_type
use m_unicode,       only : character, ch=>character, trim
implicit none
character(len=:),allocatable :: apoem(:)
type(ut),allocatable         :: upoem(:)
character(len=:),allocatable :: astrings(:)
type(ut),allocatable         :: ustrings(:)
character(len=:),allocatable :: astring
type(ut)                     :: ustring
integer                      :: i

      apoem=[character(len=255) :: &
      '古池や    ', &
      '蛙飛びこむ', &
      '水の音    ' ]

   upoem=apoem

   write(*,'(a)')(trim(upoem(i)%ch()),i=1,size(upoem))

   ustrings=add_backslash(upoem)
   write(*,'(a)')(trim(ustrings(i)%ch()),i=1,size(ustrings))

   ustrings=expand_backslash(ustrings)
   write(*,'(a)') ch(ustrings)

   astrings=add_html(upoem)
   write(*,'(a)')(trim(astrings(i)),i=1,size(astrings))

   ustrings=expand_html(ustrings)
   write(*,'(a)') ch(ustrings)

   write(*,*)

   astrings=[character(len=60) :: &
    'c=<pi><times>d',          &
    'a=<pi><times>r<S2>',      &
    '<Delta><delta>',          &
    'Copyright<copy>']

    write(*,'(a)') ch(keyword_expand(astrings))

    astring='АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ'
    write(stdout,'(a)') astring
    write(stdout,'(a)') ch(lower(astring))
    ustring=astring
    astring=ustring
    write(stdout,'(a)') astring

end program demo_add_backslash
