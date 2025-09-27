program note
use,intrinsic :: iso_fortran_env, only : stdout => output_unit
use,intrinsic :: iso_fortran_env, only : int8,int16,int32,int64
use M_unicode
implicit none
integer,parameter  :: manicules(*) = [int(z'261A'), int(z'261B') ]
type(unicode_type) :: uline
integer            :: iostat
! 
! manicule
! 
! Miscellaneous Symbols block:
! 
!     U+261A ☚ BLACK LEFT POINTING INDEX
!     U+261B ☛ BLACK RIGHT POINTING INDEX
!     U+261C ☜ WHITE LEFT POINTING INDEX
!     U+261D ☝ WHITE UP POINTING INDEX
!     U+261E ☞ WHITE RIGHT POINTING INDEX
!     U+261F ☟ WHITE DOWN POINTING INDEX
! 
! Unicode 6.0 (2010) included four more pointing hands in Miscellaneous Symbols and Pictographs:
! 
!     U+1F446 👆 WHITE UP POINTING BACKHAND INDEX
!     U+1F447 👇 WHITE DOWN POINTING BACKHAND INDEX
!     U+1F448 👈 WHITE LEFT POINTING BACKHAND INDEX
!     U+1F449 👉 WHITE RIGHT POINTING BACKHAND INDEX
! 
! Unicode 7.0 (2014) added several more indices to the Miscellaneous Symbols and Pictographs block, sourced from the Wingdings 2 font:
! 
!     U+1F597 🖗 WHITE DOWN POINTING LEFT HAND INDEX
!     U+1F598 🖘 SIDEWAYS WHITE LEFT POINTING INDEX
!     U+1F599 🖙 SIDEWAYS WHITE RIGHT POINTING INDEX
!     U+1F59A 🖚 SIDEWAYS BLACK LEFT POINTING INDEX
!     U+1F59B 🖛 SIDEWAYS BLACK RIGHT POINTING INDEX
!     U+1F59C 🖜 BLACK LEFT POINTING BACKHAND INDEX
!     U+1F59D 🖝 BLACK RIGHT POINTING BACKHAND INDEX
!     U+1F59E 🖞 SIDEWAYS WHITE UP POINTING INDEX
!     U+1F59F 🖟 SIDEWAYS WHITE DOWN POINTING INDEX
!     U+1F5A0 🖠 SIDEWAYS BLACK UP POINTING INDEX
!     U+1F5A1 🖡 SIDEWAYS BLACK DOWN POINTING INDEX
!     U+1F5A2 🖢 BLACK UP POINTING BACKHAND INDEX
!     U+1F5A3 🖣 BLACK DOWN POINTING BACKHAND INDEX
! 
! Unicode 13.0 (2020) added a three-part index (🯁🯂🯃) in the Symbols for Legacy Computing block:
! 
!     U+1FBC1 🯁 LEFT THIRD WHITE RIGHT POINTING INDEX
!     U+1FBC2 🯂 MIDDLE THIRD WHITE RIGHT POINTING INDEX
!     U+1FBC3 🯃 RIGHT THIRD WHITE RIGHT POINTING INDEX
! 
! Emoji
! 
! Five Unicode manicule characters are emoji, including one of those in
! Unicode 1.0 and all four introduced in Unicode 6.0.[15][16] All five
! have standardized variants for text and emoji presentation.[17]
! 
! Emoji variation sequences U+ 	261D 	1F446 	1F447 	1F448 	1F449
! default presentation 	text 	emoji 	emoji 	emoji 	emoji
! base code point 	☝ 	👆 	👇 	👈 	👉
! base+VS15 (text) 	☝︎ 	👆︎ 	👇︎ 	👈︎ 	👉︎
! base+VS16 (emoji) 	☝️ 	👆️ 	👇️ 	👈️ 	👉️
! See also
! 
!     Arrow (symbol)
!     V sign
!     Obelus (historic text pointer)
!     Hand (hieroglyph) – Egyptian hieroglyph
! 
! Notes
! 
! Although the canonical name is Black left pointing index, Unicode uses
! the word 'black' to mean 'solid' and 'white' to mean 'outlined'. The
! actual colour is a user choice, for example ☚ and ☞.

   uline=manicules

   write(stdout,'(*(g0))')uline%character()

   ! preferred, but not required if not supported
   open(stdout,encoding='utf-8',iostat=iostat)

   write(stdout,'("CHARACTER STRING  :",*(g0))')uline%character()
   write(stdout,'("SINGLE CHARACTERS :",*(g0))')uline%byte()
   write(stdout,'("HEX BYTES         :",*(z0,1x))')uline%byte()
   write(stdout,'("UNICODE CODEPOINTS:",*(g0,1x))')uline%codepoint()
   write(stdout,'("HEX CODEPOINTS    :",*(z0,1x))')uline%codepoint()

end program note
