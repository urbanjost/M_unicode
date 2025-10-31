program demo_transliterate

use M_unicode, only : transliterate,ut=>unicode_type
use M_unicode, only : write(formatted),ch=>character
use M_unicode, only : assignment(=)
implicit none
type(ut)  :: STRING, UPPER, LOWER

   STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
   LOWER='abcdefghijklmnopqrstuvwxyz'
   UPPER='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   call callit()
   !
   ! | Α α | Β β | Γ γ | Δ δ | Ε ε | Ζ ζ   |
   ! | Η η | Θ θ | Ι ι | Κ κ | Λ λ | Μ μ   |
   ! | Ν ν | Ξ ξ | Ο ο | Π π | Ρ ρ | Σ σ ς |
   ! | Τ τ | Υ υ | Φ φ | Χ χ | Ψ ψ | Ω ω   |
   !
   STRING='ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω'
   ! ignoring ς for simplicity
   UPPER='ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ'
   LOWER='αβγδεζηθικλμνξοπρστυφχψω'
   call callit()
contains
subroutine callit()
     write(*,'(DT)') STRING

     ! convert -7 string to uppercase:
     write(*,'(DT)') TRANSLITERATE(STRING , LOWER, UPPER )

     ! change all miniscule letters to a colon (":"):
     write(*,'(DT)') TRANSLITERATE(STRING, LOWER, ':')

     ! delete all miniscule letters
     write(*,'(DT)') TRANSLITERATE(STRING, LOWER, '')

     end subroutine callit

end program demo_transliterate
