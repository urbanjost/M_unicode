! example/markdown_links.f90 (Non-Greedy Quantifiers & Backreference Rewriting)

! Demonstrates non-greedy quantifiers (.*?) paired with regex_replace and
! $1 / $2 backreferences to convert Markdown links into HTML anchor tags
! without greedily spanning across multiple links on the same line.

program markdown_links
   use iso_fortran_env,  only : int32
   use M_unicode__regex, only : utoc, ctou
   use M_unicode__regex, only : regex_t, regex_compile, regex_replace
   implicit none

   character(len=1), parameter   :: BS = achar(92)
   type(regex_t)                 :: re
   integer(int32), allocatable   :: doc_cp(:), repl_cp(:), html_cp(:)
   character(len=:), allocatable :: markdown, pat_str

   markdown = 'See the [Fortran Lang](https://fortran-lang.org) site or ' // &
              'read [Müller’s Guide](https://example.org) for details.'

   ! Pattern: \[(.*?)\]\((.*?)\)
   pat_str = BS // '[(.*?)' // BS // ']' // BS // '((.*?)' // BS // ')'

   re = regex_compile(utoc(pat_str))
   if (.not. re%valid) error stop 'Failed to compile Markdown link pattern'

   doc_cp  = utoc(markdown)
   repl_cp = utoc('<a href="$2">$1</a>')
   html_cp = regex_replace(re, doc_cp, repl_cp, replace_all=.true.)

   print '(A)', 'Input Markdown:'
   print '(A)', '  ' // markdown
   print '(A)', 'Converted HTML:'
   print '(A)', '  ' // ctou(html_cp)
end program markdown_links
