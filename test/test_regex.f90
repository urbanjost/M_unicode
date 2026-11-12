program test_M_unicode__regex
   use iso_fortran_env, only: int32, error_unit
   use M_unicode codepoints_to_utf8, utf8_to_codepoints
   use M_unicode__regex
   implicit none

   character(len=1), parameter :: BS = achar(92)
   integer :: passed = 0
   integer :: failed = 0

   print '(A)', 'Running M_unicode__regex test suite...'
   print '(A)', '--------------------------------------------------'

   call test_utf8_roundtrip()
   call test_basic_literals_and_anchors()
   call test_alternation_and_grouping()
   call test_quantifiers_and_nesting()
   call test_counted_repetitions()
   call test_non_greedy_quantifiers()
   call test_word_boundaries()
   call test_char_classes_and_escapes()
   call test_unicode_and_emoji()
   call test_replace_and_backrefs()
   call test_invalid_patterns()

   print '(A)', '--------------------------------------------------'
   print '(A,I0,A,I0,A)', 'Summary: ', passed, ' passed, ', failed, ' failed.'
   if (failed > 0) error stop 1

contains

   subroutine assert_true(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (cond) then
         passed = passed + 1
         print '(A,A)', '[PASS] ', name
      else
         failed = failed + 1
         write(error_unit, '(A,A)') '[FAIL] ', name
      end if
   end subroutine assert_true

   subroutine assert_str_eq(actual, expected, name)
      character(len=*), intent(in) :: actual, expected, name
      if (actual == expected) then
         passed = passed + 1
         print '(A,A)', '[PASS] ', name
      else
         failed = failed + 1
         write(error_unit, '(A,A)') '[FAIL] ', name
         write(error_unit, '(A,A,A)') '       Expected: "', expected, '"'
         write(error_unit, '(A,A,A)') '       Actual:   "', actual, '"'
      end if
   end subroutine assert_str_eq

   function extract_group(text_cp, m, grp) result(sub)
      integer(int32), intent(in) :: text_cp(:)
      type(match_t), intent(in) :: m
      integer, intent(in) :: grp
      character(len=:), allocatable :: sub
      if (.not. m%matched .or. m%start_pos(grp) < 1 .or. m%end_pos(grp) < m%start_pos(grp)) then
         sub = ''
      else
         sub = codepoints_to_utf8(text_cp(m%start_pos(grp):m%end_pos(grp)))
      end if
   end function extract_group

   subroutine test_utf8_roundtrip()
      character(len=*), parameter :: sample = 'Hello, Café 🚀 — 日本語!'
      integer(int32), allocatable :: cp(:)
      character(len=:), allocatable :: roundtrip

      cp = utf8_to_codepoints(sample)
      call assert_true(size(cp) == 20, 'UTF-8 codepoint count (ASCII + Latin + Emoji + CJK)')
      roundtrip = codepoints_to_utf8(cp)
      call assert_str_eq(roundtrip, sample, 'UTF-8 -> UTF-32 -> UTF-8 roundtrip')

      ! Empty string roundtrip
      cp = utf8_to_codepoints('')
      call assert_true(size(cp) == 0, 'Empty UTF-8 string produces 0 codepoints')
      call assert_str_eq(codepoints_to_utf8(cp), '', 'Empty codepoint array produces empty UTF-8')
   end subroutine test_utf8_roundtrip

   subroutine test_basic_literals_and_anchors()
      type(regex_t) :: re
      type(match_t) :: m
      integer(int32), allocatable :: txt(:)

      txt = utf8_to_codepoints('line1' // new_line('a') // 'target_word' // new_line('a') // 'line3')

      re = regex_compile(utf8_to_codepoints('^target_word$'))
      call assert_true(re%valid, 'Compile multiline anchored pattern')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Match ^target_word$ across newlines')
      call assert_str_eq(extract_group(txt, m, 0), 'target_word', 'Extracted multiline match')

      re = regex_compile(utf8_to_codepoints('^word$'))
      m = regex_search(re, txt)
      call assert_true(.not. m%matched, 'Reject partial line match with ^...$')

      ! Dot does not cross newline
      re = regex_compile(utf8_to_codepoints('line1.target'))
      m = regex_search(re, txt)
      call assert_true(.not. m%matched, 'Dot (.) does not match newline')
   end subroutine test_basic_literals_and_anchors

   subroutine test_alternation_and_grouping()
      type(regex_t) :: re
      type(match_t) :: m
      integer(int32), allocatable :: txt(:)

      re = regex_compile(utf8_to_codepoints('^(cat|dog|bird|fish)$'))
      call assert_true(re%valid, 'Compile 4-way alternation')

      txt = utf8_to_codepoints('cat')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Alternation matches 1st branch (cat)')
      call assert_str_eq(extract_group(txt, m, 1), 'cat', 'Capture group 1 = cat')

      txt = utf8_to_codepoints('dog')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Alternation matches 2nd branch (dog)')
      call assert_str_eq(extract_group(txt, m, 1), 'dog', 'Capture group 1 = dog')

      txt = utf8_to_codepoints('bird')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Alternation matches 3rd branch (bird)')
      call assert_str_eq(extract_group(txt, m, 1), 'bird', 'Capture group 1 = bird')

      txt = utf8_to_codepoints('fish')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Alternation matches 4th branch (fish)')
      call assert_str_eq(extract_group(txt, m, 1), 'fish', 'Capture group 1 = fish')

      txt = utf8_to_codepoints('catdog')
      m = regex_search(re, txt)
      call assert_true(.not. m%matched, 'Alternation does not fall through cat -> dog')

      ! Empty branch alternation (a|)
      re = regex_compile(utf8_to_codepoints('^(a|)$'))
      m = regex_search(re, utf8_to_codepoints(''))
      call assert_true(m%matched, 'Empty alternation branch (a|) matches empty string')
   end subroutine test_alternation_and_grouping

   subroutine test_quantifiers_and_nesting()
      type(regex_t) :: re
      type(match_t) :: m
      integer(int32), allocatable :: txt(:)

      re = regex_compile(utf8_to_codepoints('^(a(b|c)*d)+$'))
      call assert_true(re%valid, 'Compile nested alternation inside * and +')

      txt = utf8_to_codepoints('adabcbd')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Nested quantifier matches adabcbd')
      call assert_str_eq(extract_group(txt, m, 1), 'abcbd', 'Outer group captures last iteration')
      call assert_str_eq(extract_group(txt, m, 2), 'b', 'Inner group captures last alternation')

      re = regex_compile(utf8_to_codepoints('^colou?r$'))
      m = regex_search(re, utf8_to_codepoints('color'))
      call assert_true(m%matched, 'Optional ? matches zero occurrences (color)')
      m = regex_search(re, utf8_to_codepoints('colour'))
      call assert_true(m%matched, 'Optional ? matches one occurrence (colour)')
   end subroutine test_quantifiers_and_nesting

   subroutine test_counted_repetitions()
      type(regex_t) :: re
      type(match_t) :: m
      integer(int32), allocatable :: txt(:)

      ! Exact {n}
      re = regex_compile(utf8_to_codepoints('^a{3}$'))
      call assert_true(re%valid, 'Compile exact repetition a{3}')
      m = regex_search(re, utf8_to_codepoints('aaa'))
      call assert_true(m%matched, 'a{3} matches aaa')
      m = regex_search(re, utf8_to_codepoints('aa'))
      call assert_true(.not. m%matched, 'a{3} rejects aa')
      m = regex_search(re, utf8_to_codepoints('aaaa'))
      call assert_true(.not. m%matched, 'a{3} rejects aaaa')

      ! Zero repetition {0}
      re = regex_compile(utf8_to_codepoints('^ab{0}c$'))
      m = regex_search(re, utf8_to_codepoints('ac'))
      call assert_true(m%matched, 'ab{0}c matches ac')
      m = regex_search(re, utf8_to_codepoints('abc'))
      call assert_true(.not. m%matched, 'ab{0}c rejects abc')

      ! Bounded {n,m}
      re = regex_compile(utf8_to_codepoints('^' // BS // 'd{2,4}$'))
      call assert_true(re%valid, 'Compile bounded repetition \d{2,4}')
      m = regex_search(re, utf8_to_codepoints('1'))
      call assert_true(.not. m%matched, '\d{2,4} rejects 1 digit')
      m = regex_search(re, utf8_to_codepoints('123'))
      call assert_true(m%matched, '\d{2,4} matches 3 digits')
      m = regex_search(re, utf8_to_codepoints('12345'))
      call assert_true(.not. m%matched, '\d{2,4} rejects 5 digits')

      ! Bounded from zero {0,2}
      re = regex_compile(utf8_to_codepoints('^x{0,2}$'))
      m = regex_search(re, utf8_to_codepoints(''))
      call assert_true(m%matched, 'x{0,2} matches empty string')
      m = regex_search(re, utf8_to_codepoints('xx'))
      call assert_true(m%matched, 'x{0,2} matches xx')
      m = regex_search(re, utf8_to_codepoints('xxx'))
      call assert_true(.not. m%matched, 'x{0,2} rejects xxx')

      ! Unbounded {n,}
      re = regex_compile(utf8_to_codepoints('^(ab){2,}$'))
      txt = utf8_to_codepoints('ababab')
      m = regex_search(re, txt)
      call assert_true(m%matched, '(ab){2,} matches ababab')
      call assert_str_eq(extract_group(txt, m, 1), 'ab', 'Capture group in (ab){2, }')
! Non-quantifier literal braces {foo}
      re = regex_compile(utf8_to_codepoints('^a{foo}b$'))
      call assert_true(re%valid, 'Literal braces a{foo}b compile as literal chars')
      m = regex_search(re, utf8_to_codepoints('a{foo}b'))
      call assert_true(m%matched, 'Literal braces a{foo}b match literally')
   end subroutine test_counted_repetitions
   subroutine test_non_greedy_quantifiers()
      type(regex_t) :: re
      type(match_t) :: m
      integer(int32), allocatable :: txt(:), repl(:), out(:)

      txt = utf8_to_codepoints('<tag>first</tag> and <tag>second</tag>')

      ! Greedy .* spans across both tags
      re = regex_compile(utf8_to_codepoints('<tag>.*</tag>'))
      m = regex_search(re, txt)
      call assert_str_eq(extract_group(txt, m, 0), '<tag>first</tag> and <tag>second</tag>', &
         'Greedy <tag>.*</tag> spans to last closing tag')

      ! Non-greedy .*? stops at the first closing tag
      re = regex_compile(utf8_to_codepoints('<tag>(.*?)</tag>'))
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Non-greedy <tag>(.*?)</tag> matches')
      call assert_str_eq(extract_group(txt, m, 1), 'first', &
         'Non-greedy <tag>(.*?)</tag> captures first tag only')

      ! Global replacement with non-greedy +?
      re = regex_compile(utf8_to_codepoints(BS // '[(.+?)' // BS // ']'))
      txt = utf8_to_codepoints('[one] [two] [three]')
      repl = utf8_to_codepoints('($1)')
      out = regex_replace(re, txt, repl, replace_all=.true.)
      call assert_str_eq(codepoints_to_utf8(out), '(one) (two) (three)', &
         'Global replace with non-greedy \[(.+?)\]')

      ! Non-greedy bounded repetition {1,3}? prefers minimum count
      re = regex_compile(utf8_to_codepoints('a{1,3}?'))
      txt = utf8_to_codepoints('aaaa')
      m = regex_search(re, txt)
      call assert_str_eq(extract_group(txt, m, 0), 'a', &
         'Non-greedy a{1,3}? matches minimal 1 char')

      ! Non-greedy optional ?? prefers 0 matches when allowed
      re = regex_compile(utf8_to_codepoints('ab??'))
      txt = utf8_to_codepoints('ab')
      m = regex_search(re, txt)
      call assert_str_eq(extract_group(txt, m, 0), 'a', &
         'Non-greedy ab?? matches minimal "a"')
   end subroutine test_non_greedy_quantifiers
   subroutine test_word_boundaries()
      type(regex_t) :: re
      type(match_t) :: m
      integer(int32), allocatable :: txt(:), repl(:), out(:)
      re = regex_compile(utf8_to_codepoints(BS // 'bcat' // BS // 'b'))
      call assert_true(re%valid, 'Compile word boundary \bcat\b')
      txt = utf8_to_codepoints('scatter the cat around')
      m = regex_search(re, txt)
      call assert_true(m%matched, '\bcat\b matches standalone word cat')
      call assert_true(m%start_pos(0) == 13 .and. m%end_pos(0) == 15, &
         '\bcat\b skips "cat" inside "scatter"')
! Unicode word boundary with accented Latin characters
      re = regex_compile(utf8_to_codepoints(BS // 'bZürich' // BS // 'b'))
      txt = utf8_to_codepoints('In Zürich, Switzerland')
      m = regex_search(re, txt)
      call assert_true(m%matched, '\bZürich\b matches accented word boundary')
! Non-word boundary \B matches inside a word
      re = regex_compile(utf8_to_codepoints(BS // 'Bcat' // BS // 'B'))
      txt = utf8_to_codepoints('scatter')
      m = regex_search(re, txt)
      call assert_true(m%matched, '\Bcat\B matches inside "scatter"')
      txt = utf8_to_codepoints('cat')
      m = regex_search(re, txt)
      call assert_true(.not. m%matched, '\Bcat\B rejects standalone "cat"')
! Global word replacement
      re = regex_compile(utf8_to_codepoints(BS // 'bfoo' // BS // 'b'))
      txt = utf8_to_codepoints('foo foobar barfoo foo')
      repl = utf8_to_codepoints('baz')
      out = regex_replace(re, txt, repl, replace_all=.true.)
      call assert_str_eq(codepoints_to_utf8(out), 'baz foobar barfoo baz', &
         'Replace whole words only with \bfoo\b')
   end subroutine test_word_boundaries
   subroutine test_char_classes_and_escapes()
      type(regex_t) :: re
      type(match_t) :: m
      integer(int32), allocatable :: txt(:)
! Shorthand \d inside character class [...]
      re = regex_compile(utf8_to_codepoints('^[' // BS // 'da-fA-F]+$'))
      call assert_true(re%valid, 'Compile hex class with \d shorthand')
      txt = utf8_to_codepoints('deadBEEF42')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Match hex digits via [\da-fA-F]+')
! Negated class [^...]
      re = regex_compile(utf8_to_codepoints('^[^0-9]+$'))
      txt = utf8_to_codepoints('abc_XYZ!')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Negated class [^0-9]+ matches non-digits')
! Negated shorthand \D, \W ⁠
      re = regex_compile(utf8_to_codepoints('^' // BS // 'D+' // BS // 'S+' // BS // 'W+$'))
      txt = utf8_to_codepoints('abc!@#')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Negated shorthands \D, \S, \W match properly')
   end subroutine test_char_classes_and_escapes
   subroutine test_unicode_and_emoji()
      type(regex_t) :: re
      type(match_t) :: m
      integer(int32), allocatable :: txt(:)
      re = regex_compile(utf8_to_codepoints('(' // BS // 'w+)' // BS // 's+(🚀|🛸)'))
      call assert_true(re%valid, 'Compile Unicode \w and emoji alternation')
      txt = utf8_to_codepoints('Launch Zürich 🚀 now')
      m = regex_search(re, txt)
      call assert_true(m%matched, 'Match accented Unicode word + emoji')
      call assert_str_eq(extract_group(txt, m, 1), 'Zürich', 'Captured Unicode word Zürich')
      call assert_str_eq(extract_group(txt, m, 2), '🚀', 'Captured emoji 🚀')
   end subroutine test_unicode_and_emoji
   subroutine test_replace_and_backrefs()
      type(regex_t) :: re
      integer(int32), allocatable :: txt(:), repl(:), out(:)

      re = regex_compile(utf8_to_codepoints('(' // BS // 'd{4})-(' // BS // 'd{2})-(' // BS // 'd{2})'))
      txt = utf8_to_codepoints('Dates: 2026-11-11 and 2027-01-05.')
      repl = utf8_to_codepoints('$3/$2/$1')

      out = regex_replace(re, txt, repl, replace_all=.true.)
      call assert_str_eq(codepoints_to_utf8(out), 'Dates: 11/11/2026 and 05/01/2027.', &
         'Global replace with $1..$3 backreferences')

      out = regex_replace(re, txt, repl, replace_all=.false.)
      call assert_str_eq(codepoints_to_utf8(out), 'Dates: 11/11/2026 and 2027-01-05.', &
         'Single replace (replace_all=.false.)')

      ! Backslash backreference (\1) and escaped literal dollar ($$)
      repl = utf8_to_codepoints('$$' // BS // '1')
      out = regex_replace(re, txt, repl, replace_all=.false.)
      call assert_str_eq(codepoints_to_utf8(out), 'Dates: $2026 and 2027-01-05.', &
         'Backslash backref \1 and escaped $$ in replacement')

      ! Zero-length match replacement
      re = regex_compile(utf8_to_codepoints('a*'))
      txt = utf8_to_codepoints('b')
      repl = utf8_to_codepoints('X')
      out = regex_replace(re, txt, repl, replace_all=.true.)
      call assert_str_eq(codepoints_to_utf8(out), 'XbX', &
         'Zero-length match replace matches standard PCRE/Python semantics')
   end subroutine test_replace_and_backrefs
   subroutine test_invalid_patterns()
      type(regex_t) :: re
      re = regex_compile(utf8_to_codepoints('*abc'))
      call assert_true(.not. re%valid, 'Reject leading quantifier *abc')
      re = regex_compile(utf8_to_codepoints('a**'))
      call assert_true(.not. re%valid, 'Reject chained quantifier a**')
      re = regex_compile(utf8_to_codepoints('a*??'))
      call assert_true(.not. re%valid, 'Reject invalid quantifier a*??')
      re = regex_compile(utf8_to_codepoints('a{5,2}'))
      call assert_true(.not. re%valid, 'Reject inverted range a{5,2}')
      re = regex_compile(utf8_to_codepoints('a{2}{3}'))
      call assert_true(.not. re%valid, 'Reject chained brace quantifier a{2}{3}')
      re = regex_compile(utf8_to_codepoints('(unclosed'))
      call assert_true(.not. re%valid, 'Reject unclosed group (unclosed')
      re = regex_compile(utf8_to_codepoints('[z-a]'))
      call assert_true(.not. re%valid, 'Reject inverted range [z-a]')
      re = regex_compile(utf8_to_codepoints('trailing' // BS))
      call assert_true(.not. re%valid, 'Reject trailing backslash')
   end subroutine test_invalid_patterns
end program test_M_unicode__regex
