     program demo_glob
     use M_unicode, only : glob
     implicit none
        ! Cases with repeating character sequences.
        write(*,*) glob("a*abab",      "a*b") .eqv.  .true.
        write(*,*) glob("ab",          "*?")  .eqv.  .true.
        ! Additional cases where the '*' char appears in the tame string.
        write(*,*) glob("*",     "*")    .eqv.  .true.
        write(*,*) glob("a*ar",  "a*aar").eqv.  .false.
        ! More double wildcard scenarios.
        write(*,*) glob("XYXYXYZYXYz", "XY*Z*XYz") .eqv. .true.
        write(*,*) glob("mississipPI", "*issip*PI") .eqv..true.
        ! Completely tame (no wildcards) cases.
        write(*,*) glob("bLah", "bLah") .eqv..true.
        ! Simple mixed wildcard tests
        write(*,*) glob("a", "*?") .eqv..true.
        ! More mixed wildcard tests including coverage for false positives.
        write(*,*) glob("abcd",   "?b*??")      .eqv..true.
        write(*,*) glob("abcde",  "?*b*?*d*?")  .eqv..true.
        ! Single-character-match cases.
        write(*,*) glob("bLah",   "bL?h")  .eqv..true.
        write(*,*) glob("bLaH",   "?LaH")  .eqv..true.
        write(*,*) glob('abcdefghijk' ,  '?b*')     .eqv..true.
          ! Two pattern match problems that might pose difficulties
        write(*,*) glob('e '           , '*e* ')     .eqv. .true.
        write(*,*) glob('baaaaax'      , 'b*a')      .eqv. .false.
        write(*,*) glob(''             , '*')        .eqv. .true.
     end program demo_glob
