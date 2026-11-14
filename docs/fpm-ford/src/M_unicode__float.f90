!>
!!### NAME
!!    str_to_real(3f) ‐ [M_unicode:TYPE] subroutine returns numeric value
!!    from string (LICENSE:MIT)
!!
!!### SYNOPSIS
!!
!!     pure elemental subroutine str_to_real(str, val, ierr)
!!
!!         character(len=*), intent(in)  :: str
!!         real(real64),     intent(out) :: val
!!         integer,          intent(out) :: ierr
!!
!!### DESCRIPTION
!!
!!  STR_TO_REAL(3f) returns a numeric value given a string representing
!!  a numeric value; without using internal I/O (the conventional method
!!  in Fortran).
!!
!!  The procedure utilitizes full IEEE 754 support for literal
!!  Inf/Infinity, NaN (case-insensitive), and signed -0.0 preservation,
!!  while keeping the procedure pure elemental.
!!
!!  STR_TO_REAL(3f) works with any G‐format input, This includes whole
!!  number, float, and exponential syntax. That is, input strings may
!!  contain signs (+/-), decimal points, and scientific notation (e/E/d/D).
!!
!!  The output is a double-precision real (real(real64)), along with an
!!  integer error flag (ierr).
!!
!!### OPTIONS
!!      chars  input string to read numeric value from
!!
!!### RETURNS
!!      valu   DOUBLEPRECISION numeric value returned.
!!      ierr   error flag (0 == no error)
!!
!!   KEY FEATURES OF THIS IMPLEMENTATION
!!
!!   o ASCII Digit Conversion. Uses iachar(c) - iachar('0') rather than
!!     hardcoded ASCII offsets like 48 to stay portable across compiler
!!     environments.
!!
!!   o No Internal I/O Overhead. Avoids Fortran's internal READ(3f)
!!     statement, making it faster in tight loops and safe to use inside
!!     pure or elemental procedures or inside parallel OpenMP/GPU regions
!!     where I/O is restricted.
!!
!!   o Error Handling (ierr). Safely catches empty strings (1), misplaced
!!     decimals (2), malformed exponents (3 or 5), and stray non-numeric
!!     characters (4) without crashing the program.
!!
!!   o The subroutine is  pure (and elemental) so it can process arrays
!!     directly, To maintain this feature the first and last non-space
!!     character indices are located directly on the input, avoiding dynamic
!!     heap allocation. The intent is to keeps the procedure completely
!!     side-effect-free and reasonably fast while providing input valication.
!!
!!     Pure procedures are required for optimal performance or even appearance
!!     iin do concurrent loops, OpenMP simd/target regions, or directly on
!!     multi-dimensional string arrays.
!!
!!   o Zero Heap Allocations. This avoids allocating memory inside the
!!     procedure and keeps the CPU cache footprint minimal.
!!
!!
!!   o overflow protection uses Fortran intrinsic inquiry functions (huge(),
!!     maxexponent(), and minexponent()) rather than hardcoded magic numbers.
!!
!!     overflow is guarded agains in in three places:
!!
!!      1. The Exponent Integer (exp_val):  Prevents integer overflow
!!      if a malicious or malformed string has a massive exponent like
!!      1e99999999999.
!!
!!      2. The Mantissa (int_part):  This prevents floating-point overflow
!!      before the exponent is even applied (e.g., a 400-digit string with
!!      no e marker).
!!
!!      e. The Final Scaled Result (val):  Checks the combined order of
!!      magnitude against maxexponent(val) before raising 10.0 to a power
!!      that would produce Infinity or trigger a floating-point exception.
!!
!!    SUMMARY OF PROTECTIONS ADDED
!!
!!      Error Code 6 (Numeric Overflow): Returned cleanly as Infinity
!!      without halting execution if the value exceeds huge(1.0_real64)
!!      (~1.79769e308).
!!
!!      Exact Edge-Case Checking at 10^308: Distinguishes between valid
!!      numbers near the ceiling (such as "1.79e308") and overflowing numbers
!!      at the same order of magnitude (such as "1.80e308").
!!
!!      Subnormal / Underflow Handling: Numbers smaller than ~10^-324
!!      underflow cleanly to 0.0_real64. Numbers in the subnormal range
!!      (10^-308 to 10^-324) are scaled in two steps (1.0e-300 first) so
!!      10.0_real64 ** exp does not prematurely evaluate to zero before
!!      multiplying by the mantissa.
!!
!!      Zero-Mantissa Safety: Inputs like "0.0e5000" evaluate to 0.0_real64
!!      with ierr = 0 rather than falsely triggering an overflow.
!!
!!      Case-Insensitive Inf, Infinity, and NaN: After parsing any leading +
!!      or - sign, the subroutine checks if the remaining substring is up
!!      to 8 characters long and matches 'INF', 'INFINITY', or 'NAN' in any
!!      letter case (e.g., "-Infinity", "+inf", "NaN"). It returns ierr =
!!      0 alongside the corresponding IEEE value.
!!
!!      Signed Zero Preservation (-0.0): Added a signed_zero(sign_val) helper
!!      using ieee_negative_zero. Inputs like "-0.0", "-0e500", or negative
!!      underflows like "-1.0e-400" now preserve their negative sign bit.
!!
!!    Allocation-Free Uppercasing: Used a fixed-length 8-character helper
!!    (to_upper_8) rather than deferred-length allocatable strings so the
!!    subroutine remains fast and free of heap allocations inside pure
!!    elemental array operations.
!!
!!    Standard IEEE Intrinsics: Imported ieee_value, ieee_positive_inf, and
!!    ieee_negative_inf from ieee_arithmetic. This avoids compiler errors
!!    or runtime SIGFPE crashes caused by trying to compute 1.0_real64 /
!!    0.0_real64.
!!
!!    Sign Preservation on Overflow: Added a pure elemental helper function
!!    signed_inf(sign_val) so inputs like "-1.80e308" return -Infinity while
!!    "1.80e308" returns +Infinity.
!!
!!    Consistent Error 6 Handling: Updated all three overflow return paths
!!    (mantissa overflow, total_exp > 308, and the total_exp == 308 boundary
!!    check) to assign val = signed_inf(sign_val) alongside ierr = 6.
!!
!!### EXAMPLES
!!
!!  Sample Program:
!!
!!   program demo_str_to_real
!!    use, intrinsic :: iso_fortran_env, only: real64
!!    use M_unicode__float, only : str_to_real
!!    implicit none
!!
!!    character(len=16)  :: inputs(4) = [&
!!    & "  -42.75 ", &
!!    & "+1.23e4  ", &
!!    & "bad_input", &
!!    & "3.14D-2  "]
!!    real(real64)       :: values(4), value
!!    integer            :: errors(4), error
!!    integer            :: k
!!    character(len=80)  :: edge_cases(7) = [ character(len=80) :: &
!!    "1.79e308             Valid: just under real64 max (err: 0)      ",  &
!!    "1.80e308             Overflow: just over real64 max (err: 6)    ",  &
!!    "1e999999999999       Overflow: massive exponent string (err: 6) ",  &
!!    "-1.80e308            -Overflow: and just over real64 max (err: 6)", &
!!    "0.0e5000             Valid: zero stays zero (err: 0)            ",  &
!!    "5.0e-324             Valid: subnormal underflow limit (err: 0)  ",  &
!!    "1.0e-400             Valid: graceful underflow to 0.0 (err: 0)  " ]
!!    integer            :: iostat
!!    character(len=256) :: iomsg
!!
!!    ! Process the entire array in a single elemental call
!!    call str_to_real(inputs, values, errors)
!!
!!    do k = 1, size(inputs)
!!       print '(A, " -> val: ", G0, ", err: ", I0)', &
!!       & trim(inputs(k)), values(k), errors(k)
!!    enddo
!!
!!    do k = 1, size(edge_cases)
!!       print *
!!       call str_to_real(edge_cases(k)(:20), value, error)
!!       print '(A, " -> val: ", G0, ", err: ", I0, 1x, a)', &
!!       & trim(edge_cases(k)(:20)), value, error
!!       print '("Expected:",A)', edge_cases(k)(21:)
!!       iomsg = '----------------'
!!       read(edge_cases(k),'(e20.0)',iostat=iostat,iomsg=iomsg)value
!!       print *, 'from internal read',value,iostat,trim(iomsg)
!!    enddo
!!   end program demo_str_to_real
!!
!!### AUTHOR
!!      John S. Urban
!!
!!### LICENSE
!!      MIT
module M_unicode__float
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf, ieee_negative_inf, &
                                             ieee_quiet_nan, ieee_negative_zero
    implicit none
    private
    public :: str_to_real
contains

! Convert a string representing a numeric value to a number with Overflow & IEEE 754 Support
    pure elemental subroutine str_to_real(str, val, ierr)
        character(len=*), intent(in)  :: str
        real(real64),     intent(out) :: val
        integer,          intent(out) :: ierr

        integer                 :: i, i_start, i_end, digit
        integer                 :: sign_val, exp_sign, exp_val
        integer                 :: sig_digits, total_exp
        real(real64)            :: int_part, frac_part, frac_div, mantissa
        logical                 :: has_digits, in_frac, in_exp, has_exp_digits
        character(len=8)        :: token

        ! Hardware-independent limits for real64 (~1.7976931348623157E+308)
        real(real64), parameter :: MAX_VAL      = huge(1.0_real64)
        real(real64), parameter :: SAFE_INT_LIM = MAX_VAL / 10.0_real64
        integer,      parameter :: MAX_EXP_MAG  = 10000  ! Caps exp_val safely below int32 huge()

        ! Initialize outputs
        val  = 0.0_real64
        ierr = 0

        ! Find bounds of non-space characters
        i_start = verify(str, ' ')
        if (i_start == 0) then
            ierr = 1  ! Error: empty or whitespace-only string
            return
        end if
        i_end = len_trim(str)

        i = i_start
        sign_val = 1
        if (str(i:i) == '-') then
            sign_val = -1
            i = i + 1
        else if (str(i:i) == '+') then
            i = i + 1
        end if

        if (i > i_end) then
            ierr = 5  ! Error: lone '+' or '-' sign
            return
        end if

        ! Check for IEEE 754 literal keywords ("INF", "INFINITY", "NAN")
        if ((i_end - i + 1) <= 8) then
            token = to_upper_8(str(i:i_end))
            if (token == 'INF' .or. token == 'INFINITY') then
                val = signed_inf(sign_val)
                return
            else if (token == 'NAN') then
                val = ieee_value(0.0_real64, ieee_quiet_nan)
                return
            end if
        end if

        int_part       = 0.0_real64
        frac_part      = 0.0_real64
        frac_div       = 1.0_real64
        exp_val        = 0
        exp_sign       = 1
        sig_digits     = 0
        has_digits     = .false.
        in_frac        = .false.
        in_exp         = .false.
        has_exp_digits = .false.

        do while (i <= i_end)
            select case (str(i:i))
            case ('0':'9')
                digit = iachar(str(i:i)) - iachar('0')
                if (in_exp) then
                    has_exp_digits = .true.
                    ! Prevent integer overflow on absurdly large exponent strings (e.g., 1e9999999999)
                    if (exp_val <= MAX_EXP_MAG) then
                        exp_val = exp_val * 10 + digit
                    end if
                else if (in_frac) then
                    has_digits = .true.
                    ! Track leading zeros in fraction if int_part is zero (e.g., 0.0005 -> sig_digits = -3)
                    if (int_part == 0.0_real64 .and. frac_part == 0.0_real64 .and. digit == 0) then
                        sig_digits = sig_digits - 1
                    end if
                    if (frac_div < SAFE_INT_LIM) then
                        frac_div  = frac_div * 10.0_real64
                        frac_part = frac_part + real(digit, real64) / frac_div
                    end if
                else
                    has_digits = .true.
                    if (int_part > 0.0_real64 .or. digit > 0) then
                        sig_digits = sig_digits + 1
                    end if
                    ! Guard against mantissa overflow before exponent is even parsed
                    if (int_part > SAFE_INT_LIM) then
                        ierr = 6  ! Error: numeric overflow in integer part
                        val  = signed_inf(sign_val)
                        return
                    end if
                    int_part = int_part * 10.0_real64 + real(digit, real64)
                end if

            case ('.')
                if (in_frac .or. in_exp) then
                    ierr = 2  ! Error: multiple decimal points or decimal in exponent
                    return
                end if
                in_frac = .true.

            case ('e', 'E', 'd', 'D')
                if (in_exp .or. .not. has_digits) then
                    ierr = 3  ! Error: misplaced or duplicate exponent marker
                    return
                end if
                in_exp = .true.
                if (i + 1 <= i_end) then
                    if (str(i+1:i+1) == '-') then
                        exp_sign = -1
                        i = i + 1
                    else if (str(i+1:i+1) == '+') then
                        i = i + 1
                    end if
                end if

            case default
                ierr = 4  ! Error: invalid character encountered
                return
            end select

            i = i + 1
        end do

        ! Final validation checks
        if (.not. has_digits .or. (in_exp .and. .not. has_exp_digits)) then
            ierr = 5  ! Error: incomplete number (e.g., "." or "1.2e+")
            return
        end if

        mantissa = int_part + frac_part

        ! Zero value never overflows regardless of exponent; preserve -0.0 if signed negative
        if (mantissa == 0.0_real64) then
            val = signed_zero(sign_val)
            return
        end if

        if (in_exp) then
            total_exp = (exp_sign * exp_val) + (sig_digits - 1)

            ! Quick bounds check against IEEE 754 double precision (~10^308)
            if (total_exp > 308) then
                ierr = 6  ! Error: numeric overflow
                val  = signed_inf(sign_val)
                return
            else if (total_exp < -324) then
                ! Graceful underflow to signed zero (subnormal limit for real64 is ~4.94E-324)
                val = signed_zero(sign_val)
                return
            end if

            ! Exact boundary check around 10^308 (e.g., 1.79e308 vs 1.80e308)
            if (total_exp == 308) then
                ! Normalize mantissa to [1.0, 10.0) and compare against MAX_VAL / 10^308
                if (mantissa / (10.0_real64 ** (sig_digits - 1)) > MAX_VAL / 1.0e308_real64) then
                    ierr = 6  ! Error: numeric overflow
                    val  = signed_inf(sign_val)
                    return
                end if
            end if

            ! Safe scaling: split extreme negative exponents to avoid 10**(-320) underflowing to 0 prematurely
            if (exp_sign * exp_val < -300) then
                val = real(sign_val, real64) * (mantissa * 1.0e-300_real64) * (10.0_real64 ** (exp_sign * exp_val + 300))
            else
                val = real(sign_val, real64) * mantissa * (10.0_real64 ** (exp_sign * exp_val))
            end if
        else
            val = real(sign_val, real64) * mantissa
        end if
    end subroutine str_to_real

    ! Helper: Return +Inf or -Inf based on sign_val
    pure elemental real(real64) function signed_inf(sign_val)
        integer, intent(in) :: sign_val
        if (sign_val < 0) then
            signed_inf = ieee_value(0.0_real64, ieee_negative_inf)
        else
            signed_inf = ieee_value(0.0_real64, ieee_positive_inf)
        end if
    end function signed_inf

    ! Helper: Return +0.0 or -0.0 based on sign_val
    pure elemental real(real64) function signed_zero(sign_val)
        integer, intent(in) :: sign_val
        if (sign_val < 0) then
            signed_zero = ieee_value(0.0_real64, ieee_negative_zero)
        else
            signed_zero = 0.0_real64
        end if
    end function signed_zero

    ! Helper: Pure ASCII uppercase conversion for fixed 8-char buffer (avoids dynamic allocation)
    pure character(len=8) function to_upper_8(s)
        character(len=*), intent(in) :: s
        integer :: k, c
        to_upper_8 = ' '
        do k = 1, min(len(s), 8)
            c = iachar(s(k:k))
            if (c >= iachar('a') .and. c <= iachar('z')) then
                to_upper_8(k:k) = achar(c - 32)
            else
                to_upper_8(k:k) = s(k:k)
            end if
        end do
    end function to_upper_8

end module M_unicode__float
